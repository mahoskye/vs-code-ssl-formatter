import { describe, it } from "mocha";
import { expect } from "chai";
import { decideBlockCloser, leadingIndent, FAMILIES, classifyLineStarts } from "../src/sslBlockCloser";

describe("SSL Block Closer — decideBlockCloser", () => {
    describe("opener detection", () => {
        // One row per family. The FAMILIES-registry sanity test below
        // already pins the registry's *contents*; this table exercises
        // that each row actually fires through decideBlockCloser end-to-end.
        const openerCases: Array<[string, string]> = [
            [":IF nValue > 0;", ":ENDIF;"],
            [":WHILE bRun;", ":ENDWHILE;"],
            [":FOR i := 1 :TO 10;", ":NEXT;"],
            [":BEGINCASE;", ":ENDCASE;"],
            [":TRY;", ":ENDTRY;"],
            [":PROCEDURE Foo;", ":ENDPROC;"],
            [":CLASS Bar;", ":ENDCLASS;"],
            [":REGION Quux;", ":ENDREGION;"],
        ];
        openerCases.forEach(([opener, closer]) => {
            it(`inserts ${closer} after ${opener}`, () => {
                const decision = decideBlockCloser([opener, ""], 0, 1);
                expect(decision, `expected decision for ${opener}`).to.not.be.null;
                expect(decision!.family.closerText).to.equal(closer);
                expect(decision!.insertText).to.equal(`\n${closer}`);
            });
        });

        it("matches case-insensitively on the opener keyword", () => {
            const decision = decideBlockCloser([":if cond;", ""], 0, 1);
            expect(decision!.family.closerText).to.equal(":ENDIF;");
        });

        it("returns null when the line is not a recognised opener", () => {
            // Covers any colon-prefixed statement that isn't in FAMILIES
            // (:DECLARE / :RETURN / :LABEL / etc.) — they all fall through
            // the same regex-miss path.
            expect(decideBlockCloser(["nValue := 1;", ""], 0, 1)).to.be.null;
            expect(decideBlockCloser([":DECLARE x, y;", ""], 0, 1)).to.be.null;
            expect(decideBlockCloser([":RETURN nValue;", ""], 0, 1)).to.be.null;
        });

        it("returns null when the opener is missing its terminating semicolon", () => {
            // Heuristic: we only auto-close once the user has actually typed `;`.
            expect(decideBlockCloser([":IF cond", ""], 0, 1)).to.be.null;
        });

        it("returns null when the line contains text BEFORE :IF (not at line start)", () => {
            // The opener must start the line (modulo leading whitespace).
            expect(decideBlockCloser(["x := 1; :IF cond;", ""], 0, 1)).to.be.null;
        });

        it("ignores :ELSE / :CASE / :CATCH (middle keywords, not openers)", () => {
            expect(decideBlockCloser([":ELSE;", ""], 0, 1)).to.be.null;
            expect(decideBlockCloser([":CASE cond;", ""], 0, 1)).to.be.null;
            expect(decideBlockCloser([":CATCH;", ""], 0, 1)).to.be.null;
        });
    });

    describe("indent preservation", () => {
        // The no-indent case is already exercised by the opener-detection
        // table; this section covers the two non-trivial leading-whitespace
        // forms.
        it("copies the opener's leading whitespace verbatim onto the closer", () => {
            expect(decideBlockCloser(["\t\t:IF cond;", "\t\t\t"], 0, 1)!.insertText)
                .to.equal("\n\t\t:ENDIF;");
            expect(decideBlockCloser(["    :IF cond;", "        "], 0, 1)!.insertText)
                .to.equal("\n    :ENDIF;");
            expect(decideBlockCloser([" \t :IF cond;", ""], 0, 1)!.insertText)
                .to.equal("\n \t :ENDIF;");
        });
    });

    describe("nested-balance scan (closer-already-exists)", () => {
        it("does not insert when a matching :ENDIF; already exists below", () => {
            const lines = [
                ":IF cond;",
                "", // cursor here
                ":ENDIF;",
            ];
            expect(decideBlockCloser(lines, 0, 1)).to.be.null;
        });

        it("pins limitation: a downstream :ENDIF satisfies the new opener even when the user intended nesting", () => {
            // Documented limitation: when the user types `:IF inner;` ABOVE
            // an existing `:ENDIF`, the balance scan sees a closer at depth
            // 1 and concludes "already closed" — so no insertion. The user
            // has to add the inner :ENDIF themselves. We accept this trade
            // because the alternative (always inserting) would produce
            // unwanted doubles in the much more common case where the user
            // is editing within a block that's already balanced.
            const lines = [
                ":IF inner;",  // newly typed
                "",            // cursor
                ":ENDIF;",     // existing closer; will be claimed
            ];
            expect(decideBlockCloser(lines, 0, 1)).to.be.null;
        });

        it("inserts when nested :IF blocks below leave the outer unmatched", () => {
            // Outer :IF has not been closed yet. The inner :IF/:ENDIF
            // pair is fully balanced, so the existing :ENDIF doesn't
            // count toward the outer one.
            const lines = [
                ":IF outer;",      // just typed
                "",                // cursor
                "\t:IF inner;",
                "\t:ENDIF;",
                // no outer :ENDIF anywhere
            ];
            const decision = decideBlockCloser(lines, 0, 1);
            expect(decision, "expected to insert outer :ENDIF").to.not.be.null;
            expect(decision!.insertText).to.equal("\n:ENDIF;");
        });

        it("treats different families independently — :ENDWHILE; does not satisfy :IF;", () => {
            const lines = [
                ":IF cond;",
                "",
                ":ENDWHILE;", // wrong family
            ];
            const decision = decideBlockCloser(lines, 0, 1);
            expect(decision, "should still insert :ENDIF").to.not.be.null;
            expect(decision!.family.closerText).to.equal(":ENDIF;");
        });

        it("balances correctly across multiple nested :IF blocks", () => {
            const lines = [
                ":IF a;",            // 0  just typed
                "",                  // 1  cursor
                "\t:IF b;",          // 2
                "\t\t:IF c;",        // 3
                "\t\t:ENDIF;",       // 4  closes c
                "\t:ENDIF;",         // 5  closes b
                // no closer for a
            ];
            const decision = decideBlockCloser(lines, 0, 1);
            expect(decision, "outer :IF still needs a closer").to.not.be.null;
        });

        it("respects case-insensitive matching in the balance scan", () => {
            const lines = [
                ":IF cond;",
                "",
                ":endif;", // lowercase closer
            ];
            expect(decideBlockCloser(lines, 0, 1)).to.be.null;
        });
    });

    describe("less-obvious opener shapes", () => {
        it("recognises an opener with a trailing inline comment", () => {
            // `^\s*:IF\b.*;\s*$` — `.*;` greedily eats the inline comment
            // ending in its own `;`. The line still terminates with `;`.
            const decision = decideBlockCloser([":IF cond; /* note ;", ""], 0, 1);
            expect(decision, "trailing-comment opener should still match").to.not.be.null;
            expect(decision!.family.closerText).to.equal(":ENDIF;");
        });

        it("accepts an opener with trailing whitespace", () => {
            const decision = decideBlockCloser([":IF cond;   ", ""], 0, 1);
            expect(decision).to.not.be.null;
            expect(decision!.family.closerText).to.equal(":ENDIF;");
        });
    });

    describe("cursor position guard", () => {
        it("returns null if the cursor is not on the line immediately after the opener", () => {
            const lines = [":IF cond;", "", ""];
            expect(decideBlockCloser(lines, 0, 2), "cursor two lines down").to.be.null;
            expect(decideBlockCloser(lines, 0, 0), "cursor on opener line itself").to.be.null;
        });

        it("returns null when openerLineNumber is out of range", () => {
            expect(decideBlockCloser([":IF cond;"], -1, 0)).to.be.null;
            expect(decideBlockCloser([":IF cond;"], 99, 100)).to.be.null;
        });
    });

    describe("string and comment awareness", () => {
        it("does not insert when the opener line is itself inside a multi-line string", () => {
            // Line 0 opens a string with `"`. Line 1 is `:IF cond;` but
            // sits inside that string — it's literal text, not a header.
            const lines = [
                'x := "',
                ':IF cond;',
                '";',
            ];
            expect(decideBlockCloser(lines, 1, 2)).to.be.null;
        });

        it("does not insert when the opener line is inside a block comment", () => {
            const lines = [
                "/* commentary that wraps:",
                ":IF cond;",
                "across lines ;",
            ];
            expect(decideBlockCloser(lines, 1, 2)).to.be.null;
        });

        it("ignores a :ENDIF; that appears inside a string when balancing", () => {
            // The visible :ENDIF; on line 2 is inside the string. The
            // outer :IF still needs a real closer, so the function
            // should insert one.
            const lines = [
                ":IF cond;",      // 0  just typed
                "",               // 1  cursor
                'x := "',         // 2  opens string
                ":ENDIF;",        // 3  text inside the string
                '";',             // 4  closes string
                // no real :ENDIF anywhere
            ];
            const decision = decideBlockCloser(lines, 0, 1);
            expect(decision, "string-trapped :ENDIF must not satisfy outer opener").to.not.be.null;
            expect(decision!.family.closerText).to.equal(":ENDIF;");
        });

        it("ignores a :ENDIF; that appears inside a block comment when balancing", () => {
            const lines = [
                ":IF cond;",
                "",
                "/* documenting:",
                ":ENDIF;",
                "purposes ;",
            ];
            const decision = decideBlockCloser(lines, 0, 1);
            expect(decision, "comment-trapped :ENDIF must not satisfy outer opener").to.not.be.null;
        });

        it("recognises a real :ENDIF on a line that contains a trailing comment", () => {
            // The closer is real; the comment after `;` is decoration.
            // The line STARTS in code, so it counts.
            const lines = [
                ":IF cond;",
                "",
                ":ENDIF; /* end of guard ;",
            ];
            expect(decideBlockCloser(lines, 0, 1)).to.be.null;
        });

    });

    describe("classifyLineStarts helper (mini-lexer)", () => {
        it("flags every line as code when no multi-line strings/comments are used", () => {
            const mask = classifyLineStarts([
                ":PROCEDURE Foo;",
                ":DECLARE x;",
                ":ENDPROC;",
            ]);
            expect(mask).to.deep.equal([true, true, true]);
        });

        it("marks lines inside a multi-line double-quoted string as non-code", () => {
            const mask = classifyLineStarts([
                'x := "',  // 0 opens string in code
                "inside",  // 1 inside string
                'closing";', // 2 closes string mid-line
                "after;",  // 3 back to code
            ]);
            expect(mask).to.deep.equal([true, false, false, true]);
        });

        it("marks lines inside a block comment as non-code", () => {
            const mask = classifyLineStarts([
                "/* multi",
                "line",
                "comment ;",
                "after;",
            ]);
            expect(mask).to.deep.equal([true, false, false, true]);
        });

        it("handles single-quoted strings the same way", () => {
            const mask = classifyLineStarts([
                "x := '",
                "still in string",
                "';",
                "after;",
            ]);
            expect(mask).to.deep.equal([true, false, false, true]);
        });

        it("does not start a comment when /* appears inside a string", () => {
            // `"/* not a comment "` — the /* is literal text in the
            // string. After the closing quote, state must still be code.
            const mask = classifyLineStarts([
                'x := "/* not a comment ";',
                ":ENDIF;",
            ]);
            expect(mask).to.deep.equal([true, true]);
        });
    });

    describe("families registry sanity", () => {
        it("includes every documented family", () => {
            const closers = FAMILIES.map(f => f.closerText).sort();
            expect(closers).to.deep.equal(
                [
                    ":ENDCASE;",
                    ":ENDCLASS;",
                    ":ENDIF;",
                    ":ENDPROC;",
                    ":ENDREGION;",
                    ":ENDTRY;",
                    ":ENDWHILE;",
                    ":NEXT;",
                ].sort()
            );
        });
    });

    describe("helpers", () => {
        // familyForOpener case-insensitivity and rejection of non-openers
        // is already exercised through decideBlockCloser above. Only the
        // pure leadingIndent helper has behavior not covered there.
        it("leadingIndent extracts tabs and spaces verbatim", () => {
            expect(leadingIndent("\t\t:IF cond;")).to.equal("\t\t");
            expect(leadingIndent("    :IF cond;")).to.equal("    ");
            expect(leadingIndent(":IF cond;")).to.equal("");
            expect(leadingIndent("")).to.equal("");
        });
    });
});
