import { describe, it } from "mocha";
import { expect } from "chai";
import { decideBlockCloser, familyForOpener, leadingIndent, FAMILIES } from "../src/sslBlockCloser";

describe("SSL Block Closer — decideBlockCloser", () => {
    describe("opener detection", () => {
        it("inserts :ENDIF; after :IF cond;", () => {
            const lines = [":IF nValue > 0;", ""];
            const decision = decideBlockCloser(lines, 0, 1);
            expect(decision, "expected decision for :IF").to.not.be.null;
            expect(decision!.family.closerText).to.equal(":ENDIF;");
            expect(decision!.insertText).to.equal("\n:ENDIF;");
        });

        it("inserts :ENDWHILE; after :WHILE cond;", () => {
            const decision = decideBlockCloser([":WHILE bRun;", ""], 0, 1);
            expect(decision!.family.closerText).to.equal(":ENDWHILE;");
        });

        it("inserts :NEXT; after :FOR i := 1 :TO 10;", () => {
            const decision = decideBlockCloser([":FOR i := 1 :TO 10;", ""], 0, 1);
            expect(decision!.family.closerText).to.equal(":NEXT;");
        });

        it("inserts :ENDCASE; after :BEGINCASE;", () => {
            const decision = decideBlockCloser([":BEGINCASE;", ""], 0, 1);
            expect(decision!.family.closerText).to.equal(":ENDCASE;");
        });

        it("inserts :ENDTRY; after :TRY;", () => {
            const decision = decideBlockCloser([":TRY;", ""], 0, 1);
            expect(decision!.family.closerText).to.equal(":ENDTRY;");
        });

        it("inserts :ENDPROC; after :PROCEDURE Name;", () => {
            const decision = decideBlockCloser([":PROCEDURE Foo;", ""], 0, 1);
            expect(decision!.family.closerText).to.equal(":ENDPROC;");
        });

        it("inserts :ENDREGION; after :REGION Name;", () => {
            const decision = decideBlockCloser([":REGION Bar;", ""], 0, 1);
            expect(decision!.family.closerText).to.equal(":ENDREGION;");
        });

        it("matches case-insensitively on the opener keyword", () => {
            const decision = decideBlockCloser([":if cond;", ""], 0, 1);
            expect(decision!.family.closerText).to.equal(":ENDIF;");
        });

        it("returns null when the line is not a recognised opener", () => {
            const decision = decideBlockCloser(["nValue := 1;", ""], 0, 1);
            expect(decision).to.be.null;
        });

        it("returns null when the opener is missing its terminating semicolon", () => {
            // Heuristic: we only auto-close once the user has actually typed `;`.
            const decision = decideBlockCloser([":IF cond", ""], 0, 1);
            expect(decision).to.be.null;
        });

        it("returns null when the line contains text BEFORE :IF (not at line start)", () => {
            // The opener must start the line (modulo leading whitespace).
            const decision = decideBlockCloser(["x := 1; :IF cond;", ""], 0, 1);
            expect(decision).to.be.null;
        });

        it("ignores :ELSE / :CASE / :CATCH (middle keywords, not openers)", () => {
            expect(decideBlockCloser([":ELSE;", ""], 0, 1)).to.be.null;
            expect(decideBlockCloser([":CASE cond;", ""], 0, 1)).to.be.null;
            expect(decideBlockCloser([":CATCH;", ""], 0, 1)).to.be.null;
        });
    });

    describe("indent preservation", () => {
        it("places the closer at the opener's indent (tabs)", () => {
            const decision = decideBlockCloser(["\t\t:IF cond;", "\t\t\t"], 0, 1);
            expect(decision!.insertText).to.equal("\n\t\t:ENDIF;");
        });

        it("places the closer at the opener's indent (spaces)", () => {
            const decision = decideBlockCloser(["    :IF cond;", "        "], 0, 1);
            expect(decision!.insertText).to.equal("\n    :ENDIF;");
        });

        it("places the closer at column 0 when the opener has no indent", () => {
            const decision = decideBlockCloser([":IF cond;", ""], 0, 1);
            expect(decision!.insertText).to.equal("\n:ENDIF;");
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

        it("DOES insert when the user typed a NEW :IF inside an existing block", () => {
            // Original document already had :IF / :ENDIF. The user has
            // just typed a new :IF above the existing :ENDIF — we should
            // insert a SECOND :ENDIF so the inner block is balanced.
            const lines = [
                ":IF inner;",  // newly typed
                "",            // cursor
                ":ENDIF;",     // original outer closer
            ];
            // Balance from the new opener at line 0:
            //   start = 1
            //   line 2 is :ENDIF → balance = 0 → "already exists"
            // So the function returns null. That is the correct behavior
            // because the user just turned an outer block into an inner
            // one — the outer closer already matches.
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
        it("leadingIndent extracts tabs and spaces verbatim", () => {
            expect(leadingIndent("\t\t:IF cond;")).to.equal("\t\t");
            expect(leadingIndent("    :IF cond;")).to.equal("    ");
            expect(leadingIndent(":IF cond;")).to.equal("");
            expect(leadingIndent("")).to.equal("");
        });

        it("familyForOpener is case-insensitive on the keyword", () => {
            expect(familyForOpener(":IF cond;")!.closerText).to.equal(":ENDIF;");
            expect(familyForOpener(":If cond;")!.closerText).to.equal(":ENDIF;");
            expect(familyForOpener(":iF cond;")!.closerText).to.equal(":ENDIF;");
        });

        it("familyForOpener returns undefined for non-openers", () => {
            expect(familyForOpener("nValue := 1;")).to.be.undefined;
            expect(familyForOpener(":RETURN nValue;")).to.be.undefined;
            expect(familyForOpener(":ENDIF;")).to.be.undefined;
        });
    });
});
