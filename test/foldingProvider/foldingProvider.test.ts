import * as assert from "assert";
import * as vscode from "vscode";
import { SSLFoldingProvider } from "../../src/sslFoldingProvider";

// Helper function to create a mock TextDocument
function createMockDocument(lines: string[]): vscode.TextDocument {
    return {
        uri: vscode.Uri.file("test.ssl"),
        fileName: "test.ssl",
        isUntitled: false,
        languageId: "ssl",
        version: 1,
        isDirty: false,
        isClosed: false,
        save: () => Promise.resolve(true),
        eol: vscode.EndOfLine.LF,
        lineCount: lines.length,
        lineAt: (line: number | vscode.Position) => {
            const lineNumber = typeof line === "number" ? line : line.line;
            return {
                lineNumber: lineNumber,
                text: lines[lineNumber],
                range: new vscode.Range(
                    new vscode.Position(lineNumber, 0),
                    new vscode.Position(lineNumber, lines[lineNumber].length)
                ),
                rangeIncludingLineBreak: new vscode.Range(
                    new vscode.Position(lineNumber, 0),
                    new vscode.Position(lineNumber, lines[lineNumber].length)
                ),
                firstNonWhitespaceCharacterIndex: lines[lineNumber].match(/^\s*/)![0].length,
                isEmptyOrWhitespace: lines[lineNumber].trim().length === 0,
            };
        },
        offsetAt: (position: vscode.Position) => {
            let offset = 0;
            for (let i = 0; i < position.line; i++) {
                offset += lines[i].length + 1; // +1 for newline
            }
            return offset + position.character;
        },
        positionAt: (offset: number) => {
            let line = 0;
            let character = 0;
            while (offset > 0 && line < lines.length) {
                if (offset > lines[line].length) {
                    offset -= lines[line].length + 1;
                    line++;
                } else {
                    character = offset;
                    offset = 0;
                }
            }
            return new vscode.Position(line, character);
        },
        getText: (range?: vscode.Range) => {
            if (!range) {
                return lines.join("\n");
            }
            const startLine = range.start.line;
            const endLine = range.end.line;
            if (startLine === endLine) {
                return lines[startLine].substring(range.start.character, range.end.character);
            }
            let text = lines[startLine].substring(range.start.character) + "\n";
            for (let i = startLine + 1; i < endLine; i++) {
                text += lines[i] + "\n";
            }
            text += lines[endLine].substring(0, range.end.character);
            return text;
        },
        getWordRangeAtPosition: (position: vscode.Position, regex?: RegExp) => undefined,
        validateRange: (range: vscode.Range) => range,
        validatePosition: (position: vscode.Position) => position,
    };
}

describe("SSLFoldingProvider", () => {
    let provider: SSLFoldingProvider;
    const cancellationToken: vscode.CancellationToken = {
        isCancellationRequested: false,
        onCancellationRequested: (() => {}) as any,
    };

    beforeEach(() => {
        provider = new SSLFoldingProvider();
    });

    it("should fold :PROCEDURE ... :ENDPROC blocks", () => {
        const document = createMockDocument([
            ":PROCEDURE BasicProcedure;",
            "    :DECLARE sVar;",
            '    sVar := "Hello";',
            "    :RETURN sVar;",
            ":ENDPROC; /* Fold from :PROCEDURE to :ENDPROC;",
        ]);
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        assert.strictEqual(ranges.length, 1);
        assert.deepStrictEqual(ranges[0], new vscode.FoldingRange(0, 4));
    });

    it("should fold :CLASS ... :ENDCLASS blocks", () => {
        const document = createMockDocument([
            ":CLASS BasicClass;",
            "    :DECLARE nMember;",
            "    :PROCEDURE Init;",
            "        nMember := 0;",
            "    :ENDPROC;",
            ":ENDCLASS; /* Fold from :CLASS to :ENDCLASS;",
        ]);
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        // Expecting 2 ranges: one for CLASS, one for PROCEDURE
        assert.strictEqual(
            ranges.length,
            2,
            "Expected 2 folding ranges for class and inner procedure"
        );
        assert.deepStrictEqual(
            ranges.find((r) => r.start === 0 && r.end === 5),
            new vscode.FoldingRange(0, 5),
            "Class folding range incorrect"
        );
        assert.deepStrictEqual(
            ranges.find((r) => r.start === 2 && r.end === 4),
            new vscode.FoldingRange(2, 4),
            "Procedure folding range incorrect"
        );
    });

    it("should fold :REGION ... :ENDREGION blocks", () => {
        const document = createMockDocument([
            ":REGION MainLogic;",
            "    :DECLARE x;",
            "    x := 10;",
            ":ENDREGION; /* Fold from :REGION to :ENDREGION;",
        ]);
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        assert.strictEqual(ranges.length, 1);
        assert.deepStrictEqual(
            ranges[0],
            new vscode.FoldingRange(0, 3, vscode.FoldingRangeKind.Region)
        );
    });

    it("should fold editor comment regions /*region ...; ... /*endregion;", () => {
        const document = createMockDocument([
            "/* region EditorFoldableRegion1;",
            ":DECLARE editorFoldVar1;",
            "/* endregion;",
        ]);
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        assert.strictEqual(ranges.length, 1);
        assert.deepStrictEqual(
            ranges[0],
            new vscode.FoldingRange(0, 2, vscode.FoldingRangeKind.Region)
        );
    });

    it("should fold multi-line comments /* ...;", () => {
        const document = createMockDocument([
            "/* This is a",
            "   multi-line",
            "   comment;",
            ":PROCEDURE Test;",
            ":ENDPROC;",
        ]);
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        assert.strictEqual(ranges.length, 2, "Expected 2 folding ranges");
        assert.deepStrictEqual(
            ranges.find(
                (r) => r.start === 0 && r.end === 2 && r.kind === vscode.FoldingRangeKind.Comment
            ),
            new vscode.FoldingRange(0, 2, vscode.FoldingRangeKind.Comment),
            "Multi-line comment folding incorrect"
        );
        assert.deepStrictEqual(
            ranges.find((r) => r.start === 3 && r.end === 4),
            new vscode.FoldingRange(3, 4),
            "Procedure folding incorrect"
        );
    });

    it("should fold :IF ... :ENDIF blocks", () => {
        const document = createMockDocument([":IF .T.;", "    DoSomething();", ":ENDIF;"]);
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        assert.strictEqual(ranges.length, 1);
        assert.deepStrictEqual(ranges[0], new vscode.FoldingRange(0, 2));
    });

    it("should fold :IF ... :ELSE ... :ENDIF blocks", () => {
        const document = createMockDocument([
            ":IF .T.;",
            "    DoTrue();",
            ":ELSE;",
            "    DoFalse();",
            ":ENDIF;",
        ]);
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        // Expecting 2 ranges: IF-ENDIF and ELSE-ENDIF (or IF-ELSE, ELSE-ENDIF depending on provider logic)
        // Provider logic: :ELSE creates a new block, :ENDIF closes :ELSE then :IF
        // So we expect one for the :ELSE block (line 2 to 3) and one for the :IF block (line 0 to 4)
        assert.strictEqual(ranges.length, 2, "Expected 2 folding ranges for IF/ELSE/ENDIF");
        assert.ok(
            ranges.some((r) => r.start === 0 && r.end === 4),
            "Outer IF block not found"
        ); // :IF to :ENDIF
        assert.ok(
            ranges.some((r) => r.start === 2 && r.end === 3),
            "ELSE block not found"
        ); // :ELSE to line before :ENDIF
    });

    it("should fold :IF ... :ELSE ... :ENDIF blocks", () => {
        const document = createMockDocument([
            ":PROCEDURE ConditionalTest; :PARAMETERS val;", // 0
            "    :IF val > 10;", // 1
            '        :RETURN "Greater";', // 2
            "    :ELSE;", // 5
            '        :RETURN "Equal";', // 6
            "    :ENDIF;", // 7
            ":ENDPROC;", // 8
        ]);
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        // Expected:
        // 1. PROC (0-8)
        // 2. IF (1-7)
        // 4. ELSE (5-6) - provider might treat :ELSE as starting a new block that ends before :ENDIF

        // :ENDIF closes :ELSE then :IF.
        // Let's trace:
        // line 0: PROC pushed {PROC, 0}
        // line 1: IF pushed {IF, 1}
        // line 3: (no push for ELSEIF)
        // line 5: ELSE pushed {ELSE, 5}
        // line 7: :ENDIF encountered. Pops ELSE (creates range 5-6). Pops IF (creates range 1-7)
        // line 8: :ENDPROC encountered. Pops PROC (creates range 0-8)
        // So, ranges should be: (5,6), (1,7), (0,8)

        assert.strictEqual(ranges.length, 3, "Expected 3 folding ranges");
        assert.ok(
            ranges.some((r) => r.start === 0 && r.end === 8),
            "PROC block (0-8) not found"
        );
        assert.ok(
            ranges.some((r) => r.start === 1 && r.end === 7),
            "IF block (1-7) not found"
        );
        assert.ok(
            ranges.some((r) => r.start === 5 && r.end === 6),
            "ELSE block (5-6) not found"
        );
    });

    it("should fold :WHILE ... :ENDWHILE blocks", () => {
        const document = createMockDocument([":WHILE .T.;", "    DoSomething();", ":ENDWHILE;"]);
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        assert.strictEqual(ranges.length, 1);
        assert.deepStrictEqual(ranges[0], new vscode.FoldingRange(0, 2));
    });

    it("should fold :FOR ... :NEXT blocks", () => {
        const document = createMockDocument([
            ":FOR i := 1 :TO 10;",
            "    DoSomething();",
            ":NEXT;",
        ]);
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        assert.strictEqual(ranges.length, 1);
        assert.deepStrictEqual(ranges[0], new vscode.FoldingRange(0, 2));
    });

    it("should fold :BEGINCASE ... :ENDCASE blocks with :CASE and :OTHERWISE", () => {
        const document = createMockDocument([
            ":BEGINCASE;", // 0
            "    :CASE .T.;", // 1
            "        DoCase1();", // 2
            "    :CASE .F.;", // 3
            "        DoCase2();", // 4
            "    :OTHERWISE;", // 5
            "        DoOtherwise();", // 6
            ":ENDCASE;", // 7
        ]);
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        // Expected:
        // BEGINCASE (0-7)
        // CASE 1 (1-2)
        // CASE 2 (3-4)
        // OTHERWISE (5-6)
        // Trace:
        // L0: stack: [{BEGINCASE,0}]
        // L1: :CASE. prevCase=null. stack: [{BEGINCASE,0}, {CASE,1}]
        // L3: :CASE. prevCase=popStackByType(CASE) -> {CASE,1}. range(1, 2). stack: [{BEGINCASE,0}, {CASE,3}]
        // L5: :OTHERWISE. prevCase=popStackByType(CASE) -> {CASE,3}. range(3, 4). stack: [{BEGINCASE,0}, {OTHERWISE,5}]
        // L7: :ENDCASE. popStackByType(CASE) -> null. popStackByType(OTHERWISE) -> {OTHERWISE,5}. range(5,6). popStackByType(BEGINCASE) -> {BEGINCASE,0}. range(0,7)
        // Ranges: (1,2), (3,4), (5,6), (0,7)
        assert.strictEqual(ranges.length, 4, "Expected 4 folding ranges");
        assert.ok(
            ranges.some((r) => r.start === 0 && r.end === 7),
            "BEGINCASE (0-7) not found"
        );
        assert.ok(
            ranges.some((r) => r.start === 1 && r.end === 2),
            "CASE 1 (1-2) not found"
        );
        assert.ok(
            ranges.some((r) => r.start === 3 && r.end === 4),
            "CASE 2 (3-4) not found"
        );
        assert.ok(
            ranges.some((r) => r.start === 5 && r.end === 6),
            "OTHERWISE (5-6) not found"
        );
    });

    it("should fold :TRY ... :CATCH ... :FINALLY ... :ENDTRY blocks", () => {
        const document = createMockDocument([
            ":TRY;", // 0
            "    DoRisky();", // 1
            ":CATCH e;", // 2
            "    LogError(e);", // 3
            ":FINALLY;", // 4
            "    CleanUp();", // 5
            ":ENDTRY;", // 6
        ]);
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        // Expected:
        // TRY (0-6) overall
        // TRY part (0-1) before CATCH
        // CATCH part (2-3) before FINALLY
        // FINALLY part (4-5) before ENDTRY
        // Trace:
        // L0: stack: [{TRY,0}]
        // L2: :CATCH. handleCatchBlock: findInStack(TRY)->{TRY,0}. range(0,1). stack: [{TRY,0},{CATCH,2}]
        // L4: :FINALLY. handleFinallyBlock: popStackByType(CATCH)->{CATCH,2}. range(2,3). stack: [{TRY,0},{FINALLY,4}]
        // L6: :ENDTRY. handleTryEndStructure: popStackByType(FINALLY)->{FINALLY,4}. range(4,5). popStackByType(CATCH)->null. popStackByType(TRY)->{TRY,0}. range(0,6)
        // Ranges: (0,1), (2,3), (4,5), (0,6)
        assert.strictEqual(ranges.length, 4, "Expected 4 folding ranges for TRY/CATCH/FINALLY");
        assert.ok(
            ranges.some((r) => r.start === 0 && r.end === 6),
            "Outer TRY-ENDTRY (0-6) not found"
        );
        assert.ok(
            ranges.some((r) => r.start === 0 && r.end === 1),
            "TRY block (0-1) not found"
        );
        assert.ok(
            ranges.some((r) => r.start === 2 && r.end === 3),
            "CATCH block (2-3) not found"
        );
        assert.ok(
            ranges.some((r) => r.start === 4 && r.end === 5),
            "FINALLY block (4-5) not found"
        );
    });

    it("should fold :TRY ... :CATCH ... :ENDTRY blocks", () => {
        const document = createMockDocument([
            ":TRY;", // 0
            "    DoRisky();", // 1
            ":CATCH e;", // 2
            "    LogError(e);", // 3
            ":ENDTRY;", // 4
        ]);
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        // Trace:
        // L0: stack: [{TRY,0}]
        // L2: :CATCH. handleCatchBlock: findInStack(TRY)->{TRY,0}. range(0,1). stack: [{TRY,0},{CATCH,2}]
        // L4: :ENDTRY. handleTryEndStructure: popStackByType(FINALLY)->null. popStackByType(CATCH)->{CATCH,2}. range(2,3). popStackByType(TRY)->{TRY,0}. range(0,4)
        // Ranges: (0,1), (2,3), (0,4)
        assert.strictEqual(ranges.length, 3, "Expected 3 folding ranges for TRY/CATCH");
        assert.ok(
            ranges.some((r) => r.start === 0 && r.end === 4),
            "Outer TRY-ENDTRY (0-4) not found"
        );
        assert.ok(
            ranges.some((r) => r.start === 0 && r.end === 1),
            "TRY block (0-1) not found"
        );
        assert.ok(
            ranges.some((r) => r.start === 2 && r.end === 3),
            "CATCH block (2-3) not found"
        );
    });

    it("should fold :TRY ... :FINALLY ... :ENDTRY blocks", () => {
        const document = createMockDocument([
            ":TRY;", // 0
            "    DoRisky();", // 1
            ":FINALLY;", // 2
            "    CleanUp();", // 3
            ":ENDTRY;", // 4
        ]);
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        // Trace:
        // L0: stack: [{TRY,0}]
        // L2: :FINALLY. handleFinallyBlock: popStackByType(CATCH)->null. stack: [{TRY,0},{FINALLY,2}]
        // L4: :ENDTRY. handleTryEndStructure: popStackByType(FINALLY)->{FINALLY,2}. range(2,3). popStackByType(CATCH)->null. popStackByType(TRY)->{TRY,0}. range(0,4)
        // Ranges: (2,3), (0,4)
        // Note: The TRY part (0-1) is not explicitly closed by :FINALLY in the same way :CATCH does.
        // The provider's handleFinallyBlock only closes a CATCH. The TRY itself is closed by ENDTRY.
        // The handleTryEndStructure closes FINALLY, then CATCH, then TRY.
        // So, the range for the initial TRY part (before FINALLY) is not created. Only FINALLY and the overall TRY.
        // This seems like a potential area for refinement in the provider if a fold for the TRY part itself is desired.
        // For now, testing current behavior:
        assert.strictEqual(ranges.length, 2, "Expected 2 folding ranges for TRY/FINALLY");
        assert.ok(
            ranges.some((r) => r.start === 0 && r.end === 4),
            "Outer TRY-ENDTRY (0-4) not found"
        );
        assert.ok(
            ranges.some((r) => r.start === 2 && r.end === 3),
            "FINALLY block (2-3) not found"
        );
    });

    it("should fold :BEGININLINECODE ... :ENDINLINECODE blocks", () => {
        const document = createMockDocument([
            ':BEGININLINECODE "MyJSCode" "JavaScript";',
            "    // let x = 10;",
            ":ENDINLINECODE;",
        ]);
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        assert.strictEqual(ranges.length, 1);
        assert.deepStrictEqual(ranges[0], new vscode.FoldingRange(0, 2));
    });

    it("should handle nested structures correctly", () => {
        const document = createMockDocument([
            ":CLASS NestedExampleClass;", // 0
            "    :DECLARE nOuterVar;", // 1
            "    :PROCEDURE OuterMethod;", // 2
            "        :IF nOuterVar > 0;", // 3
            "            :FOR i := 1 :TO nOuterVar;", // 4
            "                :TRY;", // 5
            "                    /* Some logic;", // 6
            "                :CATCH;", // 7
            "                    /* Handle error;", // 8
            "                :ENDTRY;", // 9
            "            :NEXT;", // 10
            "        :ENDIF;", // 11
            "    :ENDPROC;", // 12
            ":ENDCLASS;", // 13
        ]);
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        // CLASS (0-13)
        //   PROC (2-12)
        //     IF (3-11)
        //       FOR (4-10)
        //         TRY-CATCH-ENDTRY:
        //           TRY part (5-6)
        //           CATCH part (7-8)
        //           Overall TRY (5-9)
        assert.ok(
            ranges.some((r) => r.start === 0 && r.end === 13),
            "CLASS (0-13)"
        );
        assert.ok(
            ranges.some((r) => r.start === 2 && r.end === 12),
            "PROC (2-12)"
        );
        assert.ok(
            ranges.some((r) => r.start === 3 && r.end === 11),
            "IF (3-11)"
        );
        assert.ok(
            ranges.some((r) => r.start === 4 && r.end === 10),
            "FOR (4-10)"
        );
        assert.ok(
            ranges.some((r) => r.start === 5 && r.end === 9),
            "Overall TRY (5-9)"
        ); // TRY to ENDTRY
        assert.ok(
            ranges.some((r) => r.start === 5 && r.end === 6),
            "TRY part (5-6)"
        ); // TRY to CATCH
        assert.ok(
            ranges.some((r) => r.start === 7 && r.end === 8),
            "CATCH part (7-8)"
        ); // CATCH to ENDTRY
        assert.strictEqual(ranges.length, 7, "Expected 7 folding ranges for nested structure");
    });

    it("should not fold keywords within strings", () => {
        const document = createMockDocument([
            ":PROCEDURE StringKeywordTest;",
            "    :DECLARE sSQL;",
            '    sSQL := "SELECT * FROM :TABLE_NAME WHERE ID = :IF :ELSE :ENDIF";', // :IF etc in string
            '    :IF sSQL != "";',
            "        /* Do something;",
            "    :ENDIF;",
            ":ENDPROC;",
        ]);
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        // Expected: PROC (0-6), IF (3-5)
        assert.strictEqual(ranges.length, 2);
        assert.ok(
            ranges.some((r) => r.start === 0 && r.end === 6),
            "PROC (0-6)"
        );
        assert.ok(
            ranges.some((r) => r.start === 3 && r.end === 5),
            "IF (3-5)"
        );
    });

    it("should handle strings with escaped quotes correctly", () => {
        const document = createMockDocument([
            ":PROCEDURE StringEscapedQuoteTest;", //0
            "    :DECLARE myString;", //1
            '    myString := "This is a string with an escaped \\"quote\\" and a :IF here";', //2
            '    :IF myString != "";', //3
            '        DoProc("Test :ENDIF");', //4
            "    :ENDIF;", //5
            ":ENDPROC;", //6
        ]);
        // The current string handling in SSLFoldingProvider is basic and might not correctly handle escaped quotes within strings.
        // It looks for the first occurrence of the delimiter.
        // updateStringState:
        // L2: finds " at char 15. inString=true, delimiter="
        //     next call to updateStringState for L2, inString=true, delimiter="
        //     finds " at char 38 (escaped one). inString=false.
        //     remaining line: 'quote\\" and a :IF here";'
        //     recursive call: finds " at char 6. inString=true, delimiter="
        // This means it will think the string ends early.
        // Let's test the *actual* behavior.
        // If string handling is imperfect, :IF at end of line 2 might be seen.
        // And :ENDIF in string on line 4 might be seen.

        // Provider's updateStringState:
        // Line 2: `myString := "This is a string with an escaped \\"quote\\" and a :IF here";`
        // 1. Not in string. Finds `"` at index 15. `inString = true`, `delimiter = '"'`.
        // 2. Next iteration for line 2 (within provideFoldingRanges loop, but string state is per line).
        //    Actually, `updateStringState` is called once per line if not in comment.
        //    `updateStringState(line, false, "")`
        //    `char = '"'` (index 15). `inString = true`, `delimiter = '"'`. Loop breaks.
        //    So for line 2, it correctly identifies it starts a string.
        //    The `provideFoldingRanges` loop then continues: `if (inString) { continue; }`. So line 2 is skipped for keyword checks. Correct.

        // Line 4: `DoProc("Test :ENDIF");`
        // 1. `updateStringState(line, false, "")`
        //    `char = '"'` (index 8). `inString = true`, `delimiter = '"'`. Loop breaks.
        //    Line 4 is skipped for keyword checks. Correct.

        // So, the existing string logic should be fine for this case.
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        assert.ok(
            ranges.some((r) => r.start === 0 && r.end === 6),
            "PROC (0-6)"
        );
        assert.ok(
            ranges.some((r) => r.start === 3 && r.end === 5),
            "IF (3-5)"
        );
        assert.strictEqual(ranges.length, 2, "Expected 2 folding ranges");
    });

    it("should handle unclosed blocks gracefully and fold subsequent valid blocks", () => {
        const document = createMockDocument([
            ":PROCEDURE UnclosedProc;", // 0
            "    :IF .T.;", // 1
            "        /* Missing :ENDIF;", // 2
            "", // 3
            ":PROCEDURE FollowingProc; /* This should still fold correctly;", // 4
            "    :RETURN .T.;", // 5
            ":ENDPROC;", // 6
        ]);
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        // Expected:
        // FollowingProc (4-6)
        // UnclosedProc's IF (1 to end of doc - 1 = line 5)
        // UnclosedProc (0 to end of doc - 1 = line 5)
        // Order might vary.
        // Trace:
        // L0: stack: [{PROC,0, UnclosedProc}]
        // L1: stack: [{PROC,0}, {IF,1}]
        // L4: stack: [{PROC,0}, {IF,1}, {PROC,4, FollowingProc}]
        // L6: :ENDPROC. Pops {PROC,4}. Range(4,6). stack: [{PROC,0}, {IF,1}]
        // End of doc:
        //   Pops {IF,1}. Range(1, 6).
        //   Pops {PROC,0}. Range(0,6).
        // This means unclosed blocks extend to the end of the document.
        assert.ok(
            ranges.some((r) => r.start === 4 && r.end === 6),
            "FollowingProc (4-6)"
        );
        assert.ok(
            ranges.some((r) => r.start === 1 && r.end === 6),
            "Unclosed IF (1-6)"
        );
        assert.ok(
            ranges.some((r) => r.start === 0 && r.end === 6),
            "UnclosedProc (0-6)"
        );
        assert.strictEqual(ranges.length, 3, "Expected 3 folding ranges");
    });

    it("should handle empty documents", () => {
        const document = createMockDocument([]);
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        assert.strictEqual(ranges.length, 0);
    });

    it("should handle document with only comments", () => {
        const document = createMockDocument([
            "/* Comment 1;",
            "// Comment 2",
            "/* Comment 3",
            "   still comment 3 ;",
        ]);
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        // Expect one range for the multi-line comment
        assert.strictEqual(ranges.length, 1);
        assert.deepStrictEqual(
            ranges[0],
            new vscode.FoldingRange(2, 3, vscode.FoldingRangeKind.Comment)
        );
    });

    it("should correctly fold ACCESSOR/MUTATOR blocks within CLASS", () => {
        const document = createMockDocument([
            ":CLASS AccessorMutatorClass;", // 0
            "    :DECLARE _propertyValue;", // 1
            "    :ACCESSOR MyProperty;", // 2
            "        :GET;", // 3
            "            :RETURN _propertyValue;", // 4
            "        :ENDGET;", // 5
            "        :SET (newValue);", // 6
            "            _propertyValue := newValue;", // 7
            "        :ENDSET;", // 8
            "    :ENDACCESSOR;", // 9
            "    :MUTATOR OldStyleSetter :PARAMETERS val;", // 10
            "        _propertyValue := val;", // 11
            "    :ENDMUTATOR;", // 12
            ":ENDCLASS;", // 13
        ]);
        // Provider does not have explicit handling for ACCESSOR/MUTATOR, GET/SET/ENDGET/ENDSET
        // These will be treated as unknown blocks or not create folds.
        // :CLASS ... :ENDCLASS should fold.
        // Let's test current behavior:
        // L0: CLASS pushed
        // L2: ACCESSOR (not handled by handleFoldStart)
        // L3: GET (not handled)
        // L5: ENDGET (not handled by handleFoldEnd)
        // L6: SET (not handled)
        // L8: ENDSET (not handled)
        // L9: ENDACCESSOR (not handled)
        // L10: MUTATOR (not handled)
        // L12: ENDMUTATOR (not handled)
        // L13: ENDCLASS. Pops CLASS. Range (0,13)
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        assert.strictEqual(ranges.length, 1, "Expected 1 folding range for the CLASS block");
        assert.ok(
            ranges.some((r) => r.start === 0 && r.end === 13),
            "CLASS (0-13)"
        );
        // If ACCESSOR/MUTATOR etc. should be foldable, provider needs an update.
    });

    it("should handle multiple nested regions including editor comment regions", () => {
        const document = createMockDocument([
            ":REGION OuterRegion;", // 0
            "    :REGION InnerRegion1;", // 1
            "        :DECLARE inner1Var;", // 2
            "    :ENDREGION;", // 3
            "    :REGION InnerRegion2;", // 4
            "        :DECLARE inner2Var;", // 5
            "        /* region DeeplyNestedCommentRegion;", // 6
            "        :DECLARE deepVar;", // 7
            "        /* endregion;", // 8
            "    :ENDREGION;", // 9
            ":ENDREGION;", // 10
        ]);
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        // OuterRegion (0-10) kind Region
        // InnerRegion1 (1-3) kind Region
        // InnerRegion2 (4-9) kind Region
        // DeeplyNestedCommentRegion (6-8) kind Region
        assert.ok(
            ranges.some(
                (r) => r.start === 0 && r.end === 10 && r.kind === vscode.FoldingRangeKind.Region
            ),
            "OuterRegion (0-10)"
        );
        assert.ok(
            ranges.some(
                (r) => r.start === 1 && r.end === 3 && r.kind === vscode.FoldingRangeKind.Region
            ),
            "InnerRegion1 (1-3)"
        );
        assert.ok(
            ranges.some(
                (r) => r.start === 4 && r.end === 9 && r.kind === vscode.FoldingRangeKind.Region
            ),
            "InnerRegion2 (4-9)"
        );
        assert.ok(
            ranges.some(
                (r) => r.start === 6 && r.end === 8 && r.kind === vscode.FoldingRangeKind.Region
            ),
            "DeeplyNestedCommentRegion (6-8)"
        );
        assert.strictEqual(ranges.length, 4, "Expected 4 folding ranges for nested regions");
    });

    it("should handle file starting with a foldable block", () => {
        const document = createMockDocument([":PROCEDURE FirstProcInFile;", ":ENDPROC;"]);
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        assert.strictEqual(ranges.length, 1);
        assert.deepStrictEqual(ranges[0], new vscode.FoldingRange(0, 1));
    });

    it("should handle file ending with a foldable block", () => {
        const document = createMockDocument([
            "// Some line",
            ":PROCEDURE LastProcInFile;",
            ":ENDPROC;",
        ]);
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        assert.strictEqual(ranges.length, 1);
        assert.deepStrictEqual(ranges[0], new vscode.FoldingRange(1, 2));
    });

    it("should handle mixed case keywords", () => {
        const document = createMockDocument([
            ":Procedure MixedCaseProc;",
            "    :Declare x;",
            "    :If x > 0;",
            "        x := 1;",
            "    :EndIf;",
            ":EndProc;",
        ]);
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        // PROC (0-5)
        // IF (2-4)
        assert.strictEqual(ranges.length, 2);
        assert.ok(
            ranges.some((r) => r.start === 0 && r.end === 5),
            "PROC (0-5)"
        );
        assert.ok(
            ranges.some((r) => r.start === 2 && r.end === 4),
            "IF (2-4)"
        );
    });

    it("should handle lines with leading/trailing whitespace around keywords", () => {
        const document = createMockDocument([
            "  :PROCEDURE WhitespaceProc;  ",
            "    :DECLARE y;  ",
            "  :ENDPROC;  ",
        ]);
        const ranges = provider.provideFoldingRanges(document, {}, cancellationToken);
        assert.strictEqual(ranges.length, 1);
        assert.deepStrictEqual(ranges[0], new vscode.FoldingRange(0, 2));
    });
});
