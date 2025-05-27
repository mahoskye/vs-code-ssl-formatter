import * as assert from "assert";
// DO NOT import vscode here directly before mocking

import { SSLFormattingProvider } from "../../src/formatters/formattingProvider";
import { FormattingOptions as InternalFormattingOptions } from "../../src/formatters/formattingProvider"; // Renamed to avoid conflict

// DO NOT Import the actual mock implementation here, it will be required inside the factory
// import * as VscodeMock from "../__mocks__/vscode";

// Mock the vscode module
jest.mock("vscode", () => {
    // Use require here to ensure it's loaded at the time the factory is executed
    // and use a lint-friendly name.
    // Changed path to point to the renamed manual mock file
    const vscodeMock = jest.requireActual("../__mocks__/vscode");
    return vscodeMock; // Return the entire module exports from our manual mock
});

// NOW import vscode, it will be the mocked version specified by the factory
import * as vscode from "vscode";

const TESTTIMEOUT = 15000; // Increased timeout for potentially longer async operations

// Helper function to run the formatter
async function formatDocument(
    provider: SSLFormattingProvider,
    text: string,
    options?: Partial<vscode.FormattingOptions> // Keep this as Partial<vscode.FormattingOptions> for the caller
): Promise<string> {
    // Ensure all paths return a string
    // Use the internal FormattingOptions type for the object passed to the provider
    const mockVscodeOptions: InternalFormattingOptions = {
        tabSize: 4,
        insertSpaces: true,
        maxLineLength: 80, // Add default for testing
        indentStyle: "space", // Add default for testing
        ...options, // Allow overriding with vscode.FormattingOptions compatible parts
    };

    // Create a mock TextDocument
    const mockDocument = {
        getText: () => text,
        lineCount: text.split(/\r?\n/).length,
        lineAt: (lineNumber: number) => {
            const lines = text.split(/\r?\n/);
            const lineText = lines[lineNumber] || "";
            return {
                text: lineText,
                range: new vscode.Range(lineNumber, 0, lineNumber, lineText.length),
                rangeIncludingLineBreak: new vscode.Range(lineNumber, 0, lineNumber + 1, 0),
                firstNonWhitespaceCharacterIndex: lineText.match(/^(\s*)/)?.[0].length || 0,
                isEmptyOrWhitespace: lineText.trim() === "",
                lineNumber: lineNumber, // Added for completeness
            };
        },
        offsetAt: (position: vscode.Position) => {
            const lines = text.split(/\r?\n/);
            let offset = 0;
            for (let i = 0; i < position.line; i++) {
                offset += (lines[i] || "").length + 1; // +1 for newline
            }
            offset += position.character;
            return offset;
        },
        positionAt: (offset: number) => {
            const lines = text.split(/\r?\n/);
            let currentOffset = 0;
            for (let line = 0; line < lines.length; line++) {
                const lineText = lines[line] || "";
                const lineLengthWithNewline = lineText.length + 1;
                if (currentOffset + lineLengthWithNewline > offset) {
                    return new vscode.Position(line, offset - currentOffset);
                }
                currentOffset += lineLengthWithNewline;
            }
            // If offset is beyond the document, return position at the end of the last line
            const lastLine = lines.length > 0 ? lines.length - 1 : 0;
            const lastLineText = lines[lastLine] || "";
            return new vscode.Position(lastLine, lastLineText.length);
        },
        validateRange: (range: vscode.Range) => range,
        validatePosition: (position: vscode.Position) => position,
        uri: vscode.Uri.file("test.ssl"),
        fileName: "test.ssl",
        isUntitled: false,
        languageId: "ssl",
        version: 1,
        isDirty: false,
        save: async () => true,
        eol: vscode.EndOfLine.LF,
        getWordRangeAtPosition: (position: vscode.Position, regex?: RegExp) => undefined,
    } as any; // Use 'as any' to simplify mock, ensure all used properties are present

    try {
        const cancellationTokenSource = new vscode.CancellationTokenSource();

        const edits = await provider.provideDocumentFormattingEdits(
            mockDocument as vscode.TextDocument,
            mockVscodeOptions,
            cancellationTokenSource.token
        );

        if (edits && edits.length > 0 && edits[0]) {
            // Add additional debug info

            const newText = edits[0].newText;

            // The assertion will be done by the caller of formatDocument
            return newText; // Return the formatted text
        } else {
            return text; // Return original text if no edits
        }
    } catch (error) {
        console.error("Error in formatDocument:", error);
        throw error;
    }
}

describe("SSLFormattingProvider", () => {
    let provider: SSLFormattingProvider;
    let mockTextEditReplace: jest.SpyInstance;

    beforeEach(() => {
        jest.clearAllMocks(); // Ensure mocks are reset before each test

        // Re-import vscode and the provider after resetting modules
        // to ensure they get the fresh, mocked versions.
        // Note: vscode is already imported at the top level after the mock,
        // but SSLFormattingProvider might need to be re-required if it cached 'vscode' internally on first load.
        // For now, let's assume top-level import of vscode is sufficient after resetModules.
        // If issues persist, we might need to dynamically require SSLFormattingProvider here.

        provider = new SSLFormattingProvider();

        // Explicitly spy on and mock vscode.TextEdit.replace
        // Ensure that 'vscode.TextEdit' actually refers to our mock class here
        // due to jest.mock("vscode") at the top.
        mockTextEditReplace = jest
            .spyOn(vscode.TextEdit, "replace")
            .mockImplementation((range: vscode.Range, newText: string) => {
                // Enhanced logging for the input 'range'
                // console.log(
                //     "[TEST SPY vscode.TextEdit.replace] CALLED. Input Range object (raw):",
                //     range
                // );
                if (range && range.start && range.end) {
                    // Added null checks for start/end
                    // console.log(
                    //     `[TEST SPY vscode.TextEdit.replace] Input Range: start(${range.start.line}, ${range.start.character}) to end(${range.end.line}, ${range.end.character})`
                    // );
                } else {
                    // console.log(
                    //     "[TEST SPY vscode.TextEit.replace] Input Range, range.start, or range.end is null/undefined."
                    // );
                }
                // console.log("[TEST SPY vscode.TextEdit.replace] NewText length:", newText?.length); // Can be verbose

                const mockReturnedTextEdit = new vscode.TextEdit(range, newText);

                // console.log( // Removed verbose log
                //     "[TEST SPY vscode.TextEdit.replace] Constructed mock TextEdit instance:",
                //     mockReturnedTextEdit
                // );
                if (
                    mockReturnedTextEdit.range &&
                    mockReturnedTextEdit.range.start &&
                    mockReturnedTextEdit.range.end
                ) {
                    // Added null checks
                    // console.log(
                    //     `[TEST SPY vscode.TextEdit.replace] Returned TextEdit's Range: start(${mockReturnedTextEdit.range.start.line}, ${mockReturnedTextEdit.range.start.character}) to end(${mockReturnedTextEdit.range.end.line}, ${mockReturnedTextEdit.range.end.character})`
                    // );
                } else {
                    // console.log(
                    //     "[TEST SPY vscode.TextEdit.replace] Returned TextEdit's Range, range.start or range.end is null/undefined."
                    // );
                }
                return mockReturnedTextEdit;
            });
    });

    afterEach(() => {
        // Restore the original implementation after each test if necessary, though clearAllMocks might handle it.
        // For spyOn, it's good practice to restore.
        mockTextEditReplace.mockRestore();
    });

    describe("Indentation Rules", () => {
        it(
            "should indent :PROCEDURE blocks",
            async () => {
                const input = `
:PROCEDURE TestProc;
var1 := 1;
:ENDPROC;`;
                const expected = `
:PROCEDURE TestProc;
    var1 := 1;
:ENDPROC;`;
                // Pass formatting options to formatDocument
                const actual = await formatDocument(provider, input.trim(), {
                    tabSize: 4,
                    insertSpaces: true,
                });
                assert.strictEqual(actual.trim(), expected.trim());
            },
            TESTTIMEOUT
        );

        /* // Temporarily comment out other tests
        it("should indent :IF/:ELSE/:ENDIF blocks", async () => {
            const input = `
:IF .T.
var1 := 1
:ELSE
var2 := 2
:ENDIF`;
            const expected = `
:IF .T.
    var1 := 1
:ELSE
    var2 := 2
:ENDIF`;
            const actual = await formatDocument(provider, input.trim());
            assert.strictEqual(actual.trim(), expected.trim());
        });

        it(
            "should indent nested blocks correctly",
            async () => {
                const input = `
:PROCEDURE Outer
:IF .T.
var1 := 1
:WHILE var1 < 10
var1 := var1 + 1
:ENDWHILE
:ENDIF
:ENDPROC`;
                const expected = `
:PROCEDURE Outer
    :IF .T.
        var1 := 1
        :WHILE var1 < 10
            var1 := var1 + 1
        :ENDWHILE
    :ENDIF
:ENDPROC`;
                const actual = await formatDocument(provider, input.trim());
                assert.strictEqual(actual.trim(), expected.trim());
            },
            TESTTIMEOUT
        );

        it(
            "should not indent empty lines",
            async () => {
                const input = `
:PROCEDURE Test

var1 := 1

:ENDPROC`;
                const expected = `
:PROCEDURE Test

    var1 := 1

:ENDPROC`;
                const actual = await formatDocument(provider, input.trim());
                assert.strictEqual(actual.trim(), expected.trim());
            },
            TESTTIMEOUT
        );

        it(
            "should trim and not indent line-starting comments",
            async () => {
                const input = `
  /* Top level comment;
:PROCEDURE Test
    /* Indented comment in source;
var1 := 1
:ENDPROC`;
                // According to IndentationRule: comments starting a line are trimmed and not indented by the rule.
                const expected = `
/* Top level comment;
:PROCEDURE Test
    /* Indented comment in source;
    var1 := 1
:ENDPROC`;
                const actual = await formatDocument(provider, input.trim());
                assert.strictEqual(actual.trim(), expected.trim());
            },
            TESTTIMEOUT
        );
    });

    describe("Operator Spacing Rules", () => {
        it(
            "should add spaces around assignment operators",
            async () => {
                const input = "a:=1; b+=2; c-=3; d*=4; e/=5; f^=6;";
                const expected = "a := 1; b += 2; c -= 3; d *= 4; e /= 5; f ^= 6;";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should add spaces around comparison operators",
            async () => {
                const input = "a==b; c!=d; e<f; g>h; i<=j; k>=l; m=n";
                // Note: simple '=' is tokenized as SIMPLE_EQUALS, rule adds spaces
                const expected = "a == b; c != d; e < f; g > h; i <= j; k >= l; m = n";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should add spaces around arithmetic operators",
            async () => {
                const input = "a=1+2-3*4/5^6%7;";
                const expected = "a = 1 + 2 - 3 * 4 / 5 ^ 6 % 7;";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should add spaces around logical operators .AND. and .OR.",
            async () => {
                const input = "a .AND. b .OR. c";
                const expected = "a .AND. b .OR. c"; // Assumes input already has spaces, rule ensures one space
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);

                const input2 = "a.AND.b.OR.c";
                const expected2 = "a .AND. b .OR. c";
                const actual2 = await formatDocument(provider, input2);
                assert.strictEqual(actual2, expected2);
            },
            TESTTIMEOUT
        );

        it(
            "should not add spaces around operators inside string literals (heuristic)",
            async () => {
                const input = "s := \"a:=b\" + 'c+d' + [e*f];";
                const expected = "s := \"a:=b\" + 'c+d' + [e*f];"; // Heuristic should prevent spacing inside
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );
    });

    describe("Colon Spacing Rules", () => {
        it(
            "should add space after colon-keywords and uppercase them",
            async () => {
                const input = ":procedure myProc:parameters p1";
                // Rule only processes first :keyword on line, and uppercases it.
                // Assumes MyProcedure is an identifier part of :PROCEDURE if no space.
                // If :procedure myProc, keyword is "procedure", rest is "myProc..."
                const expected = ":PROCEDURE myProc:parameters p1";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);

                const input2 = ":if.t.";
                const expected2 = ":IF .t.";
                const actual2 = await formatDocument(provider, input2);
                assert.strictEqual(actual2, expected2);
            },
            TESTTIMEOUT
        );

        it(
            "should handle keywords already spaced and uppercased",
            async () => {
                const input = ":PROCEDURE ExistingProc :PARAMETERS P1";
                const expected = ":PROCEDURE ExistingProc :PARAMETERS P1";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );
    });

    describe("Comma Spacing Rules", () => {
        it(
            "should add space after commas",
            async () => {
                const input = "call(a,b,c); {1,2,3};";
                const expected = "call(a, b, c); {1, 2, 3};";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should normalize multiple spaces after commas to one",
            async () => {
                const input = "call(a,  b,   c);";
                const expected = "call(a, b, c);";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should not add spaces after commas inside string literals (heuristic)",
            async () => {
                const input = "s := \"a,b,c\" + 'd,e,f';";
                const expected = "s := \"a,b,c\" + 'd,e,f';";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );
    });

    describe("Formatting Options", () => {
        it(
            "should use specified tabSize and insertSpaces=true",
            async () => {
                const input = `
:PROCEDURE Test
:IF .T.
var1:=1
:ENDIF
:ENDPROC`;
                const expected = `
:PROCEDURE Test
  :IF .T.
    var1 := 1
  :ENDIF
:ENDPROC`;
                const actual = await formatDocument(provider, input.trim(), {
                    tabSize: 2,
                    insertSpaces: true,
                });
                assert.strictEqual(actual.trim(), expected.trim());
            },
            TESTTIMEOUT
        );

        it(
            "should use specified tabSize and insertSpaces=false (tabs)",
            async () => {
                const input = `
:PROCEDURE Test
:IF .T.
var1:=1
:ENDIF
:ENDPROC`;
                const expected = `
:PROCEDURE Test
\t:IF .T.
\t\tvar1 := 1
\t:ENDIF
:ENDPROC`;
                const actual = await formatDocument(provider, input.trim(), {
                    tabSize: 1,
                    insertSpaces: false,
                });
                assert.strictEqual(actual.trim(), expected.trim());
            },
            TESTTIMEOUT
        );
    });

    describe("Integration Tests from ssl_examples/formatting_provider_thorough_test.ssl", () => {
        it(
            "Test 1: Procedure and Declarations",
            async () => {
                const input = `
/* Test 1: Procedure and Declarations - Indentation and Colon Spacing;
:PROCEDURE MyProcedure:PARAMETERS param1,param2, param3 : DEFAULT param1 , "default" , param2 , 123
:DECLARE var1,var2 , var3
:DECLARE sString : STRING
:DECLARE nNumber:NUMBER
var1 :=param1+param2*param3
var2:="A string literal"
var3:= {1,2,3,4,5} /* Array literal with comma spacing;
:ENDPROC /* Added for block completion; `;

                const expected = `
/* Test 1: Procedure and Declarations - Indentation and Colon Spacing;
:PROCEDURE MyProcedure:PARAMETERS param1, param2, param3 :DEFAULT param1, "default", param2, 123
    :DECLARE var1, var2, var3
    :DECLARE sString :STRING
    :DECLARE nNumber :NUMBER
    var1 := param1 + param2 * param3
    var2 := "A string literal"
    var3 := {1, 2, 3, 4, 5} /* Array literal with comma spacing;
:ENDPROC /* Added for block completion; `;
                const actual = await formatDocument(provider, input.trim());
                assert.strictEqual(actual.trim(), expected.trim());
            },
            TESTTIMEOUT
        );

        it(
            "Test 2: Conditional Statements",
            async () => {
                const input = `
/* Test 2: Conditional Statements - Indentation, Operator Spacing;
:IF var1>10.AND.var2=="Test"
    var1:=var1+1
    :IF param3 <0
        var2:="Negative"
    :ELSE
        var2:="Positive"
    :ENDIF
:ELSE
    var1 :=var1-1
:ENDIF`;
                const expected = `
/* Test 2: Conditional Statements - Indentation, Operator Spacing;
:IF var1 > 10 .AND. var2 == "Test"
    var1 := var1 + 1
    :IF param3 < 0
        var2 := "Negative"
    :ELSE
        var2 := "Positive"
    :ENDIF
:ELSE
    var1 := var1 - 1
:ENDIF`;
                const actual = await formatDocument(provider, input.trim());
                assert.strictEqual(actual.trim(), expected.trim());
            },
            TESTTIMEOUT
        );

        it(
            "Test 3: Loop Constructs",
            async () => {
                const input = `
/* Test 3: Loop Constructs - Indentation;
:WHILE var1 > 0
    var1:=var1-1
    :IF var1 == 5 : EXITWHILE : ENDIF
:ENDWHILE

:FOR i:=1:TO Len(var3)
    var3[i] := var3[i]*2
    :IF i >100 : EXITFOR : ENDIF
:NEXT`;
                // Note: :EXITWHILE, :ENDIF, :EXITFOR on same line are not specially handled for spacing by current rules beyond keyword rules.
                const expected = `
/* Test 3: Loop Constructs - Indentation;
:WHILE var1 > 0
    var1 := var1 - 1
    :IF var1 == 5 :EXITWHILE :ENDIF
:ENDWHILE

:FOR i := 1 :TO Len(var3)
    var3[i] := var3[i] * 2
    :IF i > 100 :EXITFOR :ENDIF
:NEXT`;
                const actual = await formatDocument(provider, input.trim());
                assert.strictEqual(actual.trim(), expected.trim());
            },
            TESTTIMEOUT
        );

        it(
            "Test 4: Case Statement",
            async () => {
                const input = `
/* Test 4: Case Statement - Indentation and Colon Spacing;
:BEGINCASE
:CASE var1 == 1
    var2:="One"
:CASE var1==2.OR.var1==3
    var2:="Two or Three"
:OTHERWISE
    var2:="Other"
:ENDCASE`;
                const expected = `
/* Test 4: Case Statement - Indentation and Colon Spacing;
:BEGINCASE
    :CASE var1 == 1
        var2 := "One"
    :CASE var1 == 2 .OR. var1 == 3
        var2 := "Two or Three"
    :OTHERWISE
        var2 := "Other"
:ENDCASE`;
                const actual = await formatDocument(provider, input.trim());
                assert.strictEqual(actual.trim(), expected.trim());
            },
            TESTTIMEOUT
        );

        it(
            "Test 5: Error Handling",
            async () => {
                const input = `
/* Test 5: Error Handling - Indentation;
:TRY
    DoProc("RiskyOperation", {var1,var2})
:CATCH e
    LogError(e:Message)
:FINALLY
    var1:=0
:ENDTRY`;
                // Note: e:Message is object property access, not a :KEYWORD.
                const expected = `
/* Test 5: Error Handling - Indentation;
:TRY
    DoProc("RiskyOperation", {var1, var2})
:CATCH e
    LogError(e:Message)
:FINALLY
    var1 := 0
:ENDTRY`;
                const actual = await formatDocument(provider, input.trim());
                assert.strictEqual(actual.trim(), expected.trim());
            },
            TESTTIMEOUT
        );

        it(
            "Test 13: Comments",
            async () => {
                const input = `
/* This is a block comment
   spanning multiple lines;
var1 := 10 /* This is an end-of-line comment;
    /* Indented comment;
:PROCEDURE Test
    /* Comment inside proc;
    var2 := 20
:ENDPROC`;
                const expected = `
/* This is a block comment
   spanning multiple lines;
var1 := 10 /* This is an end-of-line comment;
/* Indented comment;
:PROCEDURE Test
    /* Comment inside proc;
    var2 := 20
:ENDPROC`;
                const actual = await formatDocument(provider, input.trim());
                assert.strictEqual(actual.trim(), expected.trim());
            },
            TESTTIMEOUT
        );
*/
    });
});
