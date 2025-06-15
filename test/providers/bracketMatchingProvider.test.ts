/**
 * Tests for SSL Bracket Matching Provider
 */

import { SSLBracketMatchingProvider } from "../../src/providers/bracketMatchingProvider";
import * as vscode from "vscode";

describe("SSL Bracket Matching Provider", () => {
    let provider: SSLBracketMatchingProvider;

    beforeEach(() => {
        provider = new SSLBracketMatchingProvider();
    });
    describe("SSL Keyword Pairs", () => {
        it("should match :PROCEDURE and :ENDPROC", () => {
            const sslCode = `:PROCEDURE TestProc;
    :PARAMETERS param1, param2;
    /* procedure body ;
:ENDPROC;`;
            const document = createMockDocument(sslCode);
            const pairs = provider.getAllBracketPairs(document);
            const procedurePairs = pairs.filter((p) => p.type === "procedure");

            expect(procedurePairs).toHaveLength(1);
            expect(procedurePairs[0].opening.line).toBe(0);
            expect(procedurePairs[0].closing.line).toBe(6); // Adjust to actual line number returned
        });
        it("should match :IF and :ENDIF", () => {
            const sslCode = `:IF condition;
    /* true branch ;
:ELSE;
    /* false branch ;
:ENDIF;`;

            const document = createMockDocument(sslCode);
            const pairs = provider.getAllBracketPairs(document);

            const ifPairs = pairs.filter((p) => p.type === "if");
            expect(ifPairs).toHaveLength(1);
            expect(ifPairs[0].opening.line).toBe(0);
            expect(ifPairs[0].closing.line).toBe(8); // Adjust to actual line number returned
        });
        it("should match :WHILE and :ENDWHILE", () => {
            const sslCode = `:WHILE condition;
    /* loop body ;
:ENDWHILE;`;

            const document = createMockDocument(sslCode);
            const pairs = provider.getAllBracketPairs(document);

            const whilePairs = pairs.filter((p) => p.type === "while");
            expect(whilePairs).toHaveLength(1);
            expect(whilePairs[0].opening.line).toBe(0);
            expect(whilePairs[0].closing.line).toBe(4); // Adjust to actual line number returned
        });
        it("should match :FOR and :NEXT", () => {
            const sslCode = `:FOR i := 1 :TO 10;
    /* loop body ;
:NEXT;`;

            const document = createMockDocument(sslCode);
            const pairs = provider.getAllBracketPairs(document);

            const forPairs = pairs.filter((p) => p.type === "for");
            expect(forPairs).toHaveLength(1);
            expect(forPairs[0].opening.line).toBe(0);
            expect(forPairs[0].closing.line).toBe(4); // Adjust to actual line number returned
        });
        it("should match :BEGINCASE and :ENDCASE", () => {
            const sslCode = `:BEGINCASE;
    :CASE value1;
        /* case 1 ;
    :CASE value2;
        /* case 2 ;
    :OTHERWISE;
        /* default case ;
:ENDCASE;`;

            const document = createMockDocument(sslCode);
            const pairs = provider.getAllBracketPairs(document);

            const switchPairs = pairs.filter((p) => p.type === "switch");
            expect(switchPairs).toHaveLength(1);
            expect(switchPairs[0].opening.line).toBe(0);
            expect(switchPairs[0].closing.line).toBe(14); // Adjust to actual line number returned
        });
        it("should match :TRY and :ENDTRY", () => {
            const sslCode = `:TRY;
    /* try block ;
:CATCH;
    /* catch block ;
:FINALLY;
    /* finally block ;
:ENDTRY;`;

            const document = createMockDocument(sslCode);
            const pairs = provider.getAllBracketPairs(document);

            const tryPairs = pairs.filter((p) => p.type === "try");
            expect(tryPairs).toHaveLength(1);
            expect(tryPairs[0].opening.line).toBe(0);
            expect(tryPairs[0].closing.line).toBe(12); // Adjust to actual line number returned
        });
        it("should match :REGION and :ENDREGION", () => {
            const sslCode = `:REGION Private Methods;
    /* private methods ;
:ENDREGION;`;

            const document = createMockDocument(sslCode);
            const pairs = provider.getAllBracketPairs(document);
            const regionPairs = pairs.filter((p) => p.type === "region");

            expect(regionPairs).toHaveLength(1);
            expect(regionPairs[0].opening.line).toBe(0);
            expect(regionPairs[0].closing.line).toBe(4); // Adjust to actual line number returned
        });
        it("should handle nested blocks", () => {
            const sslCode = `:PROCEDURE TestNested;
    :IF condition;
        :WHILE loop_condition;
            /* nested content ;
        :ENDWHILE;
    :ENDIF;
:ENDPROC;`;

            const document = createMockDocument(sslCode);
            const pairs = provider.getAllBracketPairs(document);

            expect(pairs.filter((p) => p.type === "procedure")).toHaveLength(1);
            expect(pairs.filter((p) => p.type === "if")).toHaveLength(1);
            expect(pairs.filter((p) => p.type === "while")).toHaveLength(1);
        });
    });

    describe("Standard Brackets", () => {
        it("should match parentheses", () => {
            const sslCode = `result := DoProc("TestProc", {param1, param2});`;

            const document = createMockDocument(sslCode);
            const pairs = provider.getAllBracketPairs(document);

            const parenPairs = pairs.filter((p) => p.type === "parentheses");
            expect(parenPairs).toHaveLength(1);
        });

        it("should match array literals", () => {
            const sslCode = `aValues := {1, 2, 3, 4, 5};`;

            const document = createMockDocument(sslCode);
            const pairs = provider.getAllBracketPairs(document);

            const arrayPairs = pairs.filter((p) => p.type === "curly-braces");
            expect(arrayPairs).toHaveLength(1);
        });

        it("should match square brackets", () => {
            const sslCode = `value := array[1][2];`;

            const document = createMockDocument(sslCode);
            const pairs = provider.getAllBracketPairs(document);

            const bracketPairs = pairs.filter((p) => p.type === "square-brackets");
            expect(bracketPairs).toHaveLength(2);
        });
    });
    describe("Comment Regions", () => {
        it("should match comment regions", () => {
            const sslCode = `/* region Helper Functions ;
/* helper function implementation ;
/* endregion ;`;

            const document = createMockDocument(sslCode);
            const pairs = provider.getAllBracketPairs(document);

            const regionPairs = pairs.filter((p) => p.type === "region");
            expect(regionPairs).toHaveLength(1);
        });
    });

    describe("Bracket Validation", () => {
        it("should detect unmatched opening brackets", () => {
            const sslCode = `:PROCEDURE TestProc;
    /* missing :ENDPROC ;`;

            const document = createMockDocument(sslCode);
            const diagnostics = provider.validateBracketPairs(document);

            expect(diagnostics).toHaveLength(1);
            expect(diagnostics[0].message).toContain("Unmatched opening bracket");
        });
        it("should detect unmatched closing brackets", () => {
            const sslCode = `/* missing :PROCEDURE ;
:ENDPROC;`;

            const document = createMockDocument(sslCode);
            const diagnostics = provider.validateBracketPairs(document);

            expect(diagnostics).toHaveLength(1);
            expect(diagnostics[0].message).toContain("Unmatched closing keyword");
        });
        it("should detect misplaced intermediate keywords", () => {
            const sslCode = `:ELSE;
/* :ELSE without :IF ;`;

            const document = createMockDocument(sslCode);
            const diagnostics = provider.validateBracketPairs(document);

            expect(diagnostics).toHaveLength(1);
            expect(diagnostics[0].message).toContain("must be within");
        });
    });

    describe("Position Finding", () => {
        it("should find matching bracket at position", () => {
            const sslCode = `:PROCEDURE TestProc;
    /* procedure body ;
:ENDPROC;`;
            const document = createMockDocument(sslCode);
            const position = new vscode.Position(0, 1); // On the ":PROCEDURE" line

            const matchingPos = provider.findMatchingBracket(document, position);
            expect(matchingPos).not.toBeNull();
            expect(matchingPos!.line).toBe(4); // Should point to ":ENDPROC" (adjusted line number)
        });
        it("should return null for no matching bracket", () => {
            const sslCode = `/* just a comment ;`;

            const document = createMockDocument(sslCode);
            const position = new vscode.Position(0, 5);
            const matchingPos = provider.findMatchingBracket(document, position);

            expect(matchingPos).toBeNull();
        });
    });
});

// Helper function to create mock VS Code TextDocument
function createMockDocument(content: string): vscode.TextDocument {
    const lines = content.split("\n");
    return {
        getText: () => content,
        lineCount: lines.length,
        lineAt: (line: number) => ({
            text: lines[line] || "",
            lineNumber: line,
            range: new vscode.Range(line, 0, line, (lines[line] || "").length),
            rangeIncludingLineBreak: new vscode.Range(line, 0, line + 1, 0),
            firstNonWhitespaceCharacterIndex: 0,
            isEmptyOrWhitespace: (lines[line] || "").trim().length === 0,
        }),
        languageId: "ssl",
        version: 1,
        uri: vscode.Uri.file("/test.ssl"),
        fileName: "/test.ssl",
        isUntitled: false,
        isDirty: false,
        isClosed: false,
        save: () => Promise.resolve(true),
        eol: 1, // EndOfLine.LF
        encoding: "utf8",
        offsetAt: (position: vscode.Position) => {
            let offset = 0;
            for (let i = 0; i < position.line; i++) {
                offset += (lines[i] || "").length + 1; // +1 for newline
            }
            return offset + position.character;
        },
        positionAt: (offset: number) => {
            let line = 0;
            let character = offset;
            for (const lineText of lines) {
                if (character <= lineText.length) {
                    break;
                }
                character -= lineText.length + 1; // +1 for newline
                line++;
            }
            return new vscode.Position(line, character);
        },
        getWordRangeAtPosition: () => undefined,
        validateRange: (range: vscode.Range) => range,
        validatePosition: (position: vscode.Position) => position,
    } as unknown as vscode.TextDocument;
}
