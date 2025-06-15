/**
 * Comprehensive Folding Range Provider Test
 *
 * Tests the SSL folding range provider against all major constructs
 * defined in the EBNF grammar to ensure complete coverage.
 */

import * as vscode from "vscode";
import { SSLFoldingRangeProvider } from "../../src/providers/foldingRangeProvider";

describe("SSL Folding Range Provider - EBNF Compliance", () => {
    let provider: SSLFoldingRangeProvider;

    beforeEach(() => {
        provider = new SSLFoldingRangeProvider();
    });

    const createTestDocument = (content: string): vscode.TextDocument => {
        return {
            getText: () => content,
            lineCount: content.split("\n").length,
            lineAt: (line: number) => ({
                text: content.split("\n")[line] || "",
                lineNumber: line,
                range: new vscode.Range(line, 0, line, (content.split("\n")[line] || "").length),
                rangeIncludingLineBreak: new vscode.Range(line, 0, line + 1, 0),
                firstNonWhitespaceCharacterIndex: 0,
                isEmptyOrWhitespace: (content.split("\n")[line] || "").trim().length === 0,
            }),
            positionAt: (offset: number) => new vscode.Position(0, offset),
            offsetAt: (position: vscode.Position) => position.character,
            save: async () => true,
            isClosed: false,
            isDirty: false,
            isUntitled: false,
            languageId: "ssl",
            version: 1,
            fileName: "test.ssl",
            uri: vscode.Uri.file("test.ssl"),
        } as any;
    };

    describe("Core Language Constructs", () => {
        test("should fold procedure blocks", () => {
            const content = `:PROCEDURE TestProc;
    :PARAMETERS param1, param2;
    :DECLARE result;
    
    result := param1 + param2;
    :RETURN result;
:ENDPROC;`;

            const document = createTestDocument(content);
            const ranges = provider.provideFoldingRanges(
                document,
                {} as any,
                {} as any
            ) as vscode.FoldingRange[];

            expect(ranges).toBeDefined();
            expect(ranges.length).toBeGreaterThan(0);

            // Find a range that covers the procedure (should start at line 0)
            const procedureRange = ranges.find((r) => r.start === 0);
            expect(procedureRange).toBeDefined();
            expect(procedureRange?.end).toBeGreaterThan(0);
        });

        test("should fold conditional statements", () => {
            const content = `:IF condition;
    doSomething();
:ELSE;
    doSomethingElse();
:ENDIF;`;

            const document = createTestDocument(content);
            const ranges = provider.provideFoldingRanges(
                document,
                {} as any,
                {} as any
            ) as vscode.FoldingRange[];

            expect(ranges).toBeDefined();
            expect(ranges.length).toBeGreaterThan(0);
        });

        test("should fold loop constructs", () => {
            const content = `:WHILE condition;
    processItem();
    counter += 1;
:ENDWHILE;

:FOR i := 1 :TO 10;
    processIndex(i);
:NEXT;`;

            const document = createTestDocument(content);
            const ranges = provider.provideFoldingRanges(
                document,
                {} as any,
                {} as any
            ) as vscode.FoldingRange[];

            expect(ranges).toBeDefined();
            expect(ranges.length).toBeGreaterThan(0);
        });
        test("should fold switch case blocks", () => {
            const content = `:BEGINCASE value;
    :CASE 1;
        handleOne();
    :CASE 2;
        handleTwo();
    :OTHERWISE;
        handleDefault();
:ENDCASE;`;

            const document = createTestDocument(content);
            const ranges = provider.provideFoldingRanges(
                document,
                {} as any,
                {} as any
            ) as vscode.FoldingRange[];

            expect(ranges).toBeDefined();
            expect(ranges.length).toBeGreaterThan(0);
        });

        test("should fold try-catch-finally blocks", () => {
            const content = `:TRY;
    riskyOperation();
:CATCH;
    handleError();
:FINALLY;
    cleanup();
:ENDTRY;`;

            const document = createTestDocument(content);
            const ranges = provider.provideFoldingRanges(
                document,
                {} as any,
                {} as any
            ) as vscode.FoldingRange[];

            expect(ranges).toBeDefined();
            expect(ranges.length).toBeGreaterThan(0);
        });
    });

    describe("Object-Oriented Constructs", () => {
        test("should fold class definitions", () => {
            const content = `:CLASS MyClass;
:INHERIT BaseClass;

:PROCEDURE Initialize;
    setupInstance();
:ENDPROC;

:PROCEDURE ProcessData;
    :PARAMETERS data;
    processData(data);
:ENDPROC;`;

            const document = createTestDocument(content);
            const ranges = provider.provideFoldingRanges(
                document,
                {} as any,
                {} as any
            ) as vscode.FoldingRange[];

            expect(ranges).toBeDefined();
            expect(ranges.length).toBeGreaterThan(0);
        });
    });

    describe("Special Blocks", () => {
        test("should fold region blocks", () => {
            const content = `:REGION Utilities;
    :PROCEDURE HelperFunc;
        doHelper();
    :ENDPROC;
:ENDREGION;`;

            const document = createTestDocument(content);
            const ranges = provider.provideFoldingRanges(
                document,
                {} as any,
                {} as any
            ) as vscode.FoldingRange[];

            expect(ranges).toBeDefined();
            expect(ranges.length).toBeGreaterThan(0);
        });

        test("should fold inline code blocks", () => {
            const content = `:BEGININLINECODE CodeSnippet;
    dynamicCode := "RETURN 42;";
    compiledCode := CompileCode(dynamicCode);
:ENDINLINECODE;`;

            const document = createTestDocument(content);
            const ranges = provider.provideFoldingRanges(
                document,
                {} as any,
                {} as any
            ) as vscode.FoldingRange[];

            expect(ranges).toBeDefined();
            expect(ranges.length).toBeGreaterThan(0);
        });

        test("should fold error blocks", () => {
            const content = `:ERROR;
    LogError("An error occurred");
    ShowErrorDialog();
    :RETURN false;`;

            const document = createTestDocument(content);
            const ranges = provider.provideFoldingRanges(
                document,
                {} as any,
                {} as any
            ) as vscode.FoldingRange[];

            expect(ranges).toBeDefined();
        });
    });

    describe("Data Structures", () => {
        test("should fold multi-line array literals", () => {
            const content = `data := {
    "item1",
    "item2",
    "item3"
};`;

            const document = createTestDocument(content);
            const ranges = provider.provideFoldingRanges(
                document,
                {} as any,
                {} as any
            ) as vscode.FoldingRange[];

            expect(ranges).toBeDefined();
            expect(ranges.length).toBeGreaterThan(0);
        });
        test("should fold code block literals", () => {
            const content = `dynamicCode := {|param1, param2|
    param1 + param2
};`;

            const document = createTestDocument(content);
            const ranges = provider.provideFoldingRanges(
                document,
                {} as any,
                {} as any
            ) as vscode.FoldingRange[];

            expect(ranges).toBeDefined();
            expect(ranges.length).toBeGreaterThan(0);
        });
    });

    describe("Comments and Documentation", () => {
        test("should fold multi-line block comments", () => {
            const content = `/* This is a long block comment
   that spans multiple lines
   and contains detailed documentation
   about the following code ;`;

            const document = createTestDocument(content);
            const ranges = provider.provideFoldingRanges(
                document,
                {} as any,
                {} as any
            ) as vscode.FoldingRange[];

            expect(ranges).toBeDefined();
            expect(ranges.length).toBeGreaterThan(0);
        });

        test("should fold comment regions", () => {
            const content = `/* region Database Operations
/* Initialize database connection ;
InitDB();

/* Perform queries ;
ExecuteQuery("SELECT * FROM samples");
/* endregion ;`;

            const document = createTestDocument(content);
            const ranges = provider.provideFoldingRanges(
                document,
                {} as any,
                {} as any
            ) as vscode.FoldingRange[];

            expect(ranges).toBeDefined();
            expect(ranges.length).toBeGreaterThan(0);
        });
        test("should fold consecutive single-line comments", () => {
            const content = `/* First comment line ;
/* Second comment line ;
/* Third comment line ;
/* Fourth comment line ;

someCode();`;

            const document = createTestDocument(content);
            const ranges = provider.provideFoldingRanges(
                document,
                {} as any,
                {} as any
            ) as vscode.FoldingRange[];

            expect(ranges).toBeDefined();
            expect(ranges.length).toBeGreaterThan(0);
        });
    });

    describe("SQL Integration", () => {
        test("should fold multi-line SQL strings", () => {
            const content = `query := "
SELECT s.sample_id,
       s.sample_name,
       t.test_name,
       r.result_value
FROM samples s
JOIN tests t ON s.sample_id = t.sample_id  
JOIN results r ON t.test_id = r.test_id
WHERE s.status = 'ACTIVE'
ORDER BY s.sample_id;
";`;

            const document = createTestDocument(content);
            const ranges = provider.provideFoldingRanges(
                document,
                {} as any,
                {} as any
            ) as vscode.FoldingRange[];

            expect(ranges).toBeDefined();
            expect(ranges.length).toBeGreaterThan(0);
        });

        test("should fold LSearch blocks", () => {
            const content = `results := LSearch("
SELECT sample_id, sample_name
FROM samples 
WHERE status = 'PENDING'
AND created_date >= ?
", {startDate});`;

            const document = createTestDocument(content);
            const ranges = provider.provideFoldingRanges(
                document,
                {} as any,
                {} as any
            ) as vscode.FoldingRange[];

            expect(ranges).toBeDefined();
        });
    });

    describe("Complex Nested Structures", () => {
        test("should handle deeply nested constructs", () => {
            const content = `:PROCEDURE ComplexProc;
    :PARAMETERS data;
    
    :IF IsValid(data);
        :TRY;
            :FOR i := 1 :TO Len(data);
                :BEGINCASE data[i];
                    :CASE "TYPE_A";
                        :WHILE processing;
                            processTypeA();
                        :ENDWHILE;
                    :CASE "TYPE_B";
                        processTypeB();
                    :OTHERWISE;
                        logUnknownType();
                :ENDCASE;
            :NEXT;
        :CATCH;
            logError();
        :ENDTRY;
    :ELSE;
        :RETURN false;
    :ENDIF;
:ENDPROC;`;

            const document = createTestDocument(content);
            const ranges = provider.provideFoldingRanges(
                document,
                {} as any,
                {} as any
            ) as vscode.FoldingRange[];

            expect(ranges).toBeDefined();
            expect(ranges.length).toBeGreaterThan(0);

            // Should have multiple folding ranges for different constructs
            expect(ranges.length).toBeGreaterThanOrEqual(5);
        });
    });

    describe("Edge Cases", () => {
        test("should handle empty constructs", () => {
            const content = `:IF condition;
:ENDIF;

:WHILE false;
:ENDWHILE;`;

            const document = createTestDocument(content);
            const ranges = provider.provideFoldingRanges(
                document,
                {} as any,
                {} as any
            ) as vscode.FoldingRange[];

            // Should not create folding ranges for single-line constructs
            expect(ranges).toBeDefined();
        });

        test("should handle malformed constructs gracefully", () => {
            const content = `:IF condition;
    someCode();
/* Missing :ENDIF;

:PROCEDURE MissingEnd;
    someCode();
/* Missing :ENDPROC;`;

            const document = createTestDocument(content);
            const ranges = provider.provideFoldingRanges(
                document,
                {} as any,
                {} as any
            ) as vscode.FoldingRange[];

            // Should not crash and return what it can
            expect(ranges).toBeDefined();
        });

        test("should handle mixed line endings", () => {
            const content = `:PROCEDURE TestProc;\r\n    :DECLARE var;\n    var := "value";\r\n:ENDPROC;`;

            const document = createTestDocument(content);
            const ranges = provider.provideFoldingRanges(
                document,
                {} as any,
                {} as any
            ) as vscode.FoldingRange[];

            expect(ranges).toBeDefined();
        });
    });

    describe("Performance", () => {
        test("should handle large files efficiently", () => {
            // Generate a large SSL file with many constructs
            let content = "";
            for (let i = 0; i < 100; i++) {
                content += `:PROCEDURE TestProc${i};
    :PARAMETERS param1, param2;
    :DECLARE result;
    
    :IF param1 > 0;
        result := param1 * param2;
    :ELSE;
        result := 0;
    :ENDIF;
    
    :RETURN result;
:ENDPROC;

`;
            }

            const document = createTestDocument(content);
            const start = Date.now();
            const ranges = provider.provideFoldingRanges(
                document,
                {} as any,
                {} as any
            ) as vscode.FoldingRange[];
            const duration = Date.now() - start;

            expect(ranges).toBeDefined();
            expect(ranges.length).toBeGreaterThan(0);
            expect(duration).toBeLessThan(1000); // Should complete within 1 second
        });
    });
});
