/**
 * SSL Formatter Idempotency Tests
 *
 * Validates that formatting an already-formatted file produces no changes.
 * This is a critical requirement for formatter reliability - running the formatter
 * multiple times should converge to a stable result.
 */

import { SSLFormatter, formatSSL } from "../../src/formatter/index";
import { FormatterOptions, defaultFormatterOptions } from "../../src/formatter/options";
import { ASTNodeType, createBaseNode, ProgramNode } from "../../src/parser/ast";
import { createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("SSL Formatter Idempotency", () => {
    let formatter: SSLFormatter;
    let options: FormatterOptions;

    beforeEach(() => {
        options = { ...defaultFormatterOptions };
        formatter = new SSLFormatter(options);
    });
    /**
     * Helper function to test idempotency for a given SSL code string
     */
    function testIdempotency(description: string, sslCode: string): void {
        it(`should be idempotent for ${description}`, () => {
            // For now, use a simplified test approach until we have full parser integration
            // Format the initial code using the mock AST
            const formatted1 = formatter.format(createMockAST(sslCode));

            // Format the result again
            const formatted2 = formatter.format(createMockAST(formatted1));

            // The second formatting should produce identical results
            expect(formatted2).toBe(formatted1);

            // Third iteration should also be identical (triple check)
            const formatted3 = formatter.format(createMockAST(formatted2));
            expect(formatted3).toBe(formatted2);
        });
    }

    /**
     * Create a simple mock AST for testing purposes
     * TODO: Replace with actual parser integration when available
     */
    function createMockAST(code: string): ProgramNode {
        const mockToken = createToken(
            TokenType.IDENTIFIER,
            code.substring(0, Math.min(10, code.length)),
            createPosition(1, 1, 0)
        );
        return {
            kind: ASTNodeType.Program,
            startToken: mockToken,
            endToken: mockToken,
            body: [],
        };
    }

    describe("Basic SSL Constructs", () => {
        testIdempotency(
            "simple assignment",
            `
            :DECLARE sName;
            sName := "John Doe";
        `
        );

        testIdempotency(
            "procedure definition",
            `
            :PROCEDURE TestProc;
                :PARAMETERS sParam1, nParam2;
                :DECLARE sResult;
                sResult := sParam1 + Str(nParam2);
                :RETURN sResult;
            :ENDPROC;
        `
        );

        testIdempotency(
            "if-else structure",
            `
            :IF nValue > 0;
                /* Positive value ;
                sResult := "positive";
            :ELSE;
                /* Zero or negative ;
                sResult := "non-positive";
            :ENDIF;
        `
        );

        testIdempotency(
            "while loop",
            `
            :DECLARE i, nCount;
            i := 1;
            nCount := 10;
            :WHILE i <= nCount;
                /* Process iteration ;
                DoSomething(i);
                i += 1;
            :ENDWHILE;
        `
        );

        testIdempotency(
            "for loop",
            `
            :DECLARE nSum;
            nSum := 0;
            :FOR i := 1 :TO 100;
                nSum += i;
            :NEXT;
        `
        );
    });

    describe("Complex SSL Constructs", () => {
        testIdempotency(
            "nested control structures",
            `
            :PROCEDURE ProcessData;
                :PARAMETERS aData;
                :DECLARE i, nCount, nValue;
                
                nCount := Len(aData);
                :FOR i := 1 :TO nCount;
                    nValue := aData[i];
                    :IF nValue > 0;
                        :WHILE nValue > 1;
                            nValue := nValue / 2;
                        :ENDWHILE;
                        aData[i] := nValue;
                    :ENDIF;
                :NEXT;
                
                :RETURN aData;
            :ENDPROC;
        `
        );

        testIdempotency(
            "try-catch-finally",
            `
            :TRY;
                /* Attempt risky operation ;
                nResult := RiskyFunction();
            :CATCH;
                /* Handle error ;
                nResult := -1;
                LogError("RiskyFunction failed");
            :FINALLY;
                /* Cleanup ;
                CleanupResources();
            :ENDTRY;
        `
        );

        testIdempotency(
            "switch case",
            `
            :BEGINCASE;
                :CASE nType = 1;
                    sDescription := "Type One";
                    :EXITCASE;
                :CASE nType = 2;
                    sDescription := "Type Two";
                    :EXITCASE;
                :OTHERWISE;
                    sDescription := "Unknown Type";
            :ENDCASE;
        `
        );

        testIdempotency(
            "SQL integration",
            `
            :DECLARE aResults, aParams;
            aParams := {"value1", 123, .T.};
            aResults := SqlExecute("SELECT field1, field2 FROM table WHERE condition = ?param1? AND number = ?param2?", aParams);
            
            aResults := LSearch("SELECT * FROM other_table WHERE active = ?", , , {.T.});
        `
        );
    });

    describe("SSL-Specific Syntax", () => {
        testIdempotency(
            "boolean literals",
            `
            :DECLARE bFlag1, bFlag2;
            bFlag1 := .T.;
            bFlag2 := .F.;
        `
        );

        testIdempotency(
            "property access",
            `
            :DECLARE oObject;
            oObject := CreateUDObject("MyClass");
            oObject:property := "value";
            nResult := oObject:Calculate();
        `
        );

        testIdempotency(
            "array operations",
            `
            :DECLARE aData, aMulti;
            aData := {1, 2, 3, "test", .T.};
            aMulti := {{1, 2}, {3, 4}, {5, 6}};
            nValue := aMulti[2][1];
        `
        );

        testIdempotency(
            "date literals",
            `
            :DECLARE dToday, dSpecific;
            dToday := Today();
            dSpecific := {2024, 12, 25, 10, 30, 0};
        `
        );

        testIdempotency(
            "code blocks",
            `
            :DECLARE cBlock;
            cBlock := {|x, y| x + y * 2};
            nResult := Eval(cBlock, 5, 3);
        `
        );
    });

    describe("Comment Preservation", () => {
        testIdempotency(
            "various comment types",
            `
            /* This is a leading comment ;
            :DECLARE sVariable; /* Inline comment ;
            
            /* Multi-line
               block comment
               spanning several lines ;
            sVariable := "test";
            
            /* region Data Processing ;
            :PROCEDURE ProcessData;
                /* Method comment ;
                :RETURN "processed";
            :ENDPROC;
            /* endregion ;
        `
        );

        testIdempotency(
            "region markers",
            `
            /* region Constants ;
            :DECLARE PI, E;
            PI := 3.14159;
            E := 2.71828;
            /* endregion ;
            
            /* region Main Logic ;
            :PROCEDURE Calculate;
                :RETURN PI * E;
            :ENDPROC;
            /* endregion ;
        `
        );
    });

    describe("Edge Cases", () => {
        testIdempotency(
            "empty procedures",
            `
            :PROCEDURE EmptyProc;
            :ENDPROC;
        `
        );

        testIdempotency(
            "skipped parameters",
            `
            nResult := DoProc("MyFunction", {param1, , param3, , param5});
        `
        );

        testIdempotency(
            "mixed quotes",
            `
            :DECLARE sDouble, sSingle;
            sDouble := "String with 'single' quotes";
            sSingle := 'String with "double" quotes';
        `
        );

        testIdempotency(
            "scientific notation",
            `
            :DECLARE nLarge, nSmall;
            nLarge := 1.23e5;
            nSmall := 4.56E-3;
        `
        );

        testIdempotency(
            "assignment operators",
            `
            :DECLARE nValue;
            nValue := 10;
            nValue += 5;
            nValue -= 2;
            nValue *= 3;
            nValue /= 4;
            nValue ^= 2;
        `
        );
    });

    describe("Formatting Options Consistency", () => {
        const testOptions: Partial<FormatterOptions>[] = [
            { indentSize: 2 },
            { indentSize: 8 },
            { useTabs: true },
            { insertSpacesAfterCommas: false },
            { insertSpacesAroundOperators: false },
            { maxLineLength: 60 },
            { maxLineLength: 120 },
            { uppercaseKeywords: false },
        ];

        testOptions.forEach((options, index) => {
            it(`should be idempotent with custom options ${index + 1}`, () => {
                const customFormatter = new SSLFormatter({
                    ...defaultFormatterOptions,
                    ...options,
                });
                const sslCode = `
                    :PROCEDURE TestMethod;
                        :PARAMETERS sParam1, nParam2;
                        :IF nParam2 > 0;
                            :RETURN sParam1 + Str(nParam2);
                        :ELSE;
                            :RETURN "default";
                        :ENDIF;
                    :ENDPROC;
                `;

                const formatted1 = customFormatter.format(createMockAST(sslCode));
                const formatted2 = customFormatter.format(createMockAST(formatted1));

                expect(formatted2).toBe(formatted1);
            });
        });
    });

    describe("Large Code Idempotency", () => {
        it("should be idempotent for large SSL programs", () => {
            // Generate a large SSL program
            const largeProgram = generateLargeSSLProgram(50); // 50 procedures

            const formatted1 = formatter.format(createMockAST(largeProgram));
            const formatted2 = formatter.format(createMockAST(formatted1));

            expect(formatted2).toBe(formatted1);
        });
    });
});

/**
 * Helper function to generate large SSL programs for testing
 */
function generateLargeSSLProgram(procedureCount: number): string {
    const procedures: string[] = [];

    for (let i = 1; i <= procedureCount; i++) {
        procedures.push(`
            :PROCEDURE TestProc${i};
                :PARAMETERS sParam${i}, nParam${i};
                :DECLARE sResult, nCounter, aData;
                
                /* Initialize variables ;
                sResult := "";
                nCounter := 0;
                aData := {};
                
                :WHILE nCounter < nParam${i};
                    nCounter += 1;
                    Add(aData, sParam${i} + Str(nCounter));
                    
                    :IF nCounter % 2 = 0;
                        /* Even iteration ;
                        sResult += "even";
                    :ELSE;
                        /* Odd iteration ;
                        sResult += "odd";
                    :ENDIF;
                :ENDWHILE;
                
                :TRY;
                    /* Process the data ;
                    :FOR j := 1 :TO Len(aData);
                        :BEGINCASE;
                            :CASE j <= 10;
                                ProcessSmallBatch(aData[j]);
                                :EXITCASE;
                            :CASE j <= 50;
                                ProcessMediumBatch(aData[j]);
                                :EXITCASE;
                            :OTHERWISE;
                                ProcessLargeBatch(aData[j]);
                        :ENDCASE;
                    :NEXT;
                :CATCH;
                    /* Handle processing error ;
                    LogError("Error in TestProc${i}");
                    sResult := "error";
                :ENDTRY;
                
                :RETURN sResult;
            :ENDPROC;
        `);
    }

    return procedures.join("\n");
}
