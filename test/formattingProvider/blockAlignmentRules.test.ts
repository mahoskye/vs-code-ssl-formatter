import { ASTNodeType } from "../../src/core/parser"; // Added import for ASTNodeType
import { BlockAlignmentRule } from "../../src/formatters/rules/blockAlignmentRule"; // Added import for BlockAlignmentRule
import { FormattingContext } from "../../src/formatters/formattingProvider"; // Added import for FormattingContext
import { SSLFormattingProvider } from "../../src/formatters/formattingProvider"; // Added import for SSLFormattingProvider

// Add this section to mock console.warn
let originalConsoleWarn: any;

beforeAll(() => {
    originalConsoleWarn = console.warn;
    console.warn = (...args: any[]) => {
        if (
            args.length > 0 &&
            typeof args[0] === "string" &&
            args[0].includes("IndentationRule not found in SSLFormattingProvider")
        ) {
            // Suppress the specific warning
            return;
        }
        originalConsoleWarn.apply(console, args);
    };
});

afterAll(() => {
    console.warn = originalConsoleWarn; // Restore original console.warn
});

describe("BlockAlignmentRule", () => {
    let rule: BlockAlignmentRule;
    let context: FormattingContext;
    let provider: SSLFormattingProvider; // Added provider
    beforeEach(() => {
        rule = new BlockAlignmentRule();
        // Instantiate provider with only BlockAlignmentRule for isolated testing
        provider = new SSLFormattingProvider([new BlockAlignmentRule()]);
        context = {
            indentLevel: 0,
            blockType: null,
            previousLine: null,
            nextLine: null,
            lineNumber: 1,
            options: {
                tabSize: 4,
                insertSpaces: true,
                maxLineLength: 90,
                indentStyle: "space" as const,
            },
            blockDepth: 0,
            inMultiLineConstruct: false,
            constructType: null,
            lineTokens: [],
            ast: { type: ASTNodeType.program, children: [], line: 0 }, // Corrected to lowercase 'program'
            enclosingASTBlockType: null,
            currentLineBaseIndentLevel: 0, // Added missing property
        };
    });

    describe("SSL Block Structure Alignment", () => {
        describe("IF/ELSE/ENDIF blocks", () => {
            it("should align :IF statement at current block depth", () => {
                const line = "    :IF condition;";
                context.blockDepth = 1;

                const result = rule.apply(line, context);

                expect(result).toBe("    :IF condition;");
            });

            it("should align :ELSE at same level as :IF", () => {
                const line = "        :ELSE;";
                context.blockDepth = 2;

                const result = rule.apply(line, context);

                expect(result).toBe("    :ELSE;");
            });

            it("should align :ELSEIF at same level as :IF", () => {
                const line = "  :ELSEIF another_condition;";
                context.blockDepth = 2;

                const result = rule.apply(line, context);

                expect(result).toBe("    :ELSEIF another_condition;");
            });

            it("should align :ENDIF at same level as :IF", () => {
                const line = "      :ENDIF;";
                context.blockDepth = 2;

                const result = rule.apply(line, context);

                expect(result).toBe("    :ENDIF;");
            });
        });

        describe("WHILE loops", () => {
            it("should align :WHILE statement correctly", () => {
                const line = ":WHILE (nCounter += 1) <= nMax;";
                context.blockDepth = 0;

                const result = rule.apply(line, context);

                expect(result).toBe(":WHILE (nCounter += 1) <= nMax;");
            });

            it("should align :ENDWHILE at same level as :WHILE", () => {
                const line = "    :ENDWHILE;";
                context.blockDepth = 1;

                const result = rule.apply(line, context);

                expect(result).toBe(":ENDWHILE;");
            });
        });

        describe("FOR loops", () => {
            it("should align :FOR statement correctly", () => {
                const line = ":FOR i := 1 :TO 10;";
                context.blockDepth = 0;

                const result = rule.apply(line, context);

                expect(result).toBe(":FOR i := 1 :TO 10;");
            });

            it("should align :NEXT at same level as :FOR", () => {
                const line = "  :NEXT;";
                context.blockDepth = 1;

                const result = rule.apply(line, context);

                expect(result).toBe(":NEXT;");
            });
        });

        describe("CASE statements", () => {
            it("should align :BEGINCASE correctly", () => {
                const line = ":BEGINCASE;";
                context.blockDepth = 0;

                const result = rule.apply(line, context);

                expect(result).toBe(":BEGINCASE;");
            });

            it("should align :CASE at same level as :BEGINCASE", () => {
                const line = "  :CASE sValue;";
                context.blockDepth = 1;

                const result = rule.apply(line, context);

                expect(result).toBe(":CASE sValue;");
            });

            it("should align :OTHERWISE at same level as :CASE", () => {
                const line = "    :OTHERWISE;";
                context.blockDepth = 1;

                const result = rule.apply(line, context);

                expect(result).toBe(":OTHERWISE;");
            });

            it("should align :ENDCASE at same level as :BEGINCASE", () => {
                const line = "   :ENDCASE;";
                context.blockDepth = 1;

                const result = rule.apply(line, context);

                expect(result).toBe(":ENDCASE;");
            });
        });

        describe("TRY/CATCH blocks", () => {
            it("should align :TRY statement correctly", () => {
                const line = ":TRY;";
                context.blockDepth = 0;

                const result = rule.apply(line, context);

                expect(result).toBe(":TRY;");
            });

            it("should align :CATCH at same level as :TRY", () => {
                const line = "  :CATCH;";
                context.blockDepth = 1;

                const result = rule.apply(line, context);

                expect(result).toBe(":CATCH;");
            });

            it("should align :FINALLY at same level as :TRY", () => {
                const line = "    :FINALLY;";
                context.blockDepth = 1;

                const result = rule.apply(line, context);

                expect(result).toBe(":FINALLY;");
            });

            it("should align :ENDTRY at same level as :TRY", () => {
                const line = "      :ENDTRY;";
                context.blockDepth = 1;

                const result = rule.apply(line, context);

                expect(result).toBe(":ENDTRY;");
            });
        });

        describe("PROCEDURE blocks", () => {
            it("should align :PROCEDURE statement correctly", () => {
                const line = ":PROCEDURE MyFunction;";
                context.blockDepth = 0;

                const result = rule.apply(line, context);

                expect(result).toBe(":PROCEDURE MyFunction;");
            });

            it("should align :ENDPROC at same level as :PROCEDURE", () => {
                const line = "  :ENDPROC;";
                context.blockDepth = 1;

                const result = rule.apply(line, context);

                expect(result).toBe(":ENDPROC;");
            });
        });

        describe("REGION blocks", () => {
            it("should align :REGION statement correctly", () => {
                const line = ":REGION Initialization;";
                context.blockDepth = 0;

                const result = rule.apply(line, context);

                expect(result).toBe(":REGION Initialization;");
            });

            it("should align :ENDREGION at same level as :REGION", () => {
                const line = "  :ENDREGION;";
                context.blockDepth = 1;

                const result = rule.apply(line, context);

                expect(result).toBe(":ENDREGION;");
            });
        });

        describe("CLASS blocks", () => {
            it("should align :CLASS statement correctly", () => {
                const line = ":CLASS MyClass;";
                context.blockDepth = 0;

                const result = rule.apply(line, context);

                expect(result).toBe(":CLASS MyClass;");
            });

            it("should align :ERROR block correctly", () => {
                const line = ":ERROR;";
                context.blockDepth = 1;

                const result = rule.apply(line, context);

                expect(result).toBe("    :ERROR;");
            });
        });
    });

    describe("Content within blocks should apply base indentation", () => {
        it("should indent content inside IF blocks", () => {
            const line = "DoSomething();";
            context.blockDepth = 1;

            const result = rule.apply(line, context);

            expect(result).toBe("    DoSomething();");
        });

        it("should indent nested content correctly", () => {
            const line = "NestedCall();";
            context.blockDepth = 2;

            const result = rule.apply(line, context);

            expect(result).toBe("        NestedCall();");
        });
    });

    describe("Edge cases for block alignment", () => {
        it("should preserve empty lines", () => {
            const line = "";
            context.blockDepth = 1;

            const result = rule.apply(line, context);

            expect(result).toBe("");
        });

        it("should handle case-insensitive keywords", () => {
            const line = ":if condition;";
            context.blockDepth = 0;

            const result = rule.apply(line, context);

            expect(result).toBe(":if condition;");
        });

        it("should handle mixed case keywords", () => {
            const line = ":EndIf;";
            context.blockDepth = 1;

            const result = rule.apply(line, context);

            expect(result).toBe(":EndIf;");
        });
    });
});
