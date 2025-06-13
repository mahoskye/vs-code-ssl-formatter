/**
 * Tests for the Spacing Manager
 *
 * Verifies SSL-specific spacing rules according to EBNF grammar:
 * - Operator spacing (arithmetic, comparison, logical, assignment)
 * - List spacing (commas, parameters, arrays)
 * - Special cases (skipped parameters, property access)
 * - Parentheses and bracket spacing
 * - Function call parameter spacing
 */

import { SpacingManager, SpacingContext, SpacingResult } from "../../src/formatter/spacing";
import { FormatterOptions, defaultFormatterOptions } from "../../src/formatter/options";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("SpacingManager", () => {
    let spacingManager: SpacingManager;
    let options: FormatterOptions;

    beforeEach(() => {
        options = { ...defaultFormatterOptions };
        spacingManager = new SpacingManager(options);
    });

    describe("Comma Spacing", () => {
        it("should add space after comma when option is enabled", () => {
            options.insertSpacesAfterCommas = true;
            spacingManager = new SpacingManager(options);

            const context: SpacingContext = {
                currentTokenType: TokenType.COMMA,
                nextTokenType: TokenType.IDENTIFIER,
            };

            const result = spacingManager.getSpacing(context);
            expect(result.spaceBefore).toBe(false);
            expect(result.spaceAfter).toBe(true);
            expect(result.reason).toBe("Standard comma spacing");
        });

        it("should not add space after comma when option is disabled", () => {
            options.insertSpacesAfterCommas = false;
            spacingManager = new SpacingManager(options);

            const context: SpacingContext = {
                currentTokenType: TokenType.COMMA,
                nextTokenType: TokenType.IDENTIFIER,
            };

            const result = spacingManager.getSpacing(context);
            expect(result.spaceBefore).toBe(false);
            expect(result.spaceAfter).toBe(false);
            expect(result.reason).toBe("Standard comma spacing");
        });

        it("should handle skipped parameters (no space after comma)", () => {
            options.insertSpacesAfterCommas = true;
            spacingManager = new SpacingManager(options);

            const context: SpacingContext = {
                currentTokenType: TokenType.COMMA,
                nextTokenType: TokenType.COMMA,
                isSkippedParameter: true,
            };

            const result = spacingManager.getSpacing(context);
            expect(result.spaceBefore).toBe(false);
            expect(result.spaceAfter).toBe(false);
            expect(result.reason).toBe("Skipped parameter - SSL style");
        });

        it("should handle double comma pattern (param1,,param3)", () => {
            options.insertSpacesAfterCommas = true;
            spacingManager = new SpacingManager(options);

            const context: SpacingContext = {
                currentTokenType: TokenType.COMMA,
                nextTokenType: TokenType.COMMA,
            };

            const result = spacingManager.getSpacing(context);
            expect(result.spaceBefore).toBe(false);
            expect(result.spaceAfter).toBe(false);
            expect(result.reason).toBe("Skipped parameter - SSL style");
        });
    });

    describe("Property Access Colon Spacing", () => {
        it("should not add spaces around colon when option is disabled (SSL default)", () => {
            options.insertSpacesAroundPropertyAccess = false;
            spacingManager = new SpacingManager(options);

            const context: SpacingContext = {
                currentTokenType: TokenType.COLON,
                previousTokenType: TokenType.IDENTIFIER,
                nextTokenType: TokenType.IDENTIFIER,
            };

            const result = spacingManager.getSpacing(context);
            expect(result.spaceBefore).toBe(false);
            expect(result.spaceAfter).toBe(false);
            expect(result.reason).toBe("SSL property access colon");
        });

        it("should add spaces around colon when option is enabled", () => {
            options.insertSpacesAroundPropertyAccess = true;
            spacingManager = new SpacingManager(options);

            const context: SpacingContext = {
                currentTokenType: TokenType.COLON,
                previousTokenType: TokenType.IDENTIFIER,
                nextTokenType: TokenType.IDENTIFIER,
            };

            const result = spacingManager.getSpacing(context);
            expect(result.spaceBefore).toBe(true);
            expect(result.spaceAfter).toBe(true);
            expect(result.reason).toBe("SSL property access colon");
        });
    });

    describe("Semicolon Spacing", () => {
        it("should never add spaces around semicolon (SSL statement terminator)", () => {
            const context: SpacingContext = {
                currentTokenType: TokenType.SEMICOLON,
                previousTokenType: TokenType.IDENTIFIER,
            };

            const result = spacingManager.getSpacing(context);
            expect(result.spaceBefore).toBe(false);
            expect(result.spaceAfter).toBe(false);
            expect(result.reason).toBe("SSL statement terminator");
        });
    });

    describe("Assignment Operator Spacing", () => {
        const assignmentOperators = [
            TokenType.ASSIGN,
            TokenType.PLUS_ASSIGN,
            TokenType.MINUS_ASSIGN,
            TokenType.MULT_ASSIGN,
            TokenType.DIV_ASSIGN,
            TokenType.POWER_ASSIGN,
        ];

        assignmentOperators.forEach((operator) => {
            it(`should add spaces around ${operator} when option is enabled`, () => {
                options.insertSpacesAroundAssignmentOperators = true;
                spacingManager = new SpacingManager(options);

                const context: SpacingContext = {
                    currentTokenType: operator,
                    previousTokenType: TokenType.IDENTIFIER,
                    nextTokenType: TokenType.IDENTIFIER,
                };

                const result = spacingManager.getSpacing(context);
                expect(result.spaceBefore).toBe(true);
                expect(result.spaceAfter).toBe(true);
                expect(result.reason).toBe("Assignment operator");
            });

            it(`should not add spaces around ${operator} when option is disabled`, () => {
                options.insertSpacesAroundAssignmentOperators = false;
                spacingManager = new SpacingManager(options);

                const context: SpacingContext = {
                    currentTokenType: operator,
                    previousTokenType: TokenType.IDENTIFIER,
                    nextTokenType: TokenType.IDENTIFIER,
                };

                const result = spacingManager.getSpacing(context);
                expect(result.spaceBefore).toBe(false);
                expect(result.spaceAfter).toBe(false);
                expect(result.reason).toBe("Assignment operator");
            });
        });
    });

    describe("Comparison Operator Spacing", () => {
        const comparisonOperators = [
            TokenType.EQUAL,
            TokenType.STRICT_EQUAL,
            TokenType.NOT_EQUAL,
            TokenType.LESS_THAN,
            TokenType.GREATER_THAN,
            TokenType.LESS_EQUAL,
            TokenType.GREATER_EQUAL,
        ];

        comparisonOperators.forEach((operator) => {
            it(`should add spaces around ${operator} when option is enabled`, () => {
                options.insertSpacesAroundComparisonOperators = true;
                spacingManager = new SpacingManager(options);

                const context: SpacingContext = {
                    currentTokenType: operator,
                    previousTokenType: TokenType.IDENTIFIER,
                    nextTokenType: TokenType.IDENTIFIER,
                };

                const result = spacingManager.getSpacing(context);
                expect(result.spaceBefore).toBe(true);
                expect(result.spaceAfter).toBe(true);
                expect(result.reason).toBe("Comparison operator");
            });

            it(`should not add spaces around ${operator} when option is disabled`, () => {
                options.insertSpacesAroundComparisonOperators = false;
                spacingManager = new SpacingManager(options);

                const context: SpacingContext = {
                    currentTokenType: operator,
                    previousTokenType: TokenType.IDENTIFIER,
                    nextTokenType: TokenType.IDENTIFIER,
                };

                const result = spacingManager.getSpacing(context);
                expect(result.spaceBefore).toBe(false);
                expect(result.spaceAfter).toBe(false);
                expect(result.reason).toBe("Comparison operator");
            });
        });
    });

    describe("Logical Operator Spacing", () => {
        it("should add spaces around .AND. when option is enabled", () => {
            options.insertSpacesAroundLogicalOperators = true;
            spacingManager = new SpacingManager(options);

            const context: SpacingContext = {
                currentTokenType: TokenType.AND,
                previousTokenType: TokenType.IDENTIFIER,
                nextTokenType: TokenType.IDENTIFIER,
            };

            const result = spacingManager.getSpacing(context);
            expect(result.spaceBefore).toBe(true);
            expect(result.spaceAfter).toBe(true);
            expect(result.reason).toBe("Binary logical operator");
        });

        it("should add spaces around .OR. when option is enabled", () => {
            options.insertSpacesAroundLogicalOperators = true;
            spacingManager = new SpacingManager(options);

            const context: SpacingContext = {
                currentTokenType: TokenType.OR,
                previousTokenType: TokenType.IDENTIFIER,
                nextTokenType: TokenType.IDENTIFIER,
            };

            const result = spacingManager.getSpacing(context);
            expect(result.spaceBefore).toBe(true);
            expect(result.spaceAfter).toBe(true);
            expect(result.reason).toBe("Binary logical operator");
        });

        it("should handle .NOT. as unary operator (space after but not before)", () => {
            options.insertSpacesAroundLogicalOperators = true;
            spacingManager = new SpacingManager(options);

            const context: SpacingContext = {
                currentTokenType: TokenType.NOT,
                nextTokenType: TokenType.IDENTIFIER,
            };

            const result = spacingManager.getSpacing(context);
            expect(result.spaceBefore).toBe(false);
            expect(result.spaceAfter).toBe(true);
            expect(result.reason).toBe("Unary logical NOT operator");
        });
    });

    describe("Arithmetic Operator Spacing", () => {
        const arithmeticOperators = [
            TokenType.PLUS,
            TokenType.MINUS,
            TokenType.MULTIPLY,
            TokenType.DIVIDE,
            TokenType.MODULO,
            TokenType.POWER,
        ];

        arithmeticOperators.forEach((operator) => {
            it(`should add spaces around binary ${operator} when option is enabled`, () => {
                options.insertSpacesAroundOperators = true;
                spacingManager = new SpacingManager(options);

                const context: SpacingContext = {
                    currentTokenType: operator,
                    previousTokenType: TokenType.IDENTIFIER,
                    nextTokenType: TokenType.IDENTIFIER,
                };

                const result = spacingManager.getSpacing(context);
                expect(result.spaceBefore).toBe(true);
                expect(result.spaceAfter).toBe(true);
                expect(result.reason).toBe("Binary arithmetic operator");
            });

            it(`should not add spaces around binary ${operator} when option is disabled`, () => {
                options.insertSpacesAroundOperators = false;
                spacingManager = new SpacingManager(options);

                const context: SpacingContext = {
                    currentTokenType: operator,
                    previousTokenType: TokenType.IDENTIFIER,
                    nextTokenType: TokenType.IDENTIFIER,
                };

                const result = spacingManager.getSpacing(context);
                expect(result.spaceBefore).toBe(false);
                expect(result.spaceAfter).toBe(false);
                expect(result.reason).toBe("Binary arithmetic operator");
            });
        });

        it("should handle unary plus without spaces", () => {
            options.insertSpacesAroundOperators = true;
            spacingManager = new SpacingManager(options);

            const context: SpacingContext = {
                currentTokenType: TokenType.PLUS,
                previousTokenType: TokenType.LPAREN,
                nextTokenType: TokenType.IDENTIFIER,
            };

            const result = spacingManager.getSpacing(context);
            expect(result.spaceBefore).toBe(false);
            expect(result.spaceAfter).toBe(false);
            expect(result.reason).toBe("Unary arithmetic operator");
        });

        it("should handle unary minus without spaces", () => {
            options.insertSpacesAroundOperators = true;
            spacingManager = new SpacingManager(options);

            const context: SpacingContext = {
                currentTokenType: TokenType.MINUS,
                previousTokenType: TokenType.COMMA,
                nextTokenType: TokenType.IDENTIFIER,
            };

            const result = spacingManager.getSpacing(context);
            expect(result.spaceBefore).toBe(false);
            expect(result.spaceAfter).toBe(false);
            expect(result.reason).toBe("Unary arithmetic operator");
        });
    });

    describe("Increment/Decrement Operator Spacing", () => {
        it("should never add spaces around increment operator", () => {
            const context: SpacingContext = {
                currentTokenType: TokenType.INCREMENT,
                previousTokenType: TokenType.IDENTIFIER,
            };

            const result = spacingManager.getSpacing(context);
            expect(result.spaceBefore).toBe(false);
            expect(result.spaceAfter).toBe(false);
            expect(result.reason).toBe("Increment/decrement operator - no spaces");
        });

        it("should never add spaces around decrement operator", () => {
            const context: SpacingContext = {
                currentTokenType: TokenType.DECREMENT,
                nextTokenType: TokenType.IDENTIFIER,
            };

            const result = spacingManager.getSpacing(context);
            expect(result.spaceBefore).toBe(false);
            expect(result.spaceAfter).toBe(false);
            expect(result.reason).toBe("Increment/decrement operator - no spaces");
        });
    });

    describe("Parentheses Spacing", () => {
        it("should not add space before opening parenthesis in function calls", () => {
            const context: SpacingContext = {
                currentTokenType: TokenType.LPAREN,
                previousTokenType: TokenType.IDENTIFIER,
                inFunctionCall: true,
            };

            const result = spacingManager.getSpacing(context);
            expect(result.spaceBefore).toBe(false);
            expect(result.spaceAfter).toBe(false);
            expect(result.reason).toBe("Function call");
        });

        it("should add space before opening parenthesis for grouping", () => {
            const context: SpacingContext = {
                currentTokenType: TokenType.LPAREN,
                previousTokenType: TokenType.IDENTIFIER,
                inFunctionCall: false,
            };

            const result = spacingManager.getSpacing(context);
            expect(result.spaceBefore).toBe(true);
            expect(result.spaceAfter).toBe(false);
            expect(result.reason).toBe("Grouping parentheses");
        });

        it("should add space after closing parenthesis", () => {
            const context: SpacingContext = {
                currentTokenType: TokenType.RPAREN,
                nextTokenType: TokenType.IDENTIFIER,
            };

            const result = spacingManager.getSpacing(context);
            expect(result.spaceBefore).toBe(false);
            expect(result.spaceAfter).toBe(true);
            expect(result.reason).toBe("Closing parenthesis");
        });
    });

    describe("Bracket Spacing", () => {
        it("should not add spaces around brackets", () => {
            const openContext: SpacingContext = {
                currentTokenType: TokenType.LBRACKET,
                previousTokenType: TokenType.IDENTIFIER,
            };

            const closeContext: SpacingContext = {
                currentTokenType: TokenType.RBRACKET,
                nextTokenType: TokenType.IDENTIFIER,
            };

            const openResult = spacingManager.getSpacing(openContext);
            const closeResult = spacingManager.getSpacing(closeContext);

            expect(openResult.spaceBefore).toBe(false);
            expect(openResult.spaceAfter).toBe(false);
            expect(closeResult.spaceBefore).toBe(false);
            expect(closeResult.spaceAfter).toBe(false);
        });
    });

    describe("Brace Spacing", () => {
        it("should add space before opening brace and after closing brace", () => {
            const openContext: SpacingContext = {
                currentTokenType: TokenType.LBRACE,
                previousTokenType: TokenType.IDENTIFIER,
            };

            const closeContext: SpacingContext = {
                currentTokenType: TokenType.RBRACE,
                nextTokenType: TokenType.IDENTIFIER,
            };

            const openResult = spacingManager.getSpacing(openContext);
            const closeResult = spacingManager.getSpacing(closeContext);

            expect(openResult.spaceBefore).toBe(true);
            expect(openResult.spaceAfter).toBe(false);
            expect(closeResult.spaceBefore).toBe(false);
            expect(closeResult.spaceAfter).toBe(true);
        });
    });

    describe("Static Helper Methods", () => {
        describe("shouldAddSpacesAroundOperator", () => {
            it("should return correct spacing for logical operators", () => {
                options.insertSpacesAroundLogicalOperators = true;
                expect(SpacingManager.shouldAddSpacesAroundOperator(TokenType.AND, options)).toBe(
                    true
                );
                expect(SpacingManager.shouldAddSpacesAroundOperator(TokenType.OR, options)).toBe(
                    true
                );

                options.insertSpacesAroundLogicalOperators = false;
                expect(SpacingManager.shouldAddSpacesAroundOperator(TokenType.AND, options)).toBe(
                    false
                );
                expect(SpacingManager.shouldAddSpacesAroundOperator(TokenType.OR, options)).toBe(
                    false
                );
            });

            it("should return correct spacing for comparison operators", () => {
                options.insertSpacesAroundComparisonOperators = true;
                expect(SpacingManager.shouldAddSpacesAroundOperator(TokenType.EQUAL, options)).toBe(
                    true
                );
                expect(
                    SpacingManager.shouldAddSpacesAroundOperator(TokenType.LESS_THAN, options)
                ).toBe(true);

                options.insertSpacesAroundComparisonOperators = false;
                expect(SpacingManager.shouldAddSpacesAroundOperator(TokenType.EQUAL, options)).toBe(
                    false
                );
                expect(
                    SpacingManager.shouldAddSpacesAroundOperator(TokenType.LESS_THAN, options)
                ).toBe(false);
            });

            it("should return correct spacing for assignment operators", () => {
                options.insertSpacesAroundAssignmentOperators = true;
                expect(
                    SpacingManager.shouldAddSpacesAroundOperator(TokenType.ASSIGN, options)
                ).toBe(true);
                expect(
                    SpacingManager.shouldAddSpacesAroundOperator(TokenType.PLUS_ASSIGN, options)
                ).toBe(true);

                options.insertSpacesAroundAssignmentOperators = false;
                expect(
                    SpacingManager.shouldAddSpacesAroundOperator(TokenType.ASSIGN, options)
                ).toBe(false);
                expect(
                    SpacingManager.shouldAddSpacesAroundOperator(TokenType.PLUS_ASSIGN, options)
                ).toBe(false);
            });

            it("should return correct spacing for arithmetic operators", () => {
                options.insertSpacesAroundOperators = true;
                expect(SpacingManager.shouldAddSpacesAroundOperator(TokenType.PLUS, options)).toBe(
                    true
                );
                expect(
                    SpacingManager.shouldAddSpacesAroundOperator(TokenType.MULTIPLY, options)
                ).toBe(true);

                options.insertSpacesAroundOperators = false;
                expect(SpacingManager.shouldAddSpacesAroundOperator(TokenType.PLUS, options)).toBe(
                    false
                );
                expect(
                    SpacingManager.shouldAddSpacesAroundOperator(TokenType.MULTIPLY, options)
                ).toBe(false);
            });

            it("should return correct spacing for property access", () => {
                options.insertSpacesAroundPropertyAccess = true;
                expect(SpacingManager.shouldAddSpacesAroundOperator(TokenType.COLON, options)).toBe(
                    true
                );

                options.insertSpacesAroundPropertyAccess = false;
                expect(SpacingManager.shouldAddSpacesAroundOperator(TokenType.COLON, options)).toBe(
                    false
                );
            });

            it("should never add spaces around increment/decrement", () => {
                expect(
                    SpacingManager.shouldAddSpacesAroundOperator(TokenType.INCREMENT, options)
                ).toBe(false);
                expect(
                    SpacingManager.shouldAddSpacesAroundOperator(TokenType.DECREMENT, options)
                ).toBe(false);
            });
        });

        describe("formatList", () => {
            it("should format simple list with commas and spaces", () => {
                options.insertSpacesAfterCommas = true;
                const items = ["param1", "param2", "param3"];
                const formatter = (item: string) => item;

                const result = SpacingManager.formatList(items, formatter, options);
                expect(result).toBe("param1, param2, param3");
            });

            it("should format list without spaces when option is disabled", () => {
                options.insertSpacesAfterCommas = false;
                const items = ["param1", "param2", "param3"];
                const formatter = (item: string) => item;

                const result = SpacingManager.formatList(items, formatter, options);
                expect(result).toBe("param1,param2,param3");
            });

            it("should handle skipped parameters (param1,,param3)", () => {
                options.insertSpacesAfterCommas = true;
                const items = ["param1", null, "param3"];
                const formatter = (item: string | null) => item || "";

                const result = SpacingManager.formatList(items, formatter, options, true);
                expect(result).toBe("param1,, param3");
            });

            it("should handle multiple skipped parameters", () => {
                options.insertSpacesAfterCommas = true;
                const items = ["param1", null, null, "param4"];
                const formatter = (item: string | null) => item || "";

                const result = SpacingManager.formatList(items, formatter, options, true);
                expect(result).toBe("param1,,, param4");
            });

            it("should not allow skipped elements when disabled", () => {
                options.insertSpacesAfterCommas = true;
                const items = ["param1", null, "param3"];
                const formatter = (item: string | null) => item || "default";

                const result = SpacingManager.formatList(items, formatter, options, false);
                expect(result).toBe("param1, default, param3");
            });
        });

        describe("hasSkippedParameters", () => {
            it("should detect skipped parameters (double comma)", () => {
                const tokens = [
                    TokenType.IDENTIFIER,
                    TokenType.COMMA,
                    TokenType.COMMA,
                    TokenType.IDENTIFIER,
                ];
                expect(SpacingManager.hasSkippedParameters(tokens)).toBe(true);
            });

            it("should not detect skipped parameters in normal list", () => {
                const tokens = [
                    TokenType.IDENTIFIER,
                    TokenType.COMMA,
                    TokenType.IDENTIFIER,
                    TokenType.COMMA,
                    TokenType.IDENTIFIER,
                ];
                expect(SpacingManager.hasSkippedParameters(tokens)).toBe(false);
            });

            it("should detect multiple skipped parameters", () => {
                const tokens = [
                    TokenType.IDENTIFIER,
                    TokenType.COMMA,
                    TokenType.COMMA,
                    TokenType.COMMA,
                    TokenType.IDENTIFIER,
                ];
                expect(SpacingManager.hasSkippedParameters(tokens)).toBe(true);
            });
        });

        describe("getFunctionParameterSpacing", () => {
            it("should return empty strings for last parameter", () => {
                const result = SpacingManager.getFunctionParameterSpacing(2, 3, true, options);
                expect(result.beforeComma).toBe("");
                expect(result.afterComma).toBe("");
            });

            it("should return comma with space for non-last parameter when option is enabled", () => {
                options.insertSpacesAfterCommas = true;
                const result = SpacingManager.getFunctionParameterSpacing(0, 3, false, options);
                expect(result.beforeComma).toBe("");
                expect(result.afterComma).toBe(" ");
            });

            it("should return comma without space for non-last parameter when option is disabled", () => {
                options.insertSpacesAfterCommas = false;
                const result = SpacingManager.getFunctionParameterSpacing(0, 3, false, options);
                expect(result.beforeComma).toBe("");
                expect(result.afterComma).toBe("");
            });
        });
    });

    describe("SSL EBNF Grammar Compliance", () => {
        it("should follow SSL assignment operator spacing (:=)", () => {
            options.insertSpacesAroundAssignmentOperators = true;
            spacingManager = new SpacingManager(options);

            const context: SpacingContext = {
                currentTokenType: TokenType.ASSIGN,
                previousTokenType: TokenType.IDENTIFIER,
                nextTokenType: TokenType.IDENTIFIER,
            };

            const result = spacingManager.getSpacing(context);
            expect(result.spaceBefore).toBe(true);
            expect(result.spaceAfter).toBe(true);
        });

        it("should follow SSL logical operator spacing (.AND., .OR.)", () => {
            options.insertSpacesAroundLogicalOperators = true;
            spacingManager = new SpacingManager(options);

            const andContext: SpacingContext = {
                currentTokenType: TokenType.AND,
                previousTokenType: TokenType.IDENTIFIER,
                nextTokenType: TokenType.IDENTIFIER,
            };

            const orContext: SpacingContext = {
                currentTokenType: TokenType.OR,
                previousTokenType: TokenType.IDENTIFIER,
                nextTokenType: TokenType.IDENTIFIER,
            };

            const andResult = spacingManager.getSpacing(andContext);
            const orResult = spacingManager.getSpacing(orContext);

            expect(andResult.spaceBefore).toBe(true);
            expect(andResult.spaceAfter).toBe(true);
            expect(orResult.spaceBefore).toBe(true);
            expect(orResult.spaceAfter).toBe(true);
        });

        it("should follow SSL property access spacing (object:property)", () => {
            // SSL default is no spaces around colon
            options.insertSpacesAroundPropertyAccess = false;
            spacingManager = new SpacingManager(options);

            const context: SpacingContext = {
                currentTokenType: TokenType.COLON,
                previousTokenType: TokenType.IDENTIFIER,
                nextTokenType: TokenType.IDENTIFIER,
            };

            const result = spacingManager.getSpacing(context);
            expect(result.spaceBefore).toBe(false);
            expect(result.spaceAfter).toBe(false);
        });

        it("should follow SSL parameter list spacing with skipped parameters", () => {
            options.insertSpacesAfterCommas = true;
            const params = ["param1", null, "param3"];
            const formatter = (item: string | null) => item || "";

            const result = SpacingManager.formatList(params, formatter, options, true);
            expect(result).toBe("param1,, param3");
        });

        it("should handle SSL increment/decrement operators without spaces", () => {
            const incrementContext: SpacingContext = {
                currentTokenType: TokenType.INCREMENT,
                previousTokenType: TokenType.IDENTIFIER,
            };

            const decrementContext: SpacingContext = {
                currentTokenType: TokenType.DECREMENT,
                nextTokenType: TokenType.IDENTIFIER,
            };

            const incrementResult = spacingManager.getSpacing(incrementContext);
            const decrementResult = spacingManager.getSpacing(decrementContext);

            expect(incrementResult.spaceBefore).toBe(false);
            expect(incrementResult.spaceAfter).toBe(false);
            expect(decrementResult.spaceBefore).toBe(false);
            expect(decrementResult.spaceAfter).toBe(false);
        });
    });

    describe("Edge Cases", () => {
        it("should handle context with missing previous/next token types", () => {
            const context: SpacingContext = {
                currentTokenType: TokenType.PLUS,
            };

            const result = spacingManager.getSpacing(context);
            expect(result.spaceBefore).toBe(false);
            expect(result.spaceAfter).toBe(false);
            expect(result.reason).toBe("Unary arithmetic operator");
        });

        it("should handle unknown token types with default spacing", () => {
            const context: SpacingContext = {
                currentTokenType: TokenType.IDENTIFIER,
            };

            const result = spacingManager.getSpacing(context);
            expect(result.spaceBefore).toBe(false);
            expect(result.spaceAfter).toBe(false);
            expect(result.reason).toBe("Default - no spacing");
        });

        it("should handle empty lists", () => {
            const result = SpacingManager.formatList([], (item) => item, options);
            expect(result).toBe("");
        });

        it("should handle single-item lists", () => {
            const result = SpacingManager.formatList(["param1"], (item) => item, options);
            expect(result).toBe("param1");
        });

        it("should handle all-skipped parameter lists", () => {
            const items = [null, null, null];
            const formatter = (item: string | null) => item || "";

            const result = SpacingManager.formatList(items, formatter, options, true);
            expect(result).toBe(",,");
        });
    });
});
