/**
 * SSL Literal Formatter Tests
 *
 * Comprehensive test suite for SSL literal formatting functionality:
 * - String literals with quote preservation
 * - Number literals with scientific notation
 * - Boolean literals (.T., .F.)
 * - Array literals with spacing and line breaking
 * - Date literals with proper component formatting
 * - NIL literals
 * - Code block literals
 */

import {
    ASTNodeType,
    LiteralExpressionNode,
    NumberLiteralNode,
    StringLiteralNode,
    BooleanLiteralNode,
    ArrayLiteralNode,
    NilLiteralNode,
    DateLiteralNode,
    CodeBlockLiteralNode,
} from "../../src/parser/ast";
import { SSLLiteralFormatterVisitor } from "../../src/formatter/literals";
import { FormatterOptions, defaultFormatterOptions } from "../../src/formatter/options";
import { Token, createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("SSLLiteralFormatterVisitor", () => {
    let visitor: SSLLiteralFormatterVisitor;
    let options: FormatterOptions;
    let mockToken: Token;

    beforeEach(() => {
        options = { ...defaultFormatterOptions };
        visitor = new SSLLiteralFormatterVisitor(options);
        mockToken = createToken(TokenType.IDENTIFIER, "test", createPosition(1, 1, 0));
    });

    describe("Literal Expression Formatting", () => {
        it("should format literal expressions using token value", () => {
            const token = createToken(TokenType.STRING, '"hello"', createPosition(1, 1, 0));
            const node: LiteralExpressionNode = {
                kind: ASTNodeType.LiteralExpression,
                startToken: token,
                endToken: token,
                value: "hello",
                token: token,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe('"hello"');
        });

        it("should format literal expressions without token using value", () => {
            const node: LiteralExpressionNode = {
                kind: ASTNodeType.LiteralExpression,
                startToken: mockToken,
                endToken: mockToken,
                value: 42,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("42");
        });

        it("should handle literal expressions with undefined value", () => {
            const node: LiteralExpressionNode = {
                kind: ASTNodeType.LiteralExpression,
                startToken: mockToken,
                endToken: mockToken,
                value: undefined,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("");
        });
    });

    describe("Number Literal Formatting", () => {
        it("should format integer literals", () => {
            const node: NumberLiteralNode = {
                kind: ASTNodeType.NumberLiteral,
                startToken: createToken(TokenType.NUMBER, "42", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.NUMBER, "42", createPosition(1, 1, 0)),
                value: 42,
                raw: "42",
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("42");
        });

        it("should format decimal literals", () => {
            const node: NumberLiteralNode = {
                kind: ASTNodeType.NumberLiteral,
                startToken: createToken(TokenType.NUMBER, "42.5", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.NUMBER, "42.5", createPosition(1, 1, 0)),
                value: 42.5,
                raw: "42.5",
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("42.5");
        });

        it("should format scientific notation literals", () => {
            const testCases = [
                { raw: "1.23e5", value: 123000 },
                { raw: "4.56E-3", value: 0.00456 },
                { raw: "0.5e1", value: 5 },
                { raw: "7.0e2", value: 700 },
            ];

            testCases.forEach(({ raw, value }) => {
                const visitor = new SSLLiteralFormatterVisitor(options);
                const node: NumberLiteralNode = {
                    kind: ASTNodeType.NumberLiteral,
                    startToken: createToken(TokenType.NUMBER, raw, createPosition(1, 1, 0)),
                    endToken: createToken(TokenType.NUMBER, raw, createPosition(1, 1, 0)),
                    value: value,
                    raw: raw,
                } as any;

                const result = visitor.visit(node);

                expect(result.shouldContinue).toBe(false);
                const output = visitor.getFormattedOutput().trim();
                expect(output).toBe(raw);
            });
        });

        it("should handle number literals without raw format", () => {
            const node: NumberLiteralNode = {
                kind: ASTNodeType.NumberLiteral,
                startToken: mockToken,
                endToken: mockToken,
                value: 123.45,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("123.45");
        });

        it("should handle number literals with zero value", () => {
            const node: NumberLiteralNode = {
                kind: ASTNodeType.NumberLiteral,
                startToken: mockToken,
                endToken: mockToken,
                value: 0,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("0");
        });
    });

    describe("String Literal Formatting", () => {
        it("should preserve double quotes in string literals", () => {
            const token = createToken(TokenType.STRING, '"hello world"', createPosition(1, 1, 0));
            const node: StringLiteralNode = {
                kind: ASTNodeType.StringLiteral,
                startToken: token,
                endToken: token,
                value: "hello world",
                token: token,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe('"hello world"');
        });

        it("should preserve single quotes in string literals", () => {
            const token = createToken(TokenType.STRING, "'hello world'", createPosition(1, 1, 0));
            const node: StringLiteralNode = {
                kind: ASTNodeType.StringLiteral,
                startToken: token,
                endToken: token,
                value: "hello world",
                token: token,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("'hello world'");
        });

        it("should handle string literals without token", () => {
            const node: StringLiteralNode = {
                kind: ASTNodeType.StringLiteral,
                startToken: mockToken,
                endToken: mockToken,
                value: "test string",
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe('"test string"');
        });

        it("should escape internal double quotes when defaulting to double quotes", () => {
            const node: StringLiteralNode = {
                kind: ASTNodeType.StringLiteral,
                startToken: mockToken,
                endToken: mockToken,
                value: 'He said "hello"',
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe('"He said \\"hello\\""');
        });

        it("should handle empty string literals", () => {
            const token = createToken(TokenType.STRING, '""', createPosition(1, 1, 0));
            const node: StringLiteralNode = {
                kind: ASTNodeType.StringLiteral,
                startToken: token,
                endToken: token,
                value: "",
                token: token,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe('""');
        });
    });

    describe("Boolean Literal Formatting", () => {
        it("should format true as .T.", () => {
            const node: BooleanLiteralNode = {
                kind: ASTNodeType.BooleanLiteral,
                startToken: createToken(TokenType.BOOLEAN, ".T.", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.BOOLEAN, ".T.", createPosition(1, 1, 0)),
                value: true,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe(".T.");
        });

        it("should format false as .F.", () => {
            const node: BooleanLiteralNode = {
                kind: ASTNodeType.BooleanLiteral,
                startToken: createToken(TokenType.BOOLEAN, ".F.", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.BOOLEAN, ".F.", createPosition(1, 1, 0)),
                value: false,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe(".F.");
        });

        it("should normalize boolean values to canonical format", () => {
            // Test with different input formats that should normalize
            const testCases = [
                { value: true, expected: ".T." },
                { value: false, expected: ".F." },
                { value: 1, expected: ".T." }, // Truthy value
                { value: 0, expected: ".F." }, // Falsy value
            ];

            testCases.forEach(({ value, expected }) => {
                const visitor = new SSLLiteralFormatterVisitor(options);
                const node: BooleanLiteralNode = {
                    kind: ASTNodeType.BooleanLiteral,
                    startToken: mockToken,
                    endToken: mockToken,
                    value: value,
                } as any;

                const result = visitor.visit(node);

                expect(result.shouldContinue).toBe(false);
                const output = visitor.getFormattedOutput().trim();
                expect(output).toBe(expected);
            });
        });
    });

    describe("NIL Literal Formatting", () => {
        it("should format NIL literal in uppercase", () => {
            const node: NilLiteralNode = {
                kind: ASTNodeType.NilLiteral,
                startToken: createToken(TokenType.NIL, "NIL", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.NIL, "NIL", createPosition(1, 1, 0)),
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("NIL");
        });
    });

    describe("Array Literal Formatting", () => {
        beforeEach(() => {
            visitor = new SSLLiteralFormatterVisitor(options);
        });

        it("should format simple array literals with spaces after commas", () => {
            const elem1 = {
                kind: ASTNodeType.LiteralExpression,
                startToken: createToken(TokenType.NUMBER, "1", createPosition(1, 2, 1)),
                endToken: createToken(TokenType.NUMBER, "1", createPosition(1, 2, 1)),
                token: createToken(TokenType.NUMBER, "1", createPosition(1, 2, 1)),
            };

            const elem2 = {
                kind: ASTNodeType.LiteralExpression,
                startToken: createToken(TokenType.NUMBER, "2", createPosition(1, 5, 4)),
                endToken: createToken(TokenType.NUMBER, "2", createPosition(1, 5, 4)),
                token: createToken(TokenType.NUMBER, "2", createPosition(1, 5, 4)),
            };

            const elem3 = {
                kind: ASTNodeType.LiteralExpression,
                startToken: createToken(TokenType.NUMBER, "3", createPosition(1, 8, 7)),
                endToken: createToken(TokenType.NUMBER, "3", createPosition(1, 8, 7)),
                token: createToken(TokenType.NUMBER, "3", createPosition(1, 8, 7)),
            };

            const node: ArrayLiteralNode = {
                kind: ASTNodeType.ArrayLiteral,
                startToken: createToken(TokenType.LBRACE, "{", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.RBRACE, "}", createPosition(1, 9, 8)),
                elements: [elem1, elem2, elem3],
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("{1, 2, 3}");
        });

        it("should format empty array literals", () => {
            const node: ArrayLiteralNode = {
                kind: ASTNodeType.ArrayLiteral,
                startToken: createToken(TokenType.LBRACE, "{", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.RBRACE, "}", createPosition(1, 2, 1)),
                elements: [],
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("{}");
        });

        it("should format array literals without spaces when option is disabled", () => {
            const options = { ...defaultFormatterOptions, insertSpacesAfterCommas: false };
            visitor = new SSLLiteralFormatterVisitor(options);

            const elem1 = {
                kind: ASTNodeType.LiteralExpression,
                token: createToken(TokenType.NUMBER, "1", createPosition(1, 2, 1)),
            };

            const elem2 = {
                kind: ASTNodeType.LiteralExpression,
                token: createToken(TokenType.NUMBER, "2", createPosition(1, 3, 2)),
            };

            const node: ArrayLiteralNode = {
                kind: ASTNodeType.ArrayLiteral,
                startToken: createToken(TokenType.LBRACE, "{", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.RBRACE, "}", createPosition(1, 4, 3)),
                elements: [elem1, elem2],
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("{1,2}");
        });

        it("should break long array literals across lines", () => {
            const options = {
                ...defaultFormatterOptions,
                breakLongArrayLiterals: true,
                parameterListBreakThreshold: 2,
            };
            visitor = new SSLLiteralFormatterVisitor(options);

            const elements = Array.from({ length: 4 }, (_, i) => ({
                kind: ASTNodeType.LiteralExpression,
                token: createToken(
                    TokenType.NUMBER,
                    String(i + 1),
                    createPosition(1, i + 2, i + 1)
                ),
            }));

            const node: ArrayLiteralNode = {
                kind: ASTNodeType.ArrayLiteral,
                startToken: createToken(TokenType.LBRACE, "{", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.RBRACE, "}", createPosition(1, 10, 9)),
                elements: elements,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput();
            const lines = output.split("\n");

            expect(lines[0]).toBe("{");
            expect(lines[1]).toMatch(/^\s+1$/);
            expect(lines[2]).toMatch(/^\s+2$/);
            expect(lines[3]).toMatch(/^\s+3$/);
            expect(lines[4]).toMatch(/^\s+4$/);
            expect(lines[5]).toMatch(/^\s*}$/);
        });

        it("should handle nested array literals", () => {
            const innerArray = {
                kind: ASTNodeType.ArrayLiteral,
                elements: [
                    {
                        kind: ASTNodeType.LiteralExpression,
                        token: createToken(TokenType.NUMBER, "1", createPosition(1, 3, 2)),
                    },
                ],
            };

            const outerElement = {
                kind: ASTNodeType.LiteralExpression,
                token: createToken(TokenType.NUMBER, "2", createPosition(1, 8, 7)),
            };

            const node: ArrayLiteralNode = {
                kind: ASTNodeType.ArrayLiteral,
                startToken: createToken(TokenType.LBRACE, "{", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.RBRACE, "}", createPosition(1, 10, 9)),
                elements: [innerArray, outerElement],
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("{{1}, 2}");
        });
    });

    describe("Date Literal Formatting", () => {
        it("should format date literals with year, month, day", () => {
            const year = {
                kind: ASTNodeType.LiteralExpression,
                token: createToken(TokenType.NUMBER, "2023", createPosition(1, 2, 1)),
            };

            const month = {
                kind: ASTNodeType.LiteralExpression,
                token: createToken(TokenType.NUMBER, "12", createPosition(1, 8, 7)),
            };

            const day = {
                kind: ASTNodeType.LiteralExpression,
                token: createToken(TokenType.NUMBER, "25", createPosition(1, 12, 11)),
            };

            const node: DateLiteralNode = {
                kind: ASTNodeType.DateLiteral,
                startToken: createToken(TokenType.LBRACE, "{", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.RBRACE, "}", createPosition(1, 15, 14)),
                components: [year, month, day],
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("{2023, 12, 25}");
        });

        it("should format date literals with hour, minute, second", () => {
            const components = [
                {
                    kind: ASTNodeType.LiteralExpression,
                    token: createToken(TokenType.NUMBER, "2023", createPosition(1, 2, 1)),
                },
                {
                    kind: ASTNodeType.LiteralExpression,
                    token: createToken(TokenType.NUMBER, "12", createPosition(1, 8, 7)),
                },
                {
                    kind: ASTNodeType.LiteralExpression,
                    token: createToken(TokenType.NUMBER, "25", createPosition(1, 12, 11)),
                },
                {
                    kind: ASTNodeType.LiteralExpression,
                    token: createToken(TokenType.NUMBER, "14", createPosition(1, 16, 15)),
                },
                {
                    kind: ASTNodeType.LiteralExpression,
                    token: createToken(TokenType.NUMBER, "30", createPosition(1, 20, 19)),
                },
                {
                    kind: ASTNodeType.LiteralExpression,
                    token: createToken(TokenType.NUMBER, "45", createPosition(1, 24, 23)),
                },
            ];

            const node: DateLiteralNode = {
                kind: ASTNodeType.DateLiteral,
                startToken: createToken(TokenType.LBRACE, "{", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.RBRACE, "}", createPosition(1, 27, 26)),
                components: components,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("{2023, 12, 25, 14, 30, 45}");
        });

        it("should format date literals without spaces when option is disabled", () => {
            const options = { ...defaultFormatterOptions, insertSpacesAfterCommas: false };
            visitor = new SSLLiteralFormatterVisitor(options);

            const components = [
                {
                    kind: ASTNodeType.LiteralExpression,
                    token: createToken(TokenType.NUMBER, "2023", createPosition(1, 2, 1)),
                },
                {
                    kind: ASTNodeType.LiteralExpression,
                    token: createToken(TokenType.NUMBER, "1", createPosition(1, 7, 6)),
                },
                {
                    kind: ASTNodeType.LiteralExpression,
                    token: createToken(TokenType.NUMBER, "1", createPosition(1, 9, 8)),
                },
            ];

            const node: DateLiteralNode = {
                kind: ASTNodeType.DateLiteral,
                startToken: createToken(TokenType.LBRACE, "{", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.RBRACE, "}", createPosition(1, 11, 10)),
                components: components,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("{2023,1,1}");
        });

        it("should handle empty date literals", () => {
            const node: DateLiteralNode = {
                kind: ASTNodeType.DateLiteral,
                startToken: createToken(TokenType.LBRACE, "{", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.RBRACE, "}", createPosition(1, 2, 1)),
                components: [],
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("{}");
        });
    });

    describe("Code Block Literal Formatting", () => {
        it("should format code block literals with single parameter", () => {
            const param = createToken(TokenType.IDENTIFIER, "x", createPosition(1, 3, 2));
            const bodyExpr = {
                kind: ASTNodeType.ArithmeticExpression,
                startToken: createToken(TokenType.IDENTIFIER, "x", createPosition(1, 6, 5)),
                endToken: createToken(TokenType.IDENTIFIER, "x", createPosition(1, 10, 9)),
            };

            const node: CodeBlockLiteralNode = {
                kind: ASTNodeType.CodeBlockLiteral,
                startToken: createToken(TokenType.CODE_BLOCK_START, "{|", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.RBRACE, "}", createPosition(1, 11, 10)),
                parameters: {
                    kind: ASTNodeType.IdentifierList,
                    identifiers: [{ value: "x" }],
                },
                body: bodyExpr,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("{|x|}");
        });

        it("should format code block literals with multiple parameters", () => {
            const parameters = {
                kind: ASTNodeType.IdentifierList,
                identifiers: [{ value: "a" }, { value: "b" }],
            };

            const bodyExpr = {
                kind: ASTNodeType.ArithmeticExpression,
            };

            const node: CodeBlockLiteralNode = {
                kind: ASTNodeType.CodeBlockLiteral,
                startToken: createToken(TokenType.CODE_BLOCK_START, "{|", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.RBRACE, "}", createPosition(1, 15, 14)),
                parameters: parameters,
                body: bodyExpr,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("{|a, b|}");
        });

        it("should format code block literals with no parameters", () => {
            const bodyExpr = {
                kind: ASTNodeType.LiteralExpression,
                token: createToken(TokenType.NUMBER, "42", createPosition(1, 4, 3)),
            };

            const node: CodeBlockLiteralNode = {
                kind: ASTNodeType.CodeBlockLiteral,
                startToken: createToken(TokenType.CODE_BLOCK_START, "{|", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.RBRACE, "}", createPosition(1, 7, 6)),
                parameters: null,
                body: bodyExpr,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("{|42}");
        });

        it("should format code block literals with array body", () => {
            const bodyExprs = [
                {
                    kind: ASTNodeType.LiteralExpression,
                    token: createToken(TokenType.IDENTIFIER, "x", createPosition(1, 6, 5)),
                },
                {
                    kind: ASTNodeType.LiteralExpression,
                    token: createToken(TokenType.IDENTIFIER, "y", createPosition(1, 9, 8)),
                },
            ];

            const node: CodeBlockLiteralNode = {
                kind: ASTNodeType.CodeBlockLiteral,
                startToken: createToken(TokenType.CODE_BLOCK_START, "{|", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.RBRACE, "}", createPosition(1, 11, 10)),
                parameters: {
                    identifiers: [{ value: "x" }],
                },
                body: bodyExprs,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("{|x|xy}");
        });

        it("should handle code block literals without spaces when option is disabled", () => {
            const options = { ...defaultFormatterOptions, insertSpacesAfterCommas: false };
            visitor = new SSLLiteralFormatterVisitor(options);

            const parameters = {
                identifiers: [{ value: "a" }, { value: "b" }, { value: "c" }],
            };

            const node: CodeBlockLiteralNode = {
                kind: ASTNodeType.CodeBlockLiteral,
                startToken: createToken(TokenType.CODE_BLOCK_START, "{|", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.RBRACE, "}", createPosition(1, 10, 9)),
                parameters: parameters,
                body: null,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("{|a,b,c|}");
        });
    });

    describe("EBNF Grammar Compliance", () => {
        it("should handle all literal types according to EBNF grammar", () => {
            // Test cases covering all literal types from EBNF
            const literalTestCases = [
                {
                    description: "Number literal (integer)",
                    nodeType: ASTNodeType.NumberLiteral,
                    value: 42,
                    expected: "42",
                },
                {
                    description: "Number literal (decimal)",
                    nodeType: ASTNodeType.NumberLiteral,
                    value: 3.14,
                    raw: "3.14",
                    expected: "3.14",
                },
                {
                    description: "String literal (double quotes)",
                    nodeType: ASTNodeType.StringLiteral,
                    value: "hello",
                    token: createToken(TokenType.STRING, '"hello"', createPosition(1, 1, 0)),
                    expected: '"hello"',
                },
                {
                    description: "Boolean literal (true)",
                    nodeType: ASTNodeType.BooleanLiteral,
                    value: true,
                    expected: ".T.",
                },
                {
                    description: "Boolean literal (false)",
                    nodeType: ASTNodeType.BooleanLiteral,
                    value: false,
                    expected: ".F.",
                },
                {
                    description: "NIL literal",
                    nodeType: ASTNodeType.NilLiteral,
                    expected: "NIL",
                },
            ];

            literalTestCases.forEach(({ description, nodeType, value, raw, token, expected }) => {
                const visitor = new SSLLiteralFormatterVisitor(options);
                const node: any = {
                    kind: nodeType,
                    startToken: mockToken,
                    endToken: mockToken,
                    value,
                    raw,
                    token,
                };

                const result = visitor.visit(node);

                expect(result.shouldContinue).toBe(false);
                const output = visitor.getFormattedOutput().trim();
                expect(output).toBe(expected);
            });
        });

        it("should support nested array literals as per EBNF", () => {
            // EBNF: ArrayLiteral ::= "{" [ExpressionList] "}" | "{" ArrayLiteral {"," ArrayLiteral} "}"
            const innerArray1 = {
                kind: ASTNodeType.ArrayLiteral,
                elements: [
                    {
                        kind: ASTNodeType.LiteralExpression,
                        token: createToken(TokenType.NUMBER, "1", createPosition(1, 3, 2)),
                    },
                ],
            };

            const innerArray2 = {
                kind: ASTNodeType.ArrayLiteral,
                elements: [
                    {
                        kind: ASTNodeType.LiteralExpression,
                        token: createToken(TokenType.NUMBER, "2", createPosition(1, 8, 7)),
                    },
                    {
                        kind: ASTNodeType.LiteralExpression,
                        token: createToken(TokenType.NUMBER, "3", createPosition(1, 11, 10)),
                    },
                ],
            };

            const node: ArrayLiteralNode = {
                kind: ASTNodeType.ArrayLiteral,
                startToken: createToken(TokenType.LBRACE, "{", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.RBRACE, "}", createPosition(1, 15, 14)),
                elements: [innerArray1, innerArray2],
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("{{1}, {2, 3}}");
        });

        it("should support scientific notation as per EBNF NumberLiteral rules", () => {
            // EBNF: NumberLiteral ::= IntegerPart ( DecimalPart Exponent? )?
            // Note: SSL requires decimal point before 'e' for scientific notation
            const validScientificNotations = ["1.0e5", "2.5E-3", "0.1e10", "123.456e-2"];

            validScientificNotations.forEach((notation) => {
                const visitor = new SSLLiteralFormatterVisitor(options);
                const node: NumberLiteralNode = {
                    kind: ASTNodeType.NumberLiteral,
                    startToken: createToken(TokenType.NUMBER, notation, createPosition(1, 1, 0)),
                    endToken: createToken(TokenType.NUMBER, notation, createPosition(1, 1, 0)),
                    value: parseFloat(notation),
                    raw: notation,
                } as any;

                const result = visitor.visit(node);

                expect(result.shouldContinue).toBe(false);
                const output = visitor.getFormattedOutput().trim();
                expect(output).toBe(notation);
            });
        });
    });

    describe("Edge Cases and Error Handling", () => {
        it("should handle null/undefined elements gracefully", () => {
            const node: ArrayLiteralNode = {
                kind: ASTNodeType.ArrayLiteral,
                startToken: createToken(TokenType.LBRACE, "{", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.RBRACE, "}", createPosition(1, 2, 1)),
                elements: undefined,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("{}");
        });

        it("should handle very large numbers", () => {
            const largeNumber = Number.MAX_SAFE_INTEGER;
            const node: NumberLiteralNode = {
                kind: ASTNodeType.NumberLiteral,
                startToken: mockToken,
                endToken: mockToken,
                value: largeNumber,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe(String(largeNumber));
        });

        it("should handle special string characters", () => {
            const specialStrings = [
                { input: "line1\nline2", description: "newlines" },
                { input: "tab\tseparated", description: "tabs" },
                { input: 'quote"test', description: "internal quotes" },
                { input: "", description: "empty string" },
            ];

            specialStrings.forEach(({ input, description }) => {
                const visitor = new SSLLiteralFormatterVisitor(options);
                const node: StringLiteralNode = {
                    kind: ASTNodeType.StringLiteral,
                    startToken: mockToken,
                    endToken: mockToken,
                    value: input,
                } as any;

                const result = visitor.visit(node);

                expect(result.shouldContinue).toBe(false);
                const output = visitor.getFormattedOutput().trim();
                expect(output).toContain('"'); // Should be wrapped in quotes
            });
        });
    });
});
