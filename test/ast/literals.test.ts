import {
    SpecificLiteralNode,
    LiteralExpressionNode,
    NumberLiteralNode,
    StringLiteralNode,
    BooleanLiteralNode,
    ArrayLiteralNode,
    NilLiteralNode,
    DateLiteralNode,
    CodeBlockLiteralNode,
} from "../../src/parser/ast/literals";
import { ASTNodeType } from "../../src/parser/ast/base";
import { Token, createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("AST Literals", () => {
    let mockToken: Token;
    let mockExpressionNode: any;

    beforeEach(() => {
        mockToken = createToken(TokenType.IDENTIFIER, "test", createPosition(1, 1, 0));

        mockExpressionNode = {
            kind: ASTNodeType.Primary,
            startToken: mockToken,
            endToken: mockToken,
        };
    });

    describe("SpecificLiteralNode", () => {
        it("should have correct structure", () => {
            const literal: SpecificLiteralNode = {
                kind: ASTNodeType.Literal,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(literal.kind).toBe(ASTNodeType.Literal);
            expect(literal.startToken).toBe(mockToken);
            expect(literal.endToken).toBe(mockToken);
        });
    });

    describe("LiteralExpressionNode", () => {
        it("should have correct structure", () => {
            const literalExpression: LiteralExpressionNode = {
                kind: ASTNodeType.LiteralExpression,
                startToken: mockToken,
                endToken: mockToken,
                value: "test value",
                token: mockToken,
            };

            expect(literalExpression.kind).toBe(ASTNodeType.LiteralExpression);
            expect(literalExpression.value).toBe("test value");
            expect(literalExpression.token).toBe(mockToken);
            expect(literalExpression.startToken).toBe(mockToken);
            expect(literalExpression.endToken).toBe(mockToken);
        });

        it("should accept different value types", () => {
            const values = [
                "string value",
                42,
                true,
                false,
                null,
                undefined,
                { key: "value" },
                [1, 2, 3],
            ];

            values.forEach((value) => {
                const literal: LiteralExpressionNode = {
                    kind: ASTNodeType.LiteralExpression,
                    startToken: mockToken,
                    endToken: mockToken,
                    value: value,
                    token: mockToken,
                };

                expect(literal.value).toBe(value);
            });
        });
    });

    describe("NumberLiteralNode", () => {
        it("should have correct structure", () => {
            const numberLiteral: NumberLiteralNode = {
                kind: ASTNodeType.NumberLiteral,
                startToken: mockToken,
                endToken: mockToken,
                value: 42,
                raw: "42",
            };

            expect(numberLiteral.kind).toBe(ASTNodeType.NumberLiteral);
            expect(numberLiteral.value).toBe(42);
            expect(numberLiteral.raw).toBe("42");
            expect(numberLiteral.startToken).toBe(mockToken);
            expect(numberLiteral.endToken).toBe(mockToken);
        });

        it("should support different number formats", () => {
            const numbers = [
                { value: 42, raw: "42" },
                { value: 3.14, raw: "3.14" },
                { value: 0, raw: "0" },
                { value: -1, raw: "-1" },
                { value: 1000000, raw: "1000000" },
                { value: 0.001, raw: "0.001" },
            ];

            numbers.forEach((num) => {
                const literal: NumberLiteralNode = {
                    kind: ASTNodeType.NumberLiteral,
                    startToken: mockToken,
                    endToken: mockToken,
                    value: num.value,
                    raw: num.raw,
                };

                expect(literal.value).toBe(num.value);
                expect(literal.raw).toBe(num.raw);
            });
        });

        it("should handle integer values", () => {
            const integerLiteral: NumberLiteralNode = {
                kind: ASTNodeType.NumberLiteral,
                startToken: mockToken,
                endToken: mockToken,
                value: 123,
                raw: "123",
            };

            expect(Number.isInteger(integerLiteral.value)).toBe(true);
        });

        it("should handle floating point values", () => {
            const floatLiteral: NumberLiteralNode = {
                kind: ASTNodeType.NumberLiteral,
                startToken: mockToken,
                endToken: mockToken,
                value: 123.456,
                raw: "123.456",
            };

            expect(Number.isInteger(floatLiteral.value)).toBe(false);
        });
    });

    describe("StringLiteralNode", () => {
        it("should have correct structure", () => {
            const stringToken = createToken(TokenType.STRING, '"hello"', createPosition(1, 1, 0));
            const stringLiteral: StringLiteralNode = {
                kind: ASTNodeType.StringLiteral,
                startToken: stringToken,
                endToken: stringToken,
                value: "hello",
                token: stringToken,
            };

            expect(stringLiteral.kind).toBe(ASTNodeType.StringLiteral);
            expect(stringLiteral.value).toBe("hello");
            expect(stringLiteral.token).toBe(stringToken);
            expect(stringLiteral.startToken).toBe(stringToken);
            expect(stringLiteral.endToken).toBe(stringToken);
        });

        it("should support different string values", () => {
            const strings = [
                "hello world",
                "",
                "line1\\nline2",
                'with "quotes"',
                "with 'apostrophes'",
                "special chars: !@#$%^&*()",
                "unicode: αβγ",
            ];

            strings.forEach((str) => {
                const stringToken = createToken(
                    TokenType.STRING,
                    `"${str}"`,
                    createPosition(1, 1, 0)
                );
                const literal: StringLiteralNode = {
                    kind: ASTNodeType.StringLiteral,
                    startToken: stringToken,
                    endToken: stringToken,
                    value: str,
                    token: stringToken,
                };

                expect(literal.value).toBe(str);
            });
        });
    });

    describe("BooleanLiteralNode", () => {
        it("should have correct structure for true", () => {
            const booleanLiteral: BooleanLiteralNode = {
                kind: ASTNodeType.BooleanLiteral,
                startToken: mockToken,
                endToken: mockToken,
                value: true,
            };

            expect(booleanLiteral.kind).toBe(ASTNodeType.BooleanLiteral);
            expect(booleanLiteral.value).toBe(true);
            expect(booleanLiteral.startToken).toBe(mockToken);
            expect(booleanLiteral.endToken).toBe(mockToken);
        });

        it("should have correct structure for false", () => {
            const booleanLiteral: BooleanLiteralNode = {
                kind: ASTNodeType.BooleanLiteral,
                startToken: mockToken,
                endToken: mockToken,
                value: false,
            };

            expect(booleanLiteral.kind).toBe(ASTNodeType.BooleanLiteral);
            expect(booleanLiteral.value).toBe(false);
        });

        it("should only accept boolean values", () => {
            const trueLiteral: BooleanLiteralNode = {
                kind: ASTNodeType.BooleanLiteral,
                startToken: mockToken,
                endToken: mockToken,
                value: true,
            };

            const falseLiteral: BooleanLiteralNode = {
                kind: ASTNodeType.BooleanLiteral,
                startToken: mockToken,
                endToken: mockToken,
                value: false,
            };

            expect(typeof trueLiteral.value).toBe("boolean");
            expect(typeof falseLiteral.value).toBe("boolean");
            expect(trueLiteral.value).toBe(true);
            expect(falseLiteral.value).toBe(false);
        });
    });

    describe("ArrayLiteralNode", () => {
        it("should have correct structure", () => {
            const arrayLiteral: ArrayLiteralNode = {
                kind: ASTNodeType.ArrayLiteral,
                startToken: mockToken,
                endToken: mockToken,
                elements: [mockExpressionNode],
            };

            expect(arrayLiteral.kind).toBe(ASTNodeType.ArrayLiteral);
            expect(arrayLiteral.elements).toHaveLength(1);
            expect(arrayLiteral.elements[0]).toBe(mockExpressionNode);
            expect(arrayLiteral.startToken).toBe(mockToken);
            expect(arrayLiteral.endToken).toBe(mockToken);
        });

        it("should support empty arrays", () => {
            const emptyArrayLiteral: ArrayLiteralNode = {
                kind: ASTNodeType.ArrayLiteral,
                startToken: mockToken,
                endToken: mockToken,
                elements: [],
            };

            expect(emptyArrayLiteral.elements).toHaveLength(0);
            expect(Array.isArray(emptyArrayLiteral.elements)).toBe(true);
        });

        it("should support multiple elements", () => {
            const multiElementArray: ArrayLiteralNode = {
                kind: ASTNodeType.ArrayLiteral,
                startToken: mockToken,
                endToken: mockToken,
                elements: [mockExpressionNode, mockExpressionNode, mockExpressionNode],
            };

            expect(multiElementArray.elements).toHaveLength(3);
            multiElementArray.elements.forEach((element) => {
                expect(element).toBe(mockExpressionNode);
            });
        });
    });

    describe("NilLiteralNode", () => {
        it("should have correct structure", () => {
            const nilLiteral: NilLiteralNode = {
                kind: ASTNodeType.NilLiteral,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(nilLiteral.kind).toBe(ASTNodeType.NilLiteral);
            expect(nilLiteral.startToken).toBe(mockToken);
            expect(nilLiteral.endToken).toBe(mockToken);
        });
    });

    describe("DateLiteralNode", () => {
        it("should have correct structure", () => {
            const dateLiteral: DateLiteralNode = {
                kind: ASTNodeType.DateLiteral,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(dateLiteral.kind).toBe(ASTNodeType.DateLiteral);
            expect(dateLiteral.startToken).toBe(mockToken);
            expect(dateLiteral.endToken).toBe(mockToken);
        });
    });

    describe("CodeBlockLiteralNode", () => {
        it("should have correct structure", () => {
            const codeBlockLiteral: CodeBlockLiteralNode = {
                kind: ASTNodeType.CodeBlockLiteral,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(codeBlockLiteral.kind).toBe(ASTNodeType.CodeBlockLiteral);
            expect(codeBlockLiteral.startToken).toBe(mockToken);
            expect(codeBlockLiteral.endToken).toBe(mockToken);
        });
    });

    describe("Literal type consistency", () => {
        it("should have consistent naming for all literal types", () => {
            const literalTypes = [
                ASTNodeType.Literal,
                ASTNodeType.LiteralExpression,
                ASTNodeType.NumberLiteral,
                ASTNodeType.StringLiteral,
                ASTNodeType.BooleanLiteral,
                ASTNodeType.ArrayLiteral,
                ASTNodeType.NilLiteral,
                ASTNodeType.DateLiteral,
                ASTNodeType.CodeBlockLiteral,
            ];

            literalTypes.forEach((type) => {
                expect(type).toMatch(/(Literal|LiteralExpression)/);
            });
        });

        it("should have unique values for all literal types", () => {
            const literalTypes = [
                ASTNodeType.Literal,
                ASTNodeType.LiteralExpression,
                ASTNodeType.NumberLiteral,
                ASTNodeType.StringLiteral,
                ASTNodeType.BooleanLiteral,
                ASTNodeType.ArrayLiteral,
                ASTNodeType.NilLiteral,
                ASTNodeType.DateLiteral,
                ASTNodeType.CodeBlockLiteral,
            ];

            const uniqueTypes = new Set(literalTypes);
            expect(literalTypes.length).toBe(uniqueTypes.size);
        });
    });
});
