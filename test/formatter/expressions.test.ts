/**
 * SSL Expression Formatter Tests
 *
 * Tests for all expression formatting functionality including:
 * - Binary expressions with proper spacing
 * - Unary expressions
 * - Property access (colon notation)
 * - Array access and subscripts
 * - Function calls and method calls
 * - Literal expressions
 */

import {
    ASTNodeType,
    BinaryExpressionNode,
    LogicalExpressionNode,
    ComparisonExpressionNode,
    ArithmeticExpressionNode,
    TermNode,
    FactorNode,
    PowerOperandNode,
    UnaryExpressionNode,
    IncrementExpressionNode,
    VariableAccessNode,
    PropertyAccessNode,
    ArrayAccessNode,
    ArraySubscriptNode,
    LiteralExpressionNode,
    NumberLiteralNode,
    StringLiteralNode,
    BooleanLiteralNode,
    NilLiteralNode,
    ArrayLiteralNode,
    DateLiteralNode,
    CodeBlockLiteralNode,
    MethodCallNode,
    DirectFunctionCallNode,
    DoProcCallNode,
    ExecFunctionCallNode,
    BitwiseOperationNode,
    ObjectCreationNode,
} from "../../src/parser/ast";
import { SSLExpressionFormatterVisitor } from "../../src/formatter/expressions";
import { FormatterOptions, defaultFormatterOptions } from "../../src/formatter/options";
import { Token, createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("SSLExpressionFormatterVisitor", () => {
    let visitor: SSLExpressionFormatterVisitor;
    let options: FormatterOptions;
    let mockToken: Token;
    let mockExpression: any;

    beforeEach(() => {
        options = { ...defaultFormatterOptions };
        visitor = new SSLExpressionFormatterVisitor(options);
        mockToken = createToken(TokenType.IDENTIFIER, "test", createPosition(1, 1, 0));

        // Create a mock expression for testing
        mockExpression = {
            kind: ASTNodeType.VariableAccess,
            startToken: mockToken,
            endToken: mockToken,
            name: mockToken,
        };
    });

    describe("Binary Expression Formatting", () => {
        describe("Logical Expressions", () => {
            it("should format .AND. expressions with spaces", () => {
                const leftExpr = {
                    kind: ASTNodeType.VariableAccess,
                    startToken: createToken(TokenType.IDENTIFIER, "a", createPosition(1, 1, 0)),
                    endToken: createToken(TokenType.IDENTIFIER, "a", createPosition(1, 1, 0)),
                    name: createToken(TokenType.IDENTIFIER, "a", createPosition(1, 1, 0)),
                };

                const rightExpr = {
                    kind: ASTNodeType.VariableAccess,
                    startToken: createToken(TokenType.IDENTIFIER, "b", createPosition(1, 1, 0)),
                    endToken: createToken(TokenType.IDENTIFIER, "b", createPosition(1, 1, 0)),
                    name: createToken(TokenType.IDENTIFIER, "b", createPosition(1, 1, 0)),
                };

                const node: LogicalExpressionNode = {
                    kind: ASTNodeType.LogicalExpression,
                    startToken: leftExpr.startToken,
                    endToken: rightExpr.endToken,
                    left: leftExpr,
                    operator: createToken(TokenType.AND, ".AND.", createPosition(1, 3, 2)),
                    right: rightExpr,
                };

                const result = visitor.visit(node);

                expect(result.shouldContinue).toBe(false);
                expect(result.error).toBeUndefined();

                const output = visitor.getFormattedOutput().trim();
                expect(output).toBe("a .AND. b");
            });

            it("should format .OR. expressions with spaces", () => {
                const leftExpr = {
                    kind: ASTNodeType.VariableAccess,
                    startToken: createToken(TokenType.IDENTIFIER, "x", createPosition(1, 1, 0)),
                    endToken: createToken(TokenType.IDENTIFIER, "x", createPosition(1, 1, 0)),
                    name: createToken(TokenType.IDENTIFIER, "x", createPosition(1, 1, 0)),
                };

                const rightExpr = {
                    kind: ASTNodeType.VariableAccess,
                    startToken: createToken(TokenType.IDENTIFIER, "y", createPosition(1, 1, 0)),
                    endToken: createToken(TokenType.IDENTIFIER, "y", createPosition(1, 1, 0)),
                    name: createToken(TokenType.IDENTIFIER, "y", createPosition(1, 1, 0)),
                };

                const node: LogicalExpressionNode = {
                    kind: ASTNodeType.LogicalExpression,
                    startToken: leftExpr.startToken,
                    endToken: rightExpr.endToken,
                    left: leftExpr,
                    operator: createToken(TokenType.OR, ".OR.", createPosition(1, 3, 2)),
                    right: rightExpr,
                };

                const result = visitor.visit(node);

                expect(result.shouldContinue).toBe(false);
                const output = visitor.getFormattedOutput().trim();
                expect(output).toBe("x .OR. y");
            });
        });

        describe("Comparison Expressions", () => {
            it("should format equality expressions with proper spacing", () => {
                const leftExpr = mockExpression;
                const rightExpr = {
                    kind: ASTNodeType.LiteralExpression,
                    startToken: createToken(TokenType.NUMBER, "42", createPosition(1, 1, 0)),
                    endToken: createToken(TokenType.NUMBER, "42", createPosition(1, 1, 0)),
                    token: createToken(TokenType.NUMBER, "42", createPosition(1, 1, 0)),
                };

                const node: ComparisonExpressionNode = {
                    kind: ASTNodeType.ComparisonExpression,
                    startToken: leftExpr.startToken,
                    endToken: rightExpr.endToken,
                    left: leftExpr,
                    operator: createToken(TokenType.STRICT_EQUAL, "==", createPosition(1, 5, 4)),
                    right: rightExpr,
                };

                const result = visitor.visit(node);

                expect(result.shouldContinue).toBe(false);
                const output = visitor.getFormattedOutput().trim();
                expect(output).toBe("test == 42");
            });

            it("should format inequality expressions", () => {
                const operators = [
                    { token: TokenType.NOT_EQUAL, value: "!=" },
                    { token: TokenType.LESS_THAN, value: "<" },
                    { token: TokenType.GREATER_THAN, value: ">" },
                    { token: TokenType.LESS_EQUAL, value: "<=" },
                    { token: TokenType.GREATER_EQUAL, value: ">=" },
                ];

                operators.forEach((op) => {
                    visitor = new SSLExpressionFormatterVisitor(options);

                    const node: ComparisonExpressionNode = {
                        kind: ASTNodeType.ComparisonExpression,
                        startToken: mockToken,
                        endToken: mockToken,
                        left: mockExpression,
                        operator: createToken(op.token, op.value, createPosition(1, 5, 4)),
                        right: mockExpression,
                    };

                    visitor.visit(node);
                    const output = visitor.getFormattedOutput().trim();
                    expect(output).toBe(`test ${op.value} test`);
                });
            });
        });

        describe("Arithmetic Expressions", () => {
            it("should format addition and subtraction with spaces", () => {
                const operators = [
                    { token: TokenType.PLUS, value: "+" },
                    { token: TokenType.MINUS, value: "-" },
                ];

                operators.forEach((op) => {
                    visitor = new SSLExpressionFormatterVisitor(options);

                    const node: ArithmeticExpressionNode = {
                        kind: ASTNodeType.ArithmeticExpression,
                        startToken: mockToken,
                        endToken: mockToken,
                        left: mockExpression,
                        operator: createToken(op.token, op.value, createPosition(1, 5, 4)),
                        right: mockExpression,
                    };

                    visitor.visit(node);
                    const output = visitor.getFormattedOutput().trim();
                    expect(output).toBe(`test ${op.value} test`);
                });
            });

            it("should format multiplication and division with spaces", () => {
                const operators = [
                    { token: TokenType.MULTIPLY, value: "*" },
                    { token: TokenType.DIVIDE, value: "/" },
                    { token: TokenType.MODULO, value: "%" },
                ];

                operators.forEach((op) => {
                    visitor = new SSLExpressionFormatterVisitor(options);

                    const node: TermNode = {
                        kind: ASTNodeType.Term,
                        startToken: mockToken,
                        endToken: mockToken,
                        left: mockExpression,
                        operator: createToken(op.token, op.value, createPosition(1, 5, 4)),
                        right: mockExpression,
                    } as any;

                    visitor.visit(node);
                    const output = visitor.getFormattedOutput().trim();
                    expect(output).toBe(`test ${op.value} test`);
                });
            });

            it("should format power expressions", () => {
                visitor = new SSLExpressionFormatterVisitor(options);

                const node: FactorNode = {
                    kind: ASTNodeType.Factor,
                    startToken: mockToken,
                    endToken: mockToken,
                    left: mockExpression,
                    operator: createToken(TokenType.POWER, "^", createPosition(1, 5, 4)),
                    right: mockExpression,
                } as any;

                visitor.visit(node);
                const output = visitor.getFormattedOutput().trim();
                expect(output).toBe("test ^ test");
            });
        });

        describe("Spacing Configuration", () => {
            it("should respect spacing configuration for operators", () => {
                options.insertSpacesAroundOperators = false;
                visitor = new SSLExpressionFormatterVisitor(options);

                const node: ArithmeticExpressionNode = {
                    kind: ASTNodeType.ArithmeticExpression,
                    startToken: mockToken,
                    endToken: mockToken,
                    left: mockExpression,
                    operator: createToken(TokenType.PLUS, "+", createPosition(1, 5, 4)),
                    right: mockExpression,
                };

                visitor.visit(node);
                const output = visitor.getFormattedOutput().trim();
                expect(output).toBe("test+test");
            });

            it("should respect spacing configuration for comparison operators", () => {
                options.insertSpacesAroundComparisonOperators = false;
                visitor = new SSLExpressionFormatterVisitor(options);

                const node: ComparisonExpressionNode = {
                    kind: ASTNodeType.ComparisonExpression,
                    startToken: mockToken,
                    endToken: mockToken,
                    left: mockExpression,
                    operator: createToken(TokenType.STRICT_EQUAL, "==", createPosition(1, 5, 4)),
                    right: mockExpression,
                };

                visitor.visit(node);
                const output = visitor.getFormattedOutput().trim();
                expect(output).toBe("test==test");
            });
        });
    });

    describe("Unary Expression Formatting", () => {
        it("should format unary minus", () => {
            const operand = {
                kind: ASTNodeType.VariableAccess,
                startToken: createToken(TokenType.IDENTIFIER, "value", createPosition(1, 2, 1)),
                endToken: createToken(TokenType.IDENTIFIER, "value", createPosition(1, 2, 1)),
                name: createToken(TokenType.IDENTIFIER, "value", createPosition(1, 2, 1)),
            };

            const node: UnaryExpressionNode = {
                kind: ASTNodeType.UnaryExpression,
                startToken: createToken(TokenType.MINUS, "-", createPosition(1, 1, 0)),
                endToken: operand.endToken,
                operator: createToken(TokenType.MINUS, "-", createPosition(1, 1, 0)),
                operand,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("-value");
        });

        it("should format .NOT. with space", () => {
            const operand = {
                kind: ASTNodeType.VariableAccess,
                startToken: createToken(TokenType.IDENTIFIER, "flag", createPosition(1, 6, 5)),
                endToken: createToken(TokenType.IDENTIFIER, "flag", createPosition(1, 6, 5)),
                name: createToken(TokenType.IDENTIFIER, "flag", createPosition(1, 6, 5)),
            };

            const node: UnaryExpressionNode = {
                kind: ASTNodeType.UnaryExpression,
                startToken: createToken(TokenType.NOT, ".NOT.", createPosition(1, 1, 0)),
                endToken: operand.endToken,
                operator: createToken(TokenType.NOT, ".NOT.", createPosition(1, 1, 0)),
                operand,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe(".NOT. flag");
        });

        it("should format logical not (!)", () => {
            const operand = {
                kind: ASTNodeType.VariableAccess,
                startToken: createToken(TokenType.IDENTIFIER, "condition", createPosition(1, 2, 1)),
                endToken: createToken(TokenType.IDENTIFIER, "condition", createPosition(1, 2, 1)),
                name: createToken(TokenType.IDENTIFIER, "condition", createPosition(1, 2, 1)),
            };

            const node: UnaryExpressionNode = {
                kind: ASTNodeType.UnaryExpression,
                startToken: createToken(TokenType.LOGICAL_NOT, "!", createPosition(1, 1, 0)),
                endToken: operand.endToken,
                operator: createToken(TokenType.LOGICAL_NOT, "!", createPosition(1, 1, 0)),
                operand,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("! condition");
        });
    });

    describe("Increment/Decrement Expression Formatting", () => {
        it("should format prefix increment", () => {
            const node: IncrementExpressionNode = {
                kind: ASTNodeType.IncrementExpression,
                startToken: createToken(TokenType.INCREMENT, "++", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.IDENTIFIER, "counter", createPosition(1, 3, 2)),
                prefix: true,
                operator: createToken(TokenType.INCREMENT, "++", createPosition(1, 1, 0)),
                operand: createToken(TokenType.IDENTIFIER, "counter", createPosition(1, 3, 2)),
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("++counter");
        });

        it("should format postfix increment", () => {
            const node: IncrementExpressionNode = {
                kind: ASTNodeType.IncrementExpression,
                startToken: createToken(TokenType.IDENTIFIER, "counter", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.INCREMENT, "++", createPosition(1, 8, 7)),
                prefix: false,
                operator: createToken(TokenType.INCREMENT, "++", createPosition(1, 8, 7)),
                operand: createToken(TokenType.IDENTIFIER, "counter", createPosition(1, 1, 0)),
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("counter++");
        });

        it("should format decrement operators", () => {
            const prefixNode: IncrementExpressionNode = {
                kind: ASTNodeType.IncrementExpression,
                startToken: createToken(TokenType.DECREMENT, "--", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.IDENTIFIER, "index", createPosition(1, 3, 2)),
                prefix: true,
                operator: createToken(TokenType.DECREMENT, "--", createPosition(1, 1, 0)),
                operand: createToken(TokenType.IDENTIFIER, "index", createPosition(1, 3, 2)),
            } as any;

            visitor.visit(prefixNode);
            let output = visitor.getFormattedOutput().trim();
            expect(output).toBe("--index");

            // Reset visitor for postfix test
            visitor = new SSLExpressionFormatterVisitor(options);

            const postfixNode: IncrementExpressionNode = {
                kind: ASTNodeType.IncrementExpression,
                startToken: createToken(TokenType.IDENTIFIER, "index", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.DECREMENT, "--", createPosition(1, 6, 5)),
                prefix: false,
                operator: createToken(TokenType.DECREMENT, "--", createPosition(1, 6, 5)),
                operand: createToken(TokenType.IDENTIFIER, "index", createPosition(1, 1, 0)),
            } as any;

            visitor.visit(postfixNode);
            output = visitor.getFormattedOutput().trim();
            expect(output).toBe("index--");
        });
    });

    describe("Access Expression Formatting", () => {
        it("should format variable access", () => {
            const node: VariableAccessNode = {
                kind: ASTNodeType.VariableAccess,
                startToken: createToken(
                    TokenType.IDENTIFIER,
                    "sVariableName",
                    createPosition(1, 1, 0)
                ),
                endToken: createToken(
                    TokenType.IDENTIFIER,
                    "sVariableName",
                    createPosition(1, 1, 0)
                ),
                name: createToken(TokenType.IDENTIFIER, "sVariableName", createPosition(1, 1, 0)),
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("sVariableName");
        });

        it("should format property access without spaces", () => {
            const node: PropertyAccessNode = {
                kind: ASTNodeType.PropertyAccess,
                startToken: createToken(TokenType.IDENTIFIER, "oObject", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.IDENTIFIER, "sProperty", createPosition(1, 9, 8)),
                object: createToken(TokenType.IDENTIFIER, "oObject", createPosition(1, 1, 0)),
                property: createToken(TokenType.IDENTIFIER, "sProperty", createPosition(1, 9, 8)),
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("oObject:sProperty");
        });

        it("should format property access with spaces when configured", () => {
            options.insertSpacesAroundPropertyAccess = true;
            visitor = new SSLExpressionFormatterVisitor(options);

            const node: PropertyAccessNode = {
                kind: ASTNodeType.PropertyAccess,
                startToken: createToken(TokenType.IDENTIFIER, "oObject", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.IDENTIFIER, "sProperty", createPosition(1, 9, 8)),
                object: createToken(TokenType.IDENTIFIER, "oObject", createPosition(1, 1, 0)),
                property: createToken(TokenType.IDENTIFIER, "sProperty", createPosition(1, 9, 8)),
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("oObject : sProperty");
        });

        it("should format array access with single index", () => {
            const arrayExpr = {
                kind: ASTNodeType.VariableAccess,
                startToken: createToken(TokenType.IDENTIFIER, "aArray", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.IDENTIFIER, "aArray", createPosition(1, 1, 0)),
                name: createToken(TokenType.IDENTIFIER, "aArray", createPosition(1, 1, 0)),
            };

            const indexExpr = {
                kind: ASTNodeType.LiteralExpression,
                startToken: createToken(TokenType.NUMBER, "1", createPosition(1, 8, 7)),
                endToken: createToken(TokenType.NUMBER, "1", createPosition(1, 8, 7)),
                token: createToken(TokenType.NUMBER, "1", createPosition(1, 8, 7)),
            };

            const node: ArrayAccessNode = {
                kind: ASTNodeType.ArrayAccess,
                startToken: arrayExpr.startToken,
                endToken: createToken(TokenType.RBRACKET, "]", createPosition(1, 9, 8)),
                array: arrayExpr,
                indices: [indexExpr],
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("aArray[1]");
        });

        it("should format array access with multiple indices", () => {
            const arrayExpr = {
                kind: ASTNodeType.VariableAccess,
                startToken: createToken(TokenType.IDENTIFIER, "aMatrix", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.IDENTIFIER, "aMatrix", createPosition(1, 1, 0)),
                name: createToken(TokenType.IDENTIFIER, "aMatrix", createPosition(1, 1, 0)),
            };

            const index1 = {
                kind: ASTNodeType.LiteralExpression,
                startToken: createToken(TokenType.NUMBER, "2", createPosition(1, 9, 8)),
                endToken: createToken(TokenType.NUMBER, "2", createPosition(1, 9, 8)),
                token: createToken(TokenType.NUMBER, "2", createPosition(1, 9, 8)),
            };

            const index2 = {
                kind: ASTNodeType.LiteralExpression,
                startToken: createToken(TokenType.NUMBER, "3", createPosition(1, 12, 11)),
                endToken: createToken(TokenType.NUMBER, "3", createPosition(1, 12, 11)),
                token: createToken(TokenType.NUMBER, "3", createPosition(1, 12, 11)),
            };

            const node: ArrayAccessNode = {
                kind: ASTNodeType.ArrayAccess,
                startToken: arrayExpr.startToken,
                endToken: createToken(TokenType.RBRACKET, "]", createPosition(1, 13, 12)),
                array: arrayExpr,
                indices: [index1, index2],
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("aMatrix[2, 3]");
        });

        it("should format array access without comma spaces when disabled", () => {
            options.insertSpacesAfterCommas = false;
            visitor = new SSLExpressionFormatterVisitor(options);

            const arrayExpr = {
                kind: ASTNodeType.VariableAccess,
                startToken: createToken(TokenType.IDENTIFIER, "aMatrix", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.IDENTIFIER, "aMatrix", createPosition(1, 1, 0)),
                name: createToken(TokenType.IDENTIFIER, "aMatrix", createPosition(1, 1, 0)),
            };

            const index1 = {
                kind: ASTNodeType.LiteralExpression,
                startToken: createToken(TokenType.NUMBER, "1", createPosition(1, 9, 8)),
                endToken: createToken(TokenType.NUMBER, "1", createPosition(1, 9, 8)),
                token: createToken(TokenType.NUMBER, "1", createPosition(1, 9, 8)),
            };

            const index2 = {
                kind: ASTNodeType.LiteralExpression,
                startToken: createToken(TokenType.NUMBER, "2", createPosition(1, 11, 10)),
                endToken: createToken(TokenType.NUMBER, "2", createPosition(1, 11, 10)),
                token: createToken(TokenType.NUMBER, "2", createPosition(1, 11, 10)),
            };

            const node: ArrayAccessNode = {
                kind: ASTNodeType.ArrayAccess,
                startToken: arrayExpr.startToken,
                endToken: createToken(TokenType.RBRACKET, "]", createPosition(1, 12, 11)),
                array: arrayExpr,
                indices: [index1, index2],
            } as any;

            visitor.visit(node);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("aMatrix[1,2]");
        });
    });

    describe("Literal Expression Formatting", () => {
        it("should format number literals", () => {
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

        it("should format string literals with quotes", () => {
            const node: StringLiteralNode = {
                kind: ASTNodeType.StringLiteral,
                startToken: createToken(TokenType.STRING, '"hello world"', createPosition(1, 1, 0)),
                endToken: createToken(TokenType.STRING, '"hello world"', createPosition(1, 1, 0)),
                value: "hello world",
                token: createToken(TokenType.STRING, '"hello world"', createPosition(1, 1, 0)),
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe('"hello world"');
        });

        it("should format boolean literals", () => {
            const trueNode: BooleanLiteralNode = {
                kind: ASTNodeType.BooleanLiteral,
                startToken: createToken(TokenType.BOOLEAN, ".T.", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.BOOLEAN, ".T.", createPosition(1, 1, 0)),
                value: true,
            } as any;

            visitor.visit(trueNode);
            let output = visitor.getFormattedOutput().trim();
            expect(output).toBe(".T.");

            // Reset visitor for false test
            visitor = new SSLExpressionFormatterVisitor(options);

            const falseNode: BooleanLiteralNode = {
                kind: ASTNodeType.BooleanLiteral,
                startToken: createToken(TokenType.BOOLEAN, ".F.", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.BOOLEAN, ".F.", createPosition(1, 1, 0)),
                value: false,
            } as any;

            visitor.visit(falseNode);
            output = visitor.getFormattedOutput().trim();
            expect(output).toBe(".F.");
        });

        it("should format NIL literal", () => {
            const node: NilLiteralNode = {
                kind: ASTNodeType.NilLiteral,
                startToken: createToken(TokenType.NIL, "NIL", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.NIL, "NIL", createPosition(1, 1, 0)),
            };

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("NIL");
        });

        it("should format simple array literals", () => {
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

        it("should format date literals", () => {
            const year = {
                kind: ASTNodeType.LiteralExpression,
                startToken: createToken(TokenType.NUMBER, "2023", createPosition(1, 2, 1)),
                endToken: createToken(TokenType.NUMBER, "2023", createPosition(1, 2, 1)),
                token: createToken(TokenType.NUMBER, "2023", createPosition(1, 2, 1)),
            };

            const month = {
                kind: ASTNodeType.LiteralExpression,
                startToken: createToken(TokenType.NUMBER, "12", createPosition(1, 8, 7)),
                endToken: createToken(TokenType.NUMBER, "12", createPosition(1, 8, 7)),
                token: createToken(TokenType.NUMBER, "12", createPosition(1, 8, 7)),
            };

            const day = {
                kind: ASTNodeType.LiteralExpression,
                startToken: createToken(TokenType.NUMBER, "25", createPosition(1, 12, 11)),
                endToken: createToken(TokenType.NUMBER, "25", createPosition(1, 12, 11)),
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

        it("should format code block literals", () => {
            const param = createToken(TokenType.IDENTIFIER, "x", createPosition(1, 3, 2));
            const bodyExpr = {
                kind: ASTNodeType.ArithmeticExpression,
                startToken: createToken(TokenType.IDENTIFIER, "x", createPosition(1, 6, 5)),
                endToken: createToken(TokenType.IDENTIFIER, "x", createPosition(1, 10, 9)),
                left: {
                    kind: ASTNodeType.VariableAccess,
                    startToken: createToken(TokenType.IDENTIFIER, "x", createPosition(1, 6, 5)),
                    endToken: createToken(TokenType.IDENTIFIER, "x", createPosition(1, 6, 5)),
                    name: createToken(TokenType.IDENTIFIER, "x", createPosition(1, 6, 5)),
                },
                operator: createToken(TokenType.MULTIPLY, "*", createPosition(1, 8, 7)),
                right: {
                    kind: ASTNodeType.VariableAccess,
                    startToken: createToken(TokenType.IDENTIFIER, "x", createPosition(1, 10, 9)),
                    endToken: createToken(TokenType.IDENTIFIER, "x", createPosition(1, 10, 9)),
                    name: createToken(TokenType.IDENTIFIER, "x", createPosition(1, 10, 9)),
                },
            };

            const node: CodeBlockLiteralNode = {
                kind: ASTNodeType.CodeBlockLiteral,
                startToken: createToken(TokenType.CODE_BLOCK_START, "{|", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.RBRACE, "}", createPosition(1, 11, 10)),
                parameters: {
                    kind: ASTNodeType.IdentifierList,
                    startToken: param,
                    endToken: param,
                    identifiers: [param],
                },
                body: [bodyExpr],
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("{|x|x * x}");
        });
    });

    describe("Function Call Formatting", () => {
        it("should format direct function calls", () => {
            const arg1 = {
                kind: ASTNodeType.LiteralExpression,
                startToken: createToken(TokenType.STRING, '"test"', createPosition(1, 12, 11)),
                endToken: createToken(TokenType.STRING, '"test"', createPosition(1, 12, 11)),
                token: createToken(TokenType.STRING, '"test"', createPosition(1, 12, 11)),
            };

            const arg2 = {
                kind: ASTNodeType.LiteralExpression,
                startToken: createToken(TokenType.NUMBER, "42", createPosition(1, 20, 19)),
                endToken: createToken(TokenType.NUMBER, "42", createPosition(1, 20, 19)),
                token: createToken(TokenType.NUMBER, "42", createPosition(1, 20, 19)),
            };

            const node: DirectFunctionCallNode = {
                kind: ASTNodeType.DirectFunctionCall,
                startToken: createToken(
                    TokenType.IDENTIFIER,
                    "TestFunction",
                    createPosition(1, 1, 0)
                ),
                endToken: createToken(TokenType.RPAREN, ")", createPosition(1, 22, 21)),
                name: createToken(TokenType.IDENTIFIER, "TestFunction", createPosition(1, 1, 0)),
                arguments: {
                    kind: ASTNodeType.ArgumentList,
                    arguments: [arg1, arg2],
                },
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe('TestFunction("test", 42)');
        });

        it("should format DoProc calls", () => {
            const procName = {
                kind: ASTNodeType.StringLiteral,
                startToken: createToken(TokenType.STRING, '"MyProc"', createPosition(1, 8, 7)),
                endToken: createToken(TokenType.STRING, '"MyProc"', createPosition(1, 8, 7)),
                token: createToken(TokenType.STRING, '"MyProc"', createPosition(1, 8, 7)),
            };

            const args = {
                kind: ASTNodeType.ArrayLiteral,
                startToken: createToken(TokenType.LBRACE, "{", createPosition(1, 18, 17)),
                endToken: createToken(TokenType.RBRACE, "}", createPosition(1, 19, 18)),
                elements: [],
            };

            const node: DoProcCallNode = {
                kind: ASTNodeType.DoProcCall,
                startToken: createToken(TokenType.IDENTIFIER, "DoProc", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.RPAREN, ")", createPosition(1, 20, 19)),
                procedureName: procName,
                arguments: args,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe('DoProc("MyProc", {})');
        });

        it("should format ExecFunction calls", () => {
            const funcName = {
                kind: ASTNodeType.StringLiteral,
                startToken: createToken(TokenType.STRING, '"MyFunc"', createPosition(1, 13, 12)),
                endToken: createToken(TokenType.STRING, '"MyFunc"', createPosition(1, 13, 12)),
                token: createToken(TokenType.STRING, '"MyFunc"', createPosition(1, 13, 12)),
            };

            const args = {
                kind: ASTNodeType.ArrayLiteral,
                startToken: createToken(TokenType.LBRACE, "{", createPosition(1, 23, 22)),
                endToken: createToken(TokenType.RBRACE, "}", createPosition(1, 24, 23)),
                elements: [],
            };

            const node: ExecFunctionCallNode = {
                kind: ASTNodeType.ExecFunctionCall,
                startToken: createToken(
                    TokenType.IDENTIFIER,
                    "ExecFunction",
                    createPosition(1, 1, 0)
                ),
                endToken: createToken(TokenType.RPAREN, ")", createPosition(1, 25, 24)),
                functionName: funcName,
                arguments: args,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe('ExecFunction("MyFunc", {})');
        });

        it("should format method calls", () => {
            const args = {
                kind: ASTNodeType.ArgumentList,
                arguments: [],
            };

            const node: MethodCallNode = {
                kind: ASTNodeType.MethodCall,
                startToken: createToken(TokenType.IDENTIFIER, "oObject", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.RPAREN, ")", createPosition(1, 18, 17)),
                object: createToken(TokenType.IDENTIFIER, "oObject", createPosition(1, 1, 0)),
                method: createToken(TokenType.IDENTIFIER, "DoSomething", createPosition(1, 9, 8)),
                arguments: args,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("oObject:DoSomething()");
        });

        it("should format bitwise operations", () => {
            const operand1 = {
                kind: ASTNodeType.VariableAccess,
                startToken: createToken(TokenType.IDENTIFIER, "nValue1", createPosition(1, 6, 5)),
                endToken: createToken(TokenType.IDENTIFIER, "nValue1", createPosition(1, 6, 5)),
                name: createToken(TokenType.IDENTIFIER, "nValue1", createPosition(1, 6, 5)),
            };

            const operand2 = {
                kind: ASTNodeType.VariableAccess,
                startToken: createToken(TokenType.IDENTIFIER, "nValue2", createPosition(1, 15, 14)),
                endToken: createToken(TokenType.IDENTIFIER, "nValue2", createPosition(1, 15, 14)),
                name: createToken(TokenType.IDENTIFIER, "nValue2", createPosition(1, 15, 14)),
            };

            const node: BitwiseOperationNode = {
                kind: ASTNodeType.BitwiseOperation,
                startToken: createToken(TokenType.IDENTIFIER, "_AND", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.RPAREN, ")", createPosition(1, 22, 21)),
                operation: "_AND",
                operands: [operand1, operand2],
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("_AND(nValue1, nValue2)");
        });

        it("should format object creation", () => {
            const className = {
                kind: ASTNodeType.StringLiteral,
                startToken: createToken(TokenType.STRING, '"MyClass"', createPosition(1, 16, 15)),
                endToken: createToken(TokenType.STRING, '"MyClass"', createPosition(1, 16, 15)),
                token: createToken(TokenType.STRING, '"MyClass"', createPosition(1, 16, 15)),
            };

            const node: ObjectCreationNode = {
                kind: ASTNodeType.ObjectCreation,
                startToken: createToken(
                    TokenType.IDENTIFIER,
                    "CreateUDObject",
                    createPosition(1, 1, 0)
                ),
                endToken: createToken(TokenType.RPAREN, ")", createPosition(1, 25, 24)),
                className,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe('CreateUDObject("MyClass")');
        });
    });

    describe("Complex Expression Integration", () => {
        it("should format nested expressions correctly", () => {
            // Test: (a + b) * c
            const innerLeft = {
                kind: ASTNodeType.VariableAccess,
                startToken: createToken(TokenType.IDENTIFIER, "a", createPosition(1, 2, 1)),
                endToken: createToken(TokenType.IDENTIFIER, "a", createPosition(1, 2, 1)),
                name: createToken(TokenType.IDENTIFIER, "a", createPosition(1, 2, 1)),
            };

            const innerRight = {
                kind: ASTNodeType.VariableAccess,
                startToken: createToken(TokenType.IDENTIFIER, "b", createPosition(1, 6, 5)),
                endToken: createToken(TokenType.IDENTIFIER, "b", createPosition(1, 6, 5)),
                name: createToken(TokenType.IDENTIFIER, "b", createPosition(1, 6, 5)),
            };

            const innerExpr: ArithmeticExpressionNode = {
                kind: ASTNodeType.ArithmeticExpression,
                startToken: innerLeft.startToken,
                endToken: innerRight.endToken,
                left: innerLeft,
                operator: createToken(TokenType.PLUS, "+", createPosition(1, 4, 3)),
                right: innerRight,
            };

            const outerRight = {
                kind: ASTNodeType.VariableAccess,
                startToken: createToken(TokenType.IDENTIFIER, "c", createPosition(1, 11, 10)),
                endToken: createToken(TokenType.IDENTIFIER, "c", createPosition(1, 11, 10)),
                name: createToken(TokenType.IDENTIFIER, "c", createPosition(1, 11, 10)),
            };

            const outerExpr: TermNode = {
                kind: ASTNodeType.Term,
                startToken: innerExpr.startToken,
                endToken: outerRight.endToken,
                left: innerExpr,
                operator: createToken(TokenType.MULTIPLY, "*", createPosition(1, 9, 8)),
                right: outerRight,
            } as any;

            const result = visitor.visit(outerExpr);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("a + b * c");
        });
    });

    describe("EBNF Grammar Compliance", () => {
        it("should handle all SSL-specific expression types", () => {
            const sslSpecificTypes = [
                ASTNodeType.LogicalExpression,
                ASTNodeType.ComparisonExpression,
                ASTNodeType.ArithmeticExpression,
                ASTNodeType.Term,
                ASTNodeType.Factor,
                ASTNodeType.PowerOperand,
                ASTNodeType.UnaryExpression,
                ASTNodeType.IncrementExpression,
                ASTNodeType.VariableAccess,
                ASTNodeType.PropertyAccess,
                ASTNodeType.ArrayAccess,
                ASTNodeType.MethodCall,
                ASTNodeType.BitwiseOperation,
                ASTNodeType.ObjectCreation,
            ];

            sslSpecificTypes.forEach((nodeType) => {
                const node = {
                    kind: nodeType,
                    startToken: mockToken,
                    endToken: mockToken,
                };

                const result = visitor.visit(node);
                expect(result.shouldContinue).toBe(false);
                expect(result.error).toBeUndefined();
            });
        });

        it("should properly format SSL boolean literals (.T., .F.)", () => {
            const testCases = [
                { value: true, expected: ".T." },
                { value: false, expected: ".F." },
            ];

            testCases.forEach((testCase) => {
                visitor = new SSLExpressionFormatterVisitor(options);

                const node: BooleanLiteralNode = {
                    kind: ASTNodeType.BooleanLiteral,
                    startToken: mockToken,
                    endToken: mockToken,
                    value: testCase.value,
                } as any;

                visitor.visit(node);
                const output = visitor.getFormattedOutput().trim();
                expect(output).toBe(testCase.expected);
            });
        });

        it("should handle SSL property access with colon notation", () => {
            const node: PropertyAccessNode = {
                kind: ASTNodeType.PropertyAccess,
                startToken: createToken(TokenType.IDENTIFIER, "oUDO", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.IDENTIFIER, "sName", createPosition(1, 6, 5)),
                object: createToken(TokenType.IDENTIFIER, "oUDO", createPosition(1, 1, 0)),
                property: createToken(TokenType.IDENTIFIER, "sName", createPosition(1, 6, 5)),
            } as any;

            const result = visitor.visit(node);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("oUDO:sName");
        });
    });
});
