import {
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
    PrimaryNode,
} from "../../src/parser/ast/expressions";
import { ASTNodeType } from "../../src/parser/ast/base";
import { Token, createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("AST Expressions", () => {
    let mockToken: Token;
    let operatorToken: Token;
    let mockExpressionNode: any;

    beforeEach(() => {
        mockToken = createToken(TokenType.IDENTIFIER, "test", createPosition(1, 1, 0));

        operatorToken = createToken(TokenType.PLUS, "+", createPosition(1, 1, 0));

        mockExpressionNode = {
            kind: ASTNodeType.Primary,
            startToken: mockToken,
            endToken: mockToken,
        };
    });

    describe("BinaryExpressionNode", () => {
        it("should have correct structure", () => {
            const binaryExpression: BinaryExpressionNode = {
                kind: ASTNodeType.BinaryExpression,
                startToken: mockToken,
                endToken: mockToken,
                left: mockExpressionNode,
                operator: operatorToken,
                right: mockExpressionNode,
            };

            expect(binaryExpression.left).toBe(mockExpressionNode);
            expect(binaryExpression.operator).toBe(operatorToken);
            expect(binaryExpression.right).toBe(mockExpressionNode);
            expect(binaryExpression.startToken).toBe(mockToken);
            expect(binaryExpression.endToken).toBe(mockToken);
        });

        it("should accept different operator types", () => {
            const operators = [
                { type: TokenType.PLUS, value: "+" },
                { type: TokenType.MINUS, value: "-" },
                { type: TokenType.MULTIPLY, value: "*" },
                { type: TokenType.DIVIDE, value: "/" },
                { type: TokenType.EQUAL, value: "==" },
                { type: TokenType.NOT_EQUAL, value: "!=" },
            ];

            operators.forEach((op) => {
                const opToken = createToken(op.type, op.value, createPosition(1, 1, 0));
                const expr: BinaryExpressionNode = {
                    kind: ASTNodeType.BinaryExpression,
                    startToken: mockToken,
                    endToken: mockToken,
                    left: mockExpressionNode,
                    operator: opToken,
                    right: mockExpressionNode,
                };

                expect(expr.operator.value).toBe(op.value);
                expect(expr.operator.type).toBe(op.type);
            });
        });
    });

    describe("LogicalExpressionNode", () => {
        it("should extend BinaryExpressionNode with logical kind", () => {
            const logicalExpression: LogicalExpressionNode = {
                kind: ASTNodeType.LogicalExpression,
                startToken: mockToken,
                endToken: mockToken,
                left: mockExpressionNode,
                operator: createToken(TokenType.AND, ".AND.", createPosition(1, 1, 0)),
                right: mockExpressionNode,
            };

            expect(logicalExpression.kind).toBe(ASTNodeType.LogicalExpression);
            expect(logicalExpression.operator.value).toBe(".AND.");
        });

        it("should support OR operator", () => {
            const orExpression: LogicalExpressionNode = {
                kind: ASTNodeType.LogicalExpression,
                startToken: mockToken,
                endToken: mockToken,
                left: mockExpressionNode,
                operator: createToken(TokenType.OR, ".OR.", createPosition(1, 1, 0)),
                right: mockExpressionNode,
            };

            expect(orExpression.operator.value).toBe(".OR.");
        });
    });

    describe("ComparisonExpressionNode", () => {
        it("should extend BinaryExpressionNode with comparison kind", () => {
            const comparisonExpression: ComparisonExpressionNode = {
                kind: ASTNodeType.ComparisonExpression,
                startToken: mockToken,
                endToken: mockToken,
                left: mockExpressionNode,
                operator: createToken(TokenType.EQUAL, "==", createPosition(1, 1, 0)),
                right: mockExpressionNode,
            };

            expect(comparisonExpression.kind).toBe(ASTNodeType.ComparisonExpression);
            expect(comparisonExpression.operator.value).toBe("==");
        });

        it("should support different comparison operators", () => {
            const comparisons = [
                { type: TokenType.EQUAL, value: "==" },
                { type: TokenType.NOT_EQUAL, value: "!=" },
                { type: TokenType.LESS_THAN, value: "<" },
                { type: TokenType.GREATER_THAN, value: ">" },
                { type: TokenType.LESS_EQUAL, value: "<=" },
                { type: TokenType.GREATER_EQUAL, value: ">=" },
            ];

            comparisons.forEach((comp) => {
                const compToken = createToken(comp.type, comp.value, createPosition(1, 1, 0));
                const expr: ComparisonExpressionNode = {
                    kind: ASTNodeType.ComparisonExpression,
                    startToken: mockToken,
                    endToken: mockToken,
                    left: mockExpressionNode,
                    operator: compToken,
                    right: mockExpressionNode,
                };

                expect(expr.operator.value).toBe(comp.value);
            });
        });
    });

    describe("ArithmeticExpressionNode", () => {
        it("should extend BinaryExpressionNode with arithmetic kind", () => {
            const arithmeticExpression: ArithmeticExpressionNode = {
                kind: ASTNodeType.ArithmeticExpression,
                startToken: mockToken,
                endToken: mockToken,
                left: mockExpressionNode,
                operator: operatorToken,
                right: mockExpressionNode,
            };

            expect(arithmeticExpression.kind).toBe(ASTNodeType.ArithmeticExpression);
            expect(arithmeticExpression.operator.value).toBe("+");
        });

        it("should support arithmetic operators", () => {
            const operators = [
                { type: TokenType.PLUS, value: "+" },
                { type: TokenType.MINUS, value: "-" },
            ];

            operators.forEach((op) => {
                const opToken = createToken(op.type, op.value, createPosition(1, 1, 0));
                const expr: ArithmeticExpressionNode = {
                    kind: ASTNodeType.ArithmeticExpression,
                    startToken: mockToken,
                    endToken: mockToken,
                    left: mockExpressionNode,
                    operator: opToken,
                    right: mockExpressionNode,
                };

                expect(expr.operator.value).toBe(op.value);
            });
        });
    });

    describe("UnaryExpressionNode", () => {
        it("should have correct structure", () => {
            const unaryExpression: UnaryExpressionNode = {
                kind: ASTNodeType.UnaryExpression,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(unaryExpression.kind).toBe(ASTNodeType.UnaryExpression);
            expect(unaryExpression.startToken).toBe(mockToken);
            expect(unaryExpression.endToken).toBe(mockToken);
        });
    });

    describe("IncrementExpressionNode", () => {
        it("should have correct structure", () => {
            const incrementExpression: IncrementExpressionNode = {
                kind: ASTNodeType.IncrementExpression,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(incrementExpression.kind).toBe(ASTNodeType.IncrementExpression);
            expect(incrementExpression.startToken).toBe(mockToken);
            expect(incrementExpression.endToken).toBe(mockToken);
        });
    });

    describe("VariableAccessNode", () => {
        it("should have correct structure", () => {
            const variableAccess: VariableAccessNode = {
                kind: ASTNodeType.VariableAccess,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(variableAccess.kind).toBe(ASTNodeType.VariableAccess);
            expect(variableAccess.startToken).toBe(mockToken);
            expect(variableAccess.endToken).toBe(mockToken);
        });
    });

    describe("PropertyAccessNode", () => {
        it("should have correct structure", () => {
            const propertyAccess: PropertyAccessNode = {
                kind: ASTNodeType.PropertyAccess,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(propertyAccess.kind).toBe(ASTNodeType.PropertyAccess);
            expect(propertyAccess.startToken).toBe(mockToken);
            expect(propertyAccess.endToken).toBe(mockToken);
        });
    });

    describe("ArrayAccessNode", () => {
        it("should have correct structure", () => {
            const arrayAccess: ArrayAccessNode = {
                kind: ASTNodeType.ArrayAccess,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(arrayAccess.kind).toBe(ASTNodeType.ArrayAccess);
            expect(arrayAccess.startToken).toBe(mockToken);
            expect(arrayAccess.endToken).toBe(mockToken);
        });
    });

    describe("ArraySubscriptNode", () => {
        it("should have correct structure", () => {
            const arraySubscript: ArraySubscriptNode = {
                kind: ASTNodeType.ArraySubscript,
                startToken: mockToken,
                endToken: mockToken,
                dimensions: [mockExpressionNode],
            };

            expect(arraySubscript.kind).toBe(ASTNodeType.ArraySubscript);
            expect(arraySubscript.dimensions).toHaveLength(1);
            expect(arraySubscript.dimensions[0]).toBe(mockExpressionNode);
        });

        it("should support multiple dimensions", () => {
            const arraySubscript: ArraySubscriptNode = {
                kind: ASTNodeType.ArraySubscript,
                startToken: mockToken,
                endToken: mockToken,
                dimensions: [mockExpressionNode, mockExpressionNode, mockExpressionNode],
            };

            expect(arraySubscript.dimensions).toHaveLength(3);
            arraySubscript.dimensions.forEach((dim) => {
                expect(dim).toBe(mockExpressionNode);
            });
        });
    });

    describe("PrimaryNode", () => {
        it("should have correct structure", () => {
            const primary: PrimaryNode = {
                kind: ASTNodeType.Primary,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(primary.kind).toBe(ASTNodeType.Primary);
            expect(primary.startToken).toBe(mockToken);
            expect(primary.endToken).toBe(mockToken);
        });
    });

    describe("TermNode", () => {
        it("should have correct structure", () => {
            const term: TermNode = {
                kind: ASTNodeType.Term,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(term.kind).toBe(ASTNodeType.Term);
            expect(term.startToken).toBe(mockToken);
            expect(term.endToken).toBe(mockToken);
        });
    });

    describe("FactorNode", () => {
        it("should have correct structure", () => {
            const factor: FactorNode = {
                kind: ASTNodeType.Factor,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(factor.kind).toBe(ASTNodeType.Factor);
            expect(factor.startToken).toBe(mockToken);
            expect(factor.endToken).toBe(mockToken);
        });
    });

    describe("PowerOperandNode", () => {
        it("should have correct structure", () => {
            const powerOperand: PowerOperandNode = {
                kind: ASTNodeType.PowerOperand,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(powerOperand.kind).toBe(ASTNodeType.PowerOperand);
            expect(powerOperand.startToken).toBe(mockToken);
            expect(powerOperand.endToken).toBe(mockToken);
        });
    });
});
