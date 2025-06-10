import { ASTNode, ASTNodeType, createBaseNode } from "../../src/parser/ast/base";
import { Token, createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("AST Base", () => {
    let mockToken: Token;

    beforeEach(() => {
        mockToken = createToken(TokenType.IDENTIFIER, "test", createPosition(1, 1, 0));
    });
    describe("ASTNodeType", () => {
        it("should contain all required node types", () => {
            expect(ASTNodeType.Program).toBe("Program");
            expect(ASTNodeType.ClassDefinition).toBe("ClassDefinition");
            expect(ASTNodeType.ProcedureStatement).toBe("ProcedureStatement");
            expect(ASTNodeType.IfStatement).toBe("IfStatement");
            expect(ASTNodeType.WhileLoop).toBe("WhileLoop");
            expect(ASTNodeType.Assignment).toBe("Assignment");
            expect(ASTNodeType.FunctionCall).toBe("FunctionCall");
            expect(ASTNodeType.BinaryExpression).toBe("BinaryExpression");
            expect(ASTNodeType.UnaryExpression).toBe("UnaryExpression");
            expect(ASTNodeType.Literal).toBe("Literal");
            expect(ASTNodeType.StringLiteral).toBe("StringLiteral");
            expect(ASTNodeType.NumberLiteral).toBe("NumberLiteral");
            expect(ASTNodeType.BooleanLiteral).toBe("BooleanLiteral");
        });

        it("should have unique values for all node types", () => {
            const values = Object.values(ASTNodeType);
            const uniqueValues = new Set(values);
            expect(values.length).toBe(uniqueValues.size);
        });

        it("should contain all control flow node types", () => {
            expect(ASTNodeType.ConditionalStatement).toBeDefined();
            expect(ASTNodeType.IfStatement).toBeDefined();
            expect(ASTNodeType.ElseStatement).toBeDefined();
            expect(ASTNodeType.EndIfStatement).toBeDefined();
            expect(ASTNodeType.LoopStatement).toBeDefined();
            expect(ASTNodeType.WhileLoop).toBeDefined();
            expect(ASTNodeType.WhileStatement).toBeDefined();
            expect(ASTNodeType.EndWhileStatement).toBeDefined();
            expect(ASTNodeType.ForLoop).toBeDefined();
            expect(ASTNodeType.ForStatement).toBeDefined();
            expect(ASTNodeType.NextStatement).toBeDefined();
            expect(ASTNodeType.ExitWhileStatement).toBeDefined();
            expect(ASTNodeType.ExitForStatement).toBeDefined();
            expect(ASTNodeType.LoopContinue).toBeDefined();
        });

        it("should contain all switch case node types", () => {
            expect(ASTNodeType.SwitchStatement).toBeDefined();
            expect(ASTNodeType.BeginCaseStatement).toBeDefined();
            expect(ASTNodeType.CaseBlock).toBeDefined();
            expect(ASTNodeType.CaseStatement).toBeDefined();
            expect(ASTNodeType.OtherwiseBlock).toBeDefined();
            expect(ASTNodeType.OtherwiseStatement).toBeDefined();
            expect(ASTNodeType.EndCaseStatement).toBeDefined();
            expect(ASTNodeType.ExitCaseStatement).toBeDefined();
        });

        it("should contain all error handling node types", () => {
            expect(ASTNodeType.ErrorHandlingStatement).toBeDefined();
            expect(ASTNodeType.TryBlock).toBeDefined();
            expect(ASTNodeType.TryStatement).toBeDefined();
            expect(ASTNodeType.CatchBlock).toBeDefined();
            expect(ASTNodeType.CatchStatement).toBeDefined();
            expect(ASTNodeType.FinallyBlock).toBeDefined();
            expect(ASTNodeType.FinallyStatement).toBeDefined();
            expect(ASTNodeType.EndTryStatement).toBeDefined();
            expect(ASTNodeType.ErrorBlockStanza).toBeDefined();
            expect(ASTNodeType.ErrorMarker).toBeDefined();
        });

        it("should contain all declaration node types", () => {
            expect(ASTNodeType.DeclarationStatement).toBeDefined();
            expect(ASTNodeType.ParametersStatement).toBeDefined();
            expect(ASTNodeType.DeclareStatement).toBeDefined();
            expect(ASTNodeType.DefaultStatement).toBeDefined();
            expect(ASTNodeType.PublicStatement).toBeDefined();
            expect(ASTNodeType.IncludeStatement).toBeDefined();
        });

        it("should contain all procedure and class node types", () => {
            expect(ASTNodeType.ProcedureStatement).toBeDefined();
            expect(ASTNodeType.ProcedureStart).toBeDefined();
            expect(ASTNodeType.ProcedureEnd).toBeDefined();
            expect(ASTNodeType.ParameterDeclaration).toBeDefined();
            expect(ASTNodeType.DefaultParameterDeclaration).toBeDefined();
            expect(ASTNodeType.DefaultParameterList).toBeDefined();
            expect(ASTNodeType.ParameterList).toBeDefined();
            expect(ASTNodeType.ClassDefinition).toBeDefined();
            expect(ASTNodeType.ClassDeclaration).toBeDefined();
            expect(ASTNodeType.InheritStatement).toBeDefined();
            expect(ASTNodeType.ClassFieldDeclaration).toBeDefined();
            expect(ASTNodeType.MethodDeclaration).toBeDefined();
        });

        it("should contain all expression node types", () => {
            expect(ASTNodeType.Expression).toBeDefined();
            expect(ASTNodeType.LogicalExpression).toBeDefined();
            expect(ASTNodeType.ComparisonExpression).toBeDefined();
            expect(ASTNodeType.ArithmeticExpression).toBeDefined();
            expect(ASTNodeType.Term).toBeDefined();
            expect(ASTNodeType.Factor).toBeDefined();
            expect(ASTNodeType.PowerOperand).toBeDefined();
            expect(ASTNodeType.Primary).toBeDefined();
            expect(ASTNodeType.BinaryExpression).toBeDefined();
            expect(ASTNodeType.UnaryExpression).toBeDefined();
            expect(ASTNodeType.IncrementExpression).toBeDefined();
            expect(ASTNodeType.VariableAccess).toBeDefined();
            expect(ASTNodeType.PropertyAccess).toBeDefined();
            expect(ASTNodeType.ArrayAccess).toBeDefined();
        });

        it("should contain all literal node types", () => {
            expect(ASTNodeType.LiteralExpression).toBeDefined();
            expect(ASTNodeType.NumberLiteral).toBeDefined();
            expect(ASTNodeType.StringLiteral).toBeDefined();
            expect(ASTNodeType.BooleanLiteral).toBeDefined();
            expect(ASTNodeType.ArrayLiteral).toBeDefined();
            expect(ASTNodeType.NilLiteral).toBeDefined();
            expect(ASTNodeType.DateLiteral).toBeDefined();
            expect(ASTNodeType.CodeBlockLiteral).toBeDefined();
            expect(ASTNodeType.Literal).toBeDefined();
        });

        it("should contain all function call node types", () => {
            expect(ASTNodeType.FunctionCall).toBeDefined();
            expect(ASTNodeType.DirectFunctionCall).toBeDefined();
            expect(ASTNodeType.DoProcCall).toBeDefined();
            expect(ASTNodeType.ExecFunctionCall).toBeDefined();
        });

        it("should contain all SQL integration node types", () => {
            expect(ASTNodeType.SqlStatement).toBeDefined();
            expect(ASTNodeType.SqlExecute).toBeDefined();
            expect(ASTNodeType.LSearch).toBeDefined();
            expect(ASTNodeType.SqlParameter).toBeDefined();
        });

        it("should contain all comment node types", () => {
            expect(ASTNodeType.CommentStatement).toBeDefined();
            expect(ASTNodeType.BlockComment).toBeDefined();
            expect(ASTNodeType.SingleLineComment).toBeDefined();
            expect(ASTNodeType.RegionComment).toBeDefined();
            expect(ASTNodeType.EndRegionComment).toBeDefined();
        });

        it("should contain all special structure node types", () => {
            expect(ASTNodeType.LabelStatement).toBeDefined();
            expect(ASTNodeType.RegionBlock).toBeDefined();
            expect(ASTNodeType.RegionStart).toBeDefined();
            expect(ASTNodeType.RegionEnd).toBeDefined();
            expect(ASTNodeType.InlineCodeBlock).toBeDefined();
            expect(ASTNodeType.InlineCodeStart).toBeDefined();
            expect(ASTNodeType.InlineCodeEnd).toBeDefined();
            expect(ASTNodeType.DynamicCodeExecution).toBeDefined();
            expect(ASTNodeType.BranchStatement).toBeDefined();
        });

        it("should contain all object-oriented node types", () => {
            expect(ASTNodeType.ObjectCreation).toBeDefined();
            expect(ASTNodeType.MethodCall).toBeDefined();
            expect(ASTNodeType.ObjectPropertyAccess).toBeDefined();
        });

        it("should contain all list node types", () => {
            expect(ASTNodeType.IdentifierList).toBeDefined();
            expect(ASTNodeType.ExpressionList).toBeDefined();
            expect(ASTNodeType.ArgumentList).toBeDefined();
        });

        it("should contain all operator node types", () => {
            expect(ASTNodeType.AssignmentOperator).toBeDefined();
            expect(ASTNodeType.LogicalOperator).toBeDefined();
            expect(ASTNodeType.ComparisonOperator).toBeDefined();
            expect(ASTNodeType.AdditiveOperator).toBeDefined();
            expect(ASTNodeType.MultiplicativeOperator).toBeDefined();
            expect(ASTNodeType.UnaryOperator).toBeDefined();
        });

        it("should contain utility node types", () => {
            expect(ASTNodeType.ArraySubscript).toBeDefined();
            expect(ASTNodeType.BitwiseOperation).toBeDefined();
            expect(ASTNodeType.IntegerPart).toBeDefined();
            expect(ASTNodeType.DecimalPart).toBeDefined();
            expect(ASTNodeType.Exponent).toBeDefined();
            expect(ASTNodeType.Character).toBeDefined();
            expect(ASTNodeType.Symbol).toBeDefined();
            expect(ASTNodeType.Letter).toBeDefined();
            expect(ASTNodeType.Digit).toBeDefined();
        });

        it("should contain logic statement node types", () => {
            expect(ASTNodeType.LogicStatement).toBeDefined();
            expect(ASTNodeType.Assignment).toBeDefined();
            expect(ASTNodeType.ReturnStatement).toBeDefined();
        });
    });
    describe("ASTNode interface", () => {
        it("should define required properties", () => {
            const node: ASTNode = {
                kind: ASTNodeType.Program,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(node.kind).toBe(ASTNodeType.Program);
            expect(node.startToken).toBe(mockToken);
            expect(node.endToken).toBe(mockToken);
        });

        it("should accept different node types", () => {
            const nodeTypes = [
                ASTNodeType.Program,
                ASTNodeType.ClassDefinition,
                ASTNodeType.ProcedureStatement,
                ASTNodeType.IfStatement,
                ASTNodeType.Assignment,
            ];

            nodeTypes.forEach((nodeType) => {
                const node: ASTNode = {
                    kind: nodeType,
                    startToken: mockToken,
                    endToken: mockToken,
                };
                expect(node.kind).toBe(nodeType);
            });
        });
    });

    describe("createBaseNode utility", () => {
        it("should create a node with correct properties", () => {
            const startToken = createToken(TokenType.IF, "if", createPosition(1, 0, 0));
            const endToken = createToken(TokenType.ENDIF, "endif", createPosition(5, 0, 20));

            const node = createBaseNode(ASTNodeType.IfStatement, startToken, endToken);

            expect(node.kind).toBe(ASTNodeType.IfStatement);
            expect(node.startToken).toBe(startToken);
            expect(node.endToken).toBe(endToken);
        });

        it("should create nodes for all node types", () => {
            const testCases = [
                ASTNodeType.Program,
                ASTNodeType.ClassDefinition,
                ASTNodeType.ProcedureStatement,
                ASTNodeType.Assignment,
                ASTNodeType.BinaryExpression,
                ASTNodeType.StringLiteral,
            ];

            testCases.forEach((nodeType) => {
                const node = createBaseNode(nodeType, mockToken, mockToken);
                expect(node.kind).toBe(nodeType);
                expect(node.startToken).toBe(mockToken);
                expect(node.endToken).toBe(mockToken);
            });
        });
    });
    describe("Node hierarchy validation", () => {
        it("should have consistent naming for expression types", () => {
            const expressionTypes = [
                ASTNodeType.BinaryExpression,
                ASTNodeType.UnaryExpression,
                ASTNodeType.LogicalExpression,
                ASTNodeType.ComparisonExpression,
                ASTNodeType.ArithmeticExpression,
                ASTNodeType.LiteralExpression,
                ASTNodeType.IncrementExpression,
            ];

            expressionTypes.forEach((type) => {
                expect(type).toContain("Expression");
            });
        });

        it("should have consistent naming for statement types", () => {
            const statementTypes = [
                ASTNodeType.IfStatement,
                ASTNodeType.ElseStatement,
                ASTNodeType.WhileStatement,
                ASTNodeType.ForStatement,
                ASTNodeType.ReturnStatement,
                ASTNodeType.ProcedureStatement,
                ASTNodeType.Assignment, // Special case
                ASTNodeType.ConditionalStatement,
                ASTNodeType.LoopStatement,
                ASTNodeType.LogicStatement,
                ASTNodeType.DeclarationStatement,
                ASTNodeType.CommentStatement,
                ASTNodeType.LabelStatement,
                ASTNodeType.BranchStatement,
            ];

            statementTypes.forEach((type) => {
                expect(type).toMatch(/(Statement|Assignment)/);
            });
        });

        it("should have consistent naming for literal types", () => {
            const literalTypes = [
                ASTNodeType.Literal,
                ASTNodeType.StringLiteral,
                ASTNodeType.NumberLiteral,
                ASTNodeType.BooleanLiteral,
                ASTNodeType.ArrayLiteral,
                ASTNodeType.NilLiteral,
                ASTNodeType.DateLiteral,
                ASTNodeType.CodeBlockLiteral,
            ];

            literalTypes.forEach((type) => {
                expect(type).toContain("Literal");
            });
        });

        it("should have consistent naming for block types", () => {
            const blockTypes = [
                ASTNodeType.TryBlock,
                ASTNodeType.CatchBlock,
                ASTNodeType.FinallyBlock,
                ASTNodeType.CaseBlock,
                ASTNodeType.OtherwiseBlock,
                ASTNodeType.RegionBlock,
                ASTNodeType.InlineCodeBlock,
                ASTNodeType.ErrorBlockStanza,
            ];

            blockTypes.forEach((type) => {
                expect(type).toContain("Block");
            });
        });

        it("should have consistent naming for end types", () => {
            const endTypes = [
                ASTNodeType.EndIfStatement,
                ASTNodeType.EndWhileStatement,
                ASTNodeType.EndCaseStatement,
                ASTNodeType.EndTryStatement,
                ASTNodeType.ProcedureEnd,
                ASTNodeType.RegionEnd,
                ASTNodeType.InlineCodeEnd,
                ASTNodeType.EndRegionComment,
            ];

            endTypes.forEach((type) => {
                expect(type).toMatch(/(End|ENDPROC)/);
            });
        });
    });

    describe("Type system validation", () => {
        it("should have proper type definitions for base types", () => {
            // Test that our type aliases are properly defined
            const statementNode: ASTNode = createBaseNode(
                ASTNodeType.Assignment,
                mockToken,
                mockToken
            );
            const expressionNode: ASTNode = createBaseNode(
                ASTNodeType.BinaryExpression,
                mockToken,
                mockToken
            );
            const literalNode: ASTNode = createBaseNode(
                ASTNodeType.StringLiteral,
                mockToken,
                mockToken
            );

            expect(statementNode.kind).toBeDefined();
            expect(expressionNode.kind).toBeDefined();
            expect(literalNode.kind).toBeDefined();
        });

        it("should support all major categories of nodes", () => {
            const categories = {
                statements: [
                    ASTNodeType.IfStatement,
                    ASTNodeType.Assignment,
                    ASTNodeType.ReturnStatement,
                ],
                expressions: [
                    ASTNodeType.BinaryExpression,
                    ASTNodeType.UnaryExpression,
                    ASTNodeType.VariableAccess,
                ],
                literals: [
                    ASTNodeType.NumberLiteral,
                    ASTNodeType.StringLiteral,
                    ASTNodeType.BooleanLiteral,
                ],
                declarations: [
                    ASTNodeType.ProcedureStatement,
                    ASTNodeType.ClassDefinition,
                    ASTNodeType.ParameterDeclaration,
                ],
                controlFlow: [ASTNodeType.IfStatement, ASTNodeType.WhileLoop, ASTNodeType.ForLoop],
                errorHandling: [
                    ASTNodeType.TryBlock,
                    ASTNodeType.CatchBlock,
                    ASTNodeType.FinallyBlock,
                ],
            };

            Object.entries(categories).forEach(([category, types]) => {
                types.forEach((type) => {
                    const node = createBaseNode(type, mockToken, mockToken);
                    expect(node.kind).toBe(type);
                });
            });
        });
    });
});
