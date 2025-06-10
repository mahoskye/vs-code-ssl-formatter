import {
    ConditionalStatementNode,
    IfStatementNode,
    ElseStatementNode,
    EndIfStatementNode,
    LoopStatementNode,
    WhileLoopNode,
    WhileStatementNode,
    EndWhileStatementNode,
    ForLoopNode,
    ForStatementNode,
    NextStatementNode,
    ExitWhileStatementNode,
    ExitForStatementNode,
    LoopContinueNode,
} from "../../src/parser/ast/controlFlow";
import { ASTNodeType } from "../../src/parser/ast/base";
import { Token, createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("AST Control Flow", () => {
    let mockToken: Token;
    let mockExpressionNode: any;
    let mockStatementNode: any;

    beforeEach(() => {
        mockToken = createToken(TokenType.IDENTIFIER, "test", createPosition(1, 1, 0));

        mockExpressionNode = {
            kind: ASTNodeType.Primary,
            startToken: mockToken,
            endToken: mockToken,
        };

        mockStatementNode = {
            kind: ASTNodeType.Assignment,
            startToken: mockToken,
            endToken: mockToken,
        };
    });

    describe("ConditionalStatementNode", () => {
        it("should have correct structure", () => {
            const conditionalStatement: ConditionalStatementNode = {
                kind: ASTNodeType.ConditionalStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(conditionalStatement.kind).toBe(ASTNodeType.ConditionalStatement);
            expect(conditionalStatement.startToken).toBe(mockToken);
            expect(conditionalStatement.endToken).toBe(mockToken);
        });
    });

    describe("IfStatementNode", () => {
        it("should have correct structure without else branch", () => {
            const endIfNode: EndIfStatementNode = {
                kind: ASTNodeType.EndIfStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            const ifStatement: IfStatementNode = {
                kind: ASTNodeType.IfStatement,
                startToken: mockToken,
                endToken: mockToken,
                condition: mockExpressionNode,
                thenBranch: [mockStatementNode],
                endIf: endIfNode,
            };

            expect(ifStatement.kind).toBe(ASTNodeType.IfStatement);
            expect(ifStatement.condition).toBe(mockExpressionNode);
            expect(ifStatement.thenBranch).toHaveLength(1);
            expect(ifStatement.thenBranch[0]).toBe(mockStatementNode);
            expect(ifStatement.elseBranch).toBeUndefined();
            expect(ifStatement.endIf).toBe(endIfNode);
        });

        it("should have correct structure with else branch", () => {
            const elseNode: ElseStatementNode = {
                kind: ASTNodeType.ElseStatement,
                startToken: mockToken,
                endToken: mockToken,
                body: [mockStatementNode],
            };

            const endIfNode: EndIfStatementNode = {
                kind: ASTNodeType.EndIfStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            const ifStatement: IfStatementNode = {
                kind: ASTNodeType.IfStatement,
                startToken: mockToken,
                endToken: mockToken,
                condition: mockExpressionNode,
                thenBranch: [mockStatementNode],
                elseBranch: elseNode,
                endIf: endIfNode,
            };

            expect(ifStatement.elseBranch).toBeDefined();
            expect(ifStatement.elseBranch).toBe(elseNode);
        });

        it("should support multiple statements in then branch", () => {
            const endIfNode: EndIfStatementNode = {
                kind: ASTNodeType.EndIfStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            const ifStatement: IfStatementNode = {
                kind: ASTNodeType.IfStatement,
                startToken: mockToken,
                endToken: mockToken,
                condition: mockExpressionNode,
                thenBranch: [mockStatementNode, mockStatementNode, mockStatementNode],
                endIf: endIfNode,
            };

            expect(ifStatement.thenBranch).toHaveLength(3);
        });
    });

    describe("ElseStatementNode", () => {
        it("should have correct structure", () => {
            const elseStatement: ElseStatementNode = {
                kind: ASTNodeType.ElseStatement,
                startToken: mockToken,
                endToken: mockToken,
                body: [mockStatementNode],
            };

            expect(elseStatement.kind).toBe(ASTNodeType.ElseStatement);
            expect(elseStatement.body).toHaveLength(1);
            expect(elseStatement.body[0]).toBe(mockStatementNode);
        });

        it("should support empty body", () => {
            const elseStatement: ElseStatementNode = {
                kind: ASTNodeType.ElseStatement,
                startToken: mockToken,
                endToken: mockToken,
                body: [],
            };

            expect(elseStatement.body).toHaveLength(0);
        });

        it("should support multiple statements", () => {
            const elseStatement: ElseStatementNode = {
                kind: ASTNodeType.ElseStatement,
                startToken: mockToken,
                endToken: mockToken,
                body: [mockStatementNode, mockStatementNode],
            };

            expect(elseStatement.body).toHaveLength(2);
        });
    });

    describe("EndIfStatementNode", () => {
        it("should have correct structure", () => {
            const endIfStatement: EndIfStatementNode = {
                kind: ASTNodeType.EndIfStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(endIfStatement.kind).toBe(ASTNodeType.EndIfStatement);
            expect(endIfStatement.startToken).toBe(mockToken);
            expect(endIfStatement.endToken).toBe(mockToken);
        });
    });

    describe("LoopStatementNode", () => {
        it("should have correct structure", () => {
            const loopStatement: LoopStatementNode = {
                kind: ASTNodeType.LoopStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(loopStatement.kind).toBe(ASTNodeType.LoopStatement);
            expect(loopStatement.startToken).toBe(mockToken);
            expect(loopStatement.endToken).toBe(mockToken);
        });
    });

    describe("WhileLoopNode", () => {
        it("should have correct structure", () => {
            const whileStatementNode: WhileStatementNode = {
                kind: ASTNodeType.WhileStatement,
                startToken: mockToken,
                endToken: mockToken,
                condition: mockExpressionNode,
            };

            const endWhileNode: EndWhileStatementNode = {
                kind: ASTNodeType.EndWhileStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            const whileLoop: WhileLoopNode = {
                kind: ASTNodeType.WhileLoop,
                startToken: mockToken,
                endToken: mockToken,
                condition: whileStatementNode,
                body: [mockStatementNode],
                end: endWhileNode,
            };

            expect(whileLoop.kind).toBe(ASTNodeType.WhileLoop);
            expect(whileLoop.condition).toBe(whileStatementNode);
            expect(whileLoop.body).toHaveLength(1);
            expect(whileLoop.body[0]).toBe(mockStatementNode);
            expect(whileLoop.end).toBe(endWhileNode);
        });

        it("should support empty body", () => {
            const whileStatementNode: WhileStatementNode = {
                kind: ASTNodeType.WhileStatement,
                startToken: mockToken,
                endToken: mockToken,
                condition: mockExpressionNode,
            };

            const endWhileNode: EndWhileStatementNode = {
                kind: ASTNodeType.EndWhileStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            const whileLoop: WhileLoopNode = {
                kind: ASTNodeType.WhileLoop,
                startToken: mockToken,
                endToken: mockToken,
                condition: whileStatementNode,
                body: [],
                end: endWhileNode,
            };

            expect(whileLoop.body).toHaveLength(0);
        });
    });

    describe("WhileStatementNode", () => {
        it("should have correct structure", () => {
            const whileStatement: WhileStatementNode = {
                kind: ASTNodeType.WhileStatement,
                startToken: mockToken,
                endToken: mockToken,
                condition: mockExpressionNode,
            };

            expect(whileStatement.kind).toBe(ASTNodeType.WhileStatement);
            expect(whileStatement.condition).toBe(mockExpressionNode);
            expect(whileStatement.startToken).toBe(mockToken);
            expect(whileStatement.endToken).toBe(mockToken);
        });
    });

    describe("EndWhileStatementNode", () => {
        it("should have correct structure", () => {
            const endWhileStatement: EndWhileStatementNode = {
                kind: ASTNodeType.EndWhileStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(endWhileStatement.kind).toBe(ASTNodeType.EndWhileStatement);
            expect(endWhileStatement.startToken).toBe(mockToken);
            expect(endWhileStatement.endToken).toBe(mockToken);
        });
    });

    describe("ForLoopNode", () => {
        it("should have correct structure", () => {
            const forStatementNode: ForStatementNode = {
                kind: ASTNodeType.ForStatement,
                startToken: mockToken,
                endToken: mockToken,
                variable: mockToken,
                startValue: mockExpressionNode,
                endValue: mockExpressionNode,
            };

            const nextStatementNode: NextStatementNode = {
                kind: ASTNodeType.NextStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            const forLoop: ForLoopNode = {
                kind: ASTNodeType.ForLoop,
                startToken: mockToken,
                endToken: mockToken,
                declaration: forStatementNode,
                body: [mockStatementNode],
                next: nextStatementNode,
            };

            expect(forLoop.kind).toBe(ASTNodeType.ForLoop);
            expect(forLoop.declaration).toBe(forStatementNode);
            expect(forLoop.body).toHaveLength(1);
            expect(forLoop.next).toBe(nextStatementNode);
        });

        it("should support multiple statements in body", () => {
            const forStatementNode: ForStatementNode = {
                kind: ASTNodeType.ForStatement,
                startToken: mockToken,
                endToken: mockToken,
                variable: mockToken,
                startValue: mockExpressionNode,
                endValue: mockExpressionNode,
            };

            const nextStatementNode: NextStatementNode = {
                kind: ASTNodeType.NextStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            const forLoop: ForLoopNode = {
                kind: ASTNodeType.ForLoop,
                startToken: mockToken,
                endToken: mockToken,
                declaration: forStatementNode,
                body: [mockStatementNode, mockStatementNode, mockStatementNode],
                next: nextStatementNode,
            };

            expect(forLoop.body).toHaveLength(3);
        });
    });

    describe("ForStatementNode", () => {
        it("should have correct structure", () => {
            const forStatement: ForStatementNode = {
                kind: ASTNodeType.ForStatement,
                startToken: mockToken,
                endToken: mockToken,
                variable: mockToken,
                startValue: mockExpressionNode,
                endValue: mockExpressionNode,
            };

            expect(forStatement.kind).toBe(ASTNodeType.ForStatement);
            expect(forStatement.variable).toBe(mockToken);
            expect(forStatement.startValue).toBe(mockExpressionNode);
            expect(forStatement.endValue).toBe(mockExpressionNode);
        });
    });

    describe("NextStatementNode", () => {
        it("should have correct structure", () => {
            const nextStatement: NextStatementNode = {
                kind: ASTNodeType.NextStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(nextStatement.kind).toBe(ASTNodeType.NextStatement);
            expect(nextStatement.startToken).toBe(mockToken);
            expect(nextStatement.endToken).toBe(mockToken);
        });
    });

    describe("ExitWhileStatementNode", () => {
        it("should have correct structure", () => {
            const exitWhileStatement: ExitWhileStatementNode = {
                kind: ASTNodeType.ExitWhileStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(exitWhileStatement.kind).toBe(ASTNodeType.ExitWhileStatement);
            expect(exitWhileStatement.startToken).toBe(mockToken);
            expect(exitWhileStatement.endToken).toBe(mockToken);
        });
    });

    describe("ExitForStatementNode", () => {
        it("should have correct structure", () => {
            const exitForStatement: ExitForStatementNode = {
                kind: ASTNodeType.ExitForStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(exitForStatement.kind).toBe(ASTNodeType.ExitForStatement);
            expect(exitForStatement.startToken).toBe(mockToken);
            expect(exitForStatement.endToken).toBe(mockToken);
        });
    });

    describe("LoopContinueNode", () => {
        it("should have correct structure", () => {
            const loopContinue: LoopContinueNode = {
                kind: ASTNodeType.LoopContinue,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(loopContinue.kind).toBe(ASTNodeType.LoopContinue);
            expect(loopContinue.startToken).toBe(mockToken);
            expect(loopContinue.endToken).toBe(mockToken);
        });
    });

    describe("Control flow consistency", () => {
        it("should have consistent naming patterns", () => {
            const controlFlowTypes = [
                ASTNodeType.IfStatement,
                ASTNodeType.ElseStatement,
                ASTNodeType.EndIfStatement,
                ASTNodeType.WhileStatement,
                ASTNodeType.EndWhileStatement,
                ASTNodeType.ForStatement,
                ASTNodeType.NextStatement,
                ASTNodeType.ExitWhileStatement,
                ASTNodeType.ExitForStatement,
            ];

            controlFlowTypes.forEach((type) => {
                expect(type).toContain("Statement");
            });
        });

        it("should have loop-related types", () => {
            const loopTypes = [
                ASTNodeType.LoopStatement,
                ASTNodeType.WhileLoop,
                ASTNodeType.ForLoop,
                ASTNodeType.LoopContinue,
            ];

            loopTypes.forEach((type) => {
                expect(type).toMatch(/(Loop|Continue)/);
            });
        });
    });
});
