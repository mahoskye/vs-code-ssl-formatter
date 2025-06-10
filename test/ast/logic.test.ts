import {
    LogicStatementNode,
    AssignmentNode,
    ReturnStatementNode,
} from "../../src/parser/ast/logic";
import { ASTNodeType, createBaseNode } from "../../src/parser/ast/base";
import { createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("AST Logic", () => {
    let mockToken: any;

    beforeEach(() => {
        mockToken = createToken(TokenType.IDENTIFIER, "variable", createPosition(1, 1, 0));
    });

    describe("LogicStatementNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.LogicStatement,
                mockToken,
                mockToken
            ) as LogicStatementNode;
            expect(node.kind).toBe(ASTNodeType.LogicStatement);
        });

        it("should implement ASTNode interface", () => {
            const node = createBaseNode(
                ASTNodeType.LogicStatement,
                mockToken,
                mockToken
            ) as LogicStatementNode;
            expect(node.startToken).toBe(mockToken);
            expect(node.endToken).toBe(mockToken);
        });
    });

    describe("AssignmentNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.Assignment,
                mockToken,
                mockToken
            ) as AssignmentNode;
            expect(node.kind).toBe(ASTNodeType.Assignment);
        });

        it("should implement ASTNode interface", () => {
            const node = createBaseNode(
                ASTNodeType.Assignment,
                mockToken,
                mockToken
            ) as AssignmentNode;
            expect(node.startToken).toBe(mockToken);
            expect(node.endToken).toBe(mockToken);
        });

        it("should be a fundamental statement type", () => {
            // Assignment is one of the most common statement types
            expect(ASTNodeType.Assignment).toBe("Assignment");
        });
    });

    describe("ReturnStatementNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.ReturnStatement,
                mockToken,
                mockToken
            ) as ReturnStatementNode;
            expect(node.kind).toBe(ASTNodeType.ReturnStatement);
        });

        it("should implement ASTNode interface", () => {
            const node = createBaseNode(
                ASTNodeType.ReturnStatement,
                mockToken,
                mockToken
            ) as ReturnStatementNode;
            expect(node.startToken).toBe(mockToken);
            expect(node.endToken).toBe(mockToken);
        });

        it("should follow statement naming convention", () => {
            expect(ASTNodeType.ReturnStatement).toContain("Statement");
        });
    });

    describe("Logic statement validation", () => {
        it("should have all required logic statement types", () => {
            const logicTypes = [
                ASTNodeType.LogicStatement,
                ASTNodeType.Assignment,
                ASTNodeType.ReturnStatement,
            ];

            logicTypes.forEach((type) => {
                expect(type).toBeDefined();
                const node = createBaseNode(type, mockToken, mockToken);
                expect(node.kind).toBe(type);
            });
        });

        it("should support basic logic operations", () => {
            // Test that we can create all types of logic statements
            const assignmentNode = createBaseNode(ASTNodeType.Assignment, mockToken, mockToken);
            const returnNode = createBaseNode(ASTNodeType.ReturnStatement, mockToken, mockToken);
            const logicNode = createBaseNode(ASTNodeType.LogicStatement, mockToken, mockToken);

            expect(assignmentNode.kind).toBe(ASTNodeType.Assignment);
            expect(returnNode.kind).toBe(ASTNodeType.ReturnStatement);
            expect(logicNode.kind).toBe(ASTNodeType.LogicStatement);
        });

        it("should have proper enum values", () => {
            expect(ASTNodeType.LogicStatement).toBe("LogicStatement");
            expect(ASTNodeType.Assignment).toBe("Assignment");
            expect(ASTNodeType.ReturnStatement).toBe("ReturnStatement");
        });
    });
});
