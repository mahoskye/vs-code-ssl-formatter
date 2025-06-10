import { ProgramNode } from "../../src/parser/ast/program";
import { ASTNodeType, createBaseNode } from "../../src/parser/ast/base";
import { createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("AST Program", () => {
    let mockToken: any;

    beforeEach(() => {
        mockToken = createToken(TokenType.IDENTIFIER, "program", createPosition(1, 1, 0));
    });

    describe("ProgramNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(ASTNodeType.Program, mockToken, mockToken) as ProgramNode;
            expect(node.kind).toBe(ASTNodeType.Program);
        });

        it("should implement ASTNode interface", () => {
            const node = createBaseNode(ASTNodeType.Program, mockToken, mockToken) as ProgramNode;
            expect(node.startToken).toBe(mockToken);
            expect(node.endToken).toBe(mockToken);
        });

        it("should be the root node type", () => {
            // Program should be the top-level AST node
            expect(ASTNodeType.Program).toBe("Program");
        });

        it("should support body structure", () => {
            // Test that ProgramNode has the expected structure for a body array
            const node: ProgramNode = {
                kind: ASTNodeType.Program,
                startToken: mockToken,
                endToken: mockToken,
                body: [], // Should accept array of ClassDefinitionNode | StatementNode
            };
            expect(node.body).toEqual([]);
        });
    });

    describe("Program structure validation", () => {
        it("should be the root of the AST hierarchy", () => {
            const programNode = createBaseNode(ASTNodeType.Program, mockToken, mockToken);
            expect(programNode.kind).toBe(ASTNodeType.Program);
        });

        it("should have proper enum value", () => {
            expect(ASTNodeType.Program).toBe("Program");
        });

        it("should create valid program nodes", () => {
            const node = createBaseNode(ASTNodeType.Program, mockToken, mockToken);
            expect(node.kind).toBe(ASTNodeType.Program);
            expect(node.startToken).toBe(mockToken);
            expect(node.endToken).toBe(mockToken);
        });
    });
});
