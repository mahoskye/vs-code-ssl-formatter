import { IdentifierListNode, ExpressionListNode } from "../../src/parser/ast/lists";
import { ASTNodeType, createBaseNode } from "../../src/parser/ast/base";
import { createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("AST Lists", () => {
    let mockToken: any;

    beforeEach(() => {
        mockToken = createToken(TokenType.IDENTIFIER, "test", createPosition(1, 1, 0));
    });

    describe("IdentifierListNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.IdentifierList,
                mockToken,
                mockToken
            ) as IdentifierListNode;
            expect(node.kind).toBe(ASTNodeType.IdentifierList);
        });

        it("should implement ASTNode interface", () => {
            const node = createBaseNode(
                ASTNodeType.IdentifierList,
                mockToken,
                mockToken
            ) as IdentifierListNode;
            expect(node.startToken).toBe(mockToken);
            expect(node.endToken).toBe(mockToken);
        });

        it("should be properly typed for identifiers array", () => {
            // This test ensures the interface structure is correct
            const node: IdentifierListNode = {
                kind: ASTNodeType.IdentifierList,
                startToken: mockToken,
                endToken: mockToken,
                identifiers: [mockToken], // Should accept Token array
            };
            expect(node.identifiers).toEqual([mockToken]);
        });
    });

    describe("ExpressionListNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.ExpressionList,
                mockToken,
                mockToken
            ) as ExpressionListNode;
            expect(node.kind).toBe(ASTNodeType.ExpressionList);
        });

        it("should implement ASTNode interface", () => {
            const node = createBaseNode(
                ASTNodeType.ExpressionList,
                mockToken,
                mockToken
            ) as ExpressionListNode;
            expect(node.startToken).toBe(mockToken);
            expect(node.endToken).toBe(mockToken);
        });
    });

    describe("List node types validation", () => {
        it("should have all required list node types", () => {
            const listTypes = [
                ASTNodeType.IdentifierList,
                ASTNodeType.ExpressionList,
                ASTNodeType.ArgumentList, // Also used in functions
            ];

            listTypes.forEach((type) => {
                expect(type).toBeDefined();
                const node = createBaseNode(type, mockToken, mockToken);
                expect(node.kind).toBe(type);
            });
        });

        it("should have consistent naming for list types", () => {
            const listTypes = [
                ASTNodeType.IdentifierList,
                ASTNodeType.ExpressionList,
                ASTNodeType.ArgumentList,
                ASTNodeType.ParameterList,
                ASTNodeType.DefaultParameterList,
            ];

            listTypes.forEach((type) => {
                expect(type).toContain("List");
            });
        });

        it("should create nodes for all list types", () => {
            const listTypes = [
                ASTNodeType.IdentifierList,
                ASTNodeType.ExpressionList,
                ASTNodeType.ArgumentList,
            ];

            listTypes.forEach((type) => {
                const node = createBaseNode(type, mockToken, mockToken);
                expect(node.kind).toBe(type);
                expect(node.startToken).toBe(mockToken);
                expect(node.endToken).toBe(mockToken);
            });
        });
    });
});
