import { ObjectCreationNode, MethodCallNode } from "../../src/parser/ast/objects";
import { ASTNodeType, createBaseNode } from "../../src/parser/ast/base";
import { createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("AST Objects", () => {
    let mockToken: any;

    beforeEach(() => {
        mockToken = createToken(TokenType.IDENTIFIER, "object", createPosition(1, 1, 0));
    });

    describe("ObjectCreationNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.ObjectCreation,
                mockToken,
                mockToken
            ) as ObjectCreationNode;
            expect(node.kind).toBe(ASTNodeType.ObjectCreation);
        });

        it("should implement ASTNode interface", () => {
            const node = createBaseNode(
                ASTNodeType.ObjectCreation,
                mockToken,
                mockToken
            ) as ObjectCreationNode;
            expect(node.startToken).toBe(mockToken);
            expect(node.endToken).toBe(mockToken);
        });
    });

    describe("MethodCallNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.MethodCall,
                mockToken,
                mockToken
            ) as MethodCallNode;
            expect(node.kind).toBe(ASTNodeType.MethodCall);
        });

        it("should implement ASTNode interface", () => {
            const node = createBaseNode(
                ASTNodeType.MethodCall,
                mockToken,
                mockToken
            ) as MethodCallNode;
            expect(node.startToken).toBe(mockToken);
            expect(node.endToken).toBe(mockToken);
        });
    });

    describe("Object-oriented features validation", () => {
        it("should have all required object-oriented node types", () => {
            const objectTypes = [
                ASTNodeType.ObjectCreation,
                ASTNodeType.MethodCall,
                ASTNodeType.ObjectPropertyAccess,
            ];

            objectTypes.forEach((type) => {
                expect(type).toBeDefined();
                const node = createBaseNode(type, mockToken, mockToken);
                expect(node.kind).toBe(type);
            });
        });

        it("should support object creation and method calls", () => {
            const creationNode = createBaseNode(ASTNodeType.ObjectCreation, mockToken, mockToken);
            const methodNode = createBaseNode(ASTNodeType.MethodCall, mockToken, mockToken);

            expect(creationNode.kind).toBe(ASTNodeType.ObjectCreation);
            expect(methodNode.kind).toBe(ASTNodeType.MethodCall);
        });

        it("should have proper enum values", () => {
            expect(ASTNodeType.ObjectCreation).toBe("ObjectCreation");
            expect(ASTNodeType.MethodCall).toBe("MethodCall");
            expect(ASTNodeType.ObjectPropertyAccess).toBe("ObjectPropertyAccess");
        });

        it("should distinguish between different object operations", () => {
            // Object creation vs method call vs property access should be distinct
            const types = [
                ASTNodeType.ObjectCreation,
                ASTNodeType.MethodCall,
                ASTNodeType.ObjectPropertyAccess,
            ];

            const uniqueTypes = new Set(types);
            expect(types.length).toBe(uniqueTypes.size);
        });
    });
});
