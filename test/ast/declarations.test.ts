import {
    DeclarationStatementNode,
    ParametersStatementNode,
    DeclareStatementNode,
    DefaultStatementNode,
    PublicStatementNode,
    IncludeStatementNode,
} from "../../src/parser/ast/declarations";
import { ASTNodeType, createBaseNode } from "../../src/parser/ast/base";
import { createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("AST Declarations", () => {
    let mockToken: any;

    beforeEach(() => {
        mockToken = createToken(TokenType.IDENTIFIER, "test", createPosition(1, 1, 0));
    });

    describe("DeclarationStatementNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.DeclarationStatement,
                mockToken,
                mockToken
            ) as DeclarationStatementNode;
            expect(node.kind).toBe(ASTNodeType.DeclarationStatement);
        });

        it("should implement ASTNode interface", () => {
            const node = createBaseNode(
                ASTNodeType.DeclarationStatement,
                mockToken,
                mockToken
            ) as DeclarationStatementNode;
            expect(node.startToken).toBe(mockToken);
            expect(node.endToken).toBe(mockToken);
        });
    });

    describe("ParametersStatementNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.ParametersStatement,
                mockToken,
                mockToken
            ) as ParametersStatementNode;
            expect(node.kind).toBe(ASTNodeType.ParametersStatement);
        });

        it("should implement ASTNode interface", () => {
            const node = createBaseNode(
                ASTNodeType.ParametersStatement,
                mockToken,
                mockToken
            ) as ParametersStatementNode;
            expect(node.startToken).toBe(mockToken);
            expect(node.endToken).toBe(mockToken);
        });
    });

    describe("DeclareStatementNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.DeclareStatement,
                mockToken,
                mockToken
            ) as DeclareStatementNode;
            expect(node.kind).toBe(ASTNodeType.DeclareStatement);
        });

        it("should implement ASTNode interface", () => {
            const node = createBaseNode(
                ASTNodeType.DeclareStatement,
                mockToken,
                mockToken
            ) as DeclareStatementNode;
            expect(node.startToken).toBe(mockToken);
            expect(node.endToken).toBe(mockToken);
        });
    });

    describe("DefaultStatementNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.DefaultStatement,
                mockToken,
                mockToken
            ) as DefaultStatementNode;
            expect(node.kind).toBe(ASTNodeType.DefaultStatement);
        });

        it("should implement ASTNode interface", () => {
            const node = createBaseNode(
                ASTNodeType.DefaultStatement,
                mockToken,
                mockToken
            ) as DefaultStatementNode;
            expect(node.startToken).toBe(mockToken);
            expect(node.endToken).toBe(mockToken);
        });
    });

    describe("PublicStatementNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.PublicStatement,
                mockToken,
                mockToken
            ) as PublicStatementNode;
            expect(node.kind).toBe(ASTNodeType.PublicStatement);
        });

        it("should implement ASTNode interface", () => {
            const node = createBaseNode(
                ASTNodeType.PublicStatement,
                mockToken,
                mockToken
            ) as PublicStatementNode;
            expect(node.startToken).toBe(mockToken);
            expect(node.endToken).toBe(mockToken);
        });
    });

    describe("IncludeStatementNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.IncludeStatement,
                mockToken,
                mockToken
            ) as IncludeStatementNode;
            expect(node.kind).toBe(ASTNodeType.IncludeStatement);
        });

        it("should implement ASTNode interface", () => {
            const node = createBaseNode(
                ASTNodeType.IncludeStatement,
                mockToken,
                mockToken
            ) as IncludeStatementNode;
            expect(node.startToken).toBe(mockToken);
            expect(node.endToken).toBe(mockToken);
        });
    });

    describe("All declaration node types", () => {
        it("should be properly defined in ASTNodeType enum", () => {
            expect(ASTNodeType.DeclarationStatement).toBe("DeclarationStatement");
            expect(ASTNodeType.ParametersStatement).toBe("ParametersStatement");
            expect(ASTNodeType.DeclareStatement).toBe("DeclareStatement");
            expect(ASTNodeType.DefaultStatement).toBe("DefaultStatement");
            expect(ASTNodeType.PublicStatement).toBe("PublicStatement");
            expect(ASTNodeType.IncludeStatement).toBe("IncludeStatement");
        });

        it("should create nodes for all declaration types", () => {
            const declarationTypes = [
                ASTNodeType.DeclarationStatement,
                ASTNodeType.ParametersStatement,
                ASTNodeType.DeclareStatement,
                ASTNodeType.DefaultStatement,
                ASTNodeType.PublicStatement,
                ASTNodeType.IncludeStatement,
            ];

            declarationTypes.forEach((type) => {
                const node = createBaseNode(type, mockToken, mockToken);
                expect(node.kind).toBe(type);
                expect(node.startToken).toBe(mockToken);
                expect(node.endToken).toBe(mockToken);
            });
        });
    });
});
