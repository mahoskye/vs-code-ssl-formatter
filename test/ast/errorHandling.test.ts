import {
    ErrorHandlingStatementNode,
    TryBlockNode,
    TryStatementNode,
    CatchBlockNode,
    CatchStatementNode,
    FinallyBlockNode,
    FinallyStatementNode,
    EndTryStatementNode,
    ErrorBlockStanzaNode,
    ErrorMarkerNode,
} from "../../src/parser/ast/errorHandling";
import { ASTNodeType, createBaseNode } from "../../src/parser/ast/base";
import { createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("AST Error Handling", () => {
    let mockToken: any;

    beforeEach(() => {
        mockToken = createToken(TokenType.TRY, "try", createPosition(1, 1, 0));
    });

    describe("ErrorHandlingStatementNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.ErrorHandlingStatement,
                mockToken,
                mockToken
            ) as ErrorHandlingStatementNode;
            expect(node.kind).toBe(ASTNodeType.ErrorHandlingStatement);
        });
    });

    describe("TryBlockNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(ASTNodeType.TryBlock, mockToken, mockToken) as TryBlockNode;
            expect(node.kind).toBe(ASTNodeType.TryBlock);
        });
    });

    describe("TryStatementNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.TryStatement,
                mockToken,
                mockToken
            ) as TryStatementNode;
            expect(node.kind).toBe(ASTNodeType.TryStatement);
        });
    });

    describe("CatchBlockNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.CatchBlock,
                mockToken,
                mockToken
            ) as CatchBlockNode;
            expect(node.kind).toBe(ASTNodeType.CatchBlock);
        });
    });

    describe("CatchStatementNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.CatchStatement,
                mockToken,
                mockToken
            ) as CatchStatementNode;
            expect(node.kind).toBe(ASTNodeType.CatchStatement);
        });
    });

    describe("FinallyBlockNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.FinallyBlock,
                mockToken,
                mockToken
            ) as FinallyBlockNode;
            expect(node.kind).toBe(ASTNodeType.FinallyBlock);
        });
    });

    describe("FinallyStatementNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.FinallyStatement,
                mockToken,
                mockToken
            ) as FinallyStatementNode;
            expect(node.kind).toBe(ASTNodeType.FinallyStatement);
        });
    });

    describe("EndTryStatementNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.EndTryStatement,
                mockToken,
                mockToken
            ) as EndTryStatementNode;
            expect(node.kind).toBe(ASTNodeType.EndTryStatement);
        });
    });

    describe("ErrorBlockStanzaNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.ErrorBlockStanza,
                mockToken,
                mockToken
            ) as ErrorBlockStanzaNode;
            expect(node.kind).toBe(ASTNodeType.ErrorBlockStanza);
        });
    });

    describe("ErrorMarkerNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.ErrorMarker,
                mockToken,
                mockToken
            ) as ErrorMarkerNode;
            expect(node.kind).toBe(ASTNodeType.ErrorMarker);
        });
    });

    describe("Error handling structure validation", () => {
        it("should have all required error handling node types", () => {
            const errorHandlingTypes = [
                ASTNodeType.ErrorHandlingStatement,
                ASTNodeType.TryBlock,
                ASTNodeType.TryStatement,
                ASTNodeType.CatchBlock,
                ASTNodeType.CatchStatement,
                ASTNodeType.FinallyBlock,
                ASTNodeType.FinallyStatement,
                ASTNodeType.EndTryStatement,
                ASTNodeType.ErrorBlockStanza,
                ASTNodeType.ErrorMarker,
            ];

            errorHandlingTypes.forEach((type) => {
                expect(type).toBeDefined();
                const node = createBaseNode(type, mockToken, mockToken);
                expect(node.kind).toBe(type);
            });
        });

        it("should have consistent naming for error handling types", () => {
            const tryTypes = [
                ASTNodeType.TryBlock,
                ASTNodeType.TryStatement,
                ASTNodeType.EndTryStatement,
            ];
            const catchTypes = [ASTNodeType.CatchBlock, ASTNodeType.CatchStatement];
            const finallyTypes = [ASTNodeType.FinallyBlock, ASTNodeType.FinallyStatement];

            tryTypes.forEach((type) => {
                expect(type).toMatch(/(Try|EndTry)/);
            });

            catchTypes.forEach((type) => {
                expect(type).toContain("Catch");
            });

            finallyTypes.forEach((type) => {
                expect(type).toContain("Finally");
            });
        });
    });
});
