import {
    SqlStatementNode,
    SqlExecuteNode,
    LSearchNode,
    SqlParameterNode,
} from "../../src/parser/ast/sql";
import { ASTNodeType, createBaseNode } from "../../src/parser/ast/base";
import { createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("AST SQL", () => {
    let mockToken: any;

    beforeEach(() => {
        mockToken = createToken(TokenType.STRING, '"SELECT * FROM table"', createPosition(1, 1, 0));
    });

    describe("SqlStatementNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.SqlStatement,
                mockToken,
                mockToken
            ) as SqlStatementNode;
            expect(node.kind).toBe(ASTNodeType.SqlStatement);
        });

        it("should implement ASTNode interface", () => {
            const node = createBaseNode(
                ASTNodeType.SqlStatement,
                mockToken,
                mockToken
            ) as SqlStatementNode;
            expect(node.startToken).toBe(mockToken);
            expect(node.endToken).toBe(mockToken);
        });
    });

    describe("SqlExecuteNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.SqlExecute,
                mockToken,
                mockToken
            ) as SqlExecuteNode;
            expect(node.kind).toBe(ASTNodeType.SqlExecute);
        });

        it("should implement ASTNode interface", () => {
            const node = createBaseNode(
                ASTNodeType.SqlExecute,
                mockToken,
                mockToken
            ) as SqlExecuteNode;
            expect(node.startToken).toBe(mockToken);
            expect(node.endToken).toBe(mockToken);
        });
    });

    describe("LSearchNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(ASTNodeType.LSearch, mockToken, mockToken) as LSearchNode;
            expect(node.kind).toBe(ASTNodeType.LSearch);
        });

        it("should implement ASTNode interface", () => {
            const node = createBaseNode(ASTNodeType.LSearch, mockToken, mockToken) as LSearchNode;
            expect(node.startToken).toBe(mockToken);
            expect(node.endToken).toBe(mockToken);
        });
    });

    describe("SqlParameterNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.SqlParameter,
                mockToken,
                mockToken
            ) as SqlParameterNode;
            expect(node.kind).toBe(ASTNodeType.SqlParameter);
        });

        it("should implement ASTNode interface", () => {
            const node = createBaseNode(
                ASTNodeType.SqlParameter,
                mockToken,
                mockToken
            ) as SqlParameterNode;
            expect(node.startToken).toBe(mockToken);
            expect(node.endToken).toBe(mockToken);
        });
    });

    describe("SQL integration validation", () => {
        it("should have all required SQL node types", () => {
            const sqlTypes = [
                ASTNodeType.SqlStatement,
                ASTNodeType.SqlExecute,
                ASTNodeType.LSearch,
                ASTNodeType.SqlParameter,
            ];

            sqlTypes.forEach((type) => {
                expect(type).toBeDefined();
                const node = createBaseNode(type, mockToken, mockToken);
                expect(node.kind).toBe(type);
            });
        });

        it("should have consistent naming for SQL types", () => {
            const sqlTypes = [
                ASTNodeType.SqlStatement,
                ASTNodeType.SqlExecute,
                ASTNodeType.SqlParameter,
            ];

            sqlTypes.forEach((type) => {
                expect(type).toMatch(/Sql/);
            });
        });

        it("should support SSL's SQL integration features", () => {
            // Test SQL execution
            const executeNode = createBaseNode(ASTNodeType.SqlExecute, mockToken, mockToken);
            expect(executeNode.kind).toBe(ASTNodeType.SqlExecute);

            // Test LSearch (specific SSL SQL feature)
            const lsearchNode = createBaseNode(ASTNodeType.LSearch, mockToken, mockToken);
            expect(lsearchNode.kind).toBe(ASTNodeType.LSearch);

            // Test SQL parameters
            const paramNode = createBaseNode(ASTNodeType.SqlParameter, mockToken, mockToken);
            expect(paramNode.kind).toBe(ASTNodeType.SqlParameter);
        });

        it("should have proper enum values", () => {
            expect(ASTNodeType.SqlStatement).toBe("SqlStatement");
            expect(ASTNodeType.SqlExecute).toBe("SqlExecute");
            expect(ASTNodeType.LSearch).toBe("LSearch");
            expect(ASTNodeType.SqlParameter).toBe("SqlParameter");
        });

        it("should distinguish between different SQL operations", () => {
            const sqlTypes = [
                ASTNodeType.SqlStatement,
                ASTNodeType.SqlExecute,
                ASTNodeType.LSearch,
                ASTNodeType.SqlParameter,
            ];

            const uniqueTypes = new Set(sqlTypes);
            expect(sqlTypes.length).toBe(uniqueTypes.size);
        });
    });
});
