/**
 * SSL SQL Formatter Tests
 *
 * Tests for SQL-related formatting functionality including:
 * - SqlExecute function call formatting
 * - LSearch function call formatting
 * - SQL parameter formatting (?param?, ?)
 * - Multi-line SQL query formatting
 * - Oracle-specific SQL constructs
 * - Standard SQL formatting
 */

import {
    ASTNodeType,
    SqlStatementNode,
    SqlExecuteNode,
    LSearchNode,
    SqlParameterNode,
    StringLiteralNode,
    ArrayLiteralNode,
    ExpressionNode,
} from "../../src/parser/ast";
import { SSLSqlFormatterVisitor } from "../../src/formatter/sql";
import { FormatterOptions, defaultFormatterOptions } from "../../src/formatter/options";
import { Token, createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("SSLSqlFormatterVisitor", () => {
    let visitor: SSLSqlFormatterVisitor;
    let options: FormatterOptions;
    let mockToken: Token;

    beforeEach(() => {
        options = { ...defaultFormatterOptions };
        visitor = new SSLSqlFormatterVisitor(options);
        mockToken = createToken(TokenType.IDENTIFIER, "test", createPosition(1, 1, 0));
    }); // ================================
    // Helper Functions
    // ================================

    const createStringLiteral = (value: string): StringLiteralNode => ({
        kind: ASTNodeType.StringLiteral,
        startToken: createToken(TokenType.STRING, value, createPosition(1, 1, 0)),
        endToken: createToken(TokenType.STRING, value, createPosition(1, 1, 0)),
        value: value,
        token: createToken(TokenType.STRING, value, createPosition(1, 1, 0)),
    });

    const createArrayLiteral = (elements: any[] = []): ArrayLiteralNode => ({
        kind: ASTNodeType.ArrayLiteral,
        startToken: createToken(TokenType.LBRACE, "{", createPosition(1, 1, 0)),
        endToken: createToken(TokenType.RBRACE, "}", createPosition(1, 1, 0)),
        elements,
    });

    // ================================
    // SqlExecute Formatting Tests
    // ================================

    describe("SqlExecute Function Formatting", () => {
        it("should format simple SqlExecute calls", () => {
            const queryNode = createStringLiteral('"SELECT * FROM users"');

            const node: SqlExecuteNode = {
                kind: ASTNodeType.SqlExecute,
                startToken: mockToken,
                endToken: mockToken,
                query: queryNode,
            };

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe('SqlExecute("SELECT * FROM users")');
        });

        it("should format SqlExecute with parameters", () => {
            const queryNode = createStringLiteral('"SELECT * FROM users WHERE id = ?userId?"');
            const parametersNode = createArrayLiteral(["value1", "value2"]);

            const node: SqlExecuteNode = {
                kind: ASTNodeType.SqlExecute,
                startToken: mockToken,
                endToken: mockToken,
                query: queryNode,
                parameters: parametersNode,
            };

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toContain('SqlExecute("SELECT * FROM users WHERE id = ?userId?",');
        });

        it("should format SqlExecute with spacing after commas when enabled", () => {
            options.insertSpacesAfterCommas = true;
            visitor = new SSLSqlFormatterVisitor(options);

            const queryNode = createStringLiteral('"SELECT * FROM users"');
            const parametersNode = createArrayLiteral(["param1"]);

            const node: SqlExecuteNode = {
                kind: ASTNodeType.SqlExecute,
                startToken: mockToken,
                endToken: mockToken,
                query: queryNode,
                parameters: parametersNode,
            };

            const result = visitor.visit(node);

            const output = visitor.getFormattedOutput().trim();
            expect(output).toContain('SqlExecute("SELECT * FROM users", ');
        });

        it("should format SqlExecute without spacing after commas when disabled", () => {
            options.insertSpacesAfterCommas = false;
            visitor = new SSLSqlFormatterVisitor(options);

            const queryNode = createStringLiteral('"SELECT * FROM users"');
            const parametersNode = createArrayLiteral(["param1"]);

            const node: SqlExecuteNode = {
                kind: ASTNodeType.SqlExecute,
                startToken: mockToken,
                endToken: mockToken,
                query: queryNode,
                parameters: parametersNode,
            };

            const result = visitor.visit(node);

            const output = visitor.getFormattedOutput().trim();
            expect(output).toContain('SqlExecute("SELECT * FROM users",');
        });
    });

    // ================================
    // LSearch Formatting Tests
    // ================================

    describe("LSearch Function Formatting", () => {
        it("should format simple LSearch calls", () => {
            const queryNode = createStringLiteral('"SELECT * FROM users"');

            const node: LSearchNode = {
                kind: ASTNodeType.LSearch,
                startToken: mockToken,
                endToken: mockToken,
                query: queryNode,
            };

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe('LSearch("SELECT * FROM users")');
        });

        it("should format LSearch with all parameters", () => {
            const queryNode = createStringLiteral('"SELECT * FROM users WHERE id = ?"');
            const param1Node: ExpressionNode = {
                kind: ASTNodeType.VariableAccess,
                startToken: mockToken,
                endToken: mockToken,
            };
            const param2Node: ExpressionNode = {
                kind: ASTNodeType.VariableAccess,
                startToken: mockToken,
                endToken: mockToken,
            };
            const parametersNode = createArrayLiteral(["userId"]);

            const node: LSearchNode = {
                kind: ASTNodeType.LSearch,
                startToken: mockToken,
                endToken: mockToken,
                query: queryNode,
                parameter1: param1Node,
                parameter2: param2Node,
                parameters: parametersNode,
            };

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toContain('LSearch("SELECT * FROM users WHERE id = ?"');
        });

        it("should format LSearch with partial parameters", () => {
            const queryNode = createStringLiteral('"SELECT * FROM users WHERE id = ?"');
            const param1Node: ExpressionNode = {
                kind: ASTNodeType.VariableAccess,
                startToken: mockToken,
                endToken: mockToken,
            };

            const node: LSearchNode = {
                kind: ASTNodeType.LSearch,
                startToken: mockToken,
                endToken: mockToken,
                query: queryNode,
                parameter1: param1Node,
            };

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toContain('LSearch("SELECT * FROM users WHERE id = ?"');
        });
    });

    // ================================
    // SQL Parameter Formatting Tests
    // ================================

    describe("SQL Parameter Formatting", () => {
        it("should format named SQL parameters", () => {
            const node: SqlParameterNode = {
                kind: ASTNodeType.SqlParameter,
                startToken: mockToken,
                endToken: mockToken,
                parameterName: createToken(TokenType.IDENTIFIER, "userId", createPosition(1, 1, 0)),
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("?userId?");
        });

        it("should format unnamed SQL parameters", () => {
            const node: SqlParameterNode = {
                kind: ASTNodeType.SqlParameter,
                startToken: mockToken,
                endToken: mockToken,
            };

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("?");
        });
    });

    // ================================
    // Multi-line SQL Query Formatting Tests
    // ================================

    describe("Multi-line SQL Query Formatting", () => {
        it("should format complex SELECT statements as multi-line", () => {
            const complexQuery =
                '"SELECT u.id, u.name, p.title FROM users u JOIN profiles p ON u.id = p.user_id WHERE u.active = 1 ORDER BY u.name"';
            const queryNode = createStringLiteral(complexQuery);

            const node: SqlExecuteNode = {
                kind: ASTNodeType.SqlExecute,
                startToken: mockToken,
                endToken: mockToken,
                query: queryNode,
            };

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput();
            // Should contain multi-line formatting
            expect(output).toContain("SqlExecute(");
            expect(output.split("\n").length).toBeGreaterThan(1);
        });

        it("should keep simple queries on single line", () => {
            const simpleQuery = '"SELECT * FROM users"';
            const queryNode = createStringLiteral(simpleQuery);

            const node: SqlExecuteNode = {
                kind: ASTNodeType.SqlExecute,
                startToken: mockToken,
                endToken: mockToken,
                query: queryNode,
            };

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe('SqlExecute("SELECT * FROM users")');
            expect(output.split("\n").length).toBe(1);
        });
    });

    // ================================
    // Oracle-Specific SQL Tests
    // ================================

    describe("Oracle-Specific SQL Formatting", () => {
        it("should format Oracle queries with ROWNUM", () => {
            const oracleQuery = '"SELECT * FROM users WHERE ROWNUM <= 10"';
            const queryNode = createStringLiteral(oracleQuery);

            const node: SqlExecuteNode = {
                kind: ASTNodeType.SqlExecute,
                startToken: mockToken,
                endToken: mockToken,
                query: queryNode,
            };

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput();
            expect(output).toContain("ROWNUM");
        });

        it("should format Oracle queries with CONNECT BY", () => {
            const hierarchicalQuery =
                '"SELECT level, name FROM categories START WITH parent_id IS NULL CONNECT BY PRIOR id = parent_id"';
            const queryNode = createStringLiteral(hierarchicalQuery);

            const node: SqlExecuteNode = {
                kind: ASTNodeType.SqlExecute,
                startToken: mockToken,
                endToken: mockToken,
                query: queryNode,
            };

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput();
            expect(output).toContain("CONNECT BY");
            expect(output).toContain("START WITH");
        });

        it("should format Oracle queries with hints", () => {
            const hintedQuery =
                '"SELECT /*+ INDEX(u idx_user_name) */ * FROM users u WHERE u.name = ?name?"';
            const queryNode = createStringLiteral(hintedQuery);

            const node: SqlExecuteNode = {
                kind: ASTNodeType.SqlExecute,
                startToken: mockToken,
                endToken: mockToken,
                query: queryNode,
            };

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput();
            expect(output).toContain("/*+");
            expect(output).toContain("*/");
        });

        it("should format Oracle DECODE functions", () => {
            const decodeQuery =
                "\"SELECT DECODE(status, 1, 'Active', 0, 'Inactive', 'Unknown') FROM users\"";
            const queryNode = createStringLiteral(decodeQuery);

            const node: SqlExecuteNode = {
                kind: ASTNodeType.SqlExecute,
                startToken: mockToken,
                endToken: mockToken,
                query: queryNode,
            };

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput();
            expect(output).toContain("DECODE");
        });

        it("should format Oracle NVL functions", () => {
            const nvlQuery = "\"SELECT NVL(middle_name, 'N/A') FROM users\"";
            const queryNode = createStringLiteral(nvlQuery);

            const node: SqlExecuteNode = {
                kind: ASTNodeType.SqlExecute,
                startToken: mockToken,
                endToken: mockToken,
                query: queryNode,
            };

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput();
            expect(output).toContain("NVL");
        });

        it("should format Oracle window functions with PARTITION BY", () => {
            const windowQuery =
                '"SELECT name, ROW_NUMBER() OVER (PARTITION BY department ORDER BY salary DESC) FROM employees"';
            const queryNode = createStringLiteral(windowQuery);

            const node: SqlExecuteNode = {
                kind: ASTNodeType.SqlExecute,
                startToken: mockToken,
                endToken: mockToken,
                query: queryNode,
            };

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput();
            expect(output).toContain("PARTITION BY");
            expect(output).toContain("OVER");
        });

        it("should format Oracle MERGE statements", () => {
            const mergeQuery =
                '"MERGE INTO users u USING temp_users t ON (u.id = t.id) WHEN MATCHED THEN UPDATE SET u.name = t.name WHEN NOT MATCHED THEN INSERT VALUES (t.id, t.name)"';
            const queryNode = createStringLiteral(mergeQuery);

            const node: SqlExecuteNode = {
                kind: ASTNodeType.SqlExecute,
                startToken: mockToken,
                endToken: mockToken,
                query: queryNode,
            };

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput();
            expect(output).toContain("MERGE");
            expect(output).toContain("WHEN MATCHED");
            expect(output).toContain("WHEN NOT MATCHED");
        });
    });

    // ================================
    // SQL Statement Wrapper Tests
    // ================================

    describe("SQL Statement Wrapper", () => {
        it("should handle SqlStatement wrapper nodes", () => {
            const node: SqlStatementNode = {
                kind: ASTNodeType.SqlStatement,
                startToken: mockToken,
                endToken: mockToken,
            };
            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false); // SqlStatement is just a wrapper, should not continue
            expect(result.error).toBeUndefined();
        });
    });

    // ================================
    // Error Handling Tests
    // ================================

    describe("Error Handling", () => {
        it("should handle missing query node gracefully", () => {
            const node: SqlExecuteNode = {
                kind: ASTNodeType.SqlExecute,
                startToken: mockToken,
                endToken: mockToken,
                query: undefined as any,
            };

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("SqlExecute()");
        });

        it("should handle LSearch with no parameters gracefully", () => {
            const queryNode = createStringLiteral('"SELECT * FROM users"');

            const node: LSearchNode = {
                kind: ASTNodeType.LSearch,
                startToken: mockToken,
                endToken: mockToken,
                query: queryNode,
            };

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe('LSearch("SELECT * FROM users")');
        });
    });

    // ================================
    // Integration Tests with Visitor Pattern
    // ================================

    describe("Visitor Integration", () => {
        it("should be an instance of FormatterVisitorBase", () => {
            expect(visitor).toBeInstanceOf(SSLSqlFormatterVisitor);
            expect(typeof visitor.visit).toBe("function");
            expect(typeof visitor.getFormattedOutput).toBe("function");
        });

        it("should maintain proper visitor result structure", () => {
            const queryNode = createStringLiteral('"SELECT * FROM users"');

            const node: SqlExecuteNode = {
                kind: ASTNodeType.SqlExecute,
                startToken: mockToken,
                endToken: mockToken,
                query: queryNode,
            };

            const result = visitor.visit(node);

            expect(result).toHaveProperty("shouldContinue");
            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();
        });
    });

    // ================================
    // Performance Tests
    // ================================

    describe("Performance", () => {
        it("should handle large SQL queries efficiently", () => {
            const largeQuery = '"' + "SELECT ".repeat(100) + "* FROM users" + '"';
            const queryNode = createStringLiteral(largeQuery);

            const node: SqlExecuteNode = {
                kind: ASTNodeType.SqlExecute,
                startToken: mockToken,
                endToken: mockToken,
                query: queryNode,
            };

            const startTime = Date.now();
            const result = visitor.visit(node);
            const endTime = Date.now();

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();
            expect(endTime - startTime).toBeLessThan(100); // Should complete within 100ms
        });
    });
});
