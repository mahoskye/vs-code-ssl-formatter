/**
 * Tests for Line Breaking Manager
 *
 * Comprehensive test suite covering:
 * - Basic line breaking decisions
 * - Function call line breaking
 * - Array literal line breaking
 * - SQL statement line breaking
 * - Complex expression line breaking
 * - Breaking strategy determination
 * - Alignment and indentation options
 */

import { SSLLineBreaker, BreakingStrategy } from "../../src/formatter/lineBreaking";
import { OutputBuilder } from "../../src/formatter/outputBuilder";
import { FormatterOptions, defaultFormatterOptions } from "../../src/formatter/options";
import {
    ExpressionNode,
    ASTNodeType,
    StringLiteralNode,
    NumberLiteralNode,
    VariableAccessNode,
} from "../../src/parser/ast";
import { createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("SSLLineBreaker", () => {
    let lineBreaker: SSLLineBreaker;
    let outputBuilder: OutputBuilder;
    let options: FormatterOptions;

    beforeEach(() => {
        options = { ...defaultFormatterOptions };
        lineBreaker = new SSLLineBreaker(options);
        outputBuilder = new OutputBuilder(options);
    });

    describe("Basic Line Breaking", () => {
        it("should determine when to break a line based on length", () => {
            options.maxLineLength = 50;
            lineBreaker = new SSLLineBreaker(options);

            expect(lineBreaker.shouldBreakLine(30, "short")).toBe(false);
            expect(
                lineBreaker.shouldBreakLine(30, "this is a very long string that exceeds the limit")
            ).toBe(true);
            expect(lineBreaker.shouldBreakLine(60)).toBe(true);
        });

        it("should calculate single line length correctly", () => {
            const text = "  hello    world  ";
            expect(lineBreaker.calculateSingleLineLength(text)).toBe(11); // "hello world"
        });

        it("should handle empty and whitespace-only text", () => {
            expect(lineBreaker.calculateSingleLineLength("")).toBe(0);
            expect(lineBreaker.calculateSingleLineLength("   ")).toBe(0);
        });
    });

    describe("Function Call Breaking Strategy", () => {
        it("should return no-break for short function calls", () => {
            const strategy = lineBreaker.getFunctionCallBreakingStrategy("test", 2, 30);
            expect(strategy).toBe("no-break");
        });

        it("should return all-break for very long function calls", () => {
            options.maxLineLength = 50;
            lineBreaker = new SSLLineBreaker(options);

            const strategy = lineBreaker.getFunctionCallBreakingStrategy("test", 3, 80);
            expect(strategy).toBe("all-break");
        });

        it("should return smart-break for functions with many parameters", () => {
            options.parameterListBreakThreshold = 3;
            lineBreaker = new SSLLineBreaker(options);

            const strategy = lineBreaker.getFunctionCallBreakingStrategy("test", 4, 40);
            expect(strategy).toBe("smart-break");
        });

        it("should return smart-break for SQL functions when enabled", () => {
            options.breakLongSqlStatements = true;
            lineBreaker = new SSLLineBreaker(options);

            const strategy = lineBreaker.getFunctionCallBreakingStrategy("SqlExecute", 2, 40);
            expect(strategy).toBe("smart-break");
        });

        it("should not break SQL functions when disabled", () => {
            options.breakLongSqlStatements = false;
            lineBreaker = new SSLLineBreaker(options);

            const strategy = lineBreaker.getFunctionCallBreakingStrategy("SqlExecute", 2, 40);
            expect(strategy).toBe("no-break");
        });
    });

    describe("Array Literal Breaking Strategy", () => {
        it("should return no-break when array breaking is disabled", () => {
            options.breakLongArrayLiterals = false;
            lineBreaker = new SSLLineBreaker(options);

            const strategy = lineBreaker.getArrayLiteralBreakingStrategy(10, 100);
            expect(strategy).toBe("no-break");
        });

        it("should return all-break for very long arrays", () => {
            options.breakLongArrayLiterals = true;
            options.maxLineLength = 50;
            lineBreaker = new SSLLineBreaker(options);

            const strategy = lineBreaker.getArrayLiteralBreakingStrategy(3, 80);
            expect(strategy).toBe("all-break");
        });

        it("should return smart-break for arrays with many elements", () => {
            options.breakLongArrayLiterals = true;
            lineBreaker = new SSLLineBreaker(options);

            const strategy = lineBreaker.getArrayLiteralBreakingStrategy(6, 40);
            expect(strategy).toBe("smart-break");
        });

        it("should return no-break for short arrays", () => {
            options.breakLongArrayLiterals = true;
            lineBreaker = new SSLLineBreaker(options);

            const strategy = lineBreaker.getArrayLiteralBreakingStrategy(3, 30);
            expect(strategy).toBe("no-break");
        });
    });

    describe("Function Call Formatting", () => {
        let mockExpressions: ExpressionNode[];

        beforeEach(() => {
            // Create mock expressions for testing
            mockExpressions = [
                createMockStringLiteral("param1"),
                createMockStringLiteral("param2"),
                createMockStringLiteral("param3"),
            ];
        });

        it("should format single-line function calls", () => {
            const renderElement = (expr: ExpressionNode) => '"test"';

            lineBreaker.formatFunctionCall(
                outputBuilder,
                "TestFunction",
                mockExpressions.slice(0, 2),
                renderElement
            );

            expect(outputBuilder.getOutput()).toBe('TestFunction("test", "test")\n');
        });

        it("should format multi-line function calls with indentation", () => {
            options.maxLineLength = 20; // Force line breaking
            options.alignProcedureParameters = false;
            lineBreaker = new SSLLineBreaker(options);

            const renderElement = (expr: ExpressionNode) => '"parameter"';

            lineBreaker.formatFunctionCall(
                outputBuilder,
                "TestFunction",
                mockExpressions,
                renderElement
            );

            const result = outputBuilder.getOutput();
            expect(result).toContain("TestFunction(");
            expect(result).toContain('    "parameter"');
            expect(result).toContain(",");
            expect(result).toContain(")");
        });

        it("should format multi-line function calls with alignment", () => {
            options.maxLineLength = 20; // Force line breaking
            options.alignProcedureParameters = true;
            lineBreaker = new SSLLineBreaker(options);

            const renderElement = (expr: ExpressionNode) => '"param"';

            lineBreaker.formatFunctionCall(
                outputBuilder,
                "Func",
                mockExpressions.slice(0, 2),
                renderElement
            );

            const result = outputBuilder.getOutput();
            expect(result).toContain('Func("param",');
            expect(result).toContain('     "param")'); // Aligned with opening parenthesis
        });

        it("should handle empty parameter lists", () => {
            lineBreaker.formatFunctionCall(outputBuilder, "EmptyFunction", [], () => "");

            expect(outputBuilder.getOutput()).toBe("EmptyFunction()\n");
        });
    });

    describe("Array Literal Formatting", () => {
        let mockExpressions: ExpressionNode[];

        beforeEach(() => {
            mockExpressions = [
                createMockNumberLiteral("1"),
                createMockNumberLiteral("2"),
                createMockNumberLiteral("3"),
                createMockNumberLiteral("4"),
                createMockNumberLiteral("5"),
            ];
        });

        it("should format single-line arrays", () => {
            const renderElement = (expr: ExpressionNode) => "1";

            lineBreaker.formatArrayLiteral(
                outputBuilder,
                mockExpressions.slice(0, 3),
                renderElement
            );

            expect(outputBuilder.getOutput()).toBe("{1, 1, 1}\n");
        });

        it("should format multi-line arrays with indentation", () => {
            options.maxLineLength = 15; // Force line breaking
            options.alignArrayElements = false;
            lineBreaker = new SSLLineBreaker(options);

            const renderElement = (expr: ExpressionNode) => "element";

            lineBreaker.formatArrayLiteral(outputBuilder, mockExpressions, renderElement);

            const result = outputBuilder.getOutput();
            expect(result).toContain("{");
            expect(result).toContain("    element");
            expect(result).toContain(",");
            expect(result).toContain("}");
        });

        it("should format multi-line arrays with alignment", () => {
            options.maxLineLength = 15; // Force line breaking
            options.alignArrayElements = true;
            lineBreaker = new SSLLineBreaker(options);

            const renderElement = (expr: ExpressionNode) => "item";

            lineBreaker.formatArrayLiteral(
                outputBuilder,
                mockExpressions.slice(0, 3),
                renderElement
            );
            const result = outputBuilder.getOutput();
            expect(result).toContain("{ item,");
            expect(result).toContain(" item,"); // Single space alignment
            expect(result).toContain(" item }"); // Single space alignment
        });

        it("should handle empty arrays", () => {
            lineBreaker.formatArrayLiteral(outputBuilder, [], () => "");

            expect(outputBuilder.getOutput()).toBe("{}\n");
        });
    });

    describe("SQL Statement Formatting", () => {
        it("should format single-line SQL when breaking is disabled", () => {
            options.breakLongSqlStatements = false;
            lineBreaker = new SSLLineBreaker(options);

            const sql = "SELECT name FROM users WHERE id = 1";
            lineBreaker.formatSqlStatement(outputBuilder, sql);

            expect(outputBuilder.getOutput()).toBe(sql + "\n");
        });

        it("should preserve original formatting when requested", () => {
            options.breakLongSqlStatements = true;
            lineBreaker = new SSLLineBreaker(options);

            const sql = "SELECT name FROM users WHERE id = 1";
            lineBreaker.formatSqlStatement(outputBuilder, sql, true);

            expect(outputBuilder.getOutput()).toBe(sql + "\n");
        });

        it("should format multi-line SQL when breaking is enabled", () => {
            options.breakLongSqlStatements = true;
            options.maxLineLength = 30;
            lineBreaker = new SSLLineBreaker(options);

            const sql = "SELECT name, email FROM users WHERE status = 'active' ORDER BY name";
            lineBreaker.formatSqlStatement(outputBuilder, sql);

            const result = outputBuilder.getOutput();
            expect(result).toContain("SELECT");
            expect(result).toContain("FROM");
            expect(result).toContain("WHERE");
            expect(result).toContain("ORDER BY");
        });

        it("should handle various SQL keywords", () => {
            options.breakLongSqlStatements = true;
            options.maxLineLength = 20;
            lineBreaker = new SSLLineBreaker(options);

            const sql = "INSERT INTO table VALUES (1, 2) UPDATE table SET col = 1";
            lineBreaker.formatSqlStatement(outputBuilder, sql);

            const result = outputBuilder.getOutput();
            expect(result).toContain("INSERT");
            expect(result).toContain("UPDATE");
        });
    });

    describe("Complex Expression Formatting", () => {
        it("should format single-line expressions", () => {
            const parts = ["a", "b"];
            const operators = ["+"];

            lineBreaker.formatComplexExpression(outputBuilder, parts, operators);

            expect(outputBuilder.getOutput()).toBe("a + b\n");
        });

        it("should format multi-line expressions when long", () => {
            options.maxLineLength = 10;
            lineBreaker = new SSLLineBreaker(options);

            const parts = ["variable1", "variable2", "variable3"];
            const operators = [".AND.", ".OR."];

            lineBreaker.formatComplexExpression(outputBuilder, parts, operators);

            const result = outputBuilder.getOutput();
            expect(result).toContain("variable1");
            expect(result).toContain("    .AND. variable2");
            expect(result).toContain("    .OR. variable3");
        });

        it("should handle single expression parts", () => {
            const parts = ["singleExpression"];
            const operators: string[] = [];

            lineBreaker.formatComplexExpression(outputBuilder, parts, operators);

            expect(outputBuilder.getOutput()).toBe("singleExpression\n");
        });
        it("should handle empty expression parts", () => {
            const parts: string[] = [];
            const operators: string[] = [];

            lineBreaker.formatComplexExpression(outputBuilder, parts, operators);

            expect(outputBuilder.getOutput()).toBe(""); // Empty input produces empty output
        });
    });

    describe("Expression Length Estimation", () => {
        it("should estimate expression length", () => {
            const mockExpression = createMockVariableAccess("testVariable");
            const length = lineBreaker.estimateExpressionLength(mockExpression);

            expect(typeof length).toBe("number");
            expect(length).toBeGreaterThan(0);
        });

        it("should determine if expression should be broken", () => {
            options.maxLineLength = 25;
            lineBreaker = new SSLLineBreaker(options);

            const mockExpression = createMockVariableAccess("test");
            const shouldBreak = lineBreaker.shouldBreakExpression(mockExpression);

            expect(typeof shouldBreak).toBe("boolean");
        });
    });

    describe("SSL-Specific Formatting", () => {
        it("should recognize SQL functions", () => {
            // Test SQL function recognition indirectly through breaking strategy
            options.breakLongSqlStatements = true;
            lineBreaker = new SSLLineBreaker(options);

            const sqlStrategy = lineBreaker.getFunctionCallBreakingStrategy("SqlExecute", 2, 40);
            const normalStrategy = lineBreaker.getFunctionCallBreakingStrategy(
                "RegularFunc",
                2,
                40
            );

            expect(sqlStrategy).toBe("smart-break");
            expect(normalStrategy).toBe("no-break");
        });

        it("should handle various SQL function names", () => {
            options.breakLongSqlStatements = true;
            lineBreaker = new SSLLineBreaker(options);

            const sqlFunctions = ["LSearch", "SqlExecDirect", "SqlPrepare", "SqlConnect"];

            sqlFunctions.forEach((funcName) => {
                const strategy = lineBreaker.getFunctionCallBreakingStrategy(funcName, 2, 40);
                expect(strategy).toBe("smart-break");
            });
        });
    });

    describe("Option Handling", () => {
        it("should respect insertSpacesAfterCommas option in aligned formatting", () => {
            options.insertSpacesAfterCommas = false;
            options.maxLineLength = 5; // Force line breaking
            options.alignProcedureParameters = true;
            lineBreaker = new SSLLineBreaker(options);

            const mockExprs = [createMockStringLiteral("a"), createMockStringLiteral("b")];
            const renderElement = (expr: ExpressionNode) => '"x"';

            lineBreaker.formatFunctionCall(outputBuilder, "Test", mockExprs, renderElement);

            const result = outputBuilder.getOutput();
            expect(result).toContain('",'); // Comma without space after
            expect(result).not.toContain(", "); // Should not have space after comma
        });

        it("should respect insertSpacesAfterCommas option in array formatting", () => {
            options.insertSpacesAfterCommas = true;
            options.maxLineLength = 15;
            options.alignArrayElements = true;
            lineBreaker = new SSLLineBreaker(options);

            const mockExprs = [createMockNumberLiteral("1"), createMockNumberLiteral("2")];
            const renderElement = (expr: ExpressionNode) => "1";

            lineBreaker.formatArrayLiteral(outputBuilder, mockExprs, renderElement);

            const result = outputBuilder.getOutput();
            expect(result).toContain(", "); // Space after comma
        });
    });
});

// Helper functions to create mock AST nodes for testing
function createMockStringLiteral(value: string): StringLiteralNode {
    const token = createToken(TokenType.STRING, `"${value}"`, createPosition(1, 1, 0));
    return {
        kind: ASTNodeType.StringLiteral,
        value: value,
        token: token,
        startToken: token,
        endToken: token,
    };
}

function createMockNumberLiteral(value: string): NumberLiteralNode {
    const token = createToken(TokenType.NUMBER, value, createPosition(1, 1, 0));
    return {
        kind: ASTNodeType.NumberLiteral,
        value: parseFloat(value),
        raw: value,
        startToken: token,
        endToken: token,
    };
}

function createMockVariableAccess(name: string): VariableAccessNode {
    const token = createToken(TokenType.IDENTIFIER, name, createPosition(1, 1, 0));
    return {
        kind: ASTNodeType.VariableAccess,
        startToken: token,
        endToken: token,
        name: token,
    } as any; // Type assertion needed due to incomplete AST definition
}
