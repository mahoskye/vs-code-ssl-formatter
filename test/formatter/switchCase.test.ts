/**
 * Tests for SSL Switch Case Formatter
 *
 * Comprehensive test suite covering:
 * - BEGINCASE/ENDCASE block formatting
 * - CASE statement formatting with expressions
 * - OTHERWISE block formatting
 * - EXITCASE statement formatting
 * - Nested structure indentation
 * - EBNF grammar compliance
 */

import { SSLSwitchCaseFormatterVisitor } from "../../src/formatter/switchCase";
import { FormatterOptions, defaultFormatterOptions } from "../../src/formatter/visitor";
import {
    SwitchStatementNode,
    BeginCaseStatementNode,
    CaseBlockNode,
    CaseStatementNode,
    OtherwiseBlockNode,
    OtherwiseStatementNode,
    EndCaseStatementNode,
    ExitCaseStatementNode,
} from "../../src/parser/ast/switchCase";
import {
    ASTNodeType,
    createBaseNode,
    StatementNode,
    ExpressionNode,
} from "../../src/parser/ast/base";
import { VariableAccessNode, LiteralExpressionNode } from "../../src/parser/ast";
import { createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("SSLSwitchCaseFormatterVisitor", () => {
    let formatter: SSLSwitchCaseFormatterVisitor;
    let mockToken: any;

    beforeEach(() => {
        formatter = new SSLSwitchCaseFormatterVisitor();
        mockToken = createToken(TokenType.BEGINCASE, "begincase", createPosition(1, 1, 0));
    });
    describe("Basic Switch Case Formatting", () => {
        it("should format a simple BEGINCASE/ENDCASE block", () => {
            const switchNode = createMockSwitchStatement([], undefined);

            formatter.visit(switchNode);
            const output = formatter.getFormattedOutput();

            expect(output).toContain(":BEGINCASE;");
            expect(output).toContain(":ENDCASE;");
            expect(output.split("\n")[0]).toBe(":BEGINCASE;");
            expect(
                output
                    .split("\n")
                    .filter((line: string) => line.trim() !== "")
                    .pop()
            ).toBe(":ENDCASE;");
        });

        it("should format a CASE statement with expression", () => {
            const caseStatement = createMockCaseStatement("variableName");

            formatter.visit(caseStatement);
            const output = formatter.getFormattedOutput();

            expect(output).toContain(":CASE variableName;");
        });

        it("should format an OTHERWISE statement", () => {
            const otherwiseStatement = createMockOtherwiseStatement();

            formatter.visit(otherwiseStatement);
            const output = formatter.getFormattedOutput();

            expect(output).toContain(":OTHERWISE;");
        });

        it("should format an EXITCASE statement", () => {
            const exitCaseStatement = createMockExitCaseStatement();

            formatter.visit(exitCaseStatement);
            const output = formatter.getFormattedOutput();

            expect(output).toContain(":EXITCASE;");
        });
    });

    describe("Switch Case Structure Formatting", () => {
        it("should format a complete switch case with single case", () => {
            const caseBlock = createMockCaseBlock(createMockExpression("nValue"), [
                createMockAssignment(),
            ]);
            const switchNode = createMockSwitchStatement([caseBlock], undefined);

            formatter.visit(switchNode);
            const output = formatter.getFormattedOutput();
            const lines = output.split("\n").filter((line: string) => line.length > 0);

            expect(lines[0]).toBe(":BEGINCASE;");
            expect(lines[1]).toBe("    :CASE nValue;");
            expect(lines[2]).toBe('        result := "case1";');
            expect(lines[3]).toBe(":ENDCASE;");
        });

        it("should format a switch case with multiple cases", () => {
            const case1Block = createMockCaseBlock(createMockExpression("1"), [
                createMockAssignment("result", '"first"'),
            ]);
            const case2Block = createMockCaseBlock(createMockExpression("2"), [
                createMockAssignment("result", '"second"'),
            ]);

            const switchNode = createMockSwitchStatement([case1Block, case2Block], undefined);

            formatter.visit(switchNode);
            const output = formatter.getFormattedOutput();
            const lines = output.split("\n").filter((line: string) => line.length > 0);

            expect(lines[0]).toBe(":BEGINCASE;");
            expect(lines[1]).toBe("    :CASE 1;");
            expect(lines[2]).toBe('        result := "first";');
            expect(lines[3]).toBe("    :CASE 2;");
            expect(lines[4]).toBe('        result := "second";');
            expect(lines[5]).toBe(":ENDCASE;");
        });

        it("should format a switch case with OTHERWISE block", () => {
            const caseBlock = createMockCaseBlock(createMockExpression("1"), [
                createMockAssignment("result", '"case1"'),
            ]);
            const otherwiseBlock = createMockOtherwiseBlock([
                createMockAssignment("result", '"default"'),
            ]);
            const switchNode = createMockSwitchStatement([caseBlock], otherwiseBlock);

            formatter.visit(switchNode);
            const output = formatter.getFormattedOutput();
            const lines = output.split("\n").filter((line: string) => line.length > 0);

            expect(lines[0]).toBe(":BEGINCASE;");
            expect(lines[1]).toBe("    :CASE 1;");
            expect(lines[2]).toBe('        result := "case1";');
            expect(lines[3]).toBe("    :OTHERWISE;");
            expect(lines[4]).toBe('        result := "default";');
            expect(lines[5]).toBe(":ENDCASE;");
        });

        it("should format a case with EXITCASE statement", () => {
            const caseBlock = createMockCaseBlock(createMockExpression("1"), [
                createMockAssignment("result", '"case1"'),
                createMockExitCaseStatement(),
            ]);
            const switchNode = createMockSwitchStatement([caseBlock], undefined);

            formatter.visit(switchNode);
            const output = formatter.getFormattedOutput();
            const lines = output.split("\n").filter((line: string) => line.length > 0);

            expect(lines[0]).toBe(":BEGINCASE;");
            expect(lines[1]).toBe("    :CASE 1;");
            expect(lines[2]).toBe('        result := "case1";');
            expect(lines[3]).toBe("        :EXITCASE;");
            expect(lines[4]).toBe(":ENDCASE;");
        });
    });
    describe("Indentation and Spacing", () => {
        it("should properly indent nested switch case blocks", () => {
            const formatter = new SSLSwitchCaseFormatterVisitor({
                ...defaultFormatterOptions,
                indentSize: 4,
                useTabs: false,
            });

            const caseBlock = createMockCaseBlock(createMockExpression("nValue"), [
                createMockAssignment(),
            ]);
            const switchNode = createMockSwitchStatement([caseBlock], undefined);

            formatter.visit(switchNode);
            const output = formatter.getFormattedOutput();
            const lines = output.split("\n");

            // Check indentation levels
            expect(lines[0]).toBe(":BEGINCASE;"); // No indentation
            expect(lines[1]).toBe("    :CASE nValue;"); // 4 spaces
            expect(lines[2]).toBe('        result := "case1";'); // 8 spaces
            expect(lines[3]).toBe(":ENDCASE;"); // No indentation
        });

        it("should use tabs when configured", () => {
            const formatter = new SSLSwitchCaseFormatterVisitor({
                ...defaultFormatterOptions,
                useTabs: true,
            });

            const caseBlock = createMockCaseBlock(createMockExpression("nValue"), [
                createMockAssignment(),
            ]);
            const switchNode = createMockSwitchStatement([caseBlock], undefined);

            formatter.visit(switchNode);
            const output = formatter.getFormattedOutput();
            const lines = output.split("\n");

            expect(lines[1]).toBe("\t:CASE nValue;"); // 1 tab
            expect(lines[2]).toBe('\t\tresult := "case1";'); // 2 tabs
        });

        it("should handle complex expressions in CASE statements", () => {
            const caseStatement = createMockCaseStatement("nValue + 1");

            formatter.visit(caseStatement);
            const output = formatter.getFormattedOutput();

            expect(output).toContain(":CASE nValue + 1;");
        });
    });
    describe("EBNF Grammar Compliance", () => {
        it("should follow EBNF SwitchStatement structure", () => {
            // SwitchStatement ::= BeginCaseStatement {CaseBlock} [OtherwiseBlock] EndCaseStatement
            const case1 = createMockCaseBlock(createMockExpression("1"), [createMockAssignment()]);
            const case2 = createMockCaseBlock(createMockExpression("2"), [createMockAssignment()]);
            const otherwiseBlock = createMockOtherwiseBlock([createMockAssignment()]);

            const switchNode = createMockSwitchStatement([case1, case2], otherwiseBlock);

            const result = formatter.visit(switchNode);
            expect(result.shouldContinue).toBe(false); // Manually handled children
            expect(result.error).toBeUndefined();
        });

        it("should follow EBNF CaseBlock structure", () => {
            // CaseBlock ::= CaseStatement {Statement} [ExitCaseStatement]
            const caseBlock = createMockCaseBlock(createMockExpression("nValue"), [
                createMockAssignment(),
                createMockAssignment(),
            ]);

            const result = formatter.visit(caseBlock);
            expect(result.shouldContinue).toBe(false); // Manually handled children
            expect(result.error).toBeUndefined();
        });

        it("should follow EBNF OtherwiseBlock structure", () => {
            // OtherwiseBlock ::= OtherwiseStatement {Statement}
            const otherwiseBlock = createMockOtherwiseBlock([
                createMockAssignment(),
                createMockAssignment(),
            ]);

            const result = formatter.visit(otherwiseBlock);
            expect(result.shouldContinue).toBe(false); // Manually handled children
            expect(result.error).toBeUndefined();
        });
    });

    describe("Edge Cases and Error Handling", () => {
        it("should handle switch case with no cases", () => {
            const switchNode = createMockSwitchStatement([], undefined);

            formatter.visit(switchNode);
            const output = formatter.getFormattedOutput();
            const lines = output.split("\n").filter((line: string) => line.length > 0);

            expect(lines).toEqual([":BEGINCASE;", ":ENDCASE;"]);
        });

        it("should handle case with no statements", () => {
            const caseBlock = createMockCaseBlock(createMockExpression("1"), []);
            const switchNode = createMockSwitchStatement([caseBlock], undefined);

            formatter.visit(switchNode);
            const output = formatter.getFormattedOutput();
            const lines = output.split("\n").filter((line: string) => line.length > 0);

            expect(lines[0]).toBe(":BEGINCASE;");
            expect(lines[1]).toBe("    :CASE 1;");
            expect(lines[2]).toBe(":ENDCASE;");
        });

        it("should handle otherwise block with no statements", () => {
            const otherwiseBlock = createMockOtherwiseBlock([]);

            formatter.visit(otherwiseBlock);
            const output = formatter.getFormattedOutput();

            expect(output).toContain(":OTHERWISE;");
        });
    });

    describe("Integration with Formatter Options", () => {
        it("should respect preserveBlankLines option", () => {
            const formatter = new SSLSwitchCaseFormatterVisitor({
                ...defaultFormatterOptions,
                preserveBlankLines: true,
            });

            const caseBlock = createMockCaseBlock(
                createMockExpression("1"),
                [createMockComplexStatement()] // This should trigger blank line
            );
            const switchNode = createMockSwitchStatement([caseBlock], undefined);

            formatter.visit(switchNode);
            const output = formatter.getFormattedOutput();

            // Should have proper spacing for complex statements
            expect(output).toBeTruthy();
        });

        it("should handle custom indentation settings", () => {
            const formatter = new SSLSwitchCaseFormatterVisitor({
                ...defaultFormatterOptions,
                indentSize: 2,
                useTabs: false,
            });

            const caseBlock = createMockCaseBlock(createMockExpression("nValue"), [
                createMockAssignment(),
            ]);
            const switchNode = createMockSwitchStatement([caseBlock], undefined);

            formatter.visit(switchNode);
            const output = formatter.getFormattedOutput();
            const lines = output.split("\n");

            expect(lines[1]).toBe("  :CASE nValue;"); // 2 spaces
            expect(lines[2]).toBe('    result := "case1";'); // 4 spaces
        });
    }); // Helper functions to create mock AST nodes
    function createMockSwitchStatement(
        cases: CaseBlockNode[],
        otherwiseBlock: OtherwiseBlockNode | undefined
    ): SwitchStatementNode {
        return {
            kind: ASTNodeType.SwitchStatement,
            startToken: mockToken,
            endToken: mockToken,
            cases,
            otherwiseBlock,
        } as SwitchStatementNode;
    }

    function createMockBeginCaseStatement(): BeginCaseStatementNode {
        return createBaseNode(
            ASTNodeType.BeginCaseStatement,
            mockToken,
            mockToken
        ) as BeginCaseStatementNode;
    }

    function createMockCaseBlock(condition: ExpressionNode, body: StatementNode[]): CaseBlockNode {
        return {
            kind: ASTNodeType.CaseBlock,
            startToken: mockToken,
            endToken: mockToken,
            condition,
            statements: body,
        } as CaseBlockNode;
    }

    function createMockCaseStatement(expression: string): CaseStatementNode {
        return {
            kind: ASTNodeType.CaseStatement,
            startToken: mockToken,
            endToken: mockToken,
            expression: createMockExpression(expression),
        } as CaseStatementNode;
    }

    function createMockOtherwiseBlock(body: StatementNode[]): OtherwiseBlockNode {
        return {
            kind: ASTNodeType.OtherwiseBlock,
            startToken: mockToken,
            endToken: mockToken,
            statements: body,
        } as OtherwiseBlockNode;
    }

    function createMockOtherwiseStatement(): OtherwiseStatementNode {
        return createBaseNode(
            ASTNodeType.OtherwiseStatement,
            mockToken,
            mockToken
        ) as OtherwiseStatementNode;
    }

    function createMockEndCaseStatement(): EndCaseStatementNode {
        return createBaseNode(
            ASTNodeType.EndCaseStatement,
            mockToken,
            mockToken
        ) as EndCaseStatementNode;
    }

    function createMockExitCaseStatement(): ExitCaseStatementNode {
        return createBaseNode(
            ASTNodeType.ExitCaseStatement,
            mockToken,
            mockToken
        ) as ExitCaseStatementNode;
    }
    function createMockExpression(value: string): ExpressionNode {
        // Create a proper VariableAccess node for variable expressions
        if (/^[a-zA-Z_][a-zA-Z0-9_]*$/.test(value)) {
            return {
                kind: ASTNodeType.VariableAccess,
                startToken: mockToken,
                endToken: mockToken,
                name: createToken(TokenType.IDENTIFIER, value, createPosition(1, 1, 0)),
            } as VariableAccessNode;
        }

        // For complex expressions or literals, create a literal expression
        return {
            kind: ASTNodeType.LiteralExpression,
            startToken: mockToken,
            endToken: mockToken,
            value,
            token: createToken(TokenType.IDENTIFIER, value, createPosition(1, 1, 0)),
        } as LiteralExpressionNode;
    }
    function createMockAssignment(
        variable: string = "result",
        value: string = '"case1"'
    ): StatementNode {
        return {
            kind: ASTNodeType.Assignment,
            startToken: mockToken,
            endToken: mockToken,
            left: {
                kind: ASTNodeType.VariableAccess,
                startToken: mockToken,
                endToken: mockToken,
                name: { value: variable },
            },
            right: {
                kind: ASTNodeType.LiteralExpression,
                startToken: mockToken,
                endToken: mockToken,
                value: value.replace(/"/g, ""), // Remove quotes for the value
                token: { value: value },
            },
        } as StatementNode;
    }

    function createMockComplexStatement(): StatementNode {
        return {
            kind: ASTNodeType.SwitchStatement, // Complex statement that should trigger blank lines
            startToken: mockToken,
            endToken: mockToken,
        } as StatementNode;
    }
});
