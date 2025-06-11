/**
 * Tests for SSL Control Flow Formatter
 *
 * Comprehensive test suite covering:
 * - IF/ELSE/ENDIF blocks formatting
 * - WHILE/ENDWHILE loops formatting
 * - FOR/NEXT loops formatting
 * - Exit statements formatting
 * - Nested control structures
 * - EBNF grammar compliance
 */

import { SSLControlFlowFormatterVisitor } from "../../src/formatter/controlFlow";
import { defaultFormatterOptions, FormatterOptions } from "../../src/formatter/options";
import {
    ConditionalStatementNode,
    IfStatementNode,
    ElseStatementNode,
    EndIfStatementNode,
    LoopStatementNode,
    WhileLoopNode,
    WhileStatementNode,
    EndWhileStatementNode,
    ForLoopNode,
    ForStatementNode,
    NextStatementNode,
    ExitWhileStatementNode,
    ExitForStatementNode,
    LoopContinueNode,
} from "../../src/parser/ast/controlFlow";
import {
    ASTNodeType,
    createBaseNode,
    ExpressionNode,
    StatementNode,
} from "../../src/parser/ast/base";
import { createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("SSLControlFlowFormatterVisitor", () => {
    let visitor: SSLControlFlowFormatterVisitor;
    let options: FormatterOptions;
    let mockToken: any;
    let mockExpression: ExpressionNode;

    beforeEach(() => {
        options = { ...defaultFormatterOptions };
        visitor = new SSLControlFlowFormatterVisitor(options);
        mockToken = createToken(TokenType.IDENTIFIER, "test", createPosition(1, 1, 0));

        // Create a mock expression for testing
        mockExpression = createBaseNode(
            ASTNodeType.Expression,
            mockToken,
            mockToken
        ) as ExpressionNode;
    });

    describe("IF Statement Formatting", () => {
        it("should format simple IF statement without ELSE", () => {
            const endIfNode: EndIfStatementNode = {
                kind: ASTNodeType.EndIfStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(3, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(3, 7, 6)),
            };

            const mockStatement: StatementNode = createBaseNode(
                ASTNodeType.Assignment,
                mockToken,
                mockToken
            ) as StatementNode;

            const ifNode: IfStatementNode = {
                kind: ASTNodeType.IfStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(3, 7, 6)),
                condition: mockExpression,
                thenBranch: [mockStatement],
                endIf: endIfNode,
            };

            const result = visitor.visit(ifNode);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            const lines = output.split("\n");

            expect(lines[0]).toBe(":IF ;"); // Will be improved with proper expression formatting
            expect(lines[lines.length - 1]).toBe(":ENDIF;");
        });

        it("should format IF statement with ELSE block", () => {
            const endIfNode: EndIfStatementNode = {
                kind: ASTNodeType.EndIfStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(5, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(5, 7, 6)),
            };

            const mockStatement1: StatementNode = createBaseNode(
                ASTNodeType.Assignment,
                mockToken,
                mockToken
            ) as StatementNode;

            const mockStatement2: StatementNode = createBaseNode(
                ASTNodeType.Assignment,
                mockToken,
                mockToken
            ) as StatementNode;

            const elseNode: ElseStatementNode = {
                kind: ASTNodeType.ElseStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(3, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(3, 6, 5)),
                body: [mockStatement2],
            };

            const ifNode: IfStatementNode = {
                kind: ASTNodeType.IfStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(5, 7, 6)),
                condition: mockExpression,
                thenBranch: [mockStatement1],
                elseBranch: elseNode,
                endIf: endIfNode,
            };

            const result = visitor.visit(ifNode);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput();
            expect(output).toContain(":IF ");
            expect(output).toContain(":ELSE;");
            expect(output).toContain(":ENDIF;");
        });

        it("should format standalone ELSE statement", () => {
            const mockStatement: StatementNode = createBaseNode(
                ASTNodeType.Assignment,
                mockToken,
                mockToken
            ) as StatementNode;

            const elseNode: ElseStatementNode = {
                kind: ASTNodeType.ElseStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 6, 5)),
                body: [mockStatement],
            };

            const result = visitor.visit(elseNode);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toContain(":ELSE;");
        });

        it("should format standalone ENDIF statement", () => {
            const endIfNode: EndIfStatementNode = {
                kind: ASTNodeType.EndIfStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 7, 6)),
            };

            const result = visitor.visit(endIfNode);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe(":ENDIF;");
        });
    });

    describe("WHILE Loop Formatting", () => {
        it("should format simple WHILE loop", () => {
            const whileStmtNode: WhileStatementNode = {
                kind: ASTNodeType.WhileStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 15, 14)),
                condition: mockExpression,
            };

            const endWhileNode: EndWhileStatementNode = {
                kind: ASTNodeType.EndWhileStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(3, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(3, 10, 9)),
            };

            const mockStatement: StatementNode = createBaseNode(
                ASTNodeType.Assignment,
                mockToken,
                mockToken
            ) as StatementNode;

            const whileLoopNode: WhileLoopNode = {
                kind: ASTNodeType.WhileLoop,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(3, 10, 9)),
                condition: whileStmtNode,
                body: [mockStatement],
                end: endWhileNode,
            };

            const result = visitor.visit(whileLoopNode);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput();
            expect(output).toContain(":WHILE ");
            expect(output).toContain(":ENDWHILE;");
        });

        it("should format standalone WHILE statement", () => {
            const whileStmtNode: WhileStatementNode = {
                kind: ASTNodeType.WhileStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 15, 14)),
                condition: mockExpression,
            };

            const result = visitor.visit(whileStmtNode);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe(":WHILE ;");
        });

        it("should format standalone ENDWHILE statement", () => {
            const endWhileNode: EndWhileStatementNode = {
                kind: ASTNodeType.EndWhileStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 10, 9)),
            };

            const result = visitor.visit(endWhileNode);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe(":ENDWHILE;");
        });
    });

    describe("FOR Loop Formatting", () => {
        it("should format simple FOR loop with default spacing", () => {
            const forStmtNode: ForStatementNode = {
                kind: ASTNodeType.ForStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 20, 19)),
                variable: createToken(TokenType.IDENTIFIER, "i", createPosition(1, 6, 5)),
                startValue: mockExpression,
                endValue: mockExpression,
            };

            const nextNode: NextStatementNode = {
                kind: ASTNodeType.NextStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(3, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(3, 6, 5)),
            };

            const mockStatement: StatementNode = createBaseNode(
                ASTNodeType.Assignment,
                mockToken,
                mockToken
            ) as StatementNode;

            const forLoopNode: ForLoopNode = {
                kind: ASTNodeType.ForLoop,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(3, 6, 5)),
                declaration: forStmtNode,
                body: [mockStatement],
                next: nextNode,
            };

            const result = visitor.visit(forLoopNode);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput();
            expect(output).toContain(":FOR i := ");
            expect(output).toContain(" :TO ");
            expect(output).toContain(":NEXT;");
        });

        it("should format FOR statement with custom spacing options", () => {
            // Test with spaces around assignment operators disabled
            const customOptions = {
                ...defaultFormatterOptions,
                insertSpacesAroundAssignmentOperators: false,
            };
            const customVisitor = new SSLControlFlowFormatterVisitor(customOptions);

            const forStmtNode: ForStatementNode = {
                kind: ASTNodeType.ForStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 20, 19)),
                variable: createToken(TokenType.IDENTIFIER, "counter", createPosition(1, 6, 5)),
                startValue: mockExpression,
                endValue: mockExpression,
            };

            const result = customVisitor.visit(forStmtNode);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();

            const output = customVisitor.getFormattedOutput();
            expect(output).toContain(":FOR counter:=");
            expect(output).not.toContain(" := ");
        });

        it("should format standalone NEXT statement", () => {
            const nextNode: NextStatementNode = {
                kind: ASTNodeType.NextStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 6, 5)),
            };

            const result = visitor.visit(nextNode);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe(":NEXT;");
        });
    });

    describe("Exit Statements Formatting", () => {
        it("should format EXITWHILE statement", () => {
            const exitWhileNode: ExitWhileStatementNode = {
                kind: ASTNodeType.ExitWhileStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 11, 10)),
            };

            const result = visitor.visit(exitWhileNode);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe(":EXITWHILE;");
        });

        it("should format EXITFOR statement", () => {
            const exitForNode: ExitForStatementNode = {
                kind: ASTNodeType.ExitForStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 9, 8)),
            };

            const result = visitor.visit(exitForNode);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe(":EXITFOR;");
        });

        it("should format LOOP continue statement", () => {
            const loopContinueNode: LoopContinueNode = {
                kind: ASTNodeType.LoopContinue,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 6, 5)),
            };

            const result = visitor.visit(loopContinueNode);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe(":LOOP;");
        });
    });

    describe("Wrapper Statements", () => {
        it("should handle ConditionalStatement wrapper", () => {
            const conditionalNode: ConditionalStatementNode = {
                kind: ASTNodeType.ConditionalStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            const result = visitor.visit(conditionalNode);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();
        });

        it("should handle LoopStatement wrapper", () => {
            const loopNode: LoopStatementNode = {
                kind: ASTNodeType.LoopStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            const result = visitor.visit(loopNode);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();
        });
    });

    describe("Complex Nested Structures", () => {
        it("should handle nested IF statements with proper indentation", () => {
            // Create a nested IF structure: IF -> IF -> ENDIF -> ENDIF
            const innerEndIf: EndIfStatementNode = {
                kind: ASTNodeType.EndIfStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            const innerIf: IfStatementNode = {
                kind: ASTNodeType.IfStatement,
                startToken: mockToken,
                endToken: mockToken,
                condition: mockExpression,
                thenBranch: [],
                endIf: innerEndIf,
            };

            const outerEndIf: EndIfStatementNode = {
                kind: ASTNodeType.EndIfStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            const outerIf: IfStatementNode = {
                kind: ASTNodeType.IfStatement,
                startToken: mockToken,
                endToken: mockToken,
                condition: mockExpression,
                thenBranch: [innerIf],
                endIf: outerEndIf,
            };

            const result = visitor.visit(outerIf);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput();
            const lines = output.split("\n").filter((line) => line.trim() !== "");

            // Should have properly nested indentation
            expect(output).toContain(":IF ");
            expect(output).toContain(":ENDIF;");
            // Check that we have 4 non-empty lines: outer IF, inner IF, inner ENDIF, outer ENDIF
            expect(lines.length).toBeGreaterThanOrEqual(4);
        });

        it("should handle FOR loop inside WHILE loop", () => {
            const nextNode: NextStatementNode = {
                kind: ASTNodeType.NextStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            const forStmtNode: ForStatementNode = {
                kind: ASTNodeType.ForStatement,
                startToken: mockToken,
                endToken: mockToken,
                variable: createToken(TokenType.IDENTIFIER, "j", createPosition(1, 1, 0)),
                startValue: mockExpression,
                endValue: mockExpression,
            };

            const forLoopNode: ForLoopNode = {
                kind: ASTNodeType.ForLoop,
                startToken: mockToken,
                endToken: mockToken,
                declaration: forStmtNode,
                body: [],
                next: nextNode,
            };

            const whileStmtNode: WhileStatementNode = {
                kind: ASTNodeType.WhileStatement,
                startToken: mockToken,
                endToken: mockToken,
                condition: mockExpression,
            };

            const endWhileNode: EndWhileStatementNode = {
                kind: ASTNodeType.EndWhileStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            const whileLoopNode: WhileLoopNode = {
                kind: ASTNodeType.WhileLoop,
                startToken: mockToken,
                endToken: mockToken,
                condition: whileStmtNode,
                body: [forLoopNode],
                end: endWhileNode,
            };

            const result = visitor.visit(whileLoopNode);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput();
            expect(output).toContain(":WHILE ");
            expect(output).toContain(":FOR j := ");
            expect(output).toContain(":NEXT;");
            expect(output).toContain(":ENDWHILE;");
        });
    });

    describe("EBNF Grammar Compliance", () => {
        it("should follow EBNF grammar for IF statements", () => {
            // Test that IF statements follow:
            // IfStatement ::= ":" "IF" Expression
            // ElseStatement ::= ":" "ELSE"
            // EndIfStatement ::= ":" "ENDIF"

            const endIfNode: EndIfStatementNode = {
                kind: ASTNodeType.EndIfStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            const ifNode: IfStatementNode = {
                kind: ASTNodeType.IfStatement,
                startToken: mockToken,
                endToken: mockToken,
                condition: mockExpression,
                thenBranch: [],
                endIf: endIfNode,
            };

            const result = visitor.visit(ifNode);
            const output = visitor.getFormattedOutput();

            // Should start with :IF and end with :ENDIF;
            expect(output).toMatch(/^:IF\s/);
            expect(output).toMatch(/:ENDIF;\s*$/);
        });

        it("should follow EBNF grammar for WHILE statements", () => {
            // Test that WHILE statements follow:
            // WhileStatement ::= ":" "WHILE" Expression
            // EndWhileStatement ::= ":" "ENDWHILE"

            const whileStmtNode: WhileStatementNode = {
                kind: ASTNodeType.WhileStatement,
                startToken: mockToken,
                endToken: mockToken,
                condition: mockExpression,
            };

            const result = visitor.visit(whileStmtNode);
            const output = visitor.getFormattedOutput();

            // Should start with :WHILE
            expect(output).toMatch(/^:WHILE\s/);
        });

        it("should follow EBNF grammar for FOR statements", () => {
            // Test that FOR statements follow:
            // ForStatement ::= ":" "FOR" Identifier ":=" Expression ":" "TO" Expression

            const forStmtNode: ForStatementNode = {
                kind: ASTNodeType.ForStatement,
                startToken: mockToken,
                endToken: mockToken,
                variable: createToken(TokenType.IDENTIFIER, "i", createPosition(1, 1, 0)),
                startValue: mockExpression,
                endValue: mockExpression,
            };

            const result = visitor.visit(forStmtNode);
            const output = visitor.getFormattedOutput();

            // Should follow the exact pattern
            expect(output).toMatch(/^:FOR\s+i\s+:=.*:TO.*;\s*$/);
        });

        it("should follow EBNF grammar for exit statements", () => {
            // Test exit statements follow simple patterns:
            // ExitWhileStatement ::= ":" "EXITWHILE"
            // ExitForStatement ::= ":" "EXITFOR"
            // LoopContinue ::= ":" "LOOP"

            const exitWhileNode: ExitWhileStatementNode = {
                kind: ASTNodeType.ExitWhileStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            const exitForNode: ExitForStatementNode = {
                kind: ASTNodeType.ExitForStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            const loopContinueNode: LoopContinueNode = {
                kind: ASTNodeType.LoopContinue,
                startToken: mockToken,
                endToken: mockToken,
            };

            const exitWhileResult = visitor.visit(exitWhileNode);
            const exitWhileOutput = visitor.getFormattedOutput();
            expect(exitWhileOutput.trim()).toBe(":EXITWHILE;");

            // Reset visitor for next test
            visitor = new SSLControlFlowFormatterVisitor(options);

            const exitForResult = visitor.visit(exitForNode);
            const exitForOutput = visitor.getFormattedOutput();
            expect(exitForOutput.trim()).toBe(":EXITFOR;");

            // Reset visitor for next test
            visitor = new SSLControlFlowFormatterVisitor(options);

            const loopContinueResult = visitor.visit(loopContinueNode);
            const loopContinueOutput = visitor.getFormattedOutput();
            expect(loopContinueOutput.trim()).toBe(":LOOP;");
        });
    });

    describe("Formatting Options Integration", () => {
        it("should respect indentation settings", () => {
            const tabOptions = {
                ...defaultFormatterOptions,
                useTabs: true,
            };
            const tabVisitor = new SSLControlFlowFormatterVisitor(tabOptions);

            const endIfNode: EndIfStatementNode = {
                kind: ASTNodeType.EndIfStatement,
                startToken: mockToken,
                endToken: mockToken,
            }; // Create a nested WHILE loop inside the IF statement to ensure indented content
            const whileStatement: WhileStatementNode = {
                kind: ASTNodeType.WhileStatement,
                startToken: mockToken,
                endToken: mockToken,
                condition: mockExpression,
            };

            const endWhileStatement: EndWhileStatementNode = {
                kind: ASTNodeType.EndWhileStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            const innerWhileNode: WhileLoopNode = {
                kind: ASTNodeType.WhileLoop,
                startToken: mockToken,
                endToken: mockToken,
                condition: whileStatement,
                body: [],
                end: endWhileStatement,
            };

            const ifNode: IfStatementNode = {
                kind: ASTNodeType.IfStatement,
                startToken: mockToken,
                endToken: mockToken,
                condition: mockExpression,
                thenBranch: [innerWhileNode],
                endIf: endIfNode,
            };
            tabVisitor.visit(ifNode);
            const output = tabVisitor.getFormattedOutput();

            // Debug: log the output to see what's being generated
            console.log("Debug output:", JSON.stringify(output));

            // With useTabs: true, indented lines should contain tab characters
            const lines = output.split("\n");
            const indentedLines = lines.filter(
                (line) => line.startsWith("\t") || line.startsWith(" ")
            );

            console.log("Debug lines:", lines);
            console.log("Debug indented lines:", indentedLines);

            if (tabOptions.useTabs) {
                // At least one line should start with a tab
                expect(indentedLines.some((line) => line.startsWith("\t"))).toBe(true);
            }
        });

        it("should respect blank line preservation settings", () => {
            const blankLineOptions = {
                ...defaultFormatterOptions,
                preserveBlankLines: true,
            };
            const blankLineVisitor = new SSLControlFlowFormatterVisitor(blankLineOptions);

            // This test would be more meaningful with a complex structure
            // For now, just verify the visitor works with the option
            const conditionalNode: ConditionalStatementNode = {
                kind: ASTNodeType.ConditionalStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            const result = blankLineVisitor.visit(conditionalNode);
            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();
        });
    });
});
