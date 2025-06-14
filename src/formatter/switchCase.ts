/**
 * Switch Case Formatters
 *
 * - Format :BEGINCASE/:ENDCASE blocks
 * - Format :CASE statements
 * - Format :OTHERWISE blocks
 * - Format :EXITCASE statements
 */

import {
    FormatterVisitorBase,
    VisitorResult,
    FormatterOptions,
    defaultFormatterOptions,
} from "./visitor";
import {
    SwitchStatementNode,
    BeginCaseStatementNode,
    CaseBlockNode,
    CaseStatementNode,
    OtherwiseBlockNode,
    OtherwiseStatementNode,
    EndCaseStatementNode,
    ExitCaseStatementNode,
} from "../parser/ast/switchCase";
import { ExpressionNode, StatementNode, ASTNodeType } from "../parser/ast/base";
import { VariableAccessNode, LiteralExpressionNode, AssignmentNode } from "../parser/ast";
import { Token } from "../tokenizer/token";

// Re-export for convenience
export type { FormatterOptions } from "./visitor";
export { defaultFormatterOptions } from "./visitor";

/**
 * SSL Switch Case Formatter Visitor
 *
 * Formats SSL switch case constructs including:
 * - BEGINCASE/ENDCASE blocks with proper indentation
 * - CASE statements with expression formatting
 * - OTHERWISE blocks with proper nesting
 * - EXITCASE statements
 *
 * Follows SSL EBNF grammar:
 * SwitchStatement ::= BeginCaseStatement {CaseBlock} [OtherwiseBlock] EndCaseStatement
 * CaseBlock ::= CaseStatement {Statement} [ExitCaseStatement]
 * OtherwiseBlock ::= OtherwiseStatement {Statement}
 */
export class SSLSwitchCaseFormatterVisitor extends FormatterVisitorBase {
    constructor(options: FormatterOptions = defaultFormatterOptions) {
        super(options);
    }
    /**
     * Format switch statement block
     * Follows EBNF: SwitchStatement ::= BeginCaseStatement {CaseBlock} [OtherwiseBlock] EndCaseStatement
     */
    protected override visitSwitchStatement(node: SwitchStatementNode): VisitorResult {
        // Format :BEGINCASE
        this.output.writeIndented(":BEGINCASE;");
        this.output.writeLine();

        // Indent for case blocks
        this.output.indent();

        // Format all case blocks
        node.cases?.forEach((caseBlock: CaseBlockNode) => {
            this.visit(caseBlock);
        });

        // Format otherwise block if present
        if (node.otherwiseBlock) {
            this.visit(node.otherwiseBlock);
        }

        // Dedent and format :ENDCASE
        this.output.dedent();
        this.output.writeIndented(":ENDCASE;");
        this.output.writeLine();

        return { shouldContinue: false }; // We've handled all children manually
    }

    /**
     * Format BEGINCASE statement
     * Follows EBNF: BeginCaseStatement ::= ":" "BEGINCASE"
     */
    protected override visitBeginCaseStatement(node: BeginCaseStatementNode): VisitorResult {
        this.output.writeIndented(":BEGINCASE;");
        this.output.writeLine();
        return { shouldContinue: true };
    }
    /**
     * Format case block (case condition + body + optional exit)
     * Follows EBNF: CaseBlock ::= CaseStatement {Statement} [ExitCaseStatement]
     */
    protected override visitCaseBlock(node: CaseBlockNode): VisitorResult {
        // Format :CASE expression;
        this.output.writeIndented(":CASE ");
        this.formatExpression(node.condition);
        this.output.write(";");
        this.output.writeLine();

        // Indent for case body
        this.output.indent();

        // Format body statements
        this.formatStatementList(node.statements);

        // Dedent after case block
        this.output.dedent();

        return { shouldContinue: false }; // We've handled all children manually
    }

    /**
     * Format CASE statement (condition part)
     * Follows EBNF: CaseStatement ::= ":" "CASE" Expression
     */
    protected override visitCaseStatement(node: CaseStatementNode): VisitorResult {
        this.output.writeIndented(":CASE ");
        this.formatExpression(node.expression);
        this.output.write(";");
        this.output.writeLine();
        return { shouldContinue: true };
    }
    /**
     * Format OTHERWISE block
     * Follows EBNF: OtherwiseBlock ::= OtherwiseStatement {Statement}
     */
    protected override visitOtherwiseBlock(node: OtherwiseBlockNode): VisitorResult {
        // Format :OTHERWISE;
        this.output.writeIndented(":OTHERWISE;");
        this.output.writeLine();

        // Indent for otherwise body
        this.output.indent();

        // Format body statements
        this.formatStatementList(node.statements);

        // Dedent after otherwise block
        this.output.dedent();

        return { shouldContinue: false }; // We've handled all children manually
    }

    /**
     * Format OTHERWISE statement
     * Follows EBNF: OtherwiseStatement ::= ":" "OTHERWISE"
     */
    protected override visitOtherwiseStatement(node: OtherwiseStatementNode): VisitorResult {
        this.output.writeIndented(":OTHERWISE;");
        this.output.writeLine();
        return { shouldContinue: true };
    }

    /**
     * Format ENDCASE statement
     * Follows EBNF: EndCaseStatement ::= ":" "ENDCASE"
     */
    protected override visitEndCaseStatement(node: EndCaseStatementNode): VisitorResult {
        this.output.writeIndented(":ENDCASE;");
        this.output.writeLine();
        return { shouldContinue: true };
    }

    /**
     * Format EXITCASE statement
     * Follows EBNF: ExitCaseStatement ::= ":" "EXITCASE"
     */
    protected override visitExitCaseStatement(node: ExitCaseStatementNode): VisitorResult {
        this.output.writeIndented(":EXITCASE;");
        this.output.writeLine();
        return { shouldContinue: true };
    }
    /**
     * Format variable access expressions
     * Override to provide expression formatting within switch case context
     */
    protected override visitVariableAccess(node: VariableAccessNode): VisitorResult {
        const name = (node as any).name?.value || "";
        this.output.write(name);
        return { shouldContinue: false };
    }

    /**
     * Format literal expressions
     * Override to provide expression formatting within switch case context
     */
    protected override visitLiteralExpression(node: LiteralExpressionNode): VisitorResult {
        const token = (node as any).token;
        if (token) {
            this.output.write(token.value);
        } else {
            const value = (node as any).value;
            this.output.write(String(value || ""));
        }
        return { shouldContinue: false };
    }
    /**
     * Format assignment statements
     * Override to provide assignment formatting within switch case context
     */
    protected override visitAssignment(node: AssignmentNode): VisitorResult {
        // Format: left := right;
        // Note: Using any to access runtime AST properties that aren't in TypeScript definitions
        const anyNode = node as any;

        this.output.writeIndented("");

        // Format left-hand side (variable)
        if (anyNode.left) {
            this.visit(anyNode.left);
        }

        // Format assignment operator
        this.output.write(" := ");

        // Format right-hand side (expression)
        if (anyNode.right) {
            this.visit(anyNode.right);
        }

        // Add semicolon and newline
        this.output.write(";");
        this.output.writeLine();
        return { shouldContinue: false };
    }

    /**
     * Helper method to format a list of statements
     */
    private formatStatementList(statements: StatementNode[] | undefined): void {
        if (!statements) {
            return;
        }

        statements.forEach((statement, index) => {
            this.visit(statement);

            // Add spacing between statements if configured
            if (
                index < statements.length - 1 &&
                this.options.preserveBlankLines &&
                this.shouldAddBlankLineAfterStatement(statement)
            ) {
                this.output.writeBlankLine();
            }
        });
    }

    /**
     * Helper method to format expressions
     */
    private formatExpression(expression: ExpressionNode | undefined): void {
        if (!expression) {
            return;
        }
        this.visit(expression);
    }

    /**
     * Determine if a blank line should be added after a statement
     */ private shouldAddBlankLineAfterStatement(statement: StatementNode): boolean {
        // Add blank lines after complex statements for readability
        switch (statement.kind) {
            case ASTNodeType.SwitchStatement:
            case ASTNodeType.IfStatement:
            case ASTNodeType.WhileLoop:
            case ASTNodeType.ForLoop:
            case ASTNodeType.ProcedureStatement:
                return true;
            default:
                return false;
        }
    }
}
