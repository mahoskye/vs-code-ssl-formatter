/**
 * Error Handling Formatters
 *
 * - Format :TRY/:CATCH/:FINALLY/:ENDTRY blocks
 * - Format :ERROR blocks
 * - Proper indentation for error handling structures
 */

import {
    FormatterVisitorBase,
    VisitorResult,
    FormatterOptions,
    defaultFormatterOptions,
} from "./visitor";
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
} from "../parser/ast/errorHandling";
import { StatementNode } from "../parser/ast/base";

/**
 * SSL Error Handling Formatter Visitor
 *
 * Formats SSL error handling constructs including:
 * - TRY/CATCH/FINALLY/ENDTRY blocks with proper indentation
 * - ERROR blocks with proper statement nesting
 * - Proper indentation for nested error handling structures
 */
export class SSLErrorHandlingFormatterVisitor extends FormatterVisitorBase {
    constructor(options: FormatterOptions = defaultFormatterOptions) {
        super(options);
    }

    /**
     * Format error handling statement wrapper
     * This acts as a dispatcher for different error handling types
     */
    protected override visitErrorHandlingStatement(
        node: ErrorHandlingStatementNode
    ): VisitorResult {
        // Error handling statements are wrappers - continue to visit children
        return { shouldContinue: true };
    }
    /**
     * Format TRY block structure
     * Follows EBNF: TryBlock ::= TryStatement {Statement} CatchBlock [FinallyBlock] EndTryStatement
     */
    protected override visitTryBlock(node: TryBlockNode): VisitorResult {
        // Format :TRY; statement
        this.output.writeIndented(":TRY;");
        this.output.writeLine();

        // Indent for try body
        this.output.indent();

        // Format try body statements
        this.formatStatementList(node.tryStatements);

        // Dedent before catch/finally
        this.output.dedent();

        // Format catch block if present
        if (node.catchBlock) {
            this.visit(node.catchBlock);
        }

        // Format finally block if present
        if (node.finallyBlock) {
            this.visit(node.finallyBlock);
        }

        // Format ENDTRY
        this.output.writeIndented(":ENDTRY;");
        this.output.writeLine();

        return { shouldContinue: false }; // We've handled all children manually
    }

    /**
     * Format TRY statement
     * Follows EBNF: TryStatement ::= ":" "TRY"
     */
    protected override visitTryStatement(node: TryStatementNode): VisitorResult {
        this.output.writeIndented(":TRY;");
        this.output.writeLine();
        return { shouldContinue: true };
    }
    /**
     * Format CATCH block structure
     * Follows EBNF: CatchBlock ::= CatchStatement {Statement}
     */
    protected override visitCatchBlock(node: CatchBlockNode): VisitorResult {
        // Format :CATCH; statement
        this.output.writeIndented(":CATCH;");
        this.output.writeLine();

        // Indent for catch body
        this.output.indent();

        // Format catch body statements
        this.formatStatementList(node.statements);

        // Dedent after catch body
        this.output.dedent();

        return { shouldContinue: false }; // We've handled all children manually
    }

    /**
     * Format CATCH statement
     * Follows EBNF: CatchStatement ::= ":" "CATCH"
     */
    protected override visitCatchStatement(node: CatchStatementNode): VisitorResult {
        this.output.writeIndented(":CATCH;");
        this.output.writeLine();
        return { shouldContinue: true };
    }
    /**
     * Format FINALLY block structure
     * Follows EBNF: FinallyBlock ::= FinallyStatement {Statement}
     */
    protected override visitFinallyBlock(node: FinallyBlockNode): VisitorResult {
        // Format :FINALLY; statement
        this.output.writeIndented(":FINALLY;");
        this.output.writeLine();

        // Indent for finally body
        this.output.indent();

        // Format finally body statements
        this.formatStatementList(node.statements);

        // Dedent after finally body
        this.output.dedent();

        return { shouldContinue: false }; // We've handled all children manually
    }

    /**
     * Format FINALLY statement
     * Follows EBNF: FinallyStatement ::= ":" "FINALLY"
     */
    protected override visitFinallyStatement(node: FinallyStatementNode): VisitorResult {
        this.output.writeIndented(":FINALLY;");
        this.output.writeLine();
        return { shouldContinue: true };
    }

    /**
     * Format ENDTRY statement
     * Follows EBNF: EndTryStatement ::= ":" "ENDTRY"
     */
    protected override visitEndTryStatement(node: EndTryStatementNode): VisitorResult {
        this.output.writeIndented(":ENDTRY;");
        this.output.writeLine();
        return { shouldContinue: true };
    }
    /**
     * Format ERROR block stanza
     * Follows EBNF: ErrorBlockStanza ::= ErrorMarker {Statement}
     */
    protected override visitErrorBlockStanza(node: ErrorBlockStanzaNode): VisitorResult {
        // Format :ERROR; statement
        this.output.writeIndented(":ERROR;");
        this.output.writeLine();

        // Indent for error body
        this.output.indent();

        // Format error body statements
        this.formatStatementList(node.statements);

        // Dedent after error body
        this.output.dedent();

        return { shouldContinue: false }; // We've handled all children manually
    }

    /**
     * Format ERROR marker
     * Follows EBNF: ErrorMarker ::= ":" "ERROR"
     */
    protected override visitErrorMarker(node: ErrorMarkerNode): VisitorResult {
        this.output.writeIndented(":ERROR;");
        this.output.writeLine();
        return { shouldContinue: true };
    }
    /**
     * Helper method to format a list of statements with proper indentation
     */
    private formatStatementList(statements: StatementNode[]): void {
        if (!statements || statements.length === 0) {
            return;
        }

        for (let i = 0; i < statements.length; i++) {
            const statement = statements[i];
            const nextStatement = i < statements.length - 1 ? statements[i + 1] : null;

            this.visit(statement);

            // Add blank line before certain statement types for readability
            if (nextStatement && this.shouldAddBlankLineAfter(statement, nextStatement)) {
                this.output.writeBlankLine();
            }
        }
    }

    /**
     * Helper method to format expressions (placeholder for now)
     */
    private formatExpression(expression: any): void {
        // This is a placeholder - in a full implementation, this would
        // format expressions properly
        if (expression) {
            this.visit(expression);
        }
    }

    /**
     * Determines whether to add a blank line between statements
     */
    private shouldAddBlankLineAfter(current: StatementNode, next: StatementNode): boolean {
        // Add blank lines around major control structures for readability
        if (this.isErrorHandlingStatement(current) || this.isErrorHandlingStatement(next)) {
            return true;
        }
        return false;
    }

    /**
     * Checks if a statement is an error handling statement
     */
    private isErrorHandlingStatement(statement: StatementNode): boolean {
        return ["TryBlock", "ErrorBlockStanza"].includes(statement.kind);
    }
}
