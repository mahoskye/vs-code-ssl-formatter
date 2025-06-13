/**
 * SSL Formatter
 *
 * Main formatter entry point:
 * - Main SSLFormatter class
 * - Public API for formatting SSL code
 * - Integration with VS Code formatting provider
 * - Orchestrates the visitor pattern execution
 */

import { ASTNode, ProgramNode } from "../parser/ast";
import { FormatterOptions, defaultFormatterOptions } from "./options";
import { OutputBuilder } from "./visitor";

// Import all the individual formatter visitors
import { SSLControlFlowFormatterVisitor } from "./controlFlow";
import { SSLErrorHandlingFormatterVisitor } from "./errorHandling";
import { SSLCommentFormatterVisitor } from "./comments";
import { SSLExpressionFormatterVisitor } from "./expressions";
import { SSLDeclarationFormatterVisitor } from "./declarationVisitor";

/**
 * Main SSL Formatter class that orchestrates all formatting functionality
 *
 * This class combines multiple specialized formatter visitors to provide
 * comprehensive SSL code formatting. It uses a composition approach where
 * different visitors handle different aspects of the language.
 */
export class SSLFormatter {
    private readonly output: OutputBuilder;
    private readonly options: FormatterOptions;
    private readonly controlFlowVisitor: SSLControlFlowFormatterVisitor;
    private readonly errorHandlingVisitor: SSLErrorHandlingFormatterVisitor;
    private readonly commentVisitor: SSLCommentFormatterVisitor;
    private readonly expressionVisitor: SSLExpressionFormatterVisitor;
    private readonly declarationVisitor: SSLDeclarationFormatterVisitor;

    constructor(options: FormatterOptions = defaultFormatterOptions) {
        this.options = options;
        this.output = new OutputBuilder(options);

        // Initialize all specialized visitors with the same options
        this.controlFlowVisitor = new SSLControlFlowFormatterVisitor(options);
        this.errorHandlingVisitor = new SSLErrorHandlingFormatterVisitor(options);
        this.commentVisitor = new SSLCommentFormatterVisitor(options);
        this.expressionVisitor = new SSLExpressionFormatterVisitor(options);
        this.declarationVisitor = new SSLDeclarationFormatterVisitor(options);
    }
    /**
     * Format an SSL AST and return the formatted code string
     *
     * @param ast The root AST node (typically a ProgramNode)
     * @returns The formatted SSL code as a string
     */
    public format(ast: ASTNode): string {
        // Create a fresh output builder for this formatting operation
        const freshOutput = new OutputBuilder(this.options);

        // Temporarily replace the output builder for the visitors
        this.replaceOutputBuilder(freshOutput);

        // Visit the AST starting from the root
        this.visit(ast);

        return freshOutput.getOutput();
    }

    /**
     * Visit an AST node and route to the appropriate specialized visitor
     *
     * @param node The AST node to format
     */
    private visit(node: ASTNode): void {
        // Route to appropriate specialized visitor based on node type
        if (this.isControlFlowNode(node)) {
            this.controlFlowVisitor.visit(node);
            return;
        }

        if (this.isErrorHandlingNode(node)) {
            this.errorHandlingVisitor.visit(node);
            return;
        }

        if (this.isCommentNode(node)) {
            this.commentVisitor.visit(node);
            return;
        }

        if (this.isExpressionNode(node)) {
            this.expressionVisitor.visit(node);
            return;
        }

        if (this.isDeclarationNode(node)) {
            this.declarationVisitor.visit(node);
            return;
        }

        // Handle special cases that need main formatter logic
        if (node.kind === "Program") {
            this.visitProgram(node as ProgramNode);
            return;
        } // For unhandled node types, fall back to a default approach
        this.visitDefault(node);
    }

    /**
     * Default visitor for unhandled node types
     */
    private visitDefault(node: ASTNode): void {
        // Basic fallback - just write the node's token content if available
        const token = (node as any).token || (node as any).startToken;
        if (token && token.value) {
            this.output.writeIndented(token.value);
        }
    }

    /**
     * Helper method to determine if a node is a control flow node
     */
    private isControlFlowNode(node: ASTNode): boolean {
        const controlFlowTypes = [
            "ConditionalStatement",
            "IfStatement",
            "ElseStatement",
            "EndIfStatement",
            "LoopStatement",
            "WhileLoop",
            "WhileStatement",
            "EndWhileStatement",
            "ForLoop",
            "ForStatement",
            "NextStatement",
            "ExitWhileStatement",
            "ExitForStatement",
            "LoopContinue",
            "SwitchStatement",
            "BeginCaseStatement",
            "CaseBlock",
            "CaseStatement",
            "OtherwiseBlock",
            "OtherwiseStatement",
            "EndCaseStatement",
            "ExitCaseStatement",
        ];

        return controlFlowTypes.includes(node.kind);
    }

    /**
     * Helper method to determine if a node is an error handling node
     */
    private isErrorHandlingNode(node: ASTNode): boolean {
        const errorHandlingTypes = [
            "ErrorHandlingStatement",
            "TryBlock",
            "TryStatement",
            "CatchBlock",
            "CatchStatement",
            "FinallyBlock",
            "FinallyStatement",
            "EndTryStatement",
            "ErrorBlockStanza",
            "ErrorMarker",
        ];

        return errorHandlingTypes.includes(node.kind);
    }

    /**
     * Helper method to determine if a node is a comment node
     */
    private isCommentNode(node: ASTNode): boolean {
        const commentTypes = [
            "CommentStatement",
            "BlockComment",
            "SingleLineComment",
            "RegionComment",
            "EndRegionComment",
        ];

        return commentTypes.includes(node.kind);
    }

    /**
     * Helper method to determine if a node is an expression node
     */
    private isExpressionNode(node: ASTNode): boolean {
        const expressionTypes = [
            "Expression",
            "BinaryExpression",
            "LogicalExpression",
            "ComparisonExpression",
            "ArithmeticExpression",
            "UnaryExpression",
            "IncrementExpression",
            "VariableAccess",
            "PropertyAccess",
            "ArrayAccess",
            "ArraySubscript",
            "FunctionCall",
            "DirectFunctionCall",
            "DoProcCall",
            "ExecFunctionCall",
            "MethodCall",
            "ObjectCreation",
            "BitwiseOperation",
            "Primary",
            "Term",
            "Factor",
            "PowerOperand",
            "Literal",
            "LiteralExpression",
            "NumberLiteral",
            "StringLiteral",
            "BooleanLiteral",
            "ArrayLiteral",
            "NilLiteral",
            "DateLiteral",
            "CodeBlockLiteral",
        ];

        return expressionTypes.includes(node.kind);
    }

    /**
     * Helper method to determine if a node is a declaration node
     */
    private isDeclarationNode(node: ASTNode): boolean {
        const declarationTypes = [
            "DeclarationStatement",
            "ParametersStatement",
            "DeclareStatement",
            "DefaultStatement",
            "PublicStatement",
            "IncludeStatement",
        ];

        return declarationTypes.includes(node.kind);
    }
    /**
     * Format Program node (root of AST)
     */
    private visitProgram(node: ProgramNode): void {
        if (node.body && node.body.length > 0) {
            for (let i = 0; i < node.body.length; i++) {
                const statement = node.body[i];

                // Visit each statement
                this.visit(statement);

                // Add appropriate spacing between statements
                if (i < node.body.length - 1) {
                    this.output.writeLine();

                    // Add blank line between major sections if configured
                    if (
                        this.options.preserveBlankLines &&
                        this.shouldAddBlankLine(statement, node.body[i + 1])
                    ) {
                        this.output.writeBlankLine();
                    }
                }
            }
        }
    }

    /**
     * Helper method to determine if a blank line should be added between statements
     */
    private shouldAddBlankLine(current: ASTNode, next: ASTNode): boolean {
        // Add blank lines between major structural elements
        const majorElements = [
            "ClassDefinition",
            "ProcedureStatement",
            "ErrorHandlingStatement",
            "ConditionalStatement",
            "LoopStatement",
            "SwitchStatement",
        ];
        return majorElements.includes(current.kind) || majorElements.includes(next.kind);
    }

    /**
     * Get the formatted output (for testing and internal use)
     */
    public getFormattedOutput(): string {
        return this.output.getOutput();
    }

    /**
     * Replace the output builder for all visitors
     *
     * @param newOutput The new output builder to use
     */
    private replaceOutputBuilder(newOutput: OutputBuilder): void {
        // Replace the output builder in all visitors
        (this.controlFlowVisitor as any).output = newOutput;
        (this.errorHandlingVisitor as any).output = newOutput;
        (this.commentVisitor as any).output = newOutput;
        (this.expressionVisitor as any).output = newOutput;
        (this.declarationVisitor as any).output = newOutput;
    }
}

/**
 * Convenience function to format SSL code
 *
 * @param ast The SSL AST to format
 * @param options Optional formatting options
 * @returns The formatted SSL code string
 */
export function formatSSL(ast: ASTNode, options?: FormatterOptions): string {
    const formatter = new SSLFormatter(options);
    return formatter.format(ast);
}

// Re-export for convenience
export { FormatterOptions, defaultFormatterOptions } from "./options";
export { SSLFormatter as default };
