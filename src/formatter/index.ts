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
import {
    CommentAssociator,
    CommentAssociation,
    CommentPosition,
    defaultCommentAssociationOptions,
} from "./commentAssociation";
import {
    CommentStatementNode,
    BlockCommentNode,
    SingleLineCommentNode,
    RegionCommentNode,
    EndRegionCommentNode,
} from "../parser/ast/comments";

// Import all the individual formatter visitors
import { SSLControlFlowFormatterVisitor } from "./controlFlow";
import { SSLErrorHandlingFormatterVisitor } from "./errorHandling";
import { SSLCommentFormatterVisitor } from "./comments";
import { SSLExpressionFormatterVisitor } from "./expressions";
import { SSLDeclarationFormatterVisitor } from "./declarationVisitor";
import { SSLSqlFormatterVisitor } from "./sql";

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
    private readonly commentAssociator: CommentAssociator;
    private readonly controlFlowVisitor: SSLControlFlowFormatterVisitor;
    private readonly errorHandlingVisitor: SSLErrorHandlingFormatterVisitor;
    private readonly commentVisitor: SSLCommentFormatterVisitor;
    private readonly expressionVisitor: SSLExpressionFormatterVisitor;
    private readonly declarationVisitor: SSLDeclarationFormatterVisitor;
    private readonly sqlVisitor: SSLSqlFormatterVisitor;

    constructor(options: FormatterOptions = defaultFormatterOptions) {
        this.options = options;
        this.output = new OutputBuilder(options);
        this.commentAssociator = new CommentAssociator(defaultCommentAssociationOptions);

        // Initialize all specialized visitors with the same options
        this.controlFlowVisitor = new SSLControlFlowFormatterVisitor(options);
        this.errorHandlingVisitor = new SSLErrorHandlingFormatterVisitor(options);
        this.commentVisitor = new SSLCommentFormatterVisitor(options);
        this.expressionVisitor = new SSLExpressionFormatterVisitor(options);
        this.declarationVisitor = new SSLDeclarationFormatterVisitor(options);
        this.sqlVisitor = new SSLSqlFormatterVisitor(options);
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

        // Extract comments and non-comment nodes for association
        const { commentNodes, nonCommentNodes } = this.extractNodes(ast);

        // Associate comments with AST nodes
        const commentAssociations = this.commentAssociator.associateComments(
            commentNodes,
            nonCommentNodes
        );

        // Visit the AST starting from the root with comment awareness
        this.visitWithComments(ast, commentAssociations);

        return freshOutput.getOutput();
    }

    /**
     * Extract comment nodes and non-comment nodes from the AST
     */
    private extractNodes(ast: ASTNode): {
        commentNodes: (
            | CommentStatementNode
            | BlockCommentNode
            | SingleLineCommentNode
            | RegionCommentNode
            | EndRegionCommentNode
        )[];
        nonCommentNodes: ASTNode[];
    } {
        const commentNodes: (
            | CommentStatementNode
            | BlockCommentNode
            | SingleLineCommentNode
            | RegionCommentNode
            | EndRegionCommentNode
        )[] = [];
        const nonCommentNodes: ASTNode[] = [];

        // Traverse the AST to collect all nodes
        this.collectNodes(ast, commentNodes, nonCommentNodes);

        return { commentNodes, nonCommentNodes };
    }

    /**
     * Recursively collect comment and non-comment nodes from the AST
     */
    private collectNodes(
        node: ASTNode,
        commentNodes: (
            | CommentStatementNode
            | BlockCommentNode
            | SingleLineCommentNode
            | RegionCommentNode
            | EndRegionCommentNode
        )[],
        nonCommentNodes: ASTNode[]
    ): void {
        if (this.isCommentNode(node)) {
            commentNodes.push(node as any);
        } else {
            nonCommentNodes.push(node);
        }

        // Recursively traverse child nodes
        // Note: This is a simplified traversal - in a real implementation,
        // you would traverse based on the specific structure of each node type
        if ((node as any).body && Array.isArray((node as any).body)) {
            for (const child of (node as any).body) {
                this.collectNodes(child, commentNodes, nonCommentNodes);
            }
        }

        // Handle other common child node patterns
        if ((node as any).members && Array.isArray((node as any).members)) {
            for (const child of (node as any).members) {
                this.collectNodes(child, commentNodes, nonCommentNodes);
            }
        }
    }

    /**
     * Visit an AST node with comment awareness
     */
    private visitWithComments(node: ASTNode, commentAssociations: CommentAssociation[]): void {
        // Process leading comments for this node
        this.processLeadingComments(node, commentAssociations);

        // Visit the node itself
        this.visit(node);

        // Process trailing comments for this node
        this.processTrailingComments(node, commentAssociations);
    }

    /**
     * Process leading comments for a node
     */
    private processLeadingComments(node: ASTNode, commentAssociations: CommentAssociation[]): void {
        const leadingComments = commentAssociations
            .filter(
                (assoc) =>
                    assoc.associatedNode === node &&
                    assoc.position === CommentPosition.Leading &&
                    assoc.preserve
            )
            .sort((a, b) => a.lineNumber - b.lineNumber);

        for (const association of leadingComments) {
            this.commentVisitor.visit(association.comment);
            this.output.writeLine(); // Add line break after leading comment
        }
    }

    /**
     * Process trailing comments for a node
     */
    private processTrailingComments(
        node: ASTNode,
        commentAssociations: CommentAssociation[]
    ): void {
        const trailingComments = commentAssociations
            .filter(
                (assoc) =>
                    assoc.associatedNode === node &&
                    (assoc.position === CommentPosition.Trailing ||
                        assoc.position === CommentPosition.Following) &&
                    assoc.preserve
            )
            .sort((a, b) => a.lineNumber - b.lineNumber);

        for (const association of trailingComments) {
            if (association.position === CommentPosition.Trailing) {
                this.output.write(" "); // Add space before trailing comment on same line
            } else {
                this.output.writeLine(); // New line for following comments
            }
            this.commentVisitor.visit(association.comment);
        }
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

        if (this.isSqlNode(node)) {
            this.sqlVisitor.visit(node);
            return;
        }

        // Handle special cases that need main formatter logic
        if (node.kind === "Program") {
            this.visitProgram(node as ProgramNode);
            return;
        }

        // For unhandled node types, fall back to a default approach
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
     * Helper method to determine if a node is a SQL node
     */
    private isSqlNode(node: ASTNode): boolean {
        const sqlTypes = ["SqlStatement", "SqlExecute", "LSearch", "SqlParameter"];

        return sqlTypes.includes(node.kind);
    }
    /**
     * Format Program node (root of AST) with comment awareness
     */
    private visitProgram(node: ProgramNode): void {
        // Extract comments and non-comment nodes
        const { commentNodes, nonCommentNodes } = this.extractNodes(node);

        // Associate comments with AST nodes
        const commentAssociations = this.commentAssociator.associateComments(
            commentNodes,
            nonCommentNodes
        );

        // Process standalone comments first (those not associated with any node)
        this.processStandaloneComments(commentAssociations);

        if (node.body && node.body.length > 0) {
            for (let i = 0; i < node.body.length; i++) {
                const statement = node.body[i];

                // Skip comment nodes - they're handled through associations
                if (this.isCommentNode(statement)) {
                    continue;
                }

                // Visit each statement with comment awareness
                this.visitWithComments(statement, commentAssociations);

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
     * Process standalone comments (not associated with any specific node)
     */
    private processStandaloneComments(commentAssociations: CommentAssociation[]): void {
        const standaloneComments = commentAssociations
            .filter((assoc) => assoc.position === CommentPosition.Standalone && assoc.preserve)
            .sort((a, b) => a.lineNumber - b.lineNumber);

        for (const association of standaloneComments) {
            this.commentVisitor.visit(association.comment);
            this.output.writeLine();
        }

        // Add blank line after standalone comments if there are any
        if (standaloneComments.length > 0) {
            this.output.writeBlankLine();
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
export { SSLSqlFormatterVisitor } from "./sql";
export {
    CommentAssociator,
    CommentAssociation,
    CommentPosition,
    CommentAssociationOptions,
    defaultCommentAssociationOptions,
} from "./commentAssociation";
export { SSLFormatter as default };
