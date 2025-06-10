/**
 * Formatter Visitor Base
 *
 * Abstract base visitor class implementing the visitor pattern
 * - Core traversal logic for AST nodes
 * - State management (output builder, indentation level, options)
 */

import {
    ASTNode,
    ASTNodeType,
    ProgramNode,
    StatementNode,
    ExpressionNode,
    ClassDefinitionNode,
    ClassDeclarationNode,
    InheritStatementNode,
    ClassFieldDeclarationNode,
    MethodDeclarationNode,
    ProcedureStatementNode,
    ProcedureStartNode,
    ProcedureEndNode,
    ParameterDeclarationNode,
    DefaultParameterDeclarationNode,
    ParameterListNode,
    DefaultParameterListNode,
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
    SwitchStatementNode,
    BeginCaseStatementNode,
    CaseBlockNode,
    CaseStatementNode,
    OtherwiseBlockNode,
    OtherwiseStatementNode,
    EndCaseStatementNode,
    ExitCaseStatementNode,
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
    DeclarationStatementNode,
    ParametersStatementNode,
    DeclareStatementNode,
    DefaultStatementNode,
    PublicStatementNode,
    IncludeStatementNode,
    LogicStatementNode,
    AssignmentNode,
    ReturnStatementNode,
    FunctionCallNode,
    DirectFunctionCallNode,
    DoProcCallNode,
    ExecFunctionCallNode,
    ArgumentListNode,
    BitwiseOperationNode,
    CommentStatementNode,
    BlockCommentNode,
    SingleLineCommentNode,
    RegionCommentNode,
    EndRegionCommentNode,
    LabelStatementNode,
    RegionBlockNode,
    RegionStartNode,
    RegionEndNode,
    InlineCodeBlockNode,
    InlineCodeStartNode,
    InlineCodeEndNode,
    DynamicCodeExecutionNode,
    BranchStatementNode,
    SqlStatementNode,
    SqlExecuteNode,
    LSearchNode,
    SqlParameterNode,
    ObjectCreationNode,
    MethodCallNode,
    BinaryExpressionNode,
    LogicalExpressionNode,
    ComparisonExpressionNode,
    ArithmeticExpressionNode,
    TermNode,
    FactorNode,
    PowerOperandNode,
    UnaryExpressionNode,
    IncrementExpressionNode,
    VariableAccessNode,
    PropertyAccessNode,
    ArrayAccessNode,
    ArraySubscriptNode,
    PrimaryNode,
    LiteralNode,
    LiteralExpressionNode,
    NumberLiteralNode,
    StringLiteralNode,
    BooleanLiteralNode,
    ArrayLiteralNode,
    NilLiteralNode,
    DateLiteralNode,
    CodeBlockLiteralNode,
    IdentifierListNode,
    ExpressionListNode,
} from "../parser/ast";
import { FormatterOptions, defaultFormatterOptions } from "./options";

// Re-export for backward compatibility
export type { FormatterOptions } from "./options";
export { defaultFormatterOptions } from "./options";

/**
 * Simple output builder for managing formatted output
 */
export class OutputBuilder {
    private lines: string[] = [];
    private currentLine = "";
    private indentLevel = 0;
    private readonly options: FormatterOptions;

    constructor(options: FormatterOptions = defaultFormatterOptions) {
        this.options = options;
    }

    /**
     * Increase indentation level
     */
    indent(): void {
        this.indentLevel++;
    }

    /**
     * Decrease indentation level
     */
    dedent(): void {
        if (this.indentLevel > 0) {
            this.indentLevel--;
        }
    }

    /**
     * Get current indentation string
     */
    private getIndent(): string {
        if (this.options.useTabs) {
            return "\t".repeat(this.indentLevel);
        }
        return " ".repeat(this.indentLevel * this.options.indentSize);
    }

    /**
     * Write text to current line
     */
    write(text: string): void {
        this.currentLine += text;
    }

    /**
     * Write text with proper indentation if at start of line
     */
    writeIndented(text: string): void {
        if (this.currentLine === "") {
            this.currentLine = this.getIndent();
        }
        this.currentLine += text;
    }

    /**
     * End current line and start new one
     */
    writeLine(text?: string): void {
        if (text !== undefined) {
            this.writeIndented(text);
        }
        this.lines.push(this.currentLine);
        this.currentLine = "";
    }

    /**
     * Write a blank line
     */
    writeBlankLine(): void {
        this.lines.push("");
        this.currentLine = "";
    }
    /**
     * Get the final formatted output
     */
    getOutput(): string {
        // Add any remaining current line
        if (this.currentLine.trim() !== "") {
            this.lines.push(this.currentLine);
        }

        let result = this.lines.join("\n");

        // Trim trailing whitespace if enabled
        if (this.options.trimTrailingWhitespace) {
            result = result.replace(/[ \t]+$/gm, "");
        }

        // Insert final newline if enabled, but only if there's actual content
        if (this.options.insertFinalNewline && result.length > 0 && !result.endsWith("\n")) {
            result += "\n";
        }

        return result;
    }

    /**
     * Get current output length for line wrapping decisions
     */
    getCurrentLineLength(): number {
        return this.currentLine.length;
    }

    /**
     * Check if adding text would exceed max line length
     */
    wouldExceedMaxLength(additionalText: string): boolean {
        return this.getCurrentLineLength() + additionalText.length > this.options.maxLineLength;
    }
}

/**
 * Visitor result containing traversal information
 */
export interface VisitorResult {
    /** Whether the visitor should continue traversing child nodes */
    shouldContinue: boolean;
    /** Optional error message if visiting failed */
    error?: string;
}

/**
 * Base class for AST visitors implementing the visitor pattern
 */
export class FormatterVisitorBase {
    protected readonly output: OutputBuilder;
    protected readonly options: FormatterOptions;
    private visitedNodes = new Set<ASTNode>();

    constructor(options: FormatterOptions = defaultFormatterOptions) {
        this.options = options;
        this.output = new OutputBuilder(options);
    }

    /**
     * Visit an AST node and its children
     */
    public visit(node: ASTNode): VisitorResult {
        // Prevent infinite recursion
        if (this.visitedNodes.has(node)) {
            return { shouldContinue: false, error: "Circular reference detected" };
        }

        this.visitedNodes.add(node);

        try {
            const result = this.dispatch(node);
            if (result.shouldContinue) {
                this.visitChildren(node);
            }
            return result;
        } catch (error) {
            return {
                shouldContinue: false,
                error: error instanceof Error ? error.message : String(error),
            };
        } finally {
            this.visitedNodes.delete(node);
        }
    }

    /**
     * Dispatch to appropriate visit method based on node type
     */
    private dispatch(node: ASTNode): VisitorResult {
        switch (node.kind) {
            // Top-level
            case ASTNodeType.Program:
                return this.visitProgram(node as ProgramNode);

            // Class definitions
            case ASTNodeType.ClassDefinition:
                return this.visitClassDefinition(node as ClassDefinitionNode);
            case ASTNodeType.ClassDeclaration:
                return this.visitClassDeclaration(node as ClassDeclarationNode);
            case ASTNodeType.InheritStatement:
                return this.visitInheritStatement(node as InheritStatementNode);
            case ASTNodeType.ClassFieldDeclaration:
                return this.visitClassFieldDeclaration(node as ClassFieldDeclarationNode);
            case ASTNodeType.MethodDeclaration:
                return this.visitMethodDeclaration(node as MethodDeclarationNode);

            // Procedure declarations
            case ASTNodeType.ProcedureStatement:
                return this.visitProcedureStatement(node as ProcedureStatementNode);
            case ASTNodeType.ProcedureStart:
                return this.visitProcedureStart(node as ProcedureStartNode);
            case ASTNodeType.ProcedureEnd:
                return this.visitProcedureEnd(node as ProcedureEndNode);
            case ASTNodeType.ParameterDeclaration:
                return this.visitParameterDeclaration(node as ParameterDeclarationNode);
            case ASTNodeType.DefaultParameterDeclaration:
                return this.visitDefaultParameterDeclaration(
                    node as DefaultParameterDeclarationNode
                );
            case ASTNodeType.ParameterList:
                return this.visitParameterList(node as ParameterListNode);
            case ASTNodeType.DefaultParameterList:
                return this.visitDefaultParameterList(node as DefaultParameterListNode);

            // Control flow statements
            case ASTNodeType.ConditionalStatement:
                return this.visitConditionalStatement(node as ConditionalStatementNode);
            case ASTNodeType.IfStatement:
                return this.visitIfStatement(node as IfStatementNode);
            case ASTNodeType.ElseStatement:
                return this.visitElseStatement(node as ElseStatementNode);
            case ASTNodeType.EndIfStatement:
                return this.visitEndIfStatement(node as EndIfStatementNode);
            case ASTNodeType.LoopStatement:
                return this.visitLoopStatement(node as LoopStatementNode);
            case ASTNodeType.WhileLoop:
                return this.visitWhileLoop(node as WhileLoopNode);
            case ASTNodeType.WhileStatement:
                return this.visitWhileStatement(node as WhileStatementNode);
            case ASTNodeType.EndWhileStatement:
                return this.visitEndWhileStatement(node as EndWhileStatementNode);
            case ASTNodeType.ForLoop:
                return this.visitForLoop(node as ForLoopNode);
            case ASTNodeType.ForStatement:
                return this.visitForStatement(node as ForStatementNode);
            case ASTNodeType.NextStatement:
                return this.visitNextStatement(node as NextStatementNode);
            case ASTNodeType.ExitWhileStatement:
                return this.visitExitWhileStatement(node as ExitWhileStatementNode);
            case ASTNodeType.ExitForStatement:
                return this.visitExitForStatement(node as ExitForStatementNode);
            case ASTNodeType.LoopContinue:
                return this.visitLoopContinue(node as LoopContinueNode);

            // Switch case statements
            case ASTNodeType.SwitchStatement:
                return this.visitSwitchStatement(node as SwitchStatementNode);
            case ASTNodeType.BeginCaseStatement:
                return this.visitBeginCaseStatement(node as BeginCaseStatementNode);
            case ASTNodeType.CaseBlock:
                return this.visitCaseBlock(node as CaseBlockNode);
            case ASTNodeType.CaseStatement:
                return this.visitCaseStatement(node as CaseStatementNode);
            case ASTNodeType.OtherwiseBlock:
                return this.visitOtherwiseBlock(node as OtherwiseBlockNode);
            case ASTNodeType.OtherwiseStatement:
                return this.visitOtherwiseStatement(node as OtherwiseStatementNode);
            case ASTNodeType.EndCaseStatement:
                return this.visitEndCaseStatement(node as EndCaseStatementNode);
            case ASTNodeType.ExitCaseStatement:
                return this.visitExitCaseStatement(node as ExitCaseStatementNode);

            // Error handling
            case ASTNodeType.ErrorHandlingStatement:
                return this.visitErrorHandlingStatement(node as ErrorHandlingStatementNode);
            case ASTNodeType.TryBlock:
                return this.visitTryBlock(node as TryBlockNode);
            case ASTNodeType.TryStatement:
                return this.visitTryStatement(node as TryStatementNode);
            case ASTNodeType.CatchBlock:
                return this.visitCatchBlock(node as CatchBlockNode);
            case ASTNodeType.CatchStatement:
                return this.visitCatchStatement(node as CatchStatementNode);
            case ASTNodeType.FinallyBlock:
                return this.visitFinallyBlock(node as FinallyBlockNode);
            case ASTNodeType.FinallyStatement:
                return this.visitFinallyStatement(node as FinallyStatementNode);
            case ASTNodeType.EndTryStatement:
                return this.visitEndTryStatement(node as EndTryStatementNode);
            case ASTNodeType.ErrorBlockStanza:
                return this.visitErrorBlockStanza(node as ErrorBlockStanzaNode);
            case ASTNodeType.ErrorMarker:
                return this.visitErrorMarker(node as ErrorMarkerNode);

            // Declaration statements
            case ASTNodeType.DeclarationStatement:
                return this.visitDeclarationStatement(node as DeclarationStatementNode);
            case ASTNodeType.ParametersStatement:
                return this.visitParametersStatement(node as ParametersStatementNode);
            case ASTNodeType.DeclareStatement:
                return this.visitDeclareStatement(node as DeclareStatementNode);
            case ASTNodeType.DefaultStatement:
                return this.visitDefaultStatement(node as DefaultStatementNode);
            case ASTNodeType.PublicStatement:
                return this.visitPublicStatement(node as PublicStatementNode);
            case ASTNodeType.IncludeStatement:
                return this.visitIncludeStatement(node as IncludeStatementNode);

            // Logic statements
            case ASTNodeType.LogicStatement:
                return this.visitLogicStatement(node as LogicStatementNode);
            case ASTNodeType.Assignment:
                return this.visitAssignment(node as AssignmentNode);
            case ASTNodeType.ReturnStatement:
                return this.visitReturnStatement(node as ReturnStatementNode);

            // Function calls
            case ASTNodeType.FunctionCall:
                return this.visitFunctionCall(node as FunctionCallNode);
            case ASTNodeType.DirectFunctionCall:
                return this.visitDirectFunctionCall(node as DirectFunctionCallNode);
            case ASTNodeType.DoProcCall:
                return this.visitDoProcCall(node as DoProcCallNode);
            case ASTNodeType.ExecFunctionCall:
                return this.visitExecFunctionCall(node as ExecFunctionCallNode);
            case ASTNodeType.ArgumentList:
                return this.visitArgumentList(node as ArgumentListNode);
            case ASTNodeType.BitwiseOperation:
                return this.visitBitwiseOperation(node as BitwiseOperationNode);

            // Comments
            case ASTNodeType.CommentStatement:
                return this.visitCommentStatement(node as CommentStatementNode);
            case ASTNodeType.BlockComment:
                return this.visitBlockComment(node as BlockCommentNode);
            case ASTNodeType.SingleLineComment:
                return this.visitSingleLineComment(node as SingleLineCommentNode);
            case ASTNodeType.RegionComment:
                return this.visitRegionComment(node as RegionCommentNode);
            case ASTNodeType.EndRegionComment:
                return this.visitEndRegionComment(node as EndRegionCommentNode);

            // Special structures
            case ASTNodeType.LabelStatement:
                return this.visitLabelStatement(node as LabelStatementNode);
            case ASTNodeType.RegionBlock:
                return this.visitRegionBlock(node as RegionBlockNode);
            case ASTNodeType.RegionStart:
                return this.visitRegionStart(node as RegionStartNode);
            case ASTNodeType.RegionEnd:
                return this.visitRegionEnd(node as RegionEndNode);
            case ASTNodeType.InlineCodeBlock:
                return this.visitInlineCodeBlock(node as InlineCodeBlockNode);
            case ASTNodeType.InlineCodeStart:
                return this.visitInlineCodeStart(node as InlineCodeStartNode);
            case ASTNodeType.InlineCodeEnd:
                return this.visitInlineCodeEnd(node as InlineCodeEndNode);
            case ASTNodeType.DynamicCodeExecution:
                return this.visitDynamicCodeExecution(node as DynamicCodeExecutionNode);
            case ASTNodeType.BranchStatement:
                return this.visitBranchStatement(node as BranchStatementNode);

            // SQL Integration
            case ASTNodeType.SqlStatement:
                return this.visitSqlStatement(node as SqlStatementNode);
            case ASTNodeType.SqlExecute:
                return this.visitSqlExecute(node as SqlExecuteNode);
            case ASTNodeType.LSearch:
                return this.visitLSearch(node as LSearchNode);
            case ASTNodeType.SqlParameter:
                return this.visitSqlParameter(node as SqlParameterNode);

            // Object-oriented statements
            case ASTNodeType.ObjectCreation:
                return this.visitObjectCreation(node as ObjectCreationNode);
            case ASTNodeType.MethodCall:
                return this.visitMethodCall(node as MethodCallNode);

            // Expressions
            case ASTNodeType.Expression:
                return this.visitExpression(node as ExpressionNode);
            case ASTNodeType.BinaryExpression:
                return this.visitBinaryExpression(node as BinaryExpressionNode);
            case ASTNodeType.LogicalExpression:
                return this.visitLogicalExpression(node as LogicalExpressionNode);
            case ASTNodeType.ComparisonExpression:
                return this.visitComparisonExpression(node as ComparisonExpressionNode);
            case ASTNodeType.ArithmeticExpression:
                return this.visitArithmeticExpression(node as ArithmeticExpressionNode);
            case ASTNodeType.Term:
                return this.visitTerm(node as TermNode);
            case ASTNodeType.Factor:
                return this.visitFactor(node as FactorNode);
            case ASTNodeType.PowerOperand:
                return this.visitPowerOperand(node as PowerOperandNode);
            case ASTNodeType.UnaryExpression:
                return this.visitUnaryExpression(node as UnaryExpressionNode);
            case ASTNodeType.IncrementExpression:
                return this.visitIncrementExpression(node as IncrementExpressionNode);
            case ASTNodeType.VariableAccess:
                return this.visitVariableAccess(node as VariableAccessNode);
            case ASTNodeType.PropertyAccess:
                return this.visitPropertyAccess(node as PropertyAccessNode);
            case ASTNodeType.ArrayAccess:
                return this.visitArrayAccess(node as ArrayAccessNode);
            case ASTNodeType.ArraySubscript:
                return this.visitArraySubscript(node as ArraySubscriptNode);
            case ASTNodeType.Primary:
                return this.visitPrimary(node as PrimaryNode);

            // Literals
            case ASTNodeType.Literal:
                return this.visitLiteral(node as LiteralNode);
            case ASTNodeType.LiteralExpression:
                return this.visitLiteralExpression(node as LiteralExpressionNode);
            case ASTNodeType.NumberLiteral:
                return this.visitNumberLiteral(node as NumberLiteralNode);
            case ASTNodeType.StringLiteral:
                return this.visitStringLiteral(node as StringLiteralNode);
            case ASTNodeType.BooleanLiteral:
                return this.visitBooleanLiteral(node as BooleanLiteralNode);
            case ASTNodeType.ArrayLiteral:
                return this.visitArrayLiteral(node as ArrayLiteralNode);
            case ASTNodeType.NilLiteral:
                return this.visitNilLiteral(node as NilLiteralNode);
            case ASTNodeType.DateLiteral:
                return this.visitDateLiteral(node as DateLiteralNode);
            case ASTNodeType.CodeBlockLiteral:
                return this.visitCodeBlockLiteral(node as CodeBlockLiteralNode);

            // Lists
            case ASTNodeType.IdentifierList:
                return this.visitIdentifierList(node as IdentifierListNode);
            case ASTNodeType.ExpressionList:
                return this.visitExpressionList(node as ExpressionListNode);

            default:
                return this.visitUnknown(node);
        }
    }

    /**
     * Visit child nodes of a given node
     */
    protected visitChildren(node: ASTNode): void {
        // This is a simplified implementation
        // In a real implementation, you would traverse the actual child nodes
        // based on the specific structure of each node type

        // For now, we provide a basic implementation that can be overridden
        this.visitChildrenDefault(node);
    }

    /**
     * Default implementation for visiting children
     * Subclasses can override this for specific node types
     */
    protected visitChildrenDefault(node: ASTNode): void {
        // Default implementation - does nothing
        // Subclasses should implement specific traversal logic
    }

    /**
     * Get the formatted output
     */
    public getFormattedOutput(): string {
        return this.output.getOutput();
    }

    // Abstract visitor methods that subclasses must implement
    // These provide default implementations that can be overridden

    // Top-level
    protected visitProgram(node: ProgramNode): VisitorResult {
        return { shouldContinue: true };
    }

    // Class definitions
    protected visitClassDefinition(node: ClassDefinitionNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitClassDeclaration(node: ClassDeclarationNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitInheritStatement(node: InheritStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitClassFieldDeclaration(node: ClassFieldDeclarationNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitMethodDeclaration(node: MethodDeclarationNode): VisitorResult {
        return { shouldContinue: true };
    }

    // Procedure declarations
    protected visitProcedureStatement(node: ProcedureStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitProcedureStart(node: ProcedureStartNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitProcedureEnd(node: ProcedureEndNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitParameterDeclaration(node: ParameterDeclarationNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitDefaultParameterDeclaration(
        node: DefaultParameterDeclarationNode
    ): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitParameterList(node: ParameterListNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitDefaultParameterList(node: DefaultParameterListNode): VisitorResult {
        return { shouldContinue: true };
    }

    // Control flow statements
    protected visitConditionalStatement(node: ConditionalStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitIfStatement(node: IfStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitElseStatement(node: ElseStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitEndIfStatement(node: EndIfStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitLoopStatement(node: LoopStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitWhileLoop(node: WhileLoopNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitWhileStatement(node: WhileStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitEndWhileStatement(node: EndWhileStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitForLoop(node: ForLoopNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitForStatement(node: ForStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitNextStatement(node: NextStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitExitWhileStatement(node: ExitWhileStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitExitForStatement(node: ExitForStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitLoopContinue(node: LoopContinueNode): VisitorResult {
        return { shouldContinue: true };
    }

    // Switch case statements
    protected visitSwitchStatement(node: SwitchStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitBeginCaseStatement(node: BeginCaseStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitCaseBlock(node: CaseBlockNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitCaseStatement(node: CaseStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitOtherwiseBlock(node: OtherwiseBlockNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitOtherwiseStatement(node: OtherwiseStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitEndCaseStatement(node: EndCaseStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitExitCaseStatement(node: ExitCaseStatementNode): VisitorResult {
        return { shouldContinue: true };
    }

    // Error handling
    protected visitErrorHandlingStatement(node: ErrorHandlingStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitTryBlock(node: TryBlockNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitTryStatement(node: TryStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitCatchBlock(node: CatchBlockNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitCatchStatement(node: CatchStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitFinallyBlock(node: FinallyBlockNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitFinallyStatement(node: FinallyStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitEndTryStatement(node: EndTryStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitErrorBlockStanza(node: ErrorBlockStanzaNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitErrorMarker(node: ErrorMarkerNode): VisitorResult {
        return { shouldContinue: true };
    }

    // Declaration statements
    protected visitDeclarationStatement(node: DeclarationStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitParametersStatement(node: ParametersStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitDeclareStatement(node: DeclareStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitDefaultStatement(node: DefaultStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitPublicStatement(node: PublicStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitIncludeStatement(node: IncludeStatementNode): VisitorResult {
        return { shouldContinue: true };
    }

    // Logic statements
    protected visitLogicStatement(node: LogicStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitAssignment(node: AssignmentNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitReturnStatement(node: ReturnStatementNode): VisitorResult {
        return { shouldContinue: true };
    }

    // Function calls
    protected visitFunctionCall(node: FunctionCallNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitDirectFunctionCall(node: DirectFunctionCallNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitDoProcCall(node: DoProcCallNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitExecFunctionCall(node: ExecFunctionCallNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitArgumentList(node: ArgumentListNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitBitwiseOperation(node: BitwiseOperationNode): VisitorResult {
        return { shouldContinue: true };
    }

    // Comments
    protected visitCommentStatement(node: CommentStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitBlockComment(node: BlockCommentNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitSingleLineComment(node: SingleLineCommentNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitRegionComment(node: RegionCommentNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitEndRegionComment(node: EndRegionCommentNode): VisitorResult {
        return { shouldContinue: true };
    }

    // Special structures
    protected visitLabelStatement(node: LabelStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitRegionBlock(node: RegionBlockNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitRegionStart(node: RegionStartNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitRegionEnd(node: RegionEndNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitInlineCodeBlock(node: InlineCodeBlockNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitInlineCodeStart(node: InlineCodeStartNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitInlineCodeEnd(node: InlineCodeEndNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitDynamicCodeExecution(node: DynamicCodeExecutionNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitBranchStatement(node: BranchStatementNode): VisitorResult {
        return { shouldContinue: true };
    }

    // SQL Integration
    protected visitSqlStatement(node: SqlStatementNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitSqlExecute(node: SqlExecuteNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitLSearch(node: LSearchNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitSqlParameter(node: SqlParameterNode): VisitorResult {
        return { shouldContinue: true };
    }

    // Object-oriented statements
    protected visitObjectCreation(node: ObjectCreationNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitMethodCall(node: MethodCallNode): VisitorResult {
        return { shouldContinue: true };
    }

    // Expressions
    protected visitExpression(node: ExpressionNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitBinaryExpression(node: BinaryExpressionNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitLogicalExpression(node: LogicalExpressionNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitComparisonExpression(node: ComparisonExpressionNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitArithmeticExpression(node: ArithmeticExpressionNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitTerm(node: TermNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitFactor(node: FactorNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitPowerOperand(node: PowerOperandNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitUnaryExpression(node: UnaryExpressionNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitIncrementExpression(node: IncrementExpressionNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitVariableAccess(node: VariableAccessNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitPropertyAccess(node: PropertyAccessNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitArrayAccess(node: ArrayAccessNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitArraySubscript(node: ArraySubscriptNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitPrimary(node: PrimaryNode): VisitorResult {
        return { shouldContinue: true };
    }

    // Literals
    protected visitLiteral(node: LiteralNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitLiteralExpression(node: LiteralExpressionNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitNumberLiteral(node: NumberLiteralNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitStringLiteral(node: StringLiteralNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitBooleanLiteral(node: BooleanLiteralNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitArrayLiteral(node: ArrayLiteralNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitNilLiteral(node: NilLiteralNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitDateLiteral(node: DateLiteralNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitCodeBlockLiteral(node: CodeBlockLiteralNode): VisitorResult {
        return { shouldContinue: true };
    }

    // Lists
    protected visitIdentifierList(node: IdentifierListNode): VisitorResult {
        return { shouldContinue: true };
    }
    protected visitExpressionList(node: ExpressionListNode): VisitorResult {
        return { shouldContinue: true };
    }

    // Unknown node type
    protected visitUnknown(node: ASTNode): VisitorResult {
        return {
            shouldContinue: false,
            error: `Unknown node type: ${node.kind}`,
        };
    }
}
