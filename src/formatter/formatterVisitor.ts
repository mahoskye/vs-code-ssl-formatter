/**
 * FormatterVisitor - Implements the Visitor Pattern for AST-based SSL code formatting
 * Based on the SSL EBNF grammar specification and development plan Part 3
 */

import { Token } from "../tokenizer/token";
import { TokenType } from "../tokenizer/tokenType";
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
    ParameterDeclarationNode,
    DefaultParameterDeclarationNode,
    IfStatementNode,
    WhileLoopNode,
    ForLoopNode,
    ExitWhileStatementNode,
    ExitForStatementNode,
    LoopContinueNode,
    SwitchStatementNode,
    CaseBlockNode,
    OtherwiseBlockNode,
    TryBlockNode,
    CatchBlockNode,
    FinallyBlockNode,
    ErrorBlockStanzaNode,
    ParametersStatementNode,
    DeclareStatementNode,
    PublicStatementNode,
    IncludeStatementNode,
    AssignmentNode,
    ReturnStatementNode,
    DirectFunctionCallNode,
    DoProcCallNode,
    ExecFunctionCallNode,
    CommentStatementNode,
    SingleLineCommentNode,
    LabelStatementNode,
    RegionBlockNode,
    InlineCodeBlockNode,
    BranchStatementNode,
    SqlExecuteNode,
    LSearchNode,
    BinaryExpressionNode,
    UnaryExpressionNode,
    IncrementExpressionNode,
    VariableAccessNode,
    PropertyAccessNode,
    ArrayAccessNode,
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
    ArgumentListNode,
    DefaultParameterListNode,
} from "../parser/ast";

/**
 * Formatting options configuration
 */
export interface FormatterOptions {
    /** Number of spaces per indentation level */
    indentSize: number;
    /** Whether to use tabs instead of spaces */
    useTabs: boolean;
    /** Maximum line length before wrapping */
    maxLineLength: number;
    /** Whether to insert blank lines between procedures */
    blankLinesBetweenProcedures: boolean;
    /** Whether to align consecutive assignments */
    alignConsecutiveAssignments: boolean;
    /** Whether to format SQL strings */
    formatSqlStrings: boolean;
}

/**
 * Default formatting options
 */
export const DEFAULT_FORMATTER_OPTIONS: FormatterOptions = {
    indentSize: 4,
    useTabs: false,
    maxLineLength: 90,
    blankLinesBetweenProcedures: true,
    alignConsecutiveAssignments: false,
    formatSqlStrings: false,
};

/**
 * Output builder for efficient string construction
 */
class OutputBuilder {
    private lines: string[] = [];
    private currentLine = "";
    private indentString: string;

    constructor(private options: FormatterOptions) {
        this.indentString = options.useTabs ? "\t" : " ".repeat(options.indentSize);
    }

    /**
     * Append text to the current line
     */
    append(text: string): void {
        this.currentLine += text;
    }

    /**
     * Append text with indentation for the next line
     */
    appendWithIndentation(text: string, indentLevel: number = 0): void {
        this.finishCurrentLine();
        this.currentLine = this.indentString.repeat(indentLevel) + text;
    }

    /**
     * Add a newline and start a new line with indentation
     */
    newLine(indentLevel: number = 0): void {
        this.finishCurrentLine();
        this.currentLine = this.indentString.repeat(indentLevel);
    }

    /**
     * Add one or more blank lines
     */
    blankLines(count: number = 1): void {
        this.finishCurrentLine();
        for (let i = 0; i < count; i++) {
            this.lines.push("");
        }
    }

    /**
     * Finish the current line and add it to the lines array
     */
    private finishCurrentLine(): void {
        this.lines.push(this.currentLine);
        this.currentLine = "";
    }

    /**
     * Get the final formatted output
     */
    toString(): string {
        this.finishCurrentLine();
        return this.lines.join("\n").trimEnd();
    }
}

/**
 * FormatterVisitor implements the Visitor Pattern for AST-based SSL formatting
 */
export class FormatterVisitor {
    private output: OutputBuilder;
    private indentationLevel = 0;
    private options: FormatterOptions;

    constructor(options: Partial<FormatterOptions> = {}) {
        this.options = { ...DEFAULT_FORMATTER_OPTIONS, ...options };
        this.output = new OutputBuilder(this.options);
    }

    /**
     * Main entry point for formatting an AST
     */
    format(node: ASTNode): string {
        this.output = new OutputBuilder(this.options);
        this.indentationLevel = 0;
        this.visit(node);
        return this.output.toString();
    }
    /**
     * Generic visit method that dispatches to specific visit methods
     */
    private visit(node: ASTNode | null): void {
        if (!node) {
            return;
        }

        switch (node.kind) {
            case ASTNodeType.Program:
                this.visitProgramNode(node as ProgramNode);
                break;
            case ASTNodeType.ClassDefinition:
                this.visitClassDefinitionNode(node as ClassDefinitionNode);
                break;
            case ASTNodeType.ClassDeclaration:
                this.visitClassDeclarationNode(node as ClassDeclarationNode);
                break;
            case ASTNodeType.InheritStatement:
                this.visitInheritStatementNode(node as InheritStatementNode);
                break;
            case ASTNodeType.ClassFieldDeclaration:
                this.visitClassFieldDeclarationNode(node as ClassFieldDeclarationNode);
                break;
            case ASTNodeType.MethodDeclaration:
                this.visitMethodDeclarationNode(node as MethodDeclarationNode);
                break;
            case ASTNodeType.ProcedureStatement:
                this.visitProcedureStatementNode(node as ProcedureStatementNode);
                break;
            case ASTNodeType.IfStatement:
                this.visitIfStatementNode(node as IfStatementNode);
                break;
            case ASTNodeType.WhileLoop:
                this.visitWhileLoopNode(node as WhileLoopNode);
                break;
            case ASTNodeType.ForLoop:
                this.visitForLoopNode(node as ForLoopNode);
                break;
            case ASTNodeType.Assignment:
                this.visitAssignmentNode(node as AssignmentNode);
                break;
            case ASTNodeType.BinaryExpression:
                this.visitBinaryExpressionNode(node as BinaryExpressionNode);
                break;
            case ASTNodeType.UnaryExpression:
                this.visitUnaryExpressionNode(node as UnaryExpressionNode);
                break;
            case ASTNodeType.LiteralExpression:
                this.visitLiteralExpressionNode(node as LiteralExpressionNode);
                break;
            case ASTNodeType.StringLiteral:
                this.visitStringLiteralNode(node as StringLiteralNode);
                break;
            case ASTNodeType.NumberLiteral:
                this.visitNumberLiteralNode(node as NumberLiteralNode);
                break;
            case ASTNodeType.BooleanLiteral:
                this.visitBooleanLiteralNode(node as BooleanLiteralNode);
                break;
            case ASTNodeType.ArrayLiteral:
                this.visitArrayLiteralNode(node as ArrayLiteralNode);
                break;
            case ASTNodeType.NilLiteral:
                this.visitNilLiteralNode(node as NilLiteralNode);
                break;
            case ASTNodeType.DateLiteral:
                this.visitDateLiteralNode(node as DateLiteralNode);
                break;
            case ASTNodeType.CodeBlockLiteral:
                this.visitCodeBlockLiteralNode(node as CodeBlockLiteralNode);
                break;
            case ASTNodeType.VariableAccess:
                this.visitVariableAccessNode(node as VariableAccessNode);
                break;
            case ASTNodeType.PropertyAccess:
                this.visitPropertyAccessNode(node as PropertyAccessNode);
                break;
            case ASTNodeType.ArrayAccess:
                this.visitArrayAccessNode(node as ArrayAccessNode);
                break;
            case ASTNodeType.DirectFunctionCall:
                this.visitDirectFunctionCallNode(node as DirectFunctionCallNode);
                break;
            case ASTNodeType.DoProcCall:
                this.visitDoProcCallNode(node as DoProcCallNode);
                break;
            case ASTNodeType.ExecFunctionCall:
                this.visitExecFunctionCallNode(node as ExecFunctionCallNode);
                break;
            case ASTNodeType.ReturnStatement:
                this.visitReturnStatementNode(node as ReturnStatementNode);
                break;
            case ASTNodeType.DeclareStatement:
                this.visitDeclareStatementNode(node as DeclareStatementNode);
                break;
            case ASTNodeType.ParametersStatement:
                this.visitParametersStatementNode(node as ParametersStatementNode);
                break;
            case ASTNodeType.PublicStatement:
                this.visitPublicStatementNode(node as PublicStatementNode);
                break;
            case ASTNodeType.IncludeStatement:
                this.visitIncludeStatementNode(node as IncludeStatementNode);
                break;
            case ASTNodeType.CommentStatement:
                this.visitCommentStatementNode(node as CommentStatementNode);
                break;
            case ASTNodeType.SingleLineComment:
                this.visitSingleLineCommentNode(node as SingleLineCommentNode);
                break;
            case ASTNodeType.IdentifierList:
                this.visitIdentifierListNode(node as IdentifierListNode);
                break;
            case ASTNodeType.ExpressionList:
                this.visitExpressionListNode(node as ExpressionListNode);
                break;
            case ASTNodeType.ArgumentList:
                this.visitArgumentListNode(node as ArgumentListNode);
                break;
            case ASTNodeType.DefaultParameterList:
                this.visitDefaultParameterListNode(node as DefaultParameterListNode);
                break;
            case ASTNodeType.ParameterDeclaration:
                this.visitParameterDeclarationNode(node as ParameterDeclarationNode);
                break;
            case ASTNodeType.DefaultParameterDeclaration:
                this.visitDefaultParameterDeclarationNode(node as DefaultParameterDeclarationNode);
                break;
            case ASTNodeType.LabelStatement:
                this.visitLabelStatementNode(node as LabelStatementNode);
                break;
            case ASTNodeType.RegionBlock:
                this.visitRegionBlockNode(node as RegionBlockNode);
                break;
            case ASTNodeType.InlineCodeBlock:
                this.visitInlineCodeBlockNode(node as InlineCodeBlockNode);
                break;
            case ASTNodeType.BranchStatement:
                this.visitBranchStatementNode(node as BranchStatementNode);
                break;
            case ASTNodeType.SqlExecute:
                this.visitSqlExecuteNode(node as SqlExecuteNode);
                break;
            case ASTNodeType.LSearch:
                this.visitLSearchNode(node as LSearchNode);
                break;
            default:
                // Handle unknown node types gracefully
                console.warn(`Unknown AST node type: ${node.kind}`);
                break;
        }
    }

    // ===== Program and Top-Level Nodes =====

    private visitProgramNode(node: ProgramNode): void {
        let previousWasProcedure = false;

        for (let i = 0; i < node.body.length; i++) {
            const currentNode = node.body[i];
            const currentIsProcedure = currentNode.kind === ASTNodeType.ProcedureStatement;

            // Add blank lines between procedures if configured
            if (
                this.options.blankLinesBetweenProcedures &&
                i > 0 &&
                (previousWasProcedure || currentIsProcedure)
            ) {
                this.output.blankLines(1);
            }

            this.visit(currentNode);
            previousWasProcedure = currentIsProcedure;
        }
    }

    // ===== Class Definition Nodes =====

    private visitClassDefinitionNode(node: ClassDefinitionNode): void {
        this.visit(node.declaration);
        if (node.inherit) {
            this.output.newLine(this.indentationLevel);
            this.visit(node.inherit);
        }

        this.indentationLevel++;
        for (const member of node.members) {
            this.output.newLine(this.indentationLevel);
            this.visit(member);
        }
        this.indentationLevel--;
    }

    private visitClassDeclarationNode(node: ClassDeclarationNode): void {
        this.output.append(`:CLASS ${node.name.value};`);
    }

    private visitInheritStatementNode(node: InheritStatementNode): void {
        this.output.append(`:INHERIT ${node.className.value};`);
    }

    private visitClassFieldDeclarationNode(node: ClassFieldDeclarationNode): void {
        this.output.append(":DECLARE ");
        this.visit(node.identifiers);
        this.output.append(";");
    }

    private visitMethodDeclarationNode(node: MethodDeclarationNode): void {
        this.visit(node.procedure);
    }

    // ===== Procedure Nodes =====

    private visitProcedureStatementNode(node: ProcedureStatementNode): void {
        this.output.append(`:PROCEDURE ${node.name.value};`);

        // Parameters and default parameters
        if (node.parameters) {
            this.output.newLine(this.indentationLevel);
            this.visit(node.parameters);
        }
        if (node.defaultParameters) {
            this.output.newLine(this.indentationLevel);
            this.visit(node.defaultParameters);
        }

        // Procedure body
        this.indentationLevel++;
        for (const statement of node.body) {
            this.output.newLine(this.indentationLevel);
            this.visit(statement);
        }
        this.indentationLevel--;

        this.output.newLine(this.indentationLevel);
        this.output.append(":ENDPROC;");
    }

    private visitParameterDeclarationNode(node: ParameterDeclarationNode): void {
        this.output.append(":PARAMETERS ");
        this.visit(node.parameters);
        this.output.append(";");
    }

    private visitDefaultParameterDeclarationNode(node: DefaultParameterDeclarationNode): void {
        this.output.append(":DEFAULT ");
        this.visit(node.defaults);
        this.output.append(";");
    }

    // ===== Control Flow Nodes =====

    private visitIfStatementNode(node: IfStatementNode): void {
        this.output.append(":IF ");
        this.visit(node.condition);
        this.output.append(";");

        // Then branch
        this.indentationLevel++;
        for (const statement of node.thenBranch) {
            this.output.newLine(this.indentationLevel);
            this.visit(statement);
        }
        this.indentationLevel--;

        // Else branch
        if (node.elseBranch && node.elseBranch.length > 0) {
            this.output.newLine(this.indentationLevel);
            this.output.append(":ELSE;");
            this.indentationLevel++;
            for (const statement of node.elseBranch) {
                this.output.newLine(this.indentationLevel);
                this.visit(statement);
            }
            this.indentationLevel--;
        }

        this.output.newLine(this.indentationLevel);
        this.output.append(":ENDIF;");
    }

    private visitWhileLoopNode(node: WhileLoopNode): void {
        this.output.append(":WHILE ");
        this.visit(node.condition);
        this.output.append(";");

        this.indentationLevel++;
        for (const statement of node.body) {
            this.output.newLine(this.indentationLevel);
            this.visit(statement);
        }
        this.indentationLevel--;

        this.output.newLine(this.indentationLevel);
        this.output.append(":ENDWHILE;");
    }

    private visitForLoopNode(node: ForLoopNode): void {
        this.output.append(`:FOR ${node.variable.value} := `);
        this.visit(node.from);
        this.output.append(" :TO ");
        this.visit(node.to);
        this.output.append(";");

        this.indentationLevel++;
        for (const statement of node.body) {
            this.output.newLine(this.indentationLevel);
            this.visit(statement);
        }
        this.indentationLevel--;

        this.output.newLine(this.indentationLevel);
        this.output.append(":NEXT;");
    }

    // ===== Expression Nodes =====

    private visitBinaryExpressionNode(node: BinaryExpressionNode): void {
        this.visit(node.left);
        this.output.append(` ${node.operator.value} `);
        this.visit(node.right);
    }

    private visitUnaryExpressionNode(node: UnaryExpressionNode): void {
        this.output.append(node.operator.value);
        this.visit(node.operand);
    }

    private visitAssignmentNode(node: AssignmentNode): void {
        this.visit(node.left);
        this.output.append(` ${node.operator.value} `);
        this.visit(node.right);
        this.output.append(";");
    }

    // ===== Literal Nodes =====

    private visitLiteralExpressionNode(node: LiteralExpressionNode): void {
        this.output.append(node.token.value);
    }

    private visitStringLiteralNode(node: StringLiteralNode): void {
        this.output.append(node.token.value);
    }

    private visitNumberLiteralNode(node: NumberLiteralNode): void {
        this.output.append(node.token.value);
    }

    private visitBooleanLiteralNode(node: BooleanLiteralNode): void {
        this.output.append(node.token.value);
    }
    private visitArrayLiteralNode(node: ArrayLiteralNode): void {
        this.output.append("{");
        for (let i = 0; i < node.elements.length; i++) {
            if (i > 0) {
                this.output.append(", ");
            }
            this.visit(node.elements[i]);
        }
        this.output.append("}");
    }

    private visitNilLiteralNode(node: NilLiteralNode): void {
        this.output.append(node.token.value);
    }
    private visitDateLiteralNode(node: DateLiteralNode): void {
        this.output.append("{");
        for (let i = 0; i < node.components.length; i++) {
            if (i > 0) {
                this.output.append(", ");
            }
            this.visit(node.components[i]);
        }
        this.output.append("}");
    }

    private visitCodeBlockLiteralNode(node: CodeBlockLiteralNode): void {
        this.output.append("{");
        if (node.parameters) {
            this.output.append("|");
            this.visit(node.parameters);
            this.output.append("| ");
        } else {
            this.output.append("|| ");
        }
        for (let i = 0; i < node.body.length; i++) {
            if (i > 0) {
                this.output.append("; ");
            }
            this.visit(node.body[i]);
        }
        this.output.append("}");
    }

    // ===== Access Nodes =====

    private visitVariableAccessNode(node: VariableAccessNode): void {
        this.output.append(node.name.value);
    }
    private visitPropertyAccessNode(node: PropertyAccessNode): void {
        // Handle Token type for object (PropertyAccessNode.object is Token, not ASTNode)
        this.output.append(node.object.value);
        this.output.append(":");
        this.output.append(node.property.value);
    }
    private visitArrayAccessNode(node: ArrayAccessNode): void {
        // Handle both Token and ASTNode types for array
        if (typeof node.array === "object" && "kind" in node.array) {
            this.visit(node.array as ASTNode);
        } else {
            this.output.append((node.array as Token).value);
        }
        this.output.append("[");
        for (let i = 0; i < node.indices.length; i++) {
            if (i > 0) {
                this.output.append(", ");
            }
            this.visit(node.indices[i]);
        }
        this.output.append("]");
    }

    // ===== Function Call Nodes =====

    private visitDirectFunctionCallNode(node: DirectFunctionCallNode): void {
        this.output.append(node.name.value);
        this.output.append("(");
        if (node.arguments) {
            this.visit(node.arguments);
        }
        this.output.append(")");
    }

    private visitDoProcCallNode(node: DoProcCallNode): void {
        this.output.append("DoProc(");
        this.visit(node.procedureName);
        if (node.arguments) {
            this.output.append(", ");
            this.visit(node.arguments);
        }
        this.output.append(")");
    }

    private visitExecFunctionCallNode(node: ExecFunctionCallNode): void {
        this.output.append("ExecUDF(");
        this.visit(node.functionName);
        if (node.arguments) {
            this.output.append(", ");
            this.visit(node.arguments);
        }
        this.output.append(")");
    }

    // ===== Statement Nodes =====

    private visitReturnStatementNode(node: ReturnStatementNode): void {
        this.output.append(":RETURN");
        if (node.value) {
            this.output.append(" ");
            this.visit(node.value);
        }
        this.output.append(";");
    }

    private visitDeclareStatementNode(node: DeclareStatementNode): void {
        this.output.append(":DECLARE ");
        this.visit(node.identifiers);
        this.output.append(";");
    }

    private visitParametersStatementNode(node: ParametersStatementNode): void {
        this.output.append(":PARAMETERS ");
        this.visit(node.identifiers);
        this.output.append(";");
    }

    private visitPublicStatementNode(node: PublicStatementNode): void {
        this.output.append(":PUBLIC ");
        this.visit(node.identifiers);
        this.output.append(";");
    }

    private visitIncludeStatementNode(node: IncludeStatementNode): void {
        this.output.append(":INCLUDE ");
        this.visit(node.path);
        this.output.append(";");
    }

    // ===== Comment Nodes =====

    private visitCommentStatementNode(node: CommentStatementNode): void {
        this.output.append(`/* ${node.content} ;`);
    }

    private visitSingleLineCommentNode(node: SingleLineCommentNode): void {
        this.output.append(`/* ${node.content} ;`);
    }

    // ===== List Nodes =====
    private visitIdentifierListNode(node: IdentifierListNode): void {
        for (let i = 0; i < node.identifiers.length; i++) {
            if (i > 0) {
                this.output.append(", ");
            }
            this.output.append(node.identifiers[i].value);
        }
    }
    private visitExpressionListNode(node: ExpressionListNode): void {
        for (let i = 0; i < node.expressions.length; i++) {
            if (i > 0) {
                this.output.append(", ");
            }
            this.visit(node.expressions[i]);
        }
    }
    private visitArgumentListNode(node: ArgumentListNode): void {
        for (let i = 0; i < node.arguments.length; i++) {
            if (i > 0) {
                this.output.append(", ");
            }
            this.visit(node.arguments[i]);
        }
    }
    private visitDefaultParameterListNode(node: DefaultParameterListNode): void {
        for (let i = 0; i < node.pairs.length; i++) {
            if (i > 0) {
                this.output.append(", ");
            }
            this.output.append(node.pairs[i].identifier.value);
            this.output.append(", ");
            this.visit(node.pairs[i].defaultValue);
        }
    }

    // ===== Special Structure Nodes =====

    private visitLabelStatementNode(node: LabelStatementNode): void {
        this.output.append(`:LABEL ${node.name.value};`);
    }

    private visitRegionBlockNode(node: RegionBlockNode): void {
        this.output.append(`:REGION ${node.name.value};`);

        this.indentationLevel++;
        for (const statement of node.body) {
            this.output.newLine(this.indentationLevel);
            this.visit(statement);
        }
        this.indentationLevel--;

        this.output.newLine(this.indentationLevel);
        this.output.append(":ENDREGION;");
    }
    private visitInlineCodeBlockNode(node: InlineCodeBlockNode): void {
        this.output.append(":BEGININLINECODE");
        if (node.language) {
            this.output.append(" ");
            // Handle both Token and StringLiteralNode types for language
            if (typeof node.language === "object" && "kind" in node.language) {
                this.visit(node.language as ASTNode);
            } else {
                this.output.append((node.language as Token).value);
            }
        }
        this.output.append(";");

        this.indentationLevel++;
        for (const statement of node.body) {
            this.output.newLine(this.indentationLevel);
            this.visit(statement);
        }
        this.indentationLevel--;

        this.output.newLine(this.indentationLevel);
        this.output.append(":ENDINLINECODE;");
    }

    private visitBranchStatementNode(node: BranchStatementNode): void {
        this.output.append("Branch(");
        this.visit(node.target);
        this.output.append(")");
    }

    // ===== SQL Integration Nodes =====

    private visitSqlExecuteNode(node: SqlExecuteNode): void {
        this.output.append("SqlExecute(");
        this.visit(node.query);
        if (node.parameters) {
            this.output.append(", ");
            this.visit(node.parameters);
        }
        this.output.append(")");
    }

    private visitLSearchNode(node: LSearchNode): void {
        this.output.append("LSearch(");
        this.visit(node.query);
        if (node.parameter1) {
            this.output.append(", ");
            this.visit(node.parameter1);
        }
        if (node.parameter2) {
            this.output.append(", ");
            this.visit(node.parameter2);
        }
        if (node.parameters) {
            this.output.append(", ");
            this.visit(node.parameters);
        }
        this.output.append(")");
    }
}
