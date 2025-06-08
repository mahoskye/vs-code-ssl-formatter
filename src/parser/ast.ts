/**
 * Abstract Syntax Tree (AST) node definitions for SSL language
 * Based on the SSL EBNF grammar specification
 */

import { Token } from "../tokenizer/token";

/**
 * Base interface for all AST nodes
 */
export interface ASTNode {
    kind: ASTNodeType;
    startToken: Token;
    endToken: Token;
}

/**
 * Enum for all AST node types
 */
export enum ASTNodeType {
    // Top-level
    Program = "Program",

    // Class definitions
    ClassDefinition = "ClassDefinition",
    ClassDeclaration = "ClassDeclaration",
    InheritStatement = "InheritStatement",
    ClassFieldDeclaration = "ClassFieldDeclaration",
    MethodDeclaration = "MethodDeclaration",

    // Procedure declarations
    ProcedureStatement = "ProcedureStatement",
    ParameterDeclaration = "ParameterDeclaration",
    DefaultParameterDeclaration = "DefaultParameterDeclaration",

    // Control flow statements
    IfStatement = "IfStatement",
    ElseStatement = "ElseStatement",
    EndIfStatement = "EndIfStatement",
    WhileLoop = "WhileLoop",
    WhileStatement = "WhileStatement",
    EndWhileStatement = "EndWhileStatement",
    ForLoop = "ForLoop",
    ForStatement = "ForStatement",
    NextStatement = "NextStatement",
    ExitWhileStatement = "ExitWhileStatement",
    ExitForStatement = "ExitForStatement",
    LoopContinue = "LoopContinue",

    // Switch case statements
    SwitchStatement = "SwitchStatement",
    BeginCaseStatement = "BeginCaseStatement",
    CaseBlock = "CaseBlock",
    CaseStatement = "CaseStatement",
    OtherwiseBlock = "OtherwiseBlock",
    OtherwiseStatement = "OtherwiseStatement",
    EndCaseStatement = "EndCaseStatement",
    ExitCaseStatement = "ExitCaseStatement",

    // Error handling
    TryBlock = "TryBlock",
    TryStatement = "TryStatement",
    CatchBlock = "CatchBlock",
    CatchStatement = "CatchStatement",
    FinallyBlock = "FinallyBlock",
    FinallyStatement = "FinallyStatement",
    EndTryStatement = "EndTryStatement",
    ErrorBlockStanza = "ErrorBlockStanza",
    ErrorMarker = "ErrorMarker",

    // Declaration statements
    ParametersStatement = "ParametersStatement",
    DeclareStatement = "DeclareStatement",
    DefaultStatement = "DefaultStatement",
    PublicStatement = "PublicStatement",
    IncludeStatement = "IncludeStatement",

    // Logic statements
    Assignment = "Assignment",
    ReturnStatement = "ReturnStatement",

    // Function calls
    FunctionCall = "FunctionCall",
    DirectFunctionCall = "DirectFunctionCall",
    DoProcCall = "DoProcCall",
    ExecFunctionCall = "ExecFunctionCall",

    // Comments
    BlockComment = "BlockComment",
    SingleLineComment = "SingleLineComment",
    RegionComment = "RegionComment",
    EndRegionComment = "EndRegionComment",

    // Special structures
    LabelStatement = "LabelStatement",
    RegionBlock = "RegionBlock",
    InlineCodeBlock = "InlineCodeBlock",
    DynamicCodeExecution = "DynamicCodeExecution",
    BranchStatement = "BranchStatement",

    // SQL Integration
    SqlStatement = "SqlStatement",
    SqlExecute = "SqlExecute",
    LSearch = "LSearch",
    SqlParameter = "SqlParameter",

    // Object-oriented statements
    ObjectCreation = "ObjectCreation",
    MethodCall = "MethodCall",
    ObjectPropertyAccess = "ObjectPropertyAccess",

    // Expressions
    Expression = "Expression",
    LogicalExpression = "LogicalExpression",
    ComparisonExpression = "ComparisonExpression",
    ArithmeticExpression = "ArithmeticExpression",
    Term = "Term",
    Factor = "Factor",
    PowerOperand = "PowerOperand",
    Primary = "Primary",
    BinaryExpression = "BinaryExpression",
    UnaryExpression = "UnaryExpression",
    IncrementExpression = "IncrementExpression",
    VariableAccess = "VariableAccess",
    PropertyAccess = "PropertyAccess",
    ArrayAccess = "ArrayAccess",

    // Literals
    LiteralExpression = "LiteralExpression",
    NumberLiteral = "NumberLiteral",
    StringLiteral = "StringLiteral",
    BooleanLiteral = "BooleanLiteral",
    ArrayLiteral = "ArrayLiteral",
    NilLiteral = "NilLiteral",
    DateLiteral = "DateLiteral",
    CodeBlockLiteral = "CodeBlockLiteral",

    // Lists
    IdentifierList = "IdentifierList",
    ExpressionList = "ExpressionList",
    ArgumentList = "ArgumentList",

    // Bitwise operations (as functions)
    BitwiseOperation = "BitwiseOperation",
}

// Base types for commonly used node categories
export type StatementNode = ASTNode;
export type ExpressionNode = ASTNode;
export type LiteralNode = ASTNode;

/**
 * Top-level program node
 */
export interface ProgramNode extends ASTNode {
    kind: ASTNodeType.Program;
    body: (ClassDefinitionNode | StatementNode)[];
}

/**
 * Class definition nodes
 */
export interface ClassDefinitionNode extends ASTNode {
    kind: ASTNodeType.ClassDefinition;
    declaration: ClassDeclarationNode;
    inherit?: InheritStatementNode;
    members: (ClassFieldDeclarationNode | MethodDeclarationNode)[];
}

export interface ClassDeclarationNode extends ASTNode {
    kind: ASTNodeType.ClassDeclaration;
    name: Token;
}

export interface InheritStatementNode extends ASTNode {
    kind: ASTNodeType.InheritStatement;
    className: Token;
}

export interface ClassFieldDeclarationNode extends ASTNode {
    kind: ASTNodeType.ClassFieldDeclaration;
    identifiers: IdentifierListNode;
}

export interface MethodDeclarationNode extends ASTNode {
    kind: ASTNodeType.MethodDeclaration;
    procedure: ProcedureStatementNode;
}

/**
 * Procedure statement node
 */
export interface ProcedureStatementNode extends ASTNode {
    kind: ASTNodeType.ProcedureStatement;
    name: Token;
    parameters?: ParameterDeclarationNode;
    defaultParameters?: DefaultParameterDeclarationNode;
    body: StatementNode[];
}

export interface ParameterDeclarationNode extends ASTNode {
    kind: ASTNodeType.ParameterDeclaration;
    parameters: IdentifierListNode;
}

export interface DefaultParameterDeclarationNode extends ASTNode {
    kind: ASTNodeType.DefaultParameterDeclaration;
    defaults: DefaultParameterListNode;
}

/**
 * Control flow statement nodes
 */
export interface IfStatementNode extends ASTNode {
    kind: ASTNodeType.IfStatement;
    condition: ExpressionNode;
    thenBranch: StatementNode[];
    elseBranch?: StatementNode[];
}

export interface WhileLoopNode extends ASTNode {
    kind: ASTNodeType.WhileLoop;
    condition: ExpressionNode;
    body: StatementNode[];
}

export interface ForLoopNode extends ASTNode {
    kind: ASTNodeType.ForLoop;
    variable: Token;
    from: ExpressionNode;
    to: ExpressionNode;
    body: StatementNode[];
}

export interface ExitWhileStatementNode extends ASTNode {
    kind: ASTNodeType.ExitWhileStatement;
}

export interface ExitForStatementNode extends ASTNode {
    kind: ASTNodeType.ExitForStatement;
}

export interface LoopContinueNode extends ASTNode {
    kind: ASTNodeType.LoopContinue;
}

/**
 * Switch case statement nodes
 */
export interface SwitchStatementNode extends ASTNode {
    kind: ASTNodeType.SwitchStatement;
    cases: CaseBlockNode[];
    otherwise?: OtherwiseBlockNode;
}

export interface CaseBlockNode extends ASTNode {
    kind: ASTNodeType.CaseBlock;
    condition: ExpressionNode;
    statements: StatementNode[];
    hasExit?: boolean;
}

export interface OtherwiseBlockNode extends ASTNode {
    kind: ASTNodeType.OtherwiseBlock;
    statements: StatementNode[];
}

/**
 * Error handling nodes
 */
export interface TryBlockNode extends ASTNode {
    kind: ASTNodeType.TryBlock;
    tryStatements: StatementNode[];
    catchBlock: CatchBlockNode;
    finallyBlock?: FinallyBlockNode;
}

export interface CatchBlockNode extends ASTNode {
    kind: ASTNodeType.CatchBlock;
    statements: StatementNode[];
}

export interface FinallyBlockNode extends ASTNode {
    kind: ASTNodeType.FinallyBlock;
    statements: StatementNode[];
}

export interface ErrorBlockStanzaNode extends ASTNode {
    kind: ASTNodeType.ErrorBlockStanza;
    statements: StatementNode[];
}

/**
 * Declaration statement nodes
 */
export interface ParametersStatementNode extends ASTNode {
    kind: ASTNodeType.ParametersStatement;
    identifiers: IdentifierListNode;
}

export interface DeclareStatementNode extends ASTNode {
    kind: ASTNodeType.DeclareStatement;
    identifiers: IdentifierListNode;
}

export interface PublicStatementNode extends ASTNode {
    kind: ASTNodeType.PublicStatement;
    identifiers: IdentifierListNode;
}

export interface IncludeStatementNode extends ASTNode {
    kind: ASTNodeType.IncludeStatement;
    path: StringLiteralNode;
}

/**
 * Logic statement nodes
 */
export interface AssignmentNode extends ASTNode {
    kind: ASTNodeType.Assignment;
    left: VariableAccessNode | PropertyAccessNode;
    operator: Token;
    right: ExpressionNode;
}

export interface ReturnStatementNode extends ASTNode {
    kind: ASTNodeType.ReturnStatement;
    value?: ExpressionNode;
}

/**
 * Function call nodes
 */
export interface DirectFunctionCallNode extends ASTNode {
    kind: ASTNodeType.DirectFunctionCall;
    name: Token;
    arguments: ArgumentListNode;
}

export interface DoProcCallNode extends ASTNode {
    kind: ASTNodeType.DoProcCall;
    procedureName: StringLiteralNode;
    arguments: ArrayLiteralNode;
}

export interface ExecFunctionCallNode extends ASTNode {
    kind: ASTNodeType.ExecFunctionCall;
    functionName: StringLiteralNode;
    arguments: ArrayLiteralNode;
}

/**
 * Comment nodes
 */
export interface BlockCommentNode extends ASTNode {
    kind: ASTNodeType.BlockComment;
    content: string;
}

export interface RegionCommentNode extends ASTNode {
    kind: ASTNodeType.RegionComment;
    name: string;
}

export interface EndRegionCommentNode extends ASTNode {
    kind: ASTNodeType.EndRegionComment;
    name?: string;
}

/**
 * Special structure nodes
 */
export interface LabelStatementNode extends ASTNode {
    kind: ASTNodeType.LabelStatement;
    name: Token;
}

export interface RegionBlockNode extends ASTNode {
    kind: ASTNodeType.RegionBlock;
    name: Token;
    body: StatementNode[];
}

export interface InlineCodeBlockNode extends ASTNode {
    kind: ASTNodeType.InlineCodeBlock;
    language?: StringLiteralNode | Token;
    body: StatementNode[];
}

export interface BranchStatementNode extends ASTNode {
    kind: ASTNodeType.BranchStatement;
    target: StringLiteralNode;
}

/**
 * SQL integration nodes
 */
export interface SqlExecuteNode extends ASTNode {
    kind: ASTNodeType.SqlExecute;
    query: StringLiteralNode;
    parameters?: ArrayLiteralNode;
}

export interface LSearchNode extends ASTNode {
    kind: ASTNodeType.LSearch;
    query: StringLiteralNode;
    parameter1?: ExpressionNode;
    parameter2?: ExpressionNode;
    parameters?: ArrayLiteralNode;
}

/**
 * Object-oriented nodes
 */
export interface ObjectCreationNode extends ASTNode {
    kind: ASTNodeType.ObjectCreation;
    className?: StringLiteralNode;
}

export interface MethodCallNode extends ASTNode {
    kind: ASTNodeType.MethodCall;
    object: Token;
    method: Token;
    arguments?: ArgumentListNode;
}

export interface PropertyAccessNode extends ASTNode {
    kind: ASTNodeType.PropertyAccess;
    object: Token;
    property: Token;
}

/**
 * Expression nodes
 */
export interface BinaryExpressionNode extends ASTNode {
    kind: ASTNodeType.BinaryExpression;
    left: ExpressionNode;
    operator: Token;
    right: ExpressionNode;
}

export interface UnaryExpressionNode extends ASTNode {
    kind: ASTNodeType.UnaryExpression;
    operator: Token;
    operand: ExpressionNode;
}

export interface IncrementExpressionNode extends ASTNode {
    kind: ASTNodeType.IncrementExpression;
    operator: Token; // ++ or --
    operand: Token; // identifier
    prefix: boolean; // true for ++x, false for x++
}

export interface VariableAccessNode extends ASTNode {
    kind: ASTNodeType.VariableAccess;
    name: Token;
}

export interface ArrayAccessNode extends ASTNode {
    kind: ASTNodeType.ArrayAccess;
    array: Token;
    indices: ExpressionNode[];
}

/**
 * Literal nodes
 */
export interface LiteralExpressionNode extends ASTNode {
    kind: ASTNodeType.LiteralExpression;
    value: any;
    token: Token;
}

export interface NumberLiteralNode extends ASTNode {
    kind: ASTNodeType.NumberLiteral;
    value: number;
    token: Token;
}

export interface StringLiteralNode extends ASTNode {
    kind: ASTNodeType.StringLiteral;
    value: string;
    token: Token;
}

export interface BooleanLiteralNode extends ASTNode {
    kind: ASTNodeType.BooleanLiteral;
    value: boolean;
    token: Token;
}

export interface ArrayLiteralNode extends ASTNode {
    kind: ASTNodeType.ArrayLiteral;
    elements: ExpressionNode[];
}

export interface NilLiteralNode extends ASTNode {
    kind: ASTNodeType.NilLiteral;
    token: Token;
}

export interface DateLiteralNode extends ASTNode {
    kind: ASTNodeType.DateLiteral;
    components: ExpressionNode[]; // year, month, day, [hour, minute, second]
}

export interface CodeBlockLiteralNode extends ASTNode {
    kind: ASTNodeType.CodeBlockLiteral;
    parameters?: IdentifierListNode;
    body: ExpressionNode[];
}

/**
 * List nodes
 */
export interface IdentifierListNode extends ASTNode {
    kind: ASTNodeType.IdentifierList;
    identifiers: Token[];
}

export interface ExpressionListNode extends ASTNode {
    kind: ASTNodeType.ExpressionList;
    expressions: ExpressionNode[];
}

export interface ArgumentListNode extends ASTNode {
    kind: ASTNodeType.ArgumentList;
    arguments: ExpressionNode[];
}

export interface DefaultParameterListNode extends ASTNode {
    kind: ASTNodeType.IdentifierList; // Reuse IdentifierList for now
    pairs: { identifier: Token; defaultValue: ExpressionNode }[];
}

/**
 * Bitwise operation node (functions, not operators)
 */
export interface BitwiseOperationNode extends ASTNode {
    kind: ASTNodeType.BitwiseOperation;
    operation: "_AND" | "_OR" | "_NOT";
    operands: ExpressionNode[];
}

/**
 * Utility functions for creating AST nodes
 */
export function createBaseNode(kind: ASTNodeType, startToken: Token, endToken: Token): ASTNode {
    return {
        kind,
        startToken,
        endToken,
    };
}
