import { Token } from "../../tokenizer/token";

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
    ProcedureStart = "ProcedureStart",
    ProcedureEnd = "ProcedureEnd",
    ParameterDeclaration = "ParameterDeclaration",
    DefaultParameterDeclaration = "DefaultParameterDeclaration",
    DefaultParameterList = "DefaultParameterList",
    ParameterList = "ParameterList",

    // Control flow statements
    ConditionalStatement = "ConditionalStatement",
    IfStatement = "IfStatement",
    ElseStatement = "ElseStatement",
    EndIfStatement = "EndIfStatement",
    LoopStatement = "LoopStatement",
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
    ErrorHandlingStatement = "ErrorHandlingStatement",
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
    DeclarationStatement = "DeclarationStatement",
    ParametersStatement = "ParametersStatement",
    DeclareStatement = "DeclareStatement",
    DefaultStatement = "DefaultStatement",
    PublicStatement = "PublicStatement",
    IncludeStatement = "IncludeStatement",

    // Logic statements
    LogicStatement = "LogicStatement",
    Assignment = "Assignment",
    ReturnStatement = "ReturnStatement",

    // Function calls
    FunctionCall = "FunctionCall",
    DirectFunctionCall = "DirectFunctionCall",
    DoProcCall = "DoProcCall",
    ExecFunctionCall = "ExecFunctionCall",

    // Comments
    CommentStatement = "CommentStatement",
    BlockComment = "BlockComment",
    SingleLineComment = "SingleLineComment",
    RegionComment = "RegionComment",
    EndRegionComment = "EndRegionComment",

    // Special structures
    LabelStatement = "LabelStatement",
    RegionBlock = "RegionBlock",
    RegionStart = "RegionStart",
    RegionEnd = "RegionEnd",
    InlineCodeBlock = "InlineCodeBlock",
    InlineCodeStart = "InlineCodeStart",
    InlineCodeEnd = "InlineCodeEnd",
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

    // Additional types from grammar (often part of other nodes or tokens)
    AssignmentOperator = "AssignmentOperator",
    LogicalOperator = "LogicalOperator",
    ComparisonOperator = "ComparisonOperator",
    AdditiveOperator = "AdditiveOperator",
    MultiplicativeOperator = "MultiplicativeOperator",
    UnaryOperator = "UnaryOperator",
    ArraySubscript = "ArraySubscript",
    Literal = "Literal",
    IntegerPart = "IntegerPart",
    DecimalPart = "DecimalPart",
    Exponent = "Exponent",
    Character = "Character",
    Symbol = "Symbol",
    Letter = "Letter",
    Digit = "Digit",
}

// Base types for commonly used node categories
export type StatementNode = ASTNode;
export type ExpressionNode = ASTNode;
export type LiteralNode = ASTNode;

/**
 * Utility functions for creating AST nodes
 */
export function createBaseNode(kind: ASTNodeType, startToken: Token, endToken: Token): ASTNode {
    return { kind, startToken, endToken };
}
