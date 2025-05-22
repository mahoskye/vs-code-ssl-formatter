/**
 * SSL Abstract Syntax Tree (AST) nodes
 * Based on the EBNF grammar
 */

/**
 * Represents a position in a document
 */
export interface Position {
    line: number;
    character: number;
}

/**
 * Represents a range in a document
 */
export interface Range {
    start: Position;
    end: Position;
}

/**
 * Base interface for all AST nodes
 */
export interface ASTNode {
    type: string;
    range: Range;
}

/**
 * Represents an entire SSL program
 */
export interface Program extends ASTNode {
    type: "Program";
    body: Statement[];
    isClassDefinition: boolean;
}

/**
 * Base interface for all statements
 */
export interface Statement extends ASTNode {
    type: string;
}

/**
 * Represents a procedure declaration
 */
export interface ProcedureStatement extends Statement {
    type: "ProcedureStatement";
    name: string;
    parameters: ParameterDeclaration | null;
    defaultParameters: DefaultParameterDeclaration | null;
    body: Statement[];
}

/**
 * Represents a parameter declaration
 */
export interface ParameterDeclaration extends Statement {
    type: "ParameterDeclaration";
    parameters: string[];
}

/**
 * Represents a default parameter declaration
 */
export interface DefaultParameterDeclaration extends Statement {
    type: "DefaultParameterDeclaration";
    parameters: Array<{ name: string; value: Expression }>;
}

/**
 * Represents an if statement
 */
export interface IfStatement extends Statement {
    type: "IfStatement";
    condition: Expression;
    thenBranch: Statement[];
    elseBranch: Statement[] | null;
}

/**
 * Represents a while loop
 */
export interface WhileStatement extends Statement {
    type: "WhileStatement";
    condition: Expression;
    body: Statement[];
}

/**
 * Represents a for loop
 */
export interface ForStatement extends Statement {
    type: "ForStatement";
    variable: string;
    initialValue: Expression;
    finalValue: Expression;
    body: Statement[];
}

/**
 * Represents an exit statement (EXITWHILE, EXITFOR, etc.)
 */
export interface ExitStatement extends Statement {
    type: "ExitStatement";
    exitType: "WHILE" | "FOR" | "CASE";
}

/**
 * Represents a continue statement (LOOP)
 */
export interface LoopContinueStatement extends Statement {
    type: "LoopContinueStatement";
}

/**
 * Represents a switch case statement
 */
export interface SwitchStatement extends Statement {
    type: "SwitchStatement";
    cases: Array<{
        condition: Expression;
        body: Statement[];
    }>;
    defaultCase: Statement[] | null;
}

/**
 * Represents a try-catch-finally block
 */
export interface TryStatement extends Statement {
    type: "TryStatement";
    tryBlock: Statement[];
    catchBlock: Statement[];
    finallyBlock: Statement[] | null;
}

/**
 * Represents an error block
 */
export interface ErrorBlockStatement extends Statement {
    type: "ErrorBlockStatement";
    body: Statement[];
}

/**
 * Represents a variable declaration
 */
export interface DeclarationStatement extends Statement {
    type: "DeclarationStatement";
    declarationType: "DECLARE" | "PUBLIC";
    identifiers: string[];
}

/**
 * Represents an include statement
 */
export interface IncludeStatement extends Statement {
    type: "IncludeStatement";
    path: string;
}

/**
 * Represents an assignment
 */
export interface AssignmentStatement extends Statement {
    type: "AssignmentStatement";
    left: VariableAccess | PropertyAccess | ArrayAccess;
    operator: string; // :=, +=, -=, *=, /=, ^=
    right: Expression;
}

/**
 * Represents a return statement
 */
export interface ReturnStatement extends Statement {
    type: "ReturnStatement";
    value: Expression | null;
}

/**
 * Represents a function call statement
 */
export interface FunctionCallStatement extends Statement {
    type: "FunctionCallStatement";
    expression: FunctionCall;
}

/**
 * Represents a comment
 */
export interface CommentStatement extends Statement {
    type: "CommentStatement";
    value: string;
    isRegionComment?: boolean;
    isEndRegionComment?: boolean;
}

/**
 * Represents a label
 */
export interface LabelStatement extends Statement {
    type: "LabelStatement";
    name: string;
}

/**
 * Represents a region block
 */
export interface RegionStatement extends Statement {
    type: "RegionStatement";
    name: string;
    body: Statement[];
}

/**
 * Represents an inline code block
 */
export interface InlineCodeStatement extends Statement {
    type: "InlineCodeStatement";
    name: string | null;
    body: Statement[];
}

/**
 * Represents a branch statement
 */
export interface BranchStatement extends Statement {
    type: "BranchStatement";
    target: string;
}

/**
 * Represents a SQL statement
 */
export interface SqlStatement extends Statement {
    type: "SqlStatement";
    sqlType: "SqlExecute" | "LSearch";
    query: string;
    parameters: Expression[] | null;
}

/**
 * Represents a class definition
 */
export interface ClassDefinition extends Statement {
    type: "ClassDefinition";
    name: string;
    inherit: string | null;
    members: Array<ClassFieldDeclaration | ProcedureStatement>;
}

/**
 * Represents a class field declaration
 */
export interface ClassFieldDeclaration extends Statement {
    type: "ClassFieldDeclaration";
    fields: string[];
}

// Expression types

/**
 * Base interface for all expressions
 */
export interface Expression extends ASTNode {
    type: string;
}

/**
 * Represents a binary expression (a + b, a * b, etc.)
 */
export interface BinaryExpression extends Expression {
    type: "BinaryExpression";
    left: Expression;
    operator: string;
    right: Expression;
}

/**
 * Represents a unary expression (-a, !a, etc.)
 */
export interface UnaryExpression extends Expression {
    type: "UnaryExpression";
    operator: string;
    argument: Expression;
}

/**
 * Represents a variable access
 */
export interface VariableAccess extends Expression {
    type: "VariableAccess";
    name: string;
}

/**
 * Represents a property access (object:property)
 */
export interface PropertyAccess extends Expression {
    type: "PropertyAccess";
    object: Expression;
    property: string;
}

/**
 * Represents an array access (array[index])
 */
export interface ArrayAccess extends Expression {
    type: "ArrayAccess";
    array: Expression;
    indices: Expression[];
    isMultiDimensional: boolean; // true for arr[1,2], false for arr[1][2]
}

/**
 * Represents a function call
 */
export interface FunctionCall extends Expression {
    type: "FunctionCall";
    callType: "Direct" | "DoProc" | "ExecFunction";
    name: string | Expression;
    arguments: Expression[];
}

/**
 * Represents a literal value
 */
export interface Literal extends Expression {
    type: "Literal";
    literalType: "String" | "Number" | "Boolean" | "Nil" | "Array" | "Date" | "CodeBlock";
    value: any;
}

/**
 * Represents a method call (object:method())
 */
export interface MethodCall extends Expression {
    type: "MethodCall";
    object: Expression;
    method: string;
    arguments: Expression[];
}

/**
 * Represents an increment/decrement expression (i++, ++i, etc.)
 */
export interface IncrementExpression extends Expression {
    type: "IncrementExpression";
    variable: string;
    isIncrement: boolean; // true for ++, false for --
    isPrefix: boolean; // true for prefix (++i), false for postfix (i++)
}

/**
 * Represents a standalone if statement (without an else branch)
 */
export interface SimpleIfStatement extends Statement {
    type: "IfStatement";
    condition: Expression;
}

/**
 * Represents a standalone while statement (without a body)
 */
export interface SimpleWhileStatement extends Statement {
    type: "WhileStatement";
    condition: Expression;
}

/**
 * Represents a standalone for statement (without a body)
 */
export interface SimpleForStatement extends Statement {
    type: "ForStatement";
    variable: string;
    initialValue: Expression;
    finalValue: Expression;
}

/**
 * Represents a begin inline code statement
 */
export interface BeginInlineCodeStatement extends Statement {
    type: "BeginInlineCodeStatement";
    name: string | null;
}

/**
 * Represents an end inline code statement
 */
export interface EndInlineCodeStatement extends Statement {
    type: "EndInlineCodeStatement";
}
