/**
 * Additional expression types and interfaces needed to resolve type issues
 */

import { Expression, Range, Statement } from "./sslAst";

/**
 * Base interface for expressions that have a left and right operand
 */
export interface ExpressionWithOperands extends Expression {
    left: Expression;
    operator: string;
    right: Expression;
}

/**
 * Base interface for expressions that refer to a name
 */
export interface NamedExpression extends Expression {
    name: string;
}

/**
 * Interface for statements that contain an expression
 */
export interface StatementWithExpression extends Statement {
    expression: Expression;
}

/**
 * Interface extending Statement to include conditions
 */
export interface StatementWithCondition extends Statement {
    condition: Expression;
}

/**
 * Interface extending Statement to include a name
 */
export interface NamedStatement extends Statement {
    name: string | null;
}

/**
 * Interface extending Statement to include a variable
 */
export interface VariableStatement extends Statement {
    variable: string;
}
