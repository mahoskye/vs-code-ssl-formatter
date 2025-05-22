/**
 * Additional statement types that are required by the parser
 * but not part of the main abstract syntax tree
 */

import { Range, Statement } from "./sslAst";

/**
 * Represents an else statement
 */
export interface ElseStatement extends Statement {
    type: "ElseStatement";
}

/**
 * Represents an endif statement
 */
export interface EndIfStatement extends Statement {
    type: "EndIfStatement";
}

/**
 * Represents an endwhile statement
 */
export interface EndWhileStatement extends Statement {
    type: "EndWhileStatement";
}

/**
 * Represents an endcase statement
 */
export interface EndCaseStatement extends Statement {
    type: "EndCaseStatement";
}

/**
 * Represents a case statement
 */
export interface CaseStatement extends Statement {
    type: "CaseStatement";
    condition: any; // Expression type from sslAst
}

/**
 * Represents a begincase statement
 */
export interface BeginCaseStatement extends Statement {
    type: "BeginCaseStatement";
}

/**
 * Represents an otherwise statement
 */
export interface OtherwiseStatement extends Statement {
    type: "OtherwiseStatement";
}

/**
 * Represents a try statement
 */
export interface CatchStatement extends Statement {
    type: "CatchStatement";
}

/**
 * Represents a finally statement
 */
export interface FinallyStatement extends Statement {
    type: "FinallyStatement";
}

/**
 * Represents an endtry statement
 */
export interface EndTryStatement extends Statement {
    type: "EndTryStatement";
}

/**
 * Represents an error statement
 */
export interface ErrorStatement extends Statement {
    type: "ErrorStatement";
    message: string;
}

/**
 * Represents a next statement (end of for loop)
 */
export interface NextStatement extends Statement {
    type: "NextStatement";
}

/**
 * Represents an endregion statement
 */
export interface EndRegionStatement extends Statement {
    type: "EndRegionStatement";
}

/**
 * Represents an end inline code statement
 */
export interface EndInlineCodeStatement extends Statement {
    type: "EndInlineCodeStatement";
}

/**
 * Represents a begin inline code statement
 */
export interface BeginInlineCodeStatement extends Statement {
    type: "BeginInlineCodeStatement";
    name: string | null;
}

/**
 * Represents a parameter statement
 */
export interface ParametersStatement extends Statement {
    type: "ParametersStatement";
    parameters: string[];
}
