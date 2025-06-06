import { TokenType } from "../tokenType";
import { equalsIgnoreCase } from "../utils";

/**
 * Map of SSL keywords to their token types
 * Keywords are case-insensitive in SSL
 */
export const SSL_KEYWORDS: Map<string, TokenType> = new Map([
    // Control Flow
    ["IF", TokenType.IF],
    ["ELSE", TokenType.ELSE],
    ["ENDIF", TokenType.ENDIF],
    ["WHILE", TokenType.WHILE],
    ["ENDWHILE", TokenType.ENDWHILE],
    ["FOR", TokenType.FOR],
    ["TO", TokenType.TO],
    ["NEXT", TokenType.NEXT],
    ["EXITWHILE", TokenType.EXITWHILE],
    ["EXITFOR", TokenType.EXITFOR],
    ["LOOP", TokenType.LOOP],

    // Switch/Case
    ["BEGINCASE", TokenType.BEGINCASE],
    ["CASE", TokenType.CASE],
    ["OTHERWISE", TokenType.OTHERWISE],
    ["ENDCASE", TokenType.ENDCASE],
    ["EXITCASE", TokenType.EXITCASE],

    // Error Handling
    ["TRY", TokenType.TRY],
    ["CATCH", TokenType.CATCH],
    ["FINALLY", TokenType.FINALLY],
    ["ENDTRY", TokenType.ENDTRY],
    ["ERROR", TokenType.ERROR],

    // Procedures and Classes
    ["PROCEDURE", TokenType.PROCEDURE],
    ["ENDPROC", TokenType.ENDPROC],
    ["PARAMETERS", TokenType.PARAMETERS],
    ["DEFAULT", TokenType.DEFAULT],
    ["RETURN", TokenType.RETURN],
    ["CLASS", TokenType.CLASS],
    ["INHERIT", TokenType.INHERIT],

    // Declarations
    ["DECLARE", TokenType.DECLARE],
    ["PUBLIC", TokenType.PUBLIC],
    ["INCLUDE", TokenType.INCLUDE],

    // Regions and Code Blocks
    ["REGION", TokenType.REGION],
    ["ENDREGION", TokenType.ENDREGION],
    ["BEGININLINECODE", TokenType.BEGININLINECODE],
    ["ENDINLINECODE", TokenType.ENDINLINECODE],
    ["LABEL", TokenType.LABEL],

    // Literals
    ["NIL", TokenType.NIL],
]);

/**
 * Built-in SSL functions that should be recognized as identifiers but have special meaning
 */
export const SSL_BUILTIN_FUNCTIONS = new Set([
    "DOPROC",
    "EXECFUNCTION",
    "EXECUDF",
    "SQLEXECUTE",
    "LSEARCH",
    "CREATEUDOBJECT",
    "BRANCH",
    "LEN",
    "CTOD",
    "TODAY",
    "NOW",
    "_AND",
    "_OR",
    "_XOR",
    "_NOT",
    // Additional SSL built-in functions based on EBNF grammar
    "SQRT",
    "ABS",
    "ROUND",
    "SUBSTR",
    "UPPER",
    "LOWER",
    "TRIM",
    "ISEMPTY",
    "ISNULL",
    "STR",
    "VAL",
    "CHR",
    "ASC",
]);

/**
 * Checks if a given identifier is a keyword
 * @param identifier The identifier to check (case-insensitive)
 * @returns The token type if it's a keyword, undefined otherwise
 */
export function getKeywordType(identifier: string): TokenType | undefined {
    return SSL_KEYWORDS.get(identifier.toUpperCase());
}

/**
 * Checks if a given identifier is a built-in function
 * @param identifier The identifier to check (case-insensitive)
 * @returns True if it's a built-in function, false otherwise
 */
export function isBuiltinFunction(identifier: string): boolean {
    return SSL_BUILTIN_FUNCTIONS.has(identifier.toUpperCase());
}

/**
 * Gets all keywords as an array of strings
 */
export function getAllKeywords(): string[] {
    return Array.from(SSL_KEYWORDS.keys());
}

/**
 * Checks if an identifier matches a keyword using case-insensitive comparison
 */
export function isKeyword(identifier: string): boolean {
    return SSL_KEYWORDS.has(identifier.toUpperCase());
}
