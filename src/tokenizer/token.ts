import { TokenType } from "./tokenType";

/**
 * Represents the position of a token in the source code
 */
export interface Position {
    line: number; // 0-based line number
    column: number; // 0-based column number
    offset: number; // 0-based absolute character offset
}

/**
 * Represents a range in the source code
 */
export interface Range {
    start: Position;
    end: Position;
}

/**
 * Represents a token in SSL source code
 */
export interface Token {
    type: TokenType;
    value: string; // Raw text value of the token
    range: Range; // Position information

    // Optional parsed value for literals
    parsedValue?: any;

    // Additional metadata
    isKeyword?: boolean;
    isOperator?: boolean;
    isLiteral?: boolean;
    isPunctuation?: boolean;

    // For error reporting
    error?: string;
}

/**
 * Creates a new token
 */
export function createToken(
    type: TokenType,
    value: string,
    start: Position,
    end?: Position
): Token {
    return {
        type,
        value,
        range: {
            start,
            end: end || {
                ...start,
                column: start.column + value.length,
                offset: start.offset + value.length,
            },
        },
        isKeyword: isKeywordType(type),
        isOperator: isOperatorType(type),
        isLiteral: isLiteralType(type),
        isPunctuation: isPunctuationType(type),
    };
}

/**
 * Creates a position object
 */
export function createPosition(line: number, column: number, offset: number): Position {
    return { line, column, offset };
}

/**
 * Checks if a token type is a keyword
 */
export function isKeywordType(type: TokenType): boolean {
    return [
        TokenType.IF,
        TokenType.ELSE,
        TokenType.ENDIF,
        TokenType.WHILE,
        TokenType.ENDWHILE,
        TokenType.FOR,
        TokenType.TO,
        TokenType.NEXT,
        TokenType.EXITWHILE,
        TokenType.EXITFOR,
        TokenType.LOOP,
        TokenType.BEGINCASE,
        TokenType.CASE,
        TokenType.OTHERWISE,
        TokenType.ENDCASE,
        TokenType.EXITCASE,
        TokenType.TRY,
        TokenType.CATCH,
        TokenType.FINALLY,
        TokenType.ENDTRY,
        TokenType.ERROR,
        TokenType.PROCEDURE,
        TokenType.ENDPROC,
        TokenType.PARAMETERS,
        TokenType.DEFAULT,
        TokenType.RETURN,
        TokenType.CLASS,
        TokenType.INHERIT,
        TokenType.DECLARE,
        TokenType.PUBLIC,
        TokenType.INCLUDE,
        TokenType.REGION,
        TokenType.ENDREGION,
        TokenType.BEGININLINECODE,
        TokenType.ENDINLINECODE,
        TokenType.LABEL,
        TokenType.NIL,
    ].includes(type);
}

/**
 * Checks if a token type is an operator
 */
export function isOperatorType(type: TokenType): boolean {
    return [
        TokenType.ASSIGN,
        TokenType.PLUS_ASSIGN,
        TokenType.MINUS_ASSIGN,
        TokenType.MULT_ASSIGN,
        TokenType.DIV_ASSIGN,
        TokenType.POWER_ASSIGN,
        TokenType.PLUS,
        TokenType.MINUS,
        TokenType.MULTIPLY,
        TokenType.DIVIDE,
        TokenType.MODULO,
        TokenType.POWER,
        TokenType.EQUAL,
        TokenType.STRICT_EQUAL,
        TokenType.NOT_EQUAL,
        TokenType.LESS_THAN,
        TokenType.GREATER_THAN,
        TokenType.LESS_EQUAL,
        TokenType.GREATER_EQUAL,
        TokenType.AND,
        TokenType.OR,
        TokenType.NOT,
        TokenType.LOGICAL_NOT,
        TokenType.INCREMENT,
        TokenType.DECREMENT,
    ].includes(type);
}

/**
 * Checks if a token type is a literal
 */
export function isLiteralType(type: TokenType): boolean {
    return [
        TokenType.NUMBER,
        TokenType.STRING,
        TokenType.SQL_STRING,
        TokenType.BOOLEAN,
        TokenType.DATE,
    ].includes(type);
}

/**
 * Checks if a token type is punctuation
 */
export function isPunctuationType(type: TokenType): boolean {
    return [
        TokenType.SEMICOLON,
        TokenType.COMMA,
        TokenType.COLON,
        TokenType.DOT,
        TokenType.LPAREN,
        TokenType.RPAREN,
        TokenType.LBRACE,
        TokenType.RBRACE,
        TokenType.LBRACKET,
        TokenType.RBRACKET,
        TokenType.PIPE,
        TokenType.QUESTION,
        TokenType.ARRAY_START,
        TokenType.ARRAY_END,
        TokenType.CODE_BLOCK_START,
        TokenType.CODE_BLOCK_END,
    ].includes(type);
}
