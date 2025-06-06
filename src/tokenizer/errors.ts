import { Position } from "./token";

/**
 * Represents a tokenization error
 */
export class TokenizerError extends Error {
    public readonly position: Position;
    public readonly code: string;

    constructor(message: string, position: Position, code: string = "TOKENIZER_ERROR") {
        super(message);
        this.name = "TokenizerError";
        this.position = position;
        this.code = code;
    }
}

/**
 * Error codes for different types of tokenization errors
 */
export enum ErrorCode {
    UNEXPECTED_CHARACTER = "UNEXPECTED_CHARACTER",
    UNTERMINATED_STRING = "UNTERMINATED_STRING",
    UNTERMINATED_COMMENT = "UNTERMINATED_COMMENT",
    INVALID_NUMBER = "INVALID_NUMBER",
    INVALID_ESCAPE_SEQUENCE = "INVALID_ESCAPE_SEQUENCE",
    INVALID_DATE_LITERAL = "INVALID_DATE_LITERAL",
    UNKNOWN_TOKEN = "UNKNOWN_TOKEN",
}

/**
 * Creates a tokenizer error for unexpected character
 */
export function createUnexpectedCharacterError(char: string, position: Position): TokenizerError {
    return new TokenizerError(
        `Unexpected character '${char}'`,
        position,
        ErrorCode.UNEXPECTED_CHARACTER
    );
}

/**
 * Creates a tokenizer error for unterminated string
 */
export function createUnterminatedStringError(position: Position): TokenizerError {
    return new TokenizerError(
        "Unterminated string literal",
        position,
        ErrorCode.UNTERMINATED_STRING
    );
}

/**
 * Creates a tokenizer error for unterminated comment
 */
export function createUnterminatedCommentError(position: Position): TokenizerError {
    return new TokenizerError("Unterminated comment", position, ErrorCode.UNTERMINATED_COMMENT);
}

/**
 * Creates a tokenizer error for invalid number
 */
export function createInvalidNumberError(value: string, position: Position): TokenizerError {
    return new TokenizerError(
        `Invalid number format: '${value}'`,
        position,
        ErrorCode.INVALID_NUMBER
    );
}

/**
 * Creates a tokenizer error for invalid escape sequence
 */
export function createInvalidEscapeSequenceError(
    sequence: string,
    position: Position
): TokenizerError {
    return new TokenizerError(
        `Invalid escape sequence: '${sequence}'`,
        position,
        ErrorCode.INVALID_ESCAPE_SEQUENCE
    );
}

/**
 * Creates a tokenizer error for invalid date literal
 */
export function createInvalidDateLiteralError(value: string, position: Position): TokenizerError {
    return new TokenizerError(
        `Invalid date literal format: '${value}'`,
        position,
        ErrorCode.INVALID_DATE_LITERAL
    );
}

/**
 * Creates a tokenizer error for unknown token
 */
export function createUnknownTokenError(value: string, position: Position): TokenizerError {
    return new TokenizerError(`Unknown token: '${value}'`, position, ErrorCode.UNKNOWN_TOKEN);
}
