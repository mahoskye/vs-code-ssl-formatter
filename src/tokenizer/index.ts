// Main entry point for the tokenizer module

// Core tokenizer classes and interfaces
export { Tokenizer, TokenizerOptions, TokenizationResult, TokenStatistics } from "./tokenizer";
export { Lexer, LexerOptions } from "./lexer";

// Token types and utilities
export { Token, Position, Range, createToken, createPosition } from "./token";
export { TokenType } from "./tokenType";

// Error handling
export { TokenizerError, ErrorCode } from "./errors";

// Pattern matching utilities
export { getKeywordType, isBuiltinFunction, getAllKeywords, isKeyword } from "./patterns/keywords";
export { matchOperator, matchLogicalOperator, getOperatorPrecedence } from "./patterns/operators";
export { matchLiteral, canStartLiteral } from "./patterns/literals";
export { matchPunctuation, isPunctuation } from "./patterns/punctuation";

// Utility functions
export {
    isLetter,
    isDigit,
    isAlphanumeric,
    isWhitespace,
    isNewline,
    isIdentifierStart,
    isIdentifierPart,
    parseNumber,
    parseBoolean,
    unescapeString,
    validateDateLiteral,
    peekChar,
    peekChars,
} from "./utils";

// Convenience functions
export { tokenize, validateSyntax } from "./tokenizer";

// Token type helper functions
export { isKeywordType, isOperatorType, isLiteralType, isPunctuationType } from "./token";
