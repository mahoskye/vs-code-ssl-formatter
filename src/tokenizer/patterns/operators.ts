import { TokenType } from "../tokenType";

/**
 * SSL operators mapped to their token types
 * Ordered by length (longest first) to ensure proper matching
 */
export const SSL_OPERATORS: [string, TokenType][] = [
    // Assignment operators (3 characters)
    [":=", TokenType.ASSIGN],

    // Compound assignment operators (2 characters)
    ["+=", TokenType.PLUS_ASSIGN],
    ["-=", TokenType.MINUS_ASSIGN],
    ["*=", TokenType.MULT_ASSIGN],
    ["/=", TokenType.DIV_ASSIGN],
    ["^=", TokenType.POWER_ASSIGN],

    // Comparison operators (2 characters)
    ["==", TokenType.STRICT_EQUAL],
    ["!=", TokenType.NOT_EQUAL],
    ["<=", TokenType.LESS_EQUAL],
    [">=", TokenType.GREATER_EQUAL],

    // Increment/Decrement operators (2 characters)
    ["++", TokenType.INCREMENT],
    ["--", TokenType.DECREMENT],

    // Single character operators
    ["+", TokenType.PLUS],
    ["-", TokenType.MINUS],
    ["*", TokenType.MULTIPLY],
    ["/", TokenType.DIVIDE],
    ["%", TokenType.MODULO],
    ["^", TokenType.POWER],
    ["=", TokenType.EQUAL],
    ["<", TokenType.LESS_THAN],
    [">", TokenType.GREATER_THAN],
    ["!", TokenType.LOGICAL_NOT],
];

/**
 * SSL logical operators with dot notation
 */
export const SSL_LOGICAL_OPERATORS: Map<string, TokenType> = new Map([
    [".AND.", TokenType.AND],
    [".OR.", TokenType.OR],
    [".NOT.", TokenType.NOT],
]);

/**
 * SSL boolean literals with dot notation
 */
export const SSL_BOOLEAN_LITERALS: Map<string, TokenType> = new Map([
    [".T.", TokenType.BOOLEAN],
    [".F.", TokenType.BOOLEAN],
]);

/**
 * Attempts to match an operator at the given position in the input
 * @param input The input string
 * @param position The current position
 * @returns A tuple of [operator_text, token_type] if found, null otherwise
 */
export function matchOperator(input: string, position: number): [string, TokenType] | null {
    for (const [op, tokenType] of SSL_OPERATORS) {
        if (input.substring(position, position + op.length) === op) {
            return [op, tokenType];
        }
    }
    return null;
}

/**
 * Attempts to match a logical operator (with dots) at the given position
 * @param input The input string
 * @param position The current position
 * @returns A tuple of [operator_text, token_type] if found, null otherwise
 */
export function matchLogicalOperator(input: string, position: number): [string, TokenType] | null {
    // Check for logical operators and boolean literals
    for (const [op, tokenType] of [
        ...SSL_LOGICAL_OPERATORS.entries(),
        ...SSL_BOOLEAN_LITERALS.entries(),
    ]) {
        if (input.substring(position, position + op.length).toUpperCase() === op) {
            return [input.substring(position, position + op.length), tokenType];
        }
    }
    return null;
}

/**
 * Checks if a character can be part of an operator
 */
export function isOperatorChar(char: string): boolean {
    return "+-*/%^=!<>:".includes(char);
}

/**
 * Gets the precedence of an operator for parsing
 */
export function getOperatorPrecedence(tokenType: TokenType): number {
    switch (tokenType) {
        case TokenType.OR:
            return 1;
        case TokenType.AND:
            return 2;
        case TokenType.EQUAL:
        case TokenType.STRICT_EQUAL:
        case TokenType.NOT_EQUAL:
        case TokenType.LESS_THAN:
        case TokenType.GREATER_THAN:
        case TokenType.LESS_EQUAL:
        case TokenType.GREATER_EQUAL:
            return 3;
        case TokenType.PLUS:
        case TokenType.MINUS:
            return 4;
        case TokenType.MULTIPLY:
        case TokenType.DIVIDE:
        case TokenType.MODULO:
            return 5;
        case TokenType.POWER:
            return 6;
        case TokenType.NOT:
        case TokenType.LOGICAL_NOT:
            return 7;
        case TokenType.INCREMENT:
        case TokenType.DECREMENT:
            return 8;
        default:
            return 0;
    }
}

/**
 * Checks if an operator is right-associative
 */
export function isRightAssociative(tokenType: TokenType): boolean {
    return (
        tokenType === TokenType.POWER ||
        tokenType === TokenType.NOT ||
        tokenType === TokenType.LOGICAL_NOT
    );
}
