import { TokenType } from "../tokenType";

/**
 * SSL punctuation characters mapped to their token types
 */
export const SSL_PUNCTUATION: Map<string, TokenType> = new Map([
    [";", TokenType.SEMICOLON],
    [",", TokenType.COMMA],
    [":", TokenType.COLON],
    [".", TokenType.DOT],
    ["(", TokenType.LPAREN],
    [")", TokenType.RPAREN],
    ["{", TokenType.LBRACE],
    ["}", TokenType.RBRACE],
    ["[", TokenType.LBRACKET],
    ["]", TokenType.RBRACKET],
    ["|", TokenType.PIPE],
    ["?", TokenType.QUESTION],
]);

/**
 * Multi-character punctuation patterns (ordered by length, longest first)
 */
export const SSL_MULTI_PUNCTUATION: [string, TokenType][] = [["{|", TokenType.CODE_BLOCK_START]];

/**
 * Attempts to match punctuation at the given position
 * @param input The input string
 * @param position The current position
 * @returns A tuple of [punctuation_char, token_type] if found, null otherwise
 */
export function matchPunctuation(input: string, position: number): [string, TokenType] | null {
    // Check multi-character punctuation first
    for (const [punct, tokenType] of SSL_MULTI_PUNCTUATION) {
        if (input.substring(position, position + punct.length) === punct) {
            return [punct, tokenType];
        }
    }

    // Check single-character punctuation
    const char = input[position];
    const tokenType = SSL_PUNCTUATION.get(char);

    if (tokenType) {
        // Special handling for { and } to distinguish array vs code block
        if (char === "{" && input.substring(position, position + 2) === "{|") {
            // This will be handled by multi-character check above
            return null;
        }
        if (char === "}" && position > 0 && input[position - 1] === "|") {
            // This will be handled by multi-character check above
            return null;
        } // For braces, use the mapped token types (LBRACE/RBRACE)
        // instead of hardcoding ARRAY_START/ARRAY_END
        // This allows date literals like {2024, 12, 25} to be tokenized
        // as individual components rather than as array structures

        return [char, tokenType];
    }

    return null;
}

/**
 * Checks if a character is punctuation
 */
export function isPunctuation(char: string): boolean {
    return SSL_PUNCTUATION.has(char);
}

/**
 * Gets all punctuation characters as an array
 */
export function getAllPunctuation(): string[] {
    return Array.from(SSL_PUNCTUATION.keys());
}

/**
 * Checks if a punctuation character is an opening bracket/brace/paren
 */
export function isOpeningBracket(char: string): boolean {
    return ["(", "{", "["].includes(char);
}

/**
 * Checks if a punctuation character is a closing bracket/brace/paren
 */
export function isClosingBracket(char: string): boolean {
    return [")", "}", "]"].includes(char);
}

/**
 * Gets the matching closing bracket for an opening bracket
 */
export function getMatchingClosingBracket(openChar: string): string | null {
    const mapping: { [key: string]: string } = {
        "(": ")",
        "{": "}",
        "[": "]",
    };
    return mapping[openChar] || null;
}

/**
 * Gets the matching opening bracket for a closing bracket
 */
export function getMatchingOpeningBracket(closeChar: string): string | null {
    const mapping: { [key: string]: string } = {
        ")": "(",
        "}": "{",
        "]": "[",
    };
    return mapping[closeChar] || null;
}

/**
 * Gets all multi-character punctuation patterns
 */
export function getAllMultiPunctuation(): string[] {
    return SSL_MULTI_PUNCTUATION.map(([punct]) => punct);
}
