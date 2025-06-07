import { TokenType } from "../tokenType";
import { isDigit, parseNumber, parseBoolean, unescapeString, validateDateLiteral } from "../utils";

/**
 * Regular expressions for matching different types of literals
 */
export const LITERAL_PATTERNS = {
    // Number: integer, decimal (with mandatory digits after decimal), scientific notation
    NUMBER: /^(\d+)(?:\.(\d+))?(?:[eE]([-]?\d+))?$/,

    // String: double or single quoted
    STRING_DOUBLE: /^"([^"\\]|\\.)*"$/,
    STRING_SINGLE: /^'([^'\\]|\\.)*'$/,

    // Boolean: .T. or .F. (case insensitive)
    BOOLEAN: /^\.([TF])\.$/i,

    // Date: {year, month, day[, hour, minute, second]}
    DATE: /^\{\s*\d+\s*,\s*\d+\s*,\s*\d+\s*(?:,\s*\d+\s*,\s*\d+\s*,\s*\d+\s*)?\}$/,
};

/**
 * Attempts to match a number literal starting at the given position
 * @param input The input string
 * @param position The starting position
 * @returns A tuple of [matched_text, parsed_value] if successful, null otherwise
 */
export function matchNumber(input: string, position: number): [string, number] | null {
    let current = position;
    let hasDecimal = false;
    let hasExponent = false;
    let text = "";

    // Match integer part
    if (!isDigit(input[current])) {
        return null;
    }

    while (current < input.length && isDigit(input[current])) {
        text += input[current];
        current++;
    }

    // Match decimal part
    if (current < input.length && input[current] === ".") {
        const nextChar = current + 1 < input.length ? input[current + 1] : "";
        if (isDigit(nextChar)) {
            hasDecimal = true;
            text += input[current]; // Add the dot
            current++;

            while (current < input.length && isDigit(input[current])) {
                text += input[current];
                current++;
            }
        }
    } // Match exponent part
    if (
        current < input.length &&
        (input[current] === "e" || input[current] === "E")
    ) {
        const nextChar = current + 1 < input.length ? input[current + 1] : "";
        const secondChar = current + 2 < input.length ? input[current + 2] : "";

        // Per EBNF note 14: explicit plus signs are not supported, only minus or no sign
        if (isDigit(nextChar) || (nextChar === "-" && isDigit(secondChar))) {
            hasExponent = true;
            text += input[current]; // Add 'e' or 'E'
            current++;

            if (input[current] === "-") {
                text += input[current];
                current++;
            }

            while (current < input.length && isDigit(input[current])) {
                text += input[current];
                current++;
            }
        }
    }

    const parsedValue = parseNumber(text);
    if (parsedValue === null) {
        return null;
    }

    return [text, parsedValue];
}

/**
 * Attempts to match a string literal starting at the given position
 * @param input The input string
 * @param position The starting position
 * @returns A tuple of [matched_text, unescaped_value] if successful, null otherwise
 */
export function matchString(input: string, position: number): [string, string] | null {
    const quote = input[position];
    if (quote !== '"' && quote !== "'") {
        return null;
    }

    let current = position + 1;
    let text = quote;
    let escaped = false;

    while (current < input.length) {
        const char = input[current];
        text += char;

        if (escaped) {
            escaped = false;
        } else if (char === "\\") {
            escaped = true;
        } else if (char === quote) {
            // Found closing quote
            const unescapedValue = unescapeString(text.slice(1, -1));
            return [text, unescapedValue];
        }

        current++;
    }

    // Unterminated string
    return null;
}

/**
 * Attempts to match a boolean literal starting at the given position
 * @param input The input string
 * @param position The starting position
 * @returns A tuple of [matched_text, boolean_value] if successful, null otherwise
 */
export function matchBoolean(input: string, position: number): [string, boolean] | null {
    if (input[position] !== ".") {
        return null;
    }

    const text = input.substring(position, position + 3);
    const boolValue = parseBoolean(text);

    if (boolValue !== null) {
        return [text, boolValue];
    }

    return null;
}

/**
 * Attempts to match a date literal starting at the given position
 * @param input The input string
 * @param position The starting position
 * @returns A tuple of [matched_text, date_components] if successful, null otherwise
 */
export function matchDate(input: string, position: number): [string, number[]] | null {
    if (input[position] !== "{") {
        return null;
    }

    let current = position + 1;
    let text = "{";
    let depth = 1;

    // Find the matching closing brace
    while (current < input.length && depth > 0) {
        const char = input[current];
        text += char;

        if (char === "{") {
            depth++;
        } else if (char === "}") {
            depth--;
        }

        current++;
    }

    if (depth !== 0) {
        return null; // Unmatched braces
    }

    // Extract the content between braces
    const content = text.slice(1, -1).trim();

    // Check if it looks like a date (only contains digits, commas, and whitespace)
    if (!/^[\d\s,]+$/.test(content)) {
        return null;
    }

    // Split by commas and parse components
    const components = content
        .split(",")
        .map((c) => c.trim())
        .filter((c) => c.length > 0);

    if (!validateDateLiteral(components)) {
        return null;
    }

    const dateComponents = components.map((c) => parseInt(c, 10));
    return [text, dateComponents];
}

/**
 * Enhanced literal matching that includes all SSL literal types
 */
export function matchLiteral(input: string, position: number): [string, TokenType, any] | null {
    const char = input[position];

    // Try number first
    if (isDigit(char)) {
        const numberMatch = matchNumber(input, position);
        if (numberMatch) {
            return [numberMatch[0], TokenType.NUMBER, numberMatch[1]];
        }
    }
    // Try string
    if (char === '"' || char === "'") {
        const stringMatch = matchString(input, position);
        if (stringMatch) {
            return [stringMatch[0], TokenType.STRING, stringMatch[1]];
        }
    } // Try boolean
    if (char === ".") {
        const boolMatch = matchBoolean(input, position);
        if (boolMatch) {
            return [boolMatch[0], TokenType.BOOLEAN, boolMatch[1]];
        }
    }

    // Note: Date literals like {2024, 12, 25} and array literals like {1, 2, 3}
    // are tokenized as individual components (LBRACE, NUMBER, COMMA, etc.)
    // rather than as single tokens, so we don't match them here.
    // The { and } will be handled as punctuation tokens.

    // Try code block start {|
    if (input.substring(position, position + 2) === "{|") {
        return ["{|", TokenType.CODE_BLOCK_START, null];
    }

    return null;
}

/**
 * Checks if a character can start a literal
 */
export function canStartLiteral(char: string): boolean {
    return isDigit(char) || char === '"' || char === "'" || char === ".";
}

/**
 * Checks if a character sequence can start a code block literal
 */
export function canStartCodeBlock(input: string, position: number): boolean {
    return input.substring(position, position + 2) === "{|";
}
