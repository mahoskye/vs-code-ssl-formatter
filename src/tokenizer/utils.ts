/**
 * Utility functions for the tokenizer module
 */

/**
 * Checks if a character is a letter (a-z, A-Z)
 */
export function isLetter(char: string): boolean {
    return /[a-zA-Z]/.test(char);
}

/**
 * Checks if a character is a digit (0-9)
 */
export function isDigit(char: string): boolean {
    return /[0-9]/.test(char);
}

/**
 * Checks if a character is alphanumeric (a-z, A-Z, 0-9)
 */
export function isAlphanumeric(char: string): boolean {
    return /[a-zA-Z0-9]/.test(char);
}

/**
 * Checks if a character is whitespace (space, tab)
 */
export function isWhitespace(char: string): boolean {
    return /[ \t]/.test(char);
}

/**
 * Checks if a character is a newline
 */
export function isNewline(char: string): boolean {
    return char === "\n" || char === "\r";
}

/**
 * Checks if a character can start an identifier (letter or underscore)
 */
export function isIdentifierStart(char: string): boolean {
    return isLetter(char) || char === "_";
}

/**
 * Checks if a character can be part of an identifier (letter, digit, underscore)
 */
export function isIdentifierPart(char: string): boolean {
    return isAlphanumeric(char) || char === "_";
}

/**
 * Checks if a character is a valid operator character
 */
export function isOperatorChar(char: string): boolean {
    return "+-*/%^=!<>:".includes(char);
}

/**
 * Checks if a character is punctuation
 */
export function isPunctuation(char: string): boolean {
    return "();{}[],.?|".includes(char);
}

/**
 * Converts a string to uppercase for case-insensitive comparisons
 */
export function toUpperCase(str: string): string {
    return str.toUpperCase();
}

/**
 * Checks if two strings are equal ignoring case
 */
export function equalsIgnoreCase(str1: string, str2: string): boolean {
    return toUpperCase(str1) === toUpperCase(str2);
}

/**
 * Escapes special characters in a string for regex
 */
export function escapeRegex(str: string): string {
    return str.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

/**
 * Parses a numeric literal, handling integers, decimals, and scientific notation
 */
export function parseNumber(value: string): number | null {
    // SSL supports: integers, decimals with mandatory decimal point digit, scientific notation
    // Examples: 123, 123.45, 1.23e5, 4.56E-3
    const numberRegex = /^(\d+)(?:\.(\d+))?(?:[eE]([-]?\d+))?$/;
    const match = value.match(numberRegex);

    if (!match) {
        return null;
    }

    const num = parseFloat(value);
    return isNaN(num) ? null : num;
}

/**
 * Parses a boolean literal (.T. or .F.)
 */
export function parseBoolean(value: string): boolean | null {
    const upper = toUpperCase(value);
    if (upper === ".T.") {return true;}
    if (upper === ".F.") {return false;}
    return null;
}

/**
 * Unescapes a string literal, handling escape sequences
 */
export function unescapeString(value: string): string {
    // SSL string escaping rules (basic implementation)
    return value.replace(/\\(.)/g, (match, char) => {
        switch (char) {
            case "n":
                return "\n";
            case "r":
                return "\r";
            case "t":
                return "\t";
            case "\\":
                return "\\";
            case '"':
                return '"';
            case "'":
                return "'";
            default:
                return char;
        }
    });
}

/**
 * Validates a date literal format {year, month, day[, hour, minute, second]}
 */
export function validateDateLiteral(components: string[]): boolean {
    // Date literals must have 3-6 numeric components
    if (components.length < 3 || components.length > 6) {
        return false;
    }

    // All components must be valid numbers
    return components.every((comp) => {
        const num = parseInt(comp.trim(), 10);
        return !isNaN(num) && num >= 0;
    });
}

/**
 * Gets the next character without advancing the position
 */
export function peekChar(input: string, index: number): string {
    return index < input.length ? input[index] : "";
}

/**
 * Gets multiple characters ahead without advancing the position
 */
export function peekChars(input: string, index: number, count: number): string {
    return input.substring(index, index + count);
}
