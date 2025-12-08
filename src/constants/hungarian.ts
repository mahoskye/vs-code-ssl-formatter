/**
 * SSL Hungarian Notation Constants
 * Centralized rules and exceptions for Hungarian notation validation
 */

export const HUNGARIAN_PREFIXES = [
    "s", // string
    "n", // number
    "b", // boolean/logical
    "l", // logical (alternative)
    "d", // date
    "a", // array
    "o", // object
    "u"  // user-defined object
] as const;

export type HungarianPrefix = typeof HUNGARIAN_PREFIXES[number];

export const HUNGARIAN_EXCEPTIONS = [
    // Loop counters
    "i", "j", "k", "x", "y", "z",
    // Constants and special values
    "NIL", ".T.", ".F.", "ID", "SQL", "URL", "XML", "HTML", "API", "UID", "GUID"
] as const;

export const HUNGARIAN_VALID_PREFIXES = new Set(HUNGARIAN_PREFIXES);
export const HUNGARIAN_VALID_EXCEPTIONS = new Set(HUNGARIAN_EXCEPTIONS);

/**
 * Check if a variable name follows valid Hungarian notation
 */
export function hasValidHungarianNotation(name: string): boolean {
    // Exceptions for loop counters and constants
    if (HUNGARIAN_VALID_EXCEPTIONS.has(name as typeof HUNGARIAN_EXCEPTIONS[number])) {
        return true;
    }

    // Allow ALL_CAPS constants (global constants pattern)
    if (/^[A-Z][A-Z0-9_]+$/.test(name)) {
        return true;
    }

    // Check minimum length
    if (name.length < 2) {
        return false;
    }

    const prefix = name[0].toLowerCase();

    // Check if first char is valid prefix and second char is uppercase
    // Cast strict type for Set lookup check
    return HUNGARIAN_VALID_PREFIXES.has(prefix as HungarianPrefix) &&
        name[1] === name[1].toUpperCase();
}

/**
 * Get the expected Hungarian notation prefix for a variable type
 */
export function getHungarianPrefixSuggestion(name: string): string {
    if (name.length < 2) {
        return "s"; // default to string
    }

    const prefix = name[0].toLowerCase();
    if (HUNGARIAN_VALID_PREFIXES.has(prefix as HungarianPrefix)) {
        return prefix;
    }

    return "s"; // default suggestion
}