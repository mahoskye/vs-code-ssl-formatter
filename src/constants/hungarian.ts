/**
 * SSL Hungarian Notation Constants
 * Centralized rules and exceptions for Hungarian notation validation
 */

export const HUNGARIAN_PREFIXES = [
    "s",  // string
    "n",  // number
    "b",  // boolean
    "d",  // date
    "a",  // array
    "o",  // object
    "fn", // code block
    "v"   // variant/any
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
export function hasValidHungarianNotation(name: string, prefixes: readonly string[] = HUNGARIAN_PREFIXES): boolean {
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

    if (prefixes.length === 0) {
        return true;
    }

    const lowerName = name.toLowerCase();
    const prefixList = prefixes === HUNGARIAN_PREFIXES
        ? [...HUNGARIAN_PREFIXES]
        : prefixes.map(value => value.toLowerCase());

    // Sort by length descending so multi-char prefixes (e.g. "fn") are checked first
    const sorted = [...prefixList].sort((a, b) => b.length - a.length);

    for (const prefix of sorted) {
        if (lowerName.startsWith(prefix) && name.length > prefix.length) {
            // Character after prefix must be uppercase
            return name[prefix.length] === name[prefix.length].toUpperCase();
        }
    }

    return false;
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