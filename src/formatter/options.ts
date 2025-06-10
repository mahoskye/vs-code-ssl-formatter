/**
 * Formatter Options
 *
 * - Configuration interface for formatting rules
 * - Default formatting options
 * - User-configurable settings (indent size, tabs vs spaces, line length)
 */

/**
 * Configuration interface for SSL formatter options
 * Based on SSL EBNF grammar and formatting best practices
 */
export interface FormatterOptions {
    // === Basic Formatting ===
    /** Number of spaces for indentation (ignored if useTabs is true) */
    indentSize: number;
    /** Use tabs instead of spaces for indentation */
    useTabs: boolean;
    /** Maximum line length before wrapping */
    maxLineLength: number;
    /** Insert final newline at end of file */
    insertFinalNewline: boolean;
    /** Trim trailing whitespace */
    trimTrailingWhitespace: boolean;

    // === Spacing Options ===
    /** Insert spaces around operators (=, +, -, etc.) */
    insertSpacesAroundOperators: boolean;
    /** Insert spaces after commas in parameter lists */
    insertSpacesAfterCommas: boolean;
    /** Insert spaces around assignment operators (:=, +=, etc.) */
    insertSpacesAroundAssignmentOperators: boolean;
    /** Insert spaces around comparison operators (=, ==, <, >, etc.) */
    insertSpacesAroundComparisonOperators: boolean;
    /** Insert spaces around logical operators (.AND., .OR.) */
    insertSpacesAroundLogicalOperators: boolean;
    /** Insert spaces around property access colons (object:property) */
    insertSpacesAroundPropertyAccess: boolean;

    // === Line Breaking ===
    /** Preserve blank lines between sections */
    preserveBlankLines: boolean;
    /** Maximum number of consecutive blank lines to preserve */
    maxPreserveBlankLines: number;
    /** Break long parameter lists onto multiple lines */
    breakLongParameterLists: boolean;
    /** Break long array literals onto multiple lines */
    breakLongArrayLiterals: boolean;
    /** Break long SQL statements onto multiple lines */
    breakLongSqlStatements: boolean;
    /** Threshold for breaking parameter lists (number of parameters) */
    parameterListBreakThreshold: number;

    // === SSL-Specific Options ===
    /** Format SQL statements embedded in strings */
    formatEmbeddedSql: boolean;
    /** Align SQL clauses (SELECT, FROM, WHERE) */
    alignSqlClauses: boolean;
    /** Preserve case of SSL keywords (true = uppercase, false = as-is) */
    uppercaseKeywords: boolean;
    /** Format Hungarian notation consistently */
    enforceHungarianNotation: boolean;
    /** Align end-of-line comments */
    alignEndOfLineComments: boolean;
    /** Column position for aligned comments */
    commentAlignmentColumn: number;

    // === Control Flow Formatting ===
    /** Insert blank lines before control flow statements */
    blankLinesBeforeControlFlow: boolean;
    /** Insert blank lines after control flow statements */
    blankLinesAfterControlFlow: boolean;
    /** Indent case statements relative to switch */
    indentCaseStatements: boolean;
    /** Align procedure parameters */
    alignProcedureParameters: boolean;

    // === Comment Formatting ===
    /** Preserve region markers and comments */
    preserveRegionMarkers: boolean;
    /** Format multi-line comments */
    formatMultiLineComments: boolean;
    /** Wrap long comments */
    wrapLongComments: boolean;

    // === Array and Object Formatting ===
    /** Align array elements when broken across lines */
    alignArrayElements: boolean;
    /** Insert trailing commas in arrays (when applicable) */
    insertTrailingCommas: boolean;
    /** Break object creation calls */
    breakObjectCreationCalls: boolean;
}

/**
 * Default formatting options optimized for SSL
 * Based on SSL style guide and best practices from the EBNF grammar
 */
export const defaultFormatterOptions: FormatterOptions = {
    // Basic Formatting
    indentSize: 4,
    useTabs: false,
    maxLineLength: 90, // Per EBNF implementation notes
    insertFinalNewline: true,
    trimTrailingWhitespace: true,

    // Spacing Options
    insertSpacesAroundOperators: true,
    insertSpacesAfterCommas: true,
    insertSpacesAroundAssignmentOperators: true,
    insertSpacesAroundComparisonOperators: true,
    insertSpacesAroundLogicalOperators: true,
    insertSpacesAroundPropertyAccess: false, // SSL uses object:property without spaces

    // Line Breaking
    preserveBlankLines: true,
    maxPreserveBlankLines: 2,
    breakLongParameterLists: true,
    breakLongArrayLiterals: true,
    breakLongSqlStatements: true,
    parameterListBreakThreshold: 4,

    // SSL-Specific Options
    formatEmbeddedSql: true,
    alignSqlClauses: true,
    uppercaseKeywords: true, // SSL keywords traditionally uppercase
    enforceHungarianNotation: false, // Leave naming to developer
    alignEndOfLineComments: true,
    commentAlignmentColumn: 60,

    // Control Flow Formatting
    blankLinesBeforeControlFlow: false,
    blankLinesAfterControlFlow: false,
    indentCaseStatements: true,
    alignProcedureParameters: true,

    // Comment Formatting
    preserveRegionMarkers: true,
    formatMultiLineComments: true,
    wrapLongComments: true,

    // Array and Object Formatting
    alignArrayElements: true,
    insertTrailingCommas: false, // SSL doesn't support trailing commas
    breakObjectCreationCalls: true,
};

/**
 * Merges user options with default options
 * @param userOptions Partial user-provided options
 * @returns Complete formatter options
 */
export function mergeFormatterOptions(
    userOptions: Partial<FormatterOptions> = {}
): FormatterOptions {
    return {
        ...defaultFormatterOptions,
        ...userOptions,
    };
}

/**
 * Validates formatter options and provides warnings for incompatible settings
 * @param options Formatter options to validate
 * @returns Array of validation warnings (empty if valid)
 */
export function validateFormatterOptions(options: FormatterOptions): string[] {
    const warnings: string[] = [];

    if (options.indentSize < 1 || options.indentSize > 8) {
        warnings.push("indentSize should be between 1 and 8");
    }

    if (options.maxLineLength < 40 || options.maxLineLength > 200) {
        warnings.push("maxLineLength should be between 40 and 200");
    }

    if (options.maxPreserveBlankLines < 0 || options.maxPreserveBlankLines > 10) {
        warnings.push("maxPreserveBlankLines should be between 0 and 10");
    }

    if (options.parameterListBreakThreshold < 1 || options.parameterListBreakThreshold > 20) {
        warnings.push("parameterListBreakThreshold should be between 1 and 20");
    }

    if (options.commentAlignmentColumn < 20 || options.commentAlignmentColumn > 120) {
        warnings.push("commentAlignmentColumn should be between 20 and 120");
    }

    // SSL-specific validations
    if (options.insertSpacesAroundPropertyAccess) {
        warnings.push(
            "insertSpacesAroundPropertyAccess=true is not recommended for SSL (object:property syntax)"
        );
    }

    return warnings;
}

/**
 * Creates formatter options from VS Code configuration
 * @param config VS Code configuration object
 * @returns Formatter options
 */
export function createFormatterOptionsFromConfig(config: any): FormatterOptions {
    const userOptions: Partial<FormatterOptions> = {};

    // Map common VS Code settings
    if (typeof config.indentSize === "number") {
        userOptions.indentSize = config.indentSize;
    }
    if (typeof config.insertSpaces === "boolean") {
        userOptions.useTabs = !config.insertSpaces;
    }
    if (typeof config.tabSize === "number" && config.insertSpaces === false) {
        userOptions.indentSize = config.tabSize;
    } // Map SSL-specific settings if they exist
    const sslConfig = config.ssl || {};
    Object.keys(defaultFormatterOptions).forEach((key) => {
        if (key in sslConfig && sslConfig[key] !== null && sslConfig[key] !== undefined) {
            (userOptions as any)[key] = sslConfig[key];
        }
    });

    return mergeFormatterOptions(userOptions);
}
