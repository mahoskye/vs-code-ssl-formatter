/**
 * SSL Configuration Constants
 * Centralized VS Code configuration keys for the SSL extension. Every key here
 * is contributed in package.json and vice versa; tests/config.test.ts checks it.
 */

export const CONFIG_KEYS = {
    // Core settings
    GLOBAL_VARIABLES: "ssl.globals",

    // Editor settings
    EDITOR_AUTO_INSERT_BLOCK_CLOSERS: "ssl.editor.autoInsertBlockClosers",

    // Formatting settings
    FORMAT_BLANK_LINES_BETWEEN_PROCS: "ssl.format.blankLinesBetweenProcs",
    FORMAT_BUILTIN_FUNCTION_CASE: "ssl.format.builtinFunctionCase",
    FORMAT_COMMA_SPACING: "ssl.format.commaSpacing",
    FORMAT_FORMAT_ON_SAVE: "ssl.format.formatOnSave",
    FORMAT_INDENT_STYLE: "ssl.format.indentStyle",
    FORMAT_INDENT_WIDTH: "ssl.format.indentWidth",
    FORMAT_MAX_CONSECUTIVE_BLANK_LINES: "ssl.format.maxConsecutiveBlankLines",
    FORMAT_OPERATOR_SPACING: "ssl.format.operatorSpacing",
    FORMAT_SEMICOLON_ENFORCEMENT: "ssl.format.semicolonEnforcement",
    FORMAT_SQL_ENABLED: "ssl.format.sql.enabled",
    FORMAT_SQL_IDENTIFIER_CASE: "ssl.format.sql.identifierCase",
    FORMAT_SQL_INDENT_SPACES: "ssl.format.sql.indentSpaces",
    FORMAT_SQL_KEYWORD_CASE: "ssl.format.sql.keywordCase",
    FORMAT_SQL_STYLE: "ssl.format.sql.style",
    FORMAT_TRIM_TRAILING_WHITESPACE: "ssl.format.trimTrailingWhitespace",
    FORMAT_WRAP_LENGTH: "ssl.format.wrapLength",

    // Diagnostics settings
    DIAGNOSTICS_ENDPOINT_PATTERNS: "ssl.diagnostics.endpointPatterns",
    DIAGNOSTICS_INFO_DIAGNOSTICS: "ssl.diagnostics.infoDiagnostics",
    DIAGNOSTICS_MAX_PROCEDURE_CODE_LINES: "ssl.diagnostics.maxProcedureCodeLines",
    DIAGNOSTICS_MAX_SCRIPT_CODE_LINES: "ssl.diagnostics.maxScriptCodeLines",
    DIAGNOSTICS_MAX_SCRIPT_PROCEDURES: "ssl.diagnostics.maxScriptProcedures",
    DIAGNOSTICS_RULES: "ssl.diagnostics.rules",
    DIAGNOSTICS_UNUSED_VARIABLES: "ssl.diagnostics.unusedVariables",

    // Naming settings
    NAMING_HUNGARIAN_NOTATION_CHECK_TYPES: "ssl.naming.hungarianNotation.checkTypes",
    NAMING_HUNGARIAN_ENABLED: "ssl.naming.hungarianNotation.enabled",
    NAMING_HUNGARIAN_PREFIXES: "ssl.naming.hungarianNotation.prefixes",

    // Style guide settings
    STYLE_GUIDE_LIMIT_BLOCK_DEPTH: "ssl.styleGuide.limitBlockDepth",

    // IntelliSense settings
    INTELLISENSE_CODE_LENS_ENABLED: "ssl.intellisense.codeLens.enabled",
    INTELLISENSE_INLAY_HINTS_ENABLED: "ssl.intellisense.inlayHints.enabled",
    INTELLISENSE_INLAY_HINTS_MIN_PARAMETER_COUNT: "ssl.intellisense.inlayHints.minParameterCount",
    INTELLISENSE_SIGNATURE_HELP_AUTO_TRIGGER: "ssl.intellisense.signatureHelp.autoTrigger",

    // Trace settings
    TRACE_SERVER: "ssl.trace.server"
} as const;

export const CONFIG_DEFAULTS = {
    [CONFIG_KEYS.DIAGNOSTICS_ENDPOINT_PATTERNS]: [],
    [CONFIG_KEYS.DIAGNOSTICS_INFO_DIAGNOSTICS]: false,
    [CONFIG_KEYS.DIAGNOSTICS_MAX_PROCEDURE_CODE_LINES]: 80,
    [CONFIG_KEYS.DIAGNOSTICS_MAX_SCRIPT_CODE_LINES]: 400,
    [CONFIG_KEYS.DIAGNOSTICS_MAX_SCRIPT_PROCEDURES]: 12,
    [CONFIG_KEYS.DIAGNOSTICS_RULES]: {},
    [CONFIG_KEYS.DIAGNOSTICS_UNUSED_VARIABLES]: false,
    [CONFIG_KEYS.EDITOR_AUTO_INSERT_BLOCK_CLOSERS]: true,
    [CONFIG_KEYS.FORMAT_BLANK_LINES_BETWEEN_PROCS]: 1,
    [CONFIG_KEYS.FORMAT_BUILTIN_FUNCTION_CASE]: "PascalCase",
    [CONFIG_KEYS.FORMAT_COMMA_SPACING]: true,
    [CONFIG_KEYS.FORMAT_FORMAT_ON_SAVE]: false,
    [CONFIG_KEYS.FORMAT_INDENT_STYLE]: "tab",
    [CONFIG_KEYS.FORMAT_INDENT_WIDTH]: 1,
    [CONFIG_KEYS.FORMAT_MAX_CONSECUTIVE_BLANK_LINES]: 2,
    [CONFIG_KEYS.FORMAT_OPERATOR_SPACING]: true,
    [CONFIG_KEYS.FORMAT_SEMICOLON_ENFORCEMENT]: true,
    [CONFIG_KEYS.FORMAT_SQL_ENABLED]: true,
    [CONFIG_KEYS.FORMAT_SQL_IDENTIFIER_CASE]: "preserve",
    [CONFIG_KEYS.FORMAT_SQL_INDENT_SPACES]: 4,
    [CONFIG_KEYS.FORMAT_SQL_KEYWORD_CASE]: "upper",
    [CONFIG_KEYS.FORMAT_SQL_STYLE]: "canonicalCompact",
    [CONFIG_KEYS.FORMAT_TRIM_TRAILING_WHITESPACE]: true,
    [CONFIG_KEYS.FORMAT_WRAP_LENGTH]: 90,
    [CONFIG_KEYS.GLOBAL_VARIABLES]: [],
    [CONFIG_KEYS.INTELLISENSE_CODE_LENS_ENABLED]: true,
    [CONFIG_KEYS.INTELLISENSE_INLAY_HINTS_ENABLED]: true,
    [CONFIG_KEYS.INTELLISENSE_INLAY_HINTS_MIN_PARAMETER_COUNT]: 2,
    [CONFIG_KEYS.INTELLISENSE_SIGNATURE_HELP_AUTO_TRIGGER]: false,
    [CONFIG_KEYS.NAMING_HUNGARIAN_NOTATION_CHECK_TYPES]: true,
    [CONFIG_KEYS.NAMING_HUNGARIAN_ENABLED]: true,
    [CONFIG_KEYS.NAMING_HUNGARIAN_PREFIXES]: ["s","n","b","d","a","o","fn","v"],
    [CONFIG_KEYS.STYLE_GUIDE_LIMIT_BLOCK_DEPTH]: 4,
    [CONFIG_KEYS.TRACE_SERVER]: "off"
} as const;
