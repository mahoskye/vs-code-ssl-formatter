/**
 * SSL Configuration Constants
 * Centralized VS Code configuration keys for the SSL extension
 */

export const CONFIG_KEYS = {
    // Core settings
    MAX_NUMBER_OF_PROBLEMS: "ssl.maxNumberOfProblems",
    STRICT_STYLE_GUIDE_MODE: "ssl.strictStyleGuideMode",

    // Formatting settings
    FORMAT_INDENT_STYLE: "ssl.format.indentStyle",
    FORMAT_INDENT_WIDTH: "ssl.format.indentWidth",
    FORMAT_KEYWORD_CASE: "ssl.format.keywordCase",
    FORMAT_BUILTIN_FUNCTION_CASE: "ssl.format.builtinFunctionCase",
    FORMAT_WRAP_LENGTH: "ssl.format.wrapLength",
    FORMAT_FORMAT_ON_SAVE: "ssl.format.formatOnSave",
    FORMAT_TRIM_TRAILING_WHITESPACE: "ssl.format.trimTrailingWhitespace",

    // Naming settings
    NAMING_HUNGARIAN_ENABLED: "ssl.naming.hungarianNotation.enabled",
    NAMING_HUNGARIAN_SEVERITY: "ssl.naming.hungarianNotation.severity",

    // Style guide settings
    STYLE_GUIDE_LIMIT_BLOCK_DEPTH: "ssl.styleGuide.limitBlockDepth",
    STYLE_GUIDE_MAX_PARAMS_PER_PROCEDURE: "ssl.styleGuide.maxParamsPerProcedure",
    STYLE_GUIDE_ENFORCE_KEYWORD_CASE: "ssl.styleGuide.enforceKeywordCase",
    STYLE_GUIDE_ENFORCE_COMMENT_SYNTAX: "ssl.styleGuide.enforceCommentSyntax",

    // Security settings
    SECURITY_PREVENT_SQL_INJECTION: "ssl.security.preventSqlInjection",
    SECURITY_REQUIRE_PARAMETERIZED_QUERIES: "ssl.security.requireParameterizedQueries",

    // IntelliSense settings
    INTELLISENSE_ENABLED: "ssl.intellisense.enabled",
    INTELLISENSE_CODE_LENS_ENABLED: "ssl.intellisense.codeLens.enabled",
    INTELLISENSE_SIGNATURE_HELP_ENABLED: "ssl.intellisense.signatureHelp.enabled",
    INTELLISENSE_INLAY_HINTS_ENABLED: "ssl.intellisense.inlayHints.enabled",
    INTELLISENSE_INLAY_HINTS_PARAMETER_NAMES: "ssl.intellisense.inlayHints.parameterNames",

    // Trace settings
    TRACE_SERVER: "ssl.trace.server"
} as const;

export const CONFIG_DEFAULTS = {
    [CONFIG_KEYS.MAX_NUMBER_OF_PROBLEMS]: 100,
    [CONFIG_KEYS.STRICT_STYLE_GUIDE_MODE]: false,
    [CONFIG_KEYS.FORMAT_INDENT_STYLE]: "tab",
    [CONFIG_KEYS.FORMAT_INDENT_WIDTH]: 1,
    [CONFIG_KEYS.FORMAT_KEYWORD_CASE]: "upper",
    [CONFIG_KEYS.FORMAT_BUILTIN_FUNCTION_CASE]: "PascalCase",
    [CONFIG_KEYS.FORMAT_WRAP_LENGTH]: 90,
    [CONFIG_KEYS.FORMAT_FORMAT_ON_SAVE]: false,
    [CONFIG_KEYS.FORMAT_TRIM_TRAILING_WHITESPACE]: true,
    [CONFIG_KEYS.NAMING_HUNGARIAN_ENABLED]: true,
    [CONFIG_KEYS.NAMING_HUNGARIAN_SEVERITY]: "warn",
    [CONFIG_KEYS.STYLE_GUIDE_LIMIT_BLOCK_DEPTH]: 4,
    [CONFIG_KEYS.STYLE_GUIDE_MAX_PARAMS_PER_PROCEDURE]: 8,
    [CONFIG_KEYS.STYLE_GUIDE_ENFORCE_KEYWORD_CASE]: true,
    [CONFIG_KEYS.STYLE_GUIDE_ENFORCE_COMMENT_SYNTAX]: true,
    [CONFIG_KEYS.SECURITY_PREVENT_SQL_INJECTION]: true,
    [CONFIG_KEYS.SECURITY_REQUIRE_PARAMETERIZED_QUERIES]: true,
    [CONFIG_KEYS.INTELLISENSE_ENABLED]: true,
    [CONFIG_KEYS.INTELLISENSE_CODE_LENS_ENABLED]: true,
    [CONFIG_KEYS.INTELLISENSE_SIGNATURE_HELP_ENABLED]: true,
    [CONFIG_KEYS.INTELLISENSE_INLAY_HINTS_ENABLED]: true,
    [CONFIG_KEYS.INTELLISENSE_INLAY_HINTS_PARAMETER_NAMES]: true,
    [CONFIG_KEYS.TRACE_SERVER]: "off"
} as const;