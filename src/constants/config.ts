/**
 * SSL Configuration Constants
 * Centralized VS Code configuration keys for the SSL extension
 */

export const CONFIG_KEYS = {
    // Core settings
    MAX_NUMBER_OF_PROBLEMS: "ssl.maxNumberOfProblems",
    STRICT_STYLE_GUIDE_MODE: "ssl.strictStyleGuideMode",
    GLOBAL_VARIABLES: "ssl.globals",

    // Formatting settings
    FORMAT_INDENT_STYLE: "ssl.format.indentStyle",
    FORMAT_INDENT_WIDTH: "ssl.format.indentWidth",
    FORMAT_BUILTIN_FUNCTION_CASE: "ssl.format.builtinFunctionCase",
    FORMAT_WRAP_LENGTH: "ssl.format.wrapLength",
    FORMAT_FORMAT_ON_SAVE: "ssl.format.formatOnSave",
    FORMAT_TRIM_TRAILING_WHITESPACE: "ssl.format.trimTrailingWhitespace",
    FORMAT_SQL_ENABLED: "ssl.format.sql.enabled",
    FORMAT_SQL_KEYWORD_CASE: "ssl.format.sql.keywordCase",
    FORMAT_SQL_INDENT_SPACES: "ssl.format.sql.indentSpaces",
    FORMAT_SQL_STYLE: "ssl.format.sql.style",
    DOCUMENT_NAMESPACES: "ssl.documentNamespaces",

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
    INTELLISENSE_INLAY_HINTS_SHOW_ON_ACTIVE_LINE_ONLY: "ssl.intellisense.inlayHints.showOnActiveLineOnly",
    INTELLISENSE_COMPLETION_KEYWORDS_ENABLED: "ssl.intellisense.completion.enableKeywords",
    INTELLISENSE_COMPLETION_FUNCTIONS_ENABLED: "ssl.intellisense.completion.enableBuiltinFunctions",
    INTELLISENSE_COMPLETION_CLASSES_ENABLED: "ssl.intellisense.completion.enableBuiltinClasses",
    INTELLISENSE_COMPLETION_SNIPPETS_ENABLED: "ssl.intellisense.completion.enableSnippets",
    INTELLISENSE_CUSTOM_FUNCTIONS: "ssl.intellisense.customFunctions",
    INTELLISENSE_CUSTOM_CLASSES: "ssl.intellisense.customClasses",

    // Trace settings
    TRACE_SERVER: "ssl.trace.server"
} as const;

export type SSLConfigKey = typeof CONFIG_KEYS[keyof typeof CONFIG_KEYS];

export const CONFIG_DEFAULTS = {
    [CONFIG_KEYS.MAX_NUMBER_OF_PROBLEMS]: 100,
    [CONFIG_KEYS.STRICT_STYLE_GUIDE_MODE]: false,
    [CONFIG_KEYS.GLOBAL_VARIABLES]: [],
    [CONFIG_KEYS.FORMAT_INDENT_STYLE]: "tab",
    [CONFIG_KEYS.FORMAT_INDENT_WIDTH]: 1,
    [CONFIG_KEYS.FORMAT_BUILTIN_FUNCTION_CASE]: "PascalCase",
    [CONFIG_KEYS.FORMAT_WRAP_LENGTH]: 90,
    [CONFIG_KEYS.FORMAT_FORMAT_ON_SAVE]: false,
    [CONFIG_KEYS.FORMAT_TRIM_TRAILING_WHITESPACE]: true,
    [CONFIG_KEYS.FORMAT_SQL_ENABLED]: true,
    [CONFIG_KEYS.FORMAT_SQL_KEYWORD_CASE]: "upper",
    [CONFIG_KEYS.FORMAT_SQL_INDENT_SPACES]: 4,
    [CONFIG_KEYS.FORMAT_SQL_STYLE]: "canonicalCompact",
    [CONFIG_KEYS.DOCUMENT_NAMESPACES]: {},
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
    [CONFIG_KEYS.INTELLISENSE_INLAY_HINTS_SHOW_ON_ACTIVE_LINE_ONLY]: true,
    [CONFIG_KEYS.INTELLISENSE_COMPLETION_KEYWORDS_ENABLED]: true,
    [CONFIG_KEYS.INTELLISENSE_COMPLETION_FUNCTIONS_ENABLED]: true,
    [CONFIG_KEYS.INTELLISENSE_COMPLETION_CLASSES_ENABLED]: true,
    [CONFIG_KEYS.INTELLISENSE_COMPLETION_SNIPPETS_ENABLED]: true,
    [CONFIG_KEYS.INTELLISENSE_CUSTOM_FUNCTIONS]: [],
    [CONFIG_KEYS.INTELLISENSE_CUSTOM_CLASSES]: [],
    [CONFIG_KEYS.TRACE_SERVER]: "off"
} as const;
