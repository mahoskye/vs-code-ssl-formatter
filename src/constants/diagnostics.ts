/**
 * SSL Diagnostic Constants
 * Centralized diagnostic messages and codes for SSL language analysis
 */

export const DIAGNOSTIC_CODES = {
    UNDECLARED_VARIABLE: "ssl-undeclared-variable",
    UNDEFINED_VARIABLE: "ssl-undefined-variable",
    GLOBAL_VARIABLE_IN_PROCEDURE: "ssl-global-variable-in-procedure",
    INVALID_SQL_PARAM: "ssl-invalid-sql-param",
    INVALID_EXEC_TARGET: "ssl-invalid-exec-target",
    INVALID_SQL_PLACEHOLDER_STYLE: "ssl-invalid-sql-placeholder-style",
    BLOCK_DEPTH: "ssl-block-depth",
    MAX_PARAMS: "ssl-max-params",
    HUNGARIAN_NOTATION: "ssl-hungarian-notation",
    SQL_INJECTION: "sql-sql-injection",
    MISSING_SEMICOLON: "ssl-missing-semicolon",

    INVALID_DECLARE: "ssl-invalid-declare",
    INVALID_CONST: "ssl-invalid-const",
    MISSING_OTHERWISE: "ssl-missing-otherwise",
    KEYWORD_CASE: "ssl-keyword-case",
    COMMENT_SYNTAX: "ssl-comment-syntax",
    MISSING_PARAMS: "ssl-missing-params",
    MISMATCHED_BLOCK_END: "ssl-mismatched-block-end",
    UNMATCHED_BLOCK_END: "ssl-unmatched-block-end",
    KEYWORD_WITHOUT_CONTEXT: "ssl-keyword-without-context"
} as const;

export type SSLDiagnosticCode = typeof DIAGNOSTIC_CODES[keyof typeof DIAGNOSTIC_CODES];

export const DIAGNOSTIC_MESSAGES = {
    UNDECLARED_VARIABLE: (varName: string) =>
        `Variable '${varName}' is used without being declared. Add ':DECLARE ${varName};' before first use.`,

    UNDEFINED_VARIABLE: (varName: string) =>
        `Variable '${varName}' is not declared. Add ':DECLARE ${varName};' or pass it as a parameter.`,

    GLOBAL_VARIABLE_IN_PROCEDURE: (varName: string) =>
        `Procedure uses global variable '${varName}' without declaring it locally. Declare it with ':DECLARE ${varName};' or pass it as a parameter for better encapsulation.`,

    INVALID_SQL_PARAM: (paramName: string) =>
        `SQL parameter '${paramName}' does not reference a valid variable or constant.`,
    INVALID_EXEC_TARGET: (literal: string, example: string) =>
        `ExecFunction target "${literal}" is incomplete. Provide script and procedure segments (e.g., ${example}).`,
    INVALID_SQL_PLACEHOLDER_STYLE: (funcName: string, expected: "named" | "positional") =>
        `${funcName} expects ${expected === "named" ? "named placeholders like ?sName?" : "positional '?' placeholders"} to match its parameter passing style.`,

    BLOCK_DEPTH_EXCEEDED: (depth: number, maxDepth: number) =>
        `Block nesting depth (${depth}) exceeds maximum (${maxDepth})`,

    MAX_PARAMS_EXCEEDED: (count: number, maxCount: number) =>
        `Procedure has ${count} parameters, exceeds maximum (${maxCount})`,

    HUNGARIAN_NOTATION_PARAM: (paramName: string) =>
        `Parameter '${paramName}' should use Hungarian notation (e.g., sName, nCount, aItems)`,

    HUNGARIAN_NOTATION_VAR: (varName: string) =>
        `Variable '${varName}' should use Hungarian notation (e.g., sName, nCount, aItems)`,

    SQL_INJECTION_RISK: "Potential SQL injection: Use parameterized queries (?PARAM?) instead of string concatenation",

    MISSING_SEMICOLON: "Statement should end with semicolon",



    INVALID_DECLARE_SYNTAX: "Invalid syntax: :DECLARE cannot initialize values. Use ':DECLARE var;' followed by 'var := value;' on separate lines",

    INVALID_CONST_KEYWORD: "Invalid syntax: 'const' is not a valid SSL keyword. Remove 'const' and use proper Hungarian notation",

    MISSING_OTHERWISE: "CASE statement should include :OTHERWISE clause for completeness",

    KEYWORD_CASE_UPPER: (keyword: string) =>
        `Keyword should be UPPERCASE: :${keyword.toUpperCase()} (style guide requires UPPERCASE keywords)`,

    INVALID_COMMENT_SYNTAX: "Invalid SSL comment syntax: Comments should use /* ... ; (semicolon terminator, not */)",

    MISSING_PROCEDURE_PARAMS: (procName: string, expectedCount: number, expectedParams: string[]) =>
        `Procedure '${procName}' expects ${expectedCount} parameter${expectedCount > 1 ? 's' : ''} (${expectedParams.join(', ')}) but none were provided`,

    MISMATCHED_BLOCK_END: (endKeyword: string, expectedEnd: string, startKeyword: string) =>
        `Mismatched block keyword: :${endKeyword} found but expected :${expectedEnd} to close :${startKeyword}`,

    UNMATCHED_BLOCK_END: (endKeyword: string, expectedStart: string) =>
        `Unmatched :${endKeyword} - no matching :${expectedStart} block found`,

    KEYWORD_WITHOUT_CONTEXT: (keyword: string, requiredContext: string) =>
        `:${keyword} requires :${requiredContext} but none was found`
} as const;

export const DIAGNOSTIC_SEVERITIES = {
    ERROR: "error",
    WARNING: "warn",
    INFO: "info"
} as const;
