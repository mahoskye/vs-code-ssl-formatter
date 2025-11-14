/**
 * SSL Pattern Constants
 * Centralized regular expressions and parsing patterns for SSL syntax
 */

export const PATTERNS = {
    // Procedure patterns
    PROCEDURE_DEFINITION: /^\s*:PROCEDURE\s+(\w+)\b/i,
    PROCEDURE_END: /^\s*:ENDPROC\b/i,
    PARAMETERS_DEFINITION: /^\s*:PARAMETERS\s+(.+?);/i,
    DEFAULT_VALUE: /^\s*:DEFAULT\s+(\w+)\s*,\s*(.+?);/i,

    // Class patterns
    CLASS_DEFINITION: /^\s*:CLASS\s+(\w+)\b/i,
    INHERIT_DEFINITION: /^\s*:INHERIT\s+(\w+)/i,

    // Variable patterns
    VARIABLE_ASSIGNMENT: /^([a-z][a-zA-Z0-9_]*)\s*:=/,
    DECLARE_STATEMENT: /^\s*:DECLARE\s+(.+?);/i,
    GLOBAL_CONSTANT: /^[A-Z_][A-Z0-9_]*\s*:=/,

    // Control flow patterns
    IF_STATEMENT: /^\s*:IF\b/i,
    ELSE_STATEMENT: /^\s*:ELSE\b/i,
    ENDIF_STATEMENT: /^\s*:ENDIF\b/i,
    WHILE_STATEMENT: /^\s*:WHILE\b/i,
    ENDWHILE_STATEMENT: /^\s*:ENDWHILE\b/i,
    FOR_STATEMENT: /^\s*:FOR\b/i,
    NEXT_STATEMENT: /^\s*:NEXT\b/i,
    BEGINCASE_STATEMENT: /^\s*:BEGINCASE\b/i,
    CASE_STATEMENT: /^\s*:CASE\b/i,
    OTHERWISE_STATEMENT: /^\s*:OTHERWISE\b/i,
    ENDCASE_STATEMENT: /^\s*:ENDCASE\b/i,
    TRY_STATEMENT: /^\s*:TRY\b/i,
    CATCH_STATEMENT: /^\s*:CATCH\b/i,
    FINALLY_STATEMENT: /^\s*:FINALLY\b/i,
    ENDTRY_STATEMENT: /^\s*:ENDTRY\b/i,

    // Block patterns
    BLOCK_START: /^\s*:(IF|WHILE|FOR|BEGINCASE|TRY|PROCEDURE|CLASS|REGION)\b/i,
    BLOCK_MIDDLE: /^\s*:(ELSE|CATCH|FINALLY)\b/i,
    CASE_KEYWORD: /^\s*:(CASE|OTHERWISE)\b/i,
    BLOCK_END: /^\s*:(ENDIF|ENDWHILE|NEXT|ENDCASE|ENDTRY|ENDPROC|ENDREGION)\b/i,

    // Comment patterns
    MULTILINE_COMMENT_START: /^\s*\/\*/,
    MULTILINE_COMMENT_END: /;\s*$/,
    SINGLELINE_COMMENT: /^\s*\*/,

    // SQL patterns
    SQL_FUNCTION_CALL: /\b(SQLExecute|RunSQL|LSearch)\s*\(/i,
    SQL_PARAMETER_PLACEHOLDER: /\?(\w+)\?/g,
    SQL_CONCATENATION: /\+/,

    // Object patterns
    OBJECT_MEMBER_ACCESS: /(\w+):(\w+)\s*:=/,
    BUILTIN_CLASS_INSTANTIATION: /\b(\w+)\s*:=\s*(\w+)\{\}/i,
    UDOBJECT_INSTANTIATION: /\b(\w+)\s*:=\s*CreateUDObject\s*\(\s*["']([^"']+)["']/i,
    UDOBJECT_ANONYMOUS: /\b(\w+)\s*:=\s*CreateUDObject\s*\(\s*\)/i,

    // String patterns
    STRING_LITERAL_DOUBLE: /"/g,
    STRING_LITERAL_SINGLE: /'/g,

    // Keyword patterns
    KEYWORD_WITH_COLON: /^\s*:([A-Za-z]+)\b/,

    // Ternary patterns
    TERNARY_EXPRESSION: /\?[^?]+\?[^?]*:[^?]*\?[^?]+\?/,

    // Semicolon patterns
    MISSING_SEMICOLON: /[^\s;]$/,
    TRAILING_SEMICOLON: /\s*;/g,

    // Operator patterns
    ASSIGNMENT_OPERATORS: /\s*:=\s*|\s*\+=\s*|\s*-=\s*|\s*\*=\s*|\s*\/=\s*|\s*\^=\s*|\s*%=\s*/,
    ARITHMETIC_OPERATORS: /([a-zA-Z0-9_\)])(\+|\*|\/|\^|%)([a-zA-Z0-9_\(])/g,
    COMPARISON_OPERATORS: /\s*(==|!=|<>|<=|>=|=|<|>|\#)\s*/g,
    LOGICAL_OPERATORS: /\s*\.AND\.\s*|\s*\.OR\.\s*|\s*\.NOT\.\s*/gi,
    UNARY_OPERATORS: /\s*!\s*/g,

    // Spacing patterns
    COMMA_SPACING: /,(\S)/g,
    OPERATOR_SPACING: /(\w)(==|!=|<>|<=|>=|=|<|>|\#)(\w)/g,

    // Indentation patterns
    LEADING_WHITESPACE: /^(\s*)/,
    ASSIGNMENT_LINE: /^(\s*)(\w+)\s*:=\s*(.+)/,

    // Continuation patterns
    ENDS_WITH_OPERATOR: /[+\-*/,]$/,
    STARTS_WITH_OPERATOR: /^[+\-*/,]/,

    // Region patterns
    REGION_START: /^\s*:REGION\s+(.+)/i,
    REGION_END: /^\s*:ENDREGION\b/i,

    // Invalid syntax patterns
    INVALID_DECLARE_ASSIGNMENT: /^\s*:DECLARE.*:=/i,
    INVALID_CONST_KEYWORD: /\b(const|CONST)\b/i,
    INVALID_COMMENT_SYNTAX: /\/\*.*\*\//,

    // Hungarian notation patterns
    HUNGARIAN_VARIABLE: /^[a-z][A-Z]/,
    VALID_IDENTIFIER: /^[a-zA-Z_][a-zA-Z0-9_]*$/,

    // DoProc patterns
    DOPROC_CALL: /DoProc\s*\(\s*["']([^"']+)["']\s*,\s*\{([^}]*)\}/i,
    DOPROC_NO_PARAMS: /DoProc\s*\(\s*["']([^"']+)["']\s*\)/i,
    DOPROC_COMPLETION: /DoProc\s*\(\s*["']([^"']*)$/i,

    // Function call patterns
    FUNCTION_CALL: /\b(\w+)\s*\(/g,

    // Error patterns
    UNDECLARED_VARIABLE: /\b([a-z][a-zA-Z0-9_]*)\b/g,
    INVALID_SQL_PARAM: /\?(\w+)\?/g
} as const;