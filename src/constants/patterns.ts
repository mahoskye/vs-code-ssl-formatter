/**
 * SSL Pattern Constants
 * Centralized regular expressions and parsing patterns for SSL syntax
 */

export const PATTERNS = {
    PROCEDURE: {
        DEFINITION: /^\s*:PROCEDURE\s+(\w+)\b/i,
        END: /^\s*:ENDPROC\b/i,
        PARAMETERS: /^\s*:PARAMETERS\s+(.+?);/i,
        DEFAULT_VALUE: /^\s*:DEFAULT\s+(\w+)\s*,\s*(.+?);/i,
    },

    CLASS: {
        DEFINITION: /^\s*:CLASS\s+(\w+)\b/i,
        INHERIT: /^\s*:INHERIT\s+(\w+)/i,
    },

    VARIABLE: {
        ASSIGNMENT: /^([a-z][a-zA-Z0-9_]*)\s*:=/,
        DECLARE: /^\s*:DECLARE\s+(.+?);/i,
        GLOBAL_CONSTANT: /^[A-Z_][A-Z0-9_]*\s*:=/,
        UNDECLARED: /\b([a-z][a-zA-Z0-9_]*)\b/g,
    },

    CONTROL_FLOW: {
        IF: /^\s*:IF\b/i,
        ELSE: /^\s*:ELSE\b/i,
        ENDIF: /^\s*:ENDIF\b/i,
        WHILE: /^\s*:WHILE\b/i,
        ENDWHILE: /^\s*:ENDWHILE\b/i,
        FOR: /^\s*:FOR\b/i,
        NEXT: /^\s*:NEXT\b/i,
        BEGINCASE: /^\s*:BEGINCASE\b/i,
        CASE: /^\s*:CASE\b/i,
        OTHERWISE: /^\s*:OTHERWISE\b/i,
        ENDCASE: /^\s*:ENDCASE\b/i,
        TRY: /^\s*:TRY\b/i,
        CATCH: /^\s*:CATCH\b/i,
        FINALLY: /^\s*:FINALLY\b/i,
        ENDTRY: /^\s*:ENDTRY\b/i,
        CASE_KEYWORD: /^\s*:(CASE|OTHERWISE)\b/i,
    },

    BLOCK: {
        START: /^\s*:(IF|WHILE|FOR|BEGINCASE|TRY|PROCEDURE|CLASS|REGION)\b/i,
        MIDDLE: /^\s*:(ELSE|CATCH|FINALLY)\b/i,
        END: /^\s*:(ENDIF|ENDWHILE|NEXT|ENDCASE|ENDTRY|ENDPROC|ENDREGION)\b/i,
    },

    COMMENT: {
        MULTILINE_START: /^\s*\/\*/,
        MULTILINE_END: /;\s*$/,
        SINGLELINE: /^\s*\*/,
        INVALID: /\/\*.*\*\//,
    },

    SQL: {
        FUNCTION_CALL: /\b(SQLExecute|RunSQL|LSearch)\s*\(/i,
        PARAMETER_PLACEHOLDER: /\?([A-Za-z0-9_]+(?:\[[^\]]+\])?)\?/g,
        CONCATENATION: /\+/,
        INVALID_PARAM: /\?([A-Za-z0-9_]+(?:\[[^\]]+\])?)\?/g,
    },

    OBJECT: {
        MEMBER_ACCESS: /(\w+):(\w+)\s*:=/,
        BUILTIN_CLASS_INSTANTIATION: /\b(\w+)\s*:=\s*(\w+)\{\}/i,
        UDOBJECT_INSTANTIATION: /\b(\w+)\s*:=\s*CreateUDObject\s*\(\s*["']([^"']+)["']/i,
        UDOBJECT_ANONYMOUS: /\b(\w+)\s*:=\s*CreateUDObject\s*\(\s*\)/i,
    },

    STRING: {
        DOUBLE: /"/g,
        SINGLE: /'/g,
    },

    KEYWORD: {
        WITH_COLON: /^\s*:([A-Za-z]+)\b/,
    },

    SEMICOLON: {
        MISSING: /[^\s;]$/,
        TRAILING: /\s*;/g,
    },

    OPERATOR: {
        ASSIGNMENT: /\s*:=\s*|\s*\+=\s*|\s*-=\s*|\s*\*=\s*|\s*\/=\s*|\s*\^=\s*|\s*%=\s*/,
        ARITHMETIC: /([a-zA-Z0-9_\)])(\+|\*|\/|\^|%)([a-zA-Z0-9_\(])/g,
        COMPARISON: /\s*(==|!=|<>|<=|>=|=|<|>|\#)\s*/g,
        LOGICAL: /\s*\.AND\.\s*|\s*\.OR\.\s*|\s*\.NOT\.\s*/gi,
        UNARY: /\s*!\s*/g,
        SPACING: /(\w)(==|!=|<>|<=|>=|=|<|>|\#)(\w)/g,
    },

    SPACING: {
        COMMA: /,(\S)/g,
    },

    INDENTATION: {
        LEADING_WHITESPACE: /^(\s*)/,
        ASSIGNMENT_LINE: /^(\s*)(\w+)\s*:=\s*(.+)/,
    },

    CONTINUATION: {
        ENDS_WITH_OPERATOR: /[+\-*/,]$/,
        STARTS_WITH_OPERATOR: /^[+\-*/,]/,
    },

    REGION: {
        START: /^\s*:REGION\s+(.+)/i,
        END: /^\s*:ENDREGION\b/i,
    },

    VALIDATION: {
        INVALID_DECLARE_ASSIGNMENT: /^\s*:DECLARE.*:=/i,
        INVALID_CONST_KEYWORD: /\b(const|CONST)\b/i,
        HUNGARIAN_VARIABLE: /^[a-z][A-Z]/,
        VALID_IDENTIFIER: /^[a-zA-Z_][a-zA-Z0-9_]*$/,
    },

    DOPROC: {
        CALL: /DoProc\s*\(\s*["']([^"']+)["']\s*,\s*\{([^}]*)\}/i,
        NO_PARAMS: /DoProc\s*\(\s*["']([^"']+)["']\s*\)/i,
        COMPLETION: /DoProc\s*\(\s*["']([^"']*)$/i,
    },

    EXEC_FUNCTION: {
        COMPLETION: /ExecFunction\s*\(\s*["']([^"']*)$/i,
    },

    FUNCTION: {
        CALL: /\b(\w+)\s*\(/g,
    }
} as const;
