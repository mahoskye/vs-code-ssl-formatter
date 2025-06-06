/**
 * Enum defining all token types in SSL language
 * Based on the SSL EBNF grammar specification
 */
export enum TokenType {
    // Literals
    NUMBER = "NUMBER",
    STRING = "STRING",
    SQL_STRING = "SQL_STRING", // Added back for future SQL processing
    BOOLEAN = "BOOLEAN",
    NIL = "NIL",
    DATE = "DATE",

    // Identifiers and Names
    IDENTIFIER = "IDENTIFIER",

    // Keywords - Control Flow
    IF = "IF",
    ELSE = "ELSE",
    ENDIF = "ENDIF",
    WHILE = "WHILE",
    ENDWHILE = "ENDWHILE",
    FOR = "FOR",
    TO = "TO",
    NEXT = "NEXT",
    EXITWHILE = "EXITWHILE",
    EXITFOR = "EXITFOR",
    LOOP = "LOOP",

    // Keywords - Switch/Case
    BEGINCASE = "BEGINCASE",
    CASE = "CASE",
    OTHERWISE = "OTHERWISE",
    ENDCASE = "ENDCASE",
    EXITCASE = "EXITCASE",

    // Keywords - Error Handling
    TRY = "TRY",
    CATCH = "CATCH",
    FINALLY = "FINALLY",
    ENDTRY = "ENDTRY",
    ERROR = "ERROR",

    // Keywords - Procedures and Classes
    PROCEDURE = "PROCEDURE",
    ENDPROC = "ENDPROC",
    PARAMETERS = "PARAMETERS",
    DEFAULT = "DEFAULT",
    RETURN = "RETURN",
    CLASS = "CLASS",
    INHERIT = "INHERIT",

    // Keywords - Declarations
    DECLARE = "DECLARE",
    PUBLIC = "PUBLIC",
    INCLUDE = "INCLUDE",

    // Keywords - Regions and Code Blocks
    REGION = "REGION",
    ENDREGION = "ENDREGION",
    BEGININLINECODE = "BEGININLINECODE",
    ENDINLINECODE = "ENDINLINECODE",
    LABEL = "LABEL",

    // Operators - Assignment
    ASSIGN = "ASSIGN", // :=
    PLUS_ASSIGN = "PLUS_ASSIGN", // +=
    MINUS_ASSIGN = "MINUS_ASSIGN", // -=
    MULT_ASSIGN = "MULT_ASSIGN", // *=
    DIV_ASSIGN = "DIV_ASSIGN", // /=
    POWER_ASSIGN = "POWER_ASSIGN", // ^=

    // Operators - Arithmetic
    PLUS = "PLUS", // +
    MINUS = "MINUS", // -
    MULTIPLY = "MULTIPLY", // *
    DIVIDE = "DIVIDE", // /
    MODULO = "MODULO", // %
    POWER = "POWER", // ^

    // Operators - Comparison
    EQUAL = "EQUAL", // =
    STRICT_EQUAL = "STRICT_EQUAL", // ==
    NOT_EQUAL = "NOT_EQUAL", // !=
    LESS_THAN = "LESS_THAN", // <
    GREATER_THAN = "GREATER_THAN", // >
    LESS_EQUAL = "LESS_EQUAL", // <=
    GREATER_EQUAL = "GREATER_EQUAL", // >=

    // Operators - Logical
    AND = "AND", // .AND.
    OR = "OR", // .OR.
    NOT = "NOT", // .NOT.
    LOGICAL_NOT = "LOGICAL_NOT", // !

    // Operators - Increment/Decrement
    INCREMENT = "INCREMENT", // ++
    DECREMENT = "DECREMENT", // --

    // Punctuation
    SEMICOLON = "SEMICOLON", // ;
    COMMA = "COMMA", // ,
    COLON = "COLON", // :
    DOT = "DOT", // .
    LPAREN = "LPAREN", // (
    RPAREN = "RPAREN", // )
    LBRACE = "LBRACE", // {
    RBRACE = "RBRACE", // }
    LBRACKET = "LBRACKET", // [
    RBRACKET = "RBRACKET", // ]
    PIPE = "PIPE", // |
    QUESTION = "QUESTION", // ?

    // Comments
    BLOCK_COMMENT = "BLOCK_COMMENT",
    SINGLE_LINE_COMMENT = "SINGLE_LINE_COMMENT",
    REGION_COMMENT = "REGION_COMMENT",
    ENDREGION_COMMENT = "ENDREGION_COMMENT",

    // Special tokens
    NEWLINE = "NEWLINE",
    WHITESPACE = "WHITESPACE",
    EOF = "EOF",
    UNKNOWN = "UNKNOWN",

    // Code block specific tokens
    CODE_BLOCK_START = "CODE_BLOCK_START", // {|
    CODE_BLOCK_END = "CODE_BLOCK_END", // |}
}
