/**
 * SSL Tokenizer - Lexical analysis for SSL (STARLIMS Scripting Language)
 * Based on the SSL EBNF grammar specification
 */

export enum TokenType {
    // Keywords - Control Flow
    procedure = "PROCEDURE",
    endproc = "ENDPROC",
    if = "IF",
    else = "ELSE",
    endif = "ENDIF",
    while = "WHILE",
    endwhile = "ENDWHILE",
    for = "FOR",
    to = "TO",
    next = "NEXT",
    begincase = "BEGINCASE",
    case = "CASE",
    otherwise = "OTHERWISE",
    endcase = "ENDCASE",
    try = "TRY",
    catch = "CATCH",
    finally = "FINALLY",
    endtry = "ENDTRY",
    error = "ERROR",

    // Keywords - Declarations
    parameters = "PARAMETERS",
    declare = "DECLARE",
    default = "DEFAULT",
    public = "PUBLIC",
    include = "INCLUDE",
    class = "CLASS",
    endclass = "ENDCLASS", // Added
    inherit = "INHERIT",

    // Keywords - Control
    return = "RETURN",
    exitwhile = "EXITWHILE",
    exitfor = "EXITFOR",
    exitcase = "EXITCASE",
    loop = "LOOP",
    label = "LABEL",

    // Keywords - Regions
    region = "REGION",
    endregion = "ENDREGION",
    begininlinecode = "BEGININLINECODE",
    endinlinecode = "ENDINLINECODE",

    // Keywords - Built-in Functions / Commands
    doProc = "DOPROC",
    execFunction = "EXECFUNCTION",
    execUDF = "EXECUDF",
    branch = "BRANCH",
    sqlExecute = "SQLEXECUTE",
    lSearch = "LSEARCH",
    createUDObject = "CREATEUDOBJECT",
    bitwiseAnd = "_AND",
    bitwiseOr = "_OR",
    bitwiseXor = "_XOR",
    bitwiseNot = "_NOT",

    // Assignment Operators
    assign = "ASSIGN", // :=
    plusAssign = "PLUS_ASSIGN", // Re-typed to ensure no hidden characters
    minusAssign = "MINUS_ASSIGN", // -=
    multAssign = "MULT_ASSIGN", // *=
    divAssign = "DIV_ASSIGN", // /=
    powerAssign = "POWER_ASSIGN", // ^=

    // Comparison Operators
    equals = "EQUALS", // ==
    notEquals = "NOT_EQUALS", // !=
    lessThan = "LESS_THAN", // <
    greaterThan = "GREATER_THAN", // >
    lessEqual = "LESS_EQUAL", // <=
    greaterEqual = "GREATER_EQUAL", // >=
    simpleEquals = "SIMPLE_EQUALS", // =

    // Arithmetic Operators
    plus = "PLUS", // +
    minus = "MINUS", // -
    multiply = "MULTIPLY", // *
    divide = "DIVIDE", // /
    modulo = "MODULO", // ^
    power = "POWER", // ^

    // Unary Operators & Special Single-Character Operators
    bang = "BANG", // !

    // Increment/Decrement
    increment = "INCREMENT", // ++
    decrement = "DECREMENT", // --    // Logical Operators
    and = "AND", // .AND.
    or = "OR", // .OR.
    not = "NOT", // .NOT.
    logicalAnd = "LOGICAL_AND", // &&
    logicalOr = "LOGICAL_OR", // ||
    logicalNot = "LOGICAL_NOT", // !

    // SQL Parameters
    sqlParameter = "SQL_PARAMETER", // ?param? or ?    // Delimiters
    colon = "COLON", // :
    semicolon = "SEMICOLON", // ;
    comma = "COMMA", // ,
    lparen = "LPAREN", // (
    rparen = "RPAREN", // )
    lbracket = "LBRACKET", // [
    rbracket = "RBRACKET", // ]
    lbrace = "LBRACE", // {
    rbrace = "RBRACE", // }
    pipe = "PIPE", // | (for code blocks)
    lBracePipe = "LBRACE_PIPE", // {|
    dot = "DOT", // .
    questionMark = "QUESTION_MARK", // ?    // Quote tokens for string delimiters
    doubleQuote = "DOUBLE_QUOTE", // "
    singleQuote = "SINGLE_QUOTE", // '

    // Literals
    stringLiteral = "STRING_LITERAL",
    numberLiteral = "NUMBER_LITERAL",
    booleanLiteral = "BOOLEAN_LITERAL", // .T. .F.
    nilLiteral = "NIL_LITERAL", // NIL
    dateLiteral = "DATE_LITERAL", // {year,month,day}
    codeBlockLiteral = "CODE_BLOCK_LITERAL", // {|x| expr}
    identifier = "IDENTIFIER",

    // Comments
    blockComment = "BLOCK_COMMENT",
    singleLineComment = "SINGLE_LINE_COMMENT",
    regionComment = "REGION_COMMENT",
    endregionComment = "ENDREGION_COMMENT",

    // Special
    newline = "NEWLINE",
    whitespace = "WHITESPACE",
    eof = "EOF",
    invalid = "INVALID",
}

export interface Token {
    type: TokenType;
    value: string;
    position: Position;
    length: number;
}

export interface Position {
    line: number;
    column: number;
    offset: number;
}

export interface StringState {
    inString: boolean;
    delimiter: string;
    // Store the precise start of the string literal
    stringStartLine: number;
    stringStartColumn: number;
    stringStartOffset: number;
}

/**
 * SSL Tokenizer class for lexical analysis
 */
export class SSLTokenizer {
    private input: string;
    private position: number;
    private line: number;
    private column: number;
    private tokens: Token[];
    private stringState: StringState; // SSL Keywords mapping
    private static readonly keywords = new Map<string, TokenType>([
        // Control flow
        ["PROCEDURE", TokenType.procedure],
        ["ENDPROC", TokenType.endproc],
        ["IF", TokenType.if],
        ["ELSE", TokenType.else],
        ["ENDIF", TokenType.endif],
        ["WHILE", TokenType.while],
        ["ENDWHILE", TokenType.endwhile],
        ["FOR", TokenType.for],
        ["TO", TokenType.to],
        ["NEXT", TokenType.next],
        ["BEGINCASE", TokenType.begincase],
        ["CASE", TokenType.case],
        ["OTHERWISE", TokenType.otherwise],
        ["ENDCASE", TokenType.endcase],
        ["TRY", TokenType.try],
        ["CATCH", TokenType.catch],
        ["FINALLY", TokenType.finally],
        ["ENDTRY", TokenType.endtry],
        ["ERROR", TokenType.error],

        // Declarations
        ["PARAMETERS", TokenType.parameters],
        ["DECLARE", TokenType.declare],
        ["DEFAULT", TokenType.default],
        ["PUBLIC", TokenType.public],
        ["INCLUDE", TokenType.include],
        ["CLASS", TokenType.class],
        ["ENDCLASS", TokenType.endclass], // Added
        ["INHERIT", TokenType.inherit], // Control
        ["RETURN", TokenType.return],
        ["EXITWHILE", TokenType.exitwhile],
        ["EXITFOR", TokenType.exitfor],
        ["EXITCASE", TokenType.exitcase],
        ["LOOP", TokenType.loop],
        ["LABEL", TokenType.label],

        // Regions
        ["REGION", TokenType.region],
        ["ENDREGION", TokenType.endregion],
        ["BEGININLINECODE", TokenType.begininlinecode],
        ["ENDINLINECODE", TokenType.endinlinecode],

        // Literals
        ["NIL", TokenType.nilLiteral],

        // Built-in Functions / Commands (should be matched case-insensitively like other keywords)
        ["DOPROC", TokenType.doProc],
        ["EXECFUNCTION", TokenType.execFunction],
        ["EXECUDF", TokenType.execUDF],
        ["BRANCH", TokenType.branch],
        ["SQLEXECUTE", TokenType.sqlExecute],
        ["LSEARCH", TokenType.lSearch],
        ["CREATEUDOBJECT", TokenType.createUDObject],
        ["_AND", TokenType.bitwiseAnd],
        ["_OR", TokenType.bitwiseOr],
        ["_XOR", TokenType.bitwiseXor],
        ["_NOT", TokenType.bitwiseNot],
    ]);

    constructor(input: string) {
        this.input = input;
        this.position = 0;
        this.line = 1;
        this.column = 1;
        this.tokens = [];
        this.stringState = {
            inString: false,
            delimiter: "",
            stringStartLine: 1,
            stringStartColumn: 1,
            stringStartOffset: 0,
        };
    }

    /**
     * Tokenize the input string and return an array of tokens
     */
    public tokenize(): Token[] {
        this.tokens = [];
        this.position = 0;
        this.line = 1;
        this.column = 1;
        this.stringState = {
            inString: false,
            delimiter: "",
            stringStartLine: 1,
            stringStartColumn: 1,
            stringStartOffset: 0,
        };

        while (this.position < this.input.length) {
            this.scanToken();
        }

        // Handle unterminated string at EOF
        if (this.stringState.inString) {
            const unterminatedStringValue = this.input.substring(
                this.stringState.stringStartOffset,
                this.position
            );
            this.addToken(
                TokenType.invalid, // Changed from TokenType.stringLiteral
                unterminatedStringValue,
                this.stringState.stringStartLine,
                this.stringState.stringStartColumn,
                this.stringState.stringStartOffset
            );
        }
        // Add EOF token
        this.addToken(TokenType.eof, "", this.line, this.column, this.position);

        return this.tokens;
    }

    private current(): string {
        if (this.position >= this.input.length) {
            return "\0"; // Return null character if at or past the end
        }
        return this.input[this.position];
    }

    private peek(): string {
        if (this.position + 1 >= this.input.length) {
            return "\0"; // Return null character if next position is at or past the end
        }
        return this.input[this.position + 1];
    }

    private scanToken(): void {
        const tokenStartOffset = this.position;
        // Capture initial line and column *before* advancing,
        // as advance() might change them if it consumes a newline directly.
        // However, the standard approach is that the token's position IS where it starts.
        // If advance() consumes a newline, the *next* token starts on the new line.
        // The current logic seems to be:
        // 1. Save current line/col/offset (these are for the char we are about to consume)
        // 2. Consume char (advance moves position and column)
        // 3. If consumed char was newline, then the *next* token will have updated line/col.

        // Let's try to capture line/col *before* the advance that consumes the token's first char.
        let tokenStartLine = this.line;
        let tokenStartColumn = this.column;

        const char = this.advance(); // Consumes current char and moves position forward
        // If the character just advanced was a newline, then the token itself is the newline.
        // Its starting position was correct. The main line/column update for the *next* token
        // happens in the switch cases for '\n' and '\r'.

        if (this.stringState.inString) {
            // Pass the char that was just consumed by advance()
            this.scanStringContent(char, tokenStartLine, tokenStartColumn, tokenStartOffset);
            return;
        }
        if (char === '"') {
            // Start string parsing mode for double-quoted strings
            this.startString(char, tokenStartLine, tokenStartColumn, tokenStartOffset);
            return;
        }

        if (char === "'") {
            // Start string parsing mode for single-quoted strings
            this.startString(char, tokenStartLine, tokenStartColumn, tokenStartOffset);
            return;
        }
        if (char === "[") {
            if (this.isStringDelimiterBracket()) {
                // Emit separate bracket token even for string brackets
                this.addToken(
                    TokenType.lbracket,
                    char,
                    tokenStartLine,
                    tokenStartColumn,
                    tokenStartOffset
                );
                return;
            } else {
                this.addToken(
                    TokenType.lbracket,
                    char,
                    tokenStartLine,
                    tokenStartColumn,
                    tokenStartOffset
                );
                return;
            }
        }

        if (char === "/" && this.current() === "*") {
            // scanComment will use tokenStartLine, tokenStartColumn, tokenStartOffset
            // It also handles its own internal line/column advances for multi-line comments.
            this.scanComment(tokenStartLine, tokenStartColumn, tokenStartOffset);
            return;
        }

        switch (char) {
            case ":":
                if (this.current() === "=") {
                    // Check char at new this.position
                    this.advance();
                    this.addToken(
                        TokenType.assign,
                        ":=",
                        tokenStartLine,
                        tokenStartColumn,
                        tokenStartOffset
                    );
                } else {
                    this.addToken(
                        TokenType.colon,
                        ":",
                        tokenStartLine,
                        tokenStartColumn,
                        tokenStartOffset
                    );
                }
                break;
            case "+":
                if (this.current() === "=") {
                    // Check char at new this.position
                    this.advance();
                    this.addToken(
                        TokenType.plusAssign,
                        "+=",
                        tokenStartLine,
                        tokenStartColumn,
                        tokenStartOffset
                    );
                } else if (this.current() === "+") {
                    // Check char at new this.position
                    this.advance();
                    this.addToken(
                        TokenType.increment,
                        "++",
                        tokenStartLine,
                        tokenStartColumn,
                        tokenStartOffset
                    );
                } else {
                    this.addToken(
                        TokenType.plus,
                        "+",
                        tokenStartLine,
                        tokenStartColumn,
                        tokenStartOffset
                    );
                }
                break;
            case "-":
                if (this.current() === "=") {
                    // Check char at new this.position
                    this.advance();
                    this.addToken(
                        TokenType.minusAssign,
                        "-=",
                        tokenStartLine,
                        tokenStartColumn,
                        tokenStartOffset
                    );
                } else if (this.current() === "-") {
                    // Check char at new this.position
                    this.advance();
                    this.addToken(
                        TokenType.decrement,
                        "--",
                        tokenStartLine,
                        tokenStartColumn,
                        tokenStartOffset
                    );
                } else {
                    this.addToken(
                        TokenType.minus,
                        "-",
                        tokenStartLine,
                        tokenStartColumn,
                        tokenStartOffset
                    );
                }
                break;
            case "*":
                if (this.current() === "=") {
                    // Check char at new this.position
                    this.advance();
                    this.addToken(
                        TokenType.multAssign,
                        "*=",
                        tokenStartLine,
                        tokenStartColumn,
                        tokenStartOffset
                    );
                } else {
                    this.addToken(
                        TokenType.multiply,
                        "*",
                        tokenStartLine,
                        tokenStartColumn,
                        tokenStartOffset
                    );
                }
                break;
            case "/":
                if (this.current() === "=") {
                    // Check char at new this.position
                    this.advance();
                    this.addToken(
                        TokenType.divAssign,
                        "/=",
                        tokenStartLine,
                        tokenStartColumn,
                        tokenStartOffset
                    );
                } else {
                    this.addToken(
                        TokenType.divide,
                        "/",
                        tokenStartLine,
                        tokenStartColumn,
                        tokenStartOffset
                    );
                }
                break;
            case "^":
                if (this.current() === "=") {
                    // Check char at new this.position
                    this.advance();
                    this.addToken(
                        TokenType.powerAssign,
                        "^=",
                        tokenStartLine,
                        tokenStartColumn,
                        tokenStartOffset
                    );
                } else {
                    this.addToken(
                        TokenType.power,
                        "^",
                        tokenStartLine,
                        tokenStartColumn,
                        tokenStartOffset
                    );
                }
                break;
            case "=":
                if (this.current() === "=") {
                    // Check char at new this.position
                    this.advance();
                    this.addToken(
                        TokenType.equals,
                        "==",
                        tokenStartLine,
                        tokenStartColumn,
                        tokenStartOffset
                    );
                } else {
                    this.addToken(
                        TokenType.simpleEquals,
                        "=",
                        tokenStartLine,
                        tokenStartColumn,
                        tokenStartOffset
                    );
                }
                break;
            case "!":
                if (this.current() === "=") {
                    // Check char at new this.position
                    this.advance();
                    this.addToken(
                        TokenType.notEquals,
                        "!=",
                        tokenStartLine,
                        tokenStartColumn,
                        tokenStartOffset
                    );
                } else {
                    this.addToken(
                        TokenType.bang,
                        "!",
                        tokenStartLine,
                        tokenStartColumn,
                        tokenStartOffset
                    );
                }
                break;
            case "<":
                if (this.current() === "=") {
                    // Check char at new this.position
                    this.advance();
                    this.addToken(
                        TokenType.lessEqual,
                        "<=",
                        tokenStartLine,
                        tokenStartColumn,
                        tokenStartOffset
                    );
                } else {
                    this.addToken(
                        TokenType.lessThan,
                        "<",
                        tokenStartLine,
                        tokenStartColumn,
                        tokenStartOffset
                    );
                }
                break;
            case ">":
                if (this.current() === "=") {
                    // Check char at new this.position
                    this.advance();
                    this.addToken(
                        TokenType.greaterEqual,
                        ">=",
                        tokenStartLine,
                        tokenStartColumn,
                        tokenStartOffset
                    );
                } else {
                    this.addToken(
                        TokenType.greaterThan,
                        ">",
                        tokenStartLine,
                        tokenStartColumn,
                        tokenStartOffset
                    );
                }
                break;
            case ";":
                this.addToken(
                    TokenType.semicolon,
                    ";",
                    tokenStartLine,
                    tokenStartColumn,
                    tokenStartOffset
                );
                break;
            case ",":
                this.addToken(
                    TokenType.comma,
                    ",",
                    tokenStartLine,
                    tokenStartColumn,
                    tokenStartOffset
                );
                break;
            case "(":
                this.addToken(
                    TokenType.lparen,
                    "(",
                    tokenStartLine,
                    tokenStartColumn,
                    tokenStartOffset
                );
                break;
            case ")":
                this.addToken(
                    TokenType.rparen,
                    ")",
                    tokenStartLine,
                    tokenStartColumn,
                    tokenStartOffset
                );
                break;
            case "[": // Now treated as LBRACKET
                this.addToken(
                    TokenType.lbracket,
                    "[",
                    tokenStartLine,
                    tokenStartColumn,
                    tokenStartOffset
                );
                break;
            case "]":
                this.addToken(
                    TokenType.rbracket,
                    "]",
                    tokenStartLine,
                    tokenStartColumn,
                    tokenStartOffset
                );
                break;
            case "{":
                if (this.current() === "|") {
                    // Check for {| pattern
                    this.advance(); // consume the |
                    this.addToken(
                        TokenType.lBracePipe,
                        "{|",
                        tokenStartLine,
                        tokenStartColumn,
                        tokenStartOffset
                    );
                } else {
                    this.addToken(
                        TokenType.lbrace,
                        "{",
                        tokenStartLine,
                        tokenStartColumn,
                        tokenStartOffset
                    );
                }
                break;
            case "}":
                this.addToken(
                    TokenType.rbrace,
                    "}",
                    tokenStartLine,
                    tokenStartColumn,
                    tokenStartOffset
                );
                break;
            case "%":
                this.addToken(
                    TokenType.modulo,
                    "%",
                    tokenStartLine,
                    tokenStartColumn,
                    tokenStartOffset
                );
                break;
            case "?":
                this.addToken(
                    TokenType.questionMark,
                    "?",
                    tokenStartLine,
                    tokenStartColumn,
                    tokenStartOffset
                );
                break;
            case "|":
                this.addToken(
                    TokenType.pipe,
                    "|",
                    tokenStartLine,
                    tokenStartColumn,
                    tokenStartOffset
                );
                break;
            case "\n": // Actual newline character LF
                this.addToken(
                    TokenType.newline,
                    "\n",
                    tokenStartLine, // The line where \n started
                    tokenStartColumn, // The column where \n started
                    tokenStartOffset
                );
                this.line++; // Next token will be on new line
                this.column = 1; // Next token will start at column 1
                break;
            case "\r": // Actual carriage return character CR
                let newlineValue = "\r";
                // tokenStartLine, tokenStartColumn, tokenStartOffset are for the CR itself.
                if (this.current() === "\n") {
                    // Check for actual LF following CR
                    this.advance(); // Consume the LF
                    newlineValue += "\n";
                }
                this.addToken(
                    TokenType.newline,
                    newlineValue,
                    tokenStartLine,
                    tokenStartColumn,
                    tokenStartOffset
                );
                this.line++; // Next token will be on new line
                this.column = 1; // Next token will start at column 1
                break;
            case " ":
            case "\t":
                this.scanWhitespace(tokenStartLine, tokenStartColumn, tokenStartOffset); // Pass start position
                break;
            case ".":
                this.scanDotOperator(tokenStartLine, tokenStartColumn, tokenStartOffset); // Pass start position
                break;
            default:
                if (this.isDigit(char)) {
                    this.scanNumber(tokenStartLine, tokenStartColumn, tokenStartOffset); // Pass start position
                } else if (this.isAlpha(char)) {
                    this.scanIdentifier(tokenStartLine, tokenStartColumn, tokenStartOffset); // Pass start position
                } else {
                    this.addToken(
                        TokenType.invalid,
                        char,
                        tokenStartLine,
                        tokenStartColumn,
                        tokenStartOffset
                    );
                }
                break;
        }
    }
    private startString(
        openingDelimiterChar: string,
        startLine: number,
        startColumn: number,
        startOffset: number
    ): void {
        this.stringState = {
            inString: true,
            delimiter: openingDelimiterChar, // Can be '"', "'", or "["
            stringStartLine: startLine,
            stringStartColumn: startColumn,
            stringStartOffset: startOffset,
        };
        // The character (opening delimiter) itself is not added as a separate token.
        // scanStringContent will create the full string literal token.
    }

    private scanStringContent(
        charJustAdvanced: string,
        stringStartLine: number,
        stringStartColumn: number,
        stringStartOffset: number
    ): void {
        // charJustAdvanced is the current character *inside* the string, or it could be the closing delimiter.
        // this.position is *after* charJustAdvanced.

        const closingDelimiter =
            this.stringState.delimiter === "[" ? "]" : this.stringState.delimiter;

        if (charJustAdvanced === closingDelimiter) {
            const stringValueWithDelimiters = this.input.substring(
                this.stringState.stringStartOffset, // Use the originally captured start offset
                this.position
            );
            this.addToken(
                TokenType.stringLiteral,
                stringValueWithDelimiters,
                this.stringState.stringStartLine, // Use originally captured start line/col
                this.stringState.stringStartColumn,
                this.stringState.stringStartOffset
            );
            this.stringState.inString = false;
            this.stringState.delimiter = "";
        } else if (charJustAdvanced === "\n") {
            // LF inside a string. The main tokenizer's line/column needs to update.
            this.line++;
            this.column = 1;
        } else if (charJustAdvanced === "\r") {
            // CR inside a string.
            this.line++;
            this.column = 1;
            if (this.current() === "\n") {
                // Check for LF following CR
                this.advance(); // Consume the LF as part of the string's CRLF
            }
        }
        // If still in string and not a special char, just continue.
        // The main loop's advance() will consume the next char.
        // The string token itself is only added once the closing delimiter is found or EOF.
    }

    private scanComment(startLine: number, startColumn: number, startOffset: number): void {
        let commentValue = "/*";
        // The initial \'/\' was consumed by scanToken\'s advance().
        // The \'*\' was peeked by scanToken. We need to consume it now as part of the comment.
        if (this.current() === "*") {
            // Ensure we are indeed at the \'*\'
            this.advance(); // Consume the \'*\'
        } else {
            // This case should ideally not happen if scanToken correctly identifies "/*"
        }

        while (this.position < this.input.length) {
            const charPeek = this.input[this.position]; // Peek current char

            if (charPeek === ";") {
                // SSL comments end with a semicolon
                const consumedChar = this.advance(); // Consume the semicolon
                commentValue += consumedChar;
                break; // End of comment
            } else {
                const consumedChar = this.advance();
                commentValue += consumedChar;
                if (consumedChar === "\n") {
                    this.line++;
                    this.column = 1;
                } else if (consumedChar === "\r") {
                    this.line++;
                    this.column = 1;
                    if (this.current() === "\n") {
                        commentValue += this.advance(); // Consume LF as part of CRLF
                    }
                }
            }
        }

        let tokenType = TokenType.blockComment;
        // Check for region/endregion based on the content *after* "/*"
        // This check should be performed regardless of whether ";" is present,
        // to align with test expectations for unterminated region/endregion comments.
        if (commentValue.length >= 2) {
            // Ensure we have at least "/*"
            const commentContentAfterPrefix = commentValue.substring(2); // Content after "/*"
            const trimmedContent = commentContentAfterPrefix.trimStart(); // Trim leading spaces
            const lowerCommentContent = trimmedContent.toLowerCase();

            if (lowerCommentContent.startsWith("region")) {
                tokenType = TokenType.regionComment;
            } else if (lowerCommentContent.startsWith("endregion")) {
                tokenType = TokenType.endregionComment;
            }
        }

        this.addToken(tokenType, commentValue, startLine, startColumn, startOffset);
    }

    private scanWhitespace(startLine: number, startColumn: number, startOffset: number): void {
        // The first whitespace char was already consumed by advance() in scanToken
        // current() is the next char
        while (
            this.position < this.input.length &&
            (this.current() === " " || this.current() === "\t") // Tab character
        ) {
            this.advance();
        }
        const whitespace = this.input.substring(startOffset, this.position);
        this.addToken(TokenType.whitespace, whitespace, startLine, startColumn, startOffset);
    }

    private scanDotOperator(startLine: number, startColumn: number, startOffset: number): void {
        // The first '.' was already consumed by advance() in scanToken
        // current() is the char after the first '.'
        // Loop to consume the content between dots, e.g., 'AND' in '.AND.'
        while (
            this.position < this.input.length &&
            this.current() !== "." && // Stop at the closing dot
            this.current() !== " " && // Stop at space
            this.current() !== "\n" && // Stop at LF
            this.current() !== "\r" && // Stop at CR
            this.current() !== ";" && // Stop at semicolon
            this.current() !== "\0" // Stop at EOF marker
        ) {
            // Ensure we are consuming valid identifier characters for the operator content
            if (!this.isAlphaNumeric(this.current())) {
                break; // Stop if not alphanumeric (e.g. .NOT! or .AND+)
            }
            this.advance();
        }

        if (this.position < this.input.length && this.current() === ".") {
            this.advance(); // consume closing '.'
            const operator = this.input.substring(startOffset, this.position).toUpperCase();
            switch (operator) {
                case ".AND.":
                    this.addToken(TokenType.and, operator, startLine, startColumn, startOffset);
                    break;
                case ".OR.":
                    this.addToken(TokenType.or, operator, startLine, startColumn, startOffset);
                    break;
                case ".NOT.":
                    this.addToken(TokenType.not, operator, startLine, startColumn, startOffset);
                    break;
                case ".T.":
                    this.addToken(
                        TokenType.booleanLiteral,
                        operator,
                        startLine,
                        startColumn,
                        startOffset
                    );
                    break;
                case ".F.":
                    this.addToken(
                        TokenType.booleanLiteral,
                        operator,
                        startLine,
                        startColumn,
                        startOffset
                    );
                    break;
                default:
                    // If it was like '.SOMETHINGELSE.' it is invalid
                    this.addToken(TokenType.invalid, operator, startLine, startColumn, startOffset);
                    break;
            }
        } else {
            // Not a valid dot operator (e.g., '.', '.A', or '.AND' not closed by '.')
            const scannedSequence = this.input.substring(startOffset, this.position);

            // If it's just a standalone dot, treat as DOT token
            if (scannedSequence === ".") {
                this.addToken(TokenType.dot, ".", startLine, startColumn, startOffset);
            } else {
                // Otherwise, treat as invalid (e.g., '.A', '.AND' without closing dot)
                this.addToken(
                    TokenType.invalid,
                    scannedSequence,
                    startLine,
                    startColumn,
                    startOffset
                );
            }
        }
    }

    private scanNumber(startLine: number, startColumn: number, startOffset: number): void {
        // The first digit was already consumed by advance() in scanToken
        // current() is the char after the first digit

        // Scan IntegerPart
        while (this.position < this.input.length && this.isDigit(this.current())) {
            this.advance();
        }

        // Handle DecimalPart
        const hasIntegerPart = this.input.substring(startOffset, this.position).length > 0;

        if (
            this.position < this.input.length &&
            this.current() === "." &&
            this.isDigit(this.peek()) // Ensures at least one digit after decimal (DecimalPart ::= "." Digit {Digit})
        ) {
            // A number like ".5" is not allowed if not preceded by an integer part (Note 14: ".5e1" unsupported)
            // However, the EBNF NumberLiteral ::= IntegerPart ( DecimalPart Exponent? )? | IntegerPart
            // implies DecimalPart must be associated with an IntegerPart.
            // The check this.isDigit(char) in the default case of scanToken ensures we start with a digit for scanNumber.
            // So, an IntegerPart is always present before we check for DecimalPart here.

            this.advance(); // consume \'.\'
            while (this.position < this.input.length && this.isDigit(this.current())) {
                this.advance();
            }

            // Handle Exponent (only if DecimalPart was present, as per EBNF and Note 14)
            // Note 14: "7e2" (IntegerPart Exponent) is not supported. Exponent needs DecimalPart.
            if (this.position < this.input.length && this.current().toLowerCase() === "e") {
                // Peek ahead for valid exponent structure: ('-')? Digit+
                // No explicit '+' for exponent (Note 14: "9E+1" not supported)
                let canParseExponent = false;
                if (this.position + 1 < this.input.length) {
                    const charAfterE = this.input[this.position + 1];
                    if (charAfterE === "-") {
                        if (
                            this.position + 2 < this.input.length &&
                            this.isDigit(this.input[this.position + 2])
                        ) {
                            canParseExponent = true;
                        }
                    } else if (this.isDigit(charAfterE)) {
                        canParseExponent = true;
                    }
                }

                if (canParseExponent) {
                    this.advance(); // consume 'e' or 'E'
                    if (this.current() === "-") {
                        this.advance(); // consume '-'
                    }
                    // Consume exponent digits
                    while (this.position < this.input.length && this.isDigit(this.current())) {
                        this.advance();
                    }
                }
            }
        }

        const number = this.input.substring(startOffset, this.position);
        this.addToken(TokenType.numberLiteral, number, startLine, startColumn, startOffset);
    }

    private scanIdentifier(startLine: number, startColumn: number, startOffset: number): void {
        // The first alpha char was already consumed by advance() in scanToken
        // current() is the char after the first alpha char
        while (
            this.position < this.input.length &&
            (this.isAlphaNumeric(this.current()) || this.current() === "_") // Identifiers can include underscores
        ) {
            this.advance();
        }
        const originalText = this.input.substring(startOffset, this.position);
        const upperText = originalText.toUpperCase();
        const tokenType = SSLTokenizer.keywords.get(upperText) || TokenType.identifier;
        this.addToken(tokenType, originalText, startLine, startColumn, startOffset);
    }

    private advance(): string {
        const char = this.current(); // Get char at current position
        // Only advance if not past the end. current() handles end of input.
        if (this.position < this.input.length) {
            this.position++;
            this.column++;
        }
        return char; // Return the char that was at the position *before* advancing
    }

    /**
     * Gets the previous non-whitespace token from the tokens array
     * Returns null if no previous non-whitespace token exists
     */ private getPreviousNonWhitespaceToken(): Token | null {
        for (let i = this.tokens.length - 1; i >= 0; i--) {
            const token = this.tokens[i];
            if (token.type !== TokenType.whitespace && token.type !== TokenType.newline) {
                return token;
            }
        }
        return null;
    }

    /**
     * Determines if a '[' character should be treated as a string delimiter
     * or as an array access operator based on the previous token context
     */
    private isStringDelimiterBracket(): boolean {
        const prevToken = this.getPreviousNonWhitespaceToken();

        // If no previous token, treat as string delimiter
        if (!prevToken) {
            return true;
        } // If previous token is an identifier, it's likely array access: identifier[index]
        if (prevToken.type === TokenType.identifier) {
            return false;
        }

        // If previous token is a closing bracket, it's likely chained access: array[0][1]
        if (prevToken.type === TokenType.rbracket) {
            return false;
        }

        // If previous token is a closing parenthesis, it's likely function result access: func()[index]
        if (prevToken.type === TokenType.rparen) {
            return false;
        }

        // In all other cases, treat as string delimiter
        return true;
    }
    private addToken(
        type: TokenType,
        value: string,
        startLine: number,
        startColumn: number,
        startOffset: number
    ): void {
        this.tokens.push({
            type,
            value,
            position: {
                line: startLine,
                column: startColumn,
                offset: startOffset,
            },
            length: value.length,
        });
    }

    // getCurrentPosition is not strictly needed if addToken always receives start positions
    // and length is derived from value. It might be useful for debugging or other features.
    // For now, it can be kept or removed if unused.
    private getCurrentPosition(): Position {
        return {
            line: this.line,
            column: this.column,
            offset: this.position,
        };
    }

    private isDigit(char: string): boolean {
        return char >= "0" && char <= "9";
    }

    private isAlpha(char: string): boolean {
        // Matches letters and underscore for the start of an identifier
        return (char >= "a" && char <= "z") || (char >= "A" && char <= "Z") || char === "_";
    }

    private isAlphaNumeric(char: string): boolean {
        // Matches letters, numbers, and underscore for subsequent parts of an identifier or keyword
        return this.isAlpha(char) || this.isDigit(char);
    }
}
