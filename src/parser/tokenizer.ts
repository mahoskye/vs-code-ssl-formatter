/**
 * SSL Tokenizer for breaking down SSL code into tokens
 * Based on the EBNF grammar specification
 */

/**
 * Represents a position in a document
 */
export interface Position {
    line: number;
    character: number;
}

/**
 * Represents a range in a document
 */
export interface Range {
    start: Position;
    end: Position;
}

/**
 * Types of tokens in the SSL language based on EBNF grammar
 */
export enum TokenType {
    // Keywords
    keyword, // For :IF, :ELSE, :PROCEDURE, etc.

    // Literals
    stringLiteral, // For string literals like "text", 'text', or [text]
    numberLiteral, // For numbers including integer and floating point
    booleanLiteral, // For .T. and .F.
    nilLiteral, // For NIL
    dateLiteral, // For date literals {YYYY, MM, DD}

    // Operators
    assignmentOperator, // For :=, +=, -=, etc.
    arithmeticOperator, // For +, -, *, /, ^, etc.
    comparisonOperator, // For ==, !=, <, >, <=, >=, =
    logicalOperator, // For .AND., .OR., .NOT.

    // Delimiters
    leftBrace, // For {
    rightBrace, // For }
    leftBracket, // For [
    rightBracket, // For ]
    leftParen, // For (
    rightParen, // For )
    semicolon, // For ;
    colon, // For :
    comma, // For ,
    pipe, // For |

    // Identifiers
    identifier, // For variable names, function names, etc.

    // Comments
    comment, // For /* ... ;
    regionComment, // For /* region ... ;
    endRegionComment, // For /* endregion ... ;

    // Special
    eof, // End of file
    unknown, // For unknown tokens
    whitespace, // For spaces, tabs, newlines
}

/**
 * Represents a token in the SSL language
 */
export interface Token {
    type: TokenType;
    value: string;
    range: Range;
}

/**
 * SSL Tokenizer class responsible for converting SSL code into tokens
 */
export class SSLTokenizer {
    private text: string;
    private currentPos: number = 0;
    private line: number = 0;
    private column: number = 0;
    private tokens: Token[] = [];

    /**
     * Creates a new instance of the SSL Tokenizer
     * @param text The SSL code to tokenize
     */
    constructor(text: string) {
        this.text = text;
    }

    /**
     * Tokenizes the SSL code into tokens
     * @returns An array of tokens
     */
    public tokenize(): Token[] {
        this.tokens = [];
        this.currentPos = 0;
        this.line = 0;
        this.column = 0;

        while (this.currentPos < this.text.length) {
            const char = this.text[this.currentPos];

            // Skip whitespace but track it for proper positioning
            if (this.isWhitespace(char)) {
                this.handleWhitespace();
                continue;
            }

            // Handle comments (/* ... ;)
            if (char === "/" && this.peekNext() === "*") {
                this.tokenizeComment();
                continue;
            }

            // Handle numbers
            if (this.isDigit(char) || (char === "." && this.isDigit(this.peekNext()))) {
                this.tokenizeNumber();
                continue;
            }

            // Handle strings
            if (char === '"' || char === "'" || char === "[") {
                this.tokenizeString(char);
                continue;
            }

            // Handle keywords, identifiers, and boolean literals
            if (this.isLetter(char) || char === ":" || char === ".") {
                this.tokenizeIdentifierOrKeyword();
                continue;
            }

            // Handle operators and delimiters
            this.tokenizeOperatorOrDelimiter();
        }

        // Add EOF token
        this.addToken(TokenType.eof, "", {
            start: { line: this.line, character: this.column },
            end: { line: this.line, character: this.column },
        });

        return this.tokens;
    }

    /**
     * Handles whitespace characters (space, tab, newline)
     */
    private handleWhitespace(): void {
        const start = {
            line: this.line,
            character: this.column,
        };

        let value = "";

        while (
            this.currentPos < this.text.length &&
            this.isWhitespace(this.text[this.currentPos])
        ) {
            const char = this.text[this.currentPos];
            value += char;

            if (char === "\n") {
                this.line++;
                this.column = 0;
            } else {
                this.column++;
            }

            this.currentPos++;
        }

        // We track whitespace tokens for precise source mapping
        this.addToken(TokenType.whitespace, value, {
            start: start,
            end: { line: this.line, character: this.column },
        });
    }
    /**
     * Tokenizes comments in SSL (/* ... ;)
     * SSL comments are different from other languages. They start with /* and end with ;
     * According to the EBNF grammar, there are two types:
     * - BlockComment: /* content ;
     * - SingleLineComment: /* content ;
     * We also need to handle special region comments: /* region Name;
     */
    private tokenizeComment(): void {
        const start = {
            line: this.line,
            character: this.column,
        };

        // Consume the /*
        let value = "";
        value += this.text[this.currentPos++];
        this.column++;
        value += this.text[this.currentPos++];
        this.column++;

        // Check for region comment
        let isRegionComment = false;
        let isEndRegionComment = false;

        // Look ahead to detect region comments without consuming
        const remainingText = this.text.substring(this.currentPos);
        if (remainingText.trim().startsWith("region")) {
            isRegionComment = true;
        } else if (remainingText.trim().startsWith("endregion")) {
            isEndRegionComment = true;
        }

        // Read until ; or end of file
        while (this.currentPos < this.text.length) {
            const char = this.text[this.currentPos];
            value += char;

            if (char === "\n") {
                this.line++;
                this.column = 0;
            } else {
                this.column++;
            }

            this.currentPos++;

            if (char === ";") {
                break;
            }
        }

        // Determine the token type based on content
        if (isRegionComment) {
            this.addToken(TokenType.regionComment, value, {
                start: start,
                end: { line: this.line, character: this.column },
            });
        } else if (isEndRegionComment) {
            this.addToken(TokenType.endRegionComment, value, {
                start: start,
                end: { line: this.line, character: this.column },
            });
        } else {
            this.addToken(TokenType.comment, value, {
                start: start,
                end: { line: this.line, character: this.column },
            });
        }
    }

    /**
     * Tokenizes number literals
     */
    private tokenizeNumber(): void {
        const start = {
            line: this.line,
            character: this.column,
        };

        let value = "";
        let hasDecimal = false;
        let hasExponent = false;

        // Consume initial part (digits before decimal point)
        while (this.currentPos < this.text.length && this.isDigit(this.text[this.currentPos])) {
            value += this.text[this.currentPos++];
            this.column++;
        }

        // Check for decimal point
        if (this.currentPos < this.text.length && this.text[this.currentPos] === ".") {
            // Only add decimal if followed by digit (to distinguish from property access)
            if (
                this.currentPos + 1 < this.text.length &&
                this.isDigit(this.text[this.currentPos + 1])
            ) {
                hasDecimal = true;
                value += this.text[this.currentPos++];
                this.column++;

                // Consume decimal part
                while (
                    this.currentPos < this.text.length &&
                    this.isDigit(this.text[this.currentPos])
                ) {
                    value += this.text[this.currentPos++];
                    this.column++;
                }
            }
        }

        // Check for exponent part (e or E followed by optional sign and digits)
        if (
            this.currentPos < this.text.length &&
            (this.text[this.currentPos] === "e" || this.text[this.currentPos] === "E")
        ) {
            hasExponent = true;
            value += this.text[this.currentPos++];
            this.column++;

            // Optional sign
            if (
                this.currentPos < this.text.length &&
                (this.text[this.currentPos] === "+" || this.text[this.currentPos] === "-")
            ) {
                value += this.text[this.currentPos++];
                this.column++;
            }

            // Must have at least one digit after exponent
            if (this.currentPos < this.text.length && this.isDigit(this.text[this.currentPos])) {
                while (
                    this.currentPos < this.text.length &&
                    this.isDigit(this.text[this.currentPos])
                ) {
                    value += this.text[this.currentPos++];
                    this.column++;
                }
            } else {
                // Invalid exponent format - don't include the e/E in the number
                this.currentPos--;
                this.column--;
                value = value.slice(0, -1);
                hasExponent = false;
            }
        }

        this.addToken(TokenType.numberLiteral, value, {
            start: start,
            end: { line: this.line, character: this.column },
        });
    }

    /**
     * Tokenizes string literals with different delimiters (", ', [)
     * @param delimiter The starting delimiter character
     */
    private tokenizeString(delimiter: string): void {
        const start = {
            line: this.line,
            character: this.column,
        };

        let value = delimiter;
        this.currentPos++;
        this.column++;

        const endDelimiter = delimiter === "[" ? "]" : delimiter;

        while (this.currentPos < this.text.length && this.text[this.currentPos] !== endDelimiter) {
            const char = this.text[this.currentPos];
            value += char;

            if (char === "\n") {
                this.line++;
                this.column = 0;
            } else {
                this.column++;
            }

            this.currentPos++;
        }

        // Include the closing delimiter if available
        if (this.currentPos < this.text.length) {
            value += this.text[this.currentPos++];
            this.column++;
        }

        this.addToken(TokenType.stringLiteral, value, {
            start: start,
            end: { line: this.line, character: this.column },
        });
    }

    /**
     * Tokenizes identifiers and keywords
     */
    private tokenizeIdentifierOrKeyword(): void {
        const start = {
            line: this.line,
            character: this.column,
        };

        let value = "";

        // Special case for keywords that start with :
        if (this.text[this.currentPos] === ":") {
            value += this.text[this.currentPos++];
            this.column++;

            // Read identifier after the colon
            while (
                this.currentPos < this.text.length &&
                (this.isAlphaNumeric(this.text[this.currentPos]) ||
                    this.text[this.currentPos] === "_")
            ) {
                value += this.text[this.currentPos++];
                this.column++;
            }

            // Check if it's a known keyword
            if (this.isKeyword(value)) {
                this.addToken(TokenType.keyword, value, {
                    start: start,
                    end: { line: this.line, character: this.column },
                });
                return;
            }
        }
        // Special case for boolean literals (.T. and .F.)
        else if (
            this.text[this.currentPos] === "." &&
            (this.peekNext() === "T" || this.peekNext() === "F") &&
            this.peek(2) === "."
        ) {
            value += this.text[this.currentPos++]; // .
            this.column++;

            value += this.text[this.currentPos++]; // T or F
            this.column++;

            value += this.text[this.currentPos++]; // .
            this.column++;

            this.addToken(TokenType.booleanLiteral, value, {
                start: start,
                end: { line: this.line, character: this.column },
            });
            return;
        }
        // Special case for logical operators (.AND., .OR., .NOT.)
        else if (
            this.text[this.currentPos] === "." &&
            (this.text.substring(this.currentPos, this.currentPos + 5).toUpperCase() === ".AND." ||
                this.text.substring(this.currentPos, this.currentPos + 4).toUpperCase() ===
                    ".OR." ||
                this.text.substring(this.currentPos, this.currentPos + 5).toUpperCase() === ".NOT.")
        ) {
            // Handle .AND.
            if (
                this.text.substring(this.currentPos, this.currentPos + 5).toUpperCase() === ".AND."
            ) {
                value = ".AND.";
                this.currentPos += 5;
                this.column += 5;
            }
            // Handle .OR.
            else if (
                this.text.substring(this.currentPos, this.currentPos + 4).toUpperCase() === ".OR."
            ) {
                value = ".OR.";
                this.currentPos += 4;
                this.column += 4;
            }
            // Handle .NOT.
            else if (
                this.text.substring(this.currentPos, this.currentPos + 5).toUpperCase() === ".NOT."
            ) {
                value = ".NOT.";
                this.currentPos += 5;
                this.column += 5;
            }

            this.addToken(TokenType.logicalOperator, value, {
                start: start,
                end: { line: this.line, character: this.column },
            });
            return;
        }
        // Handle regular identifiers
        else {
            while (
                this.currentPos < this.text.length &&
                (this.isAlphaNumeric(this.text[this.currentPos]) ||
                    this.text[this.currentPos] === "_")
            ) {
                value += this.text[this.currentPos++];
                this.column++;
            }

            // Check for NIL literal
            if (value.toUpperCase() === "NIL") {
                this.addToken(TokenType.nilLiteral, value, {
                    start: start,
                    end: { line: this.line, character: this.column },
                });
                return;
            }
        }

        // If we got here, it's a regular identifier
        this.addToken(TokenType.identifier, value, {
            start: start,
            end: { line: this.line, character: this.column },
        });
    }

    /**
     * Tokenizes operators and delimiters
     */
    private tokenizeOperatorOrDelimiter(): void {
        const start = {
            line: this.line,
            character: this.column,
        };

        const char = this.text[this.currentPos];
        let type: TokenType = TokenType.unknown;
        let value = char;
        this.currentPos++;
        this.column++;

        // Check for compound operators (two-character operators)
        if (
            char === ":" &&
            this.currentPos < this.text.length &&
            this.text[this.currentPos] === "="
        ) {
            value += this.text[this.currentPos++];
            this.column++;
            type = TokenType.assignmentOperator;
        } else if (
            char === "+" &&
            this.currentPos < this.text.length &&
            this.text[this.currentPos] === "="
        ) {
            value += this.text[this.currentPos++];
            this.column++;
            type = TokenType.assignmentOperator;
        } else if (
            char === "-" &&
            this.currentPos < this.text.length &&
            this.text[this.currentPos] === "="
        ) {
            value += this.text[this.currentPos++];
            this.column++;
            type = TokenType.assignmentOperator;
        } else if (
            char === "*" &&
            this.currentPos < this.text.length &&
            this.text[this.currentPos] === "="
        ) {
            value += this.text[this.currentPos++];
            this.column++;
            type = TokenType.assignmentOperator;
        } else if (
            char === "/" &&
            this.currentPos < this.text.length &&
            this.text[this.currentPos] === "="
        ) {
            value += this.text[this.currentPos++];
            this.column++;
            type = TokenType.assignmentOperator;
        } else if (
            char === "^" &&
            this.currentPos < this.text.length &&
            this.text[this.currentPos] === "="
        ) {
            value += this.text[this.currentPos++];
            this.column++;
            type = TokenType.assignmentOperator;
        } else if (
            char === "=" &&
            this.currentPos < this.text.length &&
            this.text[this.currentPos] === "="
        ) {
            value += this.text[this.currentPos++];
            this.column++;
            type = TokenType.comparisonOperator;
        } else if (
            char === "!" &&
            this.currentPos < this.text.length &&
            this.text[this.currentPos] === "="
        ) {
            value += this.text[this.currentPos++];
            this.column++;
            type = TokenType.comparisonOperator;
        } else if (
            char === "<" &&
            this.currentPos < this.text.length &&
            this.text[this.currentPos] === "="
        ) {
            value += this.text[this.currentPos++];
            this.column++;
            type = TokenType.comparisonOperator;
        } else if (
            char === ">" &&
            this.currentPos < this.text.length &&
            this.text[this.currentPos] === "="
        ) {
            value += this.text[this.currentPos++];
            this.column++;
            type = TokenType.comparisonOperator;
        }
        // Single character operators and delimiters
        else {
            switch (char) {
                case "+":
                case "-":
                case "*":
                case "/":
                case "^":
                case "%":
                    type = TokenType.arithmeticOperator;
                    break;
                case "<":
                case ">":
                case "=":
                    type = TokenType.comparisonOperator;
                    break;
                case "{":
                    type = TokenType.leftBrace;
                    break;
                case "}":
                    type = TokenType.rightBrace;
                    break;
                case "[":
                    type = TokenType.leftBracket;
                    break;
                case "]":
                    type = TokenType.rightBracket;
                    break;
                case "(":
                    type = TokenType.leftParen;
                    break;
                case ")":
                    type = TokenType.rightParen;
                    break;
                case ";":
                    type = TokenType.semicolon;
                    break;
                case ":":
                    type = TokenType.colon;
                    break;
                case ",":
                    type = TokenType.comma;
                    break;
                case "|":
                    type = TokenType.pipe;
                    break;
                default:
                    type = TokenType.unknown;
                    break;
            }
        }

        this.addToken(type, value, {
            start: start,
            end: { line: this.line, character: this.column },
        });
    }

    /**
     * Adds a token to the token list
     * @param type The type of the token
     * @param value The text value of the token
     * @param range The range in the document where the token is located
     */
    private addToken(type: TokenType, value: string, range: Range): void {
        this.tokens.push({
            type,
            value,
            range,
        });
    }

    /**
     * Checks if a character is whitespace (space, tab, newline)
     * @param char The character to check
     * @returns True if the character is whitespace, false otherwise
     */
    private isWhitespace(char: string): boolean {
        return char === " " || char === "\t" || char === "\n" || char === "\r";
    }

    /**
     * Checks if a character is a digit (0-9)
     * @param char The character to check
     * @returns True if the character is a digit, false otherwise
     */
    private isDigit(char: string): boolean {
        return /[0-9]/.test(char);
    }

    /**
     * Checks if a character is a letter (a-z, A-Z)
     * @param char The character to check
     * @returns True if the character is a letter, false otherwise
     */
    private isLetter(char: string): boolean {
        return /[a-zA-Z]/.test(char);
    }

    /**
     * Checks if a character is alphanumeric (a-z, A-Z, 0-9)
     * @param char The character to check
     * @returns True if the character is alphanumeric, false otherwise
     */
    private isAlphaNumeric(char: string): boolean {
        return this.isLetter(char) || this.isDigit(char);
    }

    /**
     * Peeks at the next character without consuming it
     * @returns The next character or an empty string if at the end of the text
     */
    private peekNext(): string {
        if (this.currentPos + 1 >= this.text.length) {
            return "";
        }
        return this.text[this.currentPos + 1];
    }

    /**
     * Peeks at a character at a specified offset from the current position
     * @param offset The offset from the current position
     * @returns The character at the offset or an empty string if out of bounds
     */
    private peek(offset: number): string {
        if (this.currentPos + offset >= this.text.length) {
            return "";
        }
        return this.text[this.currentPos + offset];
    }

    /**
     * Checks if a string is a SSL keyword
     * @param value The string to check
     * @returns True if the string is a keyword, false otherwise
     */
    private isKeyword(value: string): boolean {
        const upperValue = value.toUpperCase();
        const keywords = [
            ":IF",
            ":ELSE",
            ":ENDIF",
            ":WHILE",
            ":ENDWHILE",
            ":EXITWHILE",
            ":FOR",
            ":TO",
            ":NEXT",
            ":EXITFOR",
            ":BEGINCASE",
            ":CASE",
            ":OTHERWISE",
            ":EXITCASE",
            ":ENDCASE",
            ":RETURN",
            ":INCLUDE",
            ":DECLARE",
            ":PARAMETERS",
            ":PROCEDURE",
            ":ENDPROC",
            ":PUBLIC",
            ":LOOP",
            ":LABEL",
            ":REGION",
            ":ENDREGION",
            ":CLASS",
            ":INHERIT",
            ":ERROR",
            ":TRY",
            ":CATCH",
            ":FINALLY",
            ":ENDTRY",
            ":DEFAULT",
            ":BEGININLINECODE",
            ":ENDINLINECODE",
        ];

        return keywords.includes(upperValue);
    }
}
