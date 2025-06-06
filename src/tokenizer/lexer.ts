import { Token, createToken, createPosition, Position } from "./token";
import { TokenType } from "./tokenType";
import {
    isWhitespace,
    isNewline,
    isIdentifierStart,
    isIdentifierPart,
    peekChar,
    peekChars,
} from "./utils";
import {
    createUnexpectedCharacterError,
    createUnterminatedStringError,
    createUnterminatedCommentError,
    TokenizerError,
} from "./errors";
import { getKeywordType } from "./patterns/keywords";
import { matchOperator, matchLogicalOperator } from "./patterns/operators";
import { matchLiteral, canStartLiteral } from "./patterns/literals";
import { matchPunctuation, isPunctuation } from "./patterns/punctuation";

/**
 * Options for the lexer
 */
export interface LexerOptions {
    includeWhitespace?: boolean;
    includeNewlines?: boolean;
    includeComments?: boolean;
}

/**
 * Main lexer class for SSL language
 */
export class Lexer {
    private input: string;
    private position: number = 0;
    private line: number = 0;
    private column: number = 0;
    private options: LexerOptions;
    private tokens: Token[] = []; // Track tokens for context analysis

    constructor(input: string, options: LexerOptions = {}) {
        this.input = input;
        this.options = {
            includeWhitespace: false,
            includeNewlines: true,
            includeComments: true,
            ...options,
        };
    }
    /**
     * Tokenizes the entire input and returns all tokens
     */
    public tokenize(): Token[] {
        this.tokens = []; // Reset tokens array

        while (!this.isAtEnd()) {
            try {
                const token = this.nextToken();
                if (token) {
                    this.tokens.push(token);
                }
            } catch (error) {
                if (error instanceof TokenizerError) {
                    // Create an error token and continue
                    const errorToken = createToken(
                        TokenType.UNKNOWN,
                        this.currentChar(),
                        this.getCurrentPosition()
                    );
                    errorToken.error = error.message;
                    this.tokens.push(errorToken);
                    this.advance();
                } else {
                    throw error;
                }
            }
        }

        // Add EOF token
        this.tokens.push(createToken(TokenType.EOF, "", this.getCurrentPosition()));

        return this.tokens;
    }

    /**
     * Gets the next token from the input
     */
    public nextToken(): Token | null {
        this.skipWhitespace();

        if (this.isAtEnd()) {
            return null;
        }

        const startPos = this.getCurrentPosition();
        const char = this.currentChar();

        // Handle newlines
        if (isNewline(char)) {
            return this.readNewline(startPos);
        }

        // Handle comments
        if (char === "/" && this.peek() === "*") {
            return this.readComment(startPos);
        }

        // Handle identifiers and keywords
        if (isIdentifierStart(char)) {
            return this.readIdentifier(startPos);
        }

        // Handle literals
        if (canStartLiteral(char)) {
            const literalResult = this.readLiteral(startPos);
            if (literalResult) {
                return literalResult;
            }
        }

        // Handle logical operators and boolean literals (starting with .)
        if (char === ".") {
            const logicalResult = this.readLogicalOperator(startPos);
            if (logicalResult) {
                return logicalResult;
            }
        } // Handle operators
        const operatorResult = this.readOperator(startPos);
        if (operatorResult) {
            return operatorResult;
        }

        // Handle SQL parameters (must come before punctuation to avoid ? being matched as punctuation)
        if (char === "?") {
            return this.readSqlParameter(startPos);
        }

        // Handle punctuation
        const punctuationResult = this.readPunctuation(startPos);
        if (punctuationResult) {
            return punctuationResult;
        }

        // Unknown character
        throw createUnexpectedCharacterError(char, startPos);
    }

    /**
     * Reads whitespace and optionally returns a token
     */
    private skipWhitespace(): void {
        while (!this.isAtEnd() && isWhitespace(this.currentChar())) {
            if (this.options.includeWhitespace) {
                // If we want to include whitespace tokens, we should handle this differently
                break;
            }
            this.advance();
        }
    }

    /**
     * Reads a newline token
     */
    private readNewline(startPos: Position): Token | null {
        let text = "";

        // Handle different newline formats (\r\n, \n, \r)
        if (this.currentChar() === "\r") {
            text += this.currentChar();
            this.advance();
            if (this.currentChar() === "\n") {
                text += this.currentChar();
                this.advance();
            }
        } else if (this.currentChar() === "\n") {
            text += this.currentChar();
            this.advance();
        }

        this.line++;
        this.column = 0;

        return this.options.includeNewlines
            ? createToken(TokenType.NEWLINE, text, startPos, this.getCurrentPosition())
            : null;
    }

    /**
     * Reads a comment token
     */
    private readComment(startPos: Position): Token | null {
        if (this.currentChar() !== "/" || this.peek() !== "*") {
            return null;
        }

        let text = "";
        text += this.currentChar(); // '/'
        this.advance();
        text += this.currentChar(); // '*'
        this.advance();

        // Read until we find ';' (SSL comment terminator)
        while (!this.isAtEnd() && this.currentChar() !== ";") {
            if (isNewline(this.currentChar())) {
                this.line++;
                this.column = 0;
            }
            text += this.currentChar();
            this.advance();
        }

        if (this.isAtEnd()) {
            throw createUnterminatedCommentError(startPos);
        }

        // Include the terminating semicolon
        text += this.currentChar(); // ';'
        this.advance(); // Determine comment type
        let tokenType = TokenType.SINGLE_LINE_COMMENT; // Default for single-line comments
        const content = text.slice(2, -1).trim(); // Remove /* and ;

        if (content.toLowerCase().startsWith("region")) {
            tokenType = TokenType.REGION_COMMENT;
        } else if (content.toLowerCase().startsWith("endregion")) {
            tokenType = TokenType.ENDREGION_COMMENT;
        } else if (content.includes("\n")) {
            tokenType = TokenType.BLOCK_COMMENT; // Multi-line comments are block comments
        }
        // Default to SINGLE_LINE_COMMENT for single-line /* ... ; syntax

        return this.options.includeComments
            ? createToken(tokenType, text, startPos, this.getCurrentPosition())
            : null;
    }

    /**
     * Reads an identifier or keyword token
     */
    private readIdentifier(startPos: Position): Token {
        let text = "";

        while (!this.isAtEnd() && isIdentifierPart(this.currentChar())) {
            text += this.currentChar();
            this.advance();
        }

        // Check if it's a keyword
        const keywordType = getKeywordType(text);
        const tokenType = keywordType || TokenType.IDENTIFIER;

        return createToken(tokenType, text, startPos, this.getCurrentPosition());
    }
    /**
     * Reads a literal token
     */
    private readLiteral(startPos: Position): Token | null {
        const literalMatch = matchLiteral(this.input, this.position);
        if (literalMatch) {
            const [text, tokenType, parsedValue] = literalMatch;
            this.advance(text.length);

            // Check if this is a string in a SQL context
            let finalTokenType = tokenType;
            if (tokenType === TokenType.STRING && this.isInSqlContext()) {
                finalTokenType = TokenType.SQL_STRING;
            }

            const token = createToken(finalTokenType, text, startPos, this.getCurrentPosition());
            token.parsedValue = parsedValue;
            return token;
        }

        return null;
    }

    /**
     * Reads a logical operator or boolean literal starting with '.'
     */
    private readLogicalOperator(startPos: Position): Token | null {
        const logicalMatch = matchLogicalOperator(this.input, this.position);
        if (logicalMatch) {
            const [text, tokenType] = logicalMatch;
            this.advance(text.length);

            const token = createToken(tokenType, text, startPos, this.getCurrentPosition());
            if (tokenType === TokenType.BOOLEAN) {
                token.parsedValue = text.toUpperCase() === ".T.";
            }
            return token;
        }

        return null;
    }

    /**
     * Reads an operator token
     */
    private readOperator(startPos: Position): Token | null {
        const operatorMatch = matchOperator(this.input, this.position);
        if (operatorMatch) {
            const [text, tokenType] = operatorMatch;
            this.advance(text.length);
            return createToken(tokenType, text, startPos, this.getCurrentPosition());
        }

        return null;
    }
    /**
     * Reads a punctuation token
     */
    private readPunctuation(startPos: Position): Token | null {
        const punctuationMatch = matchPunctuation(this.input, this.position);
        if (punctuationMatch) {
            const [text, tokenType] = punctuationMatch;
            this.advance(text.length);
            return createToken(tokenType, text, startPos, this.getCurrentPosition());
        }

        return null;
    }

    /**
     * Reads an SQL parameter token (?paramName? or ?)
     */
    private readSqlParameter(startPos: Position): Token {
        let text = "?";
        this.advance(); // Skip first ?

        // Check if it's a named parameter (?paramName?)
        if (isIdentifierStart(this.currentChar())) {
            while (!this.isAtEnd() && isIdentifierPart(this.currentChar())) {
                text += this.currentChar();
                this.advance();
            }

            if (this.currentChar() === "?") {
                text += "?";
                this.advance();
                return createToken(
                    TokenType.SQL_PARAM_NAMED,
                    text,
                    startPos,
                    this.getCurrentPosition()
                );
            }
        }

        // It's just a ? (unnamed parameter)
        return createToken(TokenType.SQL_PARAM_UNNAMED, "?", startPos, this.getCurrentPosition());
    }

    /**
     * Gets the current character
     */
    private currentChar(): string {
        return this.isAtEnd() ? "" : this.input[this.position];
    }

    /**
     * Peeks at the next character without advancing
     */
    private peek(offset: number = 1): string {
        return peekChar(this.input, this.position + offset);
    }

    /**
     * Advances the position by the specified number of characters
     */
    private advance(count: number = 1): void {
        for (let i = 0; i < count && !this.isAtEnd(); i++) {
            if (isNewline(this.currentChar())) {
                this.line++;
                this.column = 0;
            } else {
                this.column++;
            }
            this.position++;
        }
    }

    /**
     * Checks if we've reached the end of input
     */
    private isAtEnd(): boolean {
        return this.position >= this.input.length;
    }

    /**
     * Gets the current position
     */
    private getCurrentPosition(): Position {
        return createPosition(this.line, this.column, this.position);
    }

    /**
     * Checks if we're currently in a SQL function call context
     * Looks at the last few tokens to detect patterns like "SqlExecute(" or "LSearch("
     */
    private isInSqlContext(): boolean {
        if (this.tokens.length < 2) {
            return false;
        }

        // Look for patterns: IDENTIFIER LPAREN where IDENTIFIER is a SQL function
        const lastToken = this.tokens[this.tokens.length - 1];
        const secondLastToken = this.tokens[this.tokens.length - 2];

        if (
            lastToken?.type === TokenType.LPAREN &&
            secondLastToken?.type === TokenType.IDENTIFIER
        ) {
            const sqlFunctions = ["sqlexecute", "lsearch", "sqlcommit", "sqlrollback"];
            return sqlFunctions.includes(secondLastToken.value.toLowerCase());
        }

        return false;
    }
}
