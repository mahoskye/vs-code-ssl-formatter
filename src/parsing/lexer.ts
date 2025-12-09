import { SSL_COMPOUND_OPERATORS, SSL_OPERATORS, SSL_LITERALS, SSL_KEYWORDS } from '../constants/language';

export enum TokenType {
    Whitespace,
    Comment,
    String,
    Number,
    Keyword,
    Identifier,
    Operator,
    Punctuation,
    Unknown,
    EOF
}

export interface Token {
    type: TokenType;
    text: string;
    line: number;
    column: number;
    offset: number;
    sqlTokens?: any[]; // Avoiding circular dependency for now, effectively SqlToken[]
}

export class Lexer {
    private input: string = "";
    private pos: number = 0;
    private line: number = 1;
    private column: number = 1;

    constructor(input: string) {
        this.input = input;
    }

    public tokenize(): Token[] {
        const tokens: Token[] = [];
        this.pos = 0;
        this.line = 1;
        this.column = 1;

        while (this.pos < this.input.length) {
            const char = this.input[this.pos];

            if (this.isWhitespace(char)) {
                tokens.push(this.readWhitespace());
            } else if (char === '/' && this.peek() === '*') {
                tokens.push(this.readBlockComment());
            } else if (char === ';') {
                tokens.push(this.readPunctuation());
            } else if (char === '.') {
                if (this.isDigit(this.peek())) {
                    tokens.push(this.readNumber());
                } else {
                    tokens.push(this.readDotOperatorOrBoolean());
                }
            } else if (char === ':' && this.isKeywordStart(tokens)) {
                tokens.push(this.readKeyword());
            } else if (char === ':' && this.peek() === '=') {
                tokens.push(this.readAssignmentOperator());
            } else if (char === ':') {
                tokens.push(this.readPunctuation()); // Member access or just colon
            } else if (char === '"' || char === '\'') {
                tokens.push(this.readString());
            } else if (char === '[') {
                // Context sensitive: String literal or Array Access?
                if (this.isArrayAccessContext(tokens)) {
                    tokens.push(this.readPunctuation());
                } else {
                    tokens.push(this.readString());
                }
            } else if (this.isDigit(char)) {
                tokens.push(this.readNumber());
            } else if (this.isIdentifierStart(char)) {
                tokens.push(this.readIdentifier());
            } else if (this.isOperatorChar(char)) {
                tokens.push(this.readOperator());
            } else if (this.isPunctuation(char)) {
                tokens.push(this.readPunctuation());
            } else {
                tokens.push(this.readUnknown());
            }
        }

        tokens.push({
            type: TokenType.EOF,
            text: "",
            line: this.line,
            column: this.column,
            offset: this.pos
        });

        return tokens;
    }

    private peek(offset: number = 1): string {
        if (this.pos + offset >= this.input.length) { return ""; }
        return this.input[this.pos + offset];
    }



    private isArrayAccessContext(tokens: Token[]): boolean {
        // Look back for last non-whitespace/comment token
        for (let i = tokens.length - 1; i >= 0; i--) {
            const t = tokens[i];
            if (t.type === TokenType.Whitespace || t.type === TokenType.Comment) { continue; }

            // Term tokens: Identifier, Number, String, ), ]
            if (t.type === TokenType.Identifier || t.type === TokenType.Number || t.type === TokenType.String || t.type === TokenType.Keyword) { return true; }
            if (t.type === TokenType.Punctuation && (t.text === ')' || t.text === ']')) { return true; }

            return false;
        }
        return false;
    }

    // Helpers
    private isWhitespace(char: string): boolean {
        return /\s/.test(char);
    }

    private isDigit(char: string): boolean {
        return /[0-9]/.test(char);
    }

    private isAlpha(char: string): boolean {
        return /[a-zA-Z_]/.test(char);
    }

    private isIdentifierStart(char: string): boolean {
        return /[a-zA-Z_]/.test(char);
    }

    private isIdentifierPart(char: string): boolean {
        return /[a-zA-Z0-9_]/.test(char);
    }

    private isOperatorChar(char: string): boolean {
        return "+-*/^%=!<>#$".includes(char);
    }

    private isPunctuation(char: string): boolean {
        return "(),{};".includes(char);
    }

    // Token Readers

    private readWhitespace(): Token {
        const start = this.pos;
        const line = this.line;
        const col = this.column;
        let text = "";

        while (this.pos < this.input.length && this.isWhitespace(this.input[this.pos])) {
            const char = this.input[this.pos];
            text += char;
            this.advance();
        }

        return { type: TokenType.Whitespace, text, line, column: col, offset: start };
    }

    private readBlockComment(): Token {
        // /* ... ;
        // SSL comments start with /* and end with ;
        // They can span multiple lines.
        const start = this.pos;
        const line = this.line;
        const col = this.column;
        let text = "";

        // Consume /*
        text += this.input[this.pos]; this.advance();
        text += this.input[this.pos]; this.advance();

        while (this.pos < this.input.length) {
            const char = this.input[this.pos];
            text += char;
            this.advance();

            if (char === ';') {
                break;
            }
        }

        return { type: TokenType.Comment, text, line, column: col, offset: start };
    }



    private readString(): Token {
        const start = this.pos;
        const line = this.line;
        const col = this.column;
        const quote = this.input[this.pos];
        let text = "";

        text += quote;
        this.advance();

        const closeQuote = quote === '[' ? ']' : quote;
        let isEscaped = false;

        while (this.pos < this.input.length) {
            const char = this.input[this.pos];
            text += char;
            this.advance();

            if (isEscaped) {
                isEscaped = false;
                continue;
            }

            if (char === '\\' && quote !== '[') {
                // Brackets generally don't use backslash escapes in this context? 
                // Assuming standard quotes support escaping.
                isEscaped = true;
                continue;
            }

            if (char === closeQuote) {
                break;
            }
        }

        return { type: TokenType.String, text, line, column: col, offset: start };
    }

    private readNumber(): Token {
        const start = this.pos;
        const line = this.line;
        const col = this.column;
        let text = "";

        while (this.pos < this.input.length) {
            const char = this.input[this.pos];
            if (this.isDigit(char) || char === '.') {
                // Handle 1.2.3 case? No.
                // Handle scientific? 1.2e-3
                text += char;
                this.advance();
            } else if (char.toLowerCase() === 'e' && (this.peek() === '+' || this.peek() === '-' || this.isDigit(this.peek()))) {
                text += char;
                this.advance();
                if (this.input[this.pos] === '+' || this.input[this.pos] === '-') {
                    text += this.input[this.pos];
                    this.advance();
                }
            } else {
                break;
            }
        }

        return { type: TokenType.Number, text, line, column: col, offset: start };
    }

    private isKeywordStart(tokens: Token[]): boolean {
        // Check if : followed by Alpha
        // Exception: If preceded by Identifier, it's likely Member Access (e.g. obj:Method or ns:Var)
        // unless it's a known pattern? No, usually keywords start statements.
        // SSL keywords are mostly control flow (:IF, :ELSE).
        // They rarely follow an identifier directly without punctuation.
        // E.g. "IF cond :THEN" (does not exist).

        if (tokens.length > 0) {
            const last = tokens[tokens.length - 1];
            // If preceding token is Identifier, assumed to be Member Access (:)
            // e.g. "batchData:crtRunNo"
            if (last.type === TokenType.Identifier || last.text === ')' || last.text === ']') {
                return false;
            }
        }

        return this.isAlpha(this.peek());
    }

    private readKeyword(): Token {
        const start = this.pos;
        const line = this.line;
        const col = this.column;
        let text = "";

        // Consume :
        text += this.input[this.pos];
        this.advance();

        while (this.pos < this.input.length && this.isIdentifierPart(this.input[this.pos])) {
            text += this.input[this.pos];
            this.advance();
        }

        return { type: TokenType.Keyword, text, line, column: col, offset: start };
    }

    private readIdentifier(): Token {
        const start = this.pos;
        const line = this.line;
        const col = this.column;
        let text = "";

        while (this.pos < this.input.length && this.isIdentifierPart(this.input[this.pos])) {
            text += this.input[this.pos];
            this.advance();
        }

        // Check if keyword (case insensitive or sensitive? Language seems case insensitive for keywords usually, but constants says uppercased keys)
        // SSL keywords are case insensitive usually.
        const upper = text.toUpperCase();
        if ((SSL_KEYWORDS as readonly string[]).includes(upper)) {
            return { type: TokenType.Keyword, text, line, column: col, offset: start };
        }

        return { type: TokenType.Identifier, text, line, column: col, offset: start };
    }

    private readDotOperatorOrBoolean(): Token {
        const start = this.pos;
        const line = this.line;
        const col = this.column;
        let text = "";

        // Consume first dot
        text += this.input[this.pos];
        this.advance();

        while (this.pos < this.input.length) {
            const char = this.input[this.pos];
            text += char;
            this.advance();
            if (char === '.') { break; }
            if (!this.isAlpha(char)) { break; } // Error case or just text
        }

        // Check if valid
        const upper = text.toUpperCase();
        if ((SSL_LITERALS as readonly string[]).includes(upper)) {
            return { type: TokenType.Keyword, text, line, column: col, offset: start };
        }
        if ((SSL_OPERATORS as readonly string[]).includes(upper)) {
            return { type: TokenType.Operator, text, line, column: col, offset: start };
        }

        return { type: TokenType.Unknown, text, line, column: col, offset: start };
    }

    private readOperator(): Token {
        const start = this.pos;
        const line = this.line;
        const col = this.column;
        let text = "";

        // Handle multi-char operators: :=, +=, -=, *=, /=, ==, !=, >=, <=, <>
        const char = this.input[this.pos];
        text += char;
        this.advance();

        const next = this.pos < this.input.length ? this.input[this.pos] : "";
        const twoChar = char + next;

        if ((SSL_COMPOUND_OPERATORS as readonly string[]).includes(twoChar)) {
            text += next;
            this.advance();
        }

        return { type: TokenType.Operator, text, line, column: col, offset: start };
    }

    private readAssignmentOperator(): Token {
        const start = this.pos;
        const line = this.line;
        const col = this.column;
        let text = "";

        // Consume : and =
        text += this.input[this.pos]; this.advance();
        text += this.input[this.pos]; this.advance();

        return { type: TokenType.Operator, text, line, column: col, offset: start };
    }

    private readPunctuation(): Token {
        const start = this.pos;
        const line = this.line;
        const col = this.column;
        let text = this.input[this.pos];
        this.advance();
        return { type: TokenType.Punctuation, text, line, column: col, offset: start };
    }

    private readUnknown(): Token {
        const start = this.pos;
        const line = this.line;
        const col = this.column;
        let text = this.input[this.pos];
        this.advance();
        return { type: TokenType.Unknown, text, line, column: col, offset: start };
    }

    private advance() {
        if (this.input[this.pos] === '\n') {
            this.line++;
            this.column = 1;
        } else {
            this.column++;
        }
        this.pos++;
    }
}
