
export enum SqlTokenType {
    Keyword,
    Identifier,
    String,
    Number,
    Operator,
    Punctuation,
    Whitespace,
    Unknown,
    Placeholder
}

export interface SqlToken {
    type: SqlTokenType;
    text: string;
    line: number;
    column: number;
    offset: number;
}

import { SQL_KEYWORDS } from '../constants/sql';

export class SqlLexer {
    private input: string;
    private pos: number = 0;
    private line: number = 1;
    private column: number = 1;

    constructor(input: string) {
        this.input = input;
    }

    public tokenize(): SqlToken[] {
        const tokens: SqlToken[] = [];
        this.pos = 0;
        this.line = 1;
        this.column = 1;

        while (this.pos < this.input.length) {
            const char = this.input[this.pos];

            if (/\s/.test(char)) {
                tokens.push(this.readWhitespace());
            } else if (char === '-' && this.peek() === '-') {
                tokens.push(this.readLineComment());
            } else if (char === '/' && this.peek() === '*') {
                tokens.push(this.readBlockComment());
            } else if (char === "'" || char === '"') {
                tokens.push(this.readString());
            } else if (char === '?') {
                // SSL parameter: ?varName?
                tokens.push(this.readSslParameter());
            } else if (/[0-9]/.test(char)) {
                tokens.push(this.readNumber());
            } else if (/[a-zA-Z_]/.test(char)) {
                tokens.push(this.readIdentifierOrKeyword());
            } else if ("(),;.".includes(char)) {
                tokens.push(this.readPunctuation());
            } else if ("+-*/%=!<>".includes(char)) {
                tokens.push(this.readOperator());
            } else {
                tokens.push(this.readUnknown());
            }
        }
        return tokens;
    }

    private peek(offset: number = 1): string {
        if (this.pos + offset >= this.input.length) {return "";}
        return this.input[this.pos + offset];
    }

    private readWhitespace(): SqlToken {
        const start = this.pos;
        const line = this.line;
        const col = this.column;
        let text = "";
        while (this.pos < this.input.length && /\s/.test(this.input[this.pos])) {
            text += this.input[this.pos];
            this.advance();
        }
        return { type: SqlTokenType.Whitespace, text, line, column: col, offset: start };
    }

    private readLineComment(): SqlToken {
        const start = this.pos;
        const line = this.line;
        const col = this.column;
        let text = "";
        // -- comment
        text += this.input[this.pos]; this.advance();
        text += this.input[this.pos]; this.advance();
        while (this.pos < this.input.length && this.input[this.pos] !== '\n') {
            text += this.input[this.pos];
            this.advance();
        }
        return { type: SqlTokenType.Whitespace, text, line, column: col, offset: start }; // Treat as WS or distinct? Treating as WS for now often simplifies
    }

    private readBlockComment(): SqlToken {
        const start = this.pos;
        const line = this.line;
        const col = this.column;
        let text = "";
        text += this.input[this.pos]; this.advance();
        text += this.input[this.pos]; this.advance();
        while (this.pos < this.input.length) {
            const char = this.input[this.pos];
            text += char;
            this.advance();
            if (char === '/' && text.endsWith('*/')) {break;}
        }
        return { type: SqlTokenType.Whitespace, text, line, column: col, offset: start };
    }

    private readString(): SqlToken {
        const start = this.pos;
        const line = this.line;
        const col = this.column;
        const quote = this.input[this.pos];
        let text = quote;
        this.advance();

        while (this.pos < this.input.length) {
            const char = this.input[this.pos];
            text += char;
            this.advance();
            if (char === quote) {
                // Check double quote escape in SQL: 'It''s'
                if (this.peek(0) === quote) { // peek(0) is current pos (next char)
                    text += this.input[this.pos];
                    this.advance();
                } else {
                    break;
                }
            }
        }
        return { type: SqlTokenType.String, text, line, column: col, offset: start };
    }

    private readNumber(): SqlToken {
        const start = this.pos;
        const line = this.line;
        const col = this.column;
        let text = "";
        while (this.pos < this.input.length && /[0-9.]/.test(this.input[this.pos])) {
            text += this.input[this.pos];
            this.advance();
        }
        return { type: SqlTokenType.Number, text, line, column: col, offset: start };
    }

    private readIdentifierOrKeyword(): SqlToken {
        const start = this.pos;
        const line = this.line;
        const col = this.column;
        let text = "";
        while (this.pos < this.input.length && /[a-zA-Z0-9_]/.test(this.input[this.pos])) {
            text += this.input[this.pos];
            this.advance();
        }
        const type = SQL_KEYWORDS.has(text.toUpperCase()) ? SqlTokenType.Keyword : SqlTokenType.Identifier;
        return { type, text, line, column: col, offset: start };
    }

    private readPunctuation(): SqlToken {
        const start = this.pos;
        const line = this.line;
        const col = this.column;
        let text = this.input[this.pos];
        this.advance();
        return { type: SqlTokenType.Punctuation, text, line, column: col, offset: start };
    }

    private readOperator(): SqlToken {
        const start = this.pos;
        const line = this.line;
        const col = this.column;
        let text = this.input[this.pos];
        this.advance();
        // Check for multi-char operators: <=, >=, <>, !=
        if (text === '<' || text === '>' || text === '!') {
            if (this.pos < this.input.length && this.input[this.pos] === '=') {
                text += '=';
                this.advance();
            } else if (text === '<' && this.pos < this.input.length && this.input[this.pos] === '>') {
                text += '>';
                this.advance();
            }
        }
        return { type: SqlTokenType.Operator, text, line, column: col, offset: start };
    }

    private readSslParameter(): SqlToken {
        const start = this.pos;
        const line = this.line;
        const col = this.column;
        let text = '?';
        this.advance();

        // Check if this looks like an SSL parameter ?var?
        // SSL parameters shouldn't contain spaces.
        // Scan forward to see if we find a closing '?' before any INVALID separators
        let tempPos = this.pos;
        let foundClosing = false;
        let pDepth = 0;

        while (tempPos < this.input.length) {
            const c = this.input[tempPos];

            if (c === '(') {
                pDepth++;
            } else if (c === ')') {
                if (pDepth > 0) {pDepth--;}
            }

            if (c === '?' && pDepth === 0) {
                foundClosing = true;
                break;
            }

            // Only convert separators to break if we are NOT inside parens
            if (pDepth === 0) {
                if (/\s/.test(c) || ",;".includes(c) || "+-*/%=!<>".includes(c)) {
                    // Hit separator before closing ?
                    break;
                }
            }
            tempPos++;
        }

        if (foundClosing) {
            // It is an SSL parameter, consume until closing ?
            while (this.pos < this.input.length && this.input[this.pos] !== '?') {
                text += this.input[this.pos];
                this.advance();
            }
            if (this.pos < this.input.length && this.input[this.pos] === '?') {
                text += '?';
                this.advance();
            }
        } else {
            // It is just a single '?' placeholder, stop here (already consumed initial ?)
        }

        // Treat SSL parameters AND single ? as Placeholders
        return { type: SqlTokenType.Placeholder, text, line, column: col, offset: start };
    }

    private readUnknown(): SqlToken {
        const start = this.pos;
        const line = this.line;
        const col = this.column;
        let text = this.input[this.pos];
        this.advance();
        return { type: SqlTokenType.Unknown, text, line, column: col, offset: start };
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
