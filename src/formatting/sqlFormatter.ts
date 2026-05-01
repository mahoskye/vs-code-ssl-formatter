import { SQL } from "../constants/sql";
import { SqlToken, SqlTokenType } from '../parsing/sqlLexer';

export interface SqlFormattingOptions {
    wrapLength: number;
    insertSpaces: boolean;
    tabSize: number;
    keywordCase?: 'upper' | 'lower' | 'title' | 'preserve';
    indentSpaces?: number;
    style?: string;
    concatOperator?: '||' | '+';
}

export class SqlFormatter {
    private options: SqlFormattingOptions;
    private indentString: string;

    constructor(options: SqlFormattingOptions) {
        this.options = options;
        const indentSize = options.indentSpaces ?? 4;
        this.indentString = ' '.repeat(indentSize);
    }

    // --- Casing Helpers ---

    private applyKeywordCasing(text: string): string {
        if (this.options.keywordCase === 'lower') {
            return text.toLowerCase();
        } else if (this.options.keywordCase === 'title') {
            return text.charAt(0).toUpperCase() + text.slice(1).toLowerCase();
        } else if (this.options.keywordCase === 'preserve') {
            return text;
        }
        return text.toUpperCase();
    }

    private applyTokenCasing(token: SqlToken): string {
        if (token.type === SqlTokenType.Keyword) {
            return this.applyKeywordCasing(token.text);
        }
        if (token.type === SqlTokenType.Identifier) {
            return token.text.toLowerCase();
        }
        return token.text;
    }

    // --- Main Formatting ---

    public formatSqlTokens(sqlTokens: SqlToken[], startChar: string, baseIndentColumn: number, baseIndentStr?: string): string {
        const closeQuote = startChar === '[' ? ']' : startChar;
        if (sqlTokens.length === 0) {
            return startChar + closeQuote;
        }

        const baseIndent = baseIndentStr ?? ' '.repeat(baseIndentColumn);
        const tokens = sqlTokens.filter((t: SqlToken) => t.type !== SqlTokenType.Whitespace);
        if (tokens.length === 0) {
            return startChar + closeQuote;
        }

        if (!startChar) {
            return this.formatSqlContent(tokens, baseIndent);
        }

        const formatted = this.formatSqlContent(tokens, baseIndent + this.indentString);
        if (formatted.includes('\n')) {
            return `${startChar}\n${baseIndent}${this.indentString}${formatted}\n${baseIndent}${closeQuote}`;
        }

        return `${startChar}${formatted}${closeQuote}`;
    }

    private formatSqlContent(tokens: SqlToken[], baseIndent: string): string {
        const maxLineLength = this.options.wrapLength || 90;
        const style = this.options.style || 'standard';
        const isComplex = tokens.some(t => {
            const upper = t.text.toUpperCase();
            return upper === SQL.KEYWORDS.FROM || upper === SQL.KEYWORDS.WHERE || upper === SQL.KEYWORDS.JOIN ||
                upper === SQL.KEYWORDS.GROUP || upper === SQL.KEYWORDS.ORDER || upper === SQL.KEYWORDS.UNION ||
                upper === SQL.KEYWORDS.VALUES || upper === SQL.KEYWORDS.SET ||
                (upper === SQL.KEYWORDS.SELECT && tokens.length > 5);
        });

        let result = '';
        let currentLineLen = baseIndent.length;
        let isFirstToken = true;
        let currentClause = '';
        let parenDepth = 0;
        let inSelectColumns = false;

        for (let i = 0; i < tokens.length; i++) {
            const token = tokens[i];
            const upperText = token.text.toUpperCase();
            const prev = i > 0 ? tokens[i - 1] : null;

            if (token.type === SqlTokenType.Keyword) {
                switch (upperText) {
                    case SQL.KEYWORDS.SELECT:
                        currentClause = SQL.KEYWORDS.SELECT;
                        inSelectColumns = true;
                        break;
                    case SQL.KEYWORDS.UPDATE:
                        currentClause = SQL.KEYWORDS.UPDATE;
                        break;
                    case SQL.KEYWORDS.INSERT:
                        currentClause = SQL.KEYWORDS.INSERT;
                        break;
                    case SQL.KEYWORDS.FROM:
                        inSelectColumns = false;
                        currentClause = SQL.KEYWORDS.FROM;
                        break;
                    case SQL.KEYWORDS.WHERE:
                        currentClause = SQL.KEYWORDS.WHERE;
                        break;
                    case SQL.KEYWORDS.SET:
                        currentClause = SQL.KEYWORDS.SET;
                        break;
                    case SQL.KEYWORDS.VALUES:
                        currentClause = SQL.KEYWORDS.VALUES;
                        break;
                    case SQL.KEYWORDS.ORDER:
                    case SQL.KEYWORDS.GROUP:
                        currentClause = upperText;
                        break;
                }
            }

            if (token.text === '(') {
                parenDepth++;
            } else if (token.text === ')') {
                parenDepth--;
                if (parenDepth < 0) {
                    parenDepth = 0;
                }
            }

            const tokenText = this.applyTokenCasing(token);
            let needsBreak = false;
            let extraIndent = '';

            if (!isFirstToken && isComplex) {
                const prevUpper = prev ? prev.text.toUpperCase() : '';

                if (style !== 'compact' && token.type === SqlTokenType.Keyword && SQL.FORMATTING.BREAK_BEFORE.has(upperText)) {
                    if (upperText === SQL.KEYWORDS.JOIN && prev && SQL.FORMATTING.JOIN_MODIFIERS.has(prevUpper)) {
                        needsBreak = false;
                    } else if (upperText === SQL.KEYWORDS.INTO && prevUpper === SQL.KEYWORDS.INSERT) {
                        needsBreak = false;
                    } else {
                        needsBreak = true;
                        if (upperText === SQL.KEYWORDS.ON && style === 'canonicalCompact') {
                            extraIndent = this.indentString;
                        }
                    }
                }

                if ((style === 'canonicalCompact' || style === 'expanded') &&
                    token.type === SqlTokenType.Keyword &&
                    SQL.FORMATTING.INDENTED.has(upperText)) {
                    needsBreak = true;
                    extraIndent = this.indentString;
                }

                if (style !== 'compact' && prev && prevUpper === SQL.KEYWORDS.SET && parenDepth === 0) {
                    needsBreak = true;
                    if (style === 'canonicalCompact' || style === 'expanded') {
                        extraIndent = this.indentString;
                    }
                }

                if (style !== 'compact' && prev && prev.text === ',' && currentClause === SQL.KEYWORDS.SET && parenDepth === 0) {
                    needsBreak = true;
                    if (style === 'canonicalCompact' || style === 'expanded') {
                        extraIndent = this.indentString;
                    }
                }

                if (style !== 'compact' && token.text === '(' && prevUpper === SQL.KEYWORDS.VALUES) {
                    needsBreak = true;
                    if (style === 'canonicalCompact' || style === 'expanded') {
                        extraIndent = this.indentString;
                    }
                }

                if (style !== 'compact' && upperText === SQL.KEYWORDS.SELECT && prev && prev.text === '(') {
                    needsBreak = true;
                    extraIndent = this.indentString;
                }

                if (!needsBreak && prev && maxLineLength > 0 && (style === 'canonicalCompact' || style === 'expanded')) {
                    const spaceLen = this.shouldAddSqlSpace(prev, token) ? 1 : 0;
                    const projectedLen = currentLineLen + spaceLen + tokenText.length;

                    const canBreak = prev.text === ',' ||
                        ((token.type === SqlTokenType.Keyword || token.type === SqlTokenType.Identifier) && prev.text !== '.');

                    if (projectedLen > maxLineLength && canBreak && prev.text !== '(') {
                        needsBreak = true;
                        extraIndent = inSelectColumns ? '       ' : this.indentString;
                    }
                }
            }

            if (needsBreak) {
                const parenIndent = this.indentString.repeat(parenDepth);
                result += `\n${baseIndent}${parenIndent}${extraIndent}`;
                currentLineLen = baseIndent.length + parenIndent.length + extraIndent.length;
            } else if (prev && this.shouldAddSqlSpace(prev, token)) {
                result += ' ';
                currentLineLen += 1;
            }

            result += tokenText;
            currentLineLen += tokenText.length;
            isFirstToken = false;
        }

        return result;
    }


    // --- Spacing ---

    private shouldAddSqlSpace(prev: SqlToken, curr: SqlToken): boolean {
        if (prev.type === SqlTokenType.Keyword && curr.text === '(') { return true; }
        if (prev.text === '(') { return false; }
        if (curr.text === ')') { return false; }
        if (curr.text === ',') { return false; }
        if (prev.text === ',') { return true; }
        if (prev.text === '.' || curr.text === '.') { return false; }

        if (prev.type === SqlTokenType.Operator || curr.type === SqlTokenType.Operator) {
            if ((prev.text === '-' || prev.text === '+') && curr.type === SqlTokenType.Number) {
                return false;
            }
            return true;
        }

        const isAtom = (t: SqlToken) => t.type === SqlTokenType.Keyword || t.type === SqlTokenType.Identifier || t.type === SqlTokenType.Number || t.type === SqlTokenType.Placeholder || t.type === SqlTokenType.String;

        if (isAtom(prev) && isAtom(curr)) { return true; }
        if (prev.text === ')' && isAtom(curr)) { return true; }
        return false;
    }
}

