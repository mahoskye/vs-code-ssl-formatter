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

    private applyIdentifierCasing(text: string, isFunction: boolean): string {
        if (isFunction) {
            return text.charAt(0).toUpperCase() + text.slice(1).toLowerCase();
        }
        return text.toLowerCase();
    }

    // --- Pre-processing ---

    /**
     * Pre-process VALUES/INSERT lists for balanced formatting.
     * Returns a map from opening '(' index to formatted content.
     */
    private preprocessParenLists(tokens: SqlToken[], baseIndent: string, maxLineLen: number): Map<number, string> {
        const processedRanges = new Map<number, string>();

        for (let i = 0; i < tokens.length; i++) {
            const t = tokens[i];

            if (t.text === '(') {
                const prev = i > 0 ? tokens[i - 1] : null;
                const prevUpper = prev ? prev.text.toUpperCase() : '';

                const isValuesList = prevUpper === 'VALUES';
                const isInsertColumnList = prev && prev.type === SqlTokenType.Identifier &&
                    i >= 2 && tokens[i - 2].text.toUpperCase() === 'INTO';

                if (isValuesList || isInsertColumnList) {
                    const nextToken = i + 1 < tokens.length ? tokens[i + 1] : null;
                    if (nextToken && nextToken.text.toUpperCase() === 'SELECT') {
                        continue; // Skip subqueries
                    }

                    const items: string[] = [];
                    let currentItem = '';
                    let depth = 1;
                    let j = i + 1;

                    while (j < tokens.length && depth > 0) {
                        const tok = tokens[j];
                        if (tok.text === '(') { depth++; }
                        else if (tok.text === ')') {
                            depth--;
                            if (depth === 0) { break; }
                        }

                        if (tok.text === ',' && depth === 1) {
                            if (currentItem.trim()) { items.push(currentItem.trim()); }
                            currentItem = '';
                        } else {
                            if (currentItem && tok.type !== SqlTokenType.Punctuation &&
                                !currentItem.endsWith('(')) {
                                currentItem += ' ';
                            }
                            currentItem += tok.text;
                        }
                        j++;
                    }
                    if (currentItem.trim()) { items.push(currentItem.trim()); }

                    if (items.length > 0) {
                        const itemIndent = baseIndent + '    ';
                        const formatted = this.formatBalancedList(items, itemIndent, maxLineLen);
                        processedRanges.set(i, formatted);
                    }
                }
            }
        }

        return processedRanges;
    }

    // --- Table.Column Combiner ---

    /**
     * Check if tokens[i] starts a Table.Column pattern.
     * Returns the combined text and number of tokens consumed, or null if not a pattern.
     */
    private combineTableColumn(tokens: SqlToken[], i: number): { text: string; skip: number } | null {
        const t = tokens[i];
        const isIdentOrKw = (tok: SqlToken) => tok.type === SqlTokenType.Identifier || tok.type === SqlTokenType.Keyword;

        if (isIdentOrKw(t) && i + 2 < tokens.length) {
            const n1 = tokens[i + 1];
            const n2 = tokens[i + 2];
            if (n1.text === '.' && isIdentOrKw(n2)) {
                const p1 = t.type === SqlTokenType.Identifier ? t.text.toLowerCase() : t.text.toUpperCase();
                const p2 = n2.type === SqlTokenType.Identifier ? n2.text.toLowerCase() : n2.text.toUpperCase();
                return { text: p1 + '.' + p2, skip: 2 };
            }
        }
        return null;
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

    // --- List Formatting ---

    private formatBalancedList(items: string[], indent: string, maxLen: number): string {
        if (items.length === 0) { return ''; }

        const indentLen = indent.length;
        const availableLen = maxLen - indentLen;

        const totalLen = items.join(', ').length;
        if (totalLen <= availableLen) {
            return items.join(', ');
        }

        const lines: string[] = [];
        let currentLine = '';
        let remaining = availableLen;

        for (let i = 0; i < items.length; i++) {
            const item = items[i];
            const isFirst = currentLine.length === 0;
            const addLen = isFirst ? item.length : item.length + 2;

            if (remaining - addLen >= 0) {
                currentLine += isFirst ? item : ', ' + item;
                remaining -= addLen;
            } else {
                if (item.length > availableLen) {
                    if (currentLine) {
                        lines.push(currentLine);
                        currentLine = '';
                    }

                    const isString = (item.startsWith("'") && item.endsWith("'")) ||
                        (item.startsWith('"') && item.endsWith('"'));

                    if (isString && item.length > 2) {
                        const concatOp = this.options.concatOperator || '||';
                        const quote = item[0];
                        const content = item.slice(1, -1);
                        const overheadPerLine = 6;
                        const contentPerLine = availableLen - overheadPerLine;

                        if (contentPerLine > 10) {
                            const chunks: string[] = [];
                            let pos = 0;

                            while (pos < content.length) {
                                if (pos + contentPerLine >= content.length) {
                                    chunks.push(content.slice(pos));
                                    break;
                                }

                                const searchStart = Math.max(0, pos + contentPerLine - 15);
                                const searchEnd = pos + contentPerLine;
                                const segment = content.slice(searchStart, searchEnd);

                                let breakOffset = -1;
                                const symbolPriority = [',', ' ', ';', ':', '|', '-', '_'];

                                for (const sym of symbolPriority) {
                                    const idx = segment.lastIndexOf(sym);
                                    if (idx >= 0) {
                                        breakOffset = searchStart + idx + 1;
                                        break;
                                    }
                                }

                                if (breakOffset <= pos) {
                                    breakOffset = pos + contentPerLine;
                                }

                                chunks.push(content.slice(pos, breakOffset));
                                pos = breakOffset;
                            }

                            const brokenParts = chunks.map((chunk, idx) => {
                                if (idx === 0) {
                                    return quote + chunk + quote;
                                }
                                return concatOp + ' ' + quote + chunk + quote;
                            });

                            lines.push(brokenParts.join('\n' + indent));
                        } else {
                            lines.push(item);
                        }
                    } else {
                        lines.push(item);
                    }

                    remaining = availableLen;
                } else {
                    if (currentLine) {
                        lines.push(currentLine);
                    }
                    currentLine = item;
                    remaining = availableLen - item.length;
                }
            }
        }

        if (currentLine) {
            lines.push(currentLine);
        }

        return lines.join(',\n' + indent);
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

