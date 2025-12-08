import { ALL_SQL_FUNCTIONS } from '../constants/language';
import { SQL } from "../constants/sql";
import { SqlToken, SqlTokenType } from './sqlLexer';
import { SqlFormattingState } from './sqlFormattingState';

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
        this.indentString = options.insertSpaces ? ' '.repeat(options.tabSize) : '\t';
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
        let baseIndent = baseIndentStr !== undefined ? baseIndentStr + this.indentString : ' '.repeat(baseIndentColumn);
        const maxLineLen = this.options.wrapLength || 90;
        let currentLineLen = baseIndentColumn;

        const breakBeforeKeywords = SQL.FORMATTING.BREAK_BEFORE;
        const joinModifiers = SQL.FORMATTING.JOIN_MODIFIERS;
        const indentedKeywords = SQL.FORMATTING.INDENTED;

        let result = startChar;
        let isFirstToken = true;

        const tokens = sqlTokens.filter((t: SqlToken) => t.type !== SqlTokenType.Whitespace);

        const isComplex = tokens.some(t => {
            const up = t.text.toUpperCase();
            return up === 'FROM' || up === 'WHERE' || up === 'JOIN' || up === 'GROUP' || up === 'ORDER' || up === 'UNION' || up === 'VALUES' || up === 'SET' || (up === 'SELECT' && tokens.length > 5);
        });

        if (isComplex) {
            result += '\n' + baseIndent;
            currentLineLen = baseIndentColumn;
        }

        if (tokens.length === 0) {
            return startChar + closeQuote;
        }

        const processedRanges = this.preprocessParenLists(tokens, baseIndent, maxLineLen);

        const state = new SqlFormattingState();
        let currentClause = '';

        for (let i = 0; i < tokens.length; i++) {
            const t = tokens[i];
            const upperText = t.text.toUpperCase();

            // Check if this is a pre-processed VALUES/INSERT list
            if (processedRanges.has(i)) {
                const formattedList = processedRanges.get(i)!;
                const prev = i > 0 ? tokens[i - 1] : null;
                if (prev && this.shouldAddSqlSpace(prev, t)) {
                    result += ' ';
                }
                result += '(\n' + baseIndent + '    ' + formattedList + '\n' + baseIndent + ')';

                let depth = 1;
                i++;
                while (i < tokens.length && depth > 0) {
                    if (tokens[i].text === '(') { depth++; }
                    else if (tokens[i].text === ')') { depth--; }
                    i++;
                }
                i--;

                currentLineLen = baseIndentColumn + 1;
                isFirstToken = false;
                continue;
            }

            // Track clause
            if (t.type === SqlTokenType.Keyword) {
                if (SQL.FORMATTING.BREAK_BEFORE.has(upperText) || upperText === 'SET' || upperText === 'SELECT' || upperText === 'UPDATE' || upperText === 'INSERT') {
                    currentClause = upperText;
                }
            }

            const prev = i > 0 ? tokens[i - 1] : null;
            let currentIterOffset = state.lineHasExtraIndent ? 4 : 0;

            state.poppedClosingIndent = -1;

            let isInsertParen = false;
            if (t.text === '(') {
                if (state.inInsertColumnList && prev && prev.type === SqlTokenType.Identifier) {
                    isInsertParen = true;
                } else if (prev && (prev.text.toUpperCase() === SQL.KEYWORDS.VALUES || prev.text.toUpperCase() === SQL.KEYWORDS.SET)) {
                    isInsertParen = true;
                }
            }

            // Handle paren stack
            if (t.text === '(') {
                let extraOffset = isInsertParen ? 4 : 0;
                const prevToken = i > 0 ? tokens[i - 1] : null;
                const nextToken = i + 1 < tokens.length ? tokens[i + 1] : null;
                if (prevToken && prevToken.text === '=' && nextToken && nextToken.text.toUpperCase() === SQL.KEYWORDS.SELECT) {
                    extraOffset = 4;
                } else if (nextToken && nextToken.text.toUpperCase() === SQL.KEYWORDS.SELECT) {
                    extraOffset = 4;
                }

                const currentStackOffset = state.getCurrentStackOffset();
                const inlineIndent = (state.parenDepth * 4) + currentStackOffset + extraOffset + (state.lineHasExtraIndent ? 4 : 0);
                state.pushParen(currentIterOffset + extraOffset, inlineIndent);
            }

            const isClosingParen = t.text === ')';
            let tokenText = t.text;

            state.updateContext(t);

            // Apply casing
            if (t.type === SqlTokenType.Keyword) {
                tokenText = this.applyKeywordCasing(tokenText);
            } else if (t.type === SqlTokenType.Identifier) {
                const next = i + 1 < tokens.length ? tokens[i + 1] : null;
                const isFunction = next && next.text === '(' && !state.inInsertColumnList;
                tokenText = this.applyIdentifierCasing(tokenText, !!isFunction);
            } else if (t.type === SqlTokenType.Placeholder) {
                tokenText = t.text;
            }

            // Check for break conditions
            let needsBreak = false;
            let extraIndent = '';
            const next = i < tokens.length - 1 ? tokens[i + 1] : null;

            if (!isFirstToken) {
                if (i > 0) {
                    const prevToken = tokens[i - 1];
                    const prevUpper = prevToken.text.toUpperCase();

                    if (prevToken.text === ',' && currentClause === 'SET' && state.parenDepth === 0) {
                        needsBreak = true;
                        extraIndent = this.options.indentSpaces ? ' '.repeat(this.options.indentSpaces) : this.indentString;
                    }

                    if ((prevUpper === 'SET' || prevUpper === 'VALUES') && state.parenDepth === 0) {
                        needsBreak = true;
                        extraIndent = this.options.indentSpaces ? ' '.repeat(this.options.indentSpaces) : this.indentString;
                    }
                }

                if (t.text === '(') {
                    if ((currentClause === 'VALUES' || currentClause === 'INTO' || currentClause === 'INSERT') && state.parenDepth === 0) {
                        needsBreak = true;
                        extraIndent = this.options.indentSpaces ? ' '.repeat(this.options.indentSpaces) : this.indentString;
                    } else if (next && next.text.toUpperCase() === SQL.KEYWORDS.SELECT) {
                        needsBreak = true;
                        extraIndent = '   ';
                    }
                }

                if (isClosingParen) {
                    const ctx = state.popParen();
                    if (ctx && ctx.isMultiline) {
                        needsBreak = true;
                    }
                } else if (upperText === SQL.KEYWORDS.SET && state.afterUpdate) {
                    needsBreak = false;
                } else if (prev && prev.text.toUpperCase() === SQL.KEYWORDS.SET) {
                    needsBreak = true;
                } else if (upperText === SQL.KEYWORDS.SELECT && prev && prev.text === '(') {
                    needsBreak = true;
                } else if (upperText === SQL.KEYWORDS.SELECT && state.inInsert && prev && prev.text === ')') {
                    needsBreak = true;
                } else if (upperText === SQL.KEYWORDS.JOIN && prev && joinModifiers.has(prev.text.toUpperCase())) {
                    needsBreak = false;
                } else if (upperText === SQL.KEYWORDS.INTO && prev && prev.text.toUpperCase() === SQL.KEYWORDS.INSERT) {
                    needsBreak = false;
                } else if (prev && prev.text.toUpperCase() === SQL.KEYWORDS.VALUES && t.text === '(') {
                    needsBreak = true;
                } else if (state.inInsertColumnList && t.text === '(' && prev && prev.type === SqlTokenType.Identifier) {
                    needsBreak = true;
                } else if (state.inInsertColumnList && prev && prev.text === '(') {
                    needsBreak = true;
                } else if (upperText === 'SET' && prev && prev.text === ')') {
                    needsBreak = true;
                } else if (t.text === '(' && prev && prev.text.toUpperCase() === 'SET') {
                    needsBreak = true;
                    extraIndent = '    ';
                } else if (prev && prev.text === '(' && (currentClause === 'SET' || currentClause === 'INSERT' || currentClause === 'VALUES' || currentClause === 'INTO')) {
                    needsBreak = true;
                } else if (breakBeforeKeywords.has(upperText)) {
                    needsBreak = true;
                    if (upperText === SQL.KEYWORDS.ON) {
                        extraIndent = '    ';
                    }
                } else if (indentedKeywords.has(upperText)) {
                    needsBreak = true;
                    if (this.options.indentSpaces) {
                        extraIndent = ' '.repeat(this.options.indentSpaces);
                    } else {
                        extraIndent = this.indentString;
                    }
                }
            } else {
                if (t.text === ')') { state.parenDepth--; }
            }

            // Check for Table.Column pattern
            const tableCol = this.combineTableColumn(tokens, i);
            if (tableCol) {
                tokenText = tableCol.text;
                i += tableCol.skip;
            }

            // Check for || concatenation in SELECT
            if (!needsBreak && t.text === '||' && state.inSelectColumns && prev) {
                needsBreak = true;
                extraIndent = '       ';
            }

            // Proactive wrapping
            if (!needsBreak && prev) {
                const wouldNeedSpace = this.shouldAddSqlSpace(prev, t);
                const spaceLen = wouldNeedSpace ? 1 : 0;
                const projectedLen = currentLineLen + spaceLen + tokenText.length;

                const canBreak = (prev.text === ',' || (t.type === SqlTokenType.Keyword || t.type === SqlTokenType.Identifier) || t.text === '||') && prev.text !== '.';

                if (projectedLen > maxLineLen && canBreak && prev.text !== '(') {
                    needsBreak = true;
                    if (state.inSelectColumns) {
                        extraIndent = '       ';
                    } else if (!state.inInsert) {
                        extraIndent = '    ';
                    }
                }
            }

            if (needsBreak && state.parenStack.length > 0) {
                state.parenStack[state.parenStack.length - 1].isMultiline = true;
            }

            // Add spacing or line break
            if (needsBreak) {
                state.lineHasExtraIndent = extraIndent.length > 0;
                let stackOffset = state.getCurrentStackOffset();

                if (t.text === '(' && state.parenStack.length > 0) {
                    stackOffset -= state.parenStack[state.parenStack.length - 1].indentOffset;
                    const newIndent = (state.parenDepth * 4) + stackOffset + extraIndent.length;
                    state.parenStack[state.parenStack.length - 1].closingIndent = newIndent;
                }

                let indentStr = '';
                if (isClosingParen && state.poppedClosingIndent !== -1) {
                    indentStr = ' '.repeat(state.poppedClosingIndent);
                } else {
                    const parenIndent = '    '.repeat(Math.max(0, state.parenDepth));
                    const structIndent = ' '.repeat(stackOffset);
                    indentStr = parenIndent + structIndent + extraIndent;
                }

                const indent = baseIndent + indentStr;
                result += '\n' + indent;
                currentLineLen = baseIndentColumn + indentStr.length;
            } else if (prev && this.shouldAddSqlSpace(prev, t)) {
                result += ' ';
                currentLineLen += 1;
            }

            result += tokenText;
            currentLineLen += tokenText.length;
            isFirstToken = false;
        }

        if (isComplex) {
            result += '\n' + baseIndent;
        }
        result += closeQuote;
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
            if (prev.text === '||') { return true; }
            return true;
        }

        const isAtom = (t: SqlToken) => t.type === SqlTokenType.Keyword || t.type === SqlTokenType.Identifier || t.type === SqlTokenType.Number || t.type === SqlTokenType.Placeholder || t.type === SqlTokenType.String;

        if (isAtom(prev) && isAtom(curr)) { return true; }
        if (prev.text === ')' && isAtom(curr)) { return true; }
        return false;
    }
}

