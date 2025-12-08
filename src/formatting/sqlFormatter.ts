import { ALL_SQL_FUNCTIONS } from '../constants/language';
import {
    SQL_BREAK_BEFORE_KEYWORDS, SQL_JOIN_MODIFIERS, SQL_INDENTED_KEYWORDS,
    SQL_KW_SELECT, SQL_KW_UPDATE, SQL_KW_DELETE, SQL_KW_INSERT, SQL_KW_VALUES,
    SQL_KW_SET, SQL_KW_WHERE, SQL_KW_FROM, SQL_KW_JOIN, SQL_KW_INTO, SQL_KW_ON,
    SQL_KW_AND, SQL_KW_OR
} from '../constants/sql';
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

    public formatSqlTokens(sqlTokens: SqlToken[], startChar: string, baseIndentColumn: number, baseIndentStr?: string): string {
        const closeQuote = startChar === '[' ? ']' : startChar;
        // Use provided baseIndentStr or fall back to spaces if not provided (legacy)
        let baseIndent = baseIndentStr !== undefined ? baseIndentStr + this.indentString : ' '.repeat(baseIndentColumn);
        // Use configured wrap length (default 90 if not set)
        const maxLineLen = this.options.wrapLength || 90;
        let currentLineLen = baseIndentColumn;

        // Keywords that trigger line breaks BEFORE them
        const breakBeforeKeywords = SQL_BREAK_BEFORE_KEYWORDS;

        // Join modifiers - if followed by JOIN, don't break before JOIN
        const joinModifiers = SQL_JOIN_MODIFIERS;

        // Keywords that get extra indentation (under WHERE)
        const indentedKeywords = SQL_INDENTED_KEYWORDS;

        // Block formatting: Start with newline + indent?
        // Canonical compact style prefers inline start.
        // If we force newlines, we break existing tests.
        // Let's try starting inline.
        let result = startChar;
        // Adjust currentLineLen to account for what's already on the line? 
        // We assume startChar is at baseIndentColumn? Or baseIndentColumn IS the start?
        // Typically baseIndentColumn is where the string starts.
        // But if we append to startChar, we are just continuing the line.
        // We need to account for startChar length.
        // NOTE: if startChar is '[', len is 1. SQLExecute(" is longer.
        // The tests seem to imply baseIndentColumn includes the prefix.

        let isFirstToken = true;
        // Filter out whitespace tokens
        const tokens = sqlTokens.filter((t: SqlToken) => t.type !== SqlTokenType.Whitespace);

        // Check for complexity to decide on starting newline
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

        // Pre-process VALUES/INSERT lists for balanced formatting
        // Find parenthesized lists after VALUES or INSERT INTO table(
        const processedRanges = new Map<number, string>(); // Map from '(' index to formatted content

        for (let i = 0; i < tokens.length; i++) {
            const t = tokens[i];

            // Detect VALUES ( or INSERT INTO table (
            if (t.text === '(') {
                const prev = i > 0 ? tokens[i - 1] : null;
                const prevUpper = prev ? prev.text.toUpperCase() : '';

                // Check if this is a VALUES list or INSERT column list
                const isValuesList = prevUpper === 'VALUES';
                const isInsertColumnList = prev && prev.type === SqlTokenType.Identifier &&
                    i >= 2 && tokens[i - 2].text.toUpperCase() === 'INTO';

                if (isValuesList || isInsertColumnList) {
                    // Check if this is a subquery (starts with SELECT) - don't pre-process those
                    const nextToken = i + 1 < tokens.length ? tokens[i + 1] : null;
                    if (nextToken && nextToken.text.toUpperCase() === 'SELECT') {
                        continue; // Skip subqueries, let normal formatting handle them
                    }

                    // Collect items until matching ')'
                    const items: string[] = [];
                    let currentItem = '';
                    let depth = 1;
                    let j = i + 1;

                    while (j < tokens.length && depth > 0) {
                        const tok = tokens[j];
                        if (tok.text === '(') depth++;
                        else if (tok.text === ')') {
                            depth--;
                            if (depth === 0) break;
                        }

                        if (tok.text === ',' && depth === 1) {
                            // End of item
                            if (currentItem.trim()) items.push(currentItem.trim());
                            currentItem = '';
                        } else {
                            // Add to current item with spacing
                            if (currentItem && tok.type !== SqlTokenType.Punctuation &&
                                !currentItem.endsWith('(')) {
                                currentItem += ' ';
                            }
                            currentItem += tok.text;
                        }
                        j++;
                    }
                    if (currentItem.trim()) items.push(currentItem.trim());

                    if (items.length > 0) {
                        // Calculate indent for items (paren depth * 4 + some offset)
                        const itemIndent = baseIndent + '    '; // One level deeper
                        const formatted = this.formatBalancedList(items, itemIndent, maxLineLen);
                        processedRanges.set(i, formatted);
                    }
                }
            }
        }

        // Initialize state
        const state = new SqlFormattingState();
        let currentClause = '';

        for (let i = 0; i < tokens.length; i++) {
            const t = tokens[i];
            const upperText = t.text.toUpperCase();

            // Check if this is a pre-processed VALUES/INSERT list
            if (processedRanges.has(i)) {
                const formattedList = processedRanges.get(i)!;

                // Output the opening paren with line break
                const prev = i > 0 ? tokens[i - 1] : null;
                if (prev && this.shouldAddSqlSpace(prev, t)) {
                    result += ' ';
                }
                result += '(\n' + baseIndent + '    ' + formattedList + '\n' + baseIndent + ')';

                // Skip tokens until matching ')'
                let depth = 1;
                i++;
                while (i < tokens.length && depth > 0) {
                    if (tokens[i].text === '(') depth++;
                    else if (tokens[i].text === ')') depth--;
                    i++;
                }
                i--; // Adjust for loop increment

                // Update line length (we're on the ')' line now)
                currentLineLen = baseIndentColumn + 1;
                isFirstToken = false;
                continue;
            }

            // Track clause
            if (t.type === SqlTokenType.Keyword) {
                if (SQL_BREAK_BEFORE_KEYWORDS.has(upperText) || upperText === 'SET' || upperText === 'SELECT' || upperText === 'UPDATE' || upperText === 'INSERT') {
                    currentClause = upperText;
                }
            }

            // Special handling for commas in SET clause (vertical formatting)
            if (t.text === ',' && currentClause === 'SET' && state.parenDepth === 0) {
                // Break after comma in SET clause
                // We handle this by checking logic below. 
                // Currently `needsBreak` is for BEFORE token.
                // We want break AFTER comma.
                // Loop continues. Next token.
                // We can set a flag `forceBreakNext`?
                // Or check `if (prev.text === ',')` in next iteration.
            }

            // Actually, handle in the 'prev' check of next token or here?
            // If we insert the comma, then we want a newline.
            // But we build result string directly.
            // We append `t.text`.
            // If we want newline after, append it here?
            // But next iteration handles indentation.
            // If we append `\n`, next iteration `startChar` logic might double handle?
            // `needsBreak` causes "\n" + indent BEFORE token.

            // So if we want break AFTER comma:
            // In NEXT iteration (i+1), set `needsBreak = true`.
            const prev = i > 0 ? tokens[i - 1] : null;
            // const upperText = t.text.toUpperCase(); // This line was duplicated in the original snippet, keeping the first one.

            let currentIterOffset = state.lineHasExtraIndent ? 4 : 0;

            // Re-reset popped closing indent for this iteration
            state.poppedClosingIndent = -1;

            // Check for INSERT INTO or VALUES context for parenthesis
            // Only detect INSERT table parens when we're specifically between INSERT INTO and SELECT/VALUES
            let isInsertParen = false;
            if (t.text === '(') {
                if (state.inInsertColumnList && prev && prev.type === SqlTokenType.Identifier) {
                    isInsertParen = true;
                } else if (prev && (prev.text.toUpperCase() === SQL_KW_VALUES || prev.text.toUpperCase() === SQL_KW_SET)) {
                    isInsertParen = true;
                }
            }

            // Handle paren stack
            if (t.text === '(') {
                // Add extra indent for INSERT/VALUES/SET lists to "balance" content visually
                let extraOffset = isInsertParen ? 4 : 0;

                // Check if this is a tuple subquery: = (SELECT ...) pattern
                // Only the = ( pattern needs extra indent, not FROM ( or IN (
                const prevToken = i > 0 ? tokens[i - 1] : null;
                const nextToken = i + 1 < tokens.length ? tokens[i + 1] : null;
                if (prevToken && prevToken.text === '=' &&
                    nextToken && nextToken.text.toUpperCase() === SQL_KW_SELECT) {
                    extraOffset = 4; // Tuple subqueries need their content indented
                } else if (nextToken && nextToken.text.toUpperCase() === SQL_KW_SELECT) {
                    // Subquery in list (e.g. SELECT (SELECT ...))
                    extraOffset = 4;
                }

                const currentStackOffset = state.getCurrentStackOffset();
                // Include extraOffset in closing indent so ) aligns with where ( content starts
                const inlineIndent = (state.parenDepth * 4) + currentStackOffset + extraOffset + (state.lineHasExtraIndent ? 4 : 0);

                state.pushParen(currentIterOffset + extraOffset, inlineIndent);
            }

            // Prepare to close paren
            const isClosingParen = t.text === ')';

            let tokenText = t.text;

            // Update context tracking EARLY to affect casing
            state.updateContext(t);

            // Apply casing rules
            if (t.type === SqlTokenType.Keyword) { // Keyword
                if (this.options.keywordCase === 'lower') {
                    tokenText = tokenText.toLowerCase();
                } else if (this.options.keywordCase === 'title') {
                    tokenText = tokenText.charAt(0).toUpperCase() + tokenText.slice(1).toLowerCase();
                } else if (this.options.keywordCase === 'preserve') {
                    tokenText = tokenText;
                } else {
                    tokenText = tokenText.toUpperCase();
                }
            } else if (t.type === SqlTokenType.Identifier) { // Identifier
                // Check if it's a function (followed by open paren)
                const next = i + 1 < tokens.length ? tokens[i + 1] : null;
                // Exception: If in INSERT column list context, identifier followed by '(' is likely Table(Cols), not Func(Args)
                // So force lowercase. But allow functions in WHERE/JOIN clauses to get PascalCase.
                if (next && next.text === '(' && !state.inInsertColumnList) {
                    // PascalCase for functions
                    tokenText = tokenText.charAt(0).toUpperCase() + tokenText.slice(1).toLowerCase();
                } else {
                    // Force lowercase for tables/columns (identifiers)
                    tokenText = tokenText.toLowerCase();
                }
            } else if (t.type === SqlTokenType.Placeholder) { // Placeholder
                // Preserve casing exactly for placeholders
                tokenText = t.text;
            }

            // Check if we need to break line before this token
            let needsBreak = false;
            let extraIndent = '';

            // Look ahead for subquery detection
            const next = i < tokens.length - 1 ? tokens[i + 1] : null;

            if (!isFirstToken) {
                // Check if previous token forces a break
                if (i > 0) {
                    const prev = tokens[i - 1];
                    const prevUpper = prev.text.toUpperCase();

                    if (prev.text === ',' && currentClause === 'SET' && state.parenDepth === 0) {
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
                    } else if (next && next.text.toUpperCase() === SQL_KW_SELECT) {
                        // Break before subquery in SELECT list: SELECT col, (SELECT...)
                        needsBreak = true;
                        extraIndent = '   '; // 3 spaces
                    }
                }
            } else {
                // First token special handling if it's ')'
                if (t.text === ')') { state.parenDepth--; }
            }

            // Check if WE enforce a break AFTER this token?
            // E.g. break after '(' in VALUES list
            if (t.text === '(' && (currentClause === 'VALUES' || currentClause === 'INTO' || currentClause === 'INSERT') && state.parenDepth === 0) { // Should match state? (depth increments after this token processed?)
                // Actually parenDepth is updated later? No, state update happens WHERE?
                // I need to check where `state.update` is called.
            }

            if (!isFirstToken) {
                if (isClosingParen) {
                    // If closing paren, check if the block was multiline
                    const ctx = state.popParen();
                    const wasMultiline = ctx ? ctx.isMultiline : false;

                    if (wasMultiline) {
                        needsBreak = true;
                    }
                } else if (upperText === SQL_KW_SET && state.afterUpdate) {
                    needsBreak = false;
                } else if (prev && prev.text.toUpperCase() === SQL_KW_SET) {
                    needsBreak = true;
                } else if (upperText === SQL_KW_SELECT && prev && prev.text === '(') {
                    // Subquery SELECT - break to new line, indent from paren stack
                    needsBreak = true;
                } else if (upperText === SQL_KW_SELECT && state.inInsert && prev && prev.text === ')') {
                    // INSERT INTO table (...) SELECT ... - break before SELECT
                    needsBreak = true;
                } else if (upperText === SQL_KW_JOIN && prev && joinModifiers.has(prev.text.toUpperCase())) {
                    needsBreak = false;
                } else if (upperText === SQL_KW_INTO && prev && prev.text.toUpperCase() === SQL_KW_INSERT) {
                    needsBreak = false;
                } else if (prev && prev.text.toUpperCase() === SQL_KW_VALUES && t.text === '(') {
                    // Break before '(' after VALUES
                    needsBreak = true;
                } else if (state.inInsertColumnList && t.text === '(' && prev && prev.type === SqlTokenType.Identifier) {
                    // Break before '(' in INSERT INTO Table(...) - only when in the column list area
                    needsBreak = true;
                } else if (state.inInsertColumnList && prev && prev.text === '(') {
                    // Break AFTER '(' in INSERT/VALUES context (start content on new line)
                    // But only if it was indeed an INSERT paren (detected above for push, but here relying on context)
                    // Since we don't store "isInsertParen" in stack for this check easily (we pushed it),
                    // we can re-derive or check simple logic.
                    // Simple logic: If we are inInsert, and prev was '(', it's likely the list start.
                    needsBreak = true;
                } else if (upperText === 'SET' && prev && prev.text === ')') {
                    // UPDATE ... SET after a possibly complex join/table exp - ensure break
                    needsBreak = true;
                } else if (t.text === '(' && prev && prev.text.toUpperCase() === 'SET') {
                    // UPDATE table SET (col1, col2) = ...
                    // Break before the '(' to start column list on new line
                    needsBreak = true;
                    extraIndent = '    ';
                } else if (prev && prev.text === '(' && (currentClause === 'SET' || currentClause === 'INSERT' || currentClause === 'VALUES' || currentClause === 'INTO')) {
                    // Break AFTER opening paren in these lists to verticalize them
                    needsBreak = true;
                } else if (breakBeforeKeywords.has(upperText)) {
                    needsBreak = true;
                    if (upperText === SQL_KW_ON) {
                        extraIndent = '    ';
                    }
                } else if (indentedKeywords.has(upperText)) {
                    // AND/OR/ON get line break + extra indent regardless of depth
                    needsBreak = true;
                    // Use configured indent spaces or default to tab size (usually 4 unless 2 specified)
                    // Match user feedback preference for standard indentation over 2-space hanging
                    if (this.options.indentSpaces) {
                        extraIndent = ' '.repeat(this.options.indentSpaces);
                    } else {
                        extraIndent = this.indentString;
                    }
                }
            } else {
                // First token special handling if it's ')'
                if (t.text === ')') { state.parenDepth--; }
            }


            // Helper to format identifier
            const formatIdent = (tok: any) => {
                if (tok.text.startsWith('?') && tok.text.endsWith('?')) { return tok.text; }
                return tok.text; // Preserve case
            };

            // Check for Table.Column pattern (Identifier/Keyword + Dot + Identifier/Keyword)
            // Treat as single unit for wrapping purposes
            let isTableCol = false;
            // Allow Identifier (1) or Keyword (0) for table/column names
            const isIdentOrKw = (tok: SqlToken) => tok.type === SqlTokenType.Identifier || tok.type === SqlTokenType.Keyword;

            if (isIdentOrKw(t) && i + 2 < tokens.length) {
                const n1 = tokens[i + 1];
                const n2 = tokens[i + 2];
                if (n1.text === '.' && isIdentOrKw(n2)) {
                    // Re-apply casing to the parts if they are separate?
                    // Or just let them be?
                    // Logic was: tokenText = formatIdent(t) + '.' + formatIdent(n2);
                    // This uses original text.
                    // We probably want to lowercase them too if they are identifiers.

                    const p1 = t.type === SqlTokenType.Identifier ? t.text.toLowerCase() : t.text.toUpperCase(); // crude check
                    const p2 = n2.type === SqlTokenType.Identifier ? n2.text.toLowerCase() : n2.text.toUpperCase();
                    tokenText = p1 + '.' + p2;

                    i += 2; // Advance loop past dot and next identifier
                    isTableCol = true;
                }
            }

            // Check for || concatenation operator in SELECT - should break like AND/OR
            // This provides consistent formatting for concatenated expressions
            if (!needsBreak && t.text === '||' && state.inSelectColumns && prev) {
                // Break before || operator for better readability
                needsBreak = true;
                extraIndent = '       '; // 7 spaces to align with "SELECT "
            }

            // Proactive wrapping: check if adding this token would exceed line limit
            if (!needsBreak && prev) {
                const wouldNeedSpace = this.shouldAddSqlSpace(prev, t);
                const spaceLen = wouldNeedSpace ? 1 : 0;
                const projectedLen = currentLineLen + spaceLen + tokenText.length;

                // If adding this token would exceed limit and we can safely break, do it
                // Safe break points: after comma, before identifier/keyword, or before || concatenation operator
                // CRITICAL: Do NOT break immediately after a dot (e.g. table.column with split logic)
                const canBreak = (prev.text === ',' || (t.type === SqlTokenType.Keyword || t.type === SqlTokenType.Identifier) || t.text === '||') && prev.text !== '.';

                if (projectedLen > maxLineLen && canBreak && prev.text !== '(') {
                    needsBreak = true;
                    // Align SELECT column values with "SELECT " (7 chars) for all SELECT statements
                    needsBreak = true;
                    // Align SELECT column values with "SELECT " (7 chars) for all SELECT statements
                    if (state.inSelectColumns) {
                        extraIndent = '       '; // 7 spaces for "SELECT "
                    } else if (!state.inInsert) {
                        extraIndent = '    ';
                    }
                }
            }

            // Mark stack as multiline if we are breaking
            if (needsBreak && state.parenStack.length > 0) {
                state.parenStack[state.parenStack.length - 1].isMultiline = true;
            }

            // Add spacing or line break
            if (needsBreak) {
                state.lineHasExtraIndent = extraIndent.length > 0;

                // Calculate total offset from stack
                let stackOffset = state.getCurrentStackOffset();

                // If the current token IS the opening paren, it shouldn't inherit the indent offset 
                // that it just created for its contents. It should reside at the outer level (plus paren indent).
                if (t.text === '(' && state.parenStack.length > 0) {
                    stackOffset -= state.parenStack[state.parenStack.length - 1].indentOffset;

                    // Update closing indent to match this new line
                    const newIndent = (state.parenDepth * 4) + stackOffset + extraIndent.length;
                    state.parenStack[state.parenStack.length - 1].closingIndent = newIndent;
                }

                let indentStr = '';

                if (isClosingParen && state.poppedClosingIndent !== -1) {
                    // Use the calculated closing indent for this specific paren
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

        // End block with close quote - add newline for complex queries
        if (isComplex) {
            result += '\n' + baseIndent;
        }
        result += closeQuote;
        return result;
    }

    /**
     * Format a comma-separated list with simple remaining-space algorithm.
     * Tracks remaining space on line, breaks when adding token would go negative.
     */
    private formatBalancedList(items: string[], indent: string, maxLen: number): string {
        if (items.length === 0) return '';

        const indentLen = indent.length;
        const availableLen = maxLen - indentLen;

        // If all items fit on one line
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
            const addLen = isFirst ? item.length : item.length + 2; // +2 for ", "

            if (remaining - addLen >= 0) {
                // Fits on current line
                currentLine += isFirst ? item : ', ' + item;
                remaining -= addLen;
            } else {
                // Doesn't fit - check if token itself exceeds available
                if (item.length > availableLen) {
                    // Oversized token: finish current line first
                    if (currentLine) {
                        lines.push(currentLine);
                        currentLine = '';
                    }

                    // Check if it's a string literal that can be broken
                    const isString = (item.startsWith("'") && item.endsWith("'")) ||
                        (item.startsWith('"') && item.endsWith('"'));

                    if (isString && item.length > 2) {
                        // Break the string using concat operator
                        const concatOp = this.options.concatOperator || '||';
                        const quote = item[0];
                        const content = item.slice(1, -1); // Remove quotes

                        // Calculate how much content fits per line
                        // Account for: quote + content + quote + space + concatOp + space = 6 extra chars
                        const overheadPerLine = 6; // ' ... ' ||
                        const contentPerLine = availableLen - overheadPerLine;

                        if (contentPerLine > 10) {
                            // Break content into chunks at symbol boundaries
                            const chunks: string[] = [];
                            let pos = 0;

                            while (pos < content.length) {
                                if (pos + contentPerLine >= content.length) {
                                    // Last chunk - take the rest
                                    chunks.push(content.slice(pos));
                                    break;
                                }

                                // Find best break point near contentPerLine
                                // Look for symbol boundaries: comma, space, semicolon, colon, pipe, etc.
                                const searchStart = Math.max(0, pos + contentPerLine - 15);
                                const searchEnd = pos + contentPerLine;
                                const segment = content.slice(searchStart, searchEnd);

                                // Find last symbol in segment (prefer comma, then space, then other)
                                let breakOffset = -1;
                                const symbolPriority = [',', ' ', ';', ':', '|', '-', '_'];

                                for (const sym of symbolPriority) {
                                    const idx = segment.lastIndexOf(sym);
                                    if (idx >= 0) {
                                        breakOffset = searchStart + idx + 1; // Break AFTER the symbol
                                        break;
                                    }
                                }

                                if (breakOffset <= pos) {
                                    // No symbol found, fall back to hard break
                                    breakOffset = pos + contentPerLine;
                                }

                                chunks.push(content.slice(pos, breakOffset));
                                pos = breakOffset;
                            }

                            // Build concatenated string - single item with internal newlines
                            // Put concat operator at START of next line (user preference)
                            const brokenParts = chunks.map((chunk, idx) => {
                                if (idx === 0) {
                                    return quote + chunk + quote; // First chunk, no prefix
                                }
                                return concatOp + ' ' + quote + chunk + quote; // Prefix with concat op
                            });

                            // Join broken parts as single item (newline only, no comma)
                            lines.push(brokenParts.join('\n' + indent));
                        } else {
                            // Not enough space to break meaningfully
                            lines.push(item);
                        }
                    } else {
                        // Non-string oversized token, just put on its own line
                        lines.push(item);
                    }

                    // Start fresh line after oversized item
                    remaining = availableLen;
                } else {
                    // Token fits on a new line, wrap to new line
                    if (currentLine) {
                        lines.push(currentLine);
                    }
                    currentLine = item;
                    remaining = availableLen - item.length;
                }
            }
        }

        // Don't forget the last line
        if (currentLine) {
            lines.push(currentLine);
        }

        return lines.join(',\n' + indent);
    }

    private shouldAddSqlSpace(prev: SqlToken, curr: SqlToken): boolean {
        // Space before open paren if prev is Keyword (e.g. AND (, OR (, IN ()
        if (prev.type === SqlTokenType.Keyword && curr.text === '(') { return true; }

        // No space after opening paren
        if (prev.text === '(') { return false; }
        // No space before closing paren  
        if (curr.text === ')') { return false; }
        // No space before comma
        if (curr.text === ',') { return false; }
        // Space after comma
        if (prev.text === ',') {
            // Exception: Unary minus after comma (e.g., , -1) - space after comma but let minus handle itself
            return true;
        }
        // No space around dot (table.column)
        if (prev.text === '.' || curr.text === '.') { return false; }
        // Space around operators (but single space, not double)
        // Exception: Unary minus/plus after comma should not have space before number
        if (prev.type === SqlTokenType.Operator || curr.type === SqlTokenType.Operator) {
            // Unary minus: if prev is '-' or '+' and prev-prev is ',' or '(' - no space after
            if ((prev.text === '-' || prev.text === '+') && curr.type === SqlTokenType.Number) {
                // Check if this looks like unary (e.g., after comma in a list)
                return false;
            }
            // Always add space after || concatenation operator
            if (prev.text === '||') { return true; }
            return true; // Operator
        }
        // Space between key/ident/num/placeholder/string
        // Types: 0=Kw, 1=Ident, 3=Num, 8=Placeholder, 2=String
        const isAtom = (t: SqlToken) => t.type === SqlTokenType.Keyword || t.type === SqlTokenType.Identifier || t.type === SqlTokenType.Number || t.type === SqlTokenType.Placeholder || t.type === SqlTokenType.String;

        // Space between keywords/identifiers/placeholders
        if (isAtom(prev) && isAtom(curr)) { return true; }

        // Space after closing paren if followed by atom
        if (prev.text === ')' && isAtom(curr)) { return true; }
        return false;
    }
}
