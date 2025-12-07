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

        // Let's just assume we continue from baseIndentColumn
        // currentLineLen = baseIndentColumn; 

        // Wait, if we stripped the newlines, we just proceed.
        // But if we DO `needsBreak`, we append `\n` + baseIndent + extra.
        // So baseIndent should still be valid.

        // Initial check: if we removed \n, we just proceed.


        let isFirstToken = true;
        // Filter out whitespace tokens
        const tokens = sqlTokens.filter((t: SqlToken) => t.type !== SqlTokenType.Whitespace);

        if (tokens.length === 0) {
            return startChar + closeQuote;
        }

        // Initialize state
        const state = new SqlFormattingState();
        let currentClause = '';

        for (let i = 0; i < tokens.length; i++) {
            const t = tokens[i];
            const upperText = t.text.toUpperCase();

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
                } else if (prev && prev.text.toUpperCase() === SQL_KW_VALUES) {
                    isInsertParen = true;
                }
            }

            // Handle paren stack
            if (t.text === '(') {
                // Add extra indent for INSERT/VALUES lists to "balance" content visually
                const extraOffset = isInsertParen ? 4 : 0;

                // Calculate inline closing indent (default)
                // Base: (depth) * 4. (depth is incremented in pushParen, so current depth is depth+1 conceptually)
                // But logic was: depth * 4 + stackOffset.
                // Let's replicate exact logic using state methods.

                const currentStackOffset = state.getCurrentStackOffset();
                const inlineIndent = (state.parenDepth * 4) + currentStackOffset + (state.lineHasExtraIndent ? 4 : 0);

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
                    // Preserve casing for tables/columns (identifiers)
                    tokenText = tokenText;
                }
            } else if (t.type === SqlTokenType.Placeholder) { // Placeholder
                // Preserve casing exactly for placeholders
                tokenText = t.text;
            }

            // Check if we need to break line before this token
            let needsBreak = false;
            let extraIndent = '';

            // Check if previous token forces a break (e.g. comma in SET)
            if (i > 0) {
                const prev = tokens[i - 1];
                if (prev.text === ',' && currentClause === 'SET' && state.parenDepth === 0) {
                    needsBreak = true;
                    // Indent assignments under SET?
                    // SET is usually at base indent (or slightly indented).
                    // Assignments should align or indent.
                    // If we just break, it uses currentIndent.
                    // If SET triggered a break/indent (it doesn't by default), we match it.
                    // If SET is inline 'UPDATE table SET', then assignments on new line should indent.
                    extraIndent = this.options.indentSpaces ? ' '.repeat(this.options.indentSpaces) : this.indentString;
                }

                // INSERT INTO table ( ... ) logic?
                // INSERT INTO or VALUES context for parenthesis
                // Break before '(' in INSERT lists to verticalize them?
                // Also break after VALUES?
                const prevUpper = prev.text.toUpperCase();

                // Break after SET or VALUES to start content on new line
                if ((prevUpper === 'SET' || prevUpper === 'VALUES') && state.parenDepth === 0) {
                    needsBreak = true;
                    extraIndent = this.options.indentSpaces ? ' '.repeat(this.options.indentSpaces) : this.indentString;
                }

                // Break AFTER opening paren in VALUES/INSERT/INTO lists
                if (prev.text === '(' && (currentClause === 'VALUES' || currentClause === 'INTO' || currentClause === 'INSERT')) { // Depth will be > 0 now?
                    // Prev token was '('.
                    // State update happens at END of loop?
                    // Let's check state logic.
                    // If state update is end of loop, then parenDepth is 0 (from prev line 76).
                    // If prev was '(', it should have incremented?
                    // BUT we are in loop `i`. `state` is mutated at end of loop `i`.
                    // So at start of `i` (here), `state` reflects AFTER `i-1`.
                    // So `parenDepth` should be 1.
                    if (state.parenDepth > 0) {
                        needsBreak = true;
                        extraIndent = this.options.indentSpaces ? ' '.repeat(this.options.indentSpaces) : this.indentString;
                    }
                }

                if (t.text === '(' && (currentClause === 'VALUES' || currentClause === 'INTO' || currentClause === 'INSERT') && state.parenDepth === 0) {
                    // Force break before opening paren for cleaner lists
                    needsBreak = true;
                    extraIndent = this.options.indentSpaces ? ' '.repeat(this.options.indentSpaces) : this.indentString;
                }
            } else {
                // First token special handling if it's ')'
                if (t.text === ')') {state.parenDepth--;}
            }

            // Check if WE enforce a break AFTER this token?
            // E.g. break after '(' in VALUES list
            if (t.text === '(' && (currentClause === 'VALUES' || currentClause === 'INTO' || currentClause === 'INSERT') && state.parenDepth === 0) { // Should match state? (depth increments after this token processed?)
                // Actually parenDepth is updated later? No, state update happens WHERE?
                // I need to check where `state.update` is called.
            }
            // Look ahead for subquery detection
            const next = i < sqlTokens.length - 1 ? sqlTokens[i + 1] : null; // Changed tokens to sqlTokens

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
                if (t.text === ')') {state.parenDepth--;}
            }


            // Helper to format identifier
            const formatIdent = (tok: any) => {
                if (tok.text.startsWith('?') && tok.text.endsWith('?')) {return tok.text;}
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

            // Proactive wrapping: check if adding this token would exceed line limit
            if (!needsBreak && prev) {
                const wouldNeedSpace = this.shouldAddSqlSpace(prev, t);
                const spaceLen = wouldNeedSpace ? 1 : 0;
                const projectedLen = currentLineLen + spaceLen + tokenText.length;

                // If adding this token would exceed limit and we can safely break, do it
                // Safe break points: after comma, or before identifier/keyword (not operators/parens)
                // CRITICAL: Do NOT break immediately after a dot (e.g. table.column with split logic)
                const canBreak = (prev.text === ',' || (t.type === SqlTokenType.Keyword || t.type === SqlTokenType.Identifier)) && prev.text !== '.';

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

        // End block with newline + base indent + close quote?
        // Tests expect inline close quote.
        result += closeQuote;
        return result;
    }

    private shouldAddSqlSpace(prev: SqlToken, curr: SqlToken): boolean {
        // Space before open paren if prev is Keyword (e.g. AND (, OR (, IN ()
        if (prev.type === SqlTokenType.Keyword && curr.text === '(') {return true;}

        // No space after opening paren
        if (prev.text === '(') {return false;}
        // No space before closing paren  
        if (curr.text === ')') {return false;}
        // No space before comma
        if (curr.text === ',') {return false;}
        // Space after comma
        if (prev.text === ',') {
            // Exception: Unary minus after comma (e.g., , -1) - space after comma but let minus handle itself
            return true;
        }
        // No space around dot (table.column)
        if (prev.text === '.' || curr.text === '.') {return false;}
        // Space around operators (but single space, not double)
        // Exception: Unary minus/plus after comma should not have space before number
        if (prev.type === SqlTokenType.Operator || curr.type === SqlTokenType.Operator) {
            // Unary minus: if prev is '-' or '+' and prev-prev is ',' or '(' - no space after
            if ((prev.text === '-' || prev.text === '+') && curr.type === SqlTokenType.Number) {
                // Check if this looks like unary (e.g., after comma in a list)
                return false;
            }
            return true; // Operator
        }
        // Space between key/ident/num/placeholder
        // Types: 0=Kw, 1=Ident, 3=Num, 8=Placeholder
        const isAtom = (t: SqlToken) => t.type === SqlTokenType.Keyword || t.type === SqlTokenType.Identifier || t.type === SqlTokenType.Number || t.type === SqlTokenType.Placeholder;

        // Space between keywords/identifiers/placeholders
        if (isAtom(prev) && isAtom(curr)) {return true;}

        // Space after closing paren if followed by atom
        if (prev.text === ')' && isAtom(curr)) {return true;}
        return false;
    }
}
