
import { Node, NodeType, Parser } from './parser';
import { Token, TokenType, Lexer } from './lexer';
import { SSL_KEYWORDS, ALL_SQL_FUNCTIONS } from '../constants/language';

export interface FormattingOptions {
    tabSize: number;
    insertSpaces: boolean;
    [key: string]: boolean | number | string;
}

export class SSLFormatter {
    private options: FormattingOptions;
    private indentString: string;
    private output: string[] = [];
    private currentIndentLevel: number = 0;

    constructor(options: FormattingOptions) {
        this.options = options;
        this.indentString = options.insertSpaces ? ' '.repeat(options.tabSize) : '\t';
    }

    public format(text: string, initialIndentLevel: number = 0): string {
        const lexer = new Lexer(text);
        const tokens = lexer.tokenize();
        const parser = new Parser(tokens);
        const root = parser.parse();

        // Recursively print nodes
        this.output = [];
        this.currentIndentLevel = initialIndentLevel;

        // Track previous node for vertical whitespace logic
        this.visit(root, undefined);

        // Join, trim, ensure newline
        return this.output.join('\n').trim() + '\n';
    }

    private visit(node: Node, prevSibling?: Node) {
        if (node.type === NodeType.Program) {
            let prev: Node | undefined = undefined;
            for (const child of node.children) {
                if (prev) {
                    const lines = this.getVerticalWhitespace(prev, child);
                    if (lines === -1) {
                        // Trailing inline comment - append to previous line
                        const comment = this.printStatement(child);
                        if (this.output.length > 0) {
                            this.output[this.output.length - 1] += ' ' + comment;
                        }
                        prev = child;
                        continue;
                    }
                    for (let i = 0; i < lines; i++) this.output.push("");
                }
                this.visit(child, prev);
                prev = child;
            }
        } else if (node.type === NodeType.Block) {
            this.currentIndentLevel++;
            let prev: Node | undefined = undefined;
            for (const child of node.children) {
                // Check for vertical whitespace before child
                if (prev) {
                    const lines = this.getVerticalWhitespace(prev, child);
                    if (lines === -1) {
                        // Trailing inline comment - append to previous line
                        const comment = this.printStatement(child);
                        if (this.output.length > 0) {
                            this.output[this.output.length - 1] += ' ' + comment;
                        }
                        prev = child;
                        continue;
                    }
                    for (let i = 0; i < lines; i++) this.output.push("");
                }
                this.visit(child, prev);
                prev = child;
            }
            this.currentIndentLevel--;
        } else if (node.type === NodeType.Statement) {
            // Calculate blank lines if this is a top-level statement (in Program logic above, likely filtered)
            // But 'visit' is called recursively.
            // Vertical whitespace is handled by the parent container loop usually.
            // But here we might check if WE need to prepend strict lines if not handled?
            // Let's rely on the parent loop for "between" nodes.
            // But we might need to handle "before first" or "after last"? 
            // Usually not.

            const line = this.printStatement(node);
            this.output.push(this.getIndent() + line);
        }
    }


    private getVerticalWhitespace(prev: Node, curr: Node): number {
        const currStart = this.getFirstTokenText(curr);
        const prevStart = this.getFirstTokenText(prev);

        // 1. Definition Blocks (PARAMETERS, DEFAULT, DECLARE)
        if (this.isDefinition(prevStart) && this.isDefinition(currStart)) {
            // Group only if same type (e.g. DEFAULT followed by DEFAULT)
            if (prevStart === currStart) {
                return 0;
            }
            return 1;
        }
        if (this.isDefinition(prevStart) && !this.isDefinition(currStart)) {
            return 1; // Separate definitions from code
        }

        // 2. Comments - give them room
        const currIsComment = curr.tokens.some(t => t.type === TokenType.Comment);
        const prevIsComment = prev.tokens.some(t => t.type === TokenType.Comment);
        if (currIsComment && !prevIsComment && !this.isDefinition(prevStart)) {
            // If current is a comment block and previous was code definitions
            return 1;
        }

        // 3. Control Structures (IF, WHILE, FOR, etc.)
        // Add blank line before control block start
        if (this.isControlStart(currStart)) {
            if (!this.isControlStart(prevStart)) {
                return 1;
            }
        }

        // Force blank line before :ELSE
        if (currStart.startsWith(':ELSE')) {
            return 1;
        }

        // Add blank line after control block end
        if (this.isControlEnd(prevStart)) {
            // Unless followed by ELSE or another END (nested closing)
            if (!currStart.startsWith(':ELSE') && !this.isControlEnd(currStart)) {
                return 1;
            }
        }

        // 3. Major Blocks (Procedure, Region) - Always separate
        if (currStart.startsWith(':PROC') || currStart.startsWith(':REGION') || currStart.startsWith(':CLASS')) return 1;
        if (prevStart.startsWith(':ENDPROC') || prevStart.startsWith(':ENDREGION') || prevStart.startsWith(':ENDCLASS')) return 1;

        // 4. Comment-based regions (/* region ... */)
        // Ensure blank line between endregion and region
        const isRegionComment = (text: string) => /^\/\*\s*region/i.test(text);
        const isEndRegionComment = (text: string) => /^\/\*\s*endregion/i.test(text);

        if (isEndRegionComment(prevStart) && isRegionComment(currStart)) {
            return 1;
        }

        return 0;
    }

    private isDefinition(text: string): boolean {
        return text.startsWith(':PARAM') || text.startsWith(':DEFAULT') || text.startsWith(':DECLARE');
    }

    private isControlStart(text: string): boolean {
        return [':IF', ':WHILE', ':FOR', ':REPEAT', ':DO', ':CASE'].includes(text);
    }


    private splitStringToken(content: string, quote: string, firstLineMax: number, subsequentMax: number, indent: string): string {
        // Simple word-based splitting with strict alignment
        let result = "";
        let currentLine = quote;
        let currentMax = firstLineMax; // Start with first line constraint

        const words = content.split(/(\s+)/); // Split by whitespace, capturing it

        for (let i = 0; i < words.length; i++) {
            const word = words[i];

            // Check if adding this word exceeds CURRENT constraint
            // For subsequent lines, currentLine starts with "+ " (included in length check?)
            // No, currentLine is just the string part. Operator is added at flush time or init time?
            // Actually, for strict alignment, we want:
            // Line 1: "content..."
            // Line 2: indent + "+ " + "content..."
            // So subsequentMax is reduced by length of "+ ".

            // Logic:
            // 1. Fill currentLine until max.
            // 2. If full, append quote. 
            // 3. Start new line with `\n` + `indent` + `+ ` + quote.

            if (currentLine.length + word.length + 1 > currentMax) { // +1 for closing quote
                // Need to break
                result += currentLine + quote; // Close previous
                result += "\n" + indent + "+ "; // Start new line with operator

                currentLine = quote + word; // Start new string content
                currentMax = subsequentMax; // Switch constraint
            } else {
                currentLine += word;
            }
        }

        result += currentLine + quote;
        return result;
    }

    private isControlEnd(text: string): boolean {
        return [':ENDIF', ':ENDWHILE', ':ENDFOR', ':UNTIL', ':ENDCASE'].includes(text);
    }

    private getFirstTokenText(node: Node): string {
        const t = node.tokens.find(x => x.type !== TokenType.Whitespace);
        if (t) return t.text.toUpperCase();
        if (node.children.length > 0) return this.getFirstTokenText(node.children[0]);
        return "";
    }

    private getIndent(): string {
        return this.indentString.repeat(this.currentIndentLevel);
    }

    private printStatement(node: Node): string {
        // Iterate tokens and print them with spacing rules
        let line = "";

        // Filter out whitespace tokens for printing
        const tokens = node.tokens.filter(t => t.type !== TokenType.Whitespace);

        // Check if this is a PARAMETERS or DECLARE statement for wrapping
        const firstToken = tokens.find(t => t.type === TokenType.Keyword);
        const isParameterStatement = firstToken &&
            (firstToken.text.toUpperCase() === ':PARAMETERS' || firstToken.text.toUpperCase() === ':DECLARE');

        // Standardize indentation: use current block indent
        const baseIndentStr = this.getIndent();
        const standardColumn = (this.currentIndentLevel + 1) * 4;



        // Helper to calculate visual length
        const getVisualLength = (str: string): number => {
            let len = 0;
            const tabSize = this.options.tabSize;
            for (const char of str) {
                if (char === '\t') {
                    len += tabSize;
                } else {
                    len += 1;
                }
            }
            return len;
        };

        // For manual line length tracking - track VISUAL length
        let currentLineLen = getVisualLength(baseIndentStr);

        // Alignment stacks: 
        // baseStack: The indentation of the group container (e.g. column after '('). Used for new list items.
        // itemStack: The indentation of the current list item. Used for continuations within an item.
        const baseStack: number[] = [];
        const itemStack: number[] = [];

        let resetItemNext = false;
        let effectiveIndent = currentLineLen;

        for (let i = 0; i < tokens.length; i++) {
            const token = tokens[i];
            const prev = i > 0 ? tokens[i - 1] : undefined;
            const prePrev = i > 1 ? tokens[i - 2] : undefined;
            const maxLineLen = (this.options['ssl.format.wrapLength'] as number) || 90;

            const startCol = currentLineLen; // Capture start position of this token

            // Spacing before current token?
            if (prev) {
                // Feature: Implicit String Concatenation preservation/enforcement
                // "A" "B" -> "A" + "B"
                if (prev.type === TokenType.String && token.type === TokenType.String) {
                    line += " + ";
                    currentLineLen += 3;
                } else {
                    if (this.shouldAddSpace(prev, token, prePrev)) {
                        line += " ";
                        currentLineLen += 1;
                    }
                }
            }

            // Pop alignment stack if closing grouping
            if ((token.text === ')' || token.text === '}' || token.text === ']') && baseStack.length > 0) {
                baseStack.pop();
                itemStack.pop();
            }

            // Flag reset for next item if comma
            if (prev && prev.text === ',') {
                resetItemNext = true;
            }

            let formattedToken = this.formatToken(token, standardColumn);
            let tokenLen = formattedToken.length;

            // String splitting logic (OMITTED for brevity in replacement, assuming it matches existing logic but using updated indent)
            // Note: Since I am replacing the block, I must include it.
            if (token.type === TokenType.String && !formattedToken.includes('\n')) {
                let content = formattedToken.substring(1, formattedToken.length - 1);
                const quoteProps = formattedToken.startsWith('"') ? { v: '"' } : { v: "'" };
                const quote = quoteProps.v;

                // Align with current item start
                const alignLen = currentLineLen; // Align with start of string token
                const continuationIndentStr = ' '.repeat(alignLen);

                const operatorLen = 2;
                const subsequentMax = maxLineLen - alignLen - operatorLen;
                const firstLineMax = maxLineLen - currentLineLen;

                if (currentLineLen + getVisualLength(formattedToken) > maxLineLen) {
                    let useSplit = false;
                    if (getVisualLength(formattedToken) > subsequentMax) useSplit = true;
                    else if (firstLineMax > 15) useSplit = true;

                    if (useSplit) {
                        formattedToken = this.splitStringToken(content, quote, firstLineMax, subsequentMax, continuationIndentStr);
                        tokenLen = 0;
                    }
                }
            }

            // Recalculate tokenLen 
            let checkLen = formattedToken.length;
            const newlineIndex = formattedToken.indexOf('\n');
            if (newlineIndex >= 0) {
                checkLen = newlineIndex;
            }

            // Global line wrapping check
            const isFunctionCall = token.text === '(' && prev && (prev.type === TokenType.Identifier || prev.type === TokenType.Keyword || prev.type === TokenType.Unknown);
            const isOpening = token.text === '(' || token.text === '{' || token.text === '[';

            // Lookahead (Simplified for brevity)
            let shouldWrapEarly = false;
            // ... exact logic from previous steps ...
            if (token.type === TokenType.Operator && i + 2 < tokens.length) {
                const next1 = tokens[i + 1];
                const next2 = tokens[i + 2];
                if ((next1.type === TokenType.Identifier || next1.type === TokenType.Keyword) && next2.text === '(') {
                    let groupLen = checkLen + 1 + next1.text.length + 1; // Operator + space + Ident + (

                    // Check for simple argument: Ident/String/Number + )
                    // And consume ALL trailing ')' or ';' to see if the whole suffix fits
                    let lookAheadIdx = i + 3;

                    // Optional Argument
                    if (lookAheadIdx < tokens.length) {
                        const potentialArg = tokens[lookAheadIdx];
                        if (potentialArg.text !== ')' && potentialArg.text !== ']' && potentialArg.text !== '}') {
                            // Assume it's an argument (one token simple arg)
                            groupLen += potentialArg.text.length;
                            lookAheadIdx++;
                        }
                    }

                    // Consume closers
                    while (lookAheadIdx < tokens.length) {
                        const t = tokens[lookAheadIdx];
                        if (t.text === ')' || t.text === ']' || t.text === '}' || t.text === ';') {
                            groupLen += t.text.length;
                            lookAheadIdx++;
                        } else {
                            break;
                        }
                    }

                    if (currentLineLen + groupLen > maxLineLen) {
                        shouldWrapEarly = true;
                    }
                }
            }
            if (!shouldWrapEarly && (token.type === TokenType.Identifier || token.type === TokenType.Keyword) && i + 1 < tokens.length) {
                const next1 = tokens[i + 1];
                if (next1.text === '(') {
                    const groupLen = checkLen + 1;
                    if (currentLineLen + groupLen > maxLineLen) {
                        shouldWrapEarly = true;
                    }
                }
            }

            // Determine Wrap Target
            let alignTo = 0;
            const useBase = resetItemNext || (prev && (prev.text === '(' || prev.text === '{' || prev.text === '[')); // Start of item?

            if (baseStack.length > 0) {
                alignTo = useBase ? baseStack[baseStack.length - 1] : itemStack[itemStack.length - 1];
            } else {
                alignTo = getVisualLength(baseIndentStr) + 8;
            }

            // Heuristic adjustment for operators deep in expression (if not resetting item)
            if (!useBase && token.type === TokenType.Operator && effectiveIndent > alignTo && effectiveIndent < maxLineLen - 10) {
                alignTo = effectiveIndent;
            }

            if ((currentLineLen + checkLen > maxLineLen || shouldWrapEarly) && i > 0 && !isParameterStatement && !isFunctionCall) {
                // Only wrap before "words" (identifiers, keywords, numbers) - not before punctuation
                const isWord = token.type === TokenType.Identifier || token.type === TokenType.Keyword ||
                    token.type === TokenType.Number || token.type === TokenType.String;
                if (isWord) {
                    const continuationIndent = ' '.repeat(alignTo);

                    line = line.trimEnd();
                    line += "\n" + continuationIndent;
                    currentLineLen = getVisualLength(continuationIndent);
                    effectiveIndent = currentLineLen;

                    // If we wrapped at start of item, update currentLineLen which becomes the new item alignment
                    // This update happens implicitly because we continue.
                }
            }

            // Check if we need to set/update item alignment (Post-Wrap check state)
            // If we are at the start of a list item (after comma or opener), 
            // the CURRENT position is the "Item Alignment" for this item.
            if ((resetItemNext || (prev && (prev.text === '(' || prev.text === '{' || prev.text === '['))) && itemStack.length > 0) {
                itemStack[itemStack.length - 1] = currentLineLen;
                resetItemNext = false;
            }

            line += formattedToken;

            // Recalculate currentLineLen 
            const lastNewlineVal = formattedToken.lastIndexOf('\n');
            if (lastNewlineVal >= 0) {
                const lastLineText = formattedToken.substring(lastNewlineVal + 1);
                currentLineLen = getVisualLength(lastLineText);

                const match = lastLineText.match(/^\s*/);
                if (match) effectiveIndent = getVisualLength(match[0]);
            } else {
                currentLineLen += formattedToken.length;
            }

            if (isOpening) {
                baseStack.push(currentLineLen);
                itemStack.push(currentLineLen); // Initialize item stack same as base
            }
        }

        // Apply parameter wrapping if needed
        if (isParameterStatement && line.length > 80) {
            line = this.wrapParameterLine(line, firstToken!.text.toUpperCase());
        }

        // Trim trailing whitespace from each line
        return line.trimEnd();
    }



    private wrapParameterLine(line: string, keyword: string): string {
        const maxLen = 80;
        // Continuation indent: Align with first parameter (keyword length + space)
        // e.g. :PARAMETERS (11) + space (1) = 12 spaces
        // If keyword starts with :, length includes it.
        const alignSpaces = ' '.repeat(keyword.length + 1);
        const continueIndent = this.getIndent() + alignSpaces;

        // Find semicolon and separate it
        const semiPos = line.lastIndexOf(';');
        const content = semiPos > 0 ? line.substring(0, semiPos) : line;
        const semi = semiPos > 0 ? ';' : '';

        // Split by commas while preserving them
        const parts = content.split(',');

        let result = '';
        let currentLine = '';

        for (let i = 0; i < parts.length; i++) {
            const part = parts[i].trim();
            const isFirst = i === 0;
            const isLast = i === parts.length - 1;

            // Build what we'd add
            let addition: string;
            if (isFirst) {
                addition = part;
            } else {
                addition = ', ' + part;
            }

            // Check if adding this would exceed max length
            if (currentLine.length + addition.length > maxLen && currentLine.length > 0) {
                // Wrap: add current line (with comma if not first line) and start new
                result += currentLine + ',\n';
                currentLine = continueIndent + part;
            } else {
                currentLine += addition;
            }
        }

        // Add the last line with semicolon
        result += currentLine + semi;

        return result;
    }

    private formatToken(token: Token, currentColumn: number = 0): string {
        if (token.sqlTokens && token.sqlTokens.length > 0) {
            // Use SQL tokens to format
            // Reconstruct string from SQL tokens with basic formatting?
            // Or better: Use the SqlFormatting logic I saw earlier, but adapted?
            // The user asked to "store both".
            // Providing a simple formatter here that respects the tokens:
            return this.formatSqlTokens(token.sqlTokens, token.text.charAt(0), currentColumn, this.getIndent());
        }

        if (token.type === TokenType.Comment) {
            // Trim trailing whitespace from each line within the comment
            return token.text.split('\n').map(line => line.trimEnd()).join('\n');
        }

        if (token.type === TokenType.Keyword) {
            return token.text.toUpperCase();
        } else if (token.type === TokenType.Identifier) {
            // Check if built-in function
            const lower = token.text.toLowerCase();
            const match = ALL_SQL_FUNCTIONS.find(f => f.toLowerCase() === lower);
            if (match) {
                return match;
            }
            return token.text;
        } else if (token.type === TokenType.Operator) {
            const upper = token.text.toUpperCase();
            if (['.AND.', '.OR.', '.NOT.', '.T.', '.F.', 'NIL'].includes(upper)) {
                return upper;
            }
            return token.text;
        }
        return token.text;
    }

    private formatIdent(tok: any): string {
        if (tok.text.startsWith('?') && tok.text.endsWith('?')) return tok.text;
        return tok.text; // Preserve case for identifiers
    }

    private formatSqlTokens(sqlTokens: any[], startChar: string, baseIndentColumn: number, baseIndentStr?: string): string {
        const closeQuote = startChar === '[' ? ']' : startChar;
        // Use provided baseIndentStr or fall back to spaces if not provided (legacy)
        let baseIndent = baseIndentStr !== undefined ? baseIndentStr + this.indentString : ' '.repeat(baseIndentColumn);
        // Use configured wrap length (default 90 if not set)
        const maxLineLen = (this.options['ssl.format.wrapLength'] as number) || 90;
        let currentLineLen = baseIndentColumn;

        // Keywords that trigger line breaks BEFORE them
        const breakBeforeKeywords = new Set([
            'FROM', 'WHERE', 'INNER', 'LEFT', 'RIGHT', 'FULL', 'CROSS',
            'ORDER', 'GROUP', 'HAVING', 'UNION', 'VALUES', 'INTO', 'ON'
        ]);

        // Join modifiers - if followed by JOIN, don't break before JOIN
        const joinModifiers = new Set(['INNER', 'LEFT', 'RIGHT', 'FULL', 'CROSS']);

        // Keywords that get extra indentation (under WHERE)
        const indentedKeywords = new Set(['AND', 'OR', 'ON']);

        // Block formatting: Start with newline + indent
        let result = startChar + '\n' + baseIndent;

        let isFirstToken = true;
        let parenDepth = 0;
        // Track paren context: multiline status and inherited indent offset
        interface ParenContext {
            isMultiline: boolean;
            indentOffset: number;
            closingIndent: number;
        }
        const parenStack: ParenContext[] = [];

        let afterUpdate = false;
        let afterSet = false;
        let inWhereClause = false;
        let lineHasExtraIndent = false; // Track if current line uses extra indent (e.g. AND/OR)
        let inInsert = false; // Track if we are in INSERT INTO clause
        let inInsertColumnList = false; // Track if we are specifically between INSERT INTO table and SELECT/VALUES
        let inSelectColumns = false; // Track if we are in SELECT column list (before FROM)

        // Filter out whitespace tokens
        const tokens = sqlTokens.filter((t: any) => t.type !== 6);

        if (tokens.length === 0) {
            return startChar + closeQuote;
        }

        let poppedClosingIndent = -1; // Store closing indent from popped context

        for (let i = 0; i < tokens.length; i++) {
            const t = tokens[i];
            const prev = i > 0 ? tokens[i - 1] : null;
            const upperText = t.text.toUpperCase();

            let currentIterOffset = lineHasExtraIndent ? 4 : 0;

            // Re-reset popped closing indent for this iteration
            poppedClosingIndent = -1;

            // Check for INSERT INTO or VALUES context for parenthesis
            // Only detect INSERT table parens when we're specifically between INSERT INTO and SELECT/VALUES
            let isInsertParen = false;
            if (t.text === '(') {
                if (inInsertColumnList && prev && prev.type === 1) {
                    isInsertParen = true;
                } else if (prev && prev.text.toUpperCase() === 'VALUES') {
                    isInsertParen = true;
                }
            }

            // Handle paren stack
            if (t.text === '(') {
                parenDepth++;
                // Add extra indent for INSERT/VALUES lists to "balance" content visually
                const extraOffset = isInsertParen ? 4 : 0;

                // Calculate inline closing indent (default)
                // Base: (depth - 1) * 4.
                // Stack: Sum of offsets so far (excluding current).
                // Line: +4 if lineHasExtraIndent.
                const currentStackOffset = parenStack.reduce((sum, ctx) => sum + ctx.indentOffset, 0);
                const inlineIndent = ((parenDepth - 1) * 4) + currentStackOffset + (lineHasExtraIndent ? 4 : 0);

                parenStack.push({
                    isMultiline: false,
                    indentOffset: currentIterOffset + extraOffset,
                    closingIndent: inlineIndent
                });
            }

            // Prepare to close paren
            const isClosingParen = t.text === ')';

            let tokenText = t.text;

            // Update context tracking EARLY to affect casing
            if (upperText === 'UPDATE') afterUpdate = true;
            if (upperText === 'WHERE') { inWhereClause = true; afterSet = false; }
            if (upperText === 'SET') { afterUpdate = false; afterSet = true; }
            if (upperText === 'INSERT') { inInsert = true; inInsertColumnList = true; }
            if (upperText === 'SELECT') { inSelectColumns = true; inInsertColumnList = false; }
            if (upperText === 'VALUES') { inInsertColumnList = false; }
            if (upperText === 'FROM' && inSelectColumns) { inSelectColumns = false; }

            // Apply casing rules
            if (t.type === 0) { // Keyword
                tokenText = tokenText.toUpperCase();
            } else if (t.type === 1) { // Identifier
                // Check if it's a function (followed by open paren)
                const next = i + 1 < tokens.length ? tokens[i + 1] : null;
                // Exception: If in INSERT column list context, identifier followed by '(' is likely Table(Cols), not Func(Args)
                // So force lowercase. But allow functions in WHERE/JOIN clauses to get PascalCase.
                if (next && next.text === '(' && !inInsertColumnList) {
                    // PascalCase for functions
                    tokenText = tokenText.charAt(0).toUpperCase() + tokenText.slice(1).toLowerCase();
                } else {
                    // Lowercase for tables/columns
                    tokenText = tokenText.toLowerCase();
                }
            } else if (t.type === 8) { // Placeholder
                // Preserve casing exactly for placeholders
                tokenText = t.text;
            }

            // Check if we need to break line before this token
            let needsBreak = false;
            let extraIndent = '';

            // Look ahead for subquery detection
            const next = i < tokens.length - 1 ? tokens[i + 1] : null;

            if (!isFirstToken) {
                if (isClosingParen) {
                    // If closing paren, check if the block was multiline
                    const ctx = parenStack.pop();
                    const wasMultiline = ctx ? ctx.isMultiline : false;

                    // Store the calculated closing indent specifically for this paren
                    if (ctx) poppedClosingIndent = ctx.closingIndent;

                    parenDepth--;

                    if (wasMultiline) {
                        needsBreak = true;
                    }
                } else if (upperText === 'SET' && afterUpdate) {
                    needsBreak = false;
                } else if (prev && prev.text.toUpperCase() === 'SET') {
                    needsBreak = true;
                } else if (upperText === 'SELECT' && prev && prev.text === '(') {
                    needsBreak = true;
                } else if (upperText === 'SELECT' && inInsert && prev && prev.text === ')') {
                    // INSERT INTO table (...) SELECT ... - break before SELECT
                    needsBreak = true;
                } else if (upperText === 'JOIN' && prev && joinModifiers.has(prev.text.toUpperCase())) {
                    needsBreak = false;
                } else if (upperText === 'INTO' && prev && prev.text.toUpperCase() === 'INSERT') {
                    needsBreak = false;
                } else if (prev && prev.text.toUpperCase() === 'VALUES' && t.text === '(') {
                    // Break before '(' after VALUES
                    needsBreak = true;
                } else if (inInsertColumnList && t.text === '(' && prev && prev.type === 1) {
                    // Break before '(' in INSERT INTO Table(...) - only when in the column list area
                    needsBreak = true;
                } else if (inInsertColumnList && prev && prev.text === '(') {
                    // Break AFTER '(' in INSERT/VALUES context (start content on new line)
                    // But only if it was indeed an INSERT paren (detected above for push, but here relying on context)
                    // Since we don't store "isInsertParen" in stack for this check easily (we pushed it),
                    // we can re-derive or check simple logic.
                    // Simple logic: If we are inInsert, and prev was '(', it's likely the list start.
                    needsBreak = true;

                    // Optimization: If the list is empty or single item?
                    // User request implies "better organized", favoring multiline.
                } else if (breakBeforeKeywords.has(upperText)) {
                    needsBreak = true;
                    if (upperText === 'ON') {
                        extraIndent = '    ';
                    }
                } else if (indentedKeywords.has(upperText)) {
                    // AND/OR/ON get line break + extra indent regardless of depth
                    needsBreak = true;
                    extraIndent = '    ';
                }
            } else {
                // First token special handling if it's ')'
                if (t.text === ')') parenDepth--;
            }


            // Helper to format identifier
            const formatIdent = (tok: any) => {
                if (tok.text.startsWith('?') && tok.text.endsWith('?')) return tok.text;
                return tok.text; // Preserve case
            };

            // Check for Table.Column pattern (Identifier/Keyword + Dot + Identifier/Keyword)
            // Treat as single unit for wrapping purposes
            let isTableCol = false;
            // Allow Identifier (1) or Keyword (0) for table/column names
            const isIdentOrKw = (tok: any) => tok.type === 1 || tok.type === 0;

            if (isIdentOrKw(t) && i + 2 < tokens.length) {
                const n1 = tokens[i + 1];
                const n2 = tokens[i + 2];
                if (n1.text === '.' && isIdentOrKw(n2)) {
                    // Re-apply casing to the parts if they are separate?
                    // Or just let them be?
                    // Logic was: tokenText = formatIdent(t) + '.' + formatIdent(n2);
                    // This uses original text.
                    // We probably want to lowercase them too if they are identifiers.

                    const p1 = t.type === 1 ? t.text.toLowerCase() : t.text.toUpperCase(); // crude check
                    const p2 = n2.type === 1 ? n2.text.toLowerCase() : n2.text.toUpperCase();
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
                const canBreak = (prev.text === ',' || (t.type === 0 || t.type === 1)) && prev.text !== '.';

                if (projectedLen > maxLineLen && canBreak && prev.text !== '(') {
                    needsBreak = true;
                    // Align SELECT column values with "SELECT " (7 chars) for all SELECT statements
                    if (inSelectColumns) {
                        extraIndent = '       '; // 7 spaces for "SELECT "
                    } else if (!inInsert) {
                        extraIndent = '    ';
                    }
                }
            }

            // Mark stack as multiline if we are breaking
            if (needsBreak && parenStack.length > 0) {
                parenStack[parenStack.length - 1].isMultiline = true;
            }

            // Add spacing or line break
            if (needsBreak) {
                lineHasExtraIndent = extraIndent.length > 0;

                // If we hit '(' in this loop (above), we might have pushed WRONG offset if we now change lineHasExtraIndent.
                // But wait, `(` itself doesn't trigger break usually.
                // If `AND (` -> `AND` triggers break. `lineHasExtraIndent` becomes true.
                // Then `(` is processed in NEXT iteration?
                // No! `t` is ONE token.
                // If `t` is `AND`. `needsBreak`=true. `lineHasExtraIndent`=true.
                // Stack push happened above? No, `AND` is not `(`.

                // If `t` is `(`. `needsBreak`=false usually.
                // Stack push happened above with `currentIterOffset`.
                // If `lineHasExtraIndent` was set by PREVIOUS token (e.g. `AND`), `currentIterOffset` used it. Correct.

                // What if `(` triggers break?
                // `needsBreak`=true. `lineHasExtraIndent`=false (likely).
                // Stack push used `currentIterOffset` (true?).
                // If `(` moves to new line, it should lose offset?
                // If `(` breaks, it's on a new line. Standard indent.
                // We should fix the top of stack if `(` induced a break.
                // Calculate total offset from stack
                // Calculate total offset from stack
                let stackOffset = parenStack.reduce((sum, ctx) => sum + ctx.indentOffset, 0);

                // If the current token IS the opening paren, it shouldn't inherit the indent offset 
                // that it just created for its contents. It should reside at the outer level (plus paren indent).
                if (t.text === '(' && parenStack.length > 0) {
                    stackOffset -= parenStack[parenStack.length - 1].indentOffset;

                    // Update closing indent to match this new line
                    const newIndent = (parenDepth * 4) + stackOffset + extraIndent.length;
                    parenStack[parenStack.length - 1].closingIndent = newIndent;
                }

                let indentStr = '';

                if (isClosingParen && poppedClosingIndent !== -1) {
                    // Use the calculated closing indent for this specific paren
                    indentStr = ' '.repeat(poppedClosingIndent);
                } else {
                    const parenIndent = '    '.repeat(Math.max(0, parenDepth));
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

        // End block with newline + base indent + close quote
        result += '\n' + baseIndent + closeQuote;
        return result;
    }

    private shouldAddSqlSpace(prev: any, curr: any): boolean {
        // No space after opening paren
        if (prev.text === '(') return false;
        // No space before closing paren  
        if (curr.text === ')') return false;
        // No space before comma
        if (curr.text === ',') return false;
        // Space after comma
        if (prev.text === ',') {
            // Exception: Unary minus after comma (e.g., , -1) - space after comma but let minus handle itself
            return true;
        }
        // No space around dot (table.column)
        if (prev.text === '.' || curr.text === '.') return false;
        // Space around operators (but single space, not double)
        // Exception: Unary minus/plus after comma should not have space before number
        if (prev.type === 4 || curr.type === 4) {
            // Unary minus: if prev is '-' or '+' and prev-prev is ',' or '(' - no space after
            if ((prev.text === '-' || prev.text === '+') && curr.type === 3) {
                // Check if this looks like unary (e.g., after comma in a list)
                return false;
            }
            return true; // Operator
        }
        // Space between key/ident/num/placeholder
        // Types: 0=Kw, 1=Ident, 3=Num, 8=Placeholder
        const isAtom = (t: any) => t.type === 0 || t.type === 1 || t.type === 3 || t.type === 8;

        // Space between keywords/identifiers/placeholders
        if (isAtom(prev) && isAtom(curr)) return true;

        // Space after closing paren if followed by atom
        if (prev.text === ')' && isAtom(curr)) return true;
        return false;
    }

    private shouldAddSpace(prev: Token, curr: Token, prePrev?: Token): boolean {
        // 0. Strict Punctuation Rules (Precedence High)
        // Never add space before semicolon
        if (curr.text === ';') return false;

        // Comma always followed by space
        if (prev.text === ',') {
            return true;
        }
        // Semicolon always followed by space
        if (prev.text === ';') return true;

        // 1. Operator Spacing
        if (curr.type === TokenType.Operator || prev.type === TokenType.Operator) {
            // Unary ! operator - no space after
            if (prev.text === '!') return false;

            // Unary Minus Context: No space after ( or { or [
            // E.g. :RETURN {-1 or (-1
            if ((curr.text === '-' || curr.text === '+') && prev.type === TokenType.Punctuation && ['(', '{', '['].includes(prev.text)) {
                return false;
            }

            // Unary Minus/Plus Logic (Contextual)
            if (prev.text === '-' || prev.text === '+') {
                if (prePrev) {
                    // If preceded by Operator, Keyword, or STARTING Punctuation/Separator -> Unary
                    // Closing punctuation like ')', '}', ']' usually implies Binary operator follows.
                    const isOpenerOrSeparator = ['(', '{', '[', ',', ';', ':'].includes(prePrev.text);

                    if (prePrev.type === TokenType.Operator || prePrev.type === TokenType.Keyword || (prePrev.type === TokenType.Punctuation && isOpenerOrSeparator)) {
                        return false;
                    }
                }
            }
            return true;
        }

        // 2. Other Punctuation
        if (prev.text === ')' && curr.type === TokenType.Identifier) return false;
        // Double closing parens )) -> No space
        if (prev.text === ')' && curr.text === ')') return false;

        if (prev.text === ')') {
            if (curr.type === TokenType.Keyword) return true;
            return true;
        }

        if (curr.type === TokenType.Punctuation) {
            if (curr.text === ',') return false;
            if (curr.text === ')') return false;
        }

        if (prev.type === TokenType.Identifier && curr.text === '(') {
            return false;
        }
        if (prev.type === TokenType.Keyword && curr.text === '(') {
            return true;
        }

        // Return keyword + { (Array/Object return?)
        // :RETURN { ... } -> Space
        if (prev.type === TokenType.Keyword && curr.text === '{') return true;

        // 3. Keywords/Identifiers separation
        if (this.isWord(prev) && this.isWord(curr)) return true;

        return false;
    }

    private isWord(t: Token): boolean {
        return t.type === TokenType.Keyword || t.type === TokenType.Identifier || t.type === TokenType.Number || t.type === TokenType.String;
    }
}
