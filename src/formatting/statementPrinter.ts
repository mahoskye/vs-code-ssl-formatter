import { Node } from './parser';
import { Token, TokenType } from './lexer';
import { SqlFormatter } from './sqlFormatter';
import { getVisualLength, splitStringToken, wrapParameterLine } from './formattingUtils';
import { ALL_SQL_FUNCTIONS, SSL_OPERATORS, SSL_LITERALS, PROCEDURE_LEVEL_KEYWORDS } from '../constants/language';
import { FormattingOptions } from './formatter';

export class StatementPrinter {
    private options: FormattingOptions;
    private sqlFormatter: SqlFormatter;
    private indentString: string;

    constructor(options: FormattingOptions, sqlFormatter: SqlFormatter) {
        this.options = options;
        this.sqlFormatter = sqlFormatter;
        this.indentString = options.insertSpaces ? ' '.repeat(options.tabSize) : '\t';
    }

    public printStatement(node: Node, currentIndentLevel: number): string {
        let line = "";
        const tokens = node.tokens.filter(t => t.type !== TokenType.Whitespace);

        // Check for parameter wrapping context
        const isParameterStatement = this.isParameterDeclaration(tokens);

        const baseIndentStr = this.getIndent(currentIndentLevel);
        let currentLineLen = getVisualLength(baseIndentStr, this.options.tabSize);

        const baseStack: number[] = [];
        const itemStack: number[] = [];
        // Track start column for each token to allow context-aware indentation (e.g. SQL relative to Function)
        const tokenStarts: number[] = new Array(tokens.length).fill(0);

        let resetItemNext = false;
        let effectiveIndent = currentLineLen;
        // Track wrap indent within a parameter block (same comma-separated item)
        // Only reset on comma at the top bracket level
        let parameterWrapIndent: number | undefined = undefined;
        let parameterWrapIndentDepth = 0;

        const maxLineLen = (this.options['ssl.format.wrapLength'] as number) || 90;

        for (let i = 0; i < tokens.length; i++) {
            const token = tokens[i];
            const prev = i > 0 ? tokens[i - 1] : undefined;
            const prePrev = i > 1 ? tokens[i - 2] : undefined;

            // 1. Spacing Calculation
            if (prev) {
                const spacingAdjustment = this.calculateSpacing(prev, token, prePrev);
                line += spacingAdjustment.text;
                currentLineLen += spacingAdjustment.len;
            }

            // Record start position of THIS token
            tokenStarts[i] = currentLineLen;

            // 2. Alignment Stack Management (Pop) - MOVED TO STEP 6 to allow '}' formatting to use stack
            // if ([']', '}', ')'].includes(token.text) && baseStack.length > 0) {
            //     baseStack.pop();
            //     itemStack.pop();
            // }

            // 3. Reset Item Logic - reset wrap lock on comma at outer level only
            if (prev && prev.text === ',') {
                resetItemNext = true;
                // Reset wrap indent when moving to a new parameter at TOP level only
                // CRITICAL: Do NOT reset when inside brackets (baseStack.length > 0)
                // This ensures consistent alignment throughout array/list literals
                if (baseStack.length === 0 && parameterWrapIndentDepth === 0) {
                    parameterWrapIndent = undefined;
                }
            }

            // 4. Token Formatting & Splitting
            // Calculate special indent for SQL tokens
            let customIndent = undefined;
            if (token.sqlTokens && token.sqlTokens.length > 0) {
                // ... (omitted logic remains same) ...
                const quoteStart = currentLineLen + 1; // Content starts after opening quote
                let funcStart = currentLineLen; // Default fallback if no function found

                if (prev && prev.text === '(' && prePrev && (prePrev.type === TokenType.Identifier || prePrev.type === TokenType.Keyword)) {
                    funcStart = tokenStarts[i - 2];
                } else {
                    // Fallback: use quote start relative or some other default?
                    // If no function, maybe just use quoteStart?
                    // Or check if it's an assignment? `var := "SQL"`
                    // If assignment, maybe align with var?
                    // For now, if no function detected, stick to visual alignment (quoteStart).
                    funcStart = currentLineLen - 4; // So min becomes quoteStart
                }

                const indentFromFunc = funcStart + 4;
                customIndent = Math.min(indentFromFunc, quoteStart);

                // Ensure non-negative
                customIndent = Math.max(0, customIndent);
            }

            // Member access casing check: object:property (property is Keyword)
            const preserveCase = (prev && prev.type === TokenType.Identifier && token.type === TokenType.Keyword && token.text.startsWith(':'));

            let formattedToken = this.formatToken(token, currentLineLen, customIndent, preserveCase);



            // Handle long string splitting - use locked indent if available
            if (token.type === TokenType.String && !formattedToken.includes('\n')) {
                const splitResult = this.processStringToken(formattedToken, currentLineLen, maxLineLen, parameterWrapIndent, prev, tokens, i);
                // If string was split (has newlines) and no lock yet, set the lock
                if (splitResult.includes('\n') && parameterWrapIndent === undefined) {
                    // Lock to current position (first token after split continues here)
                    parameterWrapIndent = currentLineLen;
                    parameterWrapIndentDepth = baseStack.length;
                }
                formattedToken = splitResult;
            }

            // 5. Line Wrapping
            const checkLen = this.getEffectiveTokenLength(formattedToken);
            const shouldWrapEarly = this.checkEarlyWrap(tokens, i, currentLineLen, checkLen, maxLineLen);
            const isFunctionCall = token.text === '(' && prev && (prev.type === TokenType.Identifier || prev.type === TokenType.Keyword || prev.type === TokenType.Unknown);

            if ((currentLineLen + checkLen > maxLineLen || shouldWrapEarly) && i > 0 && !isParameterStatement && !isFunctionCall) {
                if (this.isWrappablePoint(token, prev, tokens, i)) {
                    // Use locked parameter wrap indent if set, otherwise calculate and lock it
                    // Don't lock on opening brackets - they start new contexts
                    const isOpener = ['(', '{', '['].includes(token.text);
                    let indentCols: number;



                    if (parameterWrapIndent !== undefined && !isOpener && baseStack.length === parameterWrapIndentDepth) {
                        indentCols = parameterWrapIndent;
                    } else {
                        indentCols = this.determineWrapPosition(baseStack, itemStack, resetItemNext, prev, baseIndentStr, maxLineLen, checkLen);
                        // Only lock for non-opener tokens and if we are at the base depth (or just track current depth?)
                        // Better to lock only for the current depth.
                        if (!isOpener) {
                            parameterWrapIndent = indentCols;
                            parameterWrapIndentDepth = baseStack.length;
                        }
                    }
                    const continuationIndent = ' '.repeat(indentCols);

                    line = line.trimEnd() + "\n" + continuationIndent;
                    currentLineLen = getVisualLength(continuationIndent, this.options.tabSize);
                    effectiveIndent = currentLineLen;
                }
            }

            // 6. Alignment Stack Management (Update/Push)
            // Update itemStack only on the FIRST item (when prev is Opener).
            // Do NOT update on commas (resetItemNext), so alignment remains anchored to the first item.
            if ((prev && ['(', '{', '['].includes(prev.text)) && itemStack.length > 0) {
                itemStack[itemStack.length - 1] = currentLineLen;
                resetItemNext = false;
            }

            line += formattedToken;

            // Recalculate length after addition
            const lengthUpdate = this.calculatePostTokenLength(formattedToken, currentLineLen);
            currentLineLen = lengthUpdate.currentLineLen;
            if (lengthUpdate.effectiveIndent !== undefined) {
                effectiveIndent = lengthUpdate.effectiveIndent;
            }

            if (['(', '{', '['].includes(token.text)) {
                baseStack.push(currentLineLen);
                itemStack.push(currentLineLen);
            } else if ([']', '}', ')'].includes(token.text) && baseStack.length > 0) {
                baseStack.pop();
                itemStack.pop();
            }
        }



        if (isParameterStatement && currentLineLen > maxLineLen) {
            const firstToken = tokens.find(t => t.type === TokenType.Keyword);
            line = wrapParameterLine(line, firstToken!.text.toUpperCase(), this.getIndent(currentIndentLevel), maxLineLen, this.options.tabSize);
        }

        return line.trimEnd();
    }

    private isParameterDeclaration(tokens: Token[]): boolean {
        const firstToken = tokens.find(t => t.type === TokenType.Keyword);
        if (firstToken) {
            const upperKeys = firstToken.text.toUpperCase().replace(/^:/, '');
            return PROCEDURE_LEVEL_KEYWORDS.includes(upperKeys);
        }
        return false;
    }

    private calculateSpacing(prev: Token, curr: Token, prePrev: Token | undefined): { text: string, len: number } {
        // Implicit concatenation: "A" "B" -> "A" + "B"
        if (prev.type === TokenType.String && curr.type === TokenType.String) {
            return { text: " + ", len: 3 };
        }
        if (this.shouldAddSpace(prev, curr, prePrev)) {
            return { text: " ", len: 1 };
        }
        return { text: "", len: 0 };
    }

    private processStringToken(formattedToken: string, currentLineLen: number, maxLineLen: number, lockedIndent?: number, prev?: Token, tokens?: Token[], currentIndex?: number): string {
        let content = formattedToken.substring(1, formattedToken.length - 1);
        const quote = formattedToken.startsWith('"') ? '"' : "'";

        // Check if this string is the first parameter of ExecFunction, DoProc, or CreateUDObject
        // These functions take a function/procedure name as the first parameter, which should not be split
        let isSpecialFunctionFirstParam = false;
        if (prev && tokens && currentIndex !== undefined) {
            // Look back to find if we just passed an opening parenthesis after one of these function names
            if (prev.text === '(' && currentIndex >= 2) {
                const funcToken = tokens[currentIndex - 2];
                if (funcToken && funcToken.type === TokenType.Identifier) {
                    const funcName = funcToken.text.toUpperCase();
                    if (['EXECFUNCTION', 'DOPROC', 'CREATEUDOBJECT'].includes(funcName)) {
                        isSpecialFunctionFirstParam = true;
                    }
                }
            }
        }

        // Don't split if this is a special function's first parameter
        if (isSpecialFunctionFirstParam) {
            return formattedToken;
        }

        if (currentLineLen + getVisualLength(formattedToken, this.options.tabSize) > maxLineLen) {
            // Use locked indent if available, otherwise use current position
            const alignLen = lockedIndent !== undefined ? lockedIndent : currentLineLen;
            const operatorLen = 2;
            const subsequentMax = maxLineLen - alignLen - operatorLen;
            const firstLineMax = maxLineLen - currentLineLen;

            let useSplit = false;
            // Only split if we have reasonable space on both lines
            // firstLineMax > 20 ensures we don't create orphaned empty strings like ExecFunction("" + "...")
            // Also check that the actual content length is meaningful
            const contentLen = content.length;
            const firstPartLen = Math.min(firstLineMax - 2, contentLen); // -2 for quotes

            // Don't split if first part would be less than 5 characters (would create "" or nearly empty string)
            if (firstLineMax > 20 && subsequentMax > 0 && firstPartLen > 5) {
                if (getVisualLength(formattedToken, this.options.tabSize) > subsequentMax) { useSplit = true; }
            }

            if (useSplit) {
                return splitStringToken(content, quote, firstLineMax, subsequentMax, ' '.repeat(alignLen));
            }
        }
        return formattedToken;
    }

    private getEffectiveTokenLength(tokenText: string): number {
        const newlineIndex = tokenText.indexOf('\n');
        return newlineIndex >= 0 ? newlineIndex : tokenText.length;
    }

    private checkEarlyWrap(tokens: Token[], i: number, currentLineLen: number, checkLen: number, maxLineLen: number): boolean {
        const token = tokens[i];

        // Never wrap method name from object (e.g. s:Format)
        if (i > 0 && tokens[i - 1].text === ':') {
            return false;
        }

        // Multi-dimensional Array Expansion: Force wrap for { { ... }, { ... } } structures
        if (token.text === '{') {
            // Case 1: { { -> Wrap before second {
            if (i > 0 && tokens[i - 1].text === '{') { return true; }
            // Case 2: }, { -> Wrap before second {
            if (i > 1 && tokens[i - 1].text === ',' && tokens[i - 2].text === '}') { return true; }
        }
        if (token.text === '}') {
            // Case 3: } } -> Wrap before second }
            if (i > 0 && tokens[i - 1].text === '}') { return true; }
        }

        // Prevent wrapping immediately after control flow keywords
        if (i > 0 && tokens[i - 1].type === TokenType.Keyword) {
            const kw = tokens[i - 1].text.toUpperCase();
            if ([':IF', ':WHILE', ':ELSEIF', ':CASE', ':RETURN', 'IF', 'WHILE', 'ELSEIF', 'CASE', 'RETURN'].includes(kw)) {
                return false;
            }
        }

        // Prevent wrapping immediately after assignment operator `:=`
        // to avoid orphaning the LHS (e.g., `rndVal := \n LSearch(...)`)
        if (i > 0 && tokens[i - 1].text === ':=') {
            return false;
        }

        // Prevent wrapping after opening parenthesis of ExecFunction, DoProc, CreateUDObject
        // to keep first parameter (function name) on same line
        if (i > 0 && tokens[i - 1].text === '(' && i >= 2) {
            const funcToken = tokens[i - 2];
            if (funcToken && funcToken.type === TokenType.Identifier) {
                const funcName = funcToken.text.toUpperCase();
                if (['EXECFUNCTION', 'DOPROC', 'CREATEUDOBJECT'].includes(funcName)) {
                    return false;
                }
            }
        }

        // Lookahead Case 1: Binary Operator (prefer wrap before)
        // Check if the entire operand (up to next operator/separator) fits
        // Exception: Assignment operator := should stay with LHS if possible (wrap AFTER)
        if (token.type === TokenType.Operator && token.text !== ':=' && i + 1 < tokens.length) {
            let lookAheadLen = checkLen; // Start with operator length
            let j = i + 1;

            // Scan forward to capture the "operand" unit
            while (j < tokens.length) {
                const nextTok = tokens[j];

                // Stop at separators or next operator (logic handoff)
                if (nextTok.type === TokenType.Operator || nextTok.text === ',' || nextTok.text === ';') {
                    break;
                }

                // Add length (approximate spacing - assuming 1 space or 0)
                // We can be conservative and assume 0 for punct, 1 for words?
                // Or use calculateSpacing logic? Too expensive.
                // Assume worst case (tight) or standard (1 space)?
                // Simple simplification: Add length. If it's punctuation, usually attached.
                // If previous was word and this is word, add 1.

                // Simple accumulation
                const nextLen = this.getEffectiveTokenLength(nextTok.text);
                lookAheadLen += nextLen;

                // If this token limits the line (has newline), we can stop checking further?
                // Actually if it has newline, the visible length is capped.
                // But we should break the loop because subsequent tokens match "next line".
                if (nextTok.text.includes('\n')) {
                    break;
                }

                // Add 1 space allowance for safety/separation if not punct
                if (nextTok.type !== TokenType.Punctuation) {
                    lookAheadLen += 1;
                }

                j++;

                // Optimization: if we already exceed, stop
                if (currentLineLen + lookAheadLen > maxLineLen) {
                    return true;
                }

                // Limit lookahead depth to avoid perf hit on massive lines? 
                if (j - i > 20) {break;}
            }
        }


        // Lookahead Case 1.5: Member Access (Object:Method) - Keep together
        if (token.type === TokenType.Identifier && i + 1 < tokens.length && tokens[i + 1].text === ':') {
            let lookAheadLen = checkLen;
            let k = i + 1;
            while (k < tokens.length) {
                const t = tokens[k];
                if (t.text === ':' || t.type === TokenType.Identifier || t.type === TokenType.Keyword) {
                    lookAheadLen += t.text.length;
                    k++;
                } else {
                    break;
                }
            }

            if (currentLineLen + lookAheadLen > maxLineLen) { return true; }
        }

        // Lookahead Case 2: Identifier followed by (
        if ((token.type === TokenType.Identifier || token.type === TokenType.Keyword) && i + 1 < tokens.length) {
            if (tokens[i + 1].text === '(') {
                // If identifier + ( itself exceeds logic (rare), wrap
                if (currentLineLen + checkLen + 1 > maxLineLen) { return true; }

                // Deep lookahead: Check if function call "Func(...)" fits on line?
                let lookAheadFun = checkLen + 1; // Id + (
                let k = i + 2;
                let fundepth = 1;
                let prevFun = tokens[i + 1]; // (

                while (k < tokens.length && fundepth > 0) {
                    const nextFun = tokens[k];
                    if (nextFun.text === '(') {fundepth++;}
                    else if (nextFun.text === ')') {fundepth--;}

                    lookAheadFun += this.getEffectiveTokenLength(nextFun.text);
                    if (this.shouldAddSpace(prevFun, nextFun)) { lookAheadFun += 1; }
                    prevFun = nextFun;

                    // Stop accumulating if we hit a multi-line token (e.g., string with embedded newlines)
                    // The effective length only counts the first line, so further accumulation is meaningless
                    if (nextFun.text.includes('\n')) {
                        break;
                    }

                    k++;

                    if (currentLineLen + lookAheadFun > maxLineLen) {
                        return true;
                    }
                    // Limit scan
                    if (k - i > 60) {break;}
                }
            } // Close L315
        } // Close L314

        // Lookahead Case 3: Opening Brace/Bracket (prefer wrap before if entire block exceeds)
        // If we have "Func", { Content } -> if { Content } exceeds line limit, wrap before {
        if (token.text === '{' || token.text === '[') {
            let lookAheadLen = checkLen; // Start with opener length
            let j = i + 1;
            let depth = 1; // Track nesting depth
            let prevToken = token;

            while (j < tokens.length && depth > 0) {
                const nextTok = tokens[j];

                // Track depth for nested brackets
                if (nextTok.text === '{' || nextTok.text === '[') { depth++; }
                else if (nextTok.text === '}' || nextTok.text === ']') { depth--; }

                lookAheadLen += this.getEffectiveTokenLength(nextTok.text);

                // Add spacing approximation using actual logic
                if (this.shouldAddSpace(prevToken, nextTok)) {
                    lookAheadLen += 1;
                }
                prevToken = nextTok;

                // Stop accumulating if we hit a multi-line token
                if (nextTok.text.includes('\n')) {
                    break;
                }

                j++;

                // If total exceeds limit, wrap before opener
                if (currentLineLen + lookAheadLen > maxLineLen) { return true; }

                // Safety limit
                if (j - i > 50) {break;}
            }
        }

        return false;
    }

    private isWrappablePoint(token: Token, prev?: Token, tokens?: Token[], i?: number): boolean {
        if (token.text === ':') { return false; }
        if (prev && prev.text === ':') { return false; }

        // Prevent wrapping after opening parenthesis of ExecFunction, DoProc, CreateUDObject
        if (prev && prev.text === '(' && tokens && i !== undefined && i >= 2) {
            const funcToken = tokens[i - 2];
            if (funcToken && funcToken.type === TokenType.Identifier) {
                const funcName = funcToken.text.toUpperCase();
                if (['EXECFUNCTION', 'DOPROC', 'CREATEUDOBJECT'].includes(funcName)) {
                    return false;
                }
            }
        }

        return token.type === TokenType.Identifier || token.type === TokenType.Keyword ||
            token.type === TokenType.Number || token.type === TokenType.String ||
            token.type === TokenType.Operator ||
            token.text === '{' || token.text === '[' || token.text === '}' || token.text === ']';
    }

    private determineWrapPosition(baseStack: number[], itemStack: number[], resetItemNext: boolean, prev: Token | undefined, baseIndentStr: string, maxLineLen: number, effectiveCheckLen: number): number {
        const useBase = (prev && ['(', '{', '['].includes(prev.text));
        let alignTo = 0;

        if (baseStack.length > 0) {
            alignTo = useBase ? baseStack[baseStack.length - 1] : itemStack[itemStack.length - 1];
        } else {
            alignTo = getVisualLength(baseIndentStr, this.options.tabSize) + 8;
        }

        // Cap alignment to prevent excessive indentation (e.g., deep nesting)
        // Only use fixed maxAlignTo, not variable token length, to ensure consistent alignment
        const maxAlignTo = 60;
        if (alignTo > maxAlignTo) {
            const standardIndent = getVisualLength(baseIndentStr, this.options.tabSize) + 8;
            if (standardIndent < alignTo) {
                return standardIndent;
            }
        }
        return alignTo;
    }

    private calculatePostTokenLength(formattedToken: string, currentLineLen: number): { currentLineLen: number, effectiveIndent?: number } {
        const lastNewlineVal = formattedToken.lastIndexOf('\n');
        if (lastNewlineVal >= 0) {
            const lastLineText = formattedToken.substring(lastNewlineVal + 1);
            const newLen = getVisualLength(lastLineText, this.options.tabSize);
            const match = lastLineText.match(/^\s*/);
            const effParams = match ? getVisualLength(match[0], this.options.tabSize) : undefined;
            return { currentLineLen: newLen, effectiveIndent: effParams };
        } else {
            return { currentLineLen: currentLineLen + formattedToken.length };
        }
    }

    private formatToken(token: Token, currentColumn: number = 0, customBaseIndent?: number, preserveCase: boolean = false): string {


        if (this.options['ssl.format.sql.enabled'] && token.sqlTokens && token.sqlTokens.length > 0) {
            // Use customBaseIndent if provided, otherwise default to currentColumn + 1 (visual alignment)
            // The logic was: min(func+4, quote+1). customBaseIndent holds this result.
            // If customBaseIndent is undefined, fallback to old visual align?
            // "User requested removing 2 tabs (8 chars)" - this was handled by max(0, col+1-8).
            // New request overrides this with specific context logic.

            const baseIndent = customBaseIndent !== undefined ? customBaseIndent : Math.max(0, currentColumn + 1 - 8);
            return this.sqlFormatter.formatSqlTokens(token.sqlTokens, token.text.charAt(0), baseIndent, undefined);
        }

        if (token.type === TokenType.Comment) {
            // Trim trailing whitespace from each line within the comment
            return token.text.split('\n').map(line => line.trimEnd()).join('\n');
        }

        if (token.type === TokenType.Keyword) {
            return preserveCase ? token.text : token.text.toUpperCase();
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
            if (SSL_OPERATORS.includes(upper) || SSL_LITERALS.includes(upper)) {
                return upper;
            }
            return token.text;
        }
        return token.text;
    }

    private shouldAddSpace(prev: Token, curr: Token, prePrev?: Token): boolean {
        // 0. Strict Punctuation Rules (Precedence High)
        // Never add space before semicolon
        if (curr.text === ';') { return false; }

        // Comma always followed by space unless next token is also comma
        if (prev.text === ',') {
            if (curr.text === ',') { return false; }
            return true;
        }
        // Semicolon always followed by space
        if (prev.text === ';') { return true; }

        // 1. Operator Spacing
        if (curr.type === TokenType.Operator || prev.type === TokenType.Operator) {
            // Unary ! operator - no space after (e.g. !found)
            if (prev.text === '!') { return false; }

            // Postfix Increment/Decrement (e.g. i++ or i--)
            // Also identifiers treated as keywords (e.g. :property)
            if ((curr.text === '++' || curr.text === '--') && (prev.type === TokenType.Identifier || prev.type === TokenType.Number || prev.type === TokenType.Keyword)) {
                return false;
            }

            // Unary Minus/Plus/Inc/Dec Context: No space after ( or { or [ or , or : or ;
            // E.g. (-1, {-1, , -1, (++i
            if ((curr.text === '-' || curr.text === '+' || curr.text === '++' || curr.text === '--') && (prev.type === TokenType.Punctuation || prev.type === TokenType.Keyword)) {
                const isOpenerOrSeparator = ['(', '{', '[', ',', ';', ':'].includes(prev.text);
                // "RETURN -1" -> Space required. "(-1)" -> No space.
                if (prev.type === TokenType.Punctuation && isOpenerOrSeparator) {
                    return false;
                }
            }

            // Unary Minus/Plus/Inc/Dec Logic (Contextual) - No space AFTER
            if (prev.text === '-' || prev.text === '+' || prev.text === '++' || prev.text === '--') {
                if (prePrev) {
                    // If preceded by Operator, Keyword, or STARTING Punctuation/Separator -> Unary
                    // Closing punctuation like ')', '}', ']' usually implies Binary operator follows.
                    const isOpenerOrSeparator = ['(', '{', '[', ',', ';', ':'].includes(prePrev.text);

                    if (prePrev.type === TokenType.Operator || prePrev.type === TokenType.Keyword || (prePrev.type === TokenType.Punctuation && isOpenerOrSeparator)) {
                        return false;
                    }
                } else {
                    // Start of file/line -> Unary
                    return false;
                }
            }

            // Range Operator (..) check?
            return true;
        }

        // 2. Other Punctuation
        if (prev.text === ')' && curr.type === TokenType.Identifier) { return false; }
        // Double closing parens )) -> No space
        if (prev.text === ')' && curr.text === ')') { return false; }

        if (prev.text === ')') {
            if (curr.text === ',') { return false; }
            if (curr.type === TokenType.Keyword) { return true; }
            return true;
        }

        if (curr.type === TokenType.Punctuation) {
            if (curr.text === ',') { return false; }
            if (curr.text === ')') { return false; }
        }

        if (prev.type === TokenType.Identifier && curr.text === '(') {
            return false;
        }
        if (prev.type === TokenType.Keyword && curr.text === '(') {
            return true;
        }

        // Return keyword + { (Array/Object return?)
        // :RETURN { ... } -> Space
        if (prev.type === TokenType.Keyword && curr.text === '{') { return true; }

        // Pad simple braces { ... } - REMOVED per user request
        // if (prev.text === '{' && curr.text !== '}') { return true; }
        // if (curr.text === '}' && prev.text !== '{') { return true; }

        // 3. Keywords/Identifiers separation
        // Special Case: Member access "object:property" where property tokenizes as Keyword (:prop)
        if (prev.type === TokenType.Identifier && curr.type === TokenType.Keyword && curr.text.startsWith(':')) {
            return false;
        }

        // Special Case: Array access "array[i]"
        if (curr.text === '[' && this.isWord(prev)) {
            return false;
        }

        if (this.isWord(prev) && this.isWord(curr)) { return true; }

        return false;
    }

    private isWord(t: Token): boolean {
        return t.type === TokenType.Keyword || t.type === TokenType.Identifier || t.type === TokenType.Number || t.type === TokenType.String;
    }

    private getIndent(level: number): string {
        return this.indentString.repeat(level);
    }
}
