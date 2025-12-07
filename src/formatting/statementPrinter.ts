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

        let resetItemNext = false;
        let effectiveIndent = currentLineLen;

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

            // 2. Alignment Stack Management (Pop)
            if ([']', '}', ')'].includes(token.text) && baseStack.length > 0) {
                baseStack.pop();
                itemStack.pop();
            }

            // 3. Reset Item Logic
            if (prev && prev.text === ',') {
                resetItemNext = true;
            }

            // 4. Token Formatting & Splitting
            let formattedToken = this.formatToken(token, currentLineLen);

            // Handle long string splitting
            if (token.type === TokenType.String && !formattedToken.includes('\n')) {
                const splitResult = this.processStringToken(formattedToken, currentLineLen, maxLineLen);
                formattedToken = splitResult;
            }

            // 5. Line Wrapping
            const checkLen = this.getEffectiveTokenLength(formattedToken);
            const shouldWrapEarly = this.checkEarlyWrap(tokens, i, currentLineLen, checkLen, maxLineLen);
            const isFunctionCall = token.text === '(' && prev && (prev.type === TokenType.Identifier || prev.type === TokenType.Keyword || prev.type === TokenType.Unknown);

            if ((currentLineLen + checkLen > maxLineLen || shouldWrapEarly) && i > 0 && !isParameterStatement && !isFunctionCall) {
                if (this.isWrappablePoint(token)) {
                    const indentCols = this.determineWrapPosition(baseStack, itemStack, resetItemNext, prev, baseIndentStr, maxLineLen, checkLen);
                    const continuationIndent = ' '.repeat(indentCols);

                    line = line.trimEnd() + "\n" + continuationIndent;
                    currentLineLen = getVisualLength(continuationIndent, this.options.tabSize);
                    effectiveIndent = currentLineLen;
                }
            }

            // 6. Alignment Stack Management (Update/Push)
            if ((resetItemNext || (prev && ['(', '{', '['].includes(prev.text))) && itemStack.length > 0) {
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
            }
        }



        if (isParameterStatement && line.length > maxLineLen) {
            const firstToken = tokens.find(t => t.type === TokenType.Keyword);
            line = wrapParameterLine(line, firstToken!.text.toUpperCase(), this.getIndent(currentIndentLevel), maxLineLen);
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

    private processStringToken(formattedToken: string, currentLineLen: number, maxLineLen: number): string {
        let content = formattedToken.substring(1, formattedToken.length - 1);
        const quote = formattedToken.startsWith('"') ? '"' : "'";

        if (currentLineLen + getVisualLength(formattedToken, this.options.tabSize) > maxLineLen) {
            const alignLen = currentLineLen;
            const operatorLen = 2;
            const subsequentMax = maxLineLen - alignLen - operatorLen;
            const firstLineMax = maxLineLen - currentLineLen;

            let useSplit = false;
            if (getVisualLength(formattedToken, this.options.tabSize) > subsequentMax) { useSplit = true; }
            else if (firstLineMax > 15) { useSplit = true; }

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

        // Lookahead Case 1: Operator followed by function call
        if (token.type === TokenType.Operator && i + 2 < tokens.length) {
            const next1 = tokens[i + 1];
            const next2 = tokens[i + 2];
            if ((next1.type === TokenType.Identifier || next1.type === TokenType.Keyword) && next2.text === '(') {
                let groupLen = checkLen + 1 + next1.text.length + 1;
                let lookAheadIdx = i + 3;
                // (Simplified Arg lookahead from original)
                // This logic was complex in original, simplifying to heuristic: 
                // If the function call start pushes us over, wrap HERE at the operator/start
                if (currentLineLen + groupLen > maxLineLen) { return true; }
            }
        }

        // Lookahead Case 2: Identifier followed by (
        if ((token.type === TokenType.Identifier || token.type === TokenType.Keyword) && i + 1 < tokens.length) {
            if (tokens[i + 1].text === '(') {
                if (currentLineLen + checkLen + 1 > maxLineLen) { return true; }
            }
        }

        return false;
    }

    private isWrappablePoint(token: Token): boolean {
        return token.type === TokenType.Identifier || token.type === TokenType.Keyword ||
            token.type === TokenType.Number || token.type === TokenType.String ||
            token.text === '{' || token.text === '[';
    }

    private determineWrapPosition(baseStack: number[], itemStack: number[], resetItemNext: boolean, prev: Token | undefined, baseIndentStr: string, maxLineLen: number, effectiveCheckLen: number): number {
        const useBase = resetItemNext || (prev && ['(', '{', '['].includes(prev.text));
        let alignTo = 0;

        if (baseStack.length > 0) {
            alignTo = useBase ? baseStack[baseStack.length - 1] : itemStack[itemStack.length - 1];
        } else {
            alignTo = getVisualLength(baseIndentStr, this.options.tabSize) + 8;
        }

        if (alignTo + effectiveCheckLen > maxLineLen) {
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

    private formatToken(token: Token, currentColumn: number = 0): string {
        if (this.options['ssl.format.sql.enabled'] && token.sqlTokens && token.sqlTokens.length > 0) {
            // Use currentColumn for visual alignment, ignoring base indent string (forces space alignment)
            // Add 1 to align with content after the opening quote
            return this.sqlFormatter.formatSqlTokens(token.sqlTokens, token.text.charAt(0), currentColumn + 1, undefined);
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

            // Space before ! (e.g. if !found) - handled by default return true unless start of expression
            // But if previous is punctuation like (, space is not needed?
            // "if (!found)" -> prev=(, curr=! -> usually no space after (
            // Logic below handles no space after (

            // Unary Minus/Plus Context: No space after ( or { or [ or , or : or ; or return
            // E.g. :RETURN -1, (-1, {-1, , -1
            if ((curr.text === '-' || curr.text === '+') && (prev.type === TokenType.Punctuation || prev.type === TokenType.Keyword)) {
                const isOpenerOrSeparator = ['(', '{', '[', ',', ';', ':'].includes(prev.text);
                // Also keywords like RETURN, IF, WHILE might precede unary? 
                // Actually parser handles structure, here we just look at tokens.
                // "RETURN -1" -> Space required.
                // "(-1)" -> No space.
                if (prev.type === TokenType.Punctuation && isOpenerOrSeparator) {
                    return false;
                }
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
                } else {
                    // Start of file/line -> Unary
                    return false;
                }
            }

            // Range Operator (..) spacing? Usually 1..10 -> no space?
            // If lexer treats .. as operator. Assuming it doesn't or we want spaces.

            return true;
        }

        // 2. Other Punctuation
        if (prev.text === ')' && curr.type === TokenType.Identifier) { return false; }
        // Double closing parens )) -> No space
        if (prev.text === ')' && curr.text === ')') { return false; }

        if (prev.text === ')') {
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

        // Pad simple braces { ... }
        if (prev.text === '{' && curr.text !== '}') { return true; }
        if (curr.text === '}' && prev.text !== '{') { return true; }

        // 3. Keywords/Identifiers separation
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
