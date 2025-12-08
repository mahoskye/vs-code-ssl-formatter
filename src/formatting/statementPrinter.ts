import { Node } from './parser';
import { Token, TokenType } from './lexer';
import { SqlFormatter } from './sqlFormatter';
import { getVisualLength, splitStringToken, wrapParameterLine } from './formattingUtils';
import { ALL_SQL_FUNCTIONS, SSL_OPERATORS, SSL_LITERALS, PROCEDURE_LEVEL_KEYWORDS } from '../constants/language';
import { FormattingOptions } from './formatter';

// Functions whose first parameter should not be split or wrapped
const SPECIAL_FUNCTIONS = ['EXECFUNCTION', 'DOPROC', 'CREATEUDOBJECT'];

export class StatementPrinter {
    private options: FormattingOptions;
    private sqlFormatter: SqlFormatter;
    private indentString: string;

    constructor(options: FormattingOptions, sqlFormatter: SqlFormatter) {
        this.options = options;
        this.sqlFormatter = sqlFormatter;
        this.indentString = options.insertSpaces ? ' '.repeat(options.tabSize) : '\t';
    }

    // --- Main Entry Point ---

    public printStatement(node: Node, currentIndentLevel: number): string {
        let line = "";
        const tokens = node.tokens.filter(t => t.type !== TokenType.Whitespace);

        const isParameterStatement = this.isParameterDeclaration(tokens);
        const baseIndentStr = this.getIndent(currentIndentLevel);
        let currentLineLen = getVisualLength(baseIndentStr, this.options.tabSize);

        const baseStack: number[] = [];
        const itemStack: number[] = [];
        const tokenStarts: number[] = new Array(tokens.length).fill(0);

        let resetItemNext = false;
        let effectiveIndent = currentLineLen;
        let parameterWrapIndent: number | undefined = undefined;
        let parameterWrapIndentDepth = 0;

        const maxLineLen = (this.options['ssl.format.wrapLength'] as number) || 90;

        for (let i = 0; i < tokens.length; i++) {
            const token = tokens[i];
            const prev = i > 0 ? tokens[i - 1] : undefined;
            const prePrev = i > 1 ? tokens[i - 2] : undefined;

            // Spacing Calculation
            if (prev) {
                const spacingAdjustment = this.calculateSpacing(prev, token, prePrev);
                line += spacingAdjustment.text;
                currentLineLen += spacingAdjustment.len;
            }

            tokenStarts[i] = currentLineLen;

            // Reset Item Logic
            if (prev && prev.text === ',') {
                resetItemNext = true;
                if (baseStack.length === 0 && parameterWrapIndentDepth === 0) {
                    parameterWrapIndent = undefined;
                }
            }

            // SQL Token Formatting
            let customIndent = undefined;
            if (token.sqlTokens && token.sqlTokens.length > 0) {
                customIndent = this.calculateSqlIndent(currentLineLen, prev, prePrev, tokenStarts, i);
            }

            const preserveCase = (prev && prev.type === TokenType.Identifier && token.type === TokenType.Keyword && token.text.startsWith(':'));
            let formattedToken = this.formatToken(token, currentLineLen, customIndent, preserveCase);

            // Handle long string splitting
            if (token.type === TokenType.String && !formattedToken.includes('\n')) {
                const splitResult = this.processStringToken(formattedToken, currentLineLen, maxLineLen, parameterWrapIndent, prev, tokens, i);
                if (splitResult.includes('\n') && parameterWrapIndent === undefined) {
                    parameterWrapIndent = currentLineLen;
                    parameterWrapIndentDepth = baseStack.length;
                }
                formattedToken = splitResult;
            }

            // Line Wrapping
            const checkLen = this.getEffectiveTokenLength(formattedToken);
            const shouldWrapEarly = this.checkEarlyWrap(tokens, i, currentLineLen, checkLen, maxLineLen);
            const isFunctionCall = token.text === '(' && prev && (prev.type === TokenType.Identifier || prev.type === TokenType.Keyword || prev.type === TokenType.Unknown);

            if ((currentLineLen + checkLen > maxLineLen || shouldWrapEarly) && i > 0 && !isParameterStatement && !isFunctionCall) {
                if (this.isWrappablePoint(token, prev, tokens, i)) {
                    const isOpener = ['(', '{', '['].includes(token.text);
                    let indentCols: number;

                    if (parameterWrapIndent !== undefined && !isOpener && baseStack.length === parameterWrapIndentDepth) {
                        indentCols = parameterWrapIndent;
                    } else {
                        indentCols = this.determineWrapPosition(baseStack, itemStack, resetItemNext, prev, baseIndentStr, maxLineLen, checkLen);
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

            // Alignment Stack Management
            if ((prev && ['(', '{', '['].includes(prev.text)) && itemStack.length > 0) {
                itemStack[itemStack.length - 1] = currentLineLen;
                resetItemNext = false;
            }

            line += formattedToken;

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

    // --- Helper Methods ---

    private isParameterDeclaration(tokens: Token[]): boolean {
        const firstToken = tokens.find(t => t.type === TokenType.Keyword);
        if (firstToken) {
            const upperKeys = firstToken.text.toUpperCase().replace(/^:/, '');
            return (PROCEDURE_LEVEL_KEYWORDS as readonly string[]).includes(upperKeys);
        }
        return false;
    }

    private calculateSpacing(prev: Token, curr: Token, prePrev: Token | undefined): { text: string, len: number } {
        if (prev.type === TokenType.String && curr.type === TokenType.String) {
            return { text: " + ", len: 3 };
        }
        if (this.shouldAddSpace(prev, curr, prePrev)) {
            return { text: " ", len: 1 };
        }
        return { text: "", len: 0 };
    }

    private calculateSqlIndent(currentLineLen: number, prev: Token | undefined, prePrev: Token | undefined, tokenStarts: number[], i: number): number {
        const quoteStart = currentLineLen + 1;
        let funcStart = currentLineLen;

        if (prev && prev.text === '(' && prePrev && (prePrev.type === TokenType.Identifier || prePrev.type === TokenType.Keyword)) {
            funcStart = tokenStarts[i - 2];
        } else {
            funcStart = currentLineLen - 4;
        }

        const indentFromFunc = funcStart + 4;
        return Math.max(0, Math.min(indentFromFunc, quoteStart));
    }

    /** Check if token is first parameter of ExecFunction, DoProc, or CreateUDObject */
    private isSpecialFunctionFirstParam(prev: Token | undefined, tokens: Token[], currentIndex: number): boolean {
        if (!prev || prev.text !== '(' || currentIndex < 2) { return false; }
        const funcToken = tokens[currentIndex - 2];
        if (funcToken && funcToken.type === TokenType.Identifier) {
            return SPECIAL_FUNCTIONS.includes(funcToken.text.toUpperCase());
        }
        return false;
    }

    private processStringToken(formattedToken: string, currentLineLen: number, maxLineLen: number, lockedIndent?: number, prev?: Token, tokens?: Token[], currentIndex?: number): string {
        let content = formattedToken.substring(1, formattedToken.length - 1);
        const quote = formattedToken.startsWith('"') ? '"' : "'";

        if (prev && tokens && currentIndex !== undefined) {
            if (this.isSpecialFunctionFirstParam(prev, tokens, currentIndex)) {
                return formattedToken;
            }
        }

        if (currentLineLen + getVisualLength(formattedToken, this.options.tabSize) > maxLineLen) {
            const alignLen = lockedIndent !== undefined ? lockedIndent : currentLineLen;
            const operatorLen = 2;
            const subsequentMax = maxLineLen - alignLen - operatorLen;
            const firstLineMax = maxLineLen - currentLineLen;

            let useSplit = false;
            const contentLen = content.length;
            const firstPartLen = Math.min(firstLineMax - 2, contentLen);

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

        // Never wrap method name from object
        if (i > 0 && tokens[i - 1].text === ':') { return false; }

        // Multi-dimensional Array Expansion
        if (token.text === '{') {
            if (i > 0 && tokens[i - 1].text === '{') { return true; }
            if (i > 1 && tokens[i - 1].text === ',' && tokens[i - 2].text === '}') { return true; }
        }
        if (token.text === '}' && i > 0 && tokens[i - 1].text === '}') { return true; }

        // Prevent wrapping after control flow keywords
        if (i > 0 && tokens[i - 1].type === TokenType.Keyword) {
            const kw = tokens[i - 1].text.toUpperCase();
            if ([':IF', ':WHILE', ':ELSEIF', ':CASE', ':RETURN', 'IF', 'WHILE', 'ELSEIF', 'CASE', 'RETURN'].includes(kw)) {
                return false;
            }
        }

        // Prevent wrapping after assignment operator
        if (i > 0 && tokens[i - 1].text === ':=') { return false; }

        // Prevent wrapping after opening parenthesis of special functions
        if (i > 0 && tokens[i - 1].text === '(' && this.isSpecialFunctionFirstParam(tokens[i - 1], tokens, i)) {
            return false;
        }

        // Lookahead: Binary Operator
        if (token.type === TokenType.Operator && token.text !== ':=' && i + 1 < tokens.length) {
            let lookAheadLen = checkLen;
            let j = i + 1;

            while (j < tokens.length) {
                const nextTok = tokens[j];
                if (nextTok.type === TokenType.Operator || nextTok.text === ',' || nextTok.text === ';') { break; }
                lookAheadLen += this.getEffectiveTokenLength(nextTok.text);
                if (nextTok.text.includes('\n')) { break; }
                if (nextTok.type !== TokenType.Punctuation) { lookAheadLen += 1; }
                j++;
                if (currentLineLen + lookAheadLen > maxLineLen) { return true; }
                if (j - i > 20) { break; }
            }
        }

        // Lookahead: Member Access
        if (token.type === TokenType.Identifier && i + 1 < tokens.length && tokens[i + 1].text === ':') {
            let lookAheadLen = checkLen;
            let k = i + 1;
            while (k < tokens.length) {
                const t = tokens[k];
                if (t.text === ':' || t.type === TokenType.Identifier || t.type === TokenType.Keyword) {
                    lookAheadLen += t.text.length;
                    k++;
                } else { break; }
            }
            if (currentLineLen + lookAheadLen > maxLineLen) { return true; }
        }

        // Lookahead: Identifier followed by (
        if ((token.type === TokenType.Identifier || token.type === TokenType.Keyword) && i + 1 < tokens.length) {
            if (tokens[i + 1].text === '(') {
                if (currentLineLen + checkLen + 1 > maxLineLen) { return true; }

                let lookAheadFun = checkLen + 1;
                let k = i + 2;
                let fundepth = 1;
                let prevFun = tokens[i + 1];

                while (k < tokens.length && fundepth > 0) {
                    const nextFun = tokens[k];
                    if (nextFun.text === '(') { fundepth++; }
                    else if (nextFun.text === ')') { fundepth--; }

                    lookAheadFun += this.getEffectiveTokenLength(nextFun.text);
                    if (this.shouldAddSpace(prevFun, nextFun)) { lookAheadFun += 1; }
                    prevFun = nextFun;

                    if (nextFun.text.includes('\n')) { break; }
                    k++;
                    if (currentLineLen + lookAheadFun > maxLineLen) { return true; }
                    if (k - i > 60) { break; }
                }
            }
        }

        // Lookahead: Opening Brace/Bracket
        if (token.text === '{' || token.text === '[') {
            let lookAheadLen = checkLen;
            let j = i + 1;
            let depth = 1;
            let prevToken = token;

            while (j < tokens.length && depth > 0) {
                const nextTok = tokens[j];
                if (nextTok.text === '{' || nextTok.text === '[') { depth++; }
                else if (nextTok.text === '}' || nextTok.text === ']') { depth--; }

                lookAheadLen += this.getEffectiveTokenLength(nextTok.text);
                if (this.shouldAddSpace(prevToken, nextTok)) { lookAheadLen += 1; }
                prevToken = nextTok;

                if (nextTok.text.includes('\n')) { break; }
                j++;
                if (currentLineLen + lookAheadLen > maxLineLen) { return true; }
                if (j - i > 50) { break; }
            }
        }

        return false;
    }

    private isWrappablePoint(token: Token, prev?: Token, tokens?: Token[], i?: number): boolean {
        if (token.text === ':') { return false; }
        if (prev && prev.text === ':') { return false; }

        if (prev && prev.text === '(' && tokens && i !== undefined && i >= 2) {
            if (this.isSpecialFunctionFirstParam(prev, tokens, i)) { return false; }
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

        const maxAlignTo = 60;
        if (alignTo > maxAlignTo) {
            const standardIndent = getVisualLength(baseIndentStr, this.options.tabSize) + 8;
            if (standardIndent < alignTo) { return standardIndent; }
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

    // --- Token Formatting ---

    private formatToken(token: Token, currentColumn: number = 0, customBaseIndent?: number, preserveCase: boolean = false): string {
        if (this.options['ssl.format.sql.enabled'] && token.sqlTokens && token.sqlTokens.length > 0) {
            const baseIndent = customBaseIndent !== undefined ? customBaseIndent : Math.max(0, currentColumn + 1 - 8);
            return this.sqlFormatter.formatSqlTokens(token.sqlTokens, token.text.charAt(0), baseIndent, undefined);
        }

        if (token.type === TokenType.Comment) {
            return token.text.split('\n').map(line => line.trimEnd()).join('\n');
        }

        if (token.type === TokenType.Keyword) {
            return preserveCase ? token.text : token.text.toUpperCase();
        } else if (token.type === TokenType.Identifier) {
            const lower = token.text.toLowerCase();
            const match = ALL_SQL_FUNCTIONS.find(f => f.toLowerCase() === lower);
            if (match) { return match; }
            return token.text;
        } else if (token.type === TokenType.Operator) {
            const upper = token.text.toUpperCase();
            if ((SSL_OPERATORS as readonly string[]).includes(upper) || (SSL_LITERALS as readonly string[]).includes(upper)) {
                return upper;
            }
            return token.text;
        }
        return token.text;
    }

    // --- Spacing Rules ---

    private shouldAddSpace(prev: Token, curr: Token, prePrev?: Token): boolean {
        if (curr.text === ';') { return false; }

        if (prev.text === ',') {
            if (curr.text === ',') { return false; }
            return true;
        }
        if (prev.text === ';') { return true; }

        // Operator Spacing
        if (curr.type === TokenType.Operator || prev.type === TokenType.Operator) {
            return this.handleOperatorSpacing(prev, curr, prePrev);
        }

        // Punctuation
        if (prev.text === ')' && curr.type === TokenType.Identifier) { return false; }
        if (prev.text === ')' && curr.text === ')') { return false; }

        if (prev.text === ')') {
            if (curr.text === ',') { return false; }
            if (curr.type === TokenType.Keyword) { return true; }
            return true;
        }

        if (curr.type === TokenType.Punctuation) {
            if (curr.text === ',' || curr.text === ')') { return false; }
        }

        if (prev.type === TokenType.Identifier && curr.text === '(') { return false; }
        if (prev.type === TokenType.Keyword && curr.text === '(') { return true; }
        if (prev.type === TokenType.Keyword && curr.text === '{') { return true; }

        // Member access
        if (prev.type === TokenType.Identifier && curr.type === TokenType.Keyword && curr.text.startsWith(':')) {
            return false;
        }

        // Array access
        if (curr.text === '[' && this.isWord(prev)) { return false; }

        if (this.isWord(prev) && this.isWord(curr)) { return true; }

        return false;
    }

    private handleOperatorSpacing(prev: Token, curr: Token, prePrev?: Token): boolean {
        if (prev.text === '!') { return false; }

        // Postfix Increment/Decrement
        if ((curr.text === '++' || curr.text === '--') && (prev.type === TokenType.Identifier || prev.type === TokenType.Number || prev.type === TokenType.Keyword)) {
            return false;
        }

        // Unary Minus/Plus
        if ((curr.text === '-' || curr.text === '+' || curr.text === '++' || curr.text === '--') && (prev.type === TokenType.Punctuation || prev.type === TokenType.Keyword)) {
            const isOpenerOrSeparator = ['(', '{', '[', ',', ';', ':'].includes(prev.text);
            if (prev.type === TokenType.Punctuation && isOpenerOrSeparator) { return false; }
        }

        // Unary context check
        if (prev.text === '-' || prev.text === '+' || prev.text === '++' || prev.text === '--') {
            if (prePrev) {
                const isOpenerOrSeparator = ['(', '{', '[', ',', ';', ':'].includes(prePrev.text);
                if (prePrev.type === TokenType.Operator || prePrev.type === TokenType.Keyword || (prePrev.type === TokenType.Punctuation && isOpenerOrSeparator)) {
                    return false;
                }
            } else {
                return false;
            }
        }

        return true;
    }

    private isWord(t: Token): boolean {
        return t.type === TokenType.Keyword || t.type === TokenType.Identifier || t.type === TokenType.Number || t.type === TokenType.String;
    }

    private getIndent(level: number): string {
        return this.indentString.repeat(level);
    }
}

