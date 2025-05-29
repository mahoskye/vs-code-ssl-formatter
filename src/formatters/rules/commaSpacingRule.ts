import { FormattingRule, FormattingContext } from "../formattingProvider";
import { SSLTokenizer, TokenType, Token } from "../../core/tokenizer";

/**
 * Handles spacing after commas using SSL tokenizer for accurate context detection
 */
export class CommaSpacingRule implements FormattingRule {
    name = "Comma Spacing";
    description = "Ensures proper spacing after commas using SSL tokenizer";

    apply(line: string, context: FormattingContext): string {
        try {
            const tokenizer = new SSLTokenizer(line);
            const tokens = tokenizer.tokenize();

            // If tokenization fails or no tokens, return original line
            if (!tokens || tokens.length === 0) {
                return line;
            }

            return this.formatCommasWithTokens(line, tokens);
        } catch (error) {
            // Fallback to original logic if tokenization fails
            return this.fallbackCommaFormatting(line);
        }
    }

    private formatCommasWithTokens(line: string, tokens: Token[]): string {
        let result = line;
        const commaTokens = tokens
            .filter((token) => token.type === TokenType.comma)
            .sort((a, b) => b.position.offset - a.position.offset); // Process R-L

        for (const commaToken of commaTokens) {
            const commaPos = commaToken.position.offset;

            // Safety check: ensure commaPos is valid and points to a comma in the current result string.
            if (commaPos < 0 || commaPos >= result.length || result[commaPos] !== ",") {
                continue;
            }

            // If comma is in a string, comment, or bracket notation, skip formatting it.
            if (this.isCommaInStringOrComment(result, commaPos)) {
                continue;
            }

            // Part 1: Everything up to and including the comma
            const prefix = result.substring(0, commaPos + 1);

            // Part 2: Identify the start of actual content after the comma, skipping any existing spaces
            let contentStartPos = commaPos + 1;
            while (contentStartPos < result.length && result[contentStartPos] === " ") {
                contentStartPos++;
            }
            const suffix = result.substring(contentStartPos);

            // Part 3: Decide whether a space should be inserted after the comma
            let shouldInsertSpace = true; // Default to true

            if (contentStartPos >= result.length) {
                shouldInsertSpace = false;
            } else {
                const nextActualCharInResult = result[contentStartPos];

                // Condition 2: Comma is followed by a closing bracket, brace, or parenthesis
                if (
                    nextActualCharInResult === "}" ||
                    nextActualCharInResult === "]" ||
                    nextActualCharInResult === ")"
                ) {
                    shouldInsertSpace = false;
                } else if (nextActualCharInResult === ",") {
                    // Condition 3: Comma is followed by another comma (e.g., the first comma in ````,````)
                    shouldInsertSpace = false;
                } else if (nextActualCharInResult === "\\n" || nextActualCharInResult === "\\r") {
                    // Condition 4: Comma is followed by a newline character
                    shouldInsertSpace = false;
                }
            }

            // Part 4: Reconstruct the string
            if (shouldInsertSpace) {
                result = prefix + " " + suffix;
            } else {
                result = prefix + suffix;
            }
        }

        return result;
    }

    // Ensure shouldSkipSpacing uses the current line state
    private shouldSkipSpacing(currentLine: string, commaPos: number, tokens: Token[]): boolean {
        // Check if comma is at end of line
        if (commaPos >= currentLine.length - 1) {
            return true;
        }

        // Check if followed by closing bracket/brace/paren
        const nextChar = currentLine[commaPos + 1];
        if (nextChar === "}" || nextChar === "]" || nextChar === ")") {
            return true;
        }

        return false; // Default: do not skip spacing (i.e., a space is generally desired).
    }

    private findPreviousToken(
        tokens: Token[],
        position: number,
        tokenType: TokenType
    ): Token | null {
        for (let i = tokens.length - 1; i >= 0; i--) {
            const token = tokens[i];
            if (token.position.offset < position && token.type === tokenType) {
                return token;
            }
        }
        return null;
    }

    private findNextToken(tokens: Token[], position: number, tokenType: TokenType): Token | null {
        for (const token of tokens) {
            if (token.position.offset > position && token.type === tokenType) {
                return token;
            }
        }
        return null;
    }

    private fallbackCommaFormatting(line: string): string {
        // Original logic as fallback
        let result = line;
        let position = 0;

        while (position < result.length) {
            const commaIndex = result.indexOf(",", position);
            if (commaIndex === -1) {
                break;
            }

            // Check if this comma is inside a string or comment
            if (!this.isCommaInStringOrComment(result, commaIndex)) {
                // Add space after comma if needed
                if (commaIndex + 1 < result.length && result[commaIndex + 1] !== " ") {
                    result =
                        result.substring(0, commaIndex + 1) +
                        " " +
                        result.substring(commaIndex + 1);
                }
                // Normalize multiple spaces after comma to single space
                const afterComma = result.substring(commaIndex + 1);
                const spacesMatch = afterComma.match(/^(\s+)/);
                if (spacesMatch && spacesMatch[1].length > 1) {
                    result =
                        result.substring(0, commaIndex + 1) +
                        " " +
                        afterComma.substring(spacesMatch[1].length);
                }
            }
            position = commaIndex + 1;
        }

        return result;
    }

    private isCommaInStringOrComment(text: string, commaIndex: number): boolean {
        const beforeComma = text.substring(0, commaIndex);

        // Check for comment context
        // For block comments /* ... */
        const lastBlockCommentStart = beforeComma.lastIndexOf("/*");
        if (lastBlockCommentStart !== -1) {
            const lastBlockCommentEnd = beforeComma.lastIndexOf("*/");
            if (lastBlockCommentEnd === -1 || lastBlockCommentStart > lastBlockCommentEnd) {
                // We are inside a /* ... block comment that hasn't ended before the comma
                return true;
            }
        }
        // For EOL comments ;
        // The previous EOL comment detection based on ';' was too aggressive
        // and conflicted with ';' as a potential statement separator.
        // If SSL has specific EOL comment syntax (e.g., REM, //, or specific use of ;),
        // it should be handled by the tokenizer or a more context-aware rule.
        // For now, removing the generic ';' EOL check to allow formatting
        // in cases like "statement1; statement2_with_commas".

        // NOTE: The check for general bracket notation `[e*f]` like:
        // if (beforeComma.lastIndexOf("[") > beforeComma.lastIndexOf("]")) { return true; }
        // was removed. It was too broad and incorrectly prevented formatting of commas
        // in array access like `myArray[1,2,3]`. If there's a special SSL bracket
        // notation (distinct from array access) where commas must be preserved,
        // it needs to be handled by the tokenizer (e.g., by not producing
        // TokenType.comma for those internal commas) or by a more context-aware rule.

        // More accurate string detection for the comma's position
        let inSingleQuote = false;
        let inDoubleQuote = false;
        let i = 0;

        // Walk through the string and toggle quote state
        while (i < commaIndex) {
            if (text[i] === "'" && (i === 0 || text[i - 1] !== "\\")) {
                inSingleQuote = !inSingleQuote;
            } else if (text[i] === '"' && (i === 0 || text[i - 1] !== "\\")) {
                inDoubleQuote = !inDoubleQuote;
            }
            i++;
        }

        return inSingleQuote || inDoubleQuote;
    }
}
