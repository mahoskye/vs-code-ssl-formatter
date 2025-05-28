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
        let offset = 0; // Track cumulative changes to line length

        // Process comma tokens from right to left to maintain token positions
        const commaTokens = tokens.filter((token) => token.type === TokenType.comma).reverse();

        for (const commaToken of commaTokens) {
            const commaPos = commaToken.position.offset + offset;

            // Skip if comma position is invalid
            if (commaPos < 0 || commaPos >= result.length) {
                continue;
            }

            // Check if we need to add or normalize space after comma
            const afterCommaPos = commaPos + 1;

            if (afterCommaPos < result.length) {
                const charAfterComma = result[afterCommaPos];
                const restOfLine = result.substring(afterCommaPos);

                // Check for existing whitespace after comma
                const whitespaceMatch = restOfLine.match(/^(\s*)/);
                const existingWhitespace = whitespaceMatch ? whitespaceMatch[1] : "";

                // Determine desired spacing based on context
                let desiredSpacing = " "; // Default: single space

                // Special cases where no space is needed
                if (this.shouldSkipSpacing(result, commaPos, tokens)) {
                    desiredSpacing = "";
                }

                // Apply spacing if different from current
                if (existingWhitespace !== desiredSpacing) {
                    const beforeComma = result.substring(0, afterCommaPos);
                    const afterWhitespace = result.substring(
                        afterCommaPos + existingWhitespace.length
                    );

                    result = beforeComma + desiredSpacing + afterWhitespace;
                    offset += desiredSpacing.length - existingWhitespace.length;
                }
            }
        }

        return result;
    }

    private shouldSkipSpacing(line: string, commaPos: number, tokens: Token[]): boolean {
        // Check if comma is at end of line
        if (commaPos >= line.length - 1) {
            return true;
        }

        // Check if followed by closing bracket/brace/paren
        const nextChar = line[commaPos + 1];
        if (nextChar === "}" || nextChar === "]" || nextChar === ")") {
            return true;
        }

        // Check for array literal context where compact formatting might be preferred
        const prevBraceToken = this.findPreviousToken(tokens, commaPos, TokenType.lbrace);
        const nextBraceToken = this.findNextToken(tokens, commaPos, TokenType.rbrace);

        if (prevBraceToken && nextBraceToken) {
            // In array literal - check if it's a compact array
            const arrayContent = line.substring(
                prevBraceToken.position.offset + 1,
                nextBraceToken.position.offset
            );

            // If array is short and simple, keep compact
            if (arrayContent.length < 30 && !arrayContent.includes("\n")) {
                const elements = arrayContent.split(",");
                if (elements.every((elem) => elem.trim().match(/^[a-zA-Z0-9_"'.]+$/))) {
                    return false; // Still add space for readability
                }
            }
        }

        return false;
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
        const lastCommentStart = beforeComma.lastIndexOf("/*");
        const lastCommentEnd = beforeComma.lastIndexOf(";");

        if (
            lastCommentStart !== -1 &&
            (lastCommentEnd === -1 || lastCommentStart > lastCommentEnd)
        ) {
            return true; // Inside comment
        }

        // Special case for bracket notation [e*f]
        if (beforeComma.lastIndexOf("[") > beforeComma.lastIndexOf("]")) {
            return true;
        }

        // More accurate string detection
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
