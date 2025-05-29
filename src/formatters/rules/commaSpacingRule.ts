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
        // Process comma tokens from right to left.
        // Their `position.offset` refers to the original line.
        // When modifying `result` from right to left, the absolute positions
        // of commas to the left are not affected by changes to their right.
        const commaTokens = tokens
            .filter((token) => token.type === TokenType.comma)
            .sort((a, b) => b.position.offset - a.position.offset); // Explicit sort R-L

        for (const commaToken of commaTokens) {
            const commaPos = commaToken.position.offset; // Use original offset directly

            // Boundary checks for safety, though with correct R-L, commaPos should be valid in current result
            if (commaPos < 0 || commaPos >= result.length) {
                continue;
            }

            const afterCommaPos = commaPos + 1;

            if (afterCommaPos < result.length) {
                // Comma is not the last char
                const restOfLineAfterComma = result.substring(afterCommaPos);
                const whitespaceMatch = restOfLineAfterComma.match(/^(\\s*)/);
                const existingWhitespace = whitespaceMatch ? whitespaceMatch[1] : "";

                let desiredSpacing = " ";
                // Pass current `result` to shouldSkipSpacing
                if (this.shouldSkipSpacing(result, commaPos, tokens)) {
                    desiredSpacing = "";
                }

                if (existingWhitespace !== desiredSpacing) {
                    const beforeCommaPart = result.substring(0, commaPos + 1); // Includes the comma
                    const afterExistingWhitespacePart = result.substring(
                        afterCommaPos + existingWhitespace.length
                    );
                    result = beforeCommaPart + desiredSpacing + afterExistingWhitespacePart;
                }
            }
            // Else: Comma is the last character on the line.
            // Current logic doesn't explicitly add a space if missing and desired here,
            // but shouldSkipSpacing would likely return true for EOL cases anyway.
        }
        return result;
    }

    // Ensure shouldSkipSpacing uses the current line state
    private shouldSkipSpacing(currentLine: string, commaPos: number, tokens: Token[]): boolean {
        // Check if comma is at end of line
        if (commaPos >= currentLine.length - 1) {
            // Use currentLine
            return true;
        }

        // Check if followed by closing bracket/brace/paren
        const nextChar = currentLine[commaPos + 1]; // Use currentLine
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
