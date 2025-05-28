import { FormattingRule, FormattingContext } from "../formattingProvider";

/**
 * Handles spacing after commas
 */
export class CommaSpacingRule implements FormattingRule {
    name = "Comma Spacing";
    description = "Ensures proper spacing after commas";

    apply(line: string, context: FormattingContext): string {
        // Process each comma individually to respect string boundaries
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
