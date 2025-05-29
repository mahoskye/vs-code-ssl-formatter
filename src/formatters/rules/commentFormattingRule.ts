import type { FormattingRule, FormattingContext } from "../formattingProvider";
import { SSLTokenizer, TokenType } from "../../core/tokenizer";

/**
 * Special class to handle SSL-specific syntax like comments
 */
export class CommentFormattingRule implements FormattingRule {
    name = "Comment Formatting";
    description =
        "Ensures proper formatting of SSL comments, including start/end syntax, spacing, and region tags.";

    apply(line: string, context: FormattingContext): string {
        const originalLine = line;

        const leadingWhitespaceMatch = line.match(/^(\s*)/);
        const leadingWhitespace = leadingWhitespaceMatch ? leadingWhitespaceMatch[0] : "";

        let contentToProcess = line.substring(leadingWhitespace.length);
        const trailingWhitespaceMatch = contentToProcess.match(/(\s*)$/);
        const trailingWhitespace = trailingWhitespaceMatch ? trailingWhitespaceMatch[0] : "";

        contentToProcess = contentToProcess.substring(
            0,
            contentToProcess.length - trailingWhitespace.length
        );

        if (
            contentToProcess.length === 0 &&
            leadingWhitespace.length + trailingWhitespace.length === originalLine.length
        ) {
            // Line is only whitespace, or empty
            return originalLine;
        }

        let formattedContent = contentToProcess;

        // 1. Normalize comment start: "/ *" to "/*", including various whitespace characters like tabs
        // This replacement should only happen if it's at the beginning of the contentToProcess
        if (formattedContent.match(/^\/\s+\*/)) {
            // Checks for / followed by one or more whitespace chars (space, tab, etc.) then *
            formattedContent = formattedContent.replace(/^\/\s+\*/, "/*");
        }

        // Only proceed if it actually starts with "/*"
        if (formattedContent.startsWith("/*")) {
            // 2. Handle region/endregion: normalize to lowercase and ensure single space after keyword
            const lowerContent = formattedContent.toLowerCase();
            if (lowerContent.startsWith("/* region")) {
                const regionContentMatch = formattedContent.match(
                    /^\/\*\s*region\s+(.*?)(;)?\s*$/i
                );
                if (regionContentMatch) {
                    const regionText = regionContentMatch[1].trim();
                    formattedContent = `/* region ${regionText}`;
                } else {
                    // Malformed but clearly a region attempt, normalize start
                    formattedContent = "/* region";
                }
            } else if (lowerContent.startsWith("/* endregion")) {
                formattedContent = "/* endregion";
            }
            // 3. Ensure single space after "/*" (unless "/*;" or already spaced, or region/endregion handled above)
            else if (
                formattedContent !== "/*" && // Will become "/*;" later if just "/*"
                !formattedContent.startsWith("/* region") &&
                !formattedContent.startsWith("/* endregion")
            ) {
                if (formattedContent.match(/^\/\*[^\s;]/)) {
                    // e.g., /*Something or /*;
                    if (formattedContent !== "/*;") {
                        // don't add space if it's just /*;
                        formattedContent = formattedContent.replace(/^\/\*([^;])/, "/* $1");
                    }
                } else {
                    // Normalize multiple spaces to one, e.g., /*   Something
                    formattedContent = formattedContent.replace(/^\/\*\s{2,}([^;])/, "/* $1");
                }
            }

            // 4. Ensure comments end with a semicolon
            if (!formattedContent.endsWith(";")) {
                formattedContent += ";";
            }

            // If the formatted content is different from the initial content (after stripping whitespace)
            if (formattedContent !== contentToProcess) {
                return leadingWhitespace + formattedContent + trailingWhitespace;
            }
        }

        // If no relevant changes were made or it's not a comment this rule handles,
        // return the original line to avoid unintended modifications.
        return originalLine;
    }

    /**
     * Determines if a line contains an SSL comment
     */
    private isCommentLine(line: string): boolean {
        const trimmed = line.trim();
        return trimmed.startsWith("/*");
    }

    /**
     * Determines if a comment is a region marker
     */
    private isRegionComment(line: string): boolean {
        const trimmed = line.trim().toLowerCase();
        return trimmed.startsWith("/* region") || trimmed.startsWith("/* endregion");
    }

    /**
     * Validates comment syntax according to SSL grammar
     */
    private validateCommentSyntax(line: string): boolean {
        // Use tokenizer to validate comment syntax
        try {
            const tokenizer = new SSLTokenizer(line);
            const tokens = tokenizer.tokenize();

            // Check if we have proper comment tokens
            return tokens.some(
                (token) =>
                    token.type === TokenType.blockComment ||
                    token.type === TokenType.singleLineComment ||
                    token.type === TokenType.regionComment ||
                    token.type === TokenType.endregionComment
            );
        } catch (error) {
            return false;
        }
    }
}
