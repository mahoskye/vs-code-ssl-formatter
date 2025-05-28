import type { FormattingRule, FormattingContext } from "../formattingProvider";
import { SSLTokenizer, TokenType } from "../../core/tokenizer";

/**
 * Special class to handle SSL-specific syntax like comments
 */
export class CommentFormattingRule implements FormattingRule {
    name = "Comment Formatting";
    description = "Ensures proper formatting of SSL comments";

    apply(line: string, context: FormattingContext): string {
        let result = line;

        // Handle SSL comment formatting based on the grammar
        // SSL comments start with /* and end with ;

        // Fix spacing in comment start - remove space between / and *
        result = result.replace(/\/\s+\*/g, "/*");

        // Ensure proper comment termination with semicolon
        // Handle block comments that might span multiple lines
        if (result.includes("/*") && !result.includes(";")) {
            // If this is a comment line without semicolon termination, add it
            const trimmed = result.trim();
            if (trimmed.startsWith("/*") && !trimmed.endsWith(";")) {
                result = result.replace(/\s*$/, ";");
            }
        }

        // Handle region comments - ensure proper casing and format
        result = result.replace(/\/\*\s*region\s+/gi, "/* region ");
        result = result.replace(/\/\*\s*endregion\s*/gi, "/* endregion ");

        // Ensure single space after /* for readability (except for empty comments)
        result = result.replace(/\/\*([^\s;])/g, "/* $1");

        // Handle special case where comment content might need formatting
        // Preserve the internal structure but ensure consistent spacing
        result = result.replace(/\/\*\s{2,}/g, "/* ");

        return result;
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
