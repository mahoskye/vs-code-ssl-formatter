import type { FormattingRule, FormattingContext } from "../formattingProvider";

/**
 * Special class to handle SSL-specific syntax like comments
 */
export class CommentFormattingRule implements FormattingRule {
    name = "Comment Formatting";
    description = "Ensures proper formatting of SSL comments";

    apply(line: string, context: FormattingContext): string {
        let result = line;

        // Don't modify the content inside comment blocks, just ensure the format is correct
        // SSL comments start with /* without a space in between
        result = result.replace(/\/\s+\*/g, "/*");

        return result;
    }
}
