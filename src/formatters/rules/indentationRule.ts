import { FormattingRule, FormattingContext } from "../formattingProvider";

/**
 * Handles consistent indentation for SSL blocks
 */
export class IndentationRule implements FormattingRule {
    name = "Indentation";
    description = "Ensures consistent indentation for SSL blocks";
    apply(line: string, context: FormattingContext): string {
        // Don't indent empty lines
        if (line.trim() === "") {
            return "";
        }

        // Don't indent line-starting comments
        if (line.trim().startsWith("/*")) {
            return line.trim();
        }

        // Use the context's indentLevel directly (it's already calculated correctly)
        const indentLevel = context.indentLevel;

        // Use the specified tabSize from formatting options
        const tabSize = context.options.tabSize || 4; // Default to 4 if not specified
        const indentString = context.options.insertSpaces
            ? " ".repeat(tabSize * indentLevel)
            : "\t".repeat(indentLevel);

        return indentString + line.trim();
    }
}
