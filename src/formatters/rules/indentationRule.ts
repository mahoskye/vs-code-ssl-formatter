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

        const trimmedLine = line.trim();

        // Handle special cases that should not be indented or have special indentation
        if (this.shouldNotIndent(trimmedLine)) {
            return trimmedLine;
        }

        // Calculate indentation based on SSL block structure
        const indentLevel = this.calculateIndentLevel(trimmedLine, context);

        // Use the specified tabSize from formatting options
        const tabSize = context.options.tabSize || 4;
        const indentString = context.options.insertSpaces
            ? " ".repeat(tabSize * indentLevel)
            : "\t".repeat(indentLevel);

        return indentString + trimmedLine;
    }

    private shouldNotIndent(line: string): boolean {
        // Don't indent comments that start at the beginning of the line
        if (line.startsWith("/*")) {
            return true;
        }

        // Don't indent procedure declarations
        if (line.startsWith(":PROCEDURE")) {
            return true;
        }

        // Don't indent class declarations
        if (line.startsWith(":CLASS")) {
            return true;
        }

        return false;
    }

    private calculateIndentLevel(line: string, context: FormattingContext): number {
        // Handle SSL-specific block structures
        if (this.isBlockEnd(line)) {
            // Block end statements should be at the same level as their opening statement
            return Math.max(0, context.indentLevel - 1);
        }

        if (this.isMiddleBlock(line)) {
            // Middle block statements (:ELSE, :CATCH, :FINALLY, :CASE, :OTHERWISE)
            // should be at the same level as their opening statement
            return Math.max(0, context.indentLevel - 1);
        }

        // Use the context's calculated indentLevel for everything else
        return context.indentLevel;
    }

    private isBlockEnd(line: string): boolean {
        const blockEndKeywords = [
            ":ENDPROC",
            ":ENDIF",
            ":ENDWHILE",
            ":NEXT",
            ":ENDCASE",
            ":ENDTRY",
            ":ENDREGION",
            ":ENDINLINECODE",
        ];

        return blockEndKeywords.some((keyword) => line.toUpperCase().startsWith(keyword));
    }

    private isMiddleBlock(line: string): boolean {
        const middleBlockKeywords = [":ELSE", ":CATCH", ":FINALLY", ":CASE", ":OTHERWISE"];

        return middleBlockKeywords.some((keyword) => line.toUpperCase().startsWith(keyword));
    }
}
