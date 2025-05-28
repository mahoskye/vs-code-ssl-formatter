import { FormattingRule, FormattingContext } from "../formattingProvider";

/**
 * Handles consistent indentation for SSL blocks
 */
export class IndentationRule implements FormattingRule {
    name = "IndentationRule"; // Changed name to be unique and reflect it's a class name
    description = "Ensures consistent indentation for SSL blocks";

    apply(line: string, context: FormattingContext): string {
        // The core indentation logic has been moved to formattingProvider.ts (analyzeLineContext and formatText loop).
        // This rule might be deprecated or repurposed for very specific indentation adjustments
        // not covered by the main logic, or for ensuring lines adhere to the calculated indentLevel.

        // For now, this rule will simply ensure the line is trimmed and the pre-calculated indentation is applied.
        // However, the formattingProvider already does this after all rules run.
        // So, this rule, in its current state, might be redundant if formattingProvider handles final indentation.

        if (line.trim() === "") {
            // formattingProvider should handle empty line indentation (e.g. keep as is, or remove all whitespace)
            return ""; // Return empty string, provider will handle if it should be empty or just whitespace
        }

        // The `indentLevel` in `context` is already determined by `analyzeLineContext`.
        // The `formattingProvider` applies this indentation *after* all rules.
        // Therefore, this rule should not re-apply or calculate indentation itself.
        // It could, in the future, be used for complex adjustments *before* the final indentation pass.

        // Return the line as is; formattingProvider will handle the final indentation.
        return line;
    }

    // The methods below are now largely superseded by logic in formattingProvider.ts
    // and the use of lineTokens/AST for more accurate context analysis.
    // They are kept here for reference or potential future use in a more limited capacity.

    private shouldNotIndent(line: string): boolean {
        // This logic might be better placed in analyzeLineContext or handled by specific token types.
        if (line.startsWith("/*")) {
            return true;
        }
        if (line.startsWith(":PROCEDURE")) {
            return true;
        }
        if (line.startsWith(":CLASS")) {
            return true;
        }
        return false;
    }

    private calculateIndentLevel(line: string, context: FormattingContext): number {
        // This is now the primary responsibility of analyzeLineContext in formattingProvider.
        // Kept for reference.
        if (this.isBlockEnd(line)) {
            return Math.max(0, context.indentLevel - 1);
        }
        if (this.isMiddleBlock(line)) {
            return Math.max(0, context.indentLevel - 1);
        }
        return context.indentLevel;
    }

    private isBlockEnd(line: string): boolean {
        // Prefer token-based checks (see analyzeLineContext in formattingProvider)
        const blockEndKeywords = [
            ":ENDPROC",
            ":ENDIF",
            ":ENDWHILE",
            ":NEXT",
            ":ENDCASE",
            ":ENDTRY",
            ":ENDREGION",
            ":ENDINLINECODE",
            ":ENDCLASS", // Added
        ];
        return blockEndKeywords.some((keyword) => line.toUpperCase().startsWith(keyword));
    }

    private isMiddleBlock(line: string): boolean {
        // Prefer token-based checks (see analyzeLineContext in formattingProvider)
        const middleBlockKeywords = [":ELSE", ":CATCH", ":FINALLY", ":CASE", ":OTHERWISE"];
        return middleBlockKeywords.some((keyword) => line.toUpperCase().startsWith(keyword));
    }
}
