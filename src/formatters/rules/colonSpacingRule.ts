import { FormattingRule, FormattingContext } from "../formattingProvider";

/**
 * Handles spacing after colons in SSL keywords
 */
export class ColonSpacingRule implements FormattingRule {
    name = "Colon Spacing";
    description = "Ensures proper spacing after colons in SSL keywords";

    // Known SSL keywords that can appear after a colon
    private sslKeywords = [
        "procedure",
        "endproc",
        "parameters",
        "default",
        "declare",
        "public",
        "if",
        "else",
        "endif",
        "while",
        "endwhile",
        "for",
        "next",
        "to",
        "begincase",
        "case",
        "otherwise",
        "endcase",
        "exitcase",
        "try",
        "catch",
        "finally",
        "endtry",
        "error",
        "exitwhile",
        "exitfor",
        "loop",
        "return",
        "class",
        "inherit",
        "region",
        "endregion",
        "include",
        "label",
    ];
    apply(line: string, context: FormattingContext): string {
        // Handle SSL keywords that start with :
        const trimmedLine = line.trim();
        if (trimmedLine.startsWith(":")) {
            // Get leading whitespace from original line
            const leadingWhitespace = line.substring(0, line.length - line.trimStart().length);

            // Handle multiple keywords on the same line (e.g. ":procedure myProc;:parametersp1;")
            if (trimmedLine.indexOf(":", 1) > -1) {
                const parts = trimmedLine.split(":");
                let result = "";

                // Process the first part (it won't have a colon prefix in the split result)
                if (parts[0] === "") {
                    // First part was just the colon, continue with the second part
                    parts.shift();
                } else {
                    // Unexpected format - return original
                    return line;
                }

                // Process each keyword
                for (let i = 0; i < parts.length; i++) {
                    const part = parts[i];
                    const formattedPart = this.formatKeyword(":" + part);
                    result += formattedPart;

                    // Add a space between keywords if needed
                    if (i < parts.length - 1 && !formattedPart.endsWith(" ")) {
                        result += " ";
                    }
                }

                return leadingWhitespace + result;
            }

            // Handle simple single-keyword case
            return leadingWhitespace + this.formatKeyword(trimmedLine);
        }
        return line;
    }

    private formatKeyword(text: string): string {
        // Remove the colon and get the rest
        const afterColon = text.substring(1);

        // Try to find a matching keyword at the beginning
        const matchedKeyword = this.sslKeywords.find((keyword) =>
            afterColon.toLowerCase().startsWith(keyword.toLowerCase())
        );

        if (matchedKeyword) {
            // Split the line into keyword and rest
            const keywordLength = matchedKeyword.length;
            const keyword = matchedKeyword.toUpperCase();
            let rest = afterColon.substring(keywordLength);

            // Handle semicolon properly
            const hasSemicolon = rest.endsWith(";");
            if (hasSemicolon) {
                rest = rest.substring(0, rest.length - 1).trim();
                rest = rest ? ` ${rest};` : ";";
            } else {
                rest = rest.trim();
                if (rest) {
                    rest = ` ${rest}`;
                }
            }

            // Format the result
            return `:${keyword}${rest}`;
        }

        return text;
    }
}
