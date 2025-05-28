import { FormattingRule, FormattingContext } from "../formattingProvider";
import { SSLTokenizer, TokenType } from "../../core/tokenizer";

/**
 * Handles spacing after colons in SSL keywords
 */
export class ColonSpacingRule implements FormattingRule {
    name = "Colon Spacing";
    description = "Ensures proper spacing after colons in SSL keywords";

    // SSL keywords that can appear after a colon - derived from TokenType enum
    private sslKeywords = new Set([
        TokenType.procedure,
        TokenType.endproc,
        TokenType.parameters,
        TokenType.default,
        TokenType.declare,
        TokenType.public,
        TokenType.if,
        TokenType.else,
        TokenType.endif,
        TokenType.while,
        TokenType.endwhile,
        TokenType.for,
        TokenType.next,
        TokenType.to,
        TokenType.begincase,
        TokenType.case,
        TokenType.otherwise,
        TokenType.endcase,
        TokenType.exitcase,
        TokenType.try,
        TokenType.catch,
        TokenType.finally,
        TokenType.endtry,
        TokenType.error,
        TokenType.exitwhile,
        TokenType.exitfor,
        TokenType.loop,
        TokenType.return,
        TokenType.class,
        TokenType.inherit,
        TokenType.region,
        TokenType.endregion,
        TokenType.include,
        TokenType.label,
        TokenType.doProc,
        TokenType.execFunction,
        TokenType.execUDF,
        TokenType.begininlinecode,
        TokenType.endinlinecode,
    ]);

    apply(line: string, context: FormattingContext): string {
        // Handle SSL keywords that start with :
        const trimmedLine = line.trim();
        if (trimmedLine.startsWith(":")) {
            // Get leading whitespace from original line
            const leadingWhitespace = line.substring(0, line.length - line.trimStart().length);

            // Use tokenizer to properly parse the line
            try {
                const tokenizer = new SSLTokenizer(trimmedLine);
                const tokens = tokenizer.tokenize();

                // Filter out whitespace and newline tokens for processing
                const meaningfulTokens = tokens.filter(
                    (token) =>
                        token.type !== TokenType.whitespace && token.type !== TokenType.newline
                );

                if (meaningfulTokens.length === 0) {
                    return line;
                }

                // Process tokens to format keywords with proper spacing
                let result = "";
                let i = 0;

                while (i < meaningfulTokens.length) {
                    const token = meaningfulTokens[i];

                    if (token.type === TokenType.colon && i + 1 < meaningfulTokens.length) {
                        const nextToken = meaningfulTokens[i + 1];

                        // Check if the next token is a known SSL keyword
                        if (this.sslKeywords.has(nextToken.type)) {
                            result += `:${nextToken.value.toUpperCase()}`;
                            i += 2; // Skip both colon and keyword tokens

                            // Add proper spacing for any following content
                            if (i < meaningfulTokens.length) {
                                const followingToken = meaningfulTokens[i];

                                // Handle semicolon specially - no space before it
                                if (followingToken.type === TokenType.semicolon) {
                                    result += followingToken.value;
                                    i++;
                                } else {
                                    // Add space before other content
                                    result += " ";
                                    // Continue processing remaining tokens
                                    while (i < meaningfulTokens.length) {
                                        const currentToken = meaningfulTokens[i];
                                        result += currentToken.value;

                                        // Add space between tokens except before semicolons
                                        if (
                                            i + 1 < meaningfulTokens.length &&
                                            meaningfulTokens[i + 1].type !== TokenType.semicolon
                                        ) {
                                            result += " ";
                                        }
                                        i++;
                                    }
                                }
                            }
                        } else {
                            // Not a keyword, keep as-is
                            result += token.value;
                            i++;
                        }
                    } else {
                        // Regular token, add as-is
                        result += token.value;
                        i++;
                    }
                }

                return leadingWhitespace + result;
            } catch (error) {
                // Fallback to original logic if tokenization fails
                return this.fallbackFormat(line, trimmedLine, leadingWhitespace);
            }
        }

        return line;
    }

    /**
     * Fallback formatting logic for cases where tokenization fails
     */
    private fallbackFormat(line: string, trimmedLine: string, leadingWhitespace: string): string {
        // Handle multiple keywords on the same line (e.g. ":procedure myProc;:parameters p1;")
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
                const formattedPart = this.formatKeywordFallback(":" + part);
                result += formattedPart;

                // Add a space between keywords if needed
                if (i < parts.length - 1 && !formattedPart.endsWith(" ")) {
                    result += " ";
                }
            }

            return leadingWhitespace + result;
        }

        // Handle simple single-keyword case
        return leadingWhitespace + this.formatKeywordFallback(trimmedLine);
    }

    /**
     * Fallback keyword formatting for backwards compatibility
     */
    private formatKeywordFallback(text: string): string {
        // Remove the colon and get the rest
        const afterColon = text.substring(1);

        // Convert TokenType values to strings for comparison
        const keywordStrings = Array.from(this.sslKeywords).map((tokenType) =>
            tokenType.toString()
        );

        // Try to find a matching keyword at the beginning
        const matchedKeyword = keywordStrings.find((keyword) =>
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
