import { FormattingRule, FormattingContext } from "../formattingProvider";
import { SSLTokenizer, TokenType, Token } from "../../core/tokenizer";

/**
 * Handles spacing after colons in SSL keywords
 */
export class ColonSpacingRule implements FormattingRule {
    name = "Colon Spacing";
    description = "Ensures proper spacing after colons in SSL keywords";

    private sslKeywordsEnumValues = new Set([
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

    private static standaloneKeywordsLC: Set<string>;
    private allKeywordsLC: Set<string>; // Added for robust keyword detection

    constructor() {
        if (!ColonSpacingRule.standaloneKeywordsLC) {
            ColonSpacingRule.standaloneKeywordsLC = new Set(
                [
                    TokenType.endproc,
                    TokenType.else,
                    TokenType.endif,
                    TokenType.endwhile,
                    TokenType.next,
                    TokenType.otherwise,
                    TokenType.endcase,
                    TokenType.exitcase,
                    TokenType.catch,
                    TokenType.finally,
                    TokenType.endtry,
                    TokenType.exitwhile,
                    TokenType.exitfor,
                    TokenType.loop,
                    TokenType.endregion,
                ].map((t) => t.toString().toLowerCase())
            );
        }

        // Initialize allKeywordsLC from this.sslKeywordsEnumValues
        // This assumes t.toString().toLowerCase() correctly gives the keyword string (e.g., "procedure")
        this.allKeywordsLC = new Set(
            Array.from(this.sslKeywordsEnumValues).map((t) => t.toString().toLowerCase())
        );
    }

    private isStandaloneKeywordByString(keywordStringLC: string): boolean {
        // Modified to accept string
        return ColonSpacingRule.standaloneKeywordsLC.has(keywordStringLC);
    }

    apply(line: string, context: FormattingContext): string {
        const trimmedLine = line.trim();

        if (!trimmedLine || !trimmedLine.startsWith(":") || trimmedLine.length === 1) {
            return line;
        }

        // Robust leadingWhitespace calculation
        const firstCharIndex = line.search(/\\S/);
        const leadingWhitespace = firstCharIndex === -1 ? line : line.substring(0, firstCharIndex);

        let result = "";
        try {
            const tokenizer = new SSLTokenizer(trimmedLine);
            const tokens: Token[] = tokenizer
                .tokenize()
                .filter((t) => t.type !== TokenType.newline);

            if (tokens.length === 0) {
                return line;
            }

            let i = 0;
            while (i < tokens.length) {
                const currentToken = tokens[i];
                const nextToken = i + 1 < tokens.length ? tokens[i + 1] : null;

                if (currentToken.type === TokenType.colon) {
                    if (
                        result.length > 0 &&
                        !/\\s$/.test(result) &&
                        !result.endsWith("(") &&
                        !result.endsWith("[")
                    ) {
                        result += " ";
                    }

                    // Modified keyword detection logic
                    if (nextToken && nextToken.value) {
                        const nextTokenValueLC = nextToken.value.toLowerCase();
                        if (this.allKeywordsLC.has(nextTokenValueLC)) {
                            const keywordValue = nextToken.value; // Original case for uppercasing
                            result += `:${keywordValue.toUpperCase()}`;
                            i += 2; // Consumed colon and keyword token

                            // Use isStandaloneKeywordByString with the lowercase keyword string
                            if (!this.isStandaloneKeywordByString(nextTokenValueLC)) {
                                const tokenAfterKeyword = i < tokens.length ? tokens[i] : null;
                                if (
                                    tokenAfterKeyword &&
                                    tokenAfterKeyword.type !== TokenType.whitespace &&
                                    tokenAfterKeyword.type !== TokenType.semicolon
                                ) {
                                    result += " ";
                                }
                            }
                            continue;
                        }
                    }

                    // Fallback for original logic if new keyword detection doesn't match,
                    // or specific handling for ': whitespace'
                    if (nextToken && nextToken.type === TokenType.whitespace) {
                        // Handles malformed : keyword
                        result += currentToken.value;
                        result += nextToken.value;
                        i += 2;
                        continue;
                    } else {
                        // General colon handling (e.g., :=, or if keyword logic above failed)
                        result += currentToken.value;
                        i++;
                        continue;
                    }
                } else {
                    result += currentToken.value;
                    i++;
                }
            }
            return leadingWhitespace + result;
        } catch (error) {
            // console.error("Error in ColonSpacingRule for line:", line, error);
            return line;
        }
    }
}
