import { Token } from "./token";
import { TokenType } from "./tokenType";
import { Lexer, LexerOptions } from "./lexer";
import { TokenizerError } from "./errors";

/**
 * Configuration options for the tokenizer
 */
export interface TokenizerOptions extends LexerOptions {
    // Additional tokenizer-specific options can be added here
    skipErrors?: boolean; // Whether to skip errors and continue tokenizing
    maxErrors?: number; // Maximum number of errors before stopping
}

/**
 * Result of tokenization
 */
export interface TokenizationResult {
    tokens: Token[];
    errors: TokenizerError[];
    hasErrors: boolean;
}

/**
 * Main tokenizer class for SSL language
 * This is the high-level interface for tokenizing SSL source code
 */
export class Tokenizer {
    private options: TokenizerOptions;

    constructor(options: TokenizerOptions = {}) {
        this.options = {
            includeWhitespace: false,
            includeNewlines: true,
            includeComments: true,
            skipErrors: true,
            maxErrors: 100,
            ...options,
        };
    }

    /**
     * Tokenizes SSL source code
     * @param input The SSL source code to tokenize
     * @returns TokenizationResult containing tokens and any errors
     */
    public tokenize(input: string): TokenizationResult {
        const lexer = new Lexer(input, this.options);
        const errors: TokenizerError[] = [];
        let tokens: Token[] = [];

        try {
            tokens = lexer.tokenize();

            // Collect errors from error tokens
            tokens.forEach((token) => {
                if (token.error) {
                    errors.push(
                        new TokenizerError(token.error, token.range.start, "TOKENIZER_ERROR")
                    );
                }
            });

            // Remove error tokens if skipErrors is true
            if (this.options.skipErrors) {
                tokens = tokens.filter((token) => !token.error);
            }
        } catch (error) {
            if (error instanceof TokenizerError) {
                errors.push(error);
                if (!this.options.skipErrors) {
                    throw error;
                }
            } else {
                throw error;
            }
        }

        return {
            tokens,
            errors,
            hasErrors: errors.length > 0,
        };
    }

    /**
     * Tokenizes SSL source code and returns only the tokens
     * Throws an error if tokenization fails
     * @param input The SSL source code to tokenize
     * @returns Array of tokens
     */
    public tokenizeToArray(input: string): Token[] {
        const result = this.tokenize(input);

        if (result.hasErrors && !this.options.skipErrors) {
            throw result.errors[0];
        }

        return result.tokens;
    }

    /**
     * Validates SSL source code syntax by attempting to tokenize it
     * @param input The SSL source code to validate
     * @returns Array of tokenization errors (empty if valid)
     */
    public validate(input: string): TokenizerError[] {
        const originalSkipErrors = this.options.skipErrors;
        this.options.skipErrors = true;

        try {
            const result = this.tokenize(input);
            return result.errors;
        } finally {
            this.options.skipErrors = originalSkipErrors;
        }
    }

    /**
     * Gets token statistics for the given input
     * @param input The SSL source code to analyze
     * @returns Statistics about the tokens
     */
    public getTokenStatistics(input: string): TokenStatistics {
        const result = this.tokenize(input);
        const stats = new TokenStatistics();

        result.tokens.forEach((token) => {
            stats.total++;
            stats.byType.set(token.type, (stats.byType.get(token.type) || 0) + 1);

            if (token.isKeyword) {
                stats.keywords++;
            }
            if (token.isOperator) {
                stats.operators++;
            }
            if (token.isLiteral) {
                stats.literals++;
            }
            if (token.isPunctuation) {
                stats.punctuation++;
            }
            if (token.type === TokenType.IDENTIFIER) {
                stats.identifiers++;
            }
            if (
                token.type === TokenType.BLOCK_COMMENT ||
                token.type === TokenType.SINGLE_LINE_COMMENT ||
                token.type === TokenType.REGION_COMMENT ||
                token.type === TokenType.ENDREGION_COMMENT
            ) {
                stats.comments++;
            }
        });

        return stats;
    }
}

/**
 * Token statistics for analysis
 */
export class TokenStatistics {
    public total: number = 0;
    public keywords: number = 0;
    public operators: number = 0;
    public literals: number = 0;
    public punctuation: number = 0;
    public identifiers: number = 0;
    public comments: number = 0;
    public byType: Map<TokenType, number> = new Map();

    /**
     * Gets the percentage of tokens of a specific type
     */
    public getPercentage(type: TokenType): number {
        const count = this.byType.get(type) || 0;
        return this.total > 0 ? (count / this.total) * 100 : 0;
    }

    /**
     * Gets the most common token type
     */
    public getMostCommonType(): TokenType | null {
        let maxCount = 0;
        let mostCommon: TokenType | null = null;

        this.byType.forEach((count, type) => {
            if (count > maxCount) {
                maxCount = count;
                mostCommon = type;
            }
        });

        return mostCommon;
    }
}

/**
 * Convenience function to quickly tokenize SSL source code
 * @param input The SSL source code to tokenize
 * @param options Optional tokenizer options
 * @returns Array of tokens
 */
export function tokenize(input: string, options?: TokenizerOptions): Token[] {
    const tokenizer = new Tokenizer(options);
    return tokenizer.tokenizeToArray(input);
}

/**
 * Convenience function to validate SSL source code
 * @param input The SSL source code to validate
 * @returns Array of errors (empty if valid)
 */
export function validateSyntax(input: string): TokenizerError[] {
    const tokenizer = new Tokenizer({ skipErrors: true });
    return tokenizer.validate(input);
}
