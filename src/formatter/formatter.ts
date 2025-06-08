/**
 * SSL Code Formatter - Main entry point for formatting SSL code
 * Implements AST-based formatting using the Visitor Pattern
 */

import { Parser } from "../parser/parser";
import { tokenize } from "../tokenizer";
import { FormatterVisitor, FormatterOptions, DEFAULT_FORMATTER_OPTIONS } from "./formatterVisitor";
import { ASTNode } from "../parser/ast";

/**
 * Result of a formatting operation
 */
export interface FormatResult {
    /** Successfully formatted code */
    formattedCode: string;
    /** Whether the formatting was successful */
    success: boolean;
    /** Any errors encountered during formatting */
    errors: string[];
    /** Warnings about potential issues */
    warnings: string[];
}

/**
 * Main SSL Code Formatter class
 */
export class SSLFormatter {
    private options: FormatterOptions;

    constructor(options: Partial<FormatterOptions> = {}) {
        this.options = { ...DEFAULT_FORMATTER_OPTIONS, ...options };
    }

    /**
     * Format SSL source code
     */
    formatCode(sourceCode: string): FormatResult {
        const errors: string[] = [];
        const warnings: string[] = [];

        try {
            // Step 1: Tokenize the source code
            const tokens = tokenize(sourceCode);

            // Step 2: Parse tokens into AST
            const parser = new Parser(tokens);
            const parseResult = parser.parse();

            // Check for parse errors
            if (!parseResult.success || parseResult.errors.length > 0) {
                parseResult.errors.forEach((error) => {
                    errors.push(
                        `Parse error: ${error.message} at line ${error.line}, column ${error.column}`
                    );
                });

                // If there are fatal parse errors, we can't format
                if (!parseResult.success) {
                    return {
                        formattedCode: sourceCode, // Return original code
                        success: false,
                        errors,
                        warnings,
                    };
                }
            }

            // Step 3: Format using the FormatterVisitor
            const formatter = new FormatterVisitor(this.options);
            const formattedCode = formatter.format(parseResult.ast);

            // Step 4: Validate the result
            const validationResult = this.validateFormattedCode(formattedCode, sourceCode);
            warnings.push(...validationResult.warnings);

            return {
                formattedCode,
                success: true,
                errors,
                warnings,
            };
        } catch (error) {
            errors.push(
                `Formatting error: ${error instanceof Error ? error.message : String(error)}`
            );
            return {
                formattedCode: sourceCode, // Return original code on error
                success: false,
                errors,
                warnings,
            };
        }
    }

    /**
     * Format an AST directly (for testing and advanced usage)
     */
    formatAST(ast: ASTNode): string {
        const formatter = new FormatterVisitor(this.options);
        return formatter.format(ast);
    }

    /**
     * Update formatter options
     */
    updateOptions(newOptions: Partial<FormatterOptions>): void {
        this.options = { ...this.options, ...newOptions };
    }

    /**
     * Get current formatter options
     */
    getOptions(): FormatterOptions {
        return { ...this.options };
    }

    /**
     * Validate the formatted code for potential issues
     */
    private validateFormattedCode(
        formattedCode: string,
        originalCode: string
    ): { warnings: string[] } {
        const warnings: string[] = [];

        // Check if formatted code is significantly different in length
        const originalLines = originalCode.split("\n").length;
        const formattedLines = formattedCode.split("\n").length;
        const lineDifference = Math.abs(originalLines - formattedLines);

        if (lineDifference > originalLines * 0.5) {
            warnings.push(
                `Formatted code has significantly different line count: ${originalLines} -> ${formattedLines}`
            );
        }

        // Check if formatted code is empty when original wasn't
        if (originalCode.trim().length > 0 && formattedCode.trim().length === 0) {
            warnings.push("Formatted code is empty while original code was not");
        }

        // Check for potential syntax issues (basic validation)
        const unclosedBlocks = this.checkUnclosedBlocks(formattedCode);
        if (unclosedBlocks.length > 0) {
            warnings.push(...unclosedBlocks);
        }

        return { warnings };
    }

    /**
     * Basic validation for unclosed blocks in formatted code
     */
    private checkUnclosedBlocks(code: string): string[] {
        const warnings: string[] = [];
        const lines = code.split("\n");
        const blockStack: { keyword: string; line: number }[] = [];

        const openingKeywords = [
            ":PROCEDURE",
            ":IF",
            ":WHILE",
            ":FOR",
            ":BEGINCASE",
            ":TRY",
            ":REGION",
            ":BEGININLINECODE",
        ];
        const closingKeywords = [
            ":ENDPROC",
            ":ENDIF",
            ":ENDWHILE",
            ":NEXT",
            ":ENDCASE",
            ":ENDTRY",
            ":ENDREGION",
            ":ENDINLINECODE",
        ];

        const keywordPairs: { [key: string]: string } = {
            ":PROCEDURE": ":ENDPROC",
            ":IF": ":ENDIF",
            ":WHILE": ":ENDWHILE",
            ":FOR": ":NEXT",
            ":BEGINCASE": ":ENDCASE",
            ":TRY": ":ENDTRY",
            ":REGION": ":ENDREGION",
            ":BEGININLINECODE": ":ENDINLINECODE",
        };

        for (let i = 0; i < lines.length; i++) {
            const line = lines[i].trim().toUpperCase();

            // Check for opening keywords
            for (const keyword of openingKeywords) {
                if (line.startsWith(keyword)) {
                    blockStack.push({ keyword, line: i + 1 });
                    break;
                }
            }

            // Check for closing keywords
            for (const keyword of closingKeywords) {
                if (line.startsWith(keyword)) {
                    const expectedOpening = Object.keys(keywordPairs).find(
                        (k) => keywordPairs[k] === keyword
                    );
                    if (
                        blockStack.length > 0 &&
                        blockStack[blockStack.length - 1].keyword === expectedOpening
                    ) {
                        blockStack.pop();
                    } else {
                        warnings.push(`Unexpected closing keyword ${keyword} at line ${i + 1}`);
                    }
                    break;
                }
            }
        }

        // Check for unclosed blocks
        for (const block of blockStack) {
            warnings.push(`Unclosed block ${block.keyword} at line ${block.line}`);
        }

        return warnings;
    }

    /**
     * Test if the formatter produces idempotent results
     */
    testIdempotency(sourceCode: string): { isIdempotent: boolean; details: string } {
        const firstFormat = this.formatCode(sourceCode);
        if (!firstFormat.success) {
            return {
                isIdempotent: false,
                details: `First format failed: ${firstFormat.errors.join(", ")}`,
            };
        }

        const secondFormat = this.formatCode(firstFormat.formattedCode);
        if (!secondFormat.success) {
            return {
                isIdempotent: false,
                details: `Second format failed: ${secondFormat.errors.join(", ")}`,
            };
        }

        const isIdempotent = firstFormat.formattedCode === secondFormat.formattedCode;
        return {
            isIdempotent,
            details: isIdempotent
                ? "Formatter is idempotent"
                : "Formatter is not idempotent - formatting the same code twice produces different results",
        };
    }
}

/**
 * Convenience function to format SSL code with default options
 */
export function formatSSLCode(
    sourceCode: string,
    options?: Partial<FormatterOptions>
): FormatResult {
    const formatter = new SSLFormatter(options);
    return formatter.formatCode(sourceCode);
}

/**
 * Export all formatter types and constants
 */
export { FormatterOptions, DEFAULT_FORMATTER_OPTIONS } from "./formatterVisitor";
export { FormatterVisitor } from "./formatterVisitor";
