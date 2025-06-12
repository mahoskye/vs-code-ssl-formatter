/**
 * Literal Formatters
 *
 * - Format string literals (preserve quotes)
 * - Format number literals (including scientific notation)
 * - Format boolean literals (.T., .F.)
 * - Format array literals with proper spacing
 * - Format date literals
 * - Format NIL literals
 * - Format code block literals
 */

import {
    LiteralExpressionNode,
    NumberLiteralNode,
    StringLiteralNode,
    BooleanLiteralNode,
    ArrayLiteralNode,
    NilLiteralNode,
    DateLiteralNode,
    CodeBlockLiteralNode,
} from "../parser/ast";
import { FormatterVisitorBase, VisitorResult } from "./visitor";
import { TokenType } from "../tokenizer/tokenType";

/**
 * SSL Literal Formatter Visitor
 *
 * Formats SSL literals according to EBNF grammar:
 * - String literals: preserve original quotes (single or double)
 * - Number literals: support scientific notation (1.23e5, 4.56E-3)
 * - Boolean literals: canonical .T. and .F. format
 * - Array literals: proper spacing and line breaking for long arrays
 * - Date literals: {year, month, day[, hour, minute, second]} format
 * - NIL literals: uppercase NIL
 * - Code block literals: {|params| expressions} format
 */
export class SSLLiteralFormatterVisitor extends FormatterVisitorBase {
    // ================================
    // Literal Expression Formatters
    // ================================

    /**
     * Format literal expressions (wrapper for specific literals)
     * According to EBNF: LiteralExpressionNode wraps literal values for expression context
     */
    protected override visitLiteralExpression(node: LiteralExpressionNode): VisitorResult {
        const token = (node as any).token;
        if (token) {
            // Use the original token value to preserve formatting
            this.output.write(token.value);
        } else {
            // Fallback to string representation
            const value = (node as any).value;
            this.output.write(String(value || ""));
        }
        return { shouldContinue: false };
    }

    /**
     * Format number literals with proper scientific notation support
     * According to EBNF: NumberLiteral ::= IntegerPart ( DecimalPart Exponent? )?
     * Supports: 42, 42.5, 1.23e5, 4.56E-3, 0.5e1
     * Note: SSL requires decimal point before 'e' for scientific notation
     */
    protected override visitNumberLiteral(node: NumberLiteralNode): VisitorResult {
        const raw = (node as any).raw;
        if (raw) {
            // Preserve the original formatting from source
            this.output.write(raw);
        } else {
            // Generate proper format from value
            const value = (node as any).value || 0;
            this.output.write(String(value));
        }
        return { shouldContinue: false };
    }

    /**
     * Format string literals preserving original quotes
     * According to EBNF: StringLiteral ::= '"' {Character} '"' | "'" {Character} "'"
     * SSL supports both single and double quotes - preserve the original choice
     */
    protected override visitStringLiteral(node: StringLiteralNode): VisitorResult {
        const token = (node as any).token;
        if (token) {
            // Preserve original quote style and content
            this.output.write(token.value);
        } else {
            // Default to double quotes if no token available
            const value = (node as any).value || "";
            // Escape any internal double quotes
            const escaped = value.replace(/"/g, '\\"');
            this.output.write(`"${escaped}"`);
        }
        return { shouldContinue: false };
    }

    /**
     * Format boolean literals in canonical SSL format
     * According to EBNF: BooleanLiteral ::= ".T." | ".F."
     * Always use the canonical SSL boolean format regardless of input
     */
    protected override visitBooleanLiteral(node: BooleanLiteralNode): VisitorResult {
        const value = (node as any).value;
        this.output.write(value ? ".T." : ".F.");
        return { shouldContinue: false };
    }

    /**
     * Format NIL literal in uppercase
     * According to EBNF: NilLiteral ::= "NIL"
     * Always format as uppercase NIL
     */
    protected override visitNilLiteral(node: NilLiteralNode): VisitorResult {
        this.output.write("NIL");
        return { shouldContinue: false };
    }

    /**
     * Format array literals with proper spacing and line breaking
     * According to EBNF: ArrayLiteral ::= "{" [ExpressionList] "}" | "{" ArrayLiteral {"," ArrayLiteral} "}"
     * Supports nested arrays and proper formatting based on length and complexity
     */
    protected override visitArrayLiteral(node: ArrayLiteralNode): VisitorResult {
        const elements = (node as any).elements || [];

        this.output.write("{");

        // Determine if we should break long arrays across lines
        const shouldBreakLongArray =
            this.options.breakLongArrayLiterals &&
            elements.length > this.options.parameterListBreakThreshold;
        if (shouldBreakLongArray) {
            this.output.writeLine();
            this.output.indent();

            // For multi-line arrays, put each element on its own line without commas
            for (let i = 0; i < elements.length; i++) {
                this.output.writeIndented("");
                this.visit(elements[i]);
                this.output.writeLine();
            }
        } else {
            // For single-line arrays, use comma separation
            for (let i = 0; i < elements.length; i++) {
                this.visit(elements[i]);

                if (i < elements.length - 1) {
                    this.output.write(",");
                    if (this.options.insertSpacesAfterCommas) {
                        this.output.write(" ");
                    }
                }
            }
        }
        if (shouldBreakLongArray) {
            this.output.dedent();
            this.output.writeIndented("}");
        } else {
            this.output.write("}");
        }

        return { shouldContinue: false };
    }

    /**
     * Format date literals with proper component spacing
     * According to EBNF: DateLiteral ::= "{" NumberLiteral "," NumberLiteral "," NumberLiteral ["," NumberLiteral "," NumberLiteral "," NumberLiteral] "}"
     * Format: {year, month, day[, hour, minute, second]}
     * Common usage: {2023, 12, 25} or {2023, 12, 25, 14, 30, 45}
     */
    protected override visitDateLiteral(node: DateLiteralNode): VisitorResult {
        const components = (node as any).components || [];

        this.output.write("{");
        for (let i = 0; i < components.length; i++) {
            if (i > 0) {
                this.output.write(",");
                if (this.options.insertSpacesAfterCommas) {
                    this.output.write(" ");
                }
            }
            // Visit each date component (should be NumberLiteralNode)
            this.visit(components[i]);
        }
        this.output.write("}");

        return { shouldContinue: false };
    }
    /**
     * Format code block literals with proper parameter and body formatting
     * According to EBNF: CodeBlockLiteral ::= "{|" [IdentifierList] "|" ExpressionList "}"
     * Examples: {|x| x * x}, {|a, b| a + b}, {|| defaultValue}
     */
    protected override visitCodeBlockLiteral(node: CodeBlockLiteralNode): VisitorResult {
        const parameters = (node as any).parameters;
        const body = (node as any).body || [];

        this.output.write("{|");

        // Format parameters if present
        const hasParameters =
            parameters && parameters.identifiers && parameters.identifiers.length > 0;
        if (hasParameters) {
            const identifiers = parameters.identifiers;
            for (let i = 0; i < identifiers.length; i++) {
                if (i > 0) {
                    this.output.write(",");
                    if (this.options.insertSpacesAfterCommas) {
                        this.output.write(" ");
                    }
                }
                this.output.write(identifiers[i].value || identifiers[i].text || "");
            }
            this.output.write("|");
        } // Format body expressions
        if (Array.isArray(body)) {
            for (let i = 0; i < body.length; i++) {
                this.visit(body[i]);
            }
        } else if (body) {
            this.visit(body);
        }

        this.output.write("}");

        return { shouldContinue: false };
    }
}
