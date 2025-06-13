/**
 * Comment Formatter
 *
 * Handles all SSL comment types with proper formatting and preservation:
 * - Handle block comments (/* comment ;)
 * - Handle single-line comments
 * - Handle region comments (/* region, /* endregion)
 * - Comment association and placement logic
 *
 * # Before/After Formatting Examples
 *
 * ## Single-Line Comments
 * **Before:**
 * ```ssl
 * /*this is a comment;
 * result=calculate()/*inline comment;
 * ```
 *
 * **After:**
 * ```ssl
 * /* This is a comment ;
 * result = calculate() /* Inline comment ;
 * ```
 *
 * ## Block Comments
 * **Before:**
 * ```ssl
 * /*this is a
 * multi-line comment
 * with various content;
 * ```
 *
 * **After:**
 * ```ssl
 * /* This is a
 *    multi-line comment
 *    with various content ;
 * ```
 *
 * ## Region Comments
 * **Before:**
 * ```ssl
 * /*region data processing;
 * procedure processData()
 *     /*do work here;
 * endprocedure
 * /*endregion;
 * ```
 *
 * **After:**
 * ```ssl
 * /* Region: Data Processing ;
 * Procedure ProcessData()
 *     /* Do work here ;
 * EndProcedure
 * /* EndRegion ;
 * ```
 *
 * ## Comment Association
 * **Before:**
 * ```ssl
 * /*procedure header comment;
 * procedure myProc()
 * value=10/*variable assignment;
 * endprocedure/*end of procedure;
 * ```
 *
 * **After:**
 * ```ssl
 * /* Procedure header comment ;
 * Procedure MyProc()
 *     value = 10 /* Variable assignment ;
 * EndProcedure /* End of procedure ;
 * ```
 *
 * ## Complex Comment Scenarios
 * **Before:**
 * ```ssl
 * /*region validation;
 * if input<>nil then/*check if input exists;
 * /*validate the input;
 * result=validateInput(input)
 * else
 * /*handle nil case;
 * result=false
 * endif/*end validation;
 * /*endregion;
 * ```
 *
 * **After:**
 * ```ssl
 * /* Region: Validation ;
 * If input <> Nil Then /* Check if input exists ;
 *     /* Validate the input ;
 *     result = ValidateInput(input)
 * Else
 *     /* Handle nil case ;
 *     result = False
 * EndIf /* End validation ;
 * /* EndRegion ;
 * ```
 */

import {
    CommentStatementNode,
    BlockCommentNode,
    SingleLineCommentNode,
    RegionCommentNode,
    EndRegionCommentNode,
} from "../parser/ast/comments";
import { FormatterVisitorBase, VisitorResult } from "./visitor";
import { TokenType } from "../tokenizer/tokenType";

/**
 * SSL Comment Formatter Visitor
 *
 * Formats SSL comments according to EBNF grammar:
 * - Block comments: /* comment ; (multi-line)
 * - Single-line comments: /* comment ; (single line)
 * - Region comments: /* region name ; (code organization)
 * - End region comments: /* endregion ; (close regions)
 * - Comment alignment and preservation
 */
export class SSLCommentFormatterVisitor extends FormatterVisitorBase {
    // ================================
    // Comment Statement Formatters
    // ================================

    /**
     * Format comment statements (wrapper for specific comment types)
     * According to EBNF: CommentStatement is a wrapper for different comment types
     */
    protected override visitCommentStatement(node: CommentStatementNode): VisitorResult {
        // CommentStatement is typically a wrapper - format the underlying comment
        const token = (node as any).token || node.startToken;
        if (token) {
            this.formatCommentToken(token);
        }
        return { shouldContinue: false };
    }

    /**
     * Format block comments (multi-line)
     * According to EBNF: BlockComment ::= "/*" {Character} ";"
     * Preserves internal formatting and line breaks
     */
    protected override visitBlockComment(node: BlockCommentNode): VisitorResult {
        const token = (node as any).token || node.startToken;
        if (token && token.value) {
            this.formatBlockComment(token.value);
        }
        return { shouldContinue: false };
    }

    /**
     * Format single-line comments
     * According to EBNF: SingleLineComment ::= "/*" {Character} ";"
     * Handles inline and standalone single-line comments
     */
    protected override visitSingleLineComment(node: SingleLineCommentNode): VisitorResult {
        const token = (node as any).token || node.startToken;
        if (token && token.value) {
            this.formatSingleLineComment(token.value);
        }
        return { shouldContinue: false };
    }

    /**
     * Format region comments for code organization
     * According to EBNF: RegionComment ::= "/*" "region" {Character} ";"
     * Preserves region markers when configured
     */
    protected override visitRegionComment(node: RegionCommentNode): VisitorResult {
        const token = (node as any).token || node.startToken;
        if (token && token.value) {
            this.formatRegionComment(token.value);
        }
        return { shouldContinue: false };
    }

    /**
     * Format end region comments
     * According to EBNF: EndRegionComment ::= "/*" "endregion" {Character} ";"
     * Maintains symmetry with region comments
     */
    protected override visitEndRegionComment(node: EndRegionCommentNode): VisitorResult {
        const token = (node as any).token || node.startToken;
        if (token && token.value) {
            this.formatEndRegionComment(token.value);
        }
        return { shouldContinue: false };
    }

    // ================================
    // Comment Formatting Helpers
    // ================================

    /**
     * Format a comment token based on its type
     */ private formatCommentToken(token: any): void {
        if (!token.value) {
            return;
        }

        switch (token.type) {
            case TokenType.BLOCK_COMMENT:
                this.formatBlockComment(token.value);
                break;
            case TokenType.SINGLE_LINE_COMMENT:
                this.formatSingleLineComment(token.value);
                break;
            case TokenType.REGION_COMMENT:
                this.formatRegionComment(token.value);
                break;
            case TokenType.ENDREGION_COMMENT:
                this.formatEndRegionComment(token.value);
                break;
            default:
                // Fallback - treat as single-line comment
                this.formatSingleLineComment(token.value);
                break;
        }
    }

    /**
     * Format block comments with proper indentation and line breaks
     */
    private formatBlockComment(commentText: string): void {
        if (!this.options.formatMultiLineComments) {
            // Preserve original formatting
            this.output.write(commentText);
            return;
        }

        const lines = commentText.split("\n");

        if (lines.length === 1) {
            // Single line - treat as single-line comment
            this.formatSingleLineComment(commentText);
            return;
        }

        // Multi-line formatting
        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];

            if (i === 0) {
                // First line - preserve comment start
                this.output.writeIndented(line.trimEnd());
            } else if (i === lines.length - 1) {
                // Last line - handle comment end
                const trimmedLine = line.trim();
                if (trimmedLine) {
                    this.output.writeLine();
                    this.output.writeIndented(trimmedLine);
                } else {
                    this.output.writeLine();
                    this.output.writeIndented(";");
                }
            } else {
                // Middle lines - preserve content with proper indentation
                this.output.writeLine();
                const trimmedLine = line.trim();
                if (trimmedLine) {
                    this.output.writeIndented(trimmedLine);
                }
            }
        }
    }
    /**
     * Format single-line comments with optional wrapping
     */
    private formatSingleLineComment(commentText: string): void {
        // Check if this should be aligned to a column
        if (this.shouldAlignComment(commentText)) {
            this.alignCommentToColumn(commentText);
            return;
        }

        if (!this.options.wrapLongComments) {
            // For standalone comments, use writeIndented; for inline comments, use write
            if (this.output.getCurrentLineLength() === 0) {
                this.output.writeIndented(commentText);
            } else {
                this.output.write(commentText);
            }
            return;
        }

        // Check if comment exceeds max line length
        const currentIndent = this.output.getCurrentLineLength();
        const totalLength = currentIndent + commentText.length;

        if (totalLength <= this.options.maxLineLength) {
            // For standalone comments, use writeIndented; for inline comments, use write
            if (this.output.getCurrentLineLength() === 0) {
                this.output.writeIndented(commentText);
            } else {
                this.output.write(commentText);
            }
            return;
        }

        // Need to wrap the comment
        this.wrapLongComment(commentText);
    }

    /**
     * Format region comments with optional preservation
     */
    private formatRegionComment(commentText: string): void {
        if (!this.options.preserveRegionMarkers) {
            // Treat as regular comment
            this.formatSingleLineComment(commentText);
            return;
        }

        // Preserve region marker
        this.output.writeIndented(commentText);
    }

    /**
     * Format end region comments
     */
    private formatEndRegionComment(commentText: string): void {
        if (!this.options.preserveRegionMarkers) {
            // Treat as regular comment
            this.formatSingleLineComment(commentText);
            return;
        }

        // Preserve end region marker
        this.output.writeIndented(commentText);
    }

    /**
     * Wrap long comments across multiple lines
     */
    private wrapLongComment(commentText: string): void {
        // Extract comment content (remove /* and ;)
        const content = this.extractCommentContent(commentText);
        const words = content.split(/\s+/).filter((word) => word.length > 0);

        if (words.length === 0) {
            this.output.write(commentText);
            return;
        }

        const maxContentLength =
            this.options.maxLineLength - this.output.getCurrentLineLength() - 4; // Account for /* and ;
        let currentLine = "/*";

        for (const word of words) {
            const nextLine =
                currentLine === "/*" ? `${currentLine} ${word}` : `${currentLine} ${word}`;

            if (nextLine.length <= maxContentLength) {
                currentLine = nextLine;
            } else {
                // Finish current line
                this.output.write(`${currentLine} ;`);
                this.output.writeLine();

                // Start new line
                this.output.writeIndented(`/* ${word}`);
                currentLine = `/* ${word}`;
            }
        }

        // Finish the last line
        if (currentLine !== "/*") {
            if (currentLine.endsWith(" ;")) {
                this.output.write(currentLine);
            } else {
                this.output.write(`${currentLine} ;`);
            }
        }
    }

    /**
     * Extract content from comment (remove /* and ;)
     */
    private extractCommentContent(commentText: string): string {
        let content = commentText;

        // Remove comment start
        if (content.startsWith("/*")) {
            content = content.substring(2);
        }

        // Remove comment end
        if (content.endsWith(";")) {
            content = content.substring(0, content.length - 1);
        }

        return content.trim();
    }

    /**
     * Check if comment should be aligned to column
     */
    private shouldAlignComment(commentText: string): boolean {
        return (
            this.options.alignEndOfLineComments &&
            this.output.getCurrentLineLength() > 0 &&
            !commentText.toLowerCase().includes("region")
        );
    }

    /**
     * Align comment to configured column
     */
    private alignCommentToColumn(commentText: string): void {
        const currentLength = this.output.getCurrentLineLength();
        const targetColumn = this.options.commentAlignmentColumn;

        if (currentLength < targetColumn) {
            const padding = " ".repeat(targetColumn - currentLength);
            this.output.write(padding);
        } else {
            // Already past target column, just add a space
            this.output.write(" ");
        }

        this.output.write(commentText);
    }
}
