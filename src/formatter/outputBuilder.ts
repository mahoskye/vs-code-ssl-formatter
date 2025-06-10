/**
 * Output Builder
 *
 * - Efficient string building with indentation management
 * - Methods for writing lines with proper indentation
 * - Blank line management
 */

import { FormatterOptions, defaultFormatterOptions } from "./options";

/**
 * Efficient output builder for SSL code formatting
 * Manages indentation, line breaks, and blank line consolidation
 */
export class OutputBuilder {
    private lines: string[] = [];
    private currentLine = "";
    private indentLevel = 0;
    private readonly options: FormatterOptions;
    private lastLineWasBlank = false;
    private pendingBlankLines = 0;

    constructor(options: FormatterOptions = defaultFormatterOptions) {
        this.options = options;
    }

    /**
     * Increase indentation level
     */
    indent(): void {
        this.indentLevel++;
    }

    /**
     * Decrease indentation level
     */
    dedent(): void {
        if (this.indentLevel > 0) {
            this.indentLevel--;
        }
    }

    /**
     * Get current indentation level
     */
    getIndentLevel(): number {
        return this.indentLevel;
    }

    /**
     * Set indentation level to specific value
     */
    setIndentLevel(level: number): void {
        this.indentLevel = Math.max(0, level);
    }

    /**
     * Get current indentation string
     */
    private getIndent(): string {
        if (this.options.useTabs) {
            return "\t".repeat(this.indentLevel);
        }
        return " ".repeat(this.indentLevel * this.options.indentSize);
    }

    /**
     * Write text to current line without indentation
     */
    write(text: string): void {
        this.flushPendingBlankLines();
        this.currentLine += text;
        this.lastLineWasBlank = false;
    }

    /**
     * Write text with proper indentation if at start of line
     */
    writeIndented(text: string): void {
        this.flushPendingBlankLines();
        if (this.currentLine === "") {
            this.currentLine = this.getIndent();
        }
        this.currentLine += text;
        this.lastLineWasBlank = false;
    }

    /**
     * Write text and end current line
     */
    writeLine(text?: string): void {
        this.flushPendingBlankLines();
        if (text !== undefined) {
            this.writeIndented(text);
        }
        this.commitCurrentLine();
        this.lastLineWasBlank = false;
    }

    /**
     * Write text with indentation and end line
     */
    writeLineIndented(text: string): void {
        this.flushPendingBlankLines();
        this.currentLine = this.getIndent() + text;
        this.commitCurrentLine();
        this.lastLineWasBlank = false;
    }

    /**
     * End current line without adding text
     */
    endLine(): void {
        this.flushPendingBlankLines();
        this.commitCurrentLine();
        this.lastLineWasBlank = false;
    }

    /**
     * Add a blank line (managed to prevent consecutive blank lines)
     */
    writeBlankLine(): void {
        if (!this.lastLineWasBlank && this.pendingBlankLines === 0) {
            this.pendingBlankLines = 1;
        }
    }
    /**
     * Add multiple blank lines (managed to prevent excessive blank lines)
     */
    writeBlankLines(count: number): void {
        if (!this.lastLineWasBlank) {
            this.pendingBlankLines = Math.max(this.pendingBlankLines, Math.min(count, 2)); // Limit to 2 blank lines max
        }
    }

    /**
     * Force a blank line even if the last line was blank
     */
    forceBlankLine(): void {
        this.flushPendingBlankLines();
        this.commitCurrentLine();
        this.lines.push("");
        this.lastLineWasBlank = true;
    }

    /**
     * Write a keyword with proper formatting (typically uppercase)
     */
    writeKeyword(keyword: string): void {
        this.writeIndented(keyword.toUpperCase());
    }

    /**
     * Write a keyword and end line
     */
    writeKeywordLine(keyword: string): void {
        this.writeLineIndented(keyword.toUpperCase());
    }
    /**
     * Write an operator with appropriate spacing
     */
    writeOperator(operator: string, spaceBefore = true, spaceAfter = true): void {
        if (spaceBefore && !this.currentLine.endsWith(" ")) {
            this.write(" ");
        }
        this.write(operator);
        if (spaceAfter) {
            this.write(" ");
        }
    }

    /**
     * Write a comma with appropriate spacing
     */
    writeComma(spaceAfter = true): void {
        this.write(",");
        if (spaceAfter && this.options.insertSpacesAfterCommas) {
            this.write(" ");
        }
    }

    /**
     * Write a semicolon (SSL statement terminator)
     */
    writeSemicolon(): void {
        this.write(";");
    }

    /**
     * Write opening parenthesis
     */
    writeOpenParen(): void {
        this.write("(");
    }

    /**
     * Write closing parenthesis
     */
    writeCloseParen(): void {
        this.write(")");
    }

    /**
     * Write opening bracket
     */
    writeOpenBracket(): void {
        this.write("[");
    }

    /**
     * Write closing bracket
     */
    writeCloseBracket(): void {
        this.write("]");
    }

    /**
     * Write opening brace
     */
    writeOpenBrace(): void {
        this.write("{");
    }

    /**
     * Write closing brace
     */
    writeCloseBrace(): void {
        this.write("}");
    }

    /**
     * Write a space if conditions are met
     */
    writeSpaceIf(condition: boolean): void {
        if (condition) {
            this.write(" ");
        }
    }

    /**
     * Write space if not already present at end of current line
     */
    ensureSpace(): void {
        if (this.currentLine.length > 0 && !this.currentLine.endsWith(" ")) {
            this.write(" ");
        }
    }

    /**
     * Check if current line is empty
     */
    isCurrentLineEmpty(): boolean {
        return this.currentLine.trim() === "";
    }

    /**
     * Check if we're at the start of a line
     */
    isAtStartOfLine(): boolean {
        return this.currentLine === "" || this.currentLine === this.getIndent();
    }

    /**
     * Get current line content
     */
    getCurrentLine(): string {
        return this.currentLine;
    }

    /**
     * Get current output length for line wrapping decisions
     */
    getCurrentLineLength(): number {
        return this.currentLine.length;
    }

    /**
     * Check if adding text would exceed max line length
     */
    wouldExceedMaxLength(additionalText: string): boolean {
        return this.getCurrentLineLength() + additionalText.length > this.options.maxLineLength;
    }

    /**
     * Check if current line exceeds max length
     */
    isCurrentLineTooLong(): boolean {
        return this.getCurrentLineLength() > this.options.maxLineLength;
    }

    /**
     * Clear current line content
     */
    clearCurrentLine(): void {
        this.currentLine = "";
    }

    /**
     * Get total number of lines written
     */
    getLineCount(): number {
        let totalLines = this.lines.length;
        if (this.currentLine.trim() !== "") {
            totalLines++;
        }
        return totalLines;
    }

    /**
     * Check if output is empty
     */
    isEmpty(): boolean {
        return this.lines.length === 0 && this.currentLine.trim() === "";
    }

    /**
     * Commit current line to lines array
     */
    private commitCurrentLine(): void {
        this.lines.push(this.currentLine);
        this.currentLine = "";
    }

    /**
     * Flush any pending blank lines
     */
    private flushPendingBlankLines(): void {
        for (let i = 0; i < this.pendingBlankLines; i++) {
            if (this.currentLine.trim() !== "") {
                this.commitCurrentLine();
            }
            this.lines.push("");
            this.lastLineWasBlank = true;
        }
        this.pendingBlankLines = 0;
    }
    /**
     * Get the final formatted output
     */
    getOutput(): string {
        // Flush any pending blank lines
        this.flushPendingBlankLines();

        // Add any remaining current line
        if (this.currentLine.trim() !== "") {
            this.lines.push(this.currentLine);
        }

        let result = this.lines.join("\n");

        // Trim trailing whitespace if enabled
        if (this.options.trimTrailingWhitespace) {
            result = result.replace(/[ \t]+$/gm, "");
        }

        // Remove excessive blank lines (more than 2 consecutive)
        result = result.replace(/\n\n\n\n+/g, "\n\n\n");

        // Insert final newline if enabled, but only if there's actual content or lines
        if (
            this.options.insertFinalNewline &&
            (result.length > 0 || this.lines.length > 0) &&
            !result.endsWith("\n")
        ) {
            result += "\n";
        }

        return result;
    }

    /**
     * Reset the output builder to initial state
     */
    reset(): void {
        this.lines = [];
        this.currentLine = "";
        this.indentLevel = 0;
        this.lastLineWasBlank = false;
        this.pendingBlankLines = 0;
    }

    /**
     * Create a snapshot of current state for restoration
     */
    createSnapshot(): OutputBuilderSnapshot {
        return {
            lines: [...this.lines],
            currentLine: this.currentLine,
            indentLevel: this.indentLevel,
            lastLineWasBlank: this.lastLineWasBlank,
            pendingBlankLines: this.pendingBlankLines,
        };
    }

    /**
     * Restore state from a snapshot
     */
    restoreSnapshot(snapshot: OutputBuilderSnapshot): void {
        this.lines = [...snapshot.lines];
        this.currentLine = snapshot.currentLine;
        this.indentLevel = snapshot.indentLevel;
        this.lastLineWasBlank = snapshot.lastLineWasBlank;
        this.pendingBlankLines = snapshot.pendingBlankLines;
    }
}

/**
 * Snapshot of OutputBuilder state for restoration
 */
export interface OutputBuilderSnapshot {
    lines: string[];
    currentLine: string;
    indentLevel: number;
    lastLineWasBlank: boolean;
    pendingBlankLines: number;
}
