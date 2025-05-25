/**
 * SSL Formatting Provider - Document formatting for SSL (STARLIMS Scripting Language)
 * Based on the SSL EBNF grammar specification and modern parsing techniques
 */

import * as vscode from "vscode";
import { SSLTokenizer, Token, TokenType } from "./tokenizer";
import { SSLParser, ASTNode, ASTNodeType } from "./parser";

export interface FormattingOptions {
    tabSize: number;
    insertSpaces: boolean;
    maxLineLength: number;
    indentStyle: "space" | "tab";
}

export interface FormattingRule {
    name: string;
    description: string;
    apply(line: string, context: FormattingContext): string;
}

export interface FormattingContext {
    indentLevel: number;
    blockType: string | null;
    previousLine: string | null;
    nextLine: string | null;
    lineNumber: number;
    options: FormattingOptions;
    ast?: ASTNode;
}

/**
 * SSL Document Formatting Provider
 */
export class SSLFormattingProvider implements vscode.DocumentFormattingEditProvider {
    private rules: FormattingRule[];

    constructor() {
        this.rules = [
            new IndentationRule(),
            new OperatorSpacingRule(),
            new BlockAlignmentRule(),
            new ColonSpacingRule(),
            new CommaSpacingRule(),
        ];
    }

    public async provideDocumentFormattingEdits(
        document: vscode.TextDocument,
        options: vscode.FormattingOptions,
        token: vscode.CancellationToken
    ): Promise<vscode.TextEdit[]> {
        try {
            const text = document.getText();
            const formattingOptions: FormattingOptions = {
                tabSize: options.tabSize,
                insertSpaces: options.insertSpaces,
                maxLineLength: 90, // Default SSL line length
                indentStyle: options.insertSpaces ? "space" : "tab",
            };

            const formattedText = await this.formatText(text, formattingOptions, token);

            // Return a single edit that replaces the entire document
            const lastLineId = document.lineCount - 1;
            const lastLineLength = document.lineAt(lastLineId).text.length;
            const range = new vscode.Range(0, 0, lastLineId, lastLineLength);

            return [vscode.TextEdit.replace(range, formattedText)];
        } catch (error) {
            console.error("SSL Formatting error:", error);
            vscode.window.showErrorMessage(
                `SSL Formatting error: ${error instanceof Error ? error.message : String(error)}`
            );
            return [];
        }
    }

    private async formatText(
        text: string,
        options: FormattingOptions,
        token: vscode.CancellationToken
    ): Promise<string> {
        // Tokenize the input
        const tokenizer = new SSLTokenizer(text);
        const tokens = tokenizer.tokenize();

        // Parse into AST for structural understanding
        const parser = new SSLParser(tokens);
        const ast = parser.parse();

        // Process line by line with context from AST
        const lines = text.split(/\r?\n/);
        const formattedLines: string[] = [];
        let currentIndentLevel = 0;
        let currentBlockType: string | null = null;

        for (let i = 0; i < lines.length; i++) {
            if (token.isCancellationRequested) {
                throw new Error("Formatting cancelled");
            }

            const line = lines[i];
            const trimmedLine = line.trim();

            // Skip empty lines (but preserve them)
            if (trimmedLine === "") {
                formattedLines.push("");
                continue;
            } // Create formatting context BEFORE updating state
            const context: FormattingContext = {
                indentLevel: currentIndentLevel,
                blockType: currentBlockType,
                previousLine: i > 0 ? lines[i - 1] : null,
                nextLine: i < lines.length - 1 ? lines[i + 1] : null,
                lineNumber: i + 1,
                options: options,
                ast: ast,
            };

            // Apply formatting rules
            let formattedLine = trimmedLine;
            for (const rule of this.rules) {
                formattedLine = rule.apply(formattedLine, context);
            }

            formattedLines.push(formattedLine);

            // Update context based on line content AFTER formatting
            const lineContext = this.analyzeLineContext(
                trimmedLine,
                ast,
                i,
                currentIndentLevel,
                currentBlockType
            );

            // Update current state for next iteration
            currentIndentLevel = lineContext.indentLevel;
            currentBlockType = lineContext.blockType;
        }

        return formattedLines.join("\n");
    }

    private analyzeLineContext(
        line: string,
        ast: ASTNode,
        lineNumber: number,
        currentIndentLevel: number,
        currentBlockType: string | null
    ): { indentLevel: number; blockType: string | null } {
        const trimmedLine = line.trim().toLowerCase();

        // Handle block start keywords that increase indentation
        if (this.isBlockStart(trimmedLine)) {
            return {
                indentLevel: currentIndentLevel + 1,
                blockType: this.getBlockType(trimmedLine),
            };
        }

        // Handle block end keywords that decrease indentation
        if (this.isBlockEnd(trimmedLine)) {
            return {
                indentLevel: Math.max(0, currentIndentLevel - 1),
                blockType: null,
            };
        }

        // Handle intermediate keywords (like :ELSE, :CATCH) that stay at current level
        if (this.isIntermediateBlock(trimmedLine)) {
            return {
                indentLevel: currentIndentLevel,
                blockType: this.getBlockType(trimmedLine),
            };
        }

        // Default: maintain current context
        return {
            indentLevel: currentIndentLevel,
            blockType: currentBlockType,
        };
    }

    private isBlockStart(line: string): boolean {
        const blockStarts = [
            ":procedure",
            ":if",
            ":while",
            ":for",
            ":begincase",
            ":try",
            ":error",
            ":region",
            ":class",
            ":begininlinecode",
        ];
        return blockStarts.some((keyword) => line.startsWith(keyword));
    }

    private isBlockEnd(line: string): boolean {
        const blockEnds = [
            ":endproc",
            ":endif",
            ":endwhile",
            ":next",
            ":endcase",
            ":endtry",
            ":endregion",
            ":endinlinecode",
        ];
        return blockEnds.some((keyword) => line.startsWith(keyword));
    }

    private isIntermediateBlock(line: string): boolean {
        const intermediates = [":else", ":case", ":otherwise", ":catch", ":finally"];
        return intermediates.some((keyword) => line.startsWith(keyword));
    }
    private getBlockType(line: string): string {
        if (line.startsWith(":procedure")) {
            return "PROCEDURE";
        }
        if (line.startsWith(":if")) {
            return "IF";
        }
        if (line.startsWith(":else")) {
            return "ELSE";
        }
        if (line.startsWith(":while")) {
            return "WHILE";
        }
        if (line.startsWith(":for")) {
            return "FOR";
        }
        if (line.startsWith(":begincase")) {
            return "BEGINCASE";
        }
        if (line.startsWith(":case")) {
            return "CASE";
        }
        if (line.startsWith(":otherwise")) {
            return "OTHERWISE";
        }
        if (line.startsWith(":try")) {
            return "TRY";
        }
        if (line.startsWith(":catch")) {
            return "CATCH";
        }
        if (line.startsWith(":finally")) {
            return "FINALLY";
        }
        if (line.startsWith(":error")) {
            return "ERROR";
        }
        if (line.startsWith(":region")) {
            return "REGION";
        }
        if (line.startsWith(":class")) {
            return "CLASS";
        }
        return "UNKNOWN";
    }
}

/**
 * Handles consistent indentation for SSL blocks
 */
class IndentationRule implements FormattingRule {
    name = "Indentation";
    description = "Ensures consistent indentation for SSL blocks";

    apply(line: string, context: FormattingContext): string {
        // Don't indent empty lines or comments at start of line
        if (line.trim() === "" || line.trim().startsWith("/*")) {
            return line.trim();
        }

        const trimmedLine = line.trim().toLowerCase();
        let indentLevel = context.indentLevel;

        // Block end keywords and intermediate keywords should be at the same level as their opening keyword
        if (this.isBlockEnd(trimmedLine) || this.isIntermediateBlock(trimmedLine)) {
            indentLevel = Math.max(0, context.indentLevel - 1);
        }

        const indentString = context.options.insertSpaces
            ? " ".repeat(context.options.tabSize * indentLevel)
            : "\t".repeat(indentLevel);

        return indentString + line.trim();
    }

    private isBlockEnd(line: string): boolean {
        const blockEnds = [
            ":endproc",
            ":endif",
            ":endwhile",
            ":next",
            ":endcase",
            ":endtry",
            ":endregion",
            ":endinlinecode",
        ];
        return blockEnds.some((keyword) => line.startsWith(keyword));
    }
    private isIntermediateBlock(line: string): boolean {
        const intermediates = [":else", ":case", ":otherwise", ":catch", ":finally"];
        return intermediates.some((keyword) => line.startsWith(keyword));
    }
}

/**
 * Handles spacing around operators
 */
class OperatorSpacingRule implements FormattingRule {
    name = "Operator Spacing";
    description = "Ensures proper spacing around operators";

    apply(line: string, context: FormattingContext): string {
        let result = line;

        // Assignment operators
        result = this.addSpacingAroundOperator(result, ":=");
        result = this.addSpacingAroundOperator(result, "+=");
        result = this.addSpacingAroundOperator(result, "-=");
        result = this.addSpacingAroundOperator(result, "*=");
        result = this.addSpacingAroundOperator(result, "/=");
        result = this.addSpacingAroundOperator(result, "^=");

        // Comparison operators
        result = this.addSpacingAroundOperator(result, "==");
        result = this.addSpacingAroundOperator(result, "!=");
        result = this.addSpacingAroundOperator(result, "<=");
        result = this.addSpacingAroundOperator(result, ">=");
        result = this.addSpacingAroundOperator(result, "<");
        result = this.addSpacingAroundOperator(result, ">");

        // Arithmetic operators (but not when part of unary expressions)
        result = this.addSpacingAroundOperator(result, "+", true);
        result = this.addSpacingAroundOperator(result, "-", true);
        result = this.addSpacingAroundOperator(result, "*", true);
        result = this.addSpacingAroundOperator(result, "/", true);
        result = this.addSpacingAroundOperator(result, "%");
        result = this.addSpacingAroundOperator(result, "^", true);

        // Logical operators
        result = this.addSpacingAroundOperator(result, ".AND.");
        result = this.addSpacingAroundOperator(result, ".OR.");

        return result;
    }
    private addSpacingAroundOperator(text: string, operator: string, checkUnary = false): string {
        // Don't modify operators inside strings or comments
        if (this.isInsideStringOrComment(text, operator)) {
            return text;
        }

        // Escape special regex characters but keep the operator as a single unit
        const escapedOperator = operator.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
        const regex = new RegExp(`\\s*${escapedOperator}\\s*`, "g");
        return text.replace(regex, ` ${operator} `).replace(/\s+/g, " ");
    }
    private isInsideStringOrComment(text: string, operator: string): boolean {
        // Find all occurrences of the operator
        const operatorIndices: number[] = [];
        let index = text.indexOf(operator);
        while (index !== -1) {
            operatorIndices.push(index);
            index = text.indexOf(operator, index + 1);
        }

        if (operatorIndices.length === 0) {
            return false;
        }

        // Check if any occurrence is inside a string or comment
        for (const operatorIndex of operatorIndices) {
            // Check for comment context - if we find /* before the operator and no */ yet
            const beforeOperator = text.substring(0, operatorIndex);
            const afterOperator = text.substring(operatorIndex);

            // Check if we're inside a comment block
            const lastCommentStart = beforeOperator.lastIndexOf("/*");
            const lastCommentEnd = beforeOperator.lastIndexOf(";"); // SSL comments end with ;

            if (
                lastCommentStart !== -1 &&
                (lastCommentEnd === -1 || lastCommentStart > lastCommentEnd)
            ) {
                // We're inside a comment block
                return true;
            }

            // Check if we're inside a string
            const singleQuotes = (beforeOperator.match(/'/g) || []).length;
            const doubleQuotes = (beforeOperator.match(/"/g) || []).length;
            const brackets = (beforeOperator.match(/\[/g) || []).length;

            // Simple heuristic: if odd number of quotes, we're likely inside a string
            if (singleQuotes % 2 === 1 || doubleQuotes % 2 === 1 || brackets % 2 === 1) {
                return true;
            }
        }

        return false;
    }
}

/**
 * Handles alignment of multi-line statements
 */
class BlockAlignmentRule implements FormattingRule {
    name = "Block Alignment";
    description = "Aligns multi-line statements properly";

    apply(line: string, context: FormattingContext): string {
        // This is a simplified implementation
        // In a full implementation, this would handle complex multi-line alignments
        return line;
    }
}

/**
 * Handles spacing after colons in SSL keywords
 */
class ColonSpacingRule implements FormattingRule {
    name = "Colon Spacing";
    description = "Ensures proper spacing after colons in SSL keywords";

    apply(line: string, context: FormattingContext): string {
        // Handle SSL keywords that start with :
        const trimmedLine = line.trim();
        if (trimmedLine.startsWith(":")) {
            // Ensure single space after colon for keywords
            const keywordMatch = trimmedLine.match(/^:(\w+)\s*/);
            if (keywordMatch) {
                const keyword = keywordMatch[1];
                const rest = trimmedLine.substring(keywordMatch[0].length);
                const indentString = line.substring(0, line.indexOf(":"));
                return `${indentString}:${keyword.toUpperCase()}${rest ? " " + rest : ""}`;
            }
        }
        return line;
    }
}

/**
 * Handles spacing after commas
 */
class CommaSpacingRule implements FormattingRule {
    name = "Comma Spacing";
    description = "Ensures proper spacing after commas";

    apply(line: string, context: FormattingContext): string {
        // Add space after commas (but not inside strings)
        if (this.isInsideString(line)) {
            return line;
        }

        return line.replace(/,(?!\s)/g, ", ").replace(/,\s+/g, ", ");
    }

    private isInsideString(text: string): boolean {
        // Simple heuristic for string detection
        const singleQuotes = (text.match(/'/g) || []).length;
        const doubleQuotes = (text.match(/"/g) || []).length;
        const brackets = (text.match(/\[/g) || []).length;

        return singleQuotes % 2 === 1 || doubleQuotes % 2 === 1 || brackets % 2 === 1;
    }
}
