/**
 * SSL Formatting Provider - Document formatting for SSL (STARLIMS Scripting Language)
 * Based on the SSL EBNF grammar specification and modern parsing techniques
 */

import * as vscode from "vscode";
import { SSLTokenizer } from "./tokenizer"; // Changed from tokenizeSSL
import { SSLParser, ASTNode, ASTNodeType } from "./parser"; // Changed from parseTokens and added ASTNode, ASTNodeType

export interface FormattingOptions extends vscode.FormattingOptions {
    // Potentially add custom options here if needed beyond vscode.FormattingOptions
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
            new OperatorSpacingRule(),
            new CommaSpacingRule(),
            new BlockAlignmentRule(),
            new ColonSpacingRule(),
            new IndentationRule(), // Apply indentation last
        ];
    }

    public async provideDocumentFormattingEdits(
        // Added async
        document: vscode.TextDocument,
        options: FormattingOptions, // Use our extended options type
        token: vscode.CancellationToken
    ): Promise<vscode.TextEdit[]> {
        // Changed return type
        if (token.isCancellationRequested) {
            return [];
        }

        const fullText = document.getText();

        try {
            const text = document.getText();

            const formattingOptions: FormattingOptions = {
                tabSize: options.tabSize,
                insertSpaces: options.insertSpaces,
                maxLineLength: 90, // Default SSL line length
                indentStyle: options.insertSpaces ? "space" : "tab",
            };
            const formattedText = await this.formatText(text, formattingOptions, token); // Defensive check: ensure formattedText is a string
            if (typeof formattedText !== "string" || formattedText === undefined) {
                console.error(
                    "formatText returned invalid result:",
                    typeof formattedText,
                    formattedText
                );
                return []; // Return no edits if formatting failed
            }

            // Return a single edit that replaces the entire document
            const lastLineId = document.lineCount - 1;
            const lineAtLast = document.lineAt(lastLineId); // Get the TextLine object
            const lastLineLength = lineAtLast ? lineAtLast.text.length : 0; // Ensure lineAtLast is not undefined

            // console.log(
            //     `SSLFormattingProvider: lineCount=<span class="math-inline">\{document\.lineCount\}, lastLineId\=</span>{lastLineId}, lastLineLength=${lastLineLength}`
            // );

            const range = new vscode.Range(0, 0, lastLineId, lastLineLength);
            // console.log("SSLFormattingProvider: Created range:", JSON.stringify(range));
            const edit = vscode.TextEdit.replace(range, formattedText);
            return [edit];
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
        const startTime = Date.now();
        const MAX_PROCESSING_TIME = 5000; // 5 second timeout

        let ast: ASTNode;

        try {
            // Check cancellation before expensive operations
            if (token && token.isCancellationRequested) {
                return text;
            }

            // Tokenize the input
            const tokenizer = new SSLTokenizer(text); // Corrected usage
            const tokens = tokenizer.tokenize(); // Corrected usage

            // Check cancellation and timeout
            if (token && token.isCancellationRequested) {
                return text;
            }
            if (Date.now() - startTime > MAX_PROCESSING_TIME) {
                console.warn("SSL Formatting timeout during tokenization");
                return text;
            }

            // Parse into AST for structural understanding
            const parser = new SSLParser(tokens); // Corrected usage
            ast = parser.parse(); // Corrected usage

            if (Date.now() - startTime > MAX_PROCESSING_TIME) {
                console.warn("SSL Formatting timeout during parsing");
                return text;
            }
        } catch (error) {
            console.error("SSL Tokenizer/Parser error, proceeding without AST:", error);
            // Create a minimal AST to avoid crashes
            ast = { type: ASTNodeType.program, children: [] };
        }

        // Process line by line with context from AST
        const lines = text.split(/\r?\n/);
        const formattedLines: string[] = [];
        let currentIndentLevel = 0;
        let currentBlockType: string | null = null;

        // Safety limit for very large files
        const MAX_LINES = 10000;
        const actualLines = Math.min(lines.length, MAX_LINES);

        if (lines.length > MAX_LINES) {
            console.warn(
                `SSL Formatting limiting processing to ${MAX_LINES} lines (original: ${lines.length})`
            );
        }

        for (let i = 0; i < actualLines; i++) {
            // Check timeout every line for safety
            if (Date.now() - startTime > MAX_PROCESSING_TIME) {
                console.warn(`SSL Formatting timeout at line ${i + 1}/${lines.length}`);
                return text;
            }

            if (token && token.isCancellationRequested) {
                return text;
            }

            const line = lines[i];
            const trimmedLine = line.trim();

            // Skip empty lines (but preserve them)
            if (trimmedLine === "") {
                formattedLines.push("");
                continue;
            }

            // Analyze line context FIRST to determine proper indentation for current line
            const trimmedLowerLine = trimmedLine.toLowerCase();
            let lineIndentLevel = currentIndentLevel;

            // Block end keywords and intermediate keywords should be at the same level as their opening keyword
            if (this.isBlockEnd(trimmedLowerLine) || this.isIntermediateBlock(trimmedLowerLine)) {
                lineIndentLevel = Math.max(0, currentIndentLevel - 1);
            }

            // Create formatting context with correct indentation
            const context: FormattingContext = {
                indentLevel: lineIndentLevel,
                blockType: currentBlockType,
                previousLine: i > 0 ? lines[i - 1] : null,
                nextLine: i < lines.length - 1 ? lines[i + 1] : null,
                lineNumber: i + 1,
                options: options,
                ast: ast,
            }; // Apply formatting rules with safety limits
            let formattedLine = trimmedLine;

            for (const rule of this.rules) {
                try {
                    const result = rule.apply(formattedLine, context);
                    // Defensive check: ensure rule doesn't return undefined/null
                    if (result === undefined || result === null) {
                        console.error(
                            `SSL Formatting rule "${rule.name}" returned ${result} for input: "${formattedLine}"`
                        );
                        // Keep the previous value if rule returns undefined/null
                    } else if (typeof result !== "string") {
                        console.error(
                            `SSL Formatting rule "${
                                rule.name
                            }" returned non-string: ${typeof result} for input: "${formattedLine}"`
                        );
                        // Keep the previous value if rule returns non-string
                    } else {
                        formattedLine = result;
                    }
                } catch (error) {
                    console.error(`SSL Formatting rule "${rule.name}" threw error:`, error);
                    // Keep the previous value if rule throws error
                }
            }

            formattedLines.push(formattedLine);

            // Update context based on line content for NEXT iteration
            const lineContext = this.analyzeLineContext(
                trimmedLowerLine,
                ast,
                i,
                currentIndentLevel,
                currentBlockType
            );

            // Update current state for next iteration
            currentIndentLevel = lineContext.indentLevel;
            currentBlockType = lineContext.blockType;
        }
        const result = formattedLines.join("\n");

        // Defensive check: ensure we return a string
        if (typeof result !== "string") {
            console.error(`formatText returning non-string: ${typeof result}`, result);
            return text; // Return original text if something went wrong
        }

        return result;
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

        // Use the context's indentLevel directly (it's already calculated correctly)
        const indentLevel = context.indentLevel;

        const indentString = context.options.insertSpaces
            ? " ".repeat(context.options.tabSize * indentLevel)
            : "\t".repeat(indentLevel);

        return indentString + line.trim();
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
        const leadingWhitespaceMatch = text.match(/^(\s*)/);
        const leadingWhitespace = leadingWhitespaceMatch ? leadingWhitespaceMatch[0] : "";
        const contentText = text.substring(leadingWhitespace.length);

        // Early return if operator not found in content
        if (!contentText.includes(operator)) {
            return text; // Return original full text, which includes its original leading whitespace
        }

        // Don't modify operators inside strings or comments (check on contentText)
        if (this.isInsideStringOrComment(contentText, operator)) {
            return text; // Return original full text
        }

        // Escape special regex characters but keep the operator as a single unit
        const escapedOperator = operator.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
        const regex = new RegExp(`(\S)\s*${escapedOperator}\s*(\S)`, "g");

        // Replace with spaced operator on contentText
        let processedContent = contentText.replace(regex, `$1 ${operator} $2`);

        // Clean up extra spaces within the processedContent.
        // This line matches the user's restored file (applied to content): processedContent.replace(/\s{2,}/g, " ")
        processedContent = processedContent.replace(/\s{2,}/g, " ");

        return leadingWhitespace + processedContent;
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
                const keyword = keywordMatch[1]; // Get the rest of the line and preserve semicolons
                let rest = trimmedLine.substring(keywordMatch[0].length);

                // Check if rest ends with semicolon to preserve it properly
                const hasSemicolon = rest.endsWith(";");

                // If there's a semicolon, remove it temporarily for trimming
                if (hasSemicolon) {
                    rest = rest.substring(0, rest.length - 1).trim();
                    // Add semicolon back with no space before it
                    rest = rest ? rest + ";" : ";";
                } else {
                    rest = rest.trim();
                }

                // Get all whitespace up to the first non-whitespace character
                const leadingWhitespace = line.substring(0, line.length - line.trimStart().length);

                // Handle case with empty rest but with semicolon
                if (rest === ";") {
                    return `${leadingWhitespace}:${keyword.toUpperCase()};`;
                }

                // Use the existing indentation plus the keyword formatting with proper spacing
                return `${leadingWhitespace}:${keyword.toUpperCase()}${rest ? " " + rest : ""}`;
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
