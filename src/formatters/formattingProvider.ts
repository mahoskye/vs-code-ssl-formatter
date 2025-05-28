/**
 * SSL Formatting Provider - Document formatting for SSL (STARLIMS Scripting Language)
 * Based on the SSL EBNF grammar specification and modern parsing techniques
 */

import * as vscode from "vscode";
import { SSLTokenizer } from "../core/tokenizer"; // Changed from tokenizeSSL
import { SSLParser, ASTNode, ASTNodeType } from "../core/parser"; // Changed from parseTokens and added ASTNode, ASTNodeType
import { CommentFormattingRule } from "./rules/commentFormattingRule";
import { IndentationRule } from "./rules/indentationRule";
import { OperatorSpacingRule } from "./rules/operatorSpacingRule";
import { BlockAlignmentRule } from "./rules/blockAlignmentRule";
import { ColonSpacingRule } from "./rules/colonSpacingRule";
import { CommaSpacingRule } from "./rules/commaSpacingRule";

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
    blockDepth: number;
    inMultiLineConstruct: boolean;
    constructType: string | null;
}

/**
 * SSL Document Formatting Provider
 */
export class SSLFormattingProvider implements vscode.DocumentFormattingEditProvider {
    private rules: FormattingRule[];
    constructor() {
        this.rules = [
            new CommentFormattingRule(), // Apply comment formatting first
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
            } // Analyze line context FIRST to determine proper indentation for current line
            const trimmedLowerLine = trimmedLine.toLowerCase();
            let lineIndentLevel = currentIndentLevel;

            // Block end keywords should be at the same level as their opening keyword
            if (this.isBlockEnd(trimmedLowerLine)) {
                lineIndentLevel = Math.max(0, currentIndentLevel - 1);
            }
            // Intermediate keywords (like :ELSE, :CATCH, :FINALLY) should be at one level less than current
            else if (
                this.isIntermediateBlock(trimmedLowerLine) &&
                !trimmedLowerLine.startsWith(":case")
            ) {
                lineIndentLevel = Math.max(0, currentIndentLevel - 1);
            }
            // :CASE statements should be at the current indentation level (not reduced)
            else if (trimmedLowerLine.startsWith(":case")) {
                lineIndentLevel = currentIndentLevel;
            } // Create formatting context with correct indentation
            const context: FormattingContext = {
                indentLevel: lineIndentLevel,
                blockType: currentBlockType,
                previousLine: i > 0 ? lines[i - 1] : null,
                nextLine: i < lines.length - 1 ? lines[i + 1] : null,
                lineNumber: i + 1,
                options: options,
                ast: ast,
                blockDepth: lineIndentLevel,
                inMultiLineConstruct: this.isMultiLineConstruct(line, i, lines),
                constructType: this.getConstructType(line, currentBlockType),
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
        } // Handle intermediate keywords (like :ELSE, :CATCH) that stay at current level
        if (this.isIntermediateBlock(trimmedLine)) {
            // :CASE is special - it should increase indentation for its content
            if (trimmedLine.startsWith(":case")) {
                return {
                    indentLevel: currentIndentLevel + 1,
                    blockType: this.getBlockType(trimmedLine),
                };
            }
            // Other intermediate blocks maintain current level
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

    private isMultiLineConstruct(line: string, lineIndex: number, lines: string[]): boolean {
        const trimmedLine = line.trim();

        // Check for multi-line array literals
        if (trimmedLine.includes("{") && !trimmedLine.includes("}")) {
            return true;
        }

        // Check for multi-line function calls (DoProc, ExecFunction)
        if (trimmedLine.match(/(DoProc|ExecFunction)\s*\(/) && !trimmedLine.includes(");")) {
            return true;
        }

        // Check for multi-line SQL statements
        if (trimmedLine.match(/(SqlExecute|LSearch)\s*\(/) && !trimmedLine.includes(");")) {
            return true;
        }

        // Look ahead to see if we're in a multi-line construct
        for (let i = lineIndex - 1; i >= 0; i--) {
            const prevLine = lines[i].trim();
            if (prevLine.includes("{") && !prevLine.includes("}")) {
                return true;
            }
            if (
                prevLine.match(/(DoProc|ExecFunction|SqlExecute|LSearch)\s*\(/) &&
                !prevLine.includes(");")
            ) {
                return true;
            }
            // Stop looking if we hit a complete statement
            if (prevLine.endsWith(";") || prevLine.endsWith("}")) {
                break;
            }
        }

        return false;
    }

    private getConstructType(line: string, currentBlockType: string | null): string | null {
        const trimmedLine = line.trim();

        // Array literal
        if (trimmedLine.includes("{") || trimmedLine.includes("}")) {
            return "array";
        }

        // Function calls
        if (trimmedLine.match(/(DoProc|ExecFunction)\s*\(/)) {
            return "function";
        }

        // SQL statements
        if (trimmedLine.match(/(SqlExecute|LSearch)\s*\(/)) {
            return "sql";
        }

        return currentBlockType;
    }
}
