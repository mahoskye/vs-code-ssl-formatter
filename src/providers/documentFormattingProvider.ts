/**
 * SSL Document Formatting Provider
 *
 * Provides document formatting functionality for SSL files.
 * Integrates with the SSL formatter to provide format document and format selection commands.
 */

import * as vscode from "vscode";
import { SSLFormatter } from "../formatter";
import { FormatterOptions, createFormatterOptionsFromConfig } from "../formatter/options";
import { Parser } from "../parser";
import { Tokenizer } from "../tokenizer";

/**
 * SSL Document Formatting Provider
 * Implements VS Code's DocumentFormattingEditProvider and DocumentRangeFormattingEditProvider
 */
export class SSLDocumentFormattingProvider
    implements vscode.DocumentFormattingEditProvider, vscode.DocumentRangeFormattingEditProvider
{
    /**
     * Format the entire document
     */
    public provideDocumentFormattingEdits(
        document: vscode.TextDocument,
        options: vscode.FormattingOptions,
        token: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.TextEdit[]> {
        try {
            const text = document.getText();
            const formattedText = this.formatText(text, options);

            if (formattedText === text) {
                // No changes needed
                return [];
            }

            // Return a single edit that replaces the entire document
            const fullRange = new vscode.Range(
                document.positionAt(0),
                document.positionAt(text.length)
            );

            return [vscode.TextEdit.replace(fullRange, formattedText)];
        } catch (error) {
            console.error("Error formatting SSL document:", error);
            // Return empty array to indicate no changes
            return [];
        }
    }

    /**
     * Format a selected range of the document
     */
    public provideDocumentRangeFormattingEdits(
        document: vscode.TextDocument,
        range: vscode.Range,
        options: vscode.FormattingOptions,
        token: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.TextEdit[]> {
        try {
            // For SSL, we need to format the entire document to maintain proper structure
            // since SSL constructs like procedures and classes can span multiple lines and
            // partial formatting might break the structure

            // Get the text within the range
            const rangeText = document.getText(range);

            // If the range is the entire document, use document formatting
            const fullRange = new vscode.Range(
                document.positionAt(0),
                document.positionAt(document.getText().length)
            );

            if (range.isEqual(fullRange)) {
                return this.provideDocumentFormattingEdits(document, options, token);
            }

            // For partial ranges, we'll format the entire document and then extract
            // only the changes within the requested range
            const fullText = document.getText();
            const formattedFullText = this.formatText(fullText, options);

            if (formattedFullText === fullText) {
                return [];
            }

            // Calculate the formatted text for the range
            const startOffset = document.offsetAt(range.start);
            const endOffset = document.offsetAt(range.end);

            // Find corresponding positions in formatted text
            // This is a simplified approach - for production, you'd want more sophisticated
            // range mapping that accounts for formatting changes
            const formattedRangeText = formattedFullText.substring(startOffset, endOffset);

            if (formattedRangeText === rangeText) {
                return [];
            }

            return [vscode.TextEdit.replace(range, formattedRangeText)];
        } catch (error) {
            console.error("Error formatting SSL document range:", error);
            return [];
        }
    }

    /**
     * Format SSL text using the formatter
     */
    private formatText(text: string, vscodeOptions: vscode.FormattingOptions): string {
        try {
            // Create formatter options from VS Code formatting options
            const formatterOptions = this.createFormatterOptions(vscodeOptions);

            // Tokenize the input
            const tokenizer = new Tokenizer();
            const tokenizeResult = tokenizer.tokenize(text);

            if (tokenizeResult.hasErrors || tokenizeResult.tokens.length === 0) {
                console.warn("SSL tokenization failed, returning original text");
                return text;
            }

            // Parse the tokens into an AST
            const parser = new Parser(tokenizeResult.tokens);
            const parseResult = parser.parse();

            if (!parseResult.success || !parseResult.ast) {
                console.warn("SSL parsing failed, returning original text");
                return text;
            }

            // Format the AST
            const formatter = new SSLFormatter(formatterOptions);
            return formatter.format(parseResult.ast);
        } catch (error) {
            console.error("Error in SSL formatting:", error);
            return text;
        }
    }

    /**
     * Convert VS Code formatting options to SSL formatter options
     */
    private createFormatterOptions(vscodeOptions: vscode.FormattingOptions): FormatterOptions {
        // Get SSL-specific configuration
        const config = vscode.workspace.getConfiguration("sslFormatter");

        // Create base options from VS Code settings
        const baseConfig = {
            indentSize: vscodeOptions.tabSize,
            insertSpaces: vscodeOptions.insertSpaces,
            tabSize: vscodeOptions.tabSize,
            // Add SSL-specific settings from workspace configuration
            ssl: config.get("ssl", {}),
        };

        return createFormatterOptionsFromConfig(baseConfig);
    }
}
