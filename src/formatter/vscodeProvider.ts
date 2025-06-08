/**
 * VS Code Document Formatting Provider for SSL
 * Integrates the SSL formatter with VS Code's formatting commands
 */

import * as vscode from "vscode";
import { SSLFormatter, FormatResult } from "../formatter";

/**
 * SSL Document Formatting Provider
 */
export class SSLDocumentFormattingProvider implements vscode.DocumentFormattingEditProvider {
    private formatter: SSLFormatter;

    constructor() {
        this.formatter = new SSLFormatter();
        this.updateFormatterOptions();
    }

    /**
     * Provide formatting edits for the entire document
     */
    async provideDocumentFormattingEdits(
        document: vscode.TextDocument,
        options: vscode.FormattingOptions,
        token: vscode.CancellationToken
    ): Promise<vscode.TextEdit[]> {
        if (token.isCancellationRequested) {
            return [];
        }

        try {
            // Update formatter options based on VS Code settings
            this.updateFormatterOptions(options);

            // Get the full document text
            const sourceCode = document.getText();

            // Format the code
            const result = this.formatter.formatCode(sourceCode);

            // Handle formatting result
            if (!result.success) {
                this.showFormattingErrors(result);
                return [];
            }

            // Show warnings if any
            if (result.warnings.length > 0) {
                this.showFormattingWarnings(result);
            }

            // Create text edit for the entire document
            const fullRange = new vscode.Range(
                document.positionAt(0),
                document.positionAt(sourceCode.length)
            );

            return [vscode.TextEdit.replace(fullRange, result.formattedCode)];
        } catch (error) {
            vscode.window.showErrorMessage(
                `SSL Formatter error: ${error instanceof Error ? error.message : String(error)}`
            );
            return [];
        }
    }

    /**
     * Update formatter options based on VS Code configuration
     */ private updateFormatterOptions(vsCodeOptions?: vscode.FormattingOptions): void {
        const config = vscode.workspace.getConfiguration("ssl.formatting");
        const formatterOptions = {
            indentSize: vsCodeOptions?.tabSize ?? config.get<number>("indentSize", 4),
            useTabs:
                vsCodeOptions?.insertSpaces === false
                    ? false
                    : config.get<boolean>("useTabs", false),
            maxLineLength: config.get<number>("maxLineLength", 90),
            blankLinesBetweenProcedures: config.get<boolean>("blankLinesBetweenProcedures", true),
            alignConsecutiveAssignments: config.get<boolean>("alignConsecutiveAssignments", false),
            formatSqlStrings: config.get<boolean>("formatSqlStrings", false),
        };

        this.formatter.updateOptions(formatterOptions);
    }

    /**
     * Show formatting errors to the user
     */
    private showFormattingErrors(result: FormatResult): void {
        const errorMessage = `SSL formatting failed:\n${result.errors.join("\n")}`;
        vscode.window.showErrorMessage("SSL Formatting Failed", {
            detail: errorMessage,
            modal: false,
        });
    }

    /**
     * Show formatting warnings to the user
     */
    private showFormattingWarnings(result: FormatResult): void {
        const warningMessage = `SSL formatting completed with warnings:\n${result.warnings.join(
            "\n"
        )}`;
        vscode.window.showWarningMessage("SSL Formatting Warnings", {
            detail: warningMessage,
            modal: false,
        });
    }
}

/**
 * SSL Document Range Formatting Provider
 */
export class SSLDocumentRangeFormattingProvider
    implements vscode.DocumentRangeFormattingEditProvider
{
    private formatter: SSLFormatter;

    constructor() {
        this.formatter = new SSLFormatter();
    }

    /**
     * Provide formatting edits for a range of the document
     */
    async provideDocumentRangeFormattingEdits(
        document: vscode.TextDocument,
        range: vscode.Range,
        options: vscode.FormattingOptions,
        token: vscode.CancellationToken
    ): Promise<vscode.TextEdit[]> {
        if (token.isCancellationRequested) {
            return [];
        }

        try {
            // For range formatting, we'll format the entire document and extract the range
            // This ensures proper context for parsing and formatting
            const fullFormattingProvider = new SSLDocumentFormattingProvider();
            const fullEdits = await fullFormattingProvider.provideDocumentFormattingEdits(
                document,
                options,
                token
            );

            if (fullEdits.length === 0) {
                return [];
            }

            // Apply the full formatting edit and extract the requested range
            const fullEdit = fullEdits[0];
            const formattedLines = fullEdit.newText.split("\n");

            // Calculate which lines correspond to the requested range
            const startLine = range.start.line;
            const endLine = Math.min(range.end.line, formattedLines.length - 1);

            // Extract the formatted range
            const rangeText = formattedLines.slice(startLine, endLine + 1).join("\n");

            return [vscode.TextEdit.replace(range, rangeText)];
        } catch (error) {
            vscode.window.showErrorMessage(
                `SSL Range Formatter error: ${
                    error instanceof Error ? error.message : String(error)
                }`
            );
            return [];
        }
    }
}

/**
 * Register SSL formatting providers with VS Code
 */
export function registerFormattingProviders(context: vscode.ExtensionContext): void {
    const documentSelector = { scheme: "file", language: "ssl" }; // Register document formatting provider
    const documentFormattingProvider = new SSLDocumentFormattingProvider();
    context.subscriptions.push(
        vscode.languages.registerDocumentFormattingEditProvider(
            documentSelector,
            documentFormattingProvider
        )
    ); // Register range formatting provider
    const rangeFormattingProvider = new SSLDocumentRangeFormattingProvider();
    context.subscriptions.push(
        vscode.languages.registerDocumentRangeFormattingEditProvider(
            documentSelector,
            rangeFormattingProvider
        )
    ); // Register format on type provider for auto-formatting
    context.subscriptions.push(
        vscode.languages.registerOnTypeFormattingEditProvider(
            documentSelector,
            {
                provideOnTypeFormattingEdits(
                    document: vscode.TextDocument,
                    position: vscode.Position,
                    ch: string,
                    options: vscode.FormattingOptions,
                    token: vscode.CancellationToken
                ): vscode.ProviderResult<vscode.TextEdit[]> {
                    // Auto-format on semicolon (end of statement)
                    if (ch === ";") {
                        const line = document.lineAt(position.line);
                        const lineText = line.text.trim();

                        // Format the current statement
                        if (lineText.endsWith(";")) {
                            const formatter = new SSLFormatter();
                            const result = formatter.formatCode(lineText);

                            if (result.success) {
                                const lineRange = new vscode.Range(
                                    new vscode.Position(position.line, 0),
                                    new vscode.Position(position.line, line.text.length)
                                );
                                return [
                                    vscode.TextEdit.replace(lineRange, result.formattedCode.trim()),
                                ];
                            }
                        }
                    }
                    return [];
                },
            },
            ";"
        )
    );
}
