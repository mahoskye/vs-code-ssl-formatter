import * as vscode from "vscode";
import { SSLFoldingProvider } from "./sslFoldingProvider";
import { SSLCompletionProvider } from "./sslCompletionProvider";
import { SSLHoverProvider } from "./sslHoverProvider";
import { SSLFormatter } from "./sslFormatter";
import { SSLDiagnosticProvider } from "./parser/diagnostic";
import { SSLSemanticTokensProvider } from "./parser/semanticHighlighting";

/**
 * This function is called when the extension is activated.
 * It sets up the SSL language support.
 */
export function activate(context: vscode.ExtensionContext) {
    // Define the document selector for SSL files
    const selector = { scheme: "file", language: "ssl" };

    // Register the folding range provider
    const sslFoldingProvider = new SSLFoldingProvider();
    const foldingProvider = vscode.languages.registerFoldingRangeProvider(
        selector,
        sslFoldingProvider
    );

    // Register the completion item provider
    const sslCompletionProvider = new SSLCompletionProvider();
    const completionProvider = vscode.languages.registerCompletionItemProvider(
        selector,
        sslCompletionProvider
    );

    // Register the hover provider
    const sslHoverProvider = new SSLHoverProvider();
    const hoverProvider = vscode.languages.registerHoverProvider(selector, sslHoverProvider);

    // Register the formatter
    const formatter = new SSLFormatter();
    const formattingProvider = vscode.languages.registerDocumentFormattingEditProvider(
        "ssl",
        formatter
    );

    // Register the diagnostic provider
    const sslDiagnosticProvider = new SSLDiagnosticProvider();
    context.subscriptions.push(sslDiagnosticProvider);

    // Register semantic tokens provider
    const semanticTokensProvider = new SSLSemanticTokensProvider();
    const semanticTokensProviderRegistration =
        vscode.languages.registerDocumentSemanticTokensProvider(
            selector,
            semanticTokensProvider,
            semanticTokensProvider.getLegend()
        );

    // Register the command to reload completion items
    const reloadCommand = vscode.commands.registerCommand("sslFormatter.reloadCompletions", () => {
        sslCompletionProvider.loadCompletionItems();
        vscode.window.showInformationMessage("SSL completion items reloaded.");
    });

    // Add all providers and commands to the extension's subscriptions
    context.subscriptions.push(
        foldingProvider,
        completionProvider,
        hoverProvider,
        formattingProvider,
        semanticTokensProviderRegistration,
        reloadCommand
    );

    // Update diagnostics for the active editor when the extension is activated
    if (vscode.window.activeTextEditor) {
        sslDiagnosticProvider.updateDiagnostics(vscode.window.activeTextEditor.document);
    }

    // Update diagnostics when the active editor changes
    context.subscriptions.push(
        vscode.window.onDidChangeActiveTextEditor((editor) => {
            if (editor) {
                sslDiagnosticProvider.updateDiagnostics(editor.document);
            }
        })
    );

    // Update diagnostics when the document changes
    context.subscriptions.push(
        vscode.workspace.onDidChangeTextDocument((event) => {
            sslDiagnosticProvider.updateDiagnostics(event.document);
        })
    );

    // Clear diagnostics when a document is closed
    context.subscriptions.push(
        vscode.workspace.onDidCloseTextDocument((document) => {
            sslDiagnosticProvider.clearDiagnostics(document);
        })
    );
}

/**
 * This function is called when the extension is deactivated.
 */
export function deactivate() {}
