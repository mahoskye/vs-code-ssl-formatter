import * as vscode from "vscode";
import { SSLFoldingProvider } from "./sslFoldingProvider";
import { SSLFormattingProvider } from "./sslFormattingProvider";
import { SSLSymbolProvider } from "./sslSymbolProvider";
import { SSLCompletionProvider } from "./sslCompletionProvider";
import { SSLHoverProvider } from "./sslHoverProvider";
import { SSLDiagnosticProvider } from "./sslDiagnosticProvider";

/**
 * Activates the SSL extension.
 * This function is called when the extension is activated.
 * The extension is activated the first time the command is executed.
 * @param context The extension context provided by VS Code.
 */
export function activate(context: vscode.ExtensionContext) {
    console.log("SSL extension is now active!");

    const documentSelector: vscode.DocumentSelector = { language: "ssl", scheme: "file" };

    // Register the folding range provider for SSL language
    context.subscriptions.push(
        vscode.languages.registerFoldingRangeProvider(documentSelector, new SSLFoldingProvider())
    );

    // Register formatting providers
    const formattingProvider = new SSLFormattingProvider();
    context.subscriptions.push(
        vscode.languages.registerDocumentFormattingEditProvider(documentSelector, formattingProvider)
    );
    context.subscriptions.push(
        vscode.languages.registerDocumentRangeFormattingEditProvider(documentSelector, formattingProvider)
    );

    // Register document symbol provider for outline and breadcrumbs
    context.subscriptions.push(
        vscode.languages.registerDocumentSymbolProvider(documentSelector, new SSLSymbolProvider())
    );

    // Register completion provider for IntelliSense
    context.subscriptions.push(
        vscode.languages.registerCompletionItemProvider(
            documentSelector,
            new SSLCompletionProvider(),
            ":", ".", "("
        )
    );

    // Register hover provider for symbol information
    context.subscriptions.push(
        vscode.languages.registerHoverProvider(documentSelector, new SSLHoverProvider())
    );

    // Register diagnostic provider for code quality checks
    const diagnosticProvider = new SSLDiagnosticProvider();
    context.subscriptions.push(diagnosticProvider);

    // Update diagnostics on document changes
    context.subscriptions.push(
        vscode.workspace.onDidOpenTextDocument(document => {
            if (document.languageId === "ssl") {
                diagnosticProvider.updateDiagnostics(document);
            }
        })
    );

    context.subscriptions.push(
        vscode.workspace.onDidChangeTextDocument(event => {
            if (event.document.languageId === "ssl") {
                diagnosticProvider.updateDiagnostics(event.document);
            }
        })
    );

    context.subscriptions.push(
        vscode.workspace.onDidCloseTextDocument(document => {
            if (document.languageId === "ssl") {
                diagnosticProvider.clear(document);
            }
        })
    );

    // Run diagnostics on already open SSL documents
    vscode.workspace.textDocuments.forEach(document => {
        if (document.languageId === "ssl") {
            diagnosticProvider.updateDiagnostics(document);
        }
    });

    // Register format on save if enabled
    context.subscriptions.push(
        vscode.workspace.onWillSaveTextDocument(event => {
            const config = vscode.workspace.getConfiguration("ssl");
            const formatOnSave = config.get<boolean>("format.formatOnSave", false);

            if (event.document.languageId === "ssl" && formatOnSave) {
                const editor = vscode.window.activeTextEditor;
                if (editor && editor.document === event.document) {
                    event.waitUntil(
                        vscode.commands.executeCommand("editor.action.formatDocument")
                    );
                }
            }
        })
    );

    console.log("SSL extension features registered: folding, formatting, symbols, completion, hover, diagnostics");
}

/**
 * Deactivates the SSL extension.
 * This function is called when the extension is deactivated.
 * Use this to clean up your extension resources.
 */
export function deactivate() {
    console.log("SSL extension is being deactivated");
}
