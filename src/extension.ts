import * as vscode from "vscode";
import { SSLFoldingProvider } from "./sslFoldingProvider";
import { SSLCompletionProvider } from "./sslCompletionProvider";

/**
 * This function is called when the extension is activated.
 * It sets up the SSL language support.
 */
export function activate(context: vscode.ExtensionContext) {
    // Define the document selector for SSL files
    const selector = { scheme: "file", language: "ssl" };

    // Register the folding range provider
    const sslFoldingProvider = new SSLFoldingProvider();
    const foldingProvider = vscode.languages.registerFoldingRangeProvider(selector, sslFoldingProvider);

    // Register the completion item provider
    const sslCompletionProvider = new SSLCompletionProvider();
    const completionProvider = vscode.languages.registerCompletionItemProvider(selector, sslCompletionProvider);

    // Register the command to reload completion items
    const reloadCommand = vscode.commands.registerCommand("sslFormatter.reloadCompletions", () => {
        sslCompletionProvider.loadCompletionItems();
        vscode.window.showInformationMessage("SSL completion items reloaded.");
    });

    // Add all providers and commands to the extension's subscriptions
    context.subscriptions.push(foldingProvider, completionProvider, reloadCommand);
}

/**
 * This function is called when the extension is deactivated.
 */
export function deactivate() {}
