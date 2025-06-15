import * as vscode from "vscode";
import { SSLFoldingRangeProvider } from "./providers";

/**
 * This function is called when the extension is activated.
 * It sets up the SSL language support.
 */
export function activate(context: vscode.ExtensionContext) {
    // Define the document selector for SSL files
    const selector: vscode.DocumentSelector = { scheme: "file", language: "ssl" };

    // Register the folding range provider
    const foldingProvider = new SSLFoldingRangeProvider();
    const foldingDisposable = vscode.languages.registerFoldingRangeProvider(
        selector,
        foldingProvider
    );

    // Add to subscriptions for proper cleanup
    context.subscriptions.push(foldingDisposable);

    // Log successful activation
    console.log("SSL Formatter extension is now active!");
    console.log("Registered SSL folding range provider");
}

/**
 * This function is called when the extension is deactivated.
 */
export function deactivate() {}
