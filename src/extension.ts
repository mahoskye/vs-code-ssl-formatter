import * as vscode from "vscode";

/**
 * This function is called when the extension is activated.
 * It sets up the SSL language support.
 */
export function activate(context: vscode.ExtensionContext) {
    // Define the document selector for SSL files
    const selector = { scheme: "file", language: "ssl" };

    context.subscriptions.push();
}

/**
 * This function is called when the extension is deactivated.
 */
export function deactivate() {}
