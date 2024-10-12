import * as vscode from "vscode";
import { SSLFoldingProvider } from "./sslFoldingProvider";

/**
 * Activates the SSL extension.
 * This function is called when the extension is activated.
 * The extension is activated the first time the command is executed.
 * @param context The extension context provided by VS Code.
 */
export function activate(context: vscode.ExtensionContext) {
    console.log("SSL extension is now active!");

    // Register the folding range provider for SSL language
    context.subscriptions.push(
        vscode.languages.registerFoldingRangeProvider({ language: "ssl" }, new SSLFoldingProvider())
    );
}

/**
 * Deactivates the SSL extension.
 * This function is called when the extension is deactivated.
 * Use this to clean up your extension resources.
 */
export function deactivate() {
    // Currently, no cleanup is necessary
}
