import * as vscode from "vscode";
import { registerFormattingProviders } from "./formatter/vscodeProvider";

/**
 * This function is called when the extension is activated.
 * It sets up the SSL language support.
 */
export function activate(context: vscode.ExtensionContext) {
    // Define the document selector for SSL files
    const selector = { scheme: "file", language: "ssl" };

    // Register SSL formatting providers
    registerFormattingProviders(context);

    // Log successful activation
    console.log("SSL Formatter extension is now active!");
}

/**
 * This function is called when the extension is deactivated.
 */
export function deactivate() {}
