import * as vscode from "vscode";
import { SSLFoldingProvider } from "./sslFoldingProvider";

export function activate(context: vscode.ExtensionContext) {
    console.log("SSL extension is now active!");

    context.subscriptions.push(
        vscode.languages.registerFoldingRangeProvider({ language: "ssl" }, new SSLFoldingProvider())
    );
}

export function deactivate() {}
