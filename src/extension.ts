import * as vscode from "vscode";
import { SSLCodeLensProvider } from "./sslCodeLensProvider";
import { SSLCodeActionProvider } from "./sslCodeActionProvider";
import { SSLDocumentHighlightProvider } from "./sslDocumentHighlightProvider";
import { SSLCallHierarchyProvider } from "./sslCallHierarchyProvider";
import { Logger } from "./utils/logger";
import { SSL_DOCUMENT_SELECTORS } from "./utils/documentSelectors";
import { registerCommentController } from "./sslCommentController";
import { registerFormatSqlCommand } from "./commands/formatSql";
import { startClient, stopClient, restartClient, isClientRunning } from "./lspClient";
import { registerStatusBar, refreshStatusBar } from "./sslStatusBar";
import { registerBlockCloser } from "./sslBlockCloser";
import { migrateRetiredSettings } from "./utils/settingsMigration";

/**
 * Activates the SSL extension.
 *
 * The bundled starlims-lsp server provides the language features: completion,
 * hover, definition, references, symbols, folding, signature help, rename,
 * inlay hints, formatting and diagnostics. There is no native fallback for
 * them (#106): a second implementation drifted from the server, and the server
 * ships for every platform the extension is packaged for. If it fails to
 * start, the user is told why and can retry. The extension adds editor
 * features the server does not offer: code actions, CodeLens, document
 * highlights, call hierarchy, the block closer, comment toggling and the
 * Format SQL commands.
 * @param context The extension context provided by VS Code.
 */
export async function activate(context: vscode.ExtensionContext) {
    Logger.initialize(context);
    Logger.info("SSL extension is now active!");

    await migrateRetiredSettings();

    const documentSelector = SSL_DOCUMENT_SELECTORS;
    registerCommentController(context);
    registerFormatSqlCommand(context);
    registerStatusBar(context);

    context.subscriptions.push(
        vscode.commands.registerCommand("ssl.restartLanguageServer", async () => {
            try {
                if (isClientRunning()) {
                    await restartClient(context);
                    vscode.window.showInformationMessage("SSL Language Server restarted");
                } else {
                    await stopClient();
                    await startClient(context);
                    vscode.window.showInformationMessage("SSL Language Server started");
                }
            } catch (error) {
                reportStartFailure(error);
            } finally {
                refreshStatusBar();
            }
        })
    );

    try {
        await startClient(context);
        Logger.info("SSL Language Server started successfully");
    } catch (error) {
        reportStartFailure(error);
    }
    refreshStatusBar();

    // Format on save (ssl.format.formatOnSave) runs the server's formatter.
    context.subscriptions.push(
        vscode.workspace.onWillSaveTextDocument(event => {
            const formatConfig = vscode.workspace.getConfiguration("ssl");
            const formatOnSave = formatConfig.get<boolean>("format.formatOnSave", false);

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

    context.subscriptions.push(
        vscode.commands.registerCommand("ssl.toggleInlayHints", async () => {
            const config = vscode.workspace.getConfiguration("ssl");
            const currentValue = config.get<boolean>("intellisense.inlayHints.enabled", true);
            await config.update("intellisense.inlayHints.enabled", !currentValue, vscode.ConfigurationTarget.Global);
            const status = !currentValue ? "enabled" : "disabled";
            vscode.window.showInformationMessage(`SSL Inlay Hints ${status}`);
        })
    );

    // Code actions for quick fixes (not provided by the server)
    context.subscriptions.push(
        vscode.languages.registerCodeActionsProvider("ssl", new SSLCodeActionProvider(), {
            providedCodeActionKinds: SSLCodeActionProvider.providedCodeActionKinds
        })
    );

    // CodeLens reference counts (not provided by the server)
    const codeLensProvider = new SSLCodeLensProvider();
    context.subscriptions.push(
        vscode.languages.registerCodeLensProvider(documentSelector, codeLensProvider)
    );
    context.subscriptions.push(
        vscode.workspace.onDidChangeTextDocument(() => {
            codeLensProvider.refresh();
        })
    );

    // Symbol occurrence highlighting (not provided by the server)
    context.subscriptions.push(
        vscode.languages.registerDocumentHighlightProvider(documentSelector, new SSLDocumentHighlightProvider())
    );

    // Procedure call hierarchy (not provided by the server)
    context.subscriptions.push(
        vscode.languages.registerCallHierarchyProvider(documentSelector, new SSLCallHierarchyProvider())
    );

    // Auto-insert the matching closer (:ENDIF, :ENDWHILE, :NEXT, ...) when the
    // user presses Enter on an opener line. It works on text events only.
    registerBlockCloser(context);

    Logger.info("SSL extension fully activated");
}

/**
 * Tells the user the language server could not start, with ways to see why
 * and to try again. Without the server there are no language features, so
 * this is an error rather than a quiet log line.
 */
function reportStartFailure(error: unknown): void {
    const message = error instanceof Error ? error.message : String(error);
    Logger.error(`Failed to start SSL Language Server: ${message}`);
    vscode.window.showErrorMessage(
        `SSL Language Server failed to start: ${message}. SSL language features are unavailable until it runs.`,
        "Show Output",
        "Retry"
    ).then(selection => {
        if (selection === "Show Output") {
            Logger.show();
        } else if (selection === "Retry") {
            void vscode.commands.executeCommand("ssl.restartLanguageServer");
        }
    });
}

/**
 * Deactivates the SSL extension.
 * This function is called when the extension is deactivated.
 * Use this to clean up your extension resources.
 */
export async function deactivate() {
    Logger.info("SSL extension is being deactivated");

    // Stop the LSP client
    try {
        await stopClient();
        Logger.info("SSL Language Server stopped");
    } catch (error) {
        Logger.error(`Error stopping SSL Language Server: ${error}`);
    }

    Logger.dispose();
}
