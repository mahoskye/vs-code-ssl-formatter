import * as vscode from "vscode";
import { SSLFoldingProvider } from "./sslFoldingProvider";
import { SSLFormattingProvider } from "./sslFormattingProvider";
import { SSLSymbolProvider } from "./sslSymbolProvider";
import { SSLCompletionProvider } from "./sslCompletionProvider";
import { SSLHoverProvider } from "./sslHoverProvider";
import { SSLDiagnosticProvider } from "./sslDiagnosticProvider";
import { SSLDefinitionProvider } from "./sslDefinitionProvider";
import { SSLReferenceProvider } from "./sslReferenceProvider";
import { SSLRenameProvider } from "./sslRenameProvider";
import { SSLSignatureHelpProvider } from "./sslSignatureHelpProvider";
import { SSLCodeLensProvider } from "./sslCodeLensProvider";
import { SSLCodeActionProvider } from "./sslCodeActionProvider";
import { SSLWorkspaceSymbolProvider } from "./sslWorkspaceSymbolProvider";
import { SSLDocumentHighlightProvider } from "./sslDocumentHighlightProvider";
import { SSLCallHierarchyProvider } from "./sslCallHierarchyProvider";
import { SSLInlayHintsProvider } from "./sslInlayHintsProvider";
import { Logger } from "./utils/logger";
import { SSL_DOCUMENT_SELECTORS } from "./utils/documentSelectors";
import { registerCommentController } from "./sslCommentController";
import { WorkspaceClassIndex } from "./utils/classIndex";
import { WorkspaceProcedureIndex } from "./utils/procedureIndex";
import { registerConfigureNamespacesCommand } from "./commands/configureNamespaces";
import { registerFormatSqlCommand } from "./commands/formatSql";
import { startClient, stopClient, restartClient, isClientRunning } from "./lspClient";
import { loadInventory, getInventorySourceVersion } from "./utils/inventory";
import { registerStatusBar, refreshStatusBar } from "./sslStatusBar";

// Track if LSP is active for this session
let lspActive = false;

/**
 * Activates the SSL extension.
 * This function is called when the extension is activated.
 * The extension is activated the first time the command is executed.
 * @param context The extension context provided by VS Code.
 */
export async function activate(context: vscode.ExtensionContext) {
    // Initialize logger
    Logger.initialize(context);
    Logger.info("SSL extension is now active!");

    const config = vscode.workspace.getConfiguration("ssl");
    const lspEnabled = config.get<boolean>("languageServer.enabled", true);

    // Load full SSL element inventory from the bundled LSP binary so native
    // fallback providers see the same 330-function / 29-class catalog the LSP
    // uses. Failures leave the loader inactive and providers fall back to the
    // small hardcoded subset in constants/language.ts.
    loadInventory(context).then(() => {
        const ver = getInventorySourceVersion();
        if (ver) {
            Logger.info(`SSL inventory loaded from bundled LSP (${ver})`);
        }
        refreshStatusBar();
    });

    // Try to start the LSP client if enabled
    if (lspEnabled) {
        try {
            await startClient(context);
            lspActive = true;
            Logger.info("SSL Language Server started successfully");
        } catch (error) {
            const message = error instanceof Error ? error.message : String(error);
            Logger.error(`Failed to start SSL Language Server: ${message}`);
            Logger.info("Falling back to native providers");
            vscode.window.showWarningMessage(
                `SSL Language Server failed to start: ${message}. Using native providers.`,
                "Show Output"
            ).then(selection => {
                if (selection === "Show Output") {
                    Logger.show();
                }
            });
            lspActive = false;
        }
    } else {
        Logger.info("SSL Language Server disabled, using native providers");
        lspActive = false;
    }

    const documentSelector = SSL_DOCUMENT_SELECTORS;
    registerCommentController(context);
    registerConfigureNamespacesCommand(context);
    registerFormatSqlCommand(context);
    registerStatusBar(context);

    context.subscriptions.push(
        vscode.commands.registerCommand('ssl.restartLanguageServer', async () => {
            if (!lspActive || !isClientRunning()) {
                vscode.window.showWarningMessage(
                    'SSL Language Server is not active. Reload the window to start it.'
                );
                return;
            }

            try {
                await restartClient(context);
                vscode.window.showInformationMessage('SSL Language Server restarted');
            } catch (error) {
                const message = error instanceof Error ? error.message : String(error);
                Logger.error(`Failed to restart SSL Language Server: ${message}`);
                vscode.window.showErrorMessage(`Failed to restart SSL Language Server: ${message}`);
            }
        })
    );

    const classIndex = new WorkspaceClassIndex();
    context.subscriptions.push(classIndex);
    await classIndex.initialize();

    const procedureIndex = new WorkspaceProcedureIndex();
    context.subscriptions.push(procedureIndex);
    await procedureIndex.initialize();

    // Features provided by LSP when active:
    // - Completion, Hover, Definition, References, Document Symbols
    // - Folding Ranges, Signature Help, Formatting, Diagnostics
    // 
    // When LSP is NOT active, register native providers for these features.
    // When LSP IS active, skip registering native providers to avoid duplication.

    if (!lspActive) {
        // Register folding range provider (LSP provides this when active)
        context.subscriptions.push(
            vscode.languages.registerFoldingRangeProvider(documentSelector, new SSLFoldingProvider())
        );

        // Register formatting providers (LSP provides this when active)
        const formattingProvider = new SSLFormattingProvider();
        context.subscriptions.push(
            vscode.languages.registerDocumentFormattingEditProvider(documentSelector, formattingProvider)
        );
        context.subscriptions.push(
            vscode.languages.registerDocumentRangeFormattingEditProvider(documentSelector, formattingProvider)
        );

        // Register document symbol provider (LSP provides this when active)
        context.subscriptions.push(
            vscode.languages.registerDocumentSymbolProvider(documentSelector, new SSLSymbolProvider())
        );

        // Register completion provider (LSP provides this when active)
        context.subscriptions.push(
            vscode.languages.registerCompletionItemProvider(
                documentSelector,
                new SSLCompletionProvider(classIndex, procedureIndex),
                ":", ".", "(", '"', "'"
            )
        );

        // Register hover provider (LSP provides this when active)
        context.subscriptions.push(
            vscode.languages.registerHoverProvider(documentSelector, new SSLHoverProvider(classIndex, procedureIndex))
        );

        // Register diagnostic provider (LSP provides this when active)
        const diagnosticProvider = new SSLDiagnosticProvider();
        context.subscriptions.push(diagnosticProvider);

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
            vscode.workspace.onDidChangeConfiguration(e => {
                if (e.affectsConfiguration("ssl.diagnostics.onlyOpenDocuments")) {
                    const diagConfig = vscode.workspace.getConfiguration("ssl");
                    const onlyOpen = diagConfig.get<boolean>("diagnostics.onlyOpenDocuments", true);

                    if (onlyOpen) {
                        diagnosticProvider.clear();
                        vscode.workspace.textDocuments.forEach(document => {
                            if (document.languageId === "ssl") {
                                diagnosticProvider.updateDiagnostics(document);
                            }
                        });
                    }
                }
            })
        );

        // Run diagnostics on already open SSL documents
        vscode.workspace.textDocuments.forEach(document => {
            if (document.languageId === "ssl") {
                diagnosticProvider.updateDiagnostics(document);
            }
        });

        // Register definition provider (LSP provides this when active)
        context.subscriptions.push(
            vscode.languages.registerDefinitionProvider(documentSelector, new SSLDefinitionProvider(procedureIndex))
        );

        // Register reference provider (LSP provides this when active)
        context.subscriptions.push(
            vscode.languages.registerReferenceProvider(documentSelector, new SSLReferenceProvider(procedureIndex))
        );

        // Register signature help (LSP provides this when active)
        context.subscriptions.push(
            vscode.languages.registerSignatureHelpProvider(
                documentSelector,
                new SSLSignatureHelpProvider(procedureIndex),
                "(", ","
            )
        );

        // Register rename provider (LSP provides this when active)
        context.subscriptions.push(
            vscode.languages.registerRenameProvider(documentSelector, new SSLRenameProvider())
        );

        // Register workspace symbol provider (LSP provides this when active)
        context.subscriptions.push(
            vscode.languages.registerWorkspaceSymbolProvider(new SSLWorkspaceSymbolProvider(procedureIndex, classIndex))
        );

        // Register inlay hints provider (LSP provides this when active)
        const inlayHintsProvider = new SSLInlayHintsProvider(procedureIndex);
        context.subscriptions.push(
            vscode.languages.registerInlayHintsProvider(documentSelector, inlayHintsProvider)
        );

        Logger.info("Registered native providers (LSP not active)");
    } else {
        Logger.info("Using LSP for: completion, hover, definition, references, symbols, folding, signature help, formatting, diagnostics, rename, inlay hints, workspace symbols");
    }

    // Register format on save if enabled (works with both LSP and native formatting)
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

    // Register command to toggle inlay hints (works with both LSP and native)
    context.subscriptions.push(
        vscode.commands.registerCommand('ssl.toggleInlayHints', async () => {
            const config = vscode.workspace.getConfiguration("ssl");
            const currentValue = config.get<boolean>("intellisense.inlayHints.enabled", true);
            await config.update("intellisense.inlayHints.enabled", !currentValue, vscode.ConfigurationTarget.Global);
            const status = !currentValue ? "enabled" : "disabled";
            vscode.window.showInformationMessage(`SSL Inlay Hints ${status}`);
        })
    );

    // Register Code Action Provider for quick fixes (not provided by LSP)
    context.subscriptions.push(
        vscode.languages.registerCodeActionsProvider('ssl', new SSLCodeActionProvider(), {
            providedCodeActionKinds: SSLCodeActionProvider.providedCodeActionKinds
        })
    );

    // Register CodeLens provider for reference counts (not provided by LSP)
    const codeLensProvider = new SSLCodeLensProvider();
    context.subscriptions.push(
        vscode.languages.registerCodeLensProvider(documentSelector, codeLensProvider)
    );

    // Refresh CodeLens when document changes
    context.subscriptions.push(
        vscode.workspace.onDidChangeTextDocument(() => {
            codeLensProvider.refresh();
        })
    );

    // Register document highlight provider for symbol occurrence highlighting (not provided by LSP)
    context.subscriptions.push(
        vscode.languages.registerDocumentHighlightProvider(documentSelector, new SSLDocumentHighlightProvider())
    );

    // Register call hierarchy provider for procedure call trees (not provided by LSP)
    context.subscriptions.push(
        vscode.languages.registerCallHierarchyProvider(documentSelector, new SSLCallHierarchyProvider())
    );

    Logger.info("SSL extension fully activated with all language features including workspace search, call hierarchy, and inlay hints");
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
