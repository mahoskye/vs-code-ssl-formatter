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

    const documentSelector = SSL_DOCUMENT_SELECTORS;
    registerCommentController(context);
    registerConfigureNamespacesCommand(context);
    registerFormatSqlCommand(context);

    const classIndex = new WorkspaceClassIndex();
    context.subscriptions.push(classIndex);
    await classIndex.initialize();

    const procedureIndex = new WorkspaceProcedureIndex();
    context.subscriptions.push(procedureIndex);
    await procedureIndex.initialize();

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
            new SSLCompletionProvider(classIndex, procedureIndex),
            ":", ".", "(", '"', "'"
        )
    );

    // Register hover provider for symbol information
    context.subscriptions.push(
        vscode.languages.registerHoverProvider(documentSelector, new SSLHoverProvider(classIndex, procedureIndex))
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
                diagnosticProvider.clear();
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

    // Register definition provider for Go to Definition
    context.subscriptions.push(
        vscode.languages.registerDefinitionProvider(documentSelector, new SSLDefinitionProvider(procedureIndex))
    );

    // Register reference provider for Find All References
    context.subscriptions.push(
        vscode.languages.registerReferenceProvider(documentSelector, new SSLReferenceProvider(procedureIndex))
    );

    // Register rename provider for symbol renaming
    context.subscriptions.push(
        vscode.languages.registerRenameProvider(documentSelector, new SSLRenameProvider())
    );

    // Signature help
    context.subscriptions.push(
        vscode.languages.registerSignatureHelpProvider(
            documentSelector,
            new SSLSignatureHelpProvider(procedureIndex),
            "(", ","
        )
    );

    // Register CodeLens provider for reference counts
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

    // Register code action provider for quick fixes
    context.subscriptions.push(
        vscode.languages.registerCodeActionsProvider(
            documentSelector,
            new SSLCodeActionProvider(),
            {
                providedCodeActionKinds: SSLCodeActionProvider.providedCodeActionKinds
            }
        )
    );

    // Register workspace symbol provider for global search (Ctrl+T)
    context.subscriptions.push(
        vscode.languages.registerWorkspaceSymbolProvider(new SSLWorkspaceSymbolProvider(procedureIndex, classIndex))
    );

    // Register document highlight provider for symbol occurrence highlighting
    context.subscriptions.push(
        vscode.languages.registerDocumentHighlightProvider(documentSelector, new SSLDocumentHighlightProvider())
    );

    // Register call hierarchy provider for procedure call trees
    context.subscriptions.push(
        vscode.languages.registerCallHierarchyProvider(documentSelector, new SSLCallHierarchyProvider())
    );

    // Register inlay hints provider for parameter names
    const inlayHintsProvider = new SSLInlayHintsProvider(procedureIndex);
    context.subscriptions.push(
        vscode.languages.registerInlayHintsProvider(documentSelector, inlayHintsProvider)
    );

    // Refresh inlay hints when cursor position changes
    let lastActiveLine: number | undefined = undefined;
    let debounceTimer: NodeJS.Timeout | undefined;

    context.subscriptions.push(
        vscode.window.onDidChangeTextEditorSelection((event) => {
            const editor = event.textEditor;
            if (editor.document.languageId === 'ssl') {
                const config = vscode.workspace.getConfiguration("ssl");
                const showOnActiveLineOnly = config.get<boolean>("intellisense.inlayHints.showOnActiveLineOnly", true);

                if (!showOnActiveLineOnly) {
                    return;
                }

                if (debounceTimer) {
                    clearTimeout(debounceTimer);
                }

                // Debounce refresh to avoid performance impact
                debounceTimer = setTimeout(() => {
                    const currentLine = editor.selection.active.line;
                    // Always refresh if line changed or if we suspect stale hints
                    if (lastActiveLine !== currentLine) {
                        lastActiveLine = currentLine;
                        inlayHintsProvider.refresh();
                    }
                }, 25);
            }
        })
    );

    Logger.info("SSL extension fully activated with all language features including workspace search, call hierarchy, and inlay hints");
}

/**
 * Deactivates the SSL extension.
 * This function is called when the extension is deactivated.
 * Use this to clean up your extension resources.
 */
export function deactivate() {
    Logger.info("SSL extension is being deactivated");
    Logger.dispose();
}
