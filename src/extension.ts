import * as vscode from "vscode";
import { SSLFoldingRangeProvider, SSLBracketMatchingProvider } from "./providers";

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

    // Register the bracket matching provider
    const bracketMatchingProvider = new SSLBracketMatchingProvider();
    // Note: VS Code doesn't have a direct bracket matching provider API.
    // The bracket matching functionality is provided through the language configuration
    // and potentially through commands or status bar information.    // Register a command to show bracket matching information
    const bracketInfoCommand = vscode.commands.registerCommand(
        "ssl-formatter.showBracketInfo",
        () => {
            const editor = vscode.window.activeTextEditor;
            if (editor && editor.document.languageId === "ssl") {
                const info = bracketMatchingProvider.getBracketPairInfo(editor.document);
                vscode.window.showInformationMessage(`SSL Bracket Pairs:\n${info}`);
            }
        }
    );

    // Register a command to show bracket information at cursor position
    const bracketInfoAtCursorCommand = vscode.commands.registerCommand(
        "ssl-formatter.showBracketInfoAtCursor",
        () => {
            const editor = vscode.window.activeTextEditor;
            if (editor && editor.document.languageId === "ssl") {
                const position = editor.selection.active;
                const info = bracketMatchingProvider.getBracketInfoAtPosition(
                    editor.document,
                    position
                );
                vscode.window.showInformationMessage(info);
            }
        }
    );

    // Register a command to find matching bracket
    const findMatchingBracketCommand = vscode.commands.registerCommand(
        "ssl-formatter.findMatchingBracket",
        () => {
            const editor = vscode.window.activeTextEditor;
            if (editor && editor.document.languageId === "ssl") {
                const position = editor.selection.active;
                const matchingPos = bracketMatchingProvider.findMatchingBracket(
                    editor.document,
                    position
                );

                if (matchingPos) {
                    // Move cursor to matching bracket
                    const newSelection = new vscode.Selection(matchingPos, matchingPos);
                    editor.selection = newSelection;
                    editor.revealRange(new vscode.Range(matchingPos, matchingPos));
                } else {
                    vscode.window.showInformationMessage(
                        "No matching bracket found at cursor position."
                    );
                }
            }
        }
    );

    // Add diagnostic collection for bracket validation
    const diagnosticCollection = vscode.languages.createDiagnosticCollection("ssl-brackets");

    // Validate brackets on document change
    const validateBrackets = (document: vscode.TextDocument) => {
        if (document.languageId === "ssl") {
            const diagnostics = bracketMatchingProvider.validateBracketPairs(document);
            diagnosticCollection.set(document.uri, diagnostics);
        }
    };

    // Register document change listeners
    const onDidChangeTextDocument = vscode.workspace.onDidChangeTextDocument((e) => {
        validateBrackets(e.document);
    });

    const onDidOpenTextDocument = vscode.workspace.onDidOpenTextDocument((document) => {
        validateBrackets(document);
    });

    // Validate all currently open SSL documents
    vscode.workspace.textDocuments.forEach(validateBrackets); // Add to subscriptions for proper cleanup
    context.subscriptions.push(
        foldingDisposable,
        bracketInfoCommand,
        bracketInfoAtCursorCommand,
        findMatchingBracketCommand,
        diagnosticCollection,
        onDidChangeTextDocument,
        onDidOpenTextDocument
    );

    // Log successful activation
    console.log("SSL Formatter extension is now active!");
    console.log("Registered SSL folding range provider and bracket matching functionality");
}

/**
 * This function is called when the extension is deactivated.
 */
export function deactivate() {}
