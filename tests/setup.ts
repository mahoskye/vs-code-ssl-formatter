/**
 * Test setup file - runs before all tests
 * Mocks the VSCode API for unit testing
 */

import { Module } from 'module';
import {
	MockPosition,
	MockRange,
	MockTextEdit,
	MockUri,
	MockEndOfLine,
	MockDiagnosticSeverity,
	MockDiagnostic,
	MockDiagnosticCollection,
	MockLocation,
	MockWorkspaceEdit,
	MockMarkdownString,
	MockHover,
	MockEventEmitter,
	MockInlayHint,
	MockInlayHintKind,
	MockSymbolKind,
	MockDocumentSymbol,
	createSSLConfig,
	MockWorkspaceConfiguration,
	MockSnippetString,
	MockCompletionItem,
	MockCompletionItemKind,
	MockFoldingRange,
	MockFoldingRangeKind,
	MockTextDocument,
	createDocument,
	MockCodeActionKind,
	MockCodeLens
} from './helpers/mockVSCode';

// Shared state for mocks
const openDocuments: MockTextDocument[] = [];
let activeTextEditor: any = undefined;
const onDidOpenTextDocumentEmitter = new MockEventEmitter<MockTextDocument>();
const onDidCloseTextDocumentEmitter = new MockEventEmitter<MockTextDocument>();
const onDidChangeTextDocumentEmitter = new MockEventEmitter<any>();
const onWillSaveTextDocumentEmitter = new MockEventEmitter<any>();
// Store all created pools to aggregate diagnostics
const diagnosticCollections: MockDiagnosticCollection[] = [];

// Create the mock vscode module
const mockVscode = {
	workspace: {
		textDocuments: openDocuments,
		configuration: createSSLConfig(),
		getConfiguration(section?: string): MockWorkspaceConfiguration {
			return this.configuration;
		},
		asRelativePath(pathOrUri: string | any, includeWorkspaceFolder?: boolean): string {
			if (typeof pathOrUri === 'string') {
				return pathOrUri;
			}
			if (pathOrUri && pathOrUri.fsPath) {
				return pathOrUri.fsPath;
			}
			return String(pathOrUri);
		},
		applyEdit(edit: MockWorkspaceEdit): Thenable<boolean> {
			// Basic implementation: update document text in memory
			// Iterate over changes in edit
			for (const [uri, edits] of edit._changes) {
				const doc = openDocuments.find(d => d.uri.toString() === uri.toString());
				if (doc) {
					// Sort edits reverse to avoid offset shifting issues
					// Sort by line descending, then by character descending
					edits.sort((a, b) => {
						if (b.range.start.line !== a.range.start.line) {
							return b.range.start.line - a.range.start.line;
						}
						return b.range.start.character - a.range.start.character;
					});

					const lines = doc.getText().split('\n');

					for (const e of edits) {
						console.log(`Applying edit: ${JSON.stringify(e)}`);
						const startLine = e.range.start.line;
						const endLine = e.range.end.line;

						if (startLine === endLine) {
							const line = lines[startLine];
							// Ensure range is within bounds
							const startChar = Math.min(e.range.start.character, line.length);
							const endChar = Math.min(e.range.end.character, line.length);

							const pre = line.substring(0, startChar);
							const post = line.substring(endChar);
							lines[startLine] = pre + e.newText + post;
						} else {
							// Multi-line replacement
							const startChar = Math.min(e.range.start.character, lines[startLine].length);
							const endChar = Math.min(e.range.end.character, lines[endLine].length);

							const pre = lines[startLine].substring(0, startChar);
							const post = lines[endLine].substring(endChar);

							// Remove lines in between
							lines.splice(startLine, endLine - startLine + 1, pre + e.newText + post);
							// Note: e.newText might contain newlines, so we might need to split the result back into multiple lines
							// But for now keeping it simple as one string in the array slot if it gets complex might break lineCount assumption.
							// Properly, we should re-split:
							const insertedLines = (pre + e.newText + post).split('\n');
							lines.splice(startLine, 1, ...insertedLines);
						}
					}
					doc.setText(lines.join('\n'));
				}
			}
			return Promise.resolve(true);
		},
		openTextDocument(options: { language?: string, content?: string } | MockUri): Promise<MockTextDocument> {
			let doc: MockTextDocument;
			if (options instanceof MockUri) {
				doc = new MockTextDocument(options, 'ssl', '');
			} else {
				const fileName = (options as any).fileName || '/test.ssl';
				doc = new MockTextDocument(new MockUri(fileName), options.language || 'ssl', options.content || '');
			}
			openDocuments.push(doc);
			onDidOpenTextDocumentEmitter.fire(doc);
			return Promise.resolve(doc);
		},
		onDidOpenTextDocument: onDidOpenTextDocumentEmitter.event,
		onDidCloseTextDocument: onDidCloseTextDocumentEmitter.event,
		onDidChangeTextDocument: onDidChangeTextDocumentEmitter.event,
		onWillSaveTextDocument: onWillSaveTextDocumentEmitter.event,
		onDidSaveTextDocument: new MockEventEmitter<MockTextDocument>().event, // Added this
		onDidChangeConfiguration: new MockEventEmitter<any>().event, // Added this
		createFileSystemWatcher: () => ({ onDidCreate: () => ({ dispose: () => { } }), onDidChange: () => ({ dispose: () => { } }), onDidDelete: () => ({ dispose: () => { } }), dispose: () => { } }),
		findFiles: () => Promise.resolve([])
	},
	window: {
		activeTextEditor: undefined as any, // getter below
		showErrorMessage(message: string): void { },
		showWarningMessage(message: string): void { },
		showInformationMessage(message: string): void { },
		createOutputChannel(name: string): any {
			return {
				name,
				append: () => { },
				appendLine: () => { },
				clear: () => { },
				show: () => { },
				hide: () => { },
				dispose: () => { }
			};
		},
		onDidChangeTextEditorSelection(listener: (e: any) => any): any {
			return { dispose: () => { } };
		},
		showTextDocument(document: MockTextDocument): Promise<any> {
			activeTextEditor = { document };
			return Promise.resolve(activeTextEditor);
		}
	},
	commands: {
		executeCommand(command: string, ...args: any[]): Promise<any> {
			if (command === 'workbench.action.closeActiveEditor') {
				if (activeTextEditor) {
					const doc = activeTextEditor.document;
					activeTextEditor = undefined;
					const idx = openDocuments.indexOf(doc);
					if (idx !== -1) {
						openDocuments.splice(idx, 1);
					}
					onDidCloseTextDocumentEmitter.fire(doc);
				}
				return Promise.resolve();
			}
			return Promise.resolve();
		},
		registerCommand(command: string, callback: (...args: any[]) => any): any {
			return { dispose: () => { } };
		},
		registerTextEditorCommand(command: string, callback: (...args: any[]) => any): any { // Added this
			return { dispose: () => { } };
		}
	},
	CodeActionTriggerKind: {
		Invoke: 1,
		Automatic: 2
	},
	Range: MockRange,
	Position: MockPosition,
	TextEdit: MockTextEdit,
	WorkspaceEdit: MockWorkspaceEdit,
	Location: MockLocation,
	MarkdownString: MockMarkdownString,
	Hover: MockHover,
	CompletionItem: MockCompletionItem,
	CompletionItemKind: MockCompletionItemKind,
	SnippetString: MockSnippetString,
	EventEmitter: MockEventEmitter,
	InlayHint: MockInlayHint,
	InlayHintKind: MockInlayHintKind,
	DocumentSymbol: MockDocumentSymbol,
	SymbolKind: MockSymbolKind,
	Uri: MockUri,
	EndOfLine: MockEndOfLine,
	DiagnosticSeverity: MockDiagnosticSeverity,
	Diagnostic: MockDiagnostic,
	FoldingRange: MockFoldingRange,
	FoldingRangeKind: MockFoldingRangeKind,
	CodeActionKind: class MockCodeActionKind {
		static QuickFix = new MockCodeActionKind('quickfix');
		public value: string;
		constructor(value: string) { this.value = value; }
	},
	CodeAction: class MockCodeAction {
		edit?: MockWorkspaceEdit;
		diagnostics?: MockDiagnostic[];
		isPreferred?: boolean;
		public title: string;
		public kind?: MockCodeActionKind;
		constructor(title: string, kind?: MockCodeActionKind) {
			this.title = title;
			this.kind = kind;
		}
	},
	CodeLens: MockCodeLens,

	languages: {
		createDiagnosticCollection(name: string): MockDiagnosticCollection {
			const collection = new MockDiagnosticCollection();
			diagnosticCollections.push(collection);
			return collection;
		},
		getDiagnostics(uri: MockUri): MockDiagnostic[] {
			// Aggregate from all collections
			let all: MockDiagnostic[] = [];
			for (const collection of diagnosticCollections) {
				all = all.concat(collection.get(uri));
			}
			return all;
		},
		registerFoldingRangeProvider: () => ({ dispose: () => { } }),
		registerDocumentFormattingEditProvider: () => ({ dispose: () => { } }),
		registerDocumentRangeFormattingEditProvider: () => ({ dispose: () => { } }),
		registerDocumentSymbolProvider: () => ({ dispose: () => { } }),
		registerCompletionItemProvider: () => ({ dispose: () => { } }),
		registerHoverProvider: () => ({ dispose: () => { } }),
		registerDefinitionProvider: () => ({ dispose: () => { } }),
		registerReferenceProvider: () => ({ dispose: () => { } }),
		registerRenameProvider: () => ({ dispose: () => { } }),
		registerSignatureHelpProvider: () => ({ dispose: () => { } }),
		registerCodeLensProvider: () => ({ dispose: () => { } }),
		registerCodeActionsProvider: () => ({ dispose: () => { } }),
		registerWorkspaceSymbolProvider: () => ({ dispose: () => { } }),
		registerDocumentHighlightProvider: () => ({ dispose: () => { } }),
		registerCallHierarchyProvider: () => ({ dispose: () => { } }),
		registerInlayHintsProvider: () => ({ dispose: () => { } }),
		createLanguageStatusItem: () => ({ dispose: () => { } })
	},
	ExtensionContext: class {
		subscriptions: any[] = [];
	}
};

// Define activeTextEditor getter
Object.defineProperty(mockVscode.window, 'activeTextEditor', {
	get: () => activeTextEditor
});

// Override require() to return our mock when 'vscode' is imported
const originalRequire = Module.prototype.require;
(Module.prototype.require as any) = function (id: string) {
	if (id === 'vscode') {
		return mockVscode;
	}
	return originalRequire.apply(this, arguments as any);
};
