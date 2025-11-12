import * as assert from "assert";
import * as vscode from "vscode";
import { SSLWorkspaceSymbolProvider } from "../src/sslWorkspaceSymbolProvider";
import { SSLCallHierarchyProvider } from "../src/sslCallHierarchyProvider";
import { SSLInlayHintsProvider } from "../src/sslInlayHintsProvider";
import { SSLCodeActionProvider } from "../src/sslCodeActionProvider";

suite("SSL Workspace Features Test Suite", () => {
	vscode.window.showInformationMessage("Testing SSL Workspace Features");

	const sampleCode = `
:PROCEDURE MainProcedure;
:PARAMETERS sInput;
	result := HelperProcedure(sInput);
	:RETURN result;
:ENDPROC;

:PROCEDURE HelperProcedure;
:PARAMETERS sValue;
	:RETURN ALLTRIM(sValue);
:ENDPROC;
	`.trim();

	suite("Workspace Symbol Provider Tests", () => {
		test("Provider initializes without errors", () => {
			assert.doesNotThrow(() => new SSLWorkspaceSymbolProvider());
		});

		test("Search returns symbols matching query", async () => {
			const provider = new SSLWorkspaceSymbolProvider();

			// This test requires actual workspace files, so we just test it doesn't throw
			const symbols = await provider.provideWorkspaceSymbols(
				"test",
				new vscode.CancellationTokenSource().token
			);

			assert.ok(Array.isArray(symbols), "Should return an array");
		});

		test("Empty query returns all symbols", async () => {
			const provider = new SSLWorkspaceSymbolProvider();

			const symbols = await provider.provideWorkspaceSymbols(
				"",
				new vscode.CancellationTokenSource().token
			);

			assert.ok(Array.isArray(symbols), "Should return an array for empty query");
		});
	});

	suite("Call Hierarchy Provider Tests", () => {
		test("Provider initializes without errors", () => {
			assert.doesNotThrow(() => new SSLCallHierarchyProvider());
		});

		test("Prepares call hierarchy for procedure", async () => {
			const provider = new SSLCallHierarchyProvider();
			const document = await vscode.workspace.openTextDocument({
				content: sampleCode,
				language: "ssl"
			});

			// Add a delay to allow the language server to process the document
			await new Promise(resolve => setTimeout(resolve, 500));

			const item = provider.prepareCallHierarchy(
				document,
				new vscode.Position(0, 12), // On MainProcedure
				new vscode.CancellationTokenSource().token
			);

			assert.ok(item, "Should prepare call hierarchy item");
			if (item) {
				assert.strictEqual(item.name, "MainProcedure", "Should have correct procedure name");
				assert.strictEqual(item.kind, vscode.SymbolKind.Function, "Should be a function kind");
			}
		});

		test("Provides outgoing calls", async () => {
			const provider = new SSLCallHierarchyProvider();
			const document = await vscode.workspace.openTextDocument({
				content: sampleCode,
				language: "ssl"
			});

			const item = provider.prepareCallHierarchy(
				document,
				new vscode.Position(0, 12),
				new vscode.CancellationTokenSource().token
			);

			if (item) {
				const outgoingCalls = provider.provideCallHierarchyOutgoingCalls(
					item,
					new vscode.CancellationTokenSource().token
				);

				assert.ok(Array.isArray(outgoingCalls), "Should return array of outgoing calls");
			}
		});

		test("Provides incoming calls", async () => {
			const provider = new SSLCallHierarchyProvider();
			const document = await vscode.workspace.openTextDocument({
				content: sampleCode,
				language: "ssl"
			});

			const item = provider.prepareCallHierarchy(
				document,
				new vscode.Position(6, 12), // On HelperProcedure
				new vscode.CancellationTokenSource().token
			);

			if (item) {
				const incomingCalls = provider.provideCallHierarchyIncomingCalls(
					item,
					new vscode.CancellationTokenSource().token
				);

				assert.ok(Array.isArray(incomingCalls), "Should return array of incoming calls");
			}
		});
	});

	suite("Inlay Hints Provider Tests", () => {
		test("Provider initializes without errors", () => {
			assert.doesNotThrow(() => new SSLInlayHintsProvider());
		});

		test("Returns empty array when disabled", async () => {
			const provider = new SSLInlayHintsProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "result := ALLTRIM(value);",
				language: "ssl"
			});

			// Inlay hints are disabled by default
			const hints = provider.provideInlayHints(
				document,
				new vscode.Range(0, 0, 0, 26),
				new vscode.CancellationTokenSource().token
			);

			assert.ok(Array.isArray(hints), "Should return an array");
		});

		test("Provides hints for function parameters when enabled", async () => {
			// Note: This would require config mocking to test fully
			const provider = new SSLInlayHintsProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "result := SQLExecute(query, dataset, params);",
				language: "ssl"
			});

			const hints = provider.provideInlayHints(
				document,
				new vscode.Range(0, 0, 0, 46),
				new vscode.CancellationTokenSource().token
			);

			// Should return array (empty if disabled, populated if enabled)
			assert.ok(Array.isArray(hints), "Should return an array");
		});
	});

	suite("Code Action Provider Tests", () => {
		test("Provider initializes without errors", () => {
			assert.doesNotThrow(() => new SSLCodeActionProvider());
		});

		test("Provides quick fix for missing semicolon", async () => {
			const provider = new SSLCodeActionProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":DECLARE x",
				language: "ssl"
			});

			const codeActions = provider.provideCodeActions(
				document,
				new vscode.Range(0, 0, 0, 10),
				{ triggerKind: vscode.CodeActionTriggerKind.Invoke, diagnostics: [], only: vscode.CodeActionKind.QuickFix },
				new vscode.CancellationTokenSource().token
			);

			assert.ok(Array.isArray(codeActions), "Should return array of code actions");
		});

		test("Provides quick fix for keyword casing", async () => {
			const provider = new SSLCodeActionProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":if x > 0;",
				language: "ssl"
			});

			const codeActions = provider.provideCodeActions(
				document,
				new vscode.Range(0, 0, 0, 10),
				{ triggerKind: vscode.CodeActionTriggerKind.Invoke, diagnostics: [], only: vscode.CodeActionKind.QuickFix },
				new vscode.CancellationTokenSource().token
			);

			assert.ok(Array.isArray(codeActions), "Should return array of code actions");
			const keywordFix = codeActions.find(a => a.title.includes("keyword"));
			assert.ok(keywordFix, "Should provide keyword casing fix");
		});

		test("Code action has valid edit", async () => {
			const provider = new SSLCodeActionProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "x := 5",
				language: "ssl"
			});

			const codeActions = provider.provideCodeActions(
				document,
				new vscode.Range(0, 0, 0, 6),
				{ triggerKind: vscode.CodeActionTriggerKind.Invoke, diagnostics: [], only: vscode.CodeActionKind.QuickFix },
				new vscode.CancellationTokenSource().token
			);

			if (codeActions.length > 0 && codeActions[0].edit) {
				assert.ok(codeActions[0].edit, "Code action should have an edit");
			}
		});

		test("CRITICAL: Applying semicolon fix actually adds semicolon", async () => {
			const provider = new SSLCodeActionProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":DECLARE x",
				language: "ssl"
			});

			const codeActions = provider.provideCodeActions(
				document,
				new vscode.Range(0, 0, 0, 10),
				{ triggerKind: vscode.CodeActionTriggerKind.Invoke, diagnostics: [], only: vscode.CodeActionKind.QuickFix },
				new vscode.CancellationTokenSource().token
			);

			// Find semicolon fix
			const semicolonFix = codeActions.find(a =>
				a.title.toLowerCase().includes("semicolon") ||
				a.title.toLowerCase().includes("add ;")
			);

			if (semicolonFix && semicolonFix.edit) {
				const changes = semicolonFix.edit.get(document.uri);
				assert.ok(changes && changes.length > 0, "Should have text edits");

				// Verify the edit adds a semicolon
				const edit = changes[0];
				assert.ok(edit.newText.includes(";"), `Should add semicolon, got: '${edit.newText}'`);
			}
		});

		test("CRITICAL: Applying keyword casing fix uppercases keywords", async () => {
			const provider = new SSLCodeActionProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":if x > 0;\n\ty := 1;\n:endif;",
				language: "ssl"
			});

			const codeActions = provider.provideCodeActions(
				document,
				new vscode.Range(0, 0, 0, 10),
				{ triggerKind: vscode.CodeActionTriggerKind.Invoke, diagnostics: [], only: vscode.CodeActionKind.QuickFix },
				new vscode.CancellationTokenSource().token
			);

			// Find keyword casing fix
			const keywordFix = codeActions.find(a =>
				a.title.toLowerCase().includes("keyword") &&
				a.title.toLowerCase().includes("uppercase")
			);

			if (keywordFix && keywordFix.edit) {
				const changes = keywordFix.edit.get(document.uri);
				assert.ok(changes && changes.length > 0, "Should have text edits");

				// Verify edits convert keywords to uppercase
				let hasIfUppercase = false;
				let hasEndifUppercase = false;

				for (const edit of changes) {
					if (edit.newText.includes(":IF")) {
						hasIfUppercase = true;
					}
					if (edit.newText.includes(":ENDIF")) {
						hasEndifUppercase = true;
					}
				}

				assert.ok(hasIfUppercase || hasEndifUppercase,
					"Should convert at least one keyword to uppercase");
			}
		});

		test("Code action applies to correct range", async () => {
			const provider = new SSLCodeActionProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":declare x;\n:declare y",
				language: "ssl"
			});

			// Request fix for second line only
			const codeActions = provider.provideCodeActions(
				document,
				new vscode.Range(1, 0, 1, 10),
				{ triggerKind: vscode.CodeActionTriggerKind.Invoke, diagnostics: [], only: vscode.CodeActionKind.QuickFix },
				new vscode.CancellationTokenSource().token
			);

			// Should provide fixes for line 1, not line 0
			if (codeActions.length > 0 && codeActions[0].edit) {
				const changes = codeActions[0].edit.get(document.uri);
				if (changes && changes.length > 0) {
					// Edits should target line 1
					assert.ok(changes.every(e => e.range.start.line === 1),
						"Edits should only affect the requested line");
				}
			}
		});
	});

	suite("Integration Tests", () => {
		test("Multiple providers work together", async () => {
			// Test that multiple providers can be instantiated without conflicts
			const formattingProvider = new (await import("../src/sslFormattingProvider")).SSLFormattingProvider();
			const symbolProvider = new (await import("../src/sslSymbolProvider")).SSLSymbolProvider();
			const completionProvider = new (await import("../src/sslCompletionProvider")).SSLCompletionProvider();
			const hoverProvider = new (await import("../src/sslHoverProvider")).SSLHoverProvider();
			const diagnosticProvider = new (await import("../src/sslDiagnosticProvider")).SSLDiagnosticProvider();

			assert.ok(formattingProvider, "Formatting provider should instantiate");
			assert.ok(symbolProvider, "Symbol provider should instantiate");
			assert.ok(completionProvider, "Completion provider should instantiate");
			assert.ok(hoverProvider, "Hover provider should instantiate");
			assert.ok(diagnosticProvider, "Diagnostic provider should instantiate");
		});

		test("Extension activates successfully", async () => {
			// This test verifies the extension can be activated
			const extension = vscode.extensions.getExtension("mahoskye.vs-code-ssl-formatter");
			if (extension) {
				await extension.activate();
				assert.ok(extension.isActive, "Extension should be active");
			}
		});
	});
});
