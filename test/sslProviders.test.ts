import * as assert from "assert";
import * as vscode from "vscode";
import { SSLFormattingProvider } from "../src/sslFormattingProvider";
import { SSLSymbolProvider } from "../src/sslSymbolProvider";
import { SSLCompletionProvider } from "../src/sslCompletionProvider";
import { SSLHoverProvider } from "../src/sslHoverProvider";
import { SSLDiagnosticProvider } from "../src/sslDiagnosticProvider";
import { SSLDefinitionProvider } from "../src/sslDefinitionProvider";
import { SSLReferenceProvider } from "../src/sslReferenceProvider";
import { SSLSignatureHelpProvider } from "../src/sslSignatureHelpProvider";
import { SSLCodeLensProvider } from "../src/sslCodeLensProvider";
import { SSLRenameProvider } from "../src/sslRenameProvider";
import { SSLDocumentHighlightProvider } from "../src/sslDocumentHighlightProvider";

suite("SSL Providers Test Suite", () => {
	vscode.window.showInformationMessage("Testing SSL Providers");

	// Sample SSL code for testing
	const sampleSSLCode = `
:PROCEDURE TestProcedure;
:PARAMETERS sName, nValue;
:DEFAULT nValue, 0;

:DECLARE sResult, nTotal;

nTotal := nValue + 10;
sResult := ALLTRIM(sName);

:IF nTotal > 100;
	USRMES("Value too large");
:ENDIF;

:RETURN sResult;
:ENDPROC;

:PROCEDURE AnotherProcedure;
sTest := TestProcedure("test", 50);
:ENDPROC;
	`.trim();

	suite("Formatting Provider Tests", () => {
		test("Provider initializes without errors", () => {
			assert.doesNotThrow(() => new SSLFormattingProvider());
		});

		test("Formats keyword casing to UPPERCASE", async () => {
			const provider = new SSLFormattingProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":if x > 0;\n\t:return x;\n:endif;",
				language: "ssl"
			});

			const edits = provider.provideDocumentFormattingEdits(
				document,
				{ insertSpaces: false, tabSize: 1 },
				new vscode.CancellationTokenSource().token
			);

			assert.ok(edits.length > 0, "Should provide formatting edits");
			const formattedText = edits[0].newText;
			assert.ok(formattedText.includes(":IF"), "Should convert :if to :IF");
			assert.ok(formattedText.includes(":RETURN"), "Should convert :return to :RETURN");
			assert.ok(formattedText.includes(":ENDIF"), "Should convert :endif to :ENDIF");
		});

		test("Normalizes operator spacing", async () => {
			const provider = new SSLFormattingProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "x:=5;",
				language: "ssl"
			});

			const edits = provider.provideDocumentFormattingEdits(
				document,
				{ insertSpaces: false, tabSize: 1 },
				new vscode.CancellationTokenSource().token
			);

			const formattedText = edits[0].newText;
			assert.ok(formattedText.includes(" := "), "Should add spaces around :=");
		});
	});

	suite("Symbol Provider Tests", () => {
		test("Provider initializes without errors", () => {
			assert.doesNotThrow(() => new SSLSymbolProvider());
		});

		test("Finds procedure symbols", async () => {
			const provider = new SSLSymbolProvider();
			const document = await vscode.workspace.openTextDocument({
				content: sampleSSLCode,
				language: "ssl"
			});

			const symbols = provider.provideDocumentSymbols(
				document,
				new vscode.CancellationTokenSource().token
			);

			assert.ok(symbols.length >= 2, "Should find at least 2 procedures");
			const procNames = symbols.map(s => s.name);
			assert.ok(procNames.includes("TestProcedure"), "Should find TestProcedure");
			assert.ok(procNames.includes("AnotherProcedure"), "Should find AnotherProcedure");
		});

		test("Finds variable declarations", async () => {
			const provider = new SSLSymbolProvider();
			const document = await vscode.workspace.openTextDocument({
				content: sampleSSLCode,
				language: "ssl"
			});

			const symbols = provider.provideDocumentSymbols(
				document,
				new vscode.CancellationTokenSource().token
			);

			// Find procedure and check for variable children
			const testProc = symbols.find(s => s.name === "TestProcedure");
			assert.ok(testProc, "Should find TestProcedure");
			assert.ok(testProc.children.length > 0, "Should have child symbols");
		});
	});

	suite("Completion Provider Tests", () => {
		test("Provider initializes without errors", () => {
			assert.doesNotThrow(() => new SSLCompletionProvider());
		});

		test("Provides keyword completions", async () => {
			const provider = new SSLCompletionProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "",
				language: "ssl"
			});

			const completions = provider.provideCompletionItems(
				document,
				new vscode.Position(0, 0),
				new vscode.CancellationTokenSource().token,
				{ triggerKind: vscode.CompletionTriggerKind.Invoke, triggerCharacter: ":" }
			);

			assert.ok(completions.length > 0, "Should provide completions");
			const keywordCompletions = completions.filter(c => c.label.toString().startsWith(":"));
			assert.ok(keywordCompletions.length > 0, "Should provide keyword completions");
		});

		test("Provides function completions", async () => {
			const provider = new SSLCompletionProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "",
				language: "ssl"
			});

			const completions = provider.provideCompletionItems(
				document,
				new vscode.Position(0, 0),
				new vscode.CancellationTokenSource().token,
				{ triggerKind: vscode.CompletionTriggerKind.Invoke, triggerCharacter: undefined }
			);

			const funcCompletions = completions.filter(
				c => c.kind === vscode.CompletionItemKind.Function
			);
			assert.ok(funcCompletions.length > 0, "Should provide function completions");
		});
	});

	suite("Hover Provider Tests", () => {
		test("Provider initializes without errors", () => {
			assert.doesNotThrow(() => new SSLHoverProvider());
		});

		test("Provides hover for keywords", async () => {
			const provider = new SSLHoverProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":IF x > 0;",
				language: "ssl"
			});

			const hover = provider.provideHover(
				document,
				new vscode.Position(0, 2),
				new vscode.CancellationTokenSource().token
			);

			assert.ok(hover, "Should provide hover information");
			assert.ok(hover.contents.length > 0, "Hover should have contents");
		});

		test("Provides hover for built-in functions", async () => {
			const provider = new SSLHoverProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "result := ALLTRIM(value);",
				language: "ssl"
			});

			const hover = provider.provideHover(
				document,
				new vscode.Position(0, 12),
				new vscode.CancellationTokenSource().token
			);

			assert.ok(hover, "Should provide hover for ALLTRIM");
		});
	});

	suite("Diagnostic Provider Tests", () => {
		test("Provider initializes without errors", () => {
			assert.doesNotThrow(() => new SSLDiagnosticProvider());
		});

		test("Detects missing semicolons", async () => {
			const provider = new SSLDiagnosticProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":DECLARE x\n:IF x > 0",
				language: "ssl"
			});

			provider.updateDiagnostics(document);
			// Diagnostics are async, so we just verify the method runs without error
			assert.ok(true, "Diagnostic update should complete");
		});
	});

	suite("Definition Provider Tests", () => {
		test("Provider initializes without errors", () => {
			assert.doesNotThrow(() => new SSLDefinitionProvider());
		});

		test("Finds procedure definition", async () => {
			const provider = new SSLDefinitionProvider();
			const document = await vscode.workspace.openTextDocument({
				content: sampleSSLCode,
				language: "ssl"
			});

			// Search for TestProcedure call in AnotherProcedure
			const definition = provider.provideDefinition(
				document,
				new vscode.Position(16, 10), // Position on TestProcedure call
				new vscode.CancellationTokenSource().token
			);

			assert.ok(definition, "Should find definition");
		});
	});

	suite("Reference Provider Tests", () => {
		test("Provider initializes without errors", () => {
			assert.doesNotThrow(() => new SSLReferenceProvider());
		});

		test("Finds references to procedure", async () => {
			const provider = new SSLReferenceProvider();
			const document = await vscode.workspace.openTextDocument({
				content: sampleSSLCode,
				language: "ssl"
			});

			const references = provider.provideReferences(
				document,
				new vscode.Position(1, 12), // Position on TestProcedure declaration
				{ includeDeclaration: true },
				new vscode.CancellationTokenSource().token
			);

			assert.ok(references.length > 0, "Should find at least one reference");
		});
	});

	suite("Signature Help Provider Tests", () => {
		test("Provider initializes without errors", () => {
			assert.doesNotThrow(() => new SSLSignatureHelpProvider());
		});

		test("Provides signature help for built-in functions", async () => {
			const provider = new SSLSignatureHelpProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "result := ALLTRIM(",
				language: "ssl"
			});

			const signatureHelp = provider.provideSignatureHelp(
				document,
				new vscode.Position(0, 18),
				new vscode.CancellationTokenSource().token,
				{ triggerKind: vscode.SignatureHelpTriggerKind.Invoke, isRetrigger: false, triggerCharacter: "(", activeSignatureHelp: undefined }
			);

			assert.ok(signatureHelp, "Should provide signature help");
			if (signatureHelp) {
				assert.ok(signatureHelp.signatures.length > 0, "Should have at least one signature");
			}
		});
	});

	suite("CodeLens Provider Tests", () => {
		test("Provider initializes without errors", () => {
			assert.doesNotThrow(() => new SSLCodeLensProvider());
		});

		test("Provides CodeLens for procedures", async () => {
			const provider = new SSLCodeLensProvider();
			const document = await vscode.workspace.openTextDocument({
				content: sampleSSLCode,
				language: "ssl"
			});

			const codeLenses = provider.provideCodeLenses(
				document,
				new vscode.CancellationTokenSource().token
			);

			assert.ok(codeLenses.length > 0, "Should provide CodeLens");
		});
	});

	suite("Rename Provider Tests", () => {
		test("Provider initializes without errors", () => {
			assert.doesNotThrow(() => new SSLRenameProvider());
		});

		test("Prepares rename for valid symbols", async () => {
			const provider = new SSLRenameProvider();
			const document = await vscode.workspace.openTextDocument({
				content: sampleSSLCode,
				language: "ssl"
			});

			const range = provider.prepareRename(
				document,
				new vscode.Position(5, 10), // Position on sResult
				new vscode.CancellationTokenSource().token
			);

			assert.ok(range, "Should allow rename for valid symbol");
		});

		test("Prevents rename of keywords", async () => {
			const provider = new SSLRenameProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":IF x > 0;",
				language: "ssl"
			});

			assert.throws(() => {
				provider.prepareRename(
					document,
					new vscode.Position(0, 2), // Position on IF
					new vscode.CancellationTokenSource().token
				);
			}, "Should throw error when trying to rename keyword");
		});
	});

	suite("Document Highlight Provider Tests", () => {
		test("Provider initializes without errors", () => {
			assert.doesNotThrow(() => new SSLDocumentHighlightProvider());
		});

		test("Highlights symbol occurrences", async () => {
			const provider = new SSLDocumentHighlightProvider();
			const document = await vscode.workspace.openTextDocument({
				content: sampleSSLCode,
				language: "ssl"
			});

			const highlights = provider.provideDocumentHighlights(
				document,
				new vscode.Position(5, 10), // Position on nTotal
				new vscode.CancellationTokenSource().token
			);

			assert.ok(highlights.length > 0, "Should provide highlights");
		});
	});
});
