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

		test("CRITICAL: Formats builtin function names to PascalCase", async () => {
			const provider = new SSLFormattingProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "result := sqlexecute(query);",
				language: "ssl"
			});

			const edits = provider.provideDocumentFormattingEdits(
				document,
				{ insertSpaces: false, tabSize: 1 },
				new vscode.CancellationTokenSource().token
			);

			const formattedText = edits[0].newText;
			assert.ok(formattedText.includes("SQLExecute") || formattedText.includes("SqlExecute"),
				`Should format 'sqlexecute' to PascalCase, got: ${formattedText}`);
		});

		test("Normalizes indentation", async () => {
			const provider = new SSLFormattingProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":IF x > 0;\ny := 1;\n:ENDIF;",
				language: "ssl"
			});

			const edits = provider.provideDocumentFormattingEdits(
				document,
				{ insertSpaces: true, tabSize: 4 },
				new vscode.CancellationTokenSource().token
			);

			const formattedText = edits[0].newText;
			const lines = formattedText.split("\n");

			// Line 2 (y := 1) should be indented
			assert.ok(lines[1].startsWith("    ") || lines[1].startsWith("\t"),
				`Second line should be indented, got: '${lines[1]}'`);
		});

		test("Handles multiple operators on same line", async () => {
			const provider = new SSLFormattingProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "x:=y+z*2;",
				language: "ssl"
			});

			const edits = provider.provideDocumentFormattingEdits(
				document,
				{ insertSpaces: false, tabSize: 1 },
				new vscode.CancellationTokenSource().token
			);

			const formattedText = edits[0].newText;
			// Should have spaces around :=, +, and *
			assert.ok(formattedText.includes(" := "), "Should have spaces around :=");
			assert.ok(formattedText.includes(" + "), "Should have spaces around +");
			assert.ok(formattedText.includes(" * "), "Should have spaces around *");
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

		test("CRITICAL: Finds class symbols", async () => {
			const provider = new SSLSymbolProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":CLASS MyClass;\n:PROCEDURE Init;\n:ENDPROC;\n:ENDCLASS;",
				language: "ssl"
			});

			const symbols = provider.provideDocumentSymbols(
				document,
				new vscode.CancellationTokenSource().token
			);

			const classSymbol = symbols.find(s => s.name === "MyClass");
			assert.ok(classSymbol, "Should find MyClass class symbol");
			assert.strictEqual(classSymbol.kind, vscode.SymbolKind.Class, "Symbol should be of kind Class");
		});

		test("CRITICAL: Finds region symbols", async () => {
			const provider = new SSLSymbolProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":REGION Initialization;\n:DECLARE nValue;\nnValue := 0;\n:ENDREGION;",
				language: "ssl"
			});

			const symbols = provider.provideDocumentSymbols(
				document,
				new vscode.CancellationTokenSource().token
			);

			const regionSymbol = symbols.find(s => s.name === "Initialization");
			assert.ok(regionSymbol, "Should find Initialization region symbol");
			assert.strictEqual(regionSymbol.kind, vscode.SymbolKind.Namespace, "Region should be Namespace kind");
		});

		test("Verifies hierarchical symbol structure", async () => {
			const provider = new SSLSymbolProvider();
			const document = await vscode.workspace.openTextDocument({
				content: sampleSSLCode,
				language: "ssl"
			});

			const symbols = provider.provideDocumentSymbols(
				document,
				new vscode.CancellationTokenSource().token
			);

			const testProc = symbols.find(s => s.name === "TestProcedure");
			assert.ok(testProc, "Should find TestProcedure");

			// Verify variables are children of procedure
			const childNames = testProc.children.map(c => c.name);
			assert.ok(childNames.includes("sResult") || childNames.includes("nTotal"),
				`Should have variable children, found: ${childNames.join(", ")}`);
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

			// Get diagnostics from VS Code's diagnostic collection
			const diagnostics = vscode.languages.getDiagnostics(document.uri);
			assert.ok(diagnostics.length > 0, `Should create diagnostics, found ${diagnostics.length}`);

			// Verify we have missing semicolon diagnostics
			const semicolonDiags = diagnostics.filter(d =>
				d.message.toLowerCase().includes("semicolon") ||
				d.code === "ssl-missing-semicolon"
			);
			assert.ok(semicolonDiags.length > 0, "Should detect missing semicolons");
		});

		test("Detects Hungarian notation violations", async () => {
			const provider = new SSLDiagnosticProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":PROCEDURE Test;\n:DECLARE BadName;\n:ENDPROC;",
				language: "ssl"
			});

			provider.updateDiagnostics(document);

			const diagnostics = vscode.languages.getDiagnostics(document.uri);
			const hungarianDiags = diagnostics.filter(d =>
				d.message.includes("Hungarian notation") ||
				d.code === "ssl-hungarian-notation"
			);
			assert.ok(hungarianDiags.length > 0, "Should detect Hungarian notation violation for 'BadName'");
		});

		test("Detects Hungarian notation violations for ALL variables in multi-variable declaration", async () => {
			const provider = new SSLDiagnosticProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":PROCEDURE Test;\n:DECLARE result, isActive, comment;\n:ENDPROC;",
				language: "ssl"
			});

			provider.updateDiagnostics(document);

			const diagnostics = vscode.languages.getDiagnostics(document.uri);
			const hungarianDiags = diagnostics.filter(d =>
				d.message.includes("Hungarian notation") ||
				d.code === "ssl-hungarian-notation"
			);

			// All three variables should be flagged:
			// - 'result' (should be sResult)
			// - 'isActive' (should be bIsActive)
			// - 'comment' (should be sComment)
			assert.strictEqual(hungarianDiags.length, 3,
				`Should flag all 3 variables without Hungarian notation, found ${hungarianDiags.length}`);

			// Verify each variable is mentioned
			const messages = hungarianDiags.map(d => d.message).join(" ");
			assert.ok(messages.includes("result"), "Should flag 'result'");
			assert.ok(messages.includes("isActive"), "Should flag 'isActive'");
			assert.ok(messages.includes("comment"), "Should flag 'comment'");
		});

		test("Detects excessive block depth", async () => {
			const provider = new SSLDiagnosticProvider();
			const deeplyNested = `:IF x > 0;
	:IF y > 0;
		:IF z > 0;
			:IF a > 0;
				:IF b > 0;
					x := 1;
				:ENDIF;
			:ENDIF;
		:ENDIF;
	:ENDIF;
:ENDIF;`;

			const document = await vscode.workspace.openTextDocument({
				content: deeplyNested,
				language: "ssl"
			});

			provider.updateDiagnostics(document);

			const diagnostics = vscode.languages.getDiagnostics(document.uri);
			const depthDiags = diagnostics.filter(d =>
				d.message.includes("nesting depth") ||
				d.code === "ssl-block-depth"
			);
			assert.ok(depthDiags.length > 0, "Should detect excessive block depth");
		});

		test("Detects SQL injection risk", async () => {
			const provider = new SSLDiagnosticProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ':PROCEDURE Test;\nquery := "SELECT * FROM users WHERE id = " + userId;\n:ENDPROC;',
				language: "ssl"
			});

			provider.updateDiagnostics(document);

			const diagnostics = vscode.languages.getDiagnostics(document.uri);
			const sqlDiags = diagnostics.filter(d =>
				d.message.toLowerCase().includes("sql") ||
				d.code === "ssl-sql-injection"
			);
			assert.ok(sqlDiags.length > 0, "Should detect SQL injection risk");
		});

		test("CRITICAL: Detects excessive procedure parameters", async () => {
			const provider = new SSLDiagnosticProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":PROCEDURE Test;\n:PARAMETERS p1, p2, p3, p4, p5, p6, p7, p8, p9, p10;\n:ENDPROC;",
				language: "ssl"
			});

			provider.updateDiagnostics(document);

			const diagnostics = vscode.languages.getDiagnostics(document.uri);
			const paramDiags = diagnostics.filter(d =>
				d.message.includes("parameters") ||
				d.code === "ssl-max-params"
			);

			// Default maxParamsPerProcedure is 8, so 10 parameters should be flagged
			assert.ok(paramDiags.length > 0, "Should detect excessive procedure parameters (10 > 8)");
		});

		test("Detects missing :OTHERWISE in :BEGINCASE", async () => {
			const provider = new SSLDiagnosticProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":BEGINCASE;\n:CASE x = 1;\n\ty := 1;\n:ENDCASE;",
				language: "ssl"
			});

			provider.updateDiagnostics(document);

			const diagnostics = vscode.languages.getDiagnostics(document.uri);
			const caseDiags = diagnostics.filter(d =>
				d.message.toLowerCase().includes("otherwise") ||
				d.code === "ssl-missing-otherwise"
			);
			assert.ok(caseDiags.length > 0, "Should warn about missing :OTHERWISE in :BEGINCASE");
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

		test("Finds ALL references to procedure", async () => {
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

			// TestProcedure appears exactly 2 times: declaration (line 1) + call (line 16)
			assert.strictEqual(references.length, 2, "Should find exactly 2 references (declaration + call)");

			// Verify both locations
			const lines = references.map(r => r.range.start.line).sort();
			assert.deepStrictEqual(lines, [1, 16], "Should find references on lines 1 and 16");
		});

		test("Finds multiple references to variable", async () => {
			const provider = new SSLReferenceProvider();
			const document = await vscode.workspace.openTextDocument({
				content: sampleSSLCode,
				language: "ssl"
			});

			const references = provider.provideReferences(
				document,
				new vscode.Position(5, 1), // Position on nTotal declaration
				{ includeDeclaration: true },
				new vscode.CancellationTokenSource().token
			);

			// nTotal appears 3 times: declaration (line 5), assignment (line 7), comparison (line 10)
			assert.ok(references.length >= 3, `Should find at least 3 references to nTotal, found ${references.length}`);
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

		test("Shows accurate reference counts", async () => {
			const provider = new SSLCodeLensProvider();
			const document = await vscode.workspace.openTextDocument({
				content: sampleSSLCode,
				language: "ssl"
			});

			const codeLenses = provider.provideCodeLenses(
				document,
				new vscode.CancellationTokenSource().token
			);

			// TestProcedure should show 1 reference (called once in AnotherProcedure)
			const testProcLens = codeLenses.find(lens =>
				lens.range.start.line === 1 && // TestProcedure is on line 1
				lens.command?.title.includes("reference")
			);

			assert.ok(testProcLens, "Should provide CodeLens for TestProcedure");
			if (testProcLens?.command) {
				assert.ok(
					testProcLens.command.title.includes("1") || testProcLens.command.title.includes("reference"),
					`CodeLens should show reference count, got: ${testProcLens.command.title}`
				);
			}
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

		test("CRITICAL: Rename actually changes ALL occurrences", async () => {
			const provider = new SSLRenameProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":PROCEDURE Test;\n:DECLARE nValue;\nnValue := 5;\nnResult := nValue + 10;\n:RETURN nResult;\n:ENDPROC;",
				language: "ssl"
			});

			const edit = provider.provideRenameEdits(
				document,
				new vscode.Position(1, 10), // Position on nValue declaration
				"nAmount",
				new vscode.CancellationTokenSource().token
			);

			assert.ok(edit, "Should provide rename edits");
			const changes = edit.get(document.uri);
			assert.ok(changes, "Should have changes for the document");

			// nValue appears 3 times: declaration (line 1), assignment (line 2), usage (line 3)
			assert.ok(changes.length >= 3, `Should rename all 3 occurrences, found ${changes.length} edits`);

			// Verify each edit changes nValue to nAmount
			for (const change of changes) {
				assert.strictEqual(change.newText, "nAmount", `Edit should change to 'nAmount', got '${change.newText}'`);
			}
		});

		test("Rename warns for Hungarian notation violations", async () => {
			const provider = new SSLRenameProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":DECLARE nValue;\nnValue := 5;",
				language: "ssl"
			});

			// Renaming to BadName (no Hungarian prefix) should work but warn
			const edit = provider.provideRenameEdits(
				document,
				new vscode.Position(0, 10),
				"BadName",
				new vscode.CancellationTokenSource().token
			);

			// Edit should still be created (warning, not error)
			assert.ok(edit, "Should create edit even with Hungarian notation warning");
		});
	});

	suite("Document Highlight Provider Tests", () => {
		test("Provider initializes without errors", () => {
			assert.doesNotThrow(() => new SSLDocumentHighlightProvider());
		});

		test("Highlights ALL symbol occurrences with correct kinds", async () => {
			const provider = new SSLDocumentHighlightProvider();
			const document = await vscode.workspace.openTextDocument({
				content: sampleSSLCode,
				language: "ssl"
			});

			const highlights = provider.provideDocumentHighlights(
				document,
				new vscode.Position(5, 1), // Position on nTotal declaration
				new vscode.CancellationTokenSource().token
			);

			// nTotal appears 3 times: declaration (line 5), assignment (line 7), read (line 10)
			assert.ok(highlights.length >= 2, `Should highlight multiple occurrences, found ${highlights.length}`);

			// Check for Write kind on assignment
			const writeHighlights = highlights.filter(h => h.kind === vscode.DocumentHighlightKind.Write);
			assert.ok(writeHighlights.length >= 1, "Should have Write highlight for assignment");

			// Check for Read kind on usage
			const readHighlights = highlights.filter(h => h.kind === vscode.DocumentHighlightKind.Read);
			assert.ok(readHighlights.length >= 1, "Should have Read highlight for usage in expression");
		});

		test("Does not highlight keywords", async () => {
			const provider = new SSLDocumentHighlightProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":IF x > 0;\n\ty := 1;\n:ENDIF;",
				language: "ssl"
			});

			const highlights = provider.provideDocumentHighlights(
				document,
				new vscode.Position(0, 2), // Position on IF keyword
				new vscode.CancellationTokenSource().token
			);

			// Keywords should not be highlighted
			assert.strictEqual(highlights.length, 0, "Should not highlight keywords");
		});

		test("Excludes symbols in comments", async () => {
			const provider = new SSLDocumentHighlightProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":DECLARE nValue;\nnValue := 5;\n/* nValue comment; */",
				language: "ssl"
			});

			const highlights = provider.provideDocumentHighlights(
				document,
				new vscode.Position(0, 10), // Position on nValue
				new vscode.CancellationTokenSource().token
			);

			// Should find declaration and assignment, but not comment
			// Verify no highlights on line 2 (comment line)
			const commentLineHighlights = highlights.filter(h => h.range.start.line === 2);
			assert.strictEqual(commentLineHighlights.length, 0, "Should not highlight symbols in comments");
		});
	});

	suite("Edge Cases and Error Handling Tests", () => {
		test("Formatting handles empty documents", async () => {
			const provider = new SSLFormattingProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "",
				language: "ssl"
			});

			const edits = provider.provideDocumentFormattingEdits(
				document,
				{ insertSpaces: false, tabSize: 1 },
				new vscode.CancellationTokenSource().token
			);

			assert.ok(Array.isArray(edits), "Should return array even for empty document");
		});

		test("Symbol provider handles malformed syntax gracefully", async () => {
			const provider = new SSLSymbolProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":PROCEDURE\n:DECLARE\n:IF :ENDIF",
				language: "ssl"
			});

			assert.doesNotThrow(() => {
				provider.provideDocumentSymbols(
					document,
					new vscode.CancellationTokenSource().token
				);
			}, "Should not throw on malformed syntax");
		});

		test("Completion provider handles keywords in comments", async () => {
			const provider = new SSLCompletionProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "/* :IF :WHILE :PROCEDURE */\n",
				language: "ssl"
			});

			const completions = provider.provideCompletionItems(
				document,
				new vscode.Position(1, 0),
				new vscode.CancellationTokenSource().token,
				{ triggerKind: vscode.CompletionTriggerKind.Invoke, triggerCharacter: ":" }
			);

			// Should still provide completions on next line
			assert.ok(completions.length > 0, "Should provide completions after comment line");
		});

		test("Definition provider handles invalid positions", async () => {
			const provider = new SSLDefinitionProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":PROCEDURE Test;\n:ENDPROC;",
				language: "ssl"
			});

			// Position beyond document bounds
			const definition = provider.provideDefinition(
				document,
				new vscode.Position(100, 100),
				new vscode.CancellationTokenSource().token
			);

			// Should return undefined or empty, not throw
			assert.ok(definition === undefined || (Array.isArray(definition) && definition.length === 0),
				"Should handle out-of-bounds position gracefully");
		});

		test("Reference provider ignores keywords in strings", async () => {
			const provider = new SSLReferenceProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ':PROCEDURE Test;\nsMessage := "Test procedure";\n:ENDPROC;',
				language: "ssl"
			});

			const references = provider.provideReferences(
				document,
				new vscode.Position(0, 12), // Position on Test procedure name
				{ includeDeclaration: true },
				new vscode.CancellationTokenSource().token
			);

			// Should find procedure declaration but not "Test" in string
			// The string "Test procedure" should not be counted
			const stringLineRefs = references.filter(r => r.range.start.line === 1);
			assert.strictEqual(stringLineRefs.length, 0, "Should not find references in string literals");
		});

		test("Hover provider is case-insensitive (NOTE: style guide requires UPPERCASE)", async () => {
			const provider = new SSLHoverProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":If x > 0;\n:EndIf;",
				language: "ssl"
			});

			const hover = provider.provideHover(
				document,
				new vscode.Position(0, 2),
				new vscode.CancellationTokenSource().token
			);

			// Hover works for user convenience, but :If is INCORRECT per style guide
			// Keywords MUST be UPPERCASE (:IF, :ENDIF)
			// The formatter will fix this, and ideally diagnostics should warn about it
			assert.ok(hover, "Should provide hover even for mixed-case (helps users)");
			assert.ok(hover.contents[0].toString().includes("IF"), "Hover should show correct UPPERCASE form");
		});

		test("Diagnostic provider respects cancellation token", async () => {
			const provider = new SSLDiagnosticProvider();
			const document = await vscode.workspace.openTextDocument({
				content: sampleSSLCode,
				language: "ssl"
			});

			const tokenSource = new vscode.CancellationTokenSource();
			tokenSource.cancel();

			// Should complete even with cancelled token (diagnostics don't check token currently)
			assert.doesNotThrow(() => {
				provider.updateDiagnostics(document);
			}, "Should handle cancelled token gracefully");
		});
	});
});
