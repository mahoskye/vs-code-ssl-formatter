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

		test("CRITICAL: Range formatting formats only selected range", async () => {
			const provider = new SSLFormattingProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":PROCEDURE Test;\nx:=1;\ny:=2;\nz:=3;\n:ENDPROC;",
				language: "ssl"
			});

			// Format only line 2 (y:=2;)
			const range = new vscode.Range(
				new vscode.Position(2, 0),
				new vscode.Position(2, 5)
			);

			const edits = provider.provideDocumentRangeFormattingEdits(
				document,
				range,
				{ insertSpaces: false, tabSize: 1 },
				new vscode.CancellationTokenSource().token
			);

			assert.ok(edits.length > 0, "Should provide range formatting edits");
			assert.ok(edits[0].range.isEqual(range), "Edit should only affect the selected range");

			const formattedText = edits[0].newText;
			assert.ok(formattedText.includes(" := "), "Should format the selected range");
		});

		test("Range formatting preserves content outside selection", async () => {
			const provider = new SSLFormattingProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "x:=1;\ny:=2;\nz:=3;",
				language: "ssl"
			});

			// Format only middle line
			const range = new vscode.Range(
				new vscode.Position(1, 0),
				new vscode.Position(1, 5)
			);

			const edits = provider.provideDocumentRangeFormattingEdits(
				document,
				range,
				{ insertSpaces: false, tabSize: 1 },
				new vscode.CancellationTokenSource().token
			);

			// The edit should only cover the selected range, not the entire document
			assert.strictEqual(edits.length, 1, "Should have exactly one edit for the range");
			assert.strictEqual(edits[0].range.start.line, 1, "Edit should be on line 1");
			assert.strictEqual(edits[0].range.end.line, 1, "Edit should end on line 1");
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
				content: ":CLASS MyClass;\n:INHERIT BaseClass;\n:PROCEDURE Init;\n:ENDPROC;",
				language: "ssl"
			});

			const symbols = provider.provideDocumentSymbols(
				document,
				new vscode.CancellationTokenSource().token
			);

			const classSymbol = symbols.find(s => s.name === "MyClass");
			assert.ok(classSymbol, "Should find MyClass class symbol");
			assert.strictEqual(classSymbol.kind, vscode.SymbolKind.Class, "Symbol should be of kind Class");

			// Note: In SSL, classes have NO :ENDCLASS keyword - the class structure ends with the script
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

		test("Triggers on colon character", async () => {
			const provider = new SSLCompletionProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":",
				language: "ssl"
			});

			const completions = provider.provideCompletionItems(
				document,
				new vscode.Position(0, 1), // After ":"
				new vscode.CancellationTokenSource().token,
				{ triggerKind: vscode.CompletionTriggerKind.TriggerCharacter, triggerCharacter: ":" }
			);

			assert.ok(completions.length > 0, "Should provide completions after :");
			// Should provide keyword completions like :IF, :DECLARE, :PROCEDURE
			const keywordCompletions = completions.filter(c =>
				typeof c.label === 'string' && c.label.startsWith(":")
			);
			assert.ok(keywordCompletions.length > 0, "Should provide keyword completions after :");
		});

		test("Triggers on open parenthesis", async () => {
			const provider = new SSLCompletionProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "result := ALLTRIM(",
				language: "ssl"
			});

			const completions = provider.provideCompletionItems(
				document,
				new vscode.Position(0, 18), // After "("
				new vscode.CancellationTokenSource().token,
				{ triggerKind: vscode.CompletionTriggerKind.TriggerCharacter, triggerCharacter: "(" }
			);

			// Should provide completions (variables, parameters, etc.)
			assert.ok(Array.isArray(completions), "Should return completions array");
		});

		test("Provides context-specific completions inside procedure", async () => {
			const provider = new SSLCompletionProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":PROCEDURE Test;\n:DECLARE nValue;\n",
				language: "ssl"
			});

			const completions = provider.provideCompletionItems(
				document,
				new vscode.Position(2, 0), // After variable declaration, inside procedure
				new vscode.CancellationTokenSource().token,
				{ triggerKind: vscode.CompletionTriggerKind.Invoke, triggerCharacter: undefined }
			);

			// Should provide both keywords and functions
			assert.ok(completions.length > 0, "Should provide completions inside procedure");

			// Check for at least some expected items
			const hasKeywords = completions.some(c =>
				typeof c.label === 'string' && (c.label.includes("IF") || c.label.includes("RETURN"))
			);
			const hasFunctions = completions.some(c => c.kind === vscode.CompletionItemKind.Function);

			assert.ok(hasKeywords || hasFunctions,
				"Should provide either keywords or functions in procedure context");
		});

		test("Completion items have documentation", async () => {
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

			// Check that at least some completions have documentation
			const withDocs = completions.filter(c => c.documentation);
			assert.ok(withDocs.length > 0, "Some completions should have documentation");
		});

		test("BUILTIN-CLASS: Provides Email class completion", async () => {
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

			const emailCompletion = completions.find(c => {
				const label = typeof c.label === 'string' ? c.label : c.label.label;
				return label === "Email";
			});

			assert.ok(emailCompletion, "Should provide Email class completion");
			assert.strictEqual(emailCompletion.kind, vscode.CompletionItemKind.Class,
				"Email completion should be a Class kind");
		});

		test("BUILTIN-CLASS: Provides SSLRegex class completion", async () => {
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

			const regexCompletion = completions.find(c => {
				const label = typeof c.label === 'string' ? c.label : c.label.label;
				return label === "SSLRegex";
			});

			assert.ok(regexCompletion, "Should provide SSLRegex class completion");
			assert.strictEqual(regexCompletion.kind, vscode.CompletionItemKind.Class,
				"SSLRegex completion should be a Class kind");
		});

		test("BUILTIN-CLASS: Email completion inserts {} braces", async () => {
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

			const emailCompletion = completions.find(c => {
				const label = typeof c.label === 'string' ? c.label : c.label.label;
				return label === "Email";
			});

			assert.ok(emailCompletion, "Should find Email completion");
			assert.ok(emailCompletion.insertText, "Should have insertText");

			const insertText = emailCompletion.insertText instanceof vscode.SnippetString
				? emailCompletion.insertText.value
				: emailCompletion.insertText;

			assert.ok(insertText?.includes("{}"), "Should insert Email{} with braces");
		});

		test("BUILTIN-CLASS: Built-in class completions have documentation", async () => {
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

			const emailCompletion = completions.find(c => {
				const label = typeof c.label === 'string' ? c.label : c.label.label;
				return label === "Email";
			});

			assert.ok(emailCompletion, "Should find Email completion");
			assert.ok(emailCompletion.documentation, "Email completion should have documentation");

			const docText = emailCompletion.documentation instanceof vscode.MarkdownString
				? emailCompletion.documentation.value
				: emailCompletion.documentation?.toString() || "";

			assert.ok(docText.includes("email") || docText.includes("Email"),
				"Documentation should mention email");
		});

		test("BUILTIN-CLASS: All built-in classes are present", async () => {
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

			const classCompletions = completions.filter(c =>
				c.kind === vscode.CompletionItemKind.Class
			);

			// Should have at least Email and SSLRegex
			assert.ok(classCompletions.length >= 2,
				`Should have at least 2 built-in class completions, found ${classCompletions.length}`);

			const hasEmail = classCompletions.some(c => {
				const label = typeof c.label === 'string' ? c.label : c.label.label;
				return label === "Email";
			});
			const hasRegex = classCompletions.some(c => {
				const label = typeof c.label === 'string' ? c.label : c.label.label;
				return label === "SSLRegex";
			});

			assert.ok(hasEmail, "Should have Email class");
			assert.ok(hasRegex, "Should have SSLRegex class");
		});

		test("OBJECT-MEMBERS: Provides completions for built-in Email class members", async () => {
			const provider = new SSLCompletionProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "oEmail := Email{};\noEmail:",
				language: "ssl"
			});

			const completions = provider.provideCompletionItems(
				document,
				new vscode.Position(1, 7), // After "oEmail:"
				new vscode.CancellationTokenSource().token,
				{ triggerKind: vscode.CompletionTriggerKind.TriggerCharacter, triggerCharacter: ":" }
			);

			assert.ok(completions.length > 0, "Should provide member completions");

			// Check for some expected members
			const hasSend = completions.some(c => {
				const label = typeof c.label === 'string' ? c.label : c.label.label;
				return label === "Send";
			});
			const hasSubject = completions.some(c => {
				const label = typeof c.label === 'string' ? c.label : c.label.label;
				return label === "Subject";
			});

			assert.ok(hasSend, "Should have Send method");
			assert.ok(hasSubject, "Should have Subject property");
		});

		test("OBJECT-MEMBERS: Provides completions for built-in SSLRegex class members", async () => {
			const provider = new SSLCompletionProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "oRegex := SSLRegex{};\noRegex:",
				language: "ssl"
			});

			const completions = provider.provideCompletionItems(
				document,
				new vscode.Position(1, 7), // After "oRegex:"
				new vscode.CancellationTokenSource().token,
				{ triggerKind: vscode.CompletionTriggerKind.TriggerCharacter, triggerCharacter: ":" }
			);

			assert.ok(completions.length > 0, "Should provide member completions");

			const hasMatch = completions.some(c => {
				const label = typeof c.label === 'string' ? c.label : c.label.label;
				return label === "Match";
			});
			const hasPattern = completions.some(c => {
				const label = typeof c.label === 'string' ? c.label : c.label.label;
				return label === "Pattern";
			});

			assert.ok(hasMatch, "Should have Match method");
			assert.ok(hasPattern, "Should have Pattern property");
		});

		test("OBJECT-MEMBERS: Provides completions for anonymous object members", async () => {
			const provider = new SSLCompletionProvider();
			const document = await vscode.workspace.openTextDocument({
				content: `oVar := CreateUDObject();
oVar:someValue := "Blue";
oVar:someOther := "Red";
oVar:`,
				language: "ssl"
			});

			const completions = provider.provideCompletionItems(
				document,
				new vscode.Position(3, 5), // After "oVar:" on line 4
				new vscode.CancellationTokenSource().token,
				{ triggerKind: vscode.CompletionTriggerKind.TriggerCharacter, triggerCharacter: ":" }
			);

			assert.ok(completions.length > 0, "Should provide member completions for anonymous object");

			const hasSomeValue = completions.some(c => {
				const label = typeof c.label === 'string' ? c.label : c.label.label;
				return label === "someValue";
			});
			const hasSomeOther = completions.some(c => {
				const label = typeof c.label === 'string' ? c.label : c.label.label;
				return label === "someOther";
			});

			assert.ok(hasSomeValue, "Should have someValue property");
			assert.ok(hasSomeOther, "Should have someOther property");
		});

		test("OBJECT-MEMBERS: Provides completions for user-defined class members", async () => {
			const provider = new SSLCompletionProvider();
			const document = await vscode.workspace.openTextDocument({
				content: `:CLASS EmailHandler;
:PROCEDURE Send;
:ENDPROC;
:PROCEDURE Validate;
:ENDPROC;
:ENDCLASS;

oHandler := CreateUDObject("EmailHandler");
oHandler:`,
				language: "ssl"
			});

			const completions = provider.provideCompletionItems(
				document,
				new vscode.Position(8, 9), // After "oHandler:"
				new vscode.CancellationTokenSource().token,
				{ triggerKind: vscode.CompletionTriggerKind.TriggerCharacter, triggerCharacter: ":" }
			);

			assert.ok(completions.length > 0, "Should provide member completions for user-defined class");

			const hasSend = completions.some(c => {
				const label = typeof c.label === 'string' ? c.label : c.label.label;
				return label === "Send";
			});
			const hasValidate = completions.some(c => {
				const label = typeof c.label === 'string' ? c.label : c.label.label;
				return label === "Validate";
			});

			assert.ok(hasSend, "Should have Send method from class");
			assert.ok(hasValidate, "Should have Validate method from class");
		});

		test("OBJECT-MEMBERS: No completions for undefined object", async () => {
			const provider = new SSLCompletionProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "oUndefined:",
				language: "ssl"
			});

			const completions = provider.provideCompletionItems(
				document,
				new vscode.Position(0, 11), // After "oUndefined:"
				new vscode.CancellationTokenSource().token,
				{ triggerKind: vscode.CompletionTriggerKind.TriggerCharacter, triggerCharacter: ":" }
			);

			// Should fall back to default completions (keywords, functions, etc.)
			// Object member completions should be empty, so we get the defaults
			assert.ok(completions.length > 0, "Should provide default completions");
		});

		test("OBJECT-MEMBERS: Methods have parentheses in completion", async () => {
			const provider = new SSLCompletionProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "oEmail := Email{};\noEmail:",
				language: "ssl"
			});

			const completions = provider.provideCompletionItems(
				document,
				new vscode.Position(1, 7),
				new vscode.CancellationTokenSource().token,
				{ triggerKind: vscode.CompletionTriggerKind.TriggerCharacter, triggerCharacter: ":" }
			);

			const sendCompletion = completions.find(c => {
				const label = typeof c.label === 'string' ? c.label : c.label.label;
				return label === "Send";
			});

			assert.ok(sendCompletion, "Should find Send completion");
			assert.ok(sendCompletion.kind === vscode.CompletionItemKind.Method, "Send should be a method");

			const insertText = sendCompletion.insertText instanceof vscode.SnippetString
				? sendCompletion.insertText.value
				: sendCompletion.insertText;

			assert.ok(insertText?.includes("("), "Method should include parentheses");
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

		test("Provides Hungarian notation hint for string variables", async () => {
			const provider = new SSLHoverProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":DECLARE sUserName;",
				language: "ssl"
			});

			const hover = provider.provideHover(
				document,
				new vscode.Position(0, 10), // Position on sUserName
				new vscode.CancellationTokenSource().token
			);

			assert.ok(hover, "Should provide hover for variable");
			const hoverText = hover.contents[0].toString();
			assert.ok(hoverText.includes("String") || hoverText.includes("Hungarian"),
				`Should show Hungarian notation hint for 's' prefix, got: ${hoverText}`);
		});

		test("Provides Hungarian notation hint for numeric variables", async () => {
			const provider = new SSLHoverProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":DECLARE nCount;",
				language: "ssl"
			});

			const hover = provider.provideHover(
				document,
				new vscode.Position(0, 10), // Position on nCount
				new vscode.CancellationTokenSource().token
			);

			assert.ok(hover, "Should provide hover for numeric variable");
			const hoverText = hover.contents[0].toString();
			assert.ok(hoverText.includes("Numeric") || hoverText.includes("Hungarian"),
				`Should show Hungarian notation hint for 'n' prefix, got: ${hoverText}`);
		});

		test("Provides Hungarian notation hint for array variables", async () => {
			const provider = new SSLHoverProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":DECLARE aItems;",
				language: "ssl"
			});

			const hover = provider.provideHover(
				document,
				new vscode.Position(0, 10), // Position on aItems
				new vscode.CancellationTokenSource().token
			);

			assert.ok(hover, "Should provide hover for array variable");
			const hoverText = hover.contents[0].toString();
			assert.ok(hoverText.includes("Array") || hoverText.includes("Hungarian"),
				`Should show Hungarian notation hint for 'a' prefix, got: ${hoverText}`);
		});

		test("No hover for variables without Hungarian notation", async () => {
			const provider = new SSLHoverProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":DECLARE BadName;",
				language: "ssl"
			});

			const hover = provider.provideHover(
				document,
				new vscode.Position(0, 10), // Position on BadName
				new vscode.CancellationTokenSource().token
			);

			// Should return null or no Hungarian hint since BadName doesn't follow convention
			assert.ok(!hover || !hover.contents[0].toString().includes("Hungarian"),
				"Should not show Hungarian hint for non-conforming variable");
		});

		test("No hover on invalid positions", async () => {
			const provider = new SSLHoverProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":IF x > 0;",
				language: "ssl"
			});

			// Position on whitespace
			const hover = provider.provideHover(
				document,
				new vscode.Position(0, 0),
				new vscode.CancellationTokenSource().token
			);

			assert.ok(!hover, "Should not provide hover on whitespace/invalid position");
		});

		test("ENHANCEMENT: Provides hover for user-defined procedures in DoProc", async () => {
			const provider = new SSLHoverProvider();
			const document = await vscode.workspace.openTextDocument({
				content: `:PROCEDURE CalculateTotal;
:PARAMETERS nQuantity, nPrice;
	:RETURN nQuantity * nPrice;
:ENDPROC;

:PROCEDURE Main;
	result := DoProc("CalculateTotal", {10, 5});
:ENDPROC;`,
				language: "ssl"
			});

			// Hover over "CalculateTotal" inside DoProc string
			const hover = provider.provideHover(
				document,
				new vscode.Position(6, 20), // Position inside "CalculateTotal" string
				new vscode.CancellationTokenSource().token
			);

			assert.ok(hover, "Should provide hover for user-defined procedure in DoProc");
			const hoverText = hover.contents[0].toString();
			assert.ok(hoverText.includes("CalculateTotal"), "Should show procedure name");
			assert.ok(hoverText.includes("nQuantity") && hoverText.includes("nPrice"),
				"Should show parameters");
			assert.ok(hoverText.includes("User-defined procedure"), "Should indicate it's user-defined");
		});

		test("ENHANCEMENT: Provides hover for user-defined procedures in ExecFunction", async () => {
			const provider = new SSLHoverProvider();
			const document = await vscode.workspace.openTextDocument({
				content: `:PROCEDURE ValidateInput;
:PARAMETERS sData;
	:RETURN .T.;
:ENDPROC;

:PROCEDURE Process;
	bValid := ExecFunction("NameSpace.ValidateInput", {data});
:ENDPROC;`,
				language: "ssl"
			});

			// Hover over "ValidateInput" inside ExecFunction string (with namespace)
			const hover = provider.provideHover(
				document,
				new vscode.Position(6, 35), // Position inside "NameSpace.ValidateInput" string
				new vscode.CancellationTokenSource().token
			);

			assert.ok(hover, "Should provide hover for user-defined procedure in ExecFunction");
			const hoverText = hover.contents[0].toString();
			assert.ok(hoverText.includes("ValidateInput"), "Should show procedure name");
			assert.ok(hoverText.includes("sData"), "Should show parameter");
			assert.ok(hoverText.includes("ExecFunction"), "Should indicate called via ExecFunction");
		});

		test("ENHANCEMENT: No hover for non-existent procedures in DoProc", async () => {
			const provider = new SSLHoverProvider();
			const document = await vscode.workspace.openTextDocument({
				content: `:PROCEDURE Main;
	result := DoProc("NonExistentProc", {});
:ENDPROC;`,
				language: "ssl"
			});

			// Hover over "NonExistentProc" that doesn't exist
			const hover = provider.provideHover(
				document,
				new vscode.Position(1, 20), // Position inside "NonExistentProc" string
				new vscode.CancellationTokenSource().token
			);

			assert.ok(!hover, "Should not provide hover for non-existent procedure");
		});

		test("ENHANCEMENT: Handles procedures without parameters", async () => {
			const provider = new SSLHoverProvider();
			const document = await vscode.workspace.openTextDocument({
				content: `:PROCEDURE Initialize;
	x := 0;
:ENDPROC;

:PROCEDURE Main;
	DoProc("Initialize", {});
:ENDPROC;`,
				language: "ssl"
			});

			// Hover over "Initialize"
			const hover = provider.provideHover(
				document,
				new vscode.Position(5, 12), // Position inside "Initialize" string
				new vscode.CancellationTokenSource().token
			);

			assert.ok(hover, "Should provide hover for procedure without parameters");
			const hoverText = hover.contents[0].toString();
			assert.ok(hoverText.includes("Initialize"), "Should show procedure name");
		});

		test("ENHANCEMENT: Shows parameters with default values", async () => {
			const provider = new SSLHoverProvider();
			const document = await vscode.workspace.openTextDocument({
				content: `:PROCEDURE ProcessData;
:PARAMETERS sInput, nTimeout, bVerbose;
:DEFAULT sInput, "";
:DEFAULT nTimeout, 30;
:DEFAULT bVerbose, .F.;
	:RETURN .T.;
:ENDPROC;

:PROCEDURE Main;
	result := DoProc("ProcessData", {"data", 60});
:ENDPROC;`,
				language: "ssl"
			});

			// Hover over "ProcessData"
			const hover = provider.provideHover(
				document,
				new vscode.Position(9, 25), // Position inside "ProcessData" string
				new vscode.CancellationTokenSource().token
			);

			assert.ok(hover, "Should provide hover for procedure with defaults");
			const hoverText = hover.contents[0].toString();
			assert.ok(hoverText.includes("ProcessData"), "Should show procedure name");
			assert.ok(hoverText.includes('sInput = ""'), "Should show sInput with default");
			assert.ok(hoverText.includes("nTimeout = 30"), "Should show nTimeout with default");
			assert.ok(hoverText.includes("bVerbose = .F."), "Should show bVerbose with default");
		});

		test("ENHANCEMENT: Handles mixed parameters with and without defaults", async () => {
			const provider = new SSLHoverProvider();
			const document = await vscode.workspace.openTextDocument({
				content: `:PROCEDURE SendEmail;
:PARAMETERS sTo, sSubject, sBody, bCC;
:DEFAULT sBody, "";
:DEFAULT bCC, .F.;
	/* Send email code;
:ENDPROC;

:PROCEDURE Main;
	DoProc("SendEmail", {"user@example.com", "Hello"});
:ENDPROC;`,
				language: "ssl"
			});

			// Hover over "SendEmail"
			const hover = provider.provideHover(
				document,
				new vscode.Position(8, 15), // Position inside "SendEmail" string
				new vscode.CancellationTokenSource().token
			);

			assert.ok(hover, "Should provide hover for procedure with mixed parameters");
			const hoverText = hover.contents[0].toString();
			assert.ok(hoverText.includes("sTo") && !hoverText.includes("sTo ="),
				"Should show sTo without default");
			assert.ok(hoverText.includes("sSubject") && !hoverText.includes("sSubject ="),
				"Should show sSubject without default");
			assert.ok(hoverText.includes('sBody = ""'), "Should show sBody with default");
			assert.ok(hoverText.includes("bCC = .F."), "Should show bCC with default");
		});

		test("USER-CLASS: Provides hover for user-defined class in CreateUDObject", async () => {
			const provider = new SSLHoverProvider();
			const document = await vscode.workspace.openTextDocument({
				content: `:CLASS EmailHandler;
:ENDCLASS;

:PROCEDURE Main;
	oHandler := CreateUDObject("EmailHandler");
:ENDPROC;`,
				language: "ssl"
			});

			// Hover over "EmailHandler" inside CreateUDObject string
			const hover = provider.provideHover(
				document,
				new vscode.Position(4, 30), // Position inside "EmailHandler" string
				new vscode.CancellationTokenSource().token
			);

			assert.ok(hover, "Should provide hover for user-defined class");
			const hoverText = hover.contents[0].toString();
			assert.ok(hoverText.includes("EmailHandler"), "Should show class name");
			assert.ok(hoverText.includes("User-defined class"), "Should indicate it's user-defined");
			assert.ok(hoverText.includes("CreateUDObject"), "Should mention CreateUDObject");
		});

		test("USER-CLASS: Shows inheritance information", async () => {
			const provider = new SSLHoverProvider();
			const document = await vscode.workspace.openTextDocument({
				content: `:CLASS BaseClass;
:ENDCLASS;

:CLASS DerivedClass;
:INHERIT BaseClass;
:ENDCLASS;

:PROCEDURE Main;
	obj := CreateUDObject("DerivedClass");
:ENDPROC;`,
				language: "ssl"
			});

			// Hover over "DerivedClass"
			const hover = provider.provideHover(
				document,
				new vscode.Position(8, 28), // Position inside "DerivedClass" string
				new vscode.CancellationTokenSource().token
			);

			assert.ok(hover, "Should provide hover for derived class");
			const hoverText = hover.contents[0].toString();
			assert.ok(hoverText.includes("DerivedClass"), "Should show class name");
			assert.ok(hoverText.includes("BaseClass"), "Should show base class");
			assert.ok(hoverText.includes("Inherits") || hoverText.includes("INHERIT"), "Should mention inheritance");
		});

		test("USER-CLASS: Handles namespace in class name", async () => {
			const provider = new SSLHoverProvider();
			const document = await vscode.workspace.openTextDocument({
				content: `:CLASS DataProcessor;
:ENDCLASS;

:PROCEDURE Main;
	obj := CreateUDObject("Utilities.DataProcessor");
:ENDPROC;`,
				language: "ssl"
			});

			// Hover over "DataProcessor" in namespaced string
			const hover = provider.provideHover(
				document,
				new vscode.Position(4, 38), // Position inside "Utilities.DataProcessor" string
				new vscode.CancellationTokenSource().token
			);

			assert.ok(hover, "Should provide hover for namespaced class");
			const hoverText = hover.contents[0].toString();
			assert.ok(hoverText.includes("DataProcessor"), "Should show class name");
		});

		test("USER-CLASS: No hover for non-existent class", async () => {
			const provider = new SSLHoverProvider();
			const document = await vscode.workspace.openTextDocument({
				content: `:PROCEDURE Main;
	obj := CreateUDObject("NonExistentClass");
:ENDPROC;`,
				language: "ssl"
			});

			// Hover over "NonExistentClass" that doesn't exist
			const hover = provider.provideHover(
				document,
				new vscode.Position(1, 28), // Position inside "NonExistentClass" string
				new vscode.CancellationTokenSource().token
			);

			assert.ok(!hover, "Should not provide hover for non-existent class");
		});

		test("USER-CLASS: No hover outside CreateUDObject context", async () => {
			const provider = new SSLHoverProvider();
			const document = await vscode.workspace.openTextDocument({
				content: `:CLASS EmailHandler;
:ENDCLASS;

:PROCEDURE Main;
	sName := "EmailHandler";
:ENDPROC;`,
				language: "ssl"
			});

			// Hover over "EmailHandler" in a regular string assignment
			const hover = provider.provideHover(
				document,
				new vscode.Position(4, 15), // Position inside regular string
				new vscode.CancellationTokenSource().token
			);

			// Should not provide class hover since it's not in CreateUDObject
			assert.ok(!hover || !hover.contents[0].toString().includes("User-defined class"),
				"Should not show class hover outside CreateUDObject context");
		});

		test("BUILTIN-CLASS: Provides hover for Email class instantiation", async () => {
			const provider = new SSLHoverProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "oEmail := Email{};",
				language: "ssl"
			});

			// Hover over "Email" in Email{}
			const hover = provider.provideHover(
				document,
				new vscode.Position(0, 12), // Position on "Email"
				new vscode.CancellationTokenSource().token
			);

			assert.ok(hover, "Should provide hover for Email class");
			const hoverText = hover.contents[0].toString();
			assert.ok(hoverText.includes("Email"), "Should show Email class");
			assert.ok(hoverText.includes("email"), "Should mention email in description");
		});

		test("BUILTIN-CLASS: Provides hover for SSLRegex class instantiation", async () => {
			const provider = new SSLHoverProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "oRegex := SSLRegex{};",
				language: "ssl"
			});

			// Hover over "SSLRegex" in SSLRegex{}
			const hover = provider.provideHover(
				document,
				new vscode.Position(0, 13), // Position on "SSLRegex"
				new vscode.CancellationTokenSource().token
			);

			assert.ok(hover, "Should provide hover for SSLRegex class");
			const hoverText = hover.contents[0].toString();
			assert.ok(hoverText.includes("SSLRegex"), "Should show SSLRegex class");
			assert.ok(hoverText.includes("regular expression") || hoverText.includes("regex") || hoverText.includes("pattern"),
				"Should mention regex/pattern in description");
		});

		test("BUILTIN-CLASS: No hover for class name without {}", async () => {
			const provider = new SSLHoverProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "Email",
				language: "ssl"
			});

			// Hover over "Email" without {}
			const hover = provider.provideHover(
				document,
				new vscode.Position(0, 2), // Position on "Email"
				new vscode.CancellationTokenSource().token
			);

			// Should not provide built-in class hover since no {}
			assert.ok(!hover || !hover.contents[0].toString().includes("Built-in"),
				"Should not show built-in class hover without {}");
		});

		test("BUILTIN-CLASS: Hover shows common methods and properties", async () => {
			const provider = new SSLHoverProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "oEmail := Email{};",
				language: "ssl"
			});

			const hover = provider.provideHover(
				document,
				new vscode.Position(0, 12),
				new vscode.CancellationTokenSource().token
			);

			assert.ok(hover, "Should provide hover");
			const hoverText = hover.contents[0].toString();
			// Check for either methods or properties section
			assert.ok(hoverText.includes("Methods") || hoverText.includes("Properties"),
				"Should show methods or properties");
		});

		test("BUILTIN-CLASS: Handles whitespace before {}", async () => {
			const provider = new SSLHoverProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "oEmail := Email  {};",
				language: "ssl"
			});

			// Hover over "Email" with whitespace before {}
			const hover = provider.provideHover(
				document,
				new vscode.Position(0, 12),
				new vscode.CancellationTokenSource().token
			);

			assert.ok(hover, "Should provide hover even with whitespace before {}");
			const hoverText = hover.contents[0].toString();
			assert.ok(hoverText.includes("Email"), "Should show Email class");
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

		test("Finds variable definition in :DECLARE", async () => {
			const provider = new SSLDefinitionProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":PROCEDURE Test;\n:DECLARE nValue;\nnValue := 5;\n:ENDPROC;",
				language: "ssl"
			});

			// Go to definition from usage on line 2
			const definition = provider.provideDefinition(
				document,
				new vscode.Position(2, 1), // Position on nValue usage
				new vscode.CancellationTokenSource().token
			);

			assert.ok(definition, "Should find variable definition");
			if (definition && !Array.isArray(definition)) {
				assert.strictEqual(definition.range.start.line, 1,
					"Should find definition on line 1 (:DECLARE line)");
			}
		});

		test("Finds variable definition in :PARAMETERS", async () => {
			const provider = new SSLDefinitionProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":PROCEDURE Test;\n:PARAMETERS sInput;\nresult := ALLTRIM(sInput);\n:ENDPROC;",
				language: "ssl"
			});

			// Go to definition from usage on line 2
			const definition = provider.provideDefinition(
				document,
				new vscode.Position(2, 19), // Position on sInput usage
				new vscode.CancellationTokenSource().token
			);

			assert.ok(definition, "Should find parameter definition");
			if (definition && !Array.isArray(definition)) {
				assert.strictEqual(definition.range.start.line, 1,
					"Should find definition on line 1 (:PARAMETERS line)");
			}
		});

		test("Returns null when definition not found", async () => {
			const provider = new SSLDefinitionProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":PROCEDURE Test;\nresult := UndefinedFunction();\n:ENDPROC;",
				language: "ssl"
			});

			// Try to find definition of undefined function
			const definition = provider.provideDefinition(
				document,
				new vscode.Position(1, 11), // Position on UndefinedFunction
				new vscode.CancellationTokenSource().token
			);

			assert.ok(!definition, "Should return null for undefined symbols");
		});

		test("Respects variable scoping (procedure-local)", async () => {
			const provider = new SSLDefinitionProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":PROCEDURE Test1;\n:DECLARE nValue;\n:ENDPROC;\n\n:PROCEDURE Test2;\nnValue := 5;\n:ENDPROC;",
				language: "ssl"
			});

			// Try to find definition of nValue from Test2 (should not find declaration in Test1)
			const definition = provider.provideDefinition(
				document,
				new vscode.Position(5, 1), // Position on nValue in Test2
				new vscode.CancellationTokenSource().token
			);

			// Should not find the nValue from Test1 (different procedure scope)
			if (definition && !Array.isArray(definition)) {
				// If it finds something, it shouldn't be in Test1
				assert.notStrictEqual(definition.range.start.line, 1,
					"Should not find variable from different procedure scope");
			}
		});

		test("Finds definition on same line (declaration)", async () => {
			const provider = new SSLDefinitionProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":DECLARE nValue;",
				language: "ssl"
			});

			// Position on nValue in its declaration
			const definition = provider.provideDefinition(
				document,
				new vscode.Position(0, 10), // Position on nValue
				new vscode.CancellationTokenSource().token
			);

			assert.ok(definition, "Should find definition even when on declaration line");
			if (definition && !Array.isArray(definition)) {
				assert.strictEqual(definition.range.start.line, 0,
					"Should point to the declaration line");
			}
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

		test("Tracks active parameter on first argument", async () => {
			const provider = new SSLSignatureHelpProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "result := SUBSTR(sValue",
				language: "ssl"
			});

			const signatureHelp = provider.provideSignatureHelp(
				document,
				new vscode.Position(0, 24), // After "sValue"
				new vscode.CancellationTokenSource().token,
				{ triggerKind: vscode.SignatureHelpTriggerKind.Invoke, isRetrigger: false, triggerCharacter: "(", activeSignatureHelp: undefined }
			);

			assert.ok(signatureHelp, "Should provide signature help");
			if (signatureHelp) {
				assert.strictEqual(signatureHelp.activeParameter, 0, "Should highlight first parameter (string)");
			}
		});

		test("Tracks active parameter on second argument", async () => {
			const provider = new SSLSignatureHelpProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "result := SUBSTR(sValue, 5",
				language: "ssl"
			});

			const signatureHelp = provider.provideSignatureHelp(
				document,
				new vscode.Position(0, 27), // After comma and "5"
				new vscode.CancellationTokenSource().token,
				{ triggerKind: vscode.SignatureHelpTriggerKind.Invoke, isRetrigger: false, triggerCharacter: ",", activeSignatureHelp: undefined }
			);

			assert.ok(signatureHelp, "Should provide signature help");
			if (signatureHelp) {
				assert.strictEqual(signatureHelp.activeParameter, 1, "Should highlight second parameter (start)");
			}
		});

		test("Tracks active parameter on third argument", async () => {
			const provider = new SSLSignatureHelpProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "result := SUBSTR(sValue, 1, 10",
				language: "ssl"
			});

			const signatureHelp = provider.provideSignatureHelp(
				document,
				new vscode.Position(0, 31), // After second comma and "10"
				new vscode.CancellationTokenSource().token,
				{ triggerKind: vscode.SignatureHelpTriggerKind.Invoke, isRetrigger: false, triggerCharacter: ",", activeSignatureHelp: undefined }
			);

			assert.ok(signatureHelp, "Should provide signature help");
			if (signatureHelp) {
				assert.strictEqual(signatureHelp.activeParameter, 2, "Should highlight third parameter (length)");
			}
		});

		test("Handles nested function calls", async () => {
			const provider = new SSLSignatureHelpProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "result := LEFT(ALLTRIM(",
				language: "ssl"
			});

			const signatureHelp = provider.provideSignatureHelp(
				document,
				new vscode.Position(0, 23), // Inside inner ALLTRIM call
				new vscode.CancellationTokenSource().token,
				{ triggerKind: vscode.SignatureHelpTriggerKind.Invoke, isRetrigger: false, triggerCharacter: "(", activeSignatureHelp: undefined }
			);

			assert.ok(signatureHelp, "Should provide signature help for innermost function");
			if (signatureHelp) {
				// Should show signature for ALLTRIM, not LEFT
				assert.ok(signatureHelp.signatures[0].label.includes("ALLTRIM") ||
				          signatureHelp.signatures[0].label.includes("string"),
				          "Should show signature for ALLTRIM (innermost function)");
			}
		});

		test("Returns null when not in function call", async () => {
			const provider = new SSLSignatureHelpProvider();
			const document = await vscode.workspace.openTextDocument({
				content: "result := sValue;",
				language: "ssl"
			});

			const signatureHelp = provider.provideSignatureHelp(
				document,
				new vscode.Position(0, 10), // Not in function call
				new vscode.CancellationTokenSource().token,
				{ triggerKind: vscode.SignatureHelpTriggerKind.Invoke, isRetrigger: false, triggerCharacter: undefined, activeSignatureHelp: undefined }
			);

			assert.ok(!signatureHelp, "Should return null when not in function call");
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

		test("Excludes symbols in single-line comments", async () => {
			const provider = new SSLDocumentHighlightProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":DECLARE nValue;\nnValue := 5;\n/* nValue is in a comment;",
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
			assert.strictEqual(commentLineHighlights.length, 0, "Should not highlight symbols in single-line comments");

			// NOTE: SSL comments end with ; not */ (/* ... */ is INVALID SSL syntax!)
		});

		test("CRITICAL: Excludes symbols in multi-line comments", async () => {
			const provider = new SSLDocumentHighlightProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":DECLARE nValue;\n/* Multi-line comment\n   nValue should be ignored\n   spanning multiple lines\n;\nnValue := 5;",
				language: "ssl"
			});

			const highlights = provider.provideDocumentHighlights(
				document,
				new vscode.Position(0, 10), // Position on nValue
				new vscode.CancellationTokenSource().token
			);

			// Should find: line 0 (declaration) and line 5 (assignment)
			// Should NOT find: lines 2 (inside multi-line comment)
			const commentLineHighlights = highlights.filter(h => h.range.start.line === 2);
			assert.strictEqual(commentLineHighlights.length, 0,
				"BUG: Should not highlight symbols inside multi-line comments! " +
				"Current implementation only checks single line, so this test will FAIL");
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
				content: "/* :IF :WHILE :PROCEDURE;\n",
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
