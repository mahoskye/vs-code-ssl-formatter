import * as assert from "assert";
import * as vscode from "vscode";
import { SSLDiagnosticProvider } from "../src/sslDiagnosticProvider";
import { SSLFormattingProvider } from "../src/sslFormattingProvider";

suite("SSL Advanced Diagnostic and Formatting Tests", () => {
	vscode.window.showInformationMessage("Testing Advanced SSL Diagnostics and Formatting");

	suite("Invalid Syntax Detection", () => {
		test("Detects invalid :DECLARE with initialization", async () => {
			const provider = new SSLDiagnosticProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":PROCEDURE Test;\n:DECLARE nValue := 5;\n:ENDPROC;",
				language: "ssl"
			});

			provider.updateDiagnostics(document);

			const diagnostics = vscode.languages.getDiagnostics(document.uri);
			const declareDiags = diagnostics.filter(d =>
				d.code === "ssl-invalid-declare"
			);

			assert.ok(declareDiags.length > 0, "Should detect invalid :DECLARE with initialization");
			assert.ok(declareDiags[0].message.includes("cannot initialize"),
				"Error message should explain that :DECLARE cannot initialize values");
		});

		test("Detects invalid const keyword", async () => {
			const provider = new SSLDiagnosticProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":PROCEDURE Test;\n:DECLARE const nValue;\n:ENDPROC;",
				language: "ssl"
			});

			provider.updateDiagnostics(document);

			const diagnostics = vscode.languages.getDiagnostics(document.uri);
			const constDiags = diagnostics.filter(d =>
				d.code === "ssl-invalid-const"
			);

			assert.ok(constDiags.length > 0, "Should detect invalid 'const' keyword");
			assert.ok(constDiags[0].message.includes("not a valid SSL keyword"),
				"Error message should explain that const is not valid in SSL");
		});

		test("Detects invalid CONST keyword (uppercase)", async () => {
			const provider = new SSLDiagnosticProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":PROCEDURE Test;\n:DECLARE CONST nMaxValue;\n:ENDPROC;",
				language: "ssl"
			});

			provider.updateDiagnostics(document);

			const diagnostics = vscode.languages.getDiagnostics(document.uri);
			const constDiags = diagnostics.filter(d =>
				d.code === "ssl-invalid-const"
			);

			assert.ok(constDiags.length > 0, "Should detect invalid 'CONST' keyword");
		});

		test("Accepts valid :DECLARE without initialization", async () => {
			const provider = new SSLDiagnosticProvider();
			const document = await vscode.workspace.openTextDocument({
				content: ":PROCEDURE Test;\n:DECLARE nValue, sName;\nnValue := 5;\n:ENDPROC;",
				language: "ssl"
			});

			provider.updateDiagnostics(document);

			const diagnostics = vscode.languages.getDiagnostics(document.uri);
			const declareDiags = diagnostics.filter(d =>
				d.code === "ssl-invalid-declare"
			);

			assert.strictEqual(declareDiags.length, 0, "Should not flag valid :DECLARE");
		});
	});

	suite("String Preservation in Formatting", () => {
		test("Preserves strings during keyword formatting", async () => {
			const provider = new SSLFormattingProvider();
			const document = await vscode.workspace.openTextDocument({
				content: 'sMessage := "if you need help, endif the session";',
				language: "ssl"
			});

			const edits = provider.provideDocumentFormattingEdits(
				document,
				{ insertSpaces: false, tabSize: 1 },
				new vscode.CancellationTokenSource().token
			);

			const formattedText = edits[0].newText;
			// String content should NOT be changed to :IF or :ENDIF
			assert.ok(formattedText.includes('"if you need help, endif the session"'),
				`String content should be preserved, got: ${formattedText}`);
			assert.ok(!formattedText.includes(":IF"),
				"Should not convert 'if' inside string to :IF");
		});

		test("Preserves strings during operator spacing", async () => {
			const provider = new SSLFormattingProvider();
			const document = await vscode.workspace.openTextDocument({
				content: 'sPath := "C:/temp/file+backup";',
				language: "ssl"
			});

			const edits = provider.provideDocumentFormattingEdits(
				document,
				{ insertSpaces: false, tabSize: 1 },
				new vscode.CancellationTokenSource().token
			);

			const formattedText = edits[0].newText;
			// The + inside the string should NOT get spaces around it
			assert.ok(formattedText.includes('"C:/temp/file+backup"'),
				`String content should be preserved, got: ${formattedText}`);
		});

		test("Preserves strings during function name formatting", async () => {
			const provider = new SSLFormattingProvider();
			const document = await vscode.workspace.openTextDocument({
				content: 'sText := "Please use alltrim on the value";',
				language: "ssl"
			});

			const edits = provider.provideDocumentFormattingEdits(
				document,
				{ insertSpaces: false, tabSize: 1 },
				new vscode.CancellationTokenSource().token
			);

			const formattedText = edits[0].newText;
			// The word 'alltrim' inside the string should NOT be changed to ALLTRIM or AllTrim
			assert.ok(formattedText.includes('"Please use alltrim on the value"'),
				`String content should be preserved, got: ${formattedText}`);
		});

		test("Formats actual function calls but not strings", async () => {
			const provider = new SSLFormattingProvider();
			const document = await vscode.workspace.openTextDocument({
				content: 'sResult := alltrim("  alltrim removes spaces  ");',
				language: "ssl"
			});

			const edits = provider.provideDocumentFormattingEdits(
				document,
				{ insertSpaces: false, tabSize: 1 },
				new vscode.CancellationTokenSource().token
			);

			const formattedText = edits[0].newText;
			// The function call 'alltrim(' should be formatted
			assert.ok(formattedText.match(/AllTrim|ALLTRIM/),
				"Should format the function call");
			// But the string content should be preserved
			assert.ok(formattedText.includes('"  alltrim removes spaces  "'),
				`String content should be preserved, got: ${formattedText}`);
		});

		test("Preserves quoted paths with operators", async () => {
			const provider = new SSLFormattingProvider();
			const document = await vscode.workspace.openTextDocument({
				content: 'sQuery := "SELECT * FROM table WHERE id=5 AND name<>\'test\'";',
				language: "ssl"
			});

			const edits = provider.provideDocumentFormattingEdits(
				document,
				{ insertSpaces: false, tabSize: 1 },
				new vscode.CancellationTokenSource().token
			);

			const formattedText = edits[0].newText;
			// Operators inside the SQL string should not get spaces
			assert.ok(formattedText.includes('id=5'),
				`Should preserve '=' without spaces in string, got: ${formattedText}`);
			assert.ok(formattedText.includes("name<>'test'"),
				`Should preserve '<>' without spaces in string, got: ${formattedText}`);
		});
	});

	suite("Complex Formatting Scenarios", () => {
		test("Handles mixed valid and invalid declarations", async () => {
			const provider = new SSLDiagnosticProvider();
			const document = await vscode.workspace.openTextDocument({
				content: `:PROCEDURE Test;
:DECLARE nValidVar;
:DECLARE const nInvalidConst;
:DECLARE nAnotherValid;
:DECLARE nInvalidInit := 10;
:ENDPROC;`,
				language: "ssl"
			});

			provider.updateDiagnostics(document);

			const diagnostics = vscode.languages.getDiagnostics(document.uri);
			const constErrors = diagnostics.filter(d => d.code === "ssl-invalid-const");
			const initErrors = diagnostics.filter(d => d.code === "ssl-invalid-declare");

			assert.ok(constErrors.length > 0, "Should detect const keyword");
			assert.ok(initErrors.length > 0, "Should detect initialization in :DECLARE");
		});

		test("Formats code while preserving multiple string types", async () => {
			const provider = new SSLFormattingProvider();
			const document = await vscode.workspace.openTextDocument({
				content: `sDouble := "text with := operator";
sSingle := 'text with if keyword';
result := alltrim(sDouble) + alltrim(sSingle);`,
				language: "ssl"
			});

			const edits = provider.provideDocumentFormattingEdits(
				document,
				{ insertSpaces: false, tabSize: 1 },
				new vscode.CancellationTokenSource().token
			);

			const formattedText = edits[0].newText;
			
			// String contents should be preserved
			assert.ok(formattedText.includes('"text with := operator"'),
				"Double-quoted string should be preserved");
			assert.ok(formattedText.includes("'text with if keyword'"),
				"Single-quoted string should be preserved");
			
			// But actual function calls should be formatted
			assert.ok(formattedText.match(/AllTrim|ALLTRIM/),
				"Function names should be formatted");
		});
	});
});
