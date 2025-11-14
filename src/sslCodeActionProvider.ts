import * as vscode from "vscode";

/**
 * SSL Code Action Provider
 * Provides quick fixes for common SSL issues
 */
export class SSLCodeActionProvider implements vscode.CodeActionProvider {

	public static readonly providedCodeActionKinds = [
		vscode.CodeActionKind.QuickFix
	];

	public provideCodeActions(
		document: vscode.TextDocument,
		range: vscode.Range | vscode.Selection,
		context: vscode.CodeActionContext,
		token: vscode.CancellationToken
	): vscode.CodeAction[] {
		const codeActions: vscode.CodeAction[] = [];

		// Process each diagnostic
		context.diagnostics.forEach(diagnostic => {
			if (diagnostic.source === "ssl" || !diagnostic.source) {
				codeActions.push(...this.createFixesForDiagnostic(document, diagnostic));
			}
		});

		// Add general code actions
		const line = document.lineAt(range.start.line);
		const lineText = line.text;

		// Add semicolon if missing
		if (!lineText.trim().endsWith(";") && this.needsSemicolon(lineText)) {
			const addSemicolon = this.createAddSemicolonAction(document, line);
			if (addSemicolon) {
				codeActions.push(addSemicolon);
			}
		}

		// Fix keyword casing
		const keywordFix = this.createFixKeywordCasingAction(document, line);
		if (keywordFix) {
			codeActions.push(keywordFix);
		}

		return codeActions;
	}

	private createFixesForDiagnostic(
		document: vscode.TextDocument,
		diagnostic: vscode.Diagnostic
	): vscode.CodeAction[] {
		const fixes: vscode.CodeAction[] = [];

		switch (diagnostic.code) {
			case "ssl-missing-semicolon":
				fixes.push(this.createAddSemicolonFixAction(document, diagnostic));
				break;

			case "ssl-hungarian-notation":
				// Could offer to rename with proper prefix
				// For now, just document the issue
				break;

			case "ssl-sql-injection":
				const parameterFix = this.createConvertToParameterizedQueryAction(document, diagnostic);
				if (parameterFix) {
					fixes.push(parameterFix);
				}
				break;

			case "ssl-missing-otherwise":
				fixes.push(this.createAddOtherwiseAction(document, diagnostic));
				break;
		}

		return fixes;
	}

	private createAddSemicolonAction(document: vscode.TextDocument, line: vscode.TextLine): vscode.CodeAction | null {
		const action = new vscode.CodeAction("Add missing semicolon", vscode.CodeActionKind.QuickFix);

		const edit = new vscode.WorkspaceEdit();
		const lineEnd = new vscode.Position(line.lineNumber, line.text.length);
		edit.insert(document.uri, lineEnd, ";");

		action.edit = edit;
		action.isPreferred = true;

		return action;
	}

	private createAddSemicolonFixAction(
		document: vscode.TextDocument,
		diagnostic: vscode.Diagnostic
	): vscode.CodeAction {
		const action = new vscode.CodeAction("Add semicolon", vscode.CodeActionKind.QuickFix);

		const edit = new vscode.WorkspaceEdit();
		const lineEnd = new vscode.Position(diagnostic.range.end.line, diagnostic.range.end.character);
		edit.insert(document.uri, lineEnd, ";");

		action.edit = edit;
		action.diagnostics = [diagnostic];
		action.isPreferred = true;

		return action;
	}

	private createFixKeywordCasingAction(
		document: vscode.TextDocument,
		line: vscode.TextLine
	): vscode.CodeAction | null {
		const keywords = [
			"IF", "ELSE", "ENDIF",
			"WHILE", "ENDWHILE", "FOR", "TO", "STEP", "NEXT",
			"PROCEDURE", "ENDPROC", "RETURN",
			"DECLARE", "PARAMETERS", "DEFAULT"
		];

		for (const keyword of keywords) {
			const pattern = new RegExp(`:${keyword}\\b`, "i");
			const match = line.text.match(pattern);

			if (match && match[0] !== `:${keyword}`) {
				const action = new vscode.CodeAction(
					`Fix keyword casing to :${keyword}`,
					vscode.CodeActionKind.QuickFix
				);

				const edit = new vscode.WorkspaceEdit();
				const startPos = new vscode.Position(line.lineNumber, match.index!);
				const endPos = new vscode.Position(line.lineNumber, match.index! + match[0].length);
				const range = new vscode.Range(startPos, endPos);

				edit.replace(document.uri, range, `:${keyword}`);
				action.edit = edit;

				return action;
			}
		}

		return null;
	}

	private createConvertToParameterizedQueryAction(
		document: vscode.TextDocument,
		diagnostic: vscode.Diagnostic
	): vscode.CodeAction | null {
		const action = new vscode.CodeAction(
			"Convert to parameterized query",
			vscode.CodeActionKind.QuickFix
		);

		action.diagnostics = [diagnostic];

		// This is a complex transformation that would require more context
		// For now, just provide a hint
		action.command = {
			command: "editor.action.showMessage",
			title: "Show parameterized query help",
			arguments: ["Use ?PARAM_NAME? placeholders in your SQL query and pass parameters as the third argument"]
		};

		return action;
	}

	private createAddOtherwiseAction(
		document: vscode.TextDocument,
		diagnostic: vscode.Diagnostic
	): vscode.CodeAction {
		const action = new vscode.CodeAction(
			"Add :OTHERWISE clause",
			vscode.CodeActionKind.QuickFix
		);

		// Find the :ENDCASE for this :BEGINCASE
		const text = document.getText();
		const lines = text.split("\n");
		const startLine = diagnostic.range.start.line;

		let endCaseLine = -1;
		let depth = 1;

		for (let i = startLine + 1; i < lines.length; i++) {
			if (/^\s*:BEGINCASE\b/i.test(lines[i])) {
				depth++;
			} else if (/^\s*:ENDCASE\b/i.test(lines[i])) {
				depth--;
				if (depth === 0) {
					endCaseLine = i;
					break;
				}
			}
		}

		if (endCaseLine !== -1) {
			const edit = new vscode.WorkspaceEdit();
			const insertPos = new vscode.Position(endCaseLine, 0);
			const indent = this.getIndentation(lines[startLine]);
			edit.insert(document.uri, insertPos, `${indent}:OTHERWISE;\n${indent}\t/* Default case;\n`);

			action.edit = edit;
			action.diagnostics = [diagnostic];
		}

		return action;
	}

	private needsSemicolon(lineText: string): boolean {
		const trimmed = lineText.trim();

		// Lines that need semicolons
		if (trimmed.startsWith(":") && !trimmed.startsWith("/*")) {
			return true;
		}

		// Assignment statements
		if (trimmed.includes(":=")) {
			return true;
		}

		// Function calls (heuristic: ends with )
		if (/\w+\([^)]*\)$/.test(trimmed)) {
			return true;
		}

		return false;
	}

	private getIndentation(line: string): string {
		const match = line.match(/^(\s*)/);
		return match ? match[1] : "";
	}
}
