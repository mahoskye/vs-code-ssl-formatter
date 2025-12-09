import * as vscode from 'vscode';

export class SSLCodeActionProvider implements vscode.CodeActionProvider {

	public static readonly providedCodeActionKinds = [
		vscode.CodeActionKind.QuickFix
	];

	public provideCodeActions(document: vscode.TextDocument, range: vscode.Range | vscode.Selection, context: vscode.CodeActionContext, token: vscode.CancellationToken): vscode.CodeAction[] {
		const actions: vscode.CodeAction[] = [];

		for (const diagnostic of context.diagnostics) {
			if (diagnostic.code === 'ssl-invalid-direct-call') {
				const action = this.createDoProcFix(document, diagnostic);
				if (action) {
					actions.push(action);
				}
			}
		}

		return actions;
	}

	private createDoProcFix(document: vscode.TextDocument, diagnostic: vscode.Diagnostic): vscode.CodeAction | undefined {
		// Diagnostic range is on the Name.
		// We need to parse the full call: Name(arg1, arg2...)

		const lineText = document.lineAt(diagnostic.range.start.line).text;
		const name = document.getText(diagnostic.range);

		// Find start of arguments
		const openParenIndex = lineText.indexOf('(', diagnostic.range.end.character);
		if (openParenIndex === -1) { return undefined; }

		// Find matching closing parenthesis
		let balance = 1;
		let closeParenIndex = -1;
		for (let i = openParenIndex + 1; i < lineText.length; i++) {
			const char = lineText[i];
			if (char === '(') { balance++; }
			else if (char === ')') {
				balance--;
				if (balance === 0) {
					closeParenIndex = i;
					break;
				}
			}
		}

		if (closeParenIndex === -1) { return undefined; }

		// Extact arguments
		const argsContent = lineText.substring(openParenIndex + 1, closeParenIndex).trim();

		// Construct replacement
		// Old: Name(Args)
		// New: DoProc("Name", {Args})
		// Note: DoProc takes an array as the second argument.

		// Handle cases where Args is empty? DoProc("Name") or DoProc("Name", {})
		const replacement = argsContent
			? `DoProc("${name}", {${argsContent}})`
			: `DoProc("${name}")`;

		const fix = new vscode.CodeAction(`Convert to DoProc("${name}", ...)`, vscode.CodeActionKind.QuickFix);
		fix.edit = new vscode.WorkspaceEdit();
		// Replace from start of Name to closing paren
		const fullRange = new vscode.Range(
			diagnostic.range.start.line,
			diagnostic.range.start.character,
			diagnostic.range.start.line,
			closeParenIndex + 1
		);
		fix.edit.replace(document.uri, fullRange, replacement);
		fix.diagnostics = [diagnostic];
		fix.isPreferred = true;

		return fix;
	}
}
