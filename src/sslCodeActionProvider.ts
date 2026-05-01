import * as vscode from 'vscode';

/**
 * Quick-fix code actions for SSL diagnostics.
 *
 * Two parallel dispatch paths:
 *
 * 1. Extension-emitted native diagnostics (kebab-case codes like
 *    `ssl-invalid-direct-call`) — fixes for legacy native checks.
 * 2. LSP-emitted diagnostics (snake_case slugs like `udobject_array_in_clause`,
 *    `keyword_uppercase`) — fixes keyed on the stable rule slugs introduced
 *    by starlims-lsp v0.4.0.
 *
 * Codes from `diagnostic.code` arrive as `string | number | { value }` from
 * vscode-languageclient. We normalize via `getDiagnosticCode`.
 */
export class SSLCodeActionProvider implements vscode.CodeActionProvider {

	public static readonly providedCodeActionKinds = [
		vscode.CodeActionKind.QuickFix
	];

	public provideCodeActions(
		document: vscode.TextDocument,
		_range: vscode.Range | vscode.Selection,
		context: vscode.CodeActionContext,
		_token: vscode.CancellationToken
	): vscode.CodeAction[] {
		const actions: vscode.CodeAction[] = [];

		for (const diagnostic of context.diagnostics) {
			const code = getDiagnosticCode(diagnostic);
			if (!code) {
				continue;
			}

			let action: vscode.CodeAction | undefined;
			switch (code) {
				case 'ssl-invalid-direct-call':
					action = createDoProcFix(document, diagnostic);
					break;
				case 'udobject_array_in_clause':
					action = createUDObjectInClauseFix(document, diagnostic);
					break;
				case 'keyword_uppercase':
					action = createKeywordUppercaseFix(document, diagnostic);
					break;
				case 'not_preferred_operator':
					action = createNotPreferredOperatorFix(document, diagnostic);
					break;
				case 'prefer_exitcase':
					action = createPreferExitCaseFix(document, diagnostic);
					break;
			}
			if (action) {
				actions.push(action);
			}
		}

		return actions;
	}
}

function getDiagnosticCode(diagnostic: vscode.Diagnostic): string | undefined {
	const code = diagnostic.code;
	if (typeof code === 'string') {
		return code;
	}
	if (typeof code === 'number') {
		return String(code);
	}
	if (code && typeof code === 'object' && 'value' in code) {
		const v = (code as { value: string | number }).value;
		return typeof v === 'string' ? v : String(v);
	}
	return undefined;
}

// ---- ssl-invalid-direct-call ------------------------------------------------

function createDoProcFix(
	document: vscode.TextDocument,
	diagnostic: vscode.Diagnostic
): vscode.CodeAction | undefined {
	const lineText = document.lineAt(diagnostic.range.start.line).text;
	const name = document.getText(diagnostic.range);

	const openParenIndex = lineText.indexOf('(', diagnostic.range.end.character);
	if (openParenIndex === -1) { return undefined; }

	let balance = 1;
	let closeParenIndex = -1;
	for (let i = openParenIndex + 1; i < lineText.length; i++) {
		const ch = lineText[i];
		if (ch === '(') { balance++; }
		else if (ch === ')') {
			balance--;
			if (balance === 0) {
				closeParenIndex = i;
				break;
			}
		}
	}
	if (closeParenIndex === -1) { return undefined; }

	const argsContent = lineText.substring(openParenIndex + 1, closeParenIndex).trim();
	const replacement = argsContent
		? `DoProc("${name}", {${argsContent}})`
		: `DoProc("${name}")`;

	const fix = new vscode.CodeAction(
		`Convert to DoProc("${name}", ...)`,
		vscode.CodeActionKind.QuickFix
	);
	fix.edit = new vscode.WorkspaceEdit();
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

// ---- udobject_array_in_clause ----------------------------------------------

/**
 * The LSP flags `?obj:Prop?` placeholders inside a SQL `IN (...)` clause —
 * SSL's array expansion can't traverse a UDObject property reference and
 * raises "The current array has more than 1 dimmension." at runtime.
 *
 * Fix: extract the property to a local variable on the line above and
 * rewrite the placeholder. The diagnostic's range covers the placeholder
 * including the surrounding `?`s; its message embeds the property reference
 * in the form `'?<expr>?'` which we parse out for the rewrite.
 */
function createUDObjectInClauseFix(
	document: vscode.TextDocument,
	diagnostic: vscode.Diagnostic
): vscode.CodeAction | undefined {
	const placeholder = document.getText(diagnostic.range);
	const inner = placeholder.replace(/^\?+|\?+$/g, '');
	if (!inner.includes(':')) {
		return undefined;
	}

	// Derive a local name from the property segment after the last `:`.
	const propName = inner.split(':').pop() ?? 'aLocal';
	const localName = `a${propName.replace(/[^A-Za-z0-9]/g, '')}`;

	const insertLine = diagnostic.range.start.line;
	const lineText = document.lineAt(insertLine).text;
	const indentMatch = lineText.match(/^(\s*)/);
	const indent = indentMatch ? indentMatch[1] : '';

	const declaration = `${indent}:DECLARE ${localName};\n${indent}${localName} := ${inner};\n`;

	const fix = new vscode.CodeAction(
		`Extract '${inner}' to local '${localName}' before SQL call`,
		vscode.CodeActionKind.QuickFix
	);
	fix.edit = new vscode.WorkspaceEdit();
	fix.edit.insert(
		document.uri,
		new vscode.Position(insertLine, 0),
		declaration
	);
	fix.edit.replace(document.uri, diagnostic.range, `?${localName}?`);
	fix.diagnostics = [diagnostic];
	fix.isPreferred = true;
	return fix;
}

// ---- keyword_uppercase ------------------------------------------------------

function createKeywordUppercaseFix(
	document: vscode.TextDocument,
	diagnostic: vscode.Diagnostic
): vscode.CodeAction | undefined {
	const original = document.getText(diagnostic.range);
	const upper = original.toUpperCase();
	if (upper === original) {
		return undefined;
	}

	const fix = new vscode.CodeAction(
		`Uppercase keyword: ${upper}`,
		vscode.CodeActionKind.QuickFix
	);
	fix.edit = new vscode.WorkspaceEdit();
	fix.edit.replace(document.uri, diagnostic.range, upper);
	fix.diagnostics = [diagnostic];
	fix.isPreferred = true;
	return fix;
}

// ---- not_preferred_operator -------------------------------------------------

const NOT_PREFERRED_OPERATOR_REPLACEMENTS: Record<string, string> = {
	'<>': '!=',
	'#': '!=',
};

function createNotPreferredOperatorFix(
	document: vscode.TextDocument,
	diagnostic: vscode.Diagnostic
): vscode.CodeAction | undefined {
	const original = document.getText(diagnostic.range);
	const replacement = NOT_PREFERRED_OPERATOR_REPLACEMENTS[original];
	if (!replacement) {
		return undefined;
	}

	const fix = new vscode.CodeAction(
		`Replace '${original}' with '${replacement}'`,
		vscode.CodeActionKind.QuickFix
	);
	fix.edit = new vscode.WorkspaceEdit();
	fix.edit.replace(document.uri, diagnostic.range, replacement);
	fix.diagnostics = [diagnostic];
	fix.isPreferred = true;
	return fix;
}

// ---- prefer_exitcase --------------------------------------------------------

/**
 * The LSP flags `:CASE` blocks that don't end with `:EXITCASE;`. The diagnostic
 * range is on the `:CASE` keyword that opens the block; we walk forward to find
 * the next `:CASE`/`:OTHERWISE`/`:ENDCASE` and insert `:EXITCASE;` immediately
 * before it (matching the indentation of the body).
 */
function createPreferExitCaseFix(
	document: vscode.TextDocument,
	diagnostic: vscode.Diagnostic
): vscode.CodeAction | undefined {
	const startLine = diagnostic.range.start.line;
	const totalLines = document.lineCount;
	const caseEndPattern = /^\s*:(?:CASE\b|OTHERWISE\b|ENDCASE\b)/i;

	let insertLine = -1;
	for (let i = startLine + 1; i < totalLines; i++) {
		if (caseEndPattern.test(document.lineAt(i).text)) {
			insertLine = i;
			break;
		}
	}
	if (insertLine === -1) {
		return undefined;
	}

	// Match the indentation of a body line in the case block (use the line
	// just above the boundary if it has any content; otherwise fall back to
	// the indentation of the `:CASE` line plus a tab).
	let indent = '';
	for (let i = insertLine - 1; i > startLine; i--) {
		const text = document.lineAt(i).text;
		if (text.trim() !== '') {
			const m = text.match(/^(\s*)/);
			indent = m ? m[1] : '';
			break;
		}
	}
	if (indent === '') {
		const caseIndent = document.lineAt(startLine).text.match(/^(\s*)/);
		indent = (caseIndent ? caseIndent[1] : '') + '\t';
	}

	const fix = new vscode.CodeAction(
		"Insert ':EXITCASE;' before next :CASE/:ENDCASE",
		vscode.CodeActionKind.QuickFix
	);
	fix.edit = new vscode.WorkspaceEdit();
	fix.edit.insert(
		document.uri,
		new vscode.Position(insertLine, 0),
		`${indent}:EXITCASE;\n`
	);
	fix.diagnostics = [diagnostic];
	fix.isPreferred = false;
	return fix;
}
