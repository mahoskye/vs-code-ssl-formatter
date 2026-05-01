import * as vscode from 'vscode';
import { SSL_DIAGNOSTIC_SLUGS } from './constants/diagnosticSlugs';

/**
 * Lowercase slug set for O(1) "is this an LSP rule slug?" checks. Used to
 * gate the universal suppression / docs actions so they don't show up on
 * non-LSP diagnostics (legacy native codes use a `ssl-` kebab prefix).
 */
const LSP_SLUG_SET = new Set<string>(SSL_DIAGNOSTIC_SLUGS as readonly string[]);

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
		vscode.CodeActionKind.QuickFix,
		vscode.CodeActionKind.RefactorRewrite
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
				case 'class_instantiation_curly':
					action = createClassInstantiationCurlyFix(document, diagnostic);
					break;
				case 'dot_property_access':
					action = createDotPropertyAccessFix(document, diagnostic);
					break;
				case 'step_spacing':
					action = createStepSpacingFix(document, diagnostic);
					break;
				case 'comment_termination':
					action = createCommentTerminationFix(document, diagnostic);
					break;
				case 'redeclare_is_noop':
					action = createRedeclareIsNoopFix(document, diagnostic);
					break;
				case 'equals_vs_strict_equals':
					action = createEqualsVsStrictEqualsFix(document, diagnostic);
					break;
				case 'parameters_first':
					action = createParametersFirstFix(document, diagnostic);
					break;
				case 'default_after_parameters':
					action = createDefaultAfterParametersFix(document, diagnostic);
					break;
				case 'nested_iif':
					action = createNestedIifRefactor(document, diagnostic);
					break;
			}
			if (action) {
				actions.push(action);
			}

			// Universal helpers offered alongside any specific fix above:
			// - Suppress this rule (line / file)
			// - Open the rule's docs page
			// Only for diagnostics whose code is a known LSP rule slug; we
			// skip the legacy `ssl-*` native codes since the LSP-side
			// suppression directives don't apply to them.
			if (LSP_SLUG_SET.has(code)) {
				actions.push(
					createSuppressLineAction(document, diagnostic, code),
					createSuppressFileAction(document, diagnostic, code),
				);
				const docsAction = createOpenDocsAction(document, diagnostic, code);
				if (docsAction) {
					actions.push(docsAction);
				}
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

// ---- class_instantiation_curly ---------------------------------------------

/**
 * The LSP flags `Foo(...)` calls that should be `Foo{...}` for built-in
 * class instantiation. The diagnostic range covers the call. We rewrite
 * the matched parens in the diagnostic range to curly braces, leaving the
 * argument content untouched.
 */
function createClassInstantiationCurlyFix(
	document: vscode.TextDocument,
	diagnostic: vscode.Diagnostic
): vscode.CodeAction | undefined {
	const original = document.getText(diagnostic.range);
	// Replace the first `(` and the last `)` only — handles `Foo(a, b)` -> `Foo{a, b}`.
	const openIdx = original.indexOf('(');
	const closeIdx = original.lastIndexOf(')');
	if (openIdx < 0 || closeIdx < 0 || closeIdx <= openIdx) {
		return undefined;
	}
	const replacement =
		original.slice(0, openIdx) +
		'{' +
		original.slice(openIdx + 1, closeIdx) +
		'}' +
		original.slice(closeIdx + 1);

	const fix = new vscode.CodeAction(
		"Use curly-brace construction '{}'",
		vscode.CodeActionKind.QuickFix
	);
	fix.edit = new vscode.WorkspaceEdit();
	fix.edit.replace(document.uri, diagnostic.range, replacement);
	fix.diagnostics = [diagnostic];
	fix.isPreferred = true;
	return fix;
}

// ---- dot_property_access ---------------------------------------------------

/**
 * The LSP flags `obj.prop` where SSL expects `obj:prop`. The diagnostic
 * range covers the `.` itself (or the surrounding `obj.prop` expression
 * depending on the LSP build). We replace any `.` in the range with `:`.
 */
function createDotPropertyAccessFix(
	document: vscode.TextDocument,
	diagnostic: vscode.Diagnostic
): vscode.CodeAction | undefined {
	const original = document.getText(diagnostic.range);
	if (!original.includes('.')) {
		return undefined;
	}
	const replacement = original.replace(/\./g, ':');
	const fix = new vscode.CodeAction(
		"Replace '.' with ':' for property access",
		vscode.CodeActionKind.QuickFix
	);
	fix.edit = new vscode.WorkspaceEdit();
	fix.edit.replace(document.uri, diagnostic.range, replacement);
	fix.diagnostics = [diagnostic];
	fix.isPreferred = true;
	return fix;
}

// ---- step_spacing ----------------------------------------------------------

/**
 * `:FOR i := 1 :TO 10 :STEP 2;` requires whitespace before `:STEP`. The LSP
 * flags the `:STEP` token range. Insert a single space immediately before
 * the diagnostic range when there isn't already whitespace there.
 */
function createStepSpacingFix(
	document: vscode.TextDocument,
	diagnostic: vscode.Diagnostic
): vscode.CodeAction | undefined {
	const start = diagnostic.range.start;
	if (start.character === 0) {
		return undefined;
	}
	const lineText = document.lineAt(start.line).text;
	const before = lineText[start.character - 1];
	if (before === ' ' || before === '\t') {
		return undefined;
	}
	const fix = new vscode.CodeAction(
		"Insert space before ':STEP'",
		vscode.CodeActionKind.QuickFix
	);
	fix.edit = new vscode.WorkspaceEdit();
	fix.edit.insert(document.uri, start, ' ');
	fix.diagnostics = [diagnostic];
	fix.isPreferred = true;
	return fix;
}

// ---- comment_termination ---------------------------------------------------

/**
 * SSL comments are terminated with `;`, not the C-style close. The LSP
 * flags the comment range. If the text ends in the C-style close we
 * replace those two characters with `;`; otherwise we append `;` to the
 * end of the diagnostic range.
 */
function createCommentTerminationFix(
	document: vscode.TextDocument,
	diagnostic: vscode.Diagnostic
): vscode.CodeAction | undefined {
	const original = document.getText(diagnostic.range);
	let replacement: string;
	if (original.endsWith('*/')) {
		replacement = original.slice(0, -2) + ';';
	} else if (original.endsWith(';')) {
		return undefined;
	} else {
		replacement = original + ';';
	}
	const fix = new vscode.CodeAction(
		"Terminate comment with ';'",
		vscode.CodeActionKind.QuickFix
	);
	fix.edit = new vscode.WorkspaceEdit();
	fix.edit.replace(document.uri, diagnostic.range, replacement);
	fix.diagnostics = [diagnostic];
	fix.isPreferred = true;
	return fix;
}

// ---- redeclare_is_noop -----------------------------------------------------

/**
 * Re-declaring an already-declared variable with `:DECLARE` is a runtime
 * no-op. The LSP flags the duplicate declaration. Drop the entire line that
 * contains the diagnostic range, including its trailing newline so the file
 * doesn't grow stray blank lines.
 */
function createRedeclareIsNoopFix(
	document: vscode.TextDocument,
	diagnostic: vscode.Diagnostic
): vscode.CodeAction | undefined {
	const line = diagnostic.range.start.line;
	if (line >= document.lineCount) {
		return undefined;
	}
	const removeRange = new vscode.Range(
		new vscode.Position(line, 0),
		new vscode.Position(Math.min(line + 1, document.lineCount), 0)
	);
	const fix = new vscode.CodeAction(
		'Remove redundant :DECLARE',
		vscode.CodeActionKind.QuickFix
	);
	fix.edit = new vscode.WorkspaceEdit();
	fix.edit.delete(document.uri, removeRange);
	fix.diagnostics = [diagnostic];
	fix.isPreferred = true;
	return fix;
}

// ---- equals_vs_strict_equals -----------------------------------------------

/**
 * The LSP flags `=` used for strict-equality string compares (where the
 * SSL `=` operator does prefix matching). The diagnostic range covers the
 * `=` token. Replace with `==`.
 */
function createEqualsVsStrictEqualsFix(
	document: vscode.TextDocument,
	diagnostic: vscode.Diagnostic
): vscode.CodeAction | undefined {
	const original = document.getText(diagnostic.range);
	if (original !== '=') {
		// Defensive — only operate when the range really covers a single `=`.
		return undefined;
	}
	const fix = new vscode.CodeAction(
		"Replace '=' with '==' for exact match",
		vscode.CodeActionKind.QuickFix
	);
	fix.edit = new vscode.WorkspaceEdit();
	fix.edit.replace(document.uri, diagnostic.range, '==');
	fix.diagnostics = [diagnostic];
	fix.isPreferred = true;
	return fix;
}

// ---- parameters_first ------------------------------------------------------

/**
 * SSL requires `:PARAMETERS` to appear before any other statement in a
 * procedure body (or at the top of a script). The LSP flags the
 * out-of-place `:PARAMETERS` line. We move that whole line up to the
 * position immediately after the enclosing `:PROCEDURE` line, or to line 0
 * if the document is a script.
 */
function createParametersFirstFix(
	document: vscode.TextDocument,
	diagnostic: vscode.Diagnostic
): vscode.CodeAction | undefined {
	const targetLine = diagnostic.range.start.line;
	if (targetLine >= document.lineCount) {
		return undefined;
	}
	const lineText = document.lineAt(targetLine).text;
	if (!/^\s*:PARAMETERS\b/i.test(lineText)) {
		return undefined;
	}

	// Find the enclosing :PROCEDURE line (walking up). If none, this is a
	// script; insert at line 0.
	let insertAfter = -1;
	for (let i = targetLine - 1; i >= 0; i--) {
		if (/^\s*:PROCEDURE\b/i.test(document.lineAt(i).text)) {
			insertAfter = i;
			break;
		}
	}
	const insertLine = insertAfter + 1; // 0 for scripts, line-after-:PROCEDURE otherwise
	if (insertLine === targetLine) {
		// Already adjacent — nothing to do.
		return undefined;
	}

	// Build edit: insert the :PARAMETERS line at insertLine, then delete the
	// original. Using a single WorkspaceEdit, edits are applied as a unit
	// without offset-shifting between insert and delete because each edit
	// targets disjoint ranges (the insert is a zero-width position).
	const lineTextWithNewline = lineText + '\n';
	const deleteRange = new vscode.Range(
		new vscode.Position(targetLine, 0),
		new vscode.Position(Math.min(targetLine + 1, document.lineCount), 0)
	);

	const fix = new vscode.CodeAction(
		insertAfter >= 0
			? "Move ':PARAMETERS' immediately after ':PROCEDURE'"
			: "Move ':PARAMETERS' to top of script",
		vscode.CodeActionKind.QuickFix
	);
	fix.edit = new vscode.WorkspaceEdit();
	fix.edit.insert(document.uri, new vscode.Position(insertLine, 0), lineTextWithNewline);
	fix.edit.delete(document.uri, deleteRange);
	fix.diagnostics = [diagnostic];
	fix.isPreferred = true;
	return fix;
}

// ---- default_after_parameters ----------------------------------------------

/**
 * `:DEFAULT` must immediately follow `:PARAMETERS`. The LSP flags
 * out-of-place `:DEFAULT` lines. Move the flagged line to the position
 * directly after the closest preceding `:PARAMETERS` (or after the run of
 * `:DEFAULT` lines already there).
 */
function createDefaultAfterParametersFix(
	document: vscode.TextDocument,
	diagnostic: vscode.Diagnostic
): vscode.CodeAction | undefined {
	const targetLine = diagnostic.range.start.line;
	if (targetLine >= document.lineCount) {
		return undefined;
	}
	const lineText = document.lineAt(targetLine).text;
	if (!/^\s*:DEFAULT\b/i.test(lineText)) {
		return undefined;
	}

	// Walk up to find :PARAMETERS, then walk down past any contiguous
	// :DEFAULT lines so we insert at the END of the run.
	let paramsLine = -1;
	for (let i = targetLine - 1; i >= 0; i--) {
		if (/^\s*:PARAMETERS\b/i.test(document.lineAt(i).text)) {
			paramsLine = i;
			break;
		}
	}
	if (paramsLine === -1) {
		return undefined;
	}
	let insertLine = paramsLine + 1;
	while (
		insertLine < targetLine &&
		/^\s*:DEFAULT\b/i.test(document.lineAt(insertLine).text)
	) {
		insertLine++;
	}
	if (insertLine === targetLine) {
		return undefined;
	}

	const lineTextWithNewline = lineText + '\n';
	const deleteRange = new vscode.Range(
		new vscode.Position(targetLine, 0),
		new vscode.Position(Math.min(targetLine + 1, document.lineCount), 0)
	);

	const fix = new vscode.CodeAction(
		"Move ':DEFAULT' immediately after ':PARAMETERS'",
		vscode.CodeActionKind.QuickFix
	);
	fix.edit = new vscode.WorkspaceEdit();
	fix.edit.insert(document.uri, new vscode.Position(insertLine, 0), lineTextWithNewline);
	fix.edit.delete(document.uri, deleteRange);
	fix.diagnostics = [diagnostic];
	fix.isPreferred = true;
	return fix;
}

// ---- nested_iif (refactor) -------------------------------------------------

/**
 * Convert `lhs := IIF(cond, a, b);` into a block:
 *
 *     :IF cond;
 *         lhs := a;
 *     :ELSE;
 *         lhs := b;
 *     :ENDIF;
 *
 * We only handle the common assignment-RHS case here. For arbitrary
 * expression contexts (a non-trivial transform requiring a temp variable
 * with hand-tuned naming), the action is not offered and the user is left
 * to refactor manually. The diagnostic range is expected to cover the
 * `IIF(...)` expression.
 */
function createNestedIifRefactor(
	document: vscode.TextDocument,
	diagnostic: vscode.Diagnostic
): vscode.CodeAction | undefined {
	const lineNum = diagnostic.range.start.line;
	if (lineNum >= document.lineCount) {
		return undefined;
	}
	const fullLine = document.lineAt(lineNum).text;

	// Match the simple assignment case: optional indent, identifier,
	// `:=`, `IIF(<cond>, <then>, <else>)`, `;`.
	const assignMatch = fullLine.match(
		/^(\s*)([\w:]+(?:\s*:\s*[\w]+)*)\s*:=\s*IIF\s*\(\s*([\s\S]+)\s*\)\s*;\s*$/i
	);
	if (!assignMatch) {
		return undefined;
	}
	const indent = assignMatch[1];
	const lhs = assignMatch[2];
	const argsText = assignMatch[3];

	// Split the IIF arguments at top-level commas.
	const parts = splitTopLevelCommas(argsText);
	if (parts.length !== 3) {
		return undefined;
	}
	const [cond, thenExpr, elseExpr] = parts.map(s => s.trim());
	const inner = indent + '\t';
	const replacement =
		`${indent}:IF ${cond};\n` +
		`${inner}${lhs} := ${thenExpr};\n` +
		`${indent}:ELSE;\n` +
		`${inner}${lhs} := ${elseExpr};\n` +
		`${indent}:ENDIF;`;

	const lineRange = new vscode.Range(
		new vscode.Position(lineNum, 0),
		new vscode.Position(lineNum, fullLine.length)
	);

	const fix = new vscode.CodeAction(
		'Convert IIF to :IF / :ELSE block',
		vscode.CodeActionKind.RefactorRewrite
	);
	fix.edit = new vscode.WorkspaceEdit();
	fix.edit.replace(document.uri, lineRange, replacement);
	fix.diagnostics = [diagnostic];
	fix.isPreferred = false;
	return fix;
}

function splitTopLevelCommas(text: string): string[] {
	const result: string[] = [];
	let depth = 0;
	let inString: string | null = null;
	let buf = '';
	for (let i = 0; i < text.length; i++) {
		const ch = text[i];
		if (inString) {
			buf += ch;
			if (ch === inString) {
				inString = null;
			}
			continue;
		}
		if (ch === '"' || ch === '\'') {
			inString = ch;
			buf += ch;
			continue;
		}
		if (ch === '(' || ch === '[' || ch === '{') {
			depth++;
			buf += ch;
			continue;
		}
		if (ch === ')' || ch === ']' || ch === '}') {
			depth--;
			buf += ch;
			continue;
		}
		if (ch === ',' && depth === 0) {
			result.push(buf);
			buf = '';
			continue;
		}
		buf += ch;
	}
	if (buf.length > 0) {
		result.push(buf);
	}
	return result;
}

// ---- universal: suppression comments ---------------------------------------

/**
 * Builds an SSL `@ssl-disable-next-line <slug>` directive comment, indented
 * to match the diagnostic's line, and inserts it immediately above. The LSP
 * v0.5.0+ honors the directive at parse time so the diagnostic disappears
 * on the next publishDiagnostics round.
 */
function createSuppressLineAction(
	document: vscode.TextDocument,
	diagnostic: vscode.Diagnostic,
	slug: string,
): vscode.CodeAction {
	const line = diagnostic.range.start.line;
	const indent = leadingIndent(document, line);
	const insertText = `${indent}/* @ssl-disable-next-line ${slug}; */\n`;

	const fix = new vscode.CodeAction(
		`Suppress '${slug}' on this line`,
		vscode.CodeActionKind.QuickFix,
	);
	fix.edit = new vscode.WorkspaceEdit();
	fix.edit.insert(document.uri, new vscode.Position(line, 0), insertText);
	fix.diagnostics = [diagnostic];
	fix.isPreferred = false;
	return fix;
}

/**
 * Builds a file-scope SSL `@ssl-disable <slug>` directive comment at the top
 * of the document. We always insert at line 0 column 0 — placing it before
 * any existing content keeps the directive's "applies to entire file"
 * semantics unambiguous.
 */
function createSuppressFileAction(
	document: vscode.TextDocument,
	diagnostic: vscode.Diagnostic,
	slug: string,
): vscode.CodeAction {
	const fix = new vscode.CodeAction(
		`Suppress '${slug}' for this file`,
		vscode.CodeActionKind.QuickFix,
	);
	fix.edit = new vscode.WorkspaceEdit();
	fix.edit.insert(
		document.uri,
		new vscode.Position(0, 0),
		`/* @ssl-disable ${slug}; */\n`,
	);
	fix.diagnostics = [diagnostic];
	fix.isPreferred = false;
	return fix;
}

function leadingIndent(document: vscode.TextDocument, line: number): string {
	if (line < 0 || line >= document.lineCount) {
		return '';
	}
	const text = document.lineAt(line).text;
	const m = text.match(/^(\s*)/);
	return m ? m[1] : '';
}

// ---- universal: open docs page ---------------------------------------------

/**
 * Some diagnostics name a specific element (e.g. "function 'X' has no
 * documented support for named SQL parameters"). When we can pull a
 * canonical SSL identifier out of the diagnostic message, offer to jump
 * to the corresponding ssl-docs page.
 *
 * We don't try to infer element type from the message — the URL form
 * /reference/<unknown>/<X> wouldn't work. Instead we use the rule slug
 * as a hint about which category (functions, keywords, classes,
 * operators) is most likely. If the heuristic doesn't match, we omit the
 * action rather than offer a broken link.
 */
function createOpenDocsAction(
	_document: vscode.TextDocument,
	diagnostic: vscode.Diagnostic,
	slug: string,
): vscode.CodeAction | undefined {
	const elementName = extractElementName(diagnostic.message);
	if (!elementName) {
		return undefined;
	}
	const category = inferCategoryFromSlug(slug);
	if (!category) {
		return undefined;
	}

	// ssl-docs URLs follow content/reference/<category>/<name>.md.
	const url = `https://github.com/mahoskye/starlims-ssl-reference/blob/main/content/reference/${category}/${encodeURIComponent(elementName)}.md`;

	const fix = new vscode.CodeAction(
		`Show docs for '${elementName}'`,
		vscode.CodeActionKind.QuickFix,
	);
	fix.command = {
		title: fix.title,
		command: 'vscode.open',
		arguments: [vscode.Uri.parse(url)],
	};
	fix.diagnostics = [diagnostic];
	// Suppress the empty-edit warning; this action only opens a URL.
	fix.edit = undefined;
	return fix;
}

const _MESSAGE_NAME_RE = /['"`]([A-Za-z_][A-Za-z0-9_]*)['"`]/;

function extractElementName(message: string): string | undefined {
	const m = message.match(_MESSAGE_NAME_RE);
	return m ? m[1] : undefined;
}

function inferCategoryFromSlug(slug: string): string | undefined {
	// Slug name fragments map to ssl-docs categories. Order matters — check
	// the more specific fragments first.
	if (slug.includes('class')) { return 'classes'; }
	if (slug.includes('keyword')) { return 'keywords'; }
	if (slug.includes('operator')) { return 'operators'; }
	if (
		slug.includes('function') ||
		slug.includes('procedure') ||
		slug.includes('execfunction') ||
		slug.includes('sql_param') ||
		slug.includes('udobject_array_in_clause')
	) {
		return 'functions';
	}
	return undefined;
}
