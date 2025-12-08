import * as vscode from "vscode";
import { looksLikeSql } from "./parsing/sqlContext";

/**
 * SSL Code Action Provider
 * Provides quick fixes for common SSL issues
 */
export class SSLCodeActionProvider implements vscode.CodeActionProvider {

	public static readonly providedCodeActionKinds = [
		vscode.CodeActionKind.QuickFix,
		vscode.CodeActionKind.Refactor
	];

	public provideCodeActions(
		document: vscode.TextDocument,
		range: vscode.Range | vscode.Selection,
		context: vscode.CodeActionContext,
		token: vscode.CancellationToken
	): vscode.CodeAction[] {
		const codeActions: vscode.CodeAction[] = [];

		// Analyze context only once
		const analysis = this.analyzeContext(document, range.start);

		// Process each diagnostic
		context.diagnostics.forEach(diagnostic => {
			if (diagnostic.source === "ssl" || !diagnostic.source) {
				codeActions.push(...this.createFixesForDiagnostic(document, diagnostic));
			}
		});

		// Add general code actions
		const line = document.lineAt(range.start.line);

		// Semicolon check: Only if not in string/comment
		if (!analysis.insideString && !analysis.insideComment) {
			if (!line.text.trim().endsWith(";") && this.needsSemicolon(line.text)) {
				const addSemicolon = this.createAddSemicolonAction(document, line);
				if (addSemicolon) {
					codeActions.push(addSemicolon);
				}
			}

			// Keyword casing fix: Only if not in string/comment
			const keywordFix = this.createFixKeywordCasingAction(document, line);
			if (keywordFix) {
				codeActions.push(keywordFix);
			}
		}

		// Format SQL - Check if inside string and looks like SQL
		if (analysis.insideString && analysis.stringContent && analysis.stringRange) {
			if (looksLikeSql(analysis.stringContent)) {
				const formatSqlAction = this.createFormatSqlAction(document, analysis.stringRange);
				if (formatSqlAction) {
					codeActions.push(formatSqlAction);
				}
			}
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
		if (trimmed.startsWith(":") && !trimmed.startsWith("/*")) {
			return true;
		}
		if (trimmed.includes(":=")) {
			return true;
		}
		if (/\w+\([^)]*\)$/.test(trimmed)) {
			return true;
		}
		return false;
	}

	private getIndentation(line: string): string {
		const match = line.match(/^(\s*)/);
		return match ? match[1] : "";
	}

	private createFormatSqlAction(
		document: vscode.TextDocument,
		range: vscode.Range
	): vscode.CodeAction | null {
		const action = new vscode.CodeAction(
			"Format as SQL",
			vscode.CodeActionKind.Refactor
		);
		action.command = {
			command: "ssl.formatSqlRange",
			title: "Format SQL",
			arguments: [document.uri, range]
		};
		return action;
	}

	/**
	 * Scans the document up to the cursor position (and slightly beyond if inside a token)
	 * to determine the context (String, Comment, Code).
	 */
	private analyzeContext(document: vscode.TextDocument, position: vscode.Position): {
		insideString: boolean;
		insideComment: boolean;
		stringContent?: string;
		stringRange?: vscode.Range;
	} {
		const text = document.getText();
		const offset = document.offsetAt(position);

		let state: 'CODE' | 'STRING' | 'COMMENT_LINE' | 'COMMENT_BLOCK' = 'CODE';
		let stringChar: string | null = null;
		let stringStartOffset = -1;

		for (let i = 0; i < text.length; i++) {
			// Early exit if we passed offset and are back in CODE, 
			// meaning we found the relevant token or lack thereof.
			// But we need to handle "cursor is inside the string that hasn't ended yet".

			const char = text[i];
			const nextChar = i + 1 < text.length ? text[i + 1] : '';

			if (state === 'STRING') {
				if (char === stringChar) {
					// End of string
					const stringEndOffset = i + 1; // inclusive of quote
					// Did this string contain our cursor?
					if (stringStartOffset <= offset && offset <= stringEndOffset) {
						// Yes.
						// Extract content (without quotes for SQL checking, or with?)
						// `findStringAtPosition` returned without quotes.
						const content = text.substring(stringStartOffset + 1, i);
						const range = new vscode.Range(
							document.positionAt(stringStartOffset),
							document.positionAt(stringEndOffset)
						);
						return { insideString: true, insideComment: false, stringContent: content, stringRange: range };
					}

					state = 'CODE';
					stringChar = null;
				} else {
					// Check if we passed the cursor while still inside string (and string continues)
					// We only return when we find the END of the string to get full content/range.
				}
				continue;
			}

			if (state === 'COMMENT_LINE') {
				if (char === '\n') {
					if (stringStartOffset <= offset && offset < i) {
						return { insideString: false, insideComment: true };
					}
					state = 'CODE';
				} else {
					// Check if cursor is here
					if (offset === i) {
						return { insideString: false, insideComment: true };
					}
				}
				continue;
			}

			if (state === 'COMMENT_BLOCK') {
				if (char === '*' && nextChar === '/') {
					const endComment = i + 2;
					if (stringStartOffset <= offset && offset < endComment) {
						return { insideString: false, insideComment: true };
					}
					state = 'CODE';
					i++;
				} else {
					if (offset === i) {
						return { insideString: false, insideComment: true };
					}
				}
				continue;
			}

			// CODE state
			if (i > offset) {
				// We passed the cursor and are in CODE. So not in string/comment.
				return { insideString: false, insideComment: false };
			}

			// Comments
			if (char === '/' && nextChar === '/') {
				state = 'COMMENT_LINE';
				stringStartOffset = i; // borrow variable for comment start
				i++;
				continue;
			}
			if (char === '/' && nextChar === '*') {
				state = 'COMMENT_BLOCK';
				stringStartOffset = i;
				i++;
				continue;
			}

			// Strings
			if (char === '"' || char === "'") {
				state = 'STRING';
				stringChar = char;
				stringStartOffset = i;
				continue;
			}
			if (char === '[') {
				// Heuristic: check if preceded by identifier?
				// For code actions, assume [..] is string if we want to format SQL inside it.
				// NOTE: Unlike CodeLens where we want strict ID counting, here we want to offer help for SQL strings.
				// SQL in SSL often uses [ select ... ].
				// So we assume [ starts a string if appropriate.
				// Let's assume [ is string start for simplicity in this context or check simple lookbehind?
				// Simple lookbehind: Is previous char non-identifier?
				// But we are in a loop.
				// Let's assume [ is a string for now, as it covers the SQL usage case.
				state = 'STRING';
				stringChar = ']';
				stringStartOffset = i;
				continue;
			}
		}

		// If loop ends and we are inside string (e.g. unclosed string at EOF or just open), 
		// and cursor was after start...
		if (state === 'STRING') {
			if (offset >= stringStartOffset) {
				const content = text.substring(stringStartOffset + 1);
				const range = new vscode.Range(
					document.positionAt(stringStartOffset),
					document.positionAt(text.length)
				);
				return { insideString: true, insideComment: false, stringContent: content, stringRange: range };
			}
		}
		if (state === 'COMMENT_LINE' || state === 'COMMENT_BLOCK') {
			if (offset >= stringStartOffset) {
				return { insideString: false, insideComment: true };
			}
		}

		return { insideString: false, insideComment: false };
	}
}
