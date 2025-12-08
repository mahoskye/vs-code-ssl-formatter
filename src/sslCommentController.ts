import * as vscode from "vscode";
import { toggleCommentLines } from "./utils/commentToggle";

interface CommentOperation {
	range: vscode.Range;
	text: string;
}

export function registerCommentController(context: vscode.ExtensionContext) {
	const disposable = vscode.commands.registerTextEditorCommand("ssl.toggleComment", editor => {
		const operations = buildOperations(editor.document, editor.selections);
		if (!operations.length) {
			return;
		}
		editor.edit(editBuilder => {
			// Apply from bottom to top so earlier edits don't shift later ranges
			operations
				.sort((a, b) => b.range.start.line - a.range.start.line)
				.forEach(op => editBuilder.replace(op.range, op.text));
		});
	});
	context.subscriptions.push(disposable);
}

function buildOperations(document: vscode.TextDocument, selections: readonly vscode.Selection[]): CommentOperation[] {
	const operations: CommentOperation[] = [];
	for (const selection of selections) {
		const rangeLines = getLineRange(document, selection);
		if (!rangeLines) {
			continue;
		}
		const { startLine, endLine } = rangeLines;
		const lines: string[] = [];
		for (let line = startLine; line <= endLine; line++) {
			lines.push(document.lineAt(line).text);
		}
		if (!lines.length) {
			continue;
		}
		const toggled = toggleCommentLines(lines);
		const start = new vscode.Position(startLine, 0);
		const end = new vscode.Position(endLine, document.lineAt(endLine).text.length);
		operations.push({
			range: new vscode.Range(start, end),
			text: toggled.lines.join("\n")
		});
	}
	return operations;
}

function getLineRange(document: vscode.TextDocument, selection: vscode.Selection) {
	let startLine = Math.min(selection.start.line, selection.end.line);
	let endLine = Math.max(selection.start.line, selection.end.line);
	if (!selection.isEmpty && selection.end.character === 0 && selection.end.line > selection.start.line) {
		endLine -= 1;
	}
	if (startLine < 0 || startLine >= document.lineCount) {
		return undefined;
	}
	if (endLine < startLine) {
		endLine = startLine;
	}
	const expanded = expandBlockRange(document, startLine, endLine);
	return expanded;
}

function expandBlockRange(document: vscode.TextDocument, startLine: number, endLine: number) {
	let start = startLine;
	let end = endLine;
	const isBlockStart = (line: number) => document.lineAt(line).text.trim() === "/*";
	const isBlockEnd = (line: number) => document.lineAt(line).text.trim() === ";";
	if (!isBlockStart(start) && start > 0 && document.lineAt(start - 1).text.trim() === "/*") {
		start -= 1;
	}
	if (!isBlockEnd(end) && end + 1 < document.lineCount && document.lineAt(end + 1).text.trim() === ";") {
		end += 1;
	}
	return { startLine: start, endLine: end };
}
