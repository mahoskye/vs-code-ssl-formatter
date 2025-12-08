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

	// Create a set of processed lines to avoid processing the same block multiple times
	// if user has multiple cursors in the same block
	const processedLines = new Set<number>();

	for (const selection of selections) {
		// Get touched lines
		let startLine = selection.start.line;
		let endLine = selection.end.line;

		// If selection ends at character 0 of next line, exclude that line
		if (!selection.isEmpty && selection.end.character === 0 && endLine > startLine) {
			endLine--;
		}

		if (processedLines.has(startLine)) {
			continue;
		}

		// Attempt to expand to full comment block
		const expanded = findEnclosingCommentBlock(document, startLine, endLine);

		// Mark lines as processed
		for (let i = expanded.startLine; i <= expanded.endLine; i++) {
			processedLines.add(i);
		}

		const lines: string[] = [];
		for (let line = expanded.startLine; line <= expanded.endLine; line++) {
			lines.push(document.lineAt(line).text);
		}

		const toggled = toggleCommentLines(lines);

		// Calculate range including the full length of the last line
		const start = new vscode.Position(expanded.startLine, 0);
		const end = new vscode.Position(expanded.endLine, document.lineAt(expanded.endLine).text.length);

		operations.push({
			range: new vscode.Range(start, end),
			text: toggled.lines.join("\n")
		});
	}
	return operations;
}

/**
 * Robustly find enclosing comment block (/* ... ;)
 * Scans upwards for /* and downwards for ;
 */
function findEnclosingCommentBlock(document: vscode.TextDocument, startLine: number, endLine: number): { startLine: number; endLine: number } {
	let newStart = startLine;
	let newEnd = endLine;
	const SCAN_LIMIT = 100; // lines

	// Helper to check line content
	const isCommentStart = (line: number) => document.lineAt(line).text.trim().startsWith("/*");
	const isBlockEnd = (line: number) => document.lineAt(line).text.trim().endsWith(";");

	// 1. Scan Upwards for Start (if current start isn't one)
	if (!isCommentStart(newStart)) {
		for (let i = 1; i <= SCAN_LIMIT; i++) {
			const candidate = startLine - i;
			if (candidate < 0) {
				break;
			}

			// If we hit a semicolon (end of previous block or statement), stop scanning up
			if (isBlockEnd(candidate)) {
				break;
			}

			if (isCommentStart(candidate)) {
				newStart = candidate;
				break;
			}
		}
	}

	// 2. Scan Downwards for End (if current end isn't one)
	if (!isBlockEnd(newEnd)) {
		for (let i = 1; i <= SCAN_LIMIT; i++) {
			const candidate = endLine + i;
			if (candidate >= document.lineCount) {
				break;
			}

			// If we hit a comment start (start of next block), stop scanning down
			if (isCommentStart(candidate)) {
				break;
			}

			if (isBlockEnd(candidate)) {
				newEnd = candidate;
				break;
			}
		}
	}

	// 3. Validation:
	// Only return the expanded range IF it forms a valid comment block (Starts with /* and ends with ;)
	// Otherwise return original range (treat as code to be commented out)
	if (isCommentStart(newStart) && isBlockEnd(newEnd)) {
		return { startLine: newStart, endLine: newEnd };
	}

	return { startLine, endLine };
}
