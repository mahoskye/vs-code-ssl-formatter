
import * as vscode from "vscode";
import { SSLFormatter, FormattingOptions } from "./formatting/formatter";

/**
 * SSL Formatting Provider
 * Handles document and range formatting for SSL files according to the style guide
 */
export class SSLFormattingProvider implements vscode.DocumentFormattingEditProvider, vscode.DocumentRangeFormattingEditProvider {

	/**
	 * Format the entire document
	 */
	public provideDocumentFormattingEdits(
		document: vscode.TextDocument,
		options: vscode.FormattingOptions,
		token: vscode.CancellationToken
	): vscode.TextEdit[] {
		const fullRange = new vscode.Range(
			document.positionAt(0),
			document.positionAt(document.getText().length)
		);

		return this.formatText(document.getText(), options, fullRange, 0);
	}

	/**
	 * Format a specific range of the document
	 */
	public provideDocumentRangeFormattingEdits(
		document: vscode.TextDocument,
		range: vscode.Range,
		options: vscode.FormattingOptions,
		token: vscode.CancellationToken
	): vscode.TextEdit[] {
		const text = document.getText(range);

		// Calculate initial indent based on start line
		const startLine = document.lineAt(range.start.line);
		let indentLevel = 0;
		if (!options.insertSpaces) {
			// Tabs
			const leading = startLine.text.substring(0, startLine.firstNonWhitespaceCharacterIndex);
			indentLevel = (leading.match(/\t/g) || []).length;
		} else {
			// Spaces
			indentLevel = Math.floor(startLine.firstNonWhitespaceCharacterIndex / options.tabSize);
		}

		return this.formatText(text, options, range, indentLevel);
	}

	/**
	 * Format SSL code according to style guide rules
	 */
	private formatText(
		text: string,
		options: vscode.FormattingOptions,
		range: vscode.Range,
		initialIndentLevel: number
	): vscode.TextEdit[] {
		const extendedOptions = this.getFormatterOptions(options);
		const formatter = new SSLFormatter(extendedOptions);
		const formatted = formatter.format(text, initialIndentLevel);
		return [vscode.TextEdit.replace(range, formatted)];
	}

	/**
	 * Helper to extract and merge configuration options
	 */
	private getFormatterOptions(options: vscode.FormattingOptions): FormattingOptions {
		const config = vscode.workspace.getConfiguration('ssl');

		// Resolve indentSpaces: if not explicitly set by user/workspace, leave undefined
		// so that the formatter can fall back to the document's indentation (e.g. tabs).
		const indentSpaces = (() => {
			const inspect = config.inspect<number>('format.sql.indentSpaces');
			if (inspect && (inspect.globalValue !== undefined || inspect.workspaceValue !== undefined || inspect.workspaceFolderValue !== undefined)) {
				return config.get<number>('format.sql.indentSpaces');
			}
			return undefined;
		})();

		return {
			...options,
			'ssl.format.wrapLength': config.get<number>('format.wrapLength', 90),
			'ssl.format.sql.enabled': config.get<boolean>('format.sql.enabled', false),
			'ssl.format.sql.keywordCase': config.get<string>('format.sql.keywordCase', 'upper'),
			'ssl.format.sql.indentSpaces': indentSpaces,
			'ssl.format.sql.style': config.get<string>('format.sql.style', 'canonicalCompact')
		};
	}
}
