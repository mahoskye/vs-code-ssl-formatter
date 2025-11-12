import * as vscode from "vscode";

/**
 * SSL Document Highlight Provider
 * Highlights all occurrences of a symbol in the current document
 */
export class SSLDocumentHighlightProvider implements vscode.DocumentHighlightProvider {

	public provideDocumentHighlights(
		document: vscode.TextDocument,
		position: vscode.Position,
		token: vscode.CancellationToken
	): vscode.DocumentHighlight[] {
		const range = document.getWordRangeAtPosition(position);
		if (!range) {
			return [];
		}

		const word = document.getText(range);
		const text = document.getText();
		const lines = text.split("\n");
		const highlights: vscode.DocumentHighlight[] = [];

		// Don't highlight keywords
		if (this.isKeyword(word)) {
			return [];
		}

		// Find all occurrences of the word
		const wordPattern = new RegExp(`\\b${this.escapeRegex(word)}\\b`, "gi");

		for (let i = 0; i < lines.length; i++) {
			const line = lines[i];
			let match: RegExpExecArray | null;

			wordPattern.lastIndex = 0;

			while ((match = wordPattern.exec(line)) !== null) {
				// Skip if in comment
				if (this.isInComment(line, match.index)) {
					continue;
				}

				const startPos = new vscode.Position(i, match.index);
				const endPos = new vscode.Position(i, match.index + word.length);
				const highlightRange = new vscode.Range(startPos, endPos);

				// Determine highlight kind
				let kind = vscode.DocumentHighlightKind.Text;

				// Check if this is a definition (procedure or variable declaration)
				if (this.isDefinition(line, match.index)) {
					kind = vscode.DocumentHighlightKind.Write;
				} else if (this.isAssignment(line, match.index)) {
					kind = vscode.DocumentHighlightKind.Write;
				} else {
					kind = vscode.DocumentHighlightKind.Read;
				}

				highlights.push(new vscode.DocumentHighlight(highlightRange, kind));
			}
		}

		return highlights;
	}

	private isKeyword(word: string): boolean {
		const keywords = [
			"IF", "ELSE", "ELSEIF", "ENDIF",
			"WHILE", "ENDWHILE", "FOR", "TO", "STEP", "NEXT",
			"FOREACH", "IN", "BEGINCASE", "CASE", "OTHERWISE", "ENDCASE",
			"TRY", "CATCH", "FINALLY", "ENDTRY",
			"PROCEDURE", "ENDPROC", "ENDPROCEDURE", "RETURN",
			"DECLARE", "DEFAULT", "PARAMETERS", "PUBLIC",
			"CLASS", "INHERIT", "REGION", "ENDREGION"
		];
		return keywords.includes(word.toUpperCase());
	}

	private isDefinition(line: string, position: number): boolean {
		// Check if this is in a :PROCEDURE, :DECLARE, :PARAMETERS, or :CLASS line
		const beforePos = line.substring(0, position);
		return /^\s*:(PROCEDURE|DECLARE|PARAMETERS|CLASS)\s+/i.test(beforePos);
	}

	private isAssignment(line: string, position: number): boolean {
		// Check if there's an assignment operator after the word
		const afterPos = line.substring(position);
		const wordEnd = afterPos.match(/^\w+/);
		if (!wordEnd) {
			return false;
		}

		const remaining = afterPos.substring(wordEnd[0].length).trim();
		return /^(:=|\+=|-=|\*=|\/=|\^=|%=)/.test(remaining);
	}

	private isInComment(line: string, position: number): boolean {
		const beforePos = line.substring(0, position);
		const commentStart = beforePos.lastIndexOf("/*");

		if (commentStart !== -1) {
			const afterComment = line.substring(commentStart);
			const commentEnd = afterComment.indexOf(";");
			if (commentEnd === -1 || commentStart + commentEnd > position) {
				return true;
			}
		}

		return false;
	}

	private escapeRegex(str: string): string {
		return str.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
	}
}
