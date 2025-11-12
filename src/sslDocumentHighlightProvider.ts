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
				if (this.isInComment(document, i, match.index)) {
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
			"IF", "ELSE", "ENDIF",
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

	private isInComment(document: vscode.TextDocument, lineNumber: number, characterPosition: number): boolean {
		// Scan through the document from the beginning to check if we're inside a multi-line comment
		// SSL comments start with /* and end with ;
		let inComment = false;

		for (let i = 0; i <= lineNumber; i++) {
			const line = document.lineAt(i).text;
			const relevantPart = i === lineNumber ? line.substring(0, characterPosition) : line;

			for (let j = 0; j < relevantPart.length; j++) {
				// Check for comment start
				if (!inComment && j < relevantPart.length - 1 &&
				    relevantPart[j] === '/' && relevantPart[j + 1] === '*') {
					inComment = true;
					j++; // Skip the '*'
				}
				// Check for comment end
				else if (inComment && relevantPart[j] === ';') {
					inComment = false;
				}
			}
		}

		return inComment;
	}

	private escapeRegex(str: string): string {
		return str.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
	}
}
