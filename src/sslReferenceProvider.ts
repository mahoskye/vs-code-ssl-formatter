import * as vscode from "vscode";

/**
 * SSL Reference Provider
 * Provides "Find All References" functionality for procedures and variables
 */
export class SSLReferenceProvider implements vscode.ReferenceProvider {

	public provideReferences(
		document: vscode.TextDocument,
		position: vscode.Position,
		context: vscode.ReferenceContext,
		token: vscode.CancellationToken
	): vscode.Location[] {
		const range = document.getWordRangeAtPosition(position);
		if (!range) {
			return [];
		}

		const word = document.getText(range);
		const text = document.getText();
		const lines = text.split("\n");
		const locations: vscode.Location[] = [];

		// Find all references to the symbol
		const wordPattern = new RegExp(`\\b${this.escapeRegex(word)}\\b`, "gi");

		for (let i = 0; i < lines.length; i++) {
			const line = lines[i];
			let match: RegExpExecArray | null;

			// Reset lastIndex for each line
			wordPattern.lastIndex = 0;

			while ((match = wordPattern.exec(line)) !== null) {
				const startPos = new vscode.Position(i, match.index);
				const endPos = new vscode.Position(i, match.index + word.length);
				const location = new vscode.Location(
					document.uri,
					new vscode.Range(startPos, endPos)
				);
				locations.push(location);
			}
		}

		// Filter out references in comments and strings
		return locations.filter(loc => 
			!this.isInComment(document, loc.range.start) &&
			!this.isInString(document, loc.range.start)
		);
	}

	private escapeRegex(str: string): string {
		return str.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
	}

	private isInString(document: vscode.TextDocument, position: vscode.Position): boolean {
		const line = document.lineAt(position.line).text;
		let inString = false;
		let stringChar: string | null = null;

		for (let i = 0; i < position.character; i++) {
			const char = line[i];
			
			if (!inString && (char === '"' || char === "'")) {
				inString = true;
				stringChar = char;
			} else if (inString && char === stringChar) {
				inString = false;
				stringChar = null;
			}
		}

		return inString;
	}

	private isInComment(document: vscode.TextDocument, position: vscode.Position): boolean {
		// Scan through the document from the beginning to check if we're inside a multi-line comment
		// SSL comments start with /* and end with ;
		let inComment = false;

		for (let i = 0; i <= position.line; i++) {
			const line = document.lineAt(i).text;
			const relevantPart = i === position.line ? line.substring(0, position.character) : line;

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
}
