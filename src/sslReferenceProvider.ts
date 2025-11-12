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

		// Filter out references in comments if desired
		return locations.filter(loc => !this.isInComment(document, loc.range.start));
	}

	private escapeRegex(str: string): string {
		return str.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
	}

	private isInComment(document: vscode.TextDocument, position: vscode.Position): boolean {
		const line = document.lineAt(position.line).text;
		const beforePos = line.substring(0, position.character);

		// Check if we're after /* and before ;
		const commentStart = beforePos.lastIndexOf("/*");
		if (commentStart !== -1) {
			const commentEnd = line.substring(commentStart).indexOf(";");
			if (commentEnd === -1 || commentStart + commentEnd > position.character) {
				return true;
			}
		}

		return false;
	}
}
