import * as vscode from "vscode";

/**
 * SSL Definition Provider
 * Provides "Go to Definition" functionality for procedures and variables
 */
export class SSLDefinitionProvider implements vscode.DefinitionProvider {

	public provideDefinition(
		document: vscode.TextDocument,
		position: vscode.Position,
		token: vscode.CancellationToken
	): vscode.Definition | null {
		const range = document.getWordRangeAtPosition(position);
		if (!range) {
			return null;
		}

		const word = document.getText(range);
		const text = document.getText();
		const lines = text.split("\n");

		// Search for procedure definition
		const procDefinition = this.findProcedureDefinition(lines, word);
		if (procDefinition !== null) {
			return new vscode.Location(
				document.uri,
				new vscode.Position(procDefinition, 0)
			);
		}

		// Search for variable declaration
		const varDefinition = this.findVariableDeclaration(lines, word, position.line);
		if (varDefinition !== null) {
			return new vscode.Location(
				document.uri,
				new vscode.Position(varDefinition, 0)
			);
		}

		return null;
	}

	private findProcedureDefinition(lines: string[], procedureName: string): number | null {
		const procPattern = new RegExp(`^\\s*:PROCEDURE\\s+${procedureName}\\b`, "i");

		for (let i = 0; i < lines.length; i++) {
			if (procPattern.test(lines[i])) {
				return i;
			}
		}

		return null;
	}

	private findVariableDeclaration(lines: string[], varName: string, currentLine: number): number | null {
		// Search backwards from current position for variable declaration
		let inProcedure = false;
		let procedureStart = -1;

		// Find the procedure we're in
		for (let i = currentLine; i >= 0; i--) {
			if (/^\s*:PROCEDURE\b/i.test(lines[i])) {
				inProcedure = true;
				procedureStart = i;
				break;
			}
		}

		// Search for variable declaration
		const declarePattern = new RegExp(`^\\s*:DECLARE\\s+.*\\b${varName}\\b`, "i");
		const parametersPattern = new RegExp(`^\\s*:PARAMETERS\\s+.*\\b${varName}\\b`, "i");

		// If in procedure, search from procedure start to current line
		const searchStart = inProcedure ? procedureStart : 0;
		for (let i = searchStart; i <= currentLine; i++) {
			if (declarePattern.test(lines[i]) || parametersPattern.test(lines[i])) {
				return i;
			}
		}

		return null;
	}
}
