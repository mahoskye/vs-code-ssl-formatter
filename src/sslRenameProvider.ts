import * as vscode from "vscode";

/**
 * SSL Rename Provider
 * Provides symbol renaming functionality with validation
 */
export class SSLRenameProvider implements vscode.RenameProvider {

	public prepareRename(
		document: vscode.TextDocument,
		position: vscode.Position,
		token: vscode.CancellationToken
	): vscode.Range | null {
		const range = document.getWordRangeAtPosition(position);
		if (!range) {
			return null;
		}

		// Validate that the symbol at this position can be renamed
		const word = document.getText(range);

		// Don't allow renaming keywords
		if (this.isKeyword(word)) {
			throw new Error("Cannot rename SSL keywords");
		}

		// Don't allow renaming built-in functions
		if (this.isBuiltinFunction(word)) {
			throw new Error("Cannot rename built-in functions");
		}

		return range;
	}

	public provideRenameEdits(
		document: vscode.TextDocument,
		position: vscode.Position,
		newName: string,
		token: vscode.CancellationToken
	): vscode.WorkspaceEdit | null {
		const range = document.getWordRangeAtPosition(position);
		if (!range) {
			return null;
		}

		const oldName = document.getText(range);

		// Validate new name
		if (!this.isValidIdentifier(newName)) {
			vscode.window.showErrorMessage(`"${newName}" is not a valid identifier name`);
			return null;
		}

		// Get configuration for Hungarian notation
		const config = vscode.workspace.getConfiguration("ssl");
		const hungarianEnabled = config.get<boolean>("naming.hungarianNotation.enabled", true);

		// Warn if new name doesn't follow Hungarian notation
		if (hungarianEnabled && !this.hasValidHungarianNotation(newName)) {
			vscode.window.showWarningMessage(
				`"${newName}" does not follow Hungarian notation convention (e.g., sName, nCount, aItems)`
			);
		}

		const edit = new vscode.WorkspaceEdit();
		const text = document.getText();
		const lines = text.split("\n");

		// Find all occurrences of the old name
		const wordPattern = new RegExp(`\\b${this.escapeRegex(oldName)}\\b`, "g");

		for (let i = 0; i < lines.length; i++) {
			const line = lines[i];
			let match: RegExpExecArray | null;

			// Reset lastIndex for each line
			wordPattern.lastIndex = 0;

			while ((match = wordPattern.exec(line)) !== null) {
				// Skip if in comment
				if (this.isInComment(document, i, match.index)) {
					continue;
				}

				const startPos = new vscode.Position(i, match.index);
				const endPos = new vscode.Position(i, match.index + oldName.length);
				const replaceRange = new vscode.Range(startPos, endPos);

				edit.replace(document.uri, replaceRange, newName);
			}
		}

		return edit;
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

	private isBuiltinFunction(word: string): boolean {
		const builtins = [
			"SQLEXECUTE", "DOPROC", "EXECFUNCTION", "EMPTY", "LEN", "USRMES",
			"CHR", "AADD", "ALLTRIM", "AT", "NOW", "TODAY",
			"CREATEUDOBJECT", "BUILDSTRING", "ASCAN", "ALEN"
		];
		return builtins.includes(word.toUpperCase());
	}

	private isValidIdentifier(name: string): boolean {
		// Must start with letter or underscore, contain only alphanumeric and underscores
		return /^[a-zA-Z_][a-zA-Z0-9_]*$/.test(name);
	}

	private hasValidHungarianNotation(name: string): boolean {
		// Exceptions for loop counters and constants
		const exceptions = ["i", "j", "k", "NIL"];
		if (exceptions.includes(name)) {
			return true;
		}

		if (name.length < 2) {
			return false;
		}

		const prefix = name[0].toLowerCase();
		const validPrefixes = ["s", "n", "b", "l", "d", "a", "o", "u"];

		// Check if first char is valid prefix and second char is uppercase
		return validPrefixes.includes(prefix) && name[1] === name[1].toUpperCase();
	}

	private escapeRegex(str: string): string {
		return str.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
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
}
