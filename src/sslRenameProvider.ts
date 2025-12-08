import * as vscode from "vscode";
import { PATTERNS } from "./constants/patterns";

/**
 * Represents a procedure's scope in the document
 */
interface ProcedureScope {
	name: string;
	startLine: number;
	endLine: number;
}

/**
 * SSL Rename Provider
 * Provides symbol renaming functionality with validation and scope awareness
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

		// Don't allow renaming inside strings or comments
		if (this.isInString(document, position) || this.isInComment(document, position.line, position.character)) {
			throw new Error("Cannot rename inside strings or comments");
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

		// Determine the scope of the symbol being renamed
		const scope = this.findScopeAtPosition(lines, position.line);

		// Check if this is a procedure name being renamed
		const isProcedureRename = this.isProcedureName(lines, position.line, oldName);

		// Find all occurrences of the old name within the appropriate scope
		const wordPattern = new RegExp(`\\b${this.escapeRegex(oldName)}\\b`, "g");

		for (let i = 0; i < lines.length; i++) {
			// Skip lines outside the scope (unless renaming a procedure itself)
			if (!isProcedureRename && scope && (i < scope.startLine || i > scope.endLine)) {
				continue;
			}

			const line = lines[i];
			let match: RegExpExecArray | null;

			// Reset lastIndex for each line
			wordPattern.lastIndex = 0;

			while ((match = wordPattern.exec(line)) !== null) {
				const charPos = match.index;

				// Skip if in comment
				if (this.isInComment(document, i, charPos)) {
					continue;
				}

				// Check if in string literal
				if (this.isInStringAtPosition(line, charPos)) {
					// Exception: Allow renaming SQL parameters like ?varName?
					if (!this.isSqlParameter(line, charPos, oldName)) {
						continue;
					}
				}

				const startPos = new vscode.Position(i, charPos);
				const endPos = new vscode.Position(i, charPos + oldName.length);
				const replaceRange = new vscode.Range(startPos, endPos);

				edit.replace(document.uri, replaceRange, newName);
			}
		}

		return edit;
	}

	/**
	 * Find all procedure scopes in the document
	 */
	private findAllProcedureScopes(lines: string[]): ProcedureScope[] {
		const scopes: ProcedureScope[] = [];
		let currentProcedure: { name: string; startLine: number } | null = null;

		for (let i = 0; i < lines.length; i++) {
			const line = lines[i];
			const trimmed = line.trim();

			const procMatch = trimmed.match(PATTERNS.PROCEDURE_DEFINITION);
			if (procMatch) {
				currentProcedure = { name: procMatch[1], startLine: i };
			}

			if (PATTERNS.PROCEDURE_END.test(trimmed) && currentProcedure) {
				scopes.push({
					name: currentProcedure.name,
					startLine: currentProcedure.startLine,
					endLine: i
				});
				currentProcedure = null;
			}
		}

		return scopes;
	}

	/**
	 * Find the scope (procedure) containing the given line
	 * Returns null if the line is in global scope
	 */
	private findScopeAtPosition(lines: string[], lineNumber: number): ProcedureScope | null {
		const scopes = this.findAllProcedureScopes(lines);

		for (const scope of scopes) {
			if (lineNumber >= scope.startLine && lineNumber <= scope.endLine) {
				return scope;
			}
		}

		return null; // Global scope
	}

	/**
	 * Check if the word at the given line is a procedure name definition
	 */
	private isProcedureName(lines: string[], lineNumber: number, word: string): boolean {
		const line = lines[lineNumber];
		const match = line.match(PATTERNS.PROCEDURE_DEFINITION);
		return match !== null && match[1].toLowerCase() === word.toLowerCase();
	}

	/**
	 * Check if a position is inside a string literal (single line check)
	 */
	private isInStringAtPosition(line: string, charPosition: number): boolean {
		let inString = false;
		let stringChar: string | null = null;

		for (let i = 0; i < charPosition; i++) {
			const char = line[i];

			if (!inString && (char === '"' || char === "'" || char === '[')) {
				inString = true;
				stringChar = char === '[' ? ']' : char;
			} else if (inString && char === stringChar) {
				inString = false;
				stringChar = null;
			}
		}

		return inString;
	}

	/**
	 * Check if a position is inside a string literal (using document position)
	 */
	private isInString(document: vscode.TextDocument, position: vscode.Position): boolean {
		const line = document.lineAt(position.line).text;
		return this.isInStringAtPosition(line, position.character);
	}

	/**
	 * Check if the word at the given position is a SQL parameter placeholder (?varName?)
	 * SQL parameters should be renamed even when inside string literals
	 */
	private isSqlParameter(line: string, charPosition: number, word: string): boolean {
		// Check if there's a ? immediately before the word
		if (charPosition === 0 || line[charPosition - 1] !== '?') {
			return false;
		}

		// Check if there's a ? immediately after the word
		const endPosition = charPosition + word.length;
		if (endPosition >= line.length || line[endPosition] !== '?') {
			return false;
		}

		return true;
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
