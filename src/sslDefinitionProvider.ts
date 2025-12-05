import * as vscode from "vscode";
import { ProcedureIndex } from "./utils/procedureIndex";
import { CONFIG_KEYS, CONFIG_DEFAULTS } from "./constants/config";

/**
 * SSL Definition Provider
 * Provides "Go to Definition" functionality for procedures and variables
 */
export class SSLDefinitionProvider implements vscode.DefinitionProvider {

	constructor(private readonly procedureIndex?: ProcedureIndex) {}

	public provideDefinition(
		document: vscode.TextDocument,
		position: vscode.Position,
		token: vscode.CancellationToken
	): vscode.Definition | null {
		const range = document.getWordRangeAtPosition(position);
		if (!range) {
			return null;
		}

		const lineText = document.lineAt(position.line).text;

		const stringInfo = this.getStringAtPosition(lineText, position.character);
		if (stringInfo) {
			const beforeString = lineText.substring(0, stringInfo.start);
			const callMatch = beforeString.match(/\b(DoProc|ExecFunction)\s*\(\s*$/i);
			if (callMatch) {
				const workspaceProc = this.resolveWorkspaceProcedure(stringInfo.content);
				if (workspaceProc) {
					return new vscode.Location(workspaceProc.uri, workspaceProc.range);
				}
			}
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
	private getStringAtPosition(lineText: string, charPosition: number): { content: string; start: number; end: number } | null {
		let inString = false;
		let stringStart = -1;
		let quoteChar = '';

		for (let i = 0; i < lineText.length; i++) {
			const char = lineText[i];

			if (!inString && (char === '"' || char === "'")) {
				inString = true;
				stringStart = i;
				quoteChar = char;
			} else if (inString && char === quoteChar) {
				if (charPosition > stringStart && charPosition <= i) {
					return {
						content: lineText.substring(stringStart + 1, i),
						start: stringStart,
						end: i + 1
					};
				}
				inString = false;
			}
		}

		return null;
	}

	private resolveWorkspaceProcedure(literal: string) {
		if (!this.procedureIndex) {
			return undefined;
		}
		const config = vscode.workspace.getConfiguration("ssl");
		const namespaceRoots = config.get<Record<string, string>>(
			CONFIG_KEYS.DOCUMENT_NAMESPACES,
			CONFIG_DEFAULTS[CONFIG_KEYS.DOCUMENT_NAMESPACES] as Record<string, string>
		) || {};
		return this.procedureIndex.resolveProcedureLiteral(literal, namespaceRoots);
	}
}
