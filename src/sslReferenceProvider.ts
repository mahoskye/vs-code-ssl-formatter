import * as vscode from "vscode";
import { ProcedureIndex } from "./utils/procedureIndex";
import { CONFIG_KEYS, CONFIG_DEFAULTS } from "./constants/config";

/**
 * SSL Reference Provider
 * Provides "Find All References" functionality for procedures and variables
 */
export class SSLReferenceProvider implements vscode.ReferenceProvider {

	constructor(private readonly procedureIndex?: ProcedureIndex) {}

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
		const symbolType = this.getSymbolType(document, range, word);
		const invocation = this.getProcedureInvocation(document, range);
		const workspaceLocations = invocation ? this.resolveWorkspaceLocations(invocation.literal) : [];

		if (symbolType === "procedure") {
			const localReferences = this.findProcedureReferences(document, word);
			return this.mergeLocations(localReferences, workspaceLocations);
		}

		return this.findVariableReferences(document, word);
	}

	private getSymbolType(document: vscode.TextDocument, range: vscode.Range, word: string): "procedure" | "variable" {
		const lineText = document.lineAt(range.start.line).text;
		const procedureDefinitionPattern = new RegExp(`^\\s*:PROCEDURE\\s+${this.escapeRegex(word)}\\b`, "i");
		if (procedureDefinitionPattern.test(lineText)) {
			return "procedure";
		}

		const procedureCalls = this.getProcedureCallRanges(lineText, range.start.line, word);
		if (procedureCalls.some(callRange => 
			callRange.start.character === range.start.character &&
			callRange.end.character === range.end.character
		)) {
			return "procedure";
		}

		return "variable";
	}

	private findProcedureReferences(document: vscode.TextDocument, word: string): vscode.Location[] {
		const locations: vscode.Location[] = [];
		const lowerWord = word.toLowerCase();

		for (let lineNumber = 0; lineNumber < document.lineCount; lineNumber++) {
			const lineText = document.lineAt(lineNumber).text;
			const definitionPattern = new RegExp(`^\\s*:PROCEDURE\\s+${this.escapeRegex(word)}\\b`, "i");
			if (definitionPattern.test(lineText)) {
				const startIndex = lineText.toLowerCase().indexOf(lowerWord);
				const range = new vscode.Range(
					new vscode.Position(lineNumber, startIndex),
					new vscode.Position(lineNumber, startIndex + word.length)
				);
				locations.push(new vscode.Location(document.uri, range));
			}

			const callRanges = this.getProcedureCallRanges(lineText, lineNumber, word);
			callRanges.forEach(range => {
				if (!this.isInComment(document, range.start)) {
					locations.push(new vscode.Location(document.uri, range));
				}
			});
		}

		return locations;
	}

	private findVariableReferences(document: vscode.TextDocument, word: string): vscode.Location[] {
		const locations: vscode.Location[] = [];
		const text = document.getText();
		const lines = text.split("\n");
		const wordPattern = new RegExp(`\\b${this.escapeRegex(word)}\\b`, "gi");

		for (let i = 0; i < lines.length; i++) {
			const line = lines[i];
			let match: RegExpExecArray | null;
			wordPattern.lastIndex = 0;

			while ((match = wordPattern.exec(line)) !== null) {
				const startPos = new vscode.Position(i, match.index);
				const endPos = new vscode.Position(i, match.index + word.length);
				locations.push(new vscode.Location(
					document.uri,
					new vscode.Range(startPos, endPos)
				));
			}
		}

		return locations.filter(loc =>
			!this.isInComment(document, loc.range.start) &&
			!this.isInString(document, loc.range.start)
		);
	}

	private getProcedureCallRanges(lineText: string, lineNumber: number, filterWord?: string): vscode.Range[] {
		const ranges: vscode.Range[] = [];
		const pattern = /(DoProc|ExecFunction)\s*\(\s*["']([^"']+)["']/gi;
		let match: RegExpExecArray | null;
		const target = filterWord ? filterWord.toLowerCase() : undefined;

		while ((match = pattern.exec(lineText)) !== null) {
			const rawLiteral = match[2];
			const nameInfo = this.extractProcedureName(rawLiteral);
			if (!nameInfo.name) {
				continue;
			}
			if (target && nameInfo.name.toLowerCase() !== target) {
				continue;
			}

			const literalStart = match.index + match[0].indexOf(rawLiteral);
			const startCharacter = literalStart + nameInfo.offset;
			const range = new vscode.Range(
				new vscode.Position(lineNumber, startCharacter),
				new vscode.Position(lineNumber, startCharacter + nameInfo.name.length)
			);
			ranges.push(range);
		}

		return ranges;
	}

	private getProcedureInvocation(document: vscode.TextDocument, range: vscode.Range): { literal: string } | undefined {
		const lineText = document.lineAt(range.start.line).text;
		const pattern = /(DoProc|ExecFunction)\s*\(\s*["']([^"']+)["']/gi;
		let match: RegExpExecArray | null;

		while ((match = pattern.exec(lineText)) !== null) {
			const rawLiteral = match[2];
			const literalStart = match.index + match[0].indexOf(rawLiteral);
			const literalRange = new vscode.Range(
				new vscode.Position(range.start.line, literalStart),
				new vscode.Position(range.start.line, literalStart + rawLiteral.length)
			);
			if (literalRange.contains(range)) {
				return { literal: rawLiteral };
			}
		}

		return undefined;
	}

	private resolveWorkspaceLocations(literal: string): vscode.Location[] {
		const match = this.resolveWorkspaceProcedure(literal);
		if (!match) {
			return [];
		}
		return [new vscode.Location(match.uri, match.range)];
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

	private mergeLocations(local: vscode.Location[], workspace: vscode.Location[]): vscode.Location[] {
		if (!workspace.length) {
			return local;
		}

		const result = [...local];
		const seen = new Set(result.map(loc => this.locationKey(loc)));

		for (const location of workspace) {
			const key = this.locationKey(location);
			if (!seen.has(key)) {
				seen.add(key);
				result.push(location);
			}
		}

		return result;
	}

	private locationKey(location: vscode.Location): string {
		const { start, end } = location.range;
		return `${location.uri.toString()}:${start.line}:${start.character}:${end.line}:${end.character}`;
	}

	private extractProcedureName(rawLiteral: string): { name: string; offset: number } {
		const trimmedLeft = rawLiteral.replace(/^\s+/, "");
		const leftOffset = rawLiteral.length - trimmedLeft.length;
		const trimmed = trimmedLeft.replace(/\s+$/, "");
		const lastDot = trimmed.lastIndexOf('.');
		const nameStart = lastDot === -1 ? 0 : lastDot + 1;
		const name = trimmed.substring(nameStart);
		return {
			name,
			offset: leftOffset + nameStart
		};
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
