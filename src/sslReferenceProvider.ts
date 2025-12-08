import * as vscode from "vscode";
import { ProcedureIndex } from "./utils/procedureIndex";
import { CONFIG_KEYS, CONFIG_DEFAULTS } from "./constants/config";

/**
 * SSL Reference Provider
 * Provides "Find All References" functionality for procedures and variables
 * Refactored for performance (single-pass scan).
 */
export class SSLReferenceProvider implements vscode.ReferenceProvider {

	constructor(private readonly procedureIndex?: ProcedureIndex) { }

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
		const locations: vscode.Location[] = [];

		// 1. Identify what we are looking for (Procedure vs Variable)
		// Check current line context to see if we are on a procedure definition
		const lineText = document.lineAt(position.line).text;
		const isProcDef = new RegExp(`^\\s*:PROCEDURE\\s+${this.escapeRegex(word)}\\b`, "i").test(lineText);

		// Also check if we are in a string that looks like a proc call (DoProc("Name"))
		const invocation = this.getProcedureInvocation(document, range);

		// If it's a known procedure context OR if we find it defined as a procedure elsewhere?
		// Actually, "Find References" usually treats the symbol at cursor as the source of truth.
		// If it looks like a variable usage, we search for variable usages.
		// If it looks like a procedure usage/def, we search for procedure.

		// Let's do the single-pass scan to find ALL occurrences of the word, 
		// and classify them.

		const occurrences = this.scanDocumentForWord(document, word, token);
		if (token.isCancellationRequested) {
			return [];
		}

		if (isProcDef || invocation) {
			// It is a procedure. Filter for procedure-relevant occurrences.
			// 1. :PROCEDURE Name
			// 2. DoProc("Name")
			occurrences.forEach(occ => {
				if (occ.type === 'PROCEDURE_DEF' || occ.type === 'PROCEDURE_CALL') {
					locations.push(new vscode.Location(document.uri, occ.range));
				}
			});

			// Add workspace references if index available
			if (invocation) {
				const workspaceLocs = this.resolveWorkspaceLocations(invocation.literal);
				return this.mergeLocations(locations, workspaceLocs);
			}
			// If we are at definition, we should also check workspace for *other* files calling this? 
			// The current implementation only looked up workspace if *calling* (invocation).
			// "Find References" at definition should surely find calls in other files?
			// The `procedureIndex` might support finding references? 
			// The `ProcedureIndex` interface shows `getProceduresByName` but not "getReferences".
			// Typically ReferenceProvider is for the *current* file + maybe basic workspace.
			// The previous code only looked up workspace locations if `invocation` was present (literals).
			// I will retain that behavior for parity, but typically one wants global refs.

			return locations;
		} else {
			// It is likely a variable (or unknown).
			// Filter for variable usages.
			// Exclude comments/strings (handled by scanner).
			occurrences.forEach(occ => {
				if (occ.type === 'CODE' || occ.type === 'VARIABLE_DECL') {
					locations.push(new vscode.Location(document.uri, occ.range));
				}
			});
			return locations;
		}
	}

	private scanDocumentForWord(document: vscode.TextDocument, word: string, token: vscode.CancellationToken): Occurrence[] {
		const text = document.getText();
		const results: Occurrence[] = [];
		const wordLen = word.length;
		const lowerWord = word.toLowerCase(); // Case insensitive? SSL usually is.
		// Previous code used 'gi' regex, so yes.

		// Combined state machine scan
		let inString = false;
		let stringChar = '';
		let inComment = false; // Block or line? SSL has /* ... ;

		// We can iterate lines for easier Range creation, or offset.
		// Line iteration is safer for line-based regexes (like :PROCEDURE check).

		for (let i = 0; i < document.lineCount; i++) {
			if (token.isCancellationRequested) {
				break;
			}
			const line = document.lineAt(i).text;
			const lineLower = line.toLowerCase();

			// Optimization: If line doesn't contain word at all, just update state? 
			// Note: block comments span lines.

			for (let j = 0; j < line.length; j++) {
				const char = line[j];
				const next = line[j + 1] || '';

				// State Updates
				if (!inString && !inComment) {
					if (char === '/' && next === '*') {
						inComment = true;
						j++; continue;
					}
					if (char === '"' || char === '\'' || char === '[') {
						inString = true;
						stringChar = char === '[' ? ']' : char;
						// But wait, if the word STARTs here? (e.g. "Word")
						// If we are IN string, we track word? 
						// Previous finder: `findVariableReferences` EXCLUDED strings.
						// `findProcedureReferences` looked INSIDE strings for DoProc("Word").
					}
				} else if (inComment) {
					if (char === ';') {
						inComment = false;
					}
				} else if (inString) {
					if (char === stringChar) {
						inString = false;
						stringChar = '';
					}
				}

				// Check for word match at this position
				// Must be whole word? Previous code used \\bWORD\\b
				if (lineLower.startsWith(lowerWord, j)) {
					// Check boundaries for whole word
					const prevChar = j > 0 ? line[j - 1] : ' ';
					const nextChar = j + wordLen < line.length ? line[j + wordLen] : ' ';

					if (this.isWordBoundary(prevChar) && this.isWordBoundary(nextChar)) {
						// We have a match!
						const range = new vscode.Range(i, j, i, j + wordLen);

						if (inComment) {
							// Ignore comments entirely
						} else if (inString) {
							// Only interesting if it's a Procedure Call string
							// e.g. DoProc("Word")
							// We need to check context locally
							if (this.isProcedureCallStringContext(line, j, inString)) {
								results.push({ type: 'PROCEDURE_CALL', range });
							}
						} else {
							// In Code
							// Check if declaration
							if (this.isProcedureDefinition(line, j)) {
								results.push({ type: 'PROCEDURE_DEF', range });
							} else if (this.isVariableDeclaration(line, j)) {
								results.push({ type: 'VARIABLE_DECL', range });
							} else {
								results.push({ type: 'CODE', range });
							}
						}
					}

					// Skip word length - 1 (loop increments 1)
					// But we must continue state tracking!
					// Actually, if we skip, we might miss quote/comment markers inside the word?
					// Unlikely for a variable name, but safety first.
					// Just let the loop continue? No, if we matched "Word", we shouldn't match "ord" inside it.
					// But we need to update state.
					// We can fast-forward state? 
					// Let's just process char by char. Matching logic is simple enough.
				}
			}
		}
		return results;
	}

	private isWordBoundary(char: string): boolean {
		return /[^a-zA-Z0-9_]/.test(char);
	}

	private isProcedureCallStringContext(line: string, matchIndex: number, inString: boolean): boolean {
		// Look behind for DoProc/ExecFunction
		// We know we are inside a string.
		// Scan back from matchIndex to find start of string?
		// Simple heuristic: Line up to matchIndex
		const prefix = line.substring(0, matchIndex);
		// Find last quote
		// This is tricky without strict state.
		// But we know 'inString' is true. 
		// Heuristic: Check if line matches (DoProc|ExecFunction)\s*\(\s*["']...$
		// This is what previous code did approx.
		const procMatch = /(DoProc|ExecFunction)\s*\(\s*["']([^"']*)$/i.exec(prefix);
		if (procMatch && procMatch[2].length < line.length) { // match[2] is partial string content before word
			return true;
		}
		return false;
	}

	private isProcedureDefinition(line: string, index: number): boolean {
		// :PROCEDURE Word
		const prefix = line.substring(0, index);
		return /:PROCEDURE\s+$/i.test(prefix);
	}

	private isVariableDeclaration(line: string, index: number): boolean {
		// :DECLARE ... Word
		// :PARAMETERS ... Word
		// This is hard to check purely with prefix if multiple vars: :DECLARE a, b, Word;
		// But "Find References" is usually fuzzy enough.
		// Or we check start of line.
		const start = line.trimLeft();
		if (start.match(/^:(DECLARE|PARAMETERS)\b/i)) {
			return true;
		}
		return false;
	}

	// --- Helpers from original (cleaned) ---

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
		if (!this.procedureIndex) {
			return [];
		}
		const config = vscode.workspace.getConfiguration("ssl");
		const namespaceRoots = config.get<Record<string, string>>(
			CONFIG_KEYS.DOCUMENT_NAMESPACES,
			CONFIG_DEFAULTS[CONFIG_KEYS.DOCUMENT_NAMESPACES] as Record<string, string>
		) || {}; // Fixed: Added cast or fallback
		const match = this.procedureIndex.resolveProcedureLiteral(literal, namespaceRoots);
		return match ? [new vscode.Location(match.uri, match.range)] : [];
	}

	private mergeLocations(local: vscode.Location[], workspace: vscode.Location[]): vscode.Location[] {
		if (!workspace.length) {
			return local;
		}
		const result = [...local];
		const seen = new Set(result.map(loc => this.locationKey(loc)));
		for (const loc of workspace) {
			const key = this.locationKey(loc);
			if (!seen.has(key)) {
				seen.add(key);
				result.push(loc);
			}
		}
		return result;
	}

	private locationKey(location: vscode.Location): string {
		const { start, end } = location.range;
		return `${location.uri.toString()}:${start.line}:${start.character}:${end.line}:${end.character}`;
	}

	private escapeRegex(str: string): string {
		return str.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
	}
}

interface Occurrence {
	type: 'CODE' | 'PROCEDURE_DEF' | 'PROCEDURE_CALL' | 'VARIABLE_DECL';
	range: vscode.Range;
}
