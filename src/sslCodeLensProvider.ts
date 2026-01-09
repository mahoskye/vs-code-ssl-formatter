import * as vscode from "vscode";

/**
 * SSL CodeLens Provider
 * Shows reference counts above procedures and other symbols
 */
export class SSLCodeLensProvider implements vscode.CodeLensProvider {

	private onDidChangeCodeLensesEmitter = new vscode.EventEmitter<void>();
	public readonly onDidChangeCodeLenses = this.onDidChangeCodeLensesEmitter.event;

	public provideCodeLenses(
		document: vscode.TextDocument,
		token: vscode.CancellationToken
	): vscode.CodeLens[] {
		const config = vscode.workspace.getConfiguration("ssl");
		const codeLensEnabled = config.get<boolean>("intellisense.codeLens.enabled", true);

		if (!codeLensEnabled) {
			return [];
		}

		// Single-pass parse
		const { procedures, references } = this.parseDocument(document);
		const codeLenses: vscode.CodeLens[] = [];

		for (const proc of procedures) {
			const procNameLower = proc.name.toLowerCase();
			const refs = references.get(procNameLower) || [];

			// Filter out the definition location if it was accidentally caught as a reference
			// (Our parser separates definitions and generic identifiers, but let's be safe:
			// A reference should not overlap with the name range of the definition)
			const actualRefs = refs.filter(r => !r.range.intersection(proc.nameRange));

			const refCount = actualRefs.length;

			const codeLens = new vscode.CodeLens(proc.range);
			codeLens.command = {
				title: refCount === 1 ? "1 reference" : `${refCount} references`,
				command: "editor.action.showReferences",
				arguments: [
					document.uri,
					proc.nameRange.start, // Show references for the name position
					actualRefs // Locations
				]
			};

			codeLenses.push(codeLens);
		}

		return codeLenses;
	}

	public resolveCodeLens(
		codeLens: vscode.CodeLens,
		token: vscode.CancellationToken
	): vscode.CodeLens {
		return codeLens;
	}

	public refresh(): void {
		this.onDidChangeCodeLensesEmitter.fire();
	}

	/**
	 * Parse document for procedures and ALL identifier references in one pass.
	 * Returns map of references keyed by lowercase name.
	 */
	private parseDocument(document: vscode.TextDocument): {
		procedures: Array<{ name: string; range: vscode.Range; nameRange: vscode.Range }>;
		references: Map<string, vscode.Location[]>
	} {
		const text = document.getText();
		const procedures: Array<{ name: string; range: vscode.Range; nameRange: vscode.Range }> = [];
		const references = new Map<string, vscode.Location[]>();

		let state: 'CODE' | 'STRING' | 'COMMENT_LINE' | 'COMMENT_BLOCK' = 'CODE';
		let stringChar: string | null = null;
		let currentToken = '';
		let tokenStart = -1;
		let lastIdentifier: string | null = null;

		// We need to track line numbers for Ranges.
		// Doing it by char index and converting at the end is fastest for simple parsing,
		// but we need vscode.Range.
		// We can keep track of line/col manually or use document.positionAt(offset) lazily.
		// Optimization: document.positionAt is decent, simpler to use than tracking lines manually in loop.

		for (let i = 0; i < text.length; i++) {
			const char = text[i];
			const nextChar = i + 1 < text.length ? text[i + 1] : '';

			if (state === 'STRING') {
				if (char === stringChar) {
					// End of string
					state = 'CODE';
					stringChar = null;

					// Check if this string was a procedure name in DoProc/ExecFunction
					if (currentToken.length > 0 && lastIdentifier && /^(DoProc|ExecFunction)$/i.test(lastIdentifier)) {
						// Only treat as reference if it looks like a valid identifier
						this.addReference(document, references, currentToken, tokenStart);
					}
					currentToken = '';
				} else {
					if (currentToken === '') {
						tokenStart = i;
					}
					currentToken += char;
				}
				continue;
			}

			if (state === 'COMMENT_LINE') {
				if (char === '\n') {
					state = 'CODE';
				}
				continue;
			}

			if (state === 'COMMENT_BLOCK') {
				if (char === '*' && nextChar === '/') {
					state = 'CODE';
					i++; // skip /
				} else if (char === ';') {
					// SSL also supports /* ... ; style comments
					state = 'CODE';
				}
				continue;
			}

			// State CODE

			// Comments
			if (char === '/' && nextChar === '/') {
				state = 'COMMENT_LINE';
				i++; // skip /
				// Finish current token if any
				if (currentToken) {
					this.addReference(document, references, currentToken, tokenStart);
					lastIdentifier = currentToken;
					currentToken = '';
				}
				continue;
			}
			if (char === '/' && nextChar === '*') {
				state = 'COMMENT_BLOCK';
				i++; // skip *
				if (currentToken) {
					this.addReference(document, references, currentToken, tokenStart);
					lastIdentifier = currentToken;
					currentToken = '';
				}
				continue;
			}

			// Strings
			if (char === '"' || char === "'") {
				state = 'STRING';
				stringChar = char;
				if (currentToken) {
					this.addReference(document, references, currentToken, tokenStart);
					lastIdentifier = currentToken;
					currentToken = '';
				}
				continue;
			}
			if (char === '[') {
				// Check if this looks like a string or array access?
				if (currentToken.length > 0) {
					// Likely array access. Commit identifier.
					this.addReference(document, references, currentToken, tokenStart);
					lastIdentifier = currentToken;
					currentToken = '';
					// Stay in CODE, don't enter string mode.
				} else {
					// Likely string literal `[ ... ]`
					state = 'STRING';
					stringChar = ']';
				}
				continue;
			}

			// Identifiers and Keywords
			if (/[A-Za-z_0-9]/.test(char)) {
				if (currentToken === '') {
					tokenStart = i;
				}
				currentToken += char;
			} else {
				// Separator/Operator
				if (currentToken.length > 0) {
					this.addReference(document, references, currentToken, tokenStart);
					lastIdentifier = currentToken;
					currentToken = '';
				}
			}
		}

		// Final token
		if (currentToken) {
			this.addReference(document, references, currentToken, tokenStart);
		}

		// Scan for procedure definitions (same as before)
		for (let i = 0; i < document.lineCount; i++) {
			const line = document.lineAt(i);
			const match = line.text.match(/^\s*:PROCEDURE\s+(\w+)/i);
			if (match) {
				const name = match[1];
				const nameIndex = line.text.indexOf(name); // precise loc
				const nameRange = new vscode.Range(i, nameIndex, i, nameIndex + name.length);
				const fullRange = line.range; // CodeLens usually on the line

				procedures.push({ name, range: fullRange, nameRange });
			}
		}

		return { procedures, references };
	}

	private addReference(
		document: vscode.TextDocument,
		references: Map<string, vscode.Location[]>,
		token: string,
		offset: number
	): void {
		// Filter strictly for valid identifiers (start with letter/underscore)
		if (!/^[A-Za-z_][A-Za-z0-9_]*$/.test(token)) {
			return;
		}

		const key = token.toLowerCase();
		if (!references.has(key)) {
			references.set(key, []);
		}

		// Convert offset to Position (lazy here or eagerly? PositionAt is fast enough)
		const startPos = document.positionAt(offset);
		const endPos = document.positionAt(offset + token.length);
		references.get(key)!.push(new vscode.Location(document.uri, new vscode.Range(startPos, endPos)));
	}
}
