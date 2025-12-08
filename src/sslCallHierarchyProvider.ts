import * as vscode from "vscode";

interface ProcedureDefinition {
	name: string;
	startLine: number;
	endLine: number;
	nameRange: vscode.Range;
}

/**
 * SSL Call Hierarchy Provider
 * Shows incoming and outgoing calls for procedures
 */
export class SSLCallHierarchyProvider implements vscode.CallHierarchyProvider {

	public prepareCallHierarchy(
		document: vscode.TextDocument,
		position: vscode.Position,
		token: vscode.CancellationToken
	): vscode.CallHierarchyItem | undefined {
		// Use the centralized parser to find procedures
		const procedures = this.parseProcedures(document);

		// Find which procedure contains the position
		const procedure = procedures.find(p =>
			position.line >= p.startLine && position.line <= p.endLine
		);

		if (!procedure) {
			return undefined;
		}

		// Also verify the cursor is actually on the identifier if strictly required, 
		// but typically for prepareCallHierarchy, being inside the body is often accepted or 
		// being on the definition line.
		// VS Code usually calls this when triggering "Call Hierarchy".
		// Let's check if the position is on the definition line first, matching the name.
		// The original code checked `document.getWordRangeAtPosition(position)` matches `procName`.

		const wordRange = document.getWordRangeAtPosition(position);
		if (wordRange) {
			const word = document.getText(wordRange);
			if (word.toLowerCase() === procedure.name.toLowerCase()) {
				// Exact match on definition or usage?
				// If we are mostly providing hierarchy for the *definition* we are standing on.
				// If we are on a call, VS Code might want us to resolve the target. 
				// The original code only looked for `:PROCEDURE <word>` matching definition.
				// We keep that behavior: match procedure definition at the position.
				if (position.line === procedure.startLine) {
					// We are on the definition line
					return this.createCallHierarchyItem(document, procedure);
				}
			}
		}

		// Fallback: If we are not on the name, but inside the proc, maybe we want to show the proc itself?
		// VS Code guidelines: "The provider is invoked for the symbol at the given position."
		// If on a call `Bar()`, we should return `Bar`.
		// If on definition `PROCEDURE Foo`, return `Foo`.
		// Original code: Only returned if matched `^\s*:PROCEDURE\s+(\w+)`.
		// So strict definition matching.

		if (position.line === procedure.startLine) {
			return this.createCallHierarchyItem(document, procedure);
		}

		return undefined;
	}

	public provideCallHierarchyIncomingCalls(
		item: vscode.CallHierarchyItem,
		token: vscode.CancellationToken
	): vscode.CallHierarchyIncomingCall[] {
		const incomingCalls: vscode.CallHierarchyIncomingCall[] = [];
		const procedureName = item.name;

		// We need to look in "all" files. For now, the implementation assumes single-file or workspace finding mechanism.
		// The original code searched `vscode.workspace.textDocuments.find(...)`, effectively only open documents or the one matching URI.
		// We'll stick to that reference scope for now.

		// Find document for the item
		let document: vscode.TextDocument | undefined;
		for (const doc of vscode.workspace.textDocuments) {
			if (doc.uri.toString() === item.uri.toString()) {
				document = doc;
				break;
			}
		}

		if (!document) {
			// Try to open it? The API implies we return results. 
			// Assume it's available or we can't search it easily without filesystem scan.
			return [];
		}

		const text = document.getText();
		const procedures = this.parseProcedures(document);
		const targetNameLower = procedureName.toLowerCase();

		// Find calls in ALL procedures in this document
		for (const proc of procedures) {
			// Skip the target procedure itself (recursive calls usually not shown as incoming from outside, or maybe they are? standardized: yes, recursion is a call)
			// Original code skipped it: `if (proc.name.toLowerCase() === procedureName.toLowerCase()) continue;`
			if (proc.name.toLowerCase() === targetNameLower) {
				continue;
			}

			const procText = text.substring(
				document.offsetAt(new vscode.Position(proc.startLine, 0)),
				document.offsetAt(new vscode.Position(proc.endLine, document.lineAt(proc.endLine).text.length))
			);

			// Find calls to targetName within procText
			// We need absolute ranges.
			const calls = this.findFunctionCalls(procText, targetNameLower);
			const fromRanges: vscode.Range[] = [];

			const procStartOffset = document.offsetAt(new vscode.Position(proc.startLine, 0));

			for (const call of calls) {
				const startOffset = procStartOffset + call.index;
				const endOffset = startOffset + call.name.length; // function name length
				fromRanges.push(new vscode.Range(
					document.positionAt(startOffset),
					document.positionAt(endOffset)
				));
			}

			if (fromRanges.length > 0) {
				const callerItem = this.createCallHierarchyItem(document, proc);
				incomingCalls.push(new vscode.CallHierarchyIncomingCall(callerItem, fromRanges));
			}
		}

		return incomingCalls;
	}

	public provideCallHierarchyOutgoingCalls(
		item: vscode.CallHierarchyItem,
		token: vscode.CancellationToken
	): vscode.CallHierarchyOutgoingCall[] {
		const outgoingCalls: vscode.CallHierarchyOutgoingCall[] = [];

		// Find document
		let document: vscode.TextDocument | undefined;
		for (const doc of vscode.workspace.textDocuments) {
			if (doc.uri.toString() === item.uri.toString()) {
				document = doc;
				break;
			}
		}

		if (!document) {
			return [];
		}

		// Parse all procedures to resolve targets
		const procedures = this.parseProcedures(document);
		const proceduresMap = new Map<string, ProcedureDefinition>();
		for (const proc of procedures) {
			proceduresMap.set(proc.name.toLowerCase(), proc);
		}

		// Analyze body of the item
		const text = document.getText();
		// item.range is the whole procedure range according to our creation logic
		const startOffset = document.offsetAt(item.range.start);
		const endOffset = document.offsetAt(item.range.end);
		const bodyText = text.substring(startOffset, endOffset);

		// Find ALL function calls in the body
		const allCalls = this.findAllFunctionCalls(bodyText);

		// Group calls by function name
		const groupedCalls = new Map<string, vscode.Range[]>();

		for (const call of allCalls) {
			const calledName = call.name;
			const calledNameLower = calledName.toLowerCase();

			// Only if it is a defined procedure in this document (per original logic scope)
			if (proceduresMap.has(calledNameLower)) {
				const callStartOffset = startOffset + call.index;
				const callEndOffset = callStartOffset + calledName.length;
				const range = new vscode.Range(
					document.positionAt(callStartOffset),
					document.positionAt(callEndOffset)
				);

				if (!groupedCalls.has(calledNameLower)) {
					groupedCalls.set(calledNameLower, []);
				}
				groupedCalls.get(calledNameLower)!.push(range);
			}
		}

		// Create outgoing items
		for (const [nameLower, ranges] of groupedCalls) {
			const targetProc = proceduresMap.get(nameLower)!;
			const targetItem = this.createCallHierarchyItem(document, targetProc);
			outgoingCalls.push(new vscode.CallHierarchyOutgoingCall(targetItem, ranges));
		}

		return outgoingCalls;
	}

	private createCallHierarchyItem(document: vscode.TextDocument, proc: ProcedureDefinition): vscode.CallHierarchyItem {
		const fullRange = new vscode.Range(proc.startLine, 0, proc.endLine, document.lineAt(proc.endLine).text.length);
		const selectionRange = proc.nameRange; // The name identifier

		return new vscode.CallHierarchyItem(
			vscode.SymbolKind.Function,
			proc.name,
			"",
			document.uri,
			fullRange,
			selectionRange
		);
	}

	/**
	 * Single-pass parser to find all procedures in the document
	 */
	private parseProcedures(document: vscode.TextDocument): ProcedureDefinition[] {
		const procedures: ProcedureDefinition[] = [];
		const text = document.getText();
		const lines = text.split(/\r?\n/);

		for (let i = 0; i < lines.length; i++) {
			const line = lines[i];
			const procMatch = line.match(/^\s*:PROCEDURE\s+(\w+)/i);

			if (procMatch) {
				const name = procMatch[1];
				const nameIndex = line.indexOf(name);
				const nameRange = new vscode.Range(i, nameIndex, i, nameIndex + name.length);

				// Find end
				let endLine = i;
				for (let j = i + 1; j < lines.length; j++) {
					if (/^\s*:ENDPROC\b/i.test(lines[j])) {
						endLine = j;
						break;
					}
					// Also define boundary if another procedure starts (safety)
					if (/^\s*:PROCEDURE\b/i.test(lines[j])) {
						endLine = Math.max(i, j - 1);
						break;
					}
				}

				procedures.push({
					name,
					startLine: i,
					endLine,
					nameRange
				});

				// Fast forward outer loop (optional, but finding next line is simple enough)
				i = endLine - 1; // loop will increment to endLine
			}
		}
		return procedures;
	}

	/**
	 * Robustly find all function calls in text, ignoring strings
	 */
	private findAllFunctionCalls(text: string): Array<{ name: string; index: number }> {
		const results: Array<{ name: string; index: number }> = [];
		let state: 'CODE' | 'STRING' | 'PARAMS' = 'CODE';
		let stringChar: string | null = null;
		let currentToken = '';
		let tokenStart = -1;
		let parenDepth = 0;

		for (let i = 0; i < text.length; i++) {
			const char = text[i];

			if (state === 'STRING') {
				if (char === stringChar) {
					state = parenDepth > 0 ? 'PARAMS' : 'CODE';
					stringChar = null;
				}
				continue;
			}

			if (char === '"' || char === "'") {
				state = 'STRING';
				stringChar = char;
				continue;
			}
			if (char === '[') {
				if (state === 'PARAMS') {
					state = 'STRING';
					stringChar = ']';
					continue;
				}
			}

			if (state === 'CODE') {
				if (/[A-Za-z_0-9]/.test(char)) {
					if (currentToken === '') {
						tokenStart = i;
					}
					currentToken += char;
				} else if (char === '(') {
					if (currentToken !== '') {
						// Found Name(
						if (/[A-Za-z_]/.test(currentToken[0])) {
							results.push({ name: currentToken, index: tokenStart });
							state = 'PARAMS';
							parenDepth = 1;
						}
						currentToken = '';
					}
				} else if (/\s/.test(char)) {
					// keep token waiting for (
				} else {
					currentToken = '';
				}
			} else if (state === 'PARAMS') {
				if (char === '(') {
					parenDepth++;
				} else if (char === ')') {
					parenDepth--;
					if (parenDepth === 0) {
						state = 'CODE';
						currentToken = '';
					}
				}
			}
		}

		return results;
	}

	/**
	 * Find specific function calls
	 */
	private findFunctionCalls(text: string, targetNameLower: string): Array<{ name: string; index: number }> {
		const allCalls = this.findAllFunctionCalls(text);
		return allCalls.filter(c => c.name.toLowerCase() === targetNameLower);
	}
}
