import * as vscode from "vscode";

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
		const range = document.getWordRangeAtPosition(position);
		if (!range) {
			return undefined;
		}

		const word = document.getText(range);
		const text = document.getText();
		const lines = text.split("\n");

		// Find the procedure definition
		for (let i = 0; i < lines.length; i++) {
			const line = lines[i];
			const procMatch = line.match(/^\s*:PROCEDURE\s+(\w+)/i);

			if (procMatch && procMatch[1].toLowerCase() === word.toLowerCase()) {
				const procName = procMatch[1];
				const lineRange = new vscode.Range(i, 0, i, line.length);

				// Find the end of the procedure
				let endLine = i;
				for (let j = i + 1; j < lines.length; j++) {
					if (/^\s*:(ENDPROC|ENDPROCEDURE)\b/i.test(lines[j])) {
						endLine = j;
						break;
					}
				}

				const fullRange = new vscode.Range(i, 0, endLine, lines[endLine].length);

				return new vscode.CallHierarchyItem(
					vscode.SymbolKind.Function,
					procName,
					"",
					document.uri,
					fullRange,
					lineRange
				);
			}
		}

		return undefined;
	}

	public provideCallHierarchyIncomingCalls(
		item: vscode.CallHierarchyItem,
		token: vscode.CancellationToken
	): vscode.CallHierarchyIncomingCall[] {
		const incomingCalls: vscode.CallHierarchyIncomingCall[] = [];
		const procedureName = item.name;

		try {
			const document = vscode.workspace.textDocuments.find(
				doc => doc.uri.toString() === item.uri.toString()
			);

			if (!document) {
				return [];
			}

			const text = document.getText();
			const lines = text.split("\n");

			// Find all procedures in the document
			const procedures: { name: string; startLine: number; endLine: number }[] = [];

			for (let i = 0; i < lines.length; i++) {
				const procMatch = lines[i].match(/^\s*:PROCEDURE\s+(\w+)/i);
				if (procMatch) {
					const name = procMatch[1];
					const startLine = i;

					// Find end of procedure
					let endLine = i;
					for (let j = i + 1; j < lines.length; j++) {
						if (/^\s*:(ENDPROC|ENDPROCEDURE)\b/i.test(lines[j])) {
							endLine = j;
							break;
						}
					}

					procedures.push({ name, startLine, endLine });
				}
			}

			// Search for calls to the target procedure
			const callPattern = new RegExp(`\\b${this.escapeRegex(procedureName)}\\s*\\(`, "gi");

			for (const proc of procedures) {
				// Don't include the procedure itself
				if (proc.name.toLowerCase() === procedureName.toLowerCase()) {
					continue;
				}

				const fromRanges: vscode.Range[] = [];

				// Search within this procedure for calls
				for (let i = proc.startLine; i <= proc.endLine; i++) {
					const line = lines[i];
					let match: RegExpExecArray | null;

					callPattern.lastIndex = 0;

					while ((match = callPattern.exec(line)) !== null) {
						const startPos = new vscode.Position(i, match.index);
						const endPos = new vscode.Position(i, match.index + procedureName.length);
						fromRanges.push(new vscode.Range(startPos, endPos));
					}
				}

				if (fromRanges.length > 0) {
					const callerItem = new vscode.CallHierarchyItem(
						vscode.SymbolKind.Function,
						proc.name,
						"",
						document.uri,
						new vscode.Range(proc.startLine, 0, proc.endLine, lines[proc.endLine].length),
						new vscode.Range(proc.startLine, 0, proc.startLine, lines[proc.startLine].length)
					);

					incomingCalls.push(new vscode.CallHierarchyIncomingCall(callerItem, fromRanges));
				}
			}
		} catch (error) {
			// Return empty array on error
		}

		return incomingCalls;
	}

	public provideCallHierarchyOutgoingCalls(
		item: vscode.CallHierarchyItem,
		token: vscode.CancellationToken
	): vscode.CallHierarchyOutgoingCall[] {
		const outgoingCalls: vscode.CallHierarchyOutgoingCall[] = [];

		try {
			const document = vscode.workspace.textDocuments.find(
				doc => doc.uri.toString() === item.uri.toString()
			);

			if (!document) {
				return [];
			}

			const text = document.getText();
			const lines = text.split("\n");

			// Find all procedures defined in this document
			const allProcedures = new Map<string, { startLine: number; endLine: number }>();

			for (let i = 0; i < lines.length; i++) {
				const procMatch = lines[i].match(/^\s*:PROCEDURE\s+(\w+)/i);
				if (procMatch) {
					const name = procMatch[1];
					let endLine = i;

					for (let j = i + 1; j < lines.length; j++) {
						if (/^\s*:(ENDPROC|ENDPROCEDURE)\b/i.test(lines[j])) {
							endLine = j;
							break;
						}
					}

					allProcedures.set(name.toLowerCase(), { startLine: i, endLine });
				}
			}

			// Find the body of the current procedure
			const startLine = item.range.start.line;
			const endLine = item.range.end.line;

			// Look for procedure calls within this procedure
			const calledProcedures = new Map<string, vscode.Range[]>();

			for (let i = startLine; i <= endLine; i++) {
				const line = lines[i];

				// Match function calls: ProcName(
				const callPattern = /\b([A-Za-z_]\w*)\s*\(/g;
				let match: RegExpExecArray | null;

				while ((match = callPattern.exec(line)) !== null) {
					const calledName = match[1];
					const calledNameLower = calledName.toLowerCase();

					// Check if this is a defined procedure
					if (allProcedures.has(calledNameLower)) {
						const startPos = new vscode.Position(i, match.index);
						const endPos = new vscode.Position(i, match.index + calledName.length);
						const callRange = new vscode.Range(startPos, endPos);

						if (!calledProcedures.has(calledNameLower)) {
							calledProcedures.set(calledNameLower, []);
						}
						calledProcedures.get(calledNameLower)!.push(callRange);
					}
				}
			}

			// Create outgoing call items
			for (const [calledNameLower, fromRanges] of calledProcedures.entries()) {
				const procInfo = allProcedures.get(calledNameLower)!;

				// Find the actual name (with proper casing) from the definition
				const defLine = lines[procInfo.startLine];
				const defMatch = defLine.match(/^\s*:PROCEDURE\s+(\w+)/i);
				if (!defMatch) {
					continue;
				}

				const calledItem = new vscode.CallHierarchyItem(
					vscode.SymbolKind.Function,
					defMatch[1],
					"",
					document.uri,
					new vscode.Range(procInfo.startLine, 0, procInfo.endLine, lines[procInfo.endLine].length),
					new vscode.Range(procInfo.startLine, 0, procInfo.startLine, defLine.length)
				);

				outgoingCalls.push(new vscode.CallHierarchyOutgoingCall(calledItem, fromRanges));
			}
		} catch (error) {
			// Return empty array on error
		}

		return outgoingCalls;
	}

	private escapeRegex(str: string): string {
		return str.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
	}
}
