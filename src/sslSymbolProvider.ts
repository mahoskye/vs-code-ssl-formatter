import * as vscode from "vscode";
import { PATTERNS } from "./constants/patterns";

/**
 * SSL Document Symbol Provider
 * Provides symbols for outline view, breadcrumbs, and navigation
 * Refactored to support nested scopes (stack-based).
 */
export class SSLSymbolProvider implements vscode.DocumentSymbolProvider {

	public provideDocumentSymbols(
		document: vscode.TextDocument,
		token: vscode.CancellationToken
	): vscode.DocumentSymbol[] {
		const text = document.getText();
		const lines = text.split("\n");

		// Root symbols
		const rootSymbols: vscode.DocumentSymbol[] = [];

		// Stack of active containers
		const stack: vscode.DocumentSymbol[] = [];

		for (let i = 0; i < lines.length; i++) {
			if (token.isCancellationRequested) {
				return [];
			}

			const line = lines[i];
			const trimmed = line.trim();
			if (!trimmed) {
				continue;
			}

			// 1. Check for Block Starts

			// :CLASS
			const classMatch = trimmed.match(/^:CLASS\s+(\w+)/i);
			if (classMatch) {
				this.pushSymbol(
					stack, rootSymbols,
					classMatch[1], vscode.SymbolKind.Class,
					i, line, "Class"
				);
				continue;
			}

			// :PROCEDURE
			const procMatch = trimmed.match(PATTERNS ? PATTERNS.PROCEDURE.DEFINITION : /^:PROCEDURE\s+(\w+)/i);
			if (procMatch && procMatch[1]) {
				this.pushSymbol(
					stack, rootSymbols,
					procMatch[1], vscode.SymbolKind.Function,
					i, line, "Procedure"
				);
				continue;
			}

			// :REGION or /* region
			let regionName: string | null = null;
			const colonRegionRequest = trimmed.match(/^:REGION\s+(.+?)(;|$)/i);
			if (colonRegionRequest) {
				regionName = colonRegionRequest[1].trim();
			} else {
				const commentRegionMatch = trimmed.match(/^\/\*\s*region\s+(.+?)(;|$)/i);
				if (commentRegionMatch) {
					regionName = commentRegionMatch[1].trim();
				}
			}

			if (regionName) {
				this.pushSymbol(
					stack, rootSymbols,
					regionName, vscode.SymbolKind.Namespace,
					i, line, "Region"
				);
				continue;
			}

			// 2. Check for Variables (:DECLARE, :PARAMETERS)
			// These are leaves, added to current container
			const declareMatch = trimmed.match(/^:(DECLARE|PARAMETERS)\s+(.+?)(;|$)/i);
			if (declareMatch) {
				const isParam = declareMatch[1].toUpperCase() === "PARAMETERS";
				const varsContext = declareMatch[2];
				// Split by comma
				const vars = varsContext.split(",").map(v => v.trim()).filter(v => v.length > 0);

				vars.forEach(v => {
					// Check if it has type info? e.g. "x AS INTEGER" (Not typical in generic SSL, usually just names)
					// If v is "Name", easy.
					const varSymbol = new vscode.DocumentSymbol(
						v,
						isParam ? "Parameter" : "Variable",
						isParam ? vscode.SymbolKind.Variable : vscode.SymbolKind.Variable, // Field? Property? 
						new vscode.Range(i, 0, i, line.length),
						new vscode.Range(i, line.indexOf(v), i, line.indexOf(v) + v.length)
					);

					this.addToCurrent(stack, rootSymbols, varSymbol);
				});
				continue;
			}

			// 3. Check for Block Ends
			const endProcMatch = trimmed.match(/^:(ENDPROC|ENDPROCEDURE)\b/i);
			if (endProcMatch) {
				this.popSymbol(stack, "Procedure", i, line.length);
				continue;
			}

			const endRegionMatch = trimmed.match(/^:ENDREGION\b/i) || trimmed.match(/^\/\*\s*endregion/i);
			if (endRegionMatch) {
				this.popSymbol(stack, "Region", i, line.length);
				continue;
			}

			// :CLASS usually extends to end of file, but if we had :ENDCLASS? (SSL doesn't strictly require it usually implies file scope)
			// But if we encounter another :CLASS, it implies logic (only if multiple classes allowed, which is rare in SSL).
			// We'll leave Class open until end of file usually.
		}

		// Close any remaining symbols at end of document
		const lastLine = lines.length - 1;
		const lastLen = lines[Math.max(0, lastLine)].length;
		while (stack.length > 0) {
			const sym = stack.pop()!;
			sym.range = new vscode.Range(sym.range.start, new vscode.Position(lastLine, lastLen));
		}

		return rootSymbols;
	}

	private pushSymbol(
		stack: vscode.DocumentSymbol[],
		roots: vscode.DocumentSymbol[],
		name: string,
		kind: vscode.SymbolKind,
		lineIndex: number,
		lineText: string,
		detail: string
	) {
		// Create with placeholder end range (current line end)
		// We will update 'range' (full range) when popping.
		// 'selectionRange' is the identifier.
		const startPos = new vscode.Position(lineIndex, 0);
		const endPos = new vscode.Position(lineIndex, lineText.length); // Placeholder
		const nameIdx = lineText.indexOf(name);
		const selStart = new vscode.Position(lineIndex, nameIdx >= 0 ? nameIdx : 0);
		const selEnd = new vscode.Position(lineIndex, nameIdx >= 0 ? nameIdx + name.length : lineText.length);

		const symbol = new vscode.DocumentSymbol(
			name,
			detail,
			kind,
			new vscode.Range(startPos, endPos),
			new vscode.Range(selStart, selEnd)
		);

		this.addToCurrent(stack, roots, symbol);
		stack.push(symbol);
	}

	private addToCurrent(stack: vscode.DocumentSymbol[], roots: vscode.DocumentSymbol[], symbol: vscode.DocumentSymbol) {
		if (stack.length > 0) {
			stack[stack.length - 1].children.push(symbol);
		} else {
			roots.push(symbol);
		}
	}

	private popSymbol(stack: vscode.DocumentSymbol[], expectedDetail: string, lineIndex: number, lineLen: number) {
		// We look for the nearest symbol of type 'expectedDetail' in the stack.
		// If not found at top, we close intermediates?

		// Find index of matching symbol (search from top)
		let matchIdx = -1;
		for (let j = stack.length - 1; j >= 0; j--) {
			if (stack[j].detail === expectedDetail) {
				matchIdx = j;
				break;
			}
		}

		if (matchIdx !== -1) {
			// Close everything from top down to match
			while (stack.length > matchIdx) {
				const sym = stack.pop()!;
				// Update full range to include this closing line
				sym.range = new vscode.Range(sym.range.start, new vscode.Position(lineIndex, lineLen));

				// If we are popping something that ISN'T what we looked for (nested mismatch?),
				// we still close it gracefully at this point.
			}
			// Logic above pops the match itself too (loop condition > matchIdx? No, we need to pop matchIdx too)
			// Wait, while stack.length > matchIdx will pop [length-1] ... [matchIdx+1].
			// We need to pop matchIdx as well.
			// Ah, loop condition.
			// Let's rewrite:

			// Pop the specific one:
			// But we can't leave orphans on stack if we skip them.
			// We must close deeper scopes if we close outer scope?
			// Yes.
		} else {
			// Unmatched close? Ignore.
		}

		// Re-implement correctly:
		// If we found 'matchIdx', we want to pop everything from stack until we have popped that index.
		if (matchIdx !== -1) {
			while (stack.length > matchIdx) {
				const sym = stack.pop()!;
				sym.range = new vscode.Range(sym.range.start, new vscode.Position(lineIndex, lineLen));
			}
		}
	}
}
