import * as vscode from "vscode";

/**
 * SSL Document Symbol Provider
 * Provides symbols for outline view, breadcrumbs, and navigation
 */
export class SSLSymbolProvider implements vscode.DocumentSymbolProvider {

	public provideDocumentSymbols(
		document: vscode.TextDocument,
		token: vscode.CancellationToken
	): vscode.DocumentSymbol[] {
		const symbols: vscode.DocumentSymbol[] = [];
		const text = document.getText();
		const lines = text.split("\n");

		let currentProcedure: vscode.DocumentSymbol | null = null;
		let currentRegion: vscode.DocumentSymbol | null = null;

		for (let i = 0; i < lines.length; i++) {
			const line = lines[i];
			const trimmed = line.trim();

			// Match comment regions: /* region Name; or /*region Name;
			const commentRegionMatch = trimmed.match(/^\/\*\s*region\s+(.+?);/i);
			if (commentRegionMatch) {
				const name = commentRegionMatch[1].trim();
				const range = new vscode.Range(i, 0, i, line.length);
				const selectionRange = new vscode.Range(i, line.indexOf(name), i, line.indexOf(name) + name.length);

				currentRegion = new vscode.DocumentSymbol(
					name,
					"Region",
					vscode.SymbolKind.Namespace,
					range,
					selectionRange
				);

				symbols.push(currentRegion);
				continue;
			}

			// Match end of comment region: /* endregion; or /*endregion;
			const commentEndRegionMatch = trimmed.match(/^\/\*\s*endregion/i);
			if (commentEndRegionMatch && currentRegion) {
				currentRegion.range = new vscode.Range(
					currentRegion.range.start,
					new vscode.Position(i, line.length)
				);
				currentRegion = null;
				continue;
			}

			// Match procedures
			const procMatch = trimmed.match(/^:PROCEDURE\s+(\w+)/i);
			if (procMatch) {
				const name = procMatch[1];
				const range = new vscode.Range(i, 0, i, line.length);
				const selectionRange = new vscode.Range(i, line.indexOf(name), i, line.indexOf(name) + name.length);

				currentProcedure = new vscode.DocumentSymbol(
					name,
					"",
					vscode.SymbolKind.Function,
					range,
					selectionRange
				);

				if (currentRegion) {
					currentRegion.children.push(currentProcedure);
				} else {
					symbols.push(currentProcedure);
				}
				continue;
			}

			// Match end of procedure
			const endProcMatch = trimmed.match(/^:(ENDPROC|ENDPROCEDURE)\b/i);
			if (endProcMatch && currentProcedure) {
				currentProcedure.range = new vscode.Range(
					currentProcedure.range.start,
					new vscode.Position(i, line.length)
				);
				currentProcedure = null;
				continue;
			}

			// Match regions
			const regionMatch = trimmed.match(/^:REGION\s+(.+?);/i);
			if (regionMatch) {
				const name = regionMatch[1].trim();
				const range = new vscode.Range(i, 0, i, line.length);
				const selectionRange = new vscode.Range(i, line.indexOf(name), i, line.indexOf(name) + name.length);

				currentRegion = new vscode.DocumentSymbol(
					name,
					"",
					vscode.SymbolKind.Namespace,
					range,
					selectionRange
				);

				symbols.push(currentRegion);
				continue;
			}

			// Match end of region
			const endRegionMatch = trimmed.match(/^:ENDREGION\b/i);
			if (endRegionMatch && currentRegion) {
				currentRegion.range = new vscode.Range(
					currentRegion.range.start,
					new vscode.Position(i, line.length)
				);
				currentRegion = null;
				continue;
			}

			// Match variable declarations
			const declareMatch = trimmed.match(/^:DECLARE\s+(.+?);/i);
			if (declareMatch) {
				const vars = declareMatch[1].split(",").map(v => v.trim());
				vars.forEach(varName => {
					const varSymbol = new vscode.DocumentSymbol(
						varName,
						"",
						vscode.SymbolKind.Variable,
						new vscode.Range(i, 0, i, line.length),
						new vscode.Range(i, line.indexOf(varName), i, line.indexOf(varName) + varName.length)
					);

					if (currentProcedure) {
						currentProcedure.children.push(varSymbol);
					} else if (currentRegion) {
						currentRegion.children.push(varSymbol);
					} else {
						symbols.push(varSymbol);
					}
				});
				continue;
			}

			// Match classes
			const classMatch = trimmed.match(/^:CLASS\s+(\w+)/i);
			if (classMatch) {
				const name = classMatch[1];
				const range = new vscode.Range(i, 0, i, line.length);
				const selectionRange = new vscode.Range(i, line.indexOf(name), i, line.indexOf(name) + name.length);

				const classSymbol = new vscode.DocumentSymbol(
					name,
					"",
					vscode.SymbolKind.Class,
					range,
					selectionRange
				);

				symbols.push(classSymbol);
			}
		}

		return symbols;
	}
}
