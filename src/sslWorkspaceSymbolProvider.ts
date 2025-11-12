import * as vscode from "vscode";

/**
 * SSL Workspace Symbol Provider
 * Provides workspace-wide symbol search (Ctrl+T)
 */
export class SSLWorkspaceSymbolProvider implements vscode.WorkspaceSymbolProvider {

	public async provideWorkspaceSymbols(
		query: string,
		token: vscode.CancellationToken
	): Promise<vscode.SymbolInformation[]> {
		const symbols: vscode.SymbolInformation[] = [];

		// Find all SSL files in the workspace
		const sslFiles = await vscode.workspace.findFiles(
			"**/*.{ssl,srvscr,ds}",
			"**/node_modules/**",
			1000 // Limit to 1000 files for performance
		);

		// Search each file for symbols matching the query
		for (const fileUri of sslFiles) {
			if (token.isCancellationRequested) {
				break;
			}

			try {
				const document = await vscode.workspace.openTextDocument(fileUri);
				const fileSymbols = this.findSymbolsInDocument(document, query);
				symbols.push(...fileSymbols);
			} catch (error) {
				// Skip files that can't be opened
				continue;
			}
		}

		return symbols;
	}

	private findSymbolsInDocument(
		document: vscode.TextDocument,
		query: string
	): vscode.SymbolInformation[] {
		const symbols: vscode.SymbolInformation[] = [];
		const text = document.getText();
		const lines = text.split("\n");
		const queryLower = query.toLowerCase();

		for (let i = 0; i < lines.length; i++) {
			const line = lines[i];
			const trimmed = line.trim();

			// Find procedures
			const procMatch = trimmed.match(/^\s*:PROCEDURE\s+(\w+)/i);
			if (procMatch) {
				const procName = procMatch[1];

				// Filter by query if provided
				if (query && !procName.toLowerCase().includes(queryLower)) {
					continue;
				}

				const location = new vscode.Location(
					document.uri,
					new vscode.Position(i, 0)
				);

				const symbol = new vscode.SymbolInformation(
					procName,
					vscode.SymbolKind.Function,
					"",
					location
				);

				symbols.push(symbol);
			}

			// Find classes
			const classMatch = trimmed.match(/^\s*:CLASS\s+(\w+)/i);
			if (classMatch) {
				const className = classMatch[1];

				if (query && !className.toLowerCase().includes(queryLower)) {
					continue;
				}

				const location = new vscode.Location(
					document.uri,
					new vscode.Position(i, 0)
				);

				const symbol = new vscode.SymbolInformation(
					className,
					vscode.SymbolKind.Class,
					"",
					location
				);

				symbols.push(symbol);
			}

			// Find regions
			const regionMatch = trimmed.match(/^\s*:REGION\s+(.+?);/i);
			if (regionMatch) {
				const regionName = regionMatch[1].trim();

				if (query && !regionName.toLowerCase().includes(queryLower)) {
					continue;
				}

				const location = new vscode.Location(
					document.uri,
					new vscode.Position(i, 0)
				);

				const symbol = new vscode.SymbolInformation(
					regionName,
					vscode.SymbolKind.Namespace,
					"",
					location
				);

				symbols.push(symbol);
			}
		}

		return symbols;
	}
}
