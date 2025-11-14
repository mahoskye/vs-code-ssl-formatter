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

		const codeLenses: vscode.CodeLens[] = [];
		const text = document.getText();
		const lines = text.split("\n");

		// Find all procedures
		for (let i = 0; i < lines.length; i++) {
			const line = lines[i];
			const procMatch = line.match(/^\s*:PROCEDURE\s+(\w+)/i);

			if (procMatch) {
				const procName = procMatch[1];
				const range = new vscode.Range(i, 0, i, line.length);

				// Count references to this procedure
				const refCount = this.countReferences(text, procName);

				const codeLens = new vscode.CodeLens(range);
				codeLens.command = {
					title: refCount === 1 ? "1 reference" : `${refCount} references`,
					command: "editor.action.showReferences",
					arguments: [
						document.uri,
						new vscode.Position(i, 0),
						this.findReferences(document, procName, i)
					]
				};

				codeLenses.push(codeLens);
			}
		}

		return codeLenses;
	}

	public resolveCodeLens(
		codeLens: vscode.CodeLens,
		token: vscode.CancellationToken
	): vscode.CodeLens {
		// CodeLens is already resolved in provideCodeLenses
		return codeLens;
	}

	private countReferences(text: string, symbolName: string): number {
		const pattern = new RegExp(`\\b${this.escapeRegex(symbolName)}\\b`, "gi");
		const matches = text.match(pattern);
		// Subtract 1 for the definition itself
		return matches ? Math.max(0, matches.length - 1) : 0;
	}

	private findReferences(
		document: vscode.TextDocument,
		symbolName: string,
		definitionLine: number
	): vscode.Location[] {
		const locations: vscode.Location[] = [];
		const text = document.getText();
		const lines = text.split("\n");
		const pattern = new RegExp(`\\b${this.escapeRegex(symbolName)}\\b`, "gi");

		for (let i = 0; i < lines.length; i++) {
			// Skip the definition line
			if (i === definitionLine) {
				continue;
			}

			const line = lines[i];
			let match: RegExpExecArray | null;

			pattern.lastIndex = 0;

			while ((match = pattern.exec(line)) !== null) {
				const startPos = new vscode.Position(i, match.index);
				const endPos = new vscode.Position(i, match.index + symbolName.length);
				const location = new vscode.Location(
					document.uri,
					new vscode.Range(startPos, endPos)
				);
				locations.push(location);
			}
		}

		return locations;
	}

	private escapeRegex(str: string): string {
		return str.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
	}

	public refresh(): void {
		this.onDidChangeCodeLensesEmitter.fire();
	}
}
