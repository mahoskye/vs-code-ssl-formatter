import * as vscode from "vscode";
import { ProcedureIndex, ProcedureInfo } from "./utils/procedureIndex";
import { ClassIndex, ClassMembers } from "./utils/classIndex";

/**
 * SSL Workspace Symbol Provider
 * Provides workspace-wide symbol search (Ctrl+T)
 * Refactored to use cached Indexes for performance.
 */
export class SSLWorkspaceSymbolProvider implements vscode.WorkspaceSymbolProvider {

	constructor(
		private readonly procedureIndex: ProcedureIndex,
		private readonly classIndex: ClassIndex
	) { }

	public provideWorkspaceSymbols(
		query: string,
		token: vscode.CancellationToken
	): vscode.SymbolInformation[] {
		const symbols: vscode.SymbolInformation[] = [];
		const queryLower = query.toLowerCase();

		// 1. Search Procedures
		// ProcedureIndex ideally should have a way to get *all* procedures or search them.
		// We know it has `getAllProcedures()`.
		const allProcs = this.procedureIndex.getAllProcedures();

		for (const proc of allProcs) {
			if (token.isCancellationRequested) break;

			if (this.matchesQuery(proc.name, queryLower)) {
				symbols.push(this.createSymbol(
					proc,
					vscode.SymbolKind.Function
				));
			}
		}

		// 2. Search Classes <Type Fix>
		const allClasses = this.classIndex.getAllClasses();
		for (const cls of allClasses) {
			if (token.isCancellationRequested) break;

			if (this.matchesQuery(cls.className, queryLower)) {
				if (cls.uri && cls.range) {
					symbols.push(new vscode.SymbolInformation(
						cls.className,
						vscode.SymbolKind.Class,
						"",
						new vscode.Location(cls.uri, cls.range)
					));
				}
			}
		}



		return symbols;
	}

	private matchesQuery(name: string, query: string): boolean {
		// Simple case-insensitive substring match
		return name.toLowerCase().indexOf(query) >= 0;
	}

	private createSymbol(
		info: ProcedureInfo,
		kind: vscode.SymbolKind
	): vscode.SymbolInformation {
		return new vscode.SymbolInformation(
			info.name,
			kind,
			info.fileBaseName,
			new vscode.Location(info.uri, info.range)
		);
	}
}
