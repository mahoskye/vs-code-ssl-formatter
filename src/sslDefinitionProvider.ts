import * as vscode from "vscode";
import { ProcedureIndex } from "./utils/procedureIndex";
import { CONFIG_KEYS, CONFIG_DEFAULTS } from "./constants/config";

interface VariableDefinition {
	name: string;
	line: number;
}

interface ProcedureScope {
	name: string;
	startLine: number;
	endLine: number;
	variables: Map<string, VariableDefinition>;
}

interface DocumentAnalysis {
	procedures: Map<string, number>; // name -> line
	scopes: ProcedureScope[];
	globalVariables: Map<string, VariableDefinition>;
}

/**
 * SSL Definition Provider
 * Provides "Go to Definition" functionality for procedures and variables
 */
export class SSLDefinitionProvider implements vscode.DefinitionProvider {

	constructor(private readonly procedureIndex?: ProcedureIndex) { }

	public provideDefinition(
		document: vscode.TextDocument,
		position: vscode.Position,
		token: vscode.CancellationToken
	): vscode.Definition | null {
		const analysis = this.analyzeDocument(document);
		const lineText = document.lineAt(position.line).text;

		// 1. Check for String-based definition (DoProc/ExecFunction)
		const stringInfo = this.getStringAtPosition(lineText, position.character);
		if (stringInfo) {
			const beforeString = lineText.substring(0, stringInfo.start);
			if (/\b(DoProc|ExecFunction)\s*\(\s*$/i.test(beforeString)) {
				// Remove quotes from content if needed, but the original code passed content as is?
				// Wait, original getStringAtPosition returned content WITHOUT quotes.
				// My new simple checker might vary.
				// Let's rely on stringInfo content.
				const workspaceProc = this.resolveWorkspaceProcedure(stringInfo.content);
				if (workspaceProc) {
					return new vscode.Location(workspaceProc.uri, workspaceProc.range);
				}
			}
			// If inside string but not DoProc, usually no definition
			return null;
		}

		// 2. Variable or Procedure Identifier
		const range = document.getWordRangeAtPosition(position);
		if (!range) {
			return null;
		}
		const word = document.getText(range);

		// a) Check Local Scopes (Variables)
		// Find which scope we are in
		const currentScope = analysis.scopes.find(s => position.line >= s.startLine && position.line <= s.endLine);

		if (currentScope && currentScope.variables.has(word.toLowerCase())) {
			const def = currentScope.variables.get(word.toLowerCase())!;
			return new vscode.Location(document.uri, new vscode.Position(def.line, 0));
		}

		// Check global vars (if any)
		if (analysis.globalVariables.has(word.toLowerCase())) {
			const def = analysis.globalVariables.get(word.toLowerCase())!;
			return new vscode.Location(document.uri, new vscode.Position(def.line, 0));
		}

		// b) Check Local Procedure Definition
		if (analysis.procedures.has(word.toLowerCase())) {
			const line = analysis.procedures.get(word.toLowerCase())!;
			return new vscode.Location(document.uri, new vscode.Position(line, 0));
		}

		// c) Check Workspace Procedure Definition (New improvement: check index)
		if (this.procedureIndex) {
			const procs = this.procedureIndex.getProceduresByName(word);
			if (procs && procs.length > 0) {
				// Prefer one not in current file (since we checked local already, 
				// but local check was based on local analysis. 
				// Index might have it too. If it's same file, we duplicate? 
				// We already returned if local map had it. So this must be remote.)
				return procs.map(p => new vscode.Location(p.uri, p.range));
			}
		}

		return null;
	}

	private analyzeDocument(document: vscode.TextDocument): DocumentAnalysis {
		const analysis: DocumentAnalysis = {
			procedures: new Map(),
			scopes: [],
			globalVariables: new Map()
		};

		const text = document.getText();
		const lines = text.split(/\r?\n/);

		let currentScope: ProcedureScope | null = null;

		for (let i = 0; i < lines.length; i++) {
			const line = lines[i];
			const trimmed = line.trim();

			if (!trimmed) {
				continue;
			}
			if (trimmed.startsWith('//') || (trimmed.startsWith('/*') && trimmed.endsWith('*/'))) {
				continue; // Skip comments
			}
			// Note: Simplistic comment check, robust check used in other providers is better but 
			// for line-based definitions (PROCEDURE/DECLARE usually start line), this is sufficient context.

			// Procedure Start
			const procMatch = line.match(/^\s*:PROCEDURE\s+(\w+)/i);
			if (procMatch) {
				if (currentScope) {
					// Close previous scope implicitly if not closed (safety)
					currentScope.endLine = i - 1;
				}
				const name = procMatch[1];
				analysis.procedures.set(name.toLowerCase(), i);
				currentScope = {
					name,
					startLine: i,
					endLine: lines.length - 1, // default to end
					variables: new Map()
				};
				analysis.scopes.push(currentScope);
				continue;
			}

			// Procedure End
			if (/^\s*:ENDPROC\b/i.test(line)) {
				if (currentScope) {
					currentScope.endLine = i;
					currentScope = null;
				}
				continue;
			}

			// Variable Declaration
			// :DECLARE var1, var2;
			// :PARAMETERS param1, param2;
			const declareMatch = line.match(/^\s*:(DECLARE|PARAMETERS)\s+(.+);/i);
			if (declareMatch) {
				const vars = declareMatch[2].split(',').map(s => s.trim());
				for (const v of vars) {
					const varName = v.split(/\s+/)[0]; // handle "var type" if syntax allows, usually just var in simple SSL
					// Standard SSL: :DECLARE abc, def;
					// Sometimes type hints? assume simple split for now
					const cleanVar = varName.replace(/[^A-Za-z0-9_]/g, '');

					if (!cleanVar) {
						continue;
					}

					const def: VariableDefinition = { name: cleanVar, line: i };
					if (currentScope) {
						currentScope.variables.set(cleanVar.toLowerCase(), def);
					} else {
						analysis.globalVariables.set(cleanVar.toLowerCase(), def);
					}
				}
			}
		}

		return analysis;
	}

	private getStringAtPosition(lineText: string, charPosition: number): { content: string; start: number; end: number } | null {
		let inString = false;
		let stringStart = -1;
		let quoteChar = '';

		for (let i = 0; i < lineText.length; i++) {
			const char = lineText[i];

			if (!inString && (char === '"' || char === "'")) {
				inString = true;
				stringStart = i;
				quoteChar = char;
			} else if (inString && char === quoteChar) {
				if (charPosition > stringStart && charPosition <= i) {
					return {
						content: lineText.substring(stringStart + 1, i),
						start: stringStart,
						end: i + 1
					};
				}
				inString = false;
			}
		}

		return null;
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
}
