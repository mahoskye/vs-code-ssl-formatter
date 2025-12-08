import * as vscode from "vscode";
import {
	SSL_KEYWORD_DESCRIPTIONS,
	SSLFunction,
	SSLClass
} from "./constants/language";
import { ProcedureIndex, ProcedureInfo } from "./utils/procedureIndex";
import { CONFIG_KEYS, CONFIG_DEFAULTS } from "./constants/config";
import { ClassIndex } from "./utils/classIndex";
import { getConfiguredClasses, getConfiguredFunctions } from "./utils/intellisense";

/**
 * SSL Hover Provider
 * Provides hover information for SSL keywords, functions, and symbols.
 * Refactored for performance (backward scanning) and modularity.
 */
export class SSLHoverProvider implements vscode.HoverProvider {

	private functionDocs: Map<string, SSLFunction> = new Map();
	private keywordDocs: Map<string, string> = new Map();
	private builtInClassDocs: Map<string, { description: string; instantiation: string; commonMethods: string[]; commonProperties: string[] }> = new Map();

	constructor(
		private readonly classIndex?: ClassIndex,
		private readonly procedureIndex?: ProcedureIndex
	) {
		this.initializeKeywordDocs();
		this.refreshDocsFromConfig();
	}

	public provideHover(
		document: vscode.TextDocument,
		position: vscode.Position,
		token: vscode.CancellationToken
	): vscode.Hover | null {
		// 1. Fast Context Check (String/Comment)
		const context = this.getFastContext(document, position);
		if (context.insideComment) {
			return null;
		}

		const lineText = document.lineAt(position.line).text;

		// 2. String Content Hovers (SQL, Proc Names in Strings)
		if (context.insideString) {
			// Check SQL Placeholders
			const placeholderHover = this.getSqlPlaceholderHover(document, position, lineText);
			if (placeholderHover) {
				return placeholderHover;
			}

			// Check Procedure/Class names in strings (DoProc("ProcName"))
			const stringRefHover = this.getStringReferenceHover(document, position, lineText, context.stringRange);
			if (stringRefHover) {
				return stringRefHover;
			}

			return null;
		}

		// 3. Regular Code Hovers
		const range = document.getWordRangeAtPosition(position);
		if (!range) {
			return null;
		}

		const word = document.getText(range);

		// Keyword Hover (:KEYWORD)
		if (range.start.character > 0 && lineText[range.start.character - 1] === ':') {
			const kw = this.keywordDocs.get(word.toUpperCase());
			if (kw) {
				return this.createKeywordHover(word, kw, range);
			}
		}

		// Built-in Function Hover
		const funcDoc = this.functionDocs.get(word.toUpperCase());
		if (funcDoc) {
			return this.createFunctionHover(word, funcDoc, range);
		}

		// Built-in Class Hover
		const classDoc = this.builtInClassDocs.get(word.toUpperCase());
		if (classDoc && this.isClassInstantiation(lineText, range.end.character)) {
			return this.createClassHover(classDoc, range);
		}

		// User Defined Functions/Variables
		// TODO: Could add user function hover here if referenced directly (not just string)
		// Currently existing logic focused on string refs for DoProc, but regular calls exist too?
		// Legacy code didn't handle regular calls (e.g. MyFunc()). Adding generic lookup support.

		// Try to resolve as a global/workspace procedure
		const workspaceProc = this.resolveWorkspaceProcedure(word);
		if (workspaceProc) {
			return this.createProcedureHover(workspaceProc, range, "Workspace Procedure");
		}

		return null;
	}

	// --- Context Analysis ---

	private getFastContext(document: vscode.TextDocument, position: vscode.Position): { insideString: boolean; insideComment: boolean; stringRange?: vscode.Range } {
		const text = document.getText(); // Taking full text is expensive? 
		// We only really need to check if we are in a comment/string at this specific point.
		// A full scan from 0 is safest for multiline comments, but we can optimize.
		// Since VSCode doesn't give token info, we unfortunately still have to scan or rely on a "best guess".
		// Use the existing logic's structure but maybe optimize if needed. 
		// For now, retaining the linear scan but cleaning it up.
		// On very large files this is the bottleneck.
		// Optimization: Scan backwards from current line until we find a Procedure start or known "Code" anchor?
		// No, comments can be anywhere.

		// Let's stick to the linear scan for correctness but implement it cleanly.
		// (NOTE: In a real LSP server we'd maintain this state incrementally).

		let insideString = false;
		let stringChar = '';
		let insideComment = false;

		// Offset-based scan might be faster than splitting lines?
		const offset = document.offsetAt(position);
		const fullText = document.getText();

		// Check local line first for single-string containment?
		// Use a simplified state machine for the whole file up to offset
		for (let i = 0; i < offset; i++) {
			const char = fullText[i];
			const next = fullText[i + 1] || '';

			if (!insideString && !insideComment) {
				if (char === '/' && next === '*') { insideComment = true; i++; continue; }
				if (char === '"' || char === '\'' || char === '[') {
					insideString = true;
					stringChar = char === '[' ? ']' : char;
					continue;
				}
			}
			else if (insideComment) {
				if (char === ';') {
					insideComment = false;
				}
			}
			else if (insideString) {
				if (char === stringChar) {
					insideString = false;
				}
			}
		}

		if (insideComment) {
			return { insideString: false, insideComment: true };
		}
		if (insideString) {
			// Find boundaries of this string for range info
			// Scan forward from offset to find end of string
			let end = offset;
			while (end < fullText.length) {
				if (fullText[end] === stringChar) {
					end++; // include quote
					break;
				}
				end++;
			}
			// Scan backward to find start (we know we are inside, so scan back to stringChar)
			// But we might hit nested quotes? No, SSL strings are simple?
			// Safer: we passed the start in the loop above.
			// Just return insideString for now, range calculation computed on demand if needed.
			return { insideString: true, insideComment: false };
		}

		return { insideString: false, insideComment: false };
	}

	// --- Hover Creators ---

	private createKeywordHover(word: string, doc: string, range: vscode.Range): vscode.Hover {
		const md = new vscode.MarkdownString();
		md.appendCodeblock(`:${word.toUpperCase()}`, "ssl");
		md.appendMarkdown(`\n${doc}`);
		return new vscode.Hover(md, range);
	}

	private createFunctionHover(word: string, doc: SSLFunction, range: vscode.Range): vscode.Hover {
		const md = new vscode.MarkdownString();
		const sig = doc.signature || `${doc.name}(${doc.params})`;
		md.appendCodeblock(sig, "ssl");
		md.appendMarkdown(`\n**Description:** ${doc.description}\n\n`);
		if (doc.returns) {
			md.appendMarkdown(`**Returns:** ${doc.returns}\n\n`);
		}
		return new vscode.Hover(md, range);
	}

	private createClassHover(doc: { instantiation: string, description: string }, range: vscode.Range): vscode.Hover {
		const md = new vscode.MarkdownString();
		md.appendCodeblock(doc.instantiation, "ssl");
		md.appendMarkdown(`\n**Description:** ${doc.description}`);
		return new vscode.Hover(md, range);
	}

	private createProcedureHover(proc: ProcedureInfo, range: vscode.Range, label: string): vscode.Hover {
		const md = new vscode.MarkdownString();
		const sig = proc.declarationText || `:PROCEDURE ${proc.name}`;
		md.appendCodeblock(sig, "ssl");
		md.appendMarkdown(`\n**${label}**\n\n`);
		// Documentation property does not exist on ProcedureInfo currently
		// if(proc.documentation) md.appendMarkdown(`${proc.documentation}\n\n`);
		md.appendMarkdown(`Located in \`${vscode.workspace.asRelativePath(proc.uri)}\``);
		return new vscode.Hover(md, range);
	}

	// --- String & SQL Logic ---

	private getStringReferenceHover(document: vscode.TextDocument, position: vscode.Position, lineText: string, stringRange?: vscode.Range): vscode.Hover | null {
		// Find the full string literal including quotes on the current line locally
		const stringInfo = this.findLocalStringAtPosition(lineText, position.character);
		if (!stringInfo) {
			return null;
		}

		// Check preceding context for DoProc/ExecFunction/CreateUDObject
		const prefix = lineText.substring(0, stringInfo.start).trimRight();

		// 1. Procedure Calls
		const procMatch = /(DoProc|ExecFunction)\s*\(\s*$/i.exec(prefix);
		if (procMatch) {
			const procNameParts = stringInfo.content.split('.');
			const procName = procNameParts[procNameParts.length - 1];

			// Check Workspace
			const workspaceProc = this.resolveWorkspaceProcedure(procName); // Only name for now
			if (workspaceProc) {
				const range = new vscode.Range(position.line, stringInfo.start + 1, position.line, stringInfo.end - 1);
				return this.createProcedureHover(workspaceProc, range, `User-defined procedure (via ${procMatch[1]})`);
			}
		}

		// 2. Class Instantiation
		const classMatch = /CreateUDObject\s*\(\s*$/i.exec(prefix);
		if (classMatch) {
			const clsNameParts = stringInfo.content.split('.');
			const clsName = clsNameParts[clsNameParts.length - 1];

			const clsMembers = this.classIndex?.getClassMembers(clsName);
			if (clsMembers) {
				// Create a simple hover for the class
				const md = new vscode.MarkdownString();
				md.appendCodeblock(`:CLASS ${clsMembers.className}`, "ssl");
				md.appendMarkdown(`\n**User-defined class**\n\n`);
				md.appendMarkdown(`_Instantiated via CreateUDObject_`);

				const range = new vscode.Range(position.line, stringInfo.start + 1, position.line, stringInfo.end - 1);
				return new vscode.Hover(md, range);
			}
		}

		return null; // Variable lookup logic could go here
	}

	private getSqlPlaceholderHover(document: vscode.TextDocument, position: vscode.Position, lineText: string): vscode.Hover | null {
		// 1. Named Placeholders ?Var?
		const namedRegex = /\?([A-Za-z0-9_]+)(\[[^\]]+\])?\?/g;
		let match;
		while ((match = namedRegex.exec(lineText)) !== null) {
			const start = match.index;
			const end = match.index + match[0].length;
			if (position.character >= start && position.character < end) {
				const varName = match[1];
				const index = match[2]; // e.g. [1]

				// Look up variable definition (Backward scan)
				const varInfo = this.findVariableBackward(document, varName, position.line);

				const md = new vscode.MarkdownString();
				md.appendMarkdown(`**SQL Parameter:** \`${varName}${index || ''}\`\n\n`);
				if (varInfo) {
					if (varInfo.value) {
						md.appendMarkdown(`**Value:** \`${varInfo.value}\`\n\n`);
					}
					md.appendMarkdown(`Declared at line ${varInfo.line + 1}`);
				} else {
					md.appendMarkdown(`_(Variable not found in local scope)_`);
				}
				return new vscode.Hover(md, new vscode.Range(position.line, start, position.line, end));
			}
		}

		// 2. Positional Placeholders ?
		// We need to count which '?' this is to give context (e.g. "Parameter 1")
		// This requires scanning the line up to the cursor to count '?'
		// but excluding named parameters we just checked.

		// Simple regex for bare '?' (not surrounded by ?...?)
		// Actually, ?Var? consumes the ? so if we are here, we might be on a bare ?
		// But let's be careful not to match inside ?...? if the regex above didn't catch it for some reason (it should have).

		// Let's iterate all '?' and checks types
		const queryCharIndex = position.character;
		if (lineText[queryCharIndex] === '?') {
			// Check if it is part of a named param
			// Check immediate surroundings
			const prevChar = queryCharIndex > 0 ? lineText[queryCharIndex - 1] : '';
			const nextChar = queryCharIndex < lineText.length - 1 ? lineText[queryCharIndex + 1] : '';

			// If looks like ?Var or Var?, it is named.
			// Ideally we reuse the regex logic or scan cleaner.
			// Let's just check if we are inside a ?...? range found by the regex above.
			// We can't reuse the loop easily without refactoring, so let's do a quick pre-check.
			let isNamed = false;
			let m;
			// Reset regex
			namedRegex.lastIndex = 0;
			while ((m = namedRegex.exec(lineText)) !== null) {
				if (queryCharIndex >= m.index && queryCharIndex < m.index + m[0].length) {
					isNamed = true;
					break;
				}
			}

			if (!isNamed) {
				// It is a positional param. Count index.
				let paramIndex = 1;
				for (let i = 0; i < queryCharIndex; i++) {
					if (lineText[i] === '?') {
						// Is this one named?
						let thisIsNamed = false;
						namedRegex.lastIndex = 0;
						while ((m = namedRegex.exec(lineText)) !== null) {
							if (i >= m.index && i < m.index + m[0].length) {
								thisIsNamed = true;
								break;
							}
						}
						if (!thisIsNamed) {
							paramIndex++;
						}
					}
				}

				const md = new vscode.MarkdownString();
				md.appendMarkdown(`**Positional SQL placeholder**\n\n`);
				md.appendMarkdown(`Parameter ${paramIndex}`);

				// Try to find value from RunSQL/LSearch context if possible
				// (This would require parsing the arguments of the wrapping function)
				// For now, basic info is better than nothing.

				return new vscode.Hover(md, new vscode.Range(position.line, queryCharIndex, position.line, queryCharIndex + 1));
			}
		}

		return null;
	}

	private findVariableBackward(document: vscode.TextDocument, varName: string, startLine: number): { line: number, value?: string } | null {
		// Scan backwards max 500 lines or until :PROCEDURE
		for (let i = startLine; i >= Math.max(0, startLine - 100); i--) {
			const line = document.lineAt(i).text;

			// Declaration
			if (new RegExp(`:DECLARE\\s+.*\\b${varName}\\b`, 'i').test(line)) {
				return { line: i };
			}
			if (new RegExp(`:PARAMETERS\\s+.*\\b${varName}\\b`, 'i').test(line)) {
				return { line: i };
			}

			// Start of procedure - stop scanning
			if (/^\s*:PROCEDURE\b/i.test(line)) {
				break;
			}

			// Assignment (Value tracking - simple)
			const assignMatch = new RegExp(`\\b${varName}\\s*:=\\s*(.+?);`, 'i').exec(line);
			if (assignMatch) {
				return { line: i, value: assignMatch[1] };
			}
		}
		return null;
	}

	private findLocalStringAtPosition(line: string, char: number): { content: string, start: number, end: number } | null {
		let inStr = false;
		let start = -1;
		let q = '';
		for (let i = 0; i < line.length; i++) {
			const c = line[i];
			if (!inStr && (c === '"' || c === "'")) { inStr = true; start = i; q = c; }
			else if (inStr && c === q) {
				if (char > start && char <= i) { // Includes closing quote in range check
					return { content: line.substring(start + 1, i), start, end: i + 1 };
				}
				inStr = false;
			}
		}
		return null;
	}

	// --- Helpers ---

	private isClassInstantiation(line: string, index: number): boolean {
		// Check if followed by {
		const remaining = line.substring(index).trimLeft();
		return remaining.startsWith('{');
	}

	private resolveWorkspaceProcedure(name: string): ProcedureInfo | undefined {
		if (!this.procedureIndex) {
			return undefined;
		}
		const config = vscode.workspace.getConfiguration("ssl");
		const namespaceRoots = config.get<Record<string, string>>(CONFIG_KEYS.DOCUMENT_NAMESPACES) || {};
		return this.procedureIndex.resolveProcedureLiteral(name, namespaceRoots);
	}

	private initializeKeywordDocs() {
		Object.entries(SSL_KEYWORD_DESCRIPTIONS).forEach(([k, v]) => this.keywordDocs.set(k, v));
	}

	private refreshDocsFromConfig() {
		const config = vscode.workspace.getConfiguration("ssl");
		this.functionDocs.clear();
		getConfiguredFunctions(config).forEach(f => this.functionDocs.set(f.name.toUpperCase(), f));

		this.builtInClassDocs.clear();
		getConfiguredClasses(config).forEach(c => this.builtInClassDocs.set(c.name.toUpperCase(), {
			description: c.description,
			instantiation: c.instantiation,
			commonMethods: c.methods,
			commonProperties: c.properties
		}));
	}
}
