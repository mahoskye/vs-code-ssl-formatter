import * as vscode from "vscode";
import { ProcedureIndex, ProcedureInfo } from "./utils/procedureIndex";
import { CONFIG_KEYS, CONFIG_DEFAULTS } from "./constants/config";

/**
 * SSL Inlay Hints Provider
 * Shows parameter names inline for function calls
 */
export class SSLInlayHintsProvider implements vscode.InlayHintsProvider {

	private functionSignatures: Map<string, string[]>;
	private _onDidChangeInlayHints = new vscode.EventEmitter<void>();
	public readonly onDidChangeInlayHints = this._onDidChangeInlayHints.event;

	constructor(
		private readonly procedureIndex?: ProcedureIndex
	) {
		this.functionSignatures = new Map();
		this.initializeFunctionSignatures();
	}

	/**
	 * Trigger a refresh of inlay hints
	 */
	public refresh(): void {
		this._onDidChangeInlayHints.fire();
	}

	public provideInlayHints(
		document: vscode.TextDocument,
		range: vscode.Range,
		token: vscode.CancellationToken
	): vscode.InlayHint[] {
		const config = vscode.workspace.getConfiguration("ssl");
		const inlayHintsEnabled = config.get<boolean>("intellisense.inlayHints.enabled", true);
		const showParameterNames = config.get<boolean>("intellisense.inlayHints.parameterNames", true);

		if (!inlayHintsEnabled || !showParameterNames) {
			return [];
		}

		const startLine = Math.max(0, range.start.line);
		const endLine = Math.min(document.lineCount - 1, range.end.line);
		const hints: vscode.InlayHint[] = [];

		for (let lineNumber = startLine; lineNumber <= endLine; lineNumber++) {
			hints.push(...this.getLineInlayHints(document, lineNumber));
		}

		return hints;
	}

	private getLineInlayHints(document: vscode.TextDocument, lineNumber: number): vscode.InlayHint[] {
		if (lineNumber < 0 || lineNumber >= document.lineCount) {
			return [];
		}

		const line = document.lineAt(lineNumber);
		const text = line.text;
		if (!text.includes('(')) {
			return [];
		}

		const lineStartOffset = document.offsetAt(line.range.start);
		const hints: vscode.InlayHint[] = [];
		const functionMatches = this.findFunctionCalls(text);

		for (const match of functionMatches) {
			const functionName = match.name;
			const args = match.args;
			const index = match.index;

			if (functionName.toUpperCase() === "DOPROC") {
				const regExpMatch = Object.assign([
					match.fullMatch,
					functionName,
					args
				], {
					index: index,
					input: text,
					groups: undefined
				}) as RegExpExecArray;

				const doProcHints = this.getDoProcInlayHints(document, lineStartOffset, regExpMatch);
				hints.push(...doProcHints);
				continue;
			}

			if (functionName.toUpperCase() === "EXECFUNCTION") {
				const execHintMatch = Object.assign([
					match.fullMatch,
					functionName,
					args
				], {
					index,
					input: text,
					groups: undefined
				}) as RegExpExecArray;
				const execHints = this.getExecFunctionInlayHints(document, lineStartOffset, execHintMatch);
				hints.push(...execHints);
				continue;
			}

			const paramNames = this.functionSignatures.get(functionName.toUpperCase());
			if (!paramNames) {
				continue;
			}

			const argsList = this.parseArguments(args);
			let currentOffset = index + functionName.length + 1;

			for (let i = 0; i < argsList.length && i < paramNames.length; i++) {
				const absoluteOffset = lineStartOffset + currentOffset;
				const hintPosition = this.createHintPosition(document, absoluteOffset);
				const hint = new vscode.InlayHint(
					hintPosition,
					`${paramNames[i]}:`,
					vscode.InlayHintKind.Parameter
				);
				hint.paddingRight = true;
				hints.push(hint);

				currentOffset += argsList[i].length + 1;
			}
		}

		return hints;
	}

	/**
	 * Get inlay hints for DoProc calls with actual procedure parameters
	 */
	private getDoProcInlayHints(
		document: vscode.TextDocument,
		lineStartOffset: number,
		match: RegExpExecArray
	): vscode.InlayHint[] {
		const hints: vscode.InlayHint[] = [];
		const functionName = match[1];
		const args = match[2];
		const callInfo = this.parseProcedureCallArguments(args);
		if (!callInfo) {
			return hints;
		}

		const procedureName = callInfo.literal;

		// Find the procedure parameters
		const paramNames = this.getProcedureParameters(document, procedureName);
		if (paramNames.length === 0) {
			return hints;
		}

		const arrayArgs = callInfo.rawArgs;

		// Calculate offset to the start of array contents
		let currentOffset = match.index + functionName.length + 1 + callInfo.arrayStartOffset;
		
		// Create hints for each argument matching procedure parameters
		for (let i = 0; i < arrayArgs.length && i < paramNames.length; i++) {
			const absoluteOffset = lineStartOffset + currentOffset;
			const hintPosition = this.createHintPosition(document, absoluteOffset);
			const hint = new vscode.InlayHint(
				hintPosition,
				`${paramNames[i]}:`,
				vscode.InlayHintKind.Parameter
			);

			hint.paddingRight = true;
			hints.push(hint);

			currentOffset += arrayArgs[i].length + 1; // Move past arg and comma
		}

		return hints;
	}

	private getExecFunctionInlayHints(
		document: vscode.TextDocument,
		lineStartOffset: number,
		match: RegExpExecArray
	): vscode.InlayHint[] {
		if (!this.procedureIndex) {
			return [];
		}
		const hints: vscode.InlayHint[] = [];
		const functionName = match[1];
		const args = match[2];
		const callInfo = this.parseProcedureCallArguments(args);
		if (!callInfo) {
			return hints;
		}

		const targetProc = this.resolveWorkspaceProcedure(callInfo.literal);
		if (!targetProc || !targetProc.parameters.length) {
			return hints;
		}

		let currentOffset = match.index + functionName.length + 1 + callInfo.arrayStartOffset;
		for (let i = 0; i < callInfo.rawArgs.length && i < targetProc.parameters.length; i++) {
			const absoluteOffset = lineStartOffset + currentOffset;
			const hintPosition = this.createHintPosition(document, absoluteOffset);
			const hint = new vscode.InlayHint(
				hintPosition,
				`${targetProc.parameters[i]}:`,
				vscode.InlayHintKind.Parameter
			);
			hint.paddingRight = true;
			hints.push(hint);

			currentOffset += callInfo.rawArgs[i].length + 1;
		}

		return hints;
	}

	/**
	 * Get parameter names for a procedure from its definition
	 */
	private getProcedureParameters(document: vscode.TextDocument, procedureName: string): string[] {
		const local = this.getLocalProcedureParameters(document, procedureName);
		if (local.length) {
			return local;
		}

		if (this.procedureIndex) {
			const candidates = this.procedureIndex.getProceduresByName(procedureName);
			const localMatch = candidates.find(info => info.uri.toString() === document.uri.toString());
			if (localMatch && localMatch.parameters.length) {
				return localMatch.parameters;
			}
		}

		return [];
	}

	private getLocalProcedureParameters(document: vscode.TextDocument, procedureName: string): string[] {
		const text = document.getText();
		const lines = text.split('\n');

		// Find the procedure definition
		const procedurePattern = new RegExp(`^\\s*:PROCEDURE\\s+${procedureName}\\b`, 'i');
		let procedureLineIndex = -1;

		for (let i = 0; i < lines.length; i++) {
			if (procedurePattern.test(lines[i])) {
				procedureLineIndex = i;
				break;
			}
		}

		if (procedureLineIndex === -1) {
			return [];
		}

		// Look for :PARAMETERS line after :PROCEDURE
		for (let i = procedureLineIndex + 1; i < Math.min(procedureLineIndex + 20, lines.length); i++) {
			const line = lines[i].trim();

			// Check for :PARAMETERS
			const paramsMatch = line.match(/^:PARAMETERS\s+(.+?);/i);
			if (paramsMatch) {
				return paramsMatch[1].split(',').map(p => p.trim());
			}

			// Stop at next procedure or other block keyword
			if (/^:(PROCEDURE|ENDPROC|DECLARE)\b/i.test(line)) {
				break;
			}
		}

		return [];
	}

	private parseProcedureCallArguments(args: string): { literal: string; rawArgs: string[]; arrayStartOffset: number } | null {
		const procNameMatch = args.match(/["']([^"']+)["']/);
		if (!procNameMatch) {
			return null;
		}
		const procedureLiteral = procNameMatch[1];
		const openBraceIndex = args.indexOf('{', procNameMatch.index ?? 0);
		if (openBraceIndex === -1) {
			return null;
		}

		let depth = 0;
		let arrayContents = "";
		let startIndex = openBraceIndex + 1;
		for (let i = openBraceIndex; i < args.length; i++) {
			if (args[i] === '{') {
				depth++;
			} else if (args[i] === '}') {
				depth--;
				if (depth === 0) {
					arrayContents = args.substring(startIndex, i);
					break;
				}
			}
		}

		if (depth !== 0) {
			return null;
		}

		return {
			literal: procedureLiteral,
			rawArgs: this.parseArguments(arrayContents),
			arrayStartOffset: openBraceIndex + 1
		};
	}

	private resolveWorkspaceProcedure(literal: string): ProcedureInfo | undefined {
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

	private parseArguments(argsString: string): string[] {
		const args: string[] = [];
		let current = "";
		let depth = 0;

		for (const char of argsString) {
			if (char === "(" || char === "[" || char === "{") {
				depth++;
				current += char;
			} else if (char === ")" || char === "]" || char === "}") {
				depth--;
				current += char;
			} else if (char === "," && depth === 0) {
				args.push(current);
				current = "";
			} else {
				current += char;
			}
		}

		if (current) {
			args.push(current);
		}

		return args;
	}

	private createHintPosition(document: vscode.TextDocument, absoluteOffset: number): vscode.Position {
		const position = document.positionAt(absoluteOffset);
		const lineText = document.lineAt(position.line).text;
		let charIndex = position.character;
		while (charIndex < lineText.length && /\s/.test(lineText[charIndex])) {
			charIndex++;
		}
		return new vscode.Position(position.line, charIndex);
	}

	private initializeFunctionSignatures(): void {
		// Initialize with common SSL function signatures
		this.functionSignatures.set("SQLEXECUTE", ["query", "connectionName"]);
		this.functionSignatures.set("DOPROC", ["procName", "parameters"]);
		this.functionSignatures.set("EXECFUNCTION", ["funcName", "parameters"]);
		this.functionSignatures.set("EMPTY", ["value"]);
		this.functionSignatures.set("LEN", ["value"]);
		this.functionSignatures.set("USRMES", ["message1", "message2"]);
		this.functionSignatures.set("CHR", ["code"]);
		this.functionSignatures.set("AADD", ["array", "element"]);
		this.functionSignatures.set("ALLTRIM", ["string"]);
		this.functionSignatures.set("AT", ["needle", "haystack", "occurrence"]);
		this.functionSignatures.set("CREATEUDOBJECT", ["className", "parameters"]);
		this.functionSignatures.set("BUILDSTRING", ["format", "values"]);
		this.functionSignatures.set("ASCAN", ["array", "value", "startIndex", "count"]);
		this.functionSignatures.set("ALEN", ["array", "dimension"]);
		this.functionSignatures.set("LEFT", ["string", "count"]);
		this.functionSignatures.set("RIGHT", ["string", "count"]);
		this.functionSignatures.set("SUBSTR", ["string", "start", "length"]);
		this.functionSignatures.set("UPPER", ["string"]);
		this.functionSignatures.set("LOWER", ["string"]);
		this.functionSignatures.set("VAL", ["string"]);
		this.functionSignatures.set("CTOD", ["dateString"]);
		this.functionSignatures.set("GETSETTING", ["settingName", "defaultValue"]);
		this.functionSignatures.set("GETUSERDATA", ["key"]);
		this.functionSignatures.set("SETUSERDATA", ["key", "value"]);
		this.functionSignatures.set("RUNSQL", ["query", "connectionName", "returnRecords", "parameters"]);
		this.functionSignatures.set("LSEARCH", ["query", "maxRows", "connectionName", "parameters"]);
		this.functionSignatures.set("LSELECT", ["query", "connectionName", "parameters"]);
		this.functionSignatures.set("LSELECT1", ["query", "connectionName", "parameters"]);
		this.functionSignatures.set("LSELECTC", ["query", "connectionName", "parameters"]);
		this.functionSignatures.set("GETDATASET", ["query", "connectionName", "parameters"]);
		this.functionSignatures.set("GETDATASETWITHSCHEMAFROMSELECT", ["query", "connectionName", "parameters"]);
		this.functionSignatures.set("GETDATASETXMLFROMSELECT", ["query", "connectionName", "parameters"]);
		this.functionSignatures.set("GETNETDATASET", ["query", "connectionName", "parameters"]);
		this.functionSignatures.set("GETDATASETEX", ["query", "connectionName", "parameters"]);
		this.functionSignatures.set("ARRAYCALC", ["array", "expression"]);
		this.functionSignatures.set("BUILDARRAY", ["values"]);
		this.functionSignatures.set("ARRAYNEW", ["size"]);
		this.functionSignatures.set("DIRECTORY", ["path", "mask"]);
		this.functionSignatures.set("CREATEGUID", []);
		this.functionSignatures.set("DATEADD", ["date", "interval", "units"]);
		this.functionSignatures.set("DATEDIFF", ["startDate", "endDate", "units"]);
		this.functionSignatures.set("STRTRAN", ["string", "search", "replace"]);
		this.functionSignatures.set("STR", ["value", "length", "decimals"]);
		this.functionSignatures.set("NOW", []);
		this.functionSignatures.set("TODAY", []);
		this.functionSignatures.set("GETLASTSSLERROR", []);
	}

	/**
	 * Find all function calls with balanced parentheses in the text
	 */
	private findFunctionCalls(text: string): Array<{ name: string; args: string; index: number; fullMatch: string }> {
		const results: Array<{ name: string; args: string; index: number; fullMatch: string }> = [];
		const functionPattern = /\b([A-Za-z_]\w*)\s*\(/g;
		let match: RegExpExecArray | null;

		while ((match = functionPattern.exec(text)) !== null) {
			const functionName = match[1];
			const startIndex = match.index;
			const openParenIndex = match.index + match[0].length - 1; // Position of '('

			// Extract arguments with balanced parentheses
			let depth = 1;
			let i = openParenIndex + 1;
			let args = '';

			while (i < text.length && depth > 0) {
				if (text[i] === '(') {
					depth++;
				} else if (text[i] === ')') {
					depth--;
					if (depth === 0) {
						break; // Found matching closing paren
					}
				}
				args += text[i];
				i++;
			}

			if (depth === 0) {
				// Successfully matched
				const fullMatch = text.substring(startIndex, i + 1);
				results.push({
					name: functionName,
					args: args,
					index: startIndex,
					fullMatch: fullMatch
				});
			}
		}

		return results;
	}
}
