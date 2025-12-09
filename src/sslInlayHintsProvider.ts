import * as vscode from "vscode";
import { ProcedureIndex, ProcedureInfo } from "./utils/procedureIndex";
import { CONFIG_KEYS, CONFIG_DEFAULTS } from "./constants/config";
import { SSL_BUILTIN_FUNCTIONS } from "./constants/language";

/**
 * SSL Inlay Hints Provider
 * Shows parameter names inline for function calls
 */
export class SSLInlayHintsProvider implements vscode.InlayHintsProvider {

	private functionSignatures: Map<string, string[]>;
	private _onDidChangeInlayHints = new vscode.EventEmitter<void>();
	public readonly onDidChangeInlayHints = this._onDidChangeInlayHints.event;

	private debounceTimer: NodeJS.Timeout | undefined;


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

		const hints: vscode.InlayHint[] = [];

		// Show hints for all lines in the requested range
		const startLine = Math.max(0, range.start.line);
		const endLine = Math.min(document.lineCount - 1, range.end.line);
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
			const functionName = match.name.toUpperCase();
			const matchObj: RegExpExecArray = Object.assign([match.fullMatch, match.name, match.args], {
				index: match.index,
				input: text,
				groups: undefined
			}) as any;


			if (functionName === "DOPROC") {
				hints.push(...this.getDoProcInlayHints(document, lineStartOffset, matchObj));
			} else if (functionName === "EXECFUNCTION") {
				hints.push(...this.getExecFunctionInlayHints(document, lineStartOffset, matchObj));
			} else {
				hints.push(...this.getStandardFunctionHints(document, lineStartOffset, match));
			}
		}

		return hints;
	}

	private getStandardFunctionHints(
		document: vscode.TextDocument,
		lineStartOffset: number,
		match: { name: string; args: string; index: number }
	): vscode.InlayHint[] {
		const hints: vscode.InlayHint[] = [];
		const functionName = match.name;
		const paramNames = this.functionSignatures.get(functionName.toUpperCase());

		if (!paramNames) {
			return hints;
		}

		const argsList = this.parseArguments(match.args);
		let currentOffset = match.index + functionName.length + 1; // Start after 'Name('

		// Semantic abbreviations for common long parameters
		const abbreviations: { [key: string]: string } = {
			'commandString': 'cmd',
			'friendlyName': 'id',
			'rollbackExistingTransaction': 'rb',
			'invariantDateColumns': 'dates',
			'includeSchema': 'sch',
			'includeHeader': 'hdr',
			'nullAsBlank': 'nulls',
			'returnType': 'ret',
			'tableName': 'tbl',
			'arrayOfValues': 'vals',
			'incrementWith': 'inc'
		};


		for (let i = 0; i < argsList.length && i < paramNames.length; i++) {
			const absoluteOffset = lineStartOffset + currentOffset;
			const hintPosition = this.createHintPosition(document, absoluteOffset);

			// Use abbreviation or truncate
			let label = paramNames[i];
			if (abbreviations[label]) {
				label = abbreviations[label];
			} else if (label.length > 12) {
				label = label.substring(0, 10) + '..';
			}

			const hint = new vscode.InlayHint(
				hintPosition,
				`${label}:`,
				vscode.InlayHintKind.Parameter
			);
			// hint.paddingRight = true; 
			hints.push(hint);

			// Calculate length of argument including potential whitespace preserved in argsList if specific parsing used
			// The original implementation used argsList[i].length + 1
			currentOffset += argsList[i].length + 1;
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
		let inQuote = false;
		let quoteChar = '';

		for (let i = 0; i < argsString.length; i++) {
			const char = argsString[i];

			if (inQuote) {
				current += char;
				if (char === quoteChar) {
					inQuote = false;
				}
			} else {
				if (char === '"' || char === "'") {
					inQuote = true;
					quoteChar = char;
					current += char;
				} else if (char === "(" || char === "[" || char === "{") {
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
		// Initialize with function signatures from language constants
		for (const func of SSL_BUILTIN_FUNCTIONS) {
			const params = this.extractParameterNames(func.params);
			if (params && params.length > 0) {
				this.functionSignatures.set(func.name.toUpperCase(), params);
			} else {
				// Handle parameter-less functions explicitly if needed, or just set empty array
				this.functionSignatures.set(func.name.toUpperCase(), []);
			}
		}
	}

	private extractParameterNames(paramsString: string): string[] {
		if (!paramsString || paramsString.trim() === '()') {
			return [];
		}

		// Remove parentheses
		const content = paramsString.replace(/^\(|\)$/g, '');

		// Split by comma
		const params = content.split(',');

		return params.map(param => {
			param = param.trim();

			// Remove default values if present (e.g. "any val = null" -> "any val")
			const equalsIndex = param.indexOf('=');
			if (equalsIndex !== -1) {
				param = param.substring(0, equalsIndex).trim();
			}

			// Format is usually "type name", we want "name"
			// Handle array types like "any[] args" correctly
			// Just take the last word after splitting by space
			const parts = param.split(/\s+/);
			return parts[parts.length - 1];
		});
	}

	/**
	 * Find all function calls with balanced parentheses in the text
	 * Uses a single pass state machine to handle string literals and parentheses correctly
	 */
	private findFunctionCalls(text: string): Array<{ name: string; args: string; index: number; fullMatch: string }> {
		const results: Array<{ name: string; args: string; index: number; fullMatch: string }> = [];

		let state: 'CODE' | 'STRING' | 'PARAMS' = 'CODE';
		let stringChar: string | null = null; // ' or " or ] (for [ string)
		let currentToken = '';
		let tokenStart = -1;
		let paramsStart = -1;
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

			// Check for string start
			if (char === '"' || char === "'") {
				state = 'STRING';
				stringChar = char;
				continue;
			}
			// SSL strings can be [ ... ]
			if (char === '[') {
				if (state === 'PARAMS') {
					// Inside params, brackets might be array or string. 
					// The original logic handled [ as string start. 
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
						// Found FunctionName(
						const firstChar = currentToken[0];
						if (/[A-Za-z_]/.test(firstChar)) {
							state = 'PARAMS';
							paramsStart = i + 1;
							parenDepth = 1;
						} else {
							currentToken = '';
						}
					}
				} else if (/\s/.test(char)) {
					// allowed space between name and (
				} else {
					currentToken = '';
				}
			} else if (state === 'PARAMS') {
				if (char === '(') {
					parenDepth++;
				} else if (char === ')') {
					parenDepth--;
					if (parenDepth === 0) {
						// Found end of function call
						const args = text.substring(paramsStart, i);
						const fullMatch = text.substring(tokenStart, i + 1);
						results.push({
							name: currentToken,
							args: args,
							index: tokenStart,
							fullMatch: fullMatch
						});

						// Reset to CODE
						state = 'CODE';
						currentToken = '';
						paramsStart = -1;
					}
				}
			}
		}

		return results;
	}
}
