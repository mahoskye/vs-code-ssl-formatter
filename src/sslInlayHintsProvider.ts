import * as vscode from "vscode";

/**
 * SSL Inlay Hints Provider
 * Shows parameter names inline for function calls
 */
export class SSLInlayHintsProvider implements vscode.InlayHintsProvider {

	private functionSignatures: Map<string, string[]>;

	constructor() {
		this.functionSignatures = new Map();
		this.initializeFunctionSignatures();
	}

	public provideInlayHints(
		document: vscode.TextDocument,
		range: vscode.Range,
		token: vscode.CancellationToken
	): vscode.InlayHint[] {
		const config = vscode.workspace.getConfiguration("ssl");
		const inlayHintsEnabled = config.get<boolean>("intellisense.inlayHints.enabled", false);
		const showParameterNames = config.get<boolean>("intellisense.inlayHints.parameterNames", false);

		if (!inlayHintsEnabled || !showParameterNames) {
			return [];
		}

		const hints: vscode.InlayHint[] = [];
		const text = document.getText(range);

		// Find function calls with balanced parentheses
		const functionMatches = this.findFunctionCalls(text);
		
		for (const match of functionMatches) {
			const functionName = match.name;
			const args = match.args;
			const index = match.index;

			// Special handling for DoProc
			if (functionName.toUpperCase() === "DOPROC") {
				// Create a pseudo-match object for compatibility with getDoProcInlayHints
				const regExpMatch = Object.assign([
					match.fullMatch,
					functionName,
					args
				], {
					index: index,
					input: text,
					groups: undefined
				}) as RegExpExecArray;
				
				const doProcHints = this.getDoProcInlayHints(document, range, regExpMatch);
				hints.push(...doProcHints);
				continue;
			}

			// Get parameter names for this function
			const paramNames = this.functionSignatures.get(functionName.toUpperCase());
			if (!paramNames) {
				continue;
			}

			// Parse arguments
			const argsList = this.parseArguments(args);

			// Create inlay hints for each argument
			let currentOffset = index + functionName.length + 1; // Position after function name and '('

			for (let i = 0; i < argsList.length && i < paramNames.length; i++) {
				const arg = argsList[i].trim();
				if (!arg) {
					currentOffset += argsList[i].length + 1; // Include comma
					continue;
				}

				// Calculate the position in the document
				const absoluteOffset = document.offsetAt(range.start) + currentOffset;
				const position = document.positionAt(absoluteOffset);

				// Skip whitespace
				const lineText = document.lineAt(position.line).text;
				let charIndex = position.character;
				while (charIndex < lineText.length && /\s/.test(lineText[charIndex])) {
					charIndex++;
				}

				const hintPosition = new vscode.Position(position.line, charIndex);
				const hint = new vscode.InlayHint(
					hintPosition,
					`${paramNames[i]}:`,
					vscode.InlayHintKind.Parameter
				);

				hint.paddingRight = true;
				hints.push(hint);

				currentOffset += argsList[i].length + 1; // Move past arg and comma
			}
		}

		return hints;
	}

	/**
	 * Get inlay hints for DoProc calls with actual procedure parameters
	 */
	private getDoProcInlayHints(
		document: vscode.TextDocument,
		range: vscode.Range,
		match: RegExpExecArray
	): vscode.InlayHint[] {
		const hints: vscode.InlayHint[] = [];
		const fullMatch = match[0]; // e.g., "DoProc("ValidateSample", {sSampleId, @sErrorMsg})"
		const functionName = match[1]; // "DoProc"
		const args = match[2]; // everything inside the parentheses

		// Parse to get procedure name and extract array contents with proper brace matching
		// Expected format: DoProc("ProcedureName", {param1, param2, param3})
		const procNameMatch = args.match(/["']([^"']+)["']/);
		if (!procNameMatch) {
			return hints;
		}

		const procedureName = procNameMatch[1];

		// Find the opening brace and extract contents by tracking depth
		const openBraceIndex = args.indexOf('{');
		if (openBraceIndex === -1) {
			return hints;
		}

		// Extract array contents with proper nested brace handling
		let arrayContents = '';
		let depth = 0;
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

		if (!arrayContents && depth !== 0) {
			return hints; // Unmatched braces
		}

		// Find the procedure parameters
		const paramNames = this.getProcedureParameters(document, procedureName);
		if (paramNames.length === 0) {
			return hints;
		}

		// Parse the arguments inside the curly braces
		const arrayArgs = this.parseArguments(arrayContents);

		// Calculate offset to the start of array contents
		let currentOffset = match.index + functionName.length + 1 + openBraceIndex + 1; // After 'DoProc(' + position + '{'
		
		// Create hints for each argument matching procedure parameters
		for (let i = 0; i < arrayArgs.length && i < paramNames.length; i++) {
			const arg = arrayArgs[i].trim();
			if (!arg) {
				currentOffset += arrayArgs[i].length + 1;
				continue;
			}

			const absoluteOffset = document.offsetAt(range.start) + currentOffset;
			const position = document.positionAt(absoluteOffset);

			// Skip whitespace to find actual argument start
			const lineText = document.lineAt(position.line).text;
			let charIndex = position.character;
			while (charIndex < lineText.length && /\s/.test(lineText[charIndex])) {
				charIndex++;
			}

			const hintPosition = new vscode.Position(position.line, charIndex);
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

	/**
	 * Get parameter names for a procedure from its definition
	 */
	private getProcedureParameters(document: vscode.TextDocument, procedureName: string): string[] {
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

	private initializeFunctionSignatures(): void {
		// Initialize with common SSL function signatures
		this.functionSignatures.set("SQLEXECUTE", ["query", "dataset", "parameters"]);
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
		this.functionSignatures.set("RUNSQL", ["query", "parameters"]);
		this.functionSignatures.set("LSEARCH", ["query", "parameters"]);
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
