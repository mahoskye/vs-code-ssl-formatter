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
		const startLine = range.start.line;

		// Find function calls in the range
		const functionCallPattern = /\b([A-Za-z_]\w*)\s*\(([^)]*)\)/g;
		let match: RegExpExecArray | null;

		while ((match = functionCallPattern.exec(text)) !== null) {
			const functionName = match[1];
			const args = match[2];

			// Get parameter names for this function
			const paramNames = this.functionSignatures.get(functionName.toUpperCase());
			if (!paramNames) {
				continue;
			}

			// Parse arguments
			const argsList = this.parseArguments(args);

			// Create inlay hints for each argument
			let currentOffset = match.index + match[1].length + 1; // Position after function name and '('

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
		this.functionSignatures.set("USRMES", ["message", "title", "type"]);
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
		this.functionSignatures.set("GETUSERSETTING", ["settingName", "defaultValue"]);
		this.functionSignatures.set("SETUSERSETTING", ["settingName", "value"]);
		this.functionSignatures.set("RUNSQL", ["query", "parameters"]);
		this.functionSignatures.set("LSEARCH", ["query", "parameters"]);
		this.functionSignatures.set("ARRAYCALC", ["array", "expression"]);
		this.functionSignatures.set("BUILDARRAY", ["values"]);
		this.functionSignatures.set("DIRECTORY", ["path", "mask"]);
		this.functionSignatures.set("CREATEGUID", []);
		this.functionSignatures.set("DATEADD", ["date", "interval", "units"]);
		this.functionSignatures.set("DATEDIFF", ["startDate", "endDate", "units"]);
		this.functionSignatures.set("STRTRAN", ["string", "search", "replace"]);
	}
}
