import * as vscode from "vscode";

interface FunctionSignature {
	label: string;
	parameters: string[];
	documentation: string;
}

/**
 * SSL Signature Help Provider
 * Provides parameter hints while typing function calls
 */
export class SSLSignatureHelpProvider implements vscode.SignatureHelpProvider {

	private functionSignatures: Map<string, FunctionSignature>;

	constructor() {
		this.functionSignatures = new Map();
		this.initializeFunctionSignatures();
	}

	public provideSignatureHelp(
		document: vscode.TextDocument,
		position: vscode.Position,
		token: vscode.CancellationToken,
		context: vscode.SignatureHelpContext
	): vscode.SignatureHelp | null {
		const line = document.lineAt(position.line).text;
		const beforeCursor = line.substring(0, position.character);

		// Find the function call we're in
		const functionCall = this.findFunctionCall(beforeCursor);
		if (!functionCall) {
			return null;
		}

		// Special handling for DoProc - check if we're inside the array parameter
		if (functionCall.name.toUpperCase() === "DOPROC") {
			// Check if we're inside the curly braces (array parameter)
			const arrayMatch = beforeCursor.match(/DoProc\s*\(\s*["']([^"']+)["']\s*,\s*\{([^}]*)/i);
			if (arrayMatch) {
				const procedureName = arrayMatch[1];
				const insideArray = arrayMatch[2];
				
				// Count which parameter we're on inside the array
				let arrayParamIndex = 0;
				let depth = 0;
				for (let i = 0; i < insideArray.length; i++) {
					if (insideArray[i] === '{') {
						depth++;
					} else if (insideArray[i] === '}') {
						depth--;
					} else if (insideArray[i] === ',' && depth === 0) {
						arrayParamIndex++;
					}
				}
				
				// Get the procedure's signature
				const procedureSignature = this.getProcedureSignature(document, beforeCursor);
				if (procedureSignature) {
					const signatureHelp = new vscode.SignatureHelp();
					const signatureInfo = new vscode.SignatureInformation(
						`DoProc("${procedureName}", {${procedureSignature.parameters.join(', ')}})`,
						procedureSignature.documentation
					);

					procedureSignature.parameters.forEach(param => {
						signatureInfo.parameters.push(new vscode.ParameterInformation(param));
					});

					signatureHelp.signatures = [signatureInfo];
					signatureHelp.activeSignature = 0;
					signatureHelp.activeParameter = arrayParamIndex;

					return signatureHelp;
				}
			}
		}

		const signature = this.functionSignatures.get(functionCall.name.toUpperCase());
		if (!signature) {
			return null;
		}

		const signatureHelp = new vscode.SignatureHelp();
		const signatureInfo = new vscode.SignatureInformation(signature.label, signature.documentation);

		// Add parameter information
		signature.parameters.forEach(param => {
			signatureInfo.parameters.push(new vscode.ParameterInformation(param));
		});

		signatureHelp.signatures = [signatureInfo];
		signatureHelp.activeSignature = 0;
		signatureHelp.activeParameter = functionCall.parameterIndex;

		return signatureHelp;
	}

	private findFunctionCall(text: string): { name: string; parameterIndex: number } | null {
		// Find the last open parenthesis
		let depth = 0;
		let functionStart = -1;

		for (let i = text.length - 1; i >= 0; i--) {
			if (text[i] === ")") {
				depth++;
			} else if (text[i] === "(") {
				if (depth === 0) {
					functionStart = i;
					break;
				}
				depth--;
			}
		}

		if (functionStart === -1) {
			return null;
		}

		// Find the function name before the parenthesis
		const beforeParen = text.substring(0, functionStart).trim();
		const match = beforeParen.match(/([a-zA-Z_][a-zA-Z0-9_]*)\s*$/);
		if (!match) {
			return null;
		}

		const functionName = match[1];

		// Count the parameter index by counting commas
		// Track both parentheses and curly braces depth to avoid counting commas inside arrays
		const insideParen = text.substring(functionStart + 1);
		let parameterIndex = 0;
		let parenDepth = 0;
		let braceDepth = 0;

		for (let i = 0; i < insideParen.length; i++) {
			if (insideParen[i] === "(") {
				parenDepth++;
			} else if (insideParen[i] === ")") {
				parenDepth--;
			} else if (insideParen[i] === "{") {
				braceDepth++;
			} else if (insideParen[i] === "}") {
				braceDepth--;
			} else if (insideParen[i] === "," && parenDepth === 0 && braceDepth === 0) {
				parameterIndex++;
			}
		}

		return { name: functionName, parameterIndex };
	}

	/**
	 * Get signature for a procedure called via DoProc
	 */
	private getProcedureSignature(document: vscode.TextDocument, textBeforeCursor: string): FunctionSignature | null {
		// Extract the procedure name from DoProc("ProcedureName", ...)
		const procNameMatch = textBeforeCursor.match(/DoProc\s*\(\s*["']([^"']+)["']/i);
		if (!procNameMatch) {
			return null;
		}

		const procedureName = procNameMatch[1];
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
			return null;
		}

		// Look for :PARAMETERS line after :PROCEDURE
		const parameters: string[] = [];
		let documentation = `User-defined procedure: ${procedureName}`;

		for (let i = procedureLineIndex + 1; i < Math.min(procedureLineIndex + 20, lines.length); i++) {
			const line = lines[i].trim();

			// Check for :PARAMETERS
			const paramsMatch = line.match(/^:PARAMETERS\s+(.+?);/i);
			if (paramsMatch) {
				const paramList = paramsMatch[1].split(',').map(p => p.trim());
				parameters.push(...paramList);
				break;
			}

			// Stop at next procedure or other block keyword
			if (/^:(PROCEDURE|ENDPROC|DECLARE)\b/i.test(line)) {
				break;
			}

			// Extract documentation from comments
			if (line.startsWith('/*') && !line.endsWith(';')) {
				// Start of multi-line comment
				let commentText = '';
				for (let j = i; j < Math.min(i + 10, lines.length); j++) {
					const commentLine = lines[j].trim();
					if (commentLine.endsWith(';')) {
						break;
					}
					if (commentLine.startsWith('*') && !commentLine.startsWith('/*')) {
						commentText += commentLine.substring(1).trim() + ' ';
					}
				}
				if (commentText) {
					documentation = commentText.trim();
				}
			}
		}

		// Build signature label
		const paramString = parameters.length > 0 ? parameters.join(', ') : '';
		const label = `${procedureName}(${paramString})`;

		return {
			label,
			parameters,
			documentation
		};
	}

	private initializeFunctionSignatures(): void {
		this.functionSignatures.set("SQLEXECUTE", {
			label: "SQLExecute(query, dataset, parameters)",
			parameters: ["query: string", "dataset: string", "parameters: array"],
			documentation: "Execute a SQL query and return results in a dataset"
		});

		this.functionSignatures.set("DOPROC", {
			label: "DoProc(procName, parameters)",
			parameters: ["procName: string", "parameters: array"],
			documentation: "Call a procedure by name with parameters"
		});

		this.functionSignatures.set("EXECFUNCTION", {
			label: "ExecFunction(funcName, parameters)",
			parameters: ["funcName: string", "parameters: array"],
			documentation: "Execute a function dynamically by name"
		});

		this.functionSignatures.set("EMPTY", {
			label: "Empty(value)",
			parameters: ["value: any"],
			documentation: "Check if a value is empty, null, or zero"
		});

		this.functionSignatures.set("LEN", {
			label: "Len(value)",
			parameters: ["value: string|array"],
			documentation: "Get the length of a string or array"
		});

		this.functionSignatures.set("USRMES", {
			label: "UsrMes(message1, message2)",
			parameters: ["message1: any", "message2: any"],
			documentation: "Display a message to the user"
		});

		this.functionSignatures.set("CHR", {
			label: "Chr(code)",
			parameters: ["code: number"],
			documentation: "Convert ASCII code to character"
		});

		this.functionSignatures.set("AADD", {
			label: "AAdd(array, element)",
			parameters: ["array: array", "element: any"],
			documentation: "Add an element to an array"
		});

		this.functionSignatures.set("ALLTRIM", {
			label: "AllTrim(string)",
			parameters: ["string: string"],
			documentation: "Remove leading and trailing whitespace from string"
		});

		this.functionSignatures.set("AT", {
			label: "At(needle, haystack, occurrence)",
			parameters: ["needle: string", "haystack: string", "occurrence: number"],
			documentation: "Find the position of substring in string (1-based index)"
		});

		this.functionSignatures.set("CREATEUDOBJECT", {
			label: "CreateUdObject(className, parameters)",
			parameters: ["className: string", "parameters: array"],
			documentation: "Create a UDO (User Defined Object) instance"
		});

		this.functionSignatures.set("BUILDSTRING", {
			label: "BuildString(format, values)",
			parameters: ["format: string", "values: array"],
			documentation: "Build a formatted string from template and values"
		});

		this.functionSignatures.set("ASCAN", {
			label: "AScan(array, value, startIndex, count)",
			parameters: ["array: array", "value: any", "startIndex: number", "count: number"],
			documentation: "Search for a value in an array, returns 1-based index or 0 if not found"
		});

		this.functionSignatures.set("ALEN", {
			label: "ALen(array, dimension)",
			parameters: ["array: array", "dimension: number"],
			documentation: "Get the length of an array (optionally for specific dimension)"
		});

		this.functionSignatures.set("LEFT", {
			label: "Left(string, count)",
			parameters: ["string: string", "count: number"],
			documentation: "Get leftmost characters from string"
		});

		this.functionSignatures.set("RIGHT", {
			label: "Right(string, count)",
			parameters: ["string: string", "count: number"],
			documentation: "Get rightmost characters from string"
		});

		this.functionSignatures.set("SUBSTR", {
			label: "SubStr(string, start, length)",
			parameters: ["string: string", "start: number", "length: number"],
			documentation: "Extract substring from string (1-based index)"
		});

		this.functionSignatures.set("UPPER", {
			label: "Upper(string)",
			parameters: ["string: string"],
			documentation: "Convert string to uppercase"
		});

		this.functionSignatures.set("LOWER", {
			label: "Lower(string)",
			parameters: ["string: string"],
			documentation: "Convert string to lowercase"
		});

		this.functionSignatures.set("VAL", {
			label: "Val(string)",
			parameters: ["string: string"],
			documentation: "Convert string to numeric value"
		});

		this.functionSignatures.set("CTOD", {
			label: "CtoD(dateString)",
			parameters: ["dateString: string"],
			documentation: "Convert string to date"
		});

		this.functionSignatures.set("GETSETTING", {
			label: "GetSetting(settingName, defaultValue)",
			parameters: ["settingName: string", "defaultValue: any"],
			documentation: "Get a system setting value"
		});

		this.functionSignatures.set("GETUSERDATA", {
			label: "GetUserData(key)",
			parameters: ["key: string"],
			documentation: "Get user-specific data value"
		});

		this.functionSignatures.set("SETUSERDATA", {
			label: "SetUserData(key, value)",
			parameters: ["key: string", "value: any"],
			documentation: "Set user-specific data value"
		});

		this.functionSignatures.set("RUNSQL", {
			label: "RunSQL(query, parameters)",
			parameters: ["query: string", "parameters: array"],
			documentation: "Execute a SQL modification query (INSERT, UPDATE, DELETE)"
		});

		this.functionSignatures.set("LSEARCH", {
			label: "LSearch(query, parameters)",
			parameters: ["query: string", "parameters: array"],
			documentation: "Execute a SQL query and return the first value"
		});

		this.functionSignatures.set("ARRAYNEW", {
			label: "ArrayNew(size)",
			parameters: ["size: number"],
			documentation: "Create a new array with the specified size"
		});

		this.functionSignatures.set("STR", {
			label: "Str(value, length, decimals)",
			parameters: ["value: number", "length: number", "decimals: number"],
			documentation: "Convert a numeric value to a string with optional formatting"
		});

		this.functionSignatures.set("NOW", {
			label: "Now()",
			parameters: [],
			documentation: "Get the current date and time"
		});

		this.functionSignatures.set("TODAY", {
			label: "Today()",
			parameters: [],
			documentation: "Get the current date (without time)"
		});

		this.functionSignatures.set("GETLASTSSLERROR", {
			label: "GetLastSSLError()",
			parameters: [],
			documentation: "Get the last SSL error message"
		});
	}
}
