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
		const insideParen = text.substring(functionStart + 1);
		let parameterIndex = 0;
		let parenDepth = 0;

		for (let i = 0; i < insideParen.length; i++) {
			if (insideParen[i] === "(") {
				parenDepth++;
			} else if (insideParen[i] === ")") {
				parenDepth--;
			} else if (insideParen[i] === "," && parenDepth === 0) {
				parameterIndex++;
			}
		}

		return { name: functionName, parameterIndex };
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
			label: "UsrMes(message, title, type)",
			parameters: ["message: string", "title: string", "type: number"],
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

		this.functionSignatures.set("GETUSERSETTING", {
			label: "GetUserSetting(settingName, defaultValue)",
			parameters: ["settingName: string", "defaultValue: any"],
			documentation: "Get a user-specific setting value"
		});

		this.functionSignatures.set("SETUSERSETTING", {
			label: "SetUserSetting(settingName, value)",
			parameters: ["settingName: string", "value: any"],
			documentation: "Set a user-specific setting value"
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
	}
}
