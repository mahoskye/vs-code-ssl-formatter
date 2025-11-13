import * as vscode from "vscode";

/**
 * SSL Hover Provider
 * Provides hover information for SSL keywords, functions, and symbols
 */
export class SSLHoverProvider implements vscode.HoverProvider {

	private functionDocs: Map<string, { description: string; params: string; returns: string }>;
	private keywordDocs: Map<string, string>;
	private builtInClassDocs: Map<string, { description: string; instantiation: string; commonMethods: string[]; commonProperties: string[] }>;

	constructor() {
		this.functionDocs = new Map();
		this.keywordDocs = new Map();
		this.builtInClassDocs = new Map();
		this.initializeFunctionDocs();
		this.initializeKeywordDocs();
		this.initializeBuiltInClassDocs();
	}

	public provideHover(
		document: vscode.TextDocument,
		position: vscode.Position,
		token: vscode.CancellationToken
	): vscode.Hover | null {
		const lineText = document.lineAt(position.line).text;

		// Check if hovering over a procedure name in DoProc or ExecFunction
		const userProcHover = this.getUserDefinedProcedureHover(document, position, lineText);
		if (userProcHover) {
			return userProcHover;
		}

		// Check if hovering over a class name in CreateUDObject
		const userClassHover = this.getUserDefinedClassHover(document, position, lineText);
		if (userClassHover) {
			return userClassHover;
		}

		const range = document.getWordRangeAtPosition(position);
		if (!range) {
			return null;
		}

		const word = document.getText(range);

		// Check if it's a keyword (with colon prefix)
		const wordStart = range.start.character;
		const hasColon = wordStart > 0 && lineText[wordStart - 1] === ":";

		if (hasColon) {
			const keywordUpper = word.toUpperCase();
			const keywordDoc = this.keywordDocs.get(keywordUpper);
			if (keywordDoc) {
				const markdown = new vscode.MarkdownString();
				markdown.appendCodeblock(`:${keywordUpper}`, "ssl");
				markdown.appendMarkdown(`\n${keywordDoc}`);
				return new vscode.Hover(markdown, range);
			}
		}

		// Check if it's a built-in function
		const funcDoc = this.functionDocs.get(word.toUpperCase());
		if (funcDoc) {
			const markdown = new vscode.MarkdownString();
			markdown.appendCodeblock(`${word}${funcDoc.params}`, "ssl");
			markdown.appendMarkdown(`\n**Description:** ${funcDoc.description}\n\n`);
			if (funcDoc.returns) {
				markdown.appendMarkdown(`**Returns:** ${funcDoc.returns}\n\n`);
			}
			return new vscode.Hover(markdown, range);
		}

		// Check if it's a built-in class (followed by {})
		const classDoc = this.builtInClassDocs.get(word.toUpperCase());
		if (classDoc) {
			// Check if followed by {}
			const afterWord = range.end.character;
			const restOfLine = lineText.substring(afterWord);
			if (restOfLine.trimStart().startsWith("{}") || restOfLine.trimStart().startsWith("{")) {
				const markdown = new vscode.MarkdownString();
				markdown.appendCodeblock(classDoc.instantiation, "ssl");
				markdown.appendMarkdown(`\n**Description:** ${classDoc.description}\n\n`);

				if (classDoc.commonMethods.length > 0) {
					markdown.appendMarkdown(`**Common Methods:** ${classDoc.commonMethods.map(m => `\`${m}\``).join(', ')}\n\n`);
				}

				if (classDoc.commonProperties.length > 0) {
					markdown.appendMarkdown(`**Common Properties:** ${classDoc.commonProperties.map(p => `\`${p}\``).join(', ')}\n\n`);
				}

				return new vscode.Hover(markdown, range);
			}
		}

		return null;
	}

	private initializeKeywordDocs(): void {
		this.keywordDocs.set("IF", "Conditional statement - executes code block if condition is true");
		this.keywordDocs.set("ELSE", "Alternative code path when IF condition is false (use nested :IF for else-if logic)");
		this.keywordDocs.set("ENDIF", "Marks the end of an IF conditional block");
		this.keywordDocs.set("WHILE", "Loop that executes while condition is true");
		this.keywordDocs.set("ENDWHILE", "Marks the end of a WHILE loop");
		this.keywordDocs.set("FOR", "Loop with counter variable");
		this.keywordDocs.set("TO", "Specifies the upper bound of a FOR loop");
		this.keywordDocs.set("STEP", "Specifies the increment for a FOR loop");
		this.keywordDocs.set("NEXT", "Marks the end of a FOR loop");
		this.keywordDocs.set("FOREACH", "Iterates over collection elements");
		this.keywordDocs.set("IN", "Specifies the collection to iterate in FOREACH");
		this.keywordDocs.set("BEGINCASE", "Start of a CASE statement for multiple conditions");
		this.keywordDocs.set("CASE", "Individual condition in a CASE statement");
		this.keywordDocs.set("OTHERWISE", "Default case when no other CASE conditions match");
		this.keywordDocs.set("ENDCASE", "Marks the end of a CASE statement");
		this.keywordDocs.set("TRY", "Begin error handling block");
		this.keywordDocs.set("CATCH", "Handle errors from TRY block");
		this.keywordDocs.set("FINALLY", "Code that always executes after TRY/CATCH");
		this.keywordDocs.set("ENDTRY", "Marks the end of TRY/CATCH block");
		this.keywordDocs.set("PROCEDURE", "Defines a reusable code procedure/function");
		this.keywordDocs.set("ENDPROC", "Marks the end of a PROCEDURE");
		this.keywordDocs.set("ENDPROCEDURE", "Marks the end of a PROCEDURE (alternative)");
		this.keywordDocs.set("PARAMETERS", "Declares procedure parameters");
		this.keywordDocs.set("DEFAULT", "Sets default value for a parameter");
		this.keywordDocs.set("RETURN", "Returns a value from a procedure");
		this.keywordDocs.set("DECLARE", "Declares local variables");
		this.keywordDocs.set("PUBLIC", "Declares public/global variables");
		this.keywordDocs.set("INCLUDE", "Includes external SSL file");
		this.keywordDocs.set("REGION", "Marks the beginning of a code region for organization");
		this.keywordDocs.set("ENDREGION", "Marks the end of a code region");
		this.keywordDocs.set("CLASS", "Defines a class");
		this.keywordDocs.set("INHERIT", "Specifies base class for inheritance");
		this.keywordDocs.set("BEGININLINECODE", "Marks the beginning of an inline code block");
		this.keywordDocs.set("ENDINLINECODE", "Marks the end of an inline code block");
		this.keywordDocs.set("EXITFOR", "Exits a FOR loop immediately");
		this.keywordDocs.set("EXITWHILE", "Exits a WHILE loop immediately");
		this.keywordDocs.set("EXITCASE", "Exits a CASE statement immediately");
		this.keywordDocs.set("LOOP", "Jump back to start of loop");
		this.keywordDocs.set("RESUME", "Resume execution after error");
		this.keywordDocs.set("ERROR", "Mark error handling point");
		this.keywordDocs.set("LABEL", "Define a label for GOTO");
	}

	private initializeFunctionDocs(): void {
		this.functionDocs.set("SQLEXECUTE", {
			description: "Execute a SQL query and return results",
			params: "(query, dataset)",
			returns: "Result dataset or status"
		});

		this.functionDocs.set("DOPROC", {
			description: "Call a procedure by name",
			params: "(procName, params)",
			returns: "Procedure return value"
		});

		this.functionDocs.set("EXECFUNCTION", {
			description: "Execute a function dynamically",
			params: "(funcName, params)",
			returns: "Function return value"
		});

		this.functionDocs.set("EMPTY", {
			description: "Check if a value is empty, null, or zero",
			params: "(value)",
			returns: "Boolean (.T. or .F.)"
		});

		this.functionDocs.set("LEN", {
			description: "Get the length of a string or array",
			params: "(value)",
			returns: "Numeric length"
		});

		this.functionDocs.set("USRMES", {
			description: "Display a message to the user",
			params: "(message)",
			returns: "void"
		});

		this.functionDocs.set("CHR", {
			description: "Convert ASCII code to character",
			params: "(code)",
			returns: "Character string"
		});

		this.functionDocs.set("AADD", {
			description: "Add an element to an array",
			params: "(array, element)",
			returns: "Updated array"
		});

		this.functionDocs.set("ALLTRIM", {
			description: "Remove leading and trailing whitespace",
			params: "(string)",
			returns: "Trimmed string"
		});

		this.functionDocs.set("AT", {
			description: "Find the position of substring in string",
			params: "(needle, haystack)",
			returns: "Numeric position (1-based) or 0 if not found"
		});

		this.functionDocs.set("NOW", {
			description: "Get current date and time",
			params: "()",
			returns: "Date/time value"
		});

		this.functionDocs.set("TODAY", {
			description: "Get current date",
			params: "()",
			returns: "Date value"
		});

		this.functionDocs.set("CREATEUDOBJECT", {
			description: "Create a UDO (User Defined Object) instance",
			params: "(className, params)",
			returns: "Object instance"
		});

		this.functionDocs.set("BUILDSTRING", {
			description: "Build a formatted string from template and values",
			params: "(format, values)",
			returns: "Formatted string"
		});

		this.functionDocs.set("ASCAN", {
			description: "Search for a value in an array",
			params: "(array, value)",
			returns: "Index of first match (1-based) or 0 if not found"
		});

		this.functionDocs.set("ALEN", {
			description: "Get the length of an array",
			params: "(array)",
			returns: "Numeric length"
		});

		this.functionDocs.set("LEFT", {
			description: "Get leftmost characters from string",
			params: "(string, count)",
			returns: "Substring"
		});

		this.functionDocs.set("RIGHT", {
			description: "Get rightmost characters from string",
			params: "(string, count)",
			returns: "Substring"
		});

		this.functionDocs.set("SUBSTR", {
			description: "Extract substring from string",
			params: "(string, start, length)",
			returns: "Substring"
		});

		this.functionDocs.set("UPPER", {
			description: "Convert string to uppercase",
			params: "(string)",
			returns: "Uppercase string"
		});

		this.functionDocs.set("LOWER", {
			description: "Convert string to lowercase",
			params: "(string)",
			returns: "Lowercase string"
		});

		this.functionDocs.set("VAL", {
			description: "Convert string to numeric value",
			params: "(string)",
			returns: "Numeric value"
		});

		this.functionDocs.set("CTOD", {
			description: "Convert string to date",
			params: "(dateString)",
			returns: "Date value"
		});

		this.functionDocs.set("GETSETTING", {
			description: "Get a system setting value",
			params: "(settingName)",
			returns: "Setting value"
		});

		this.functionDocs.set("GETUSERDATA", {
			description: "Get user-specific data value",
			params: "(key)",
			returns: "User data value"
		});

		this.functionDocs.set("SETUSERDATA", {
			description: "Set user-specific data value",
			params: "(key, value)",
			returns: "Boolean success status"
		});

		this.functionDocs.set("ARRAYNEW", {
			description: "Create a new array with the specified size",
			params: "(size)",
			returns: "Array with specified number of elements"
		});

		this.functionDocs.set("STR", {
			description: "Convert a numeric value to a string with optional formatting",
			params: "(value, length, decimals)",
			returns: "String representation of the number"
		});

		this.functionDocs.set("GETLASTSSLERROR", {
			description: "Get the last SSL error message from the system",
			params: "()",
			returns: "Error message string"
		});
	}

	private initializeBuiltInClassDocs(): void {
		this.builtInClassDocs.set("EMAIL", {
			description: "Built-in email class for sending emails. Instantiate with Email{} then use colon notation for methods and properties.",
			instantiation: "oEmail := Email{}",
			commonMethods: [
				"oEmail:Send()",
				"oEmail:AddAttachment(path)",
				"oEmail:SetRecipient(address)"
			],
			commonProperties: [
				"oEmail:Subject",
				"oEmail:Body",
				"oEmail:From",
				"oEmail:To"
			]
		});

		this.builtInClassDocs.set("SSLREGEX", {
			description: "Built-in regular expression class for pattern matching. Instantiate with SSLRegex{} then use colon notation for methods and properties.",
			instantiation: "oRegex := SSLRegex{}",
			commonMethods: [
				"oRegex:Match(pattern, text)",
				"oRegex:Replace(pattern, replacement, text)",
				"oRegex:Test(pattern, text)"
			],
			commonProperties: [
				"oRegex:Pattern",
				"oRegex:IgnoreCase",
				"oRegex:Multiline"
			]
		});
	}

	/**
	 * Provides hover information for user-defined procedures called via DoProc or ExecFunction
	 */
	private getUserDefinedProcedureHover(
		document: vscode.TextDocument,
		position: vscode.Position,
		lineText: string
	): vscode.Hover | null {
		// Check if we're inside a string literal
		const stringInfo = this.getStringAtPosition(lineText, position.character);
		if (!stringInfo) {
			return null;
		}

		// Check if this string is the first argument to DoProc or ExecFunction
		const beforeString = lineText.substring(0, stringInfo.start);
		const doProcMatch = beforeString.match(/\b(DoProc|ExecFunction)\s*\(\s*$/i);
		if (!doProcMatch) {
			return null;
		}

		const procedureName = stringInfo.content;

		// Handle namespace in ExecFunction (e.g., "NameSpace.ProcedureName")
		const procParts = procedureName.split('.');
		const actualProcName = procParts[procParts.length - 1];

		// Find the procedure definition
		const procInfo = this.findProcedureDefinition(document, actualProcName);
		if (!procInfo) {
			return null;
		}

		// Create hover with procedure signature
		const markdown = new vscode.MarkdownString();
		markdown.appendCodeblock(`:PROCEDURE ${procInfo.name}${procInfo.params}`, "ssl");
		markdown.appendMarkdown(`\n**User-defined procedure**\n\n`);
		if (procInfo.params) {
			markdown.appendMarkdown(`**Parameters:** ${procInfo.params}\n\n`);
		}
		markdown.appendMarkdown(`_Called via ${doProcMatch[1]}_`);

		// Create range for the procedure name in the string
		const range = new vscode.Range(
			position.line,
			stringInfo.start + 1,  // +1 to skip opening quote
			position.line,
			stringInfo.end - 1     // -1 to skip closing quote
		);

		return new vscode.Hover(markdown, range);
	}

	/**
	 * Provides hover information for user-defined classes called via CreateUDObject
	 */
	private getUserDefinedClassHover(
		document: vscode.TextDocument,
		position: vscode.Position,
		lineText: string
	): vscode.Hover | null {
		// Check if we're inside a string literal
		const stringInfo = this.getStringAtPosition(lineText, position.character);
		if (!stringInfo) {
			return null;
		}

		// Check if this string is the first argument to CreateUDObject
		const beforeString = lineText.substring(0, stringInfo.start);
		const createUDOMatch = beforeString.match(/\bCreateUDObject\s*\(\s*$/i);
		if (!createUDOMatch) {
			return null;
		}

		const className = stringInfo.content;

		// Handle namespace (e.g., "NameSpace.ClassName")
		const classParts = className.split('.');
		const actualClassName = classParts[classParts.length - 1];

		// Find the class definition
		const classInfo = this.findClassDefinition(document, actualClassName);
		if (!classInfo) {
			return null;
		}

		// Create hover with class signature
		const markdown = new vscode.MarkdownString();
		let codeBlock = `:CLASS ${classInfo.name}`;
		if (classInfo.inherits) {
			codeBlock += `\n:INHERIT ${classInfo.inherits}`;
		}
		markdown.appendCodeblock(codeBlock, "ssl");
		markdown.appendMarkdown(`\n**User-defined class**\n\n`);
		if (classInfo.inherits) {
			markdown.appendMarkdown(`**Inherits from:** ${classInfo.inherits}\n\n`);
		}
		markdown.appendMarkdown(`_Instantiated via CreateUDObject_`);

		// Create range for the class name in the string
		const range = new vscode.Range(
			position.line,
			stringInfo.start + 1,  // +1 to skip opening quote
			position.line,
			stringInfo.end - 1     // -1 to skip closing quote
		);

		return new vscode.Hover(markdown, range);
	}

	/**
	 * Extracts string content and position if cursor is inside a string literal
	 */
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
				// Check if cursor is within this string
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

	/**
	 * Finds a procedure definition in the document with parameter details
	 */
	private findProcedureDefinition(
		document: vscode.TextDocument,
		procedureName: string
	): { name: string; params: string } | null {
		const text = document.getText();
		const lines = text.split('\n');

		const procPattern = new RegExp(`^\\s*:PROCEDURE\\s+(${procedureName})\\b`, 'i');

		for (let i = 0; i < lines.length; i++) {
			const match = lines[i].match(procPattern);
			if (match) {
				const name = match[1];

				// Look for parameters and defaults
				const paramInfo = this.extractProcedureParameters(lines, i);

				return { name, params: paramInfo };
			}
		}

		return null;
	}

	/**
	 * Finds a class definition in the document with inheritance details
	 */
	private findClassDefinition(
		document: vscode.TextDocument,
		className: string
	): { name: string; inherits: string | null } | null {
		const text = document.getText();
		const lines = text.split('\n');

		const classPattern = new RegExp(`^\\s*:CLASS\\s+(${className})\\b`, 'i');

		for (let i = 0; i < lines.length; i++) {
			const match = lines[i].match(classPattern);
			if (match) {
				const name = match[1];

				// Look for :INHERIT on the next few lines
				let inherits: string | null = null;
				for (let j = i + 1; j < Math.min(i + 5, lines.length); j++) {
					const inheritMatch = lines[j].match(/^\s*:INHERIT\s+(\w+)/i);
					if (inheritMatch) {
						inherits = inheritMatch[1];
						break;
					}

					// Stop if we hit another significant keyword (not :INHERIT)
					if (/^\s*:(CLASS|PROCEDURE|DECLARE|IF|WHILE|FOR)\b/i.test(lines[j])) {
						break;
					}
				}

				return { name, inherits };
			}
		}

		return null;
	}

	/**
	 * Extracts parameter information including defaults
	 */
	private extractProcedureParameters(lines: string[], procLineIndex: number): string {
		const paramList: string[] = [];
		const defaults = new Map<string, string>();

		// Scan next lines for :PARAMETERS and :DEFAULT
		for (let j = procLineIndex + 1; j < Math.min(procLineIndex + 10, lines.length); j++) {
			const line = lines[j];

			// Check for :PARAMETERS
			const paramMatch = line.match(/^\s*:PARAMETERS\s+(.+?);/i);
			if (paramMatch) {
				// Split parameters by comma
				const params = paramMatch[1].split(',').map(p => p.trim());
				paramList.push(...params);
			}

			// Check for :DEFAULT - SSL syntax is ":DEFAULT paramName, value;"
			const defaultMatch = line.match(/^\s*:DEFAULT\s+(\w+)\s*,\s*(.+?);/i);
			if (defaultMatch) {
				defaults.set(defaultMatch[1], defaultMatch[2].trim());
			}

			// Stop if we hit another significant keyword (but not :PARAMETERS or :DEFAULT)
			if (/^\s*:(PROCEDURE|DECLARE|IF|WHILE|FOR|RETURN|BEGINCASE)\b/i.test(line) && 
			    !line.match(/^\s*:(PARAMETERS|DEFAULT)\b/i)) {
				break;
			}
		}

		// Build parameter string with defaults
		if (paramList.length === 0) {
			return '';
		}

		const paramStrings = paramList.map(param => {
			const defaultValue = defaults.get(param);
			return defaultValue ? `${param} = ${defaultValue}` : param;
		});

		return `(${paramStrings.join(', ')})`;
	}
}
