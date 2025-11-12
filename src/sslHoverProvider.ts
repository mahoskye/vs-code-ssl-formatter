import * as vscode from "vscode";

/**
 * SSL Hover Provider
 * Provides hover information for SSL keywords, functions, and symbols
 */
export class SSLHoverProvider implements vscode.HoverProvider {

	private functionDocs: Map<string, { description: string; params: string; returns: string }>;
	private keywordDocs: Map<string, string>;

	constructor() {
		this.functionDocs = new Map();
		this.keywordDocs = new Map();
		this.initializeFunctionDocs();
		this.initializeKeywordDocs();
	}

	public provideHover(
		document: vscode.TextDocument,
		position: vscode.Position,
		token: vscode.CancellationToken
	): vscode.Hover | null {
		const range = document.getWordRangeAtPosition(position);
		if (!range) {
			return null;
		}

		const word = document.getText(range);

		// Check if it's a keyword (with colon prefix)
		const lineText = document.lineAt(position.line).text;
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

		// Check for Hungarian notation hints
		const hungarianHint = this.getHungarianNotationHint(word);
		if (hungarianHint) {
			const markdown = new vscode.MarkdownString();
			markdown.appendMarkdown(hungarianHint);
			return new vscode.Hover(markdown, range);
		}

		return null;
	}

	private initializeKeywordDocs(): void {
		this.keywordDocs.set("IF", "Conditional statement - executes code block if condition is true");
		this.keywordDocs.set("ELSE", "Alternative code path when IF condition is false");
		this.keywordDocs.set("ELSEIF", "Additional condition to test if previous conditions are false");
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

		this.functionDocs.set("GETUSERSETTING", {
			description: "Get a user-specific setting value",
			params: "(settingName)",
			returns: "Setting value"
		});

		this.functionDocs.set("SETUSERSETTING", {
			description: "Set a user-specific setting value",
			params: "(settingName, value)",
			returns: "Boolean success status"
		});
	}

	private getHungarianNotationHint(word: string): string | null {
		if (word.length < 2) {
			return null;
		}

		const prefix = word[0].toLowerCase();
		const prefixMap: { [key: string]: string } = {
			"s": "String variable",
			"n": "Numeric variable",
			"b": "Boolean variable",
			"l": "Logical variable",
			"d": "Date variable",
			"a": "Array variable",
			"o": "Object variable (UDO)"
		};

		const hint = prefixMap[prefix];
		if (hint && word.length > 1 && word[1] === word[1].toUpperCase()) {
			return `**Hungarian Notation:** ${hint}`;
		}

		return null;
	}
}
