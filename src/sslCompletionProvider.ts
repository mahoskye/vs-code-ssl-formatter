import * as vscode from "vscode";

/**
 * SSL Completion Provider
 * Provides IntelliSense completion for SSL keywords, functions, and snippets
 */
export class SSLCompletionProvider implements vscode.CompletionItemProvider {

	private keywords: vscode.CompletionItem[] = [];
	private builtinFunctions: vscode.CompletionItem[] = [];
	private builtinClasses: vscode.CompletionItem[] = [];
	private snippets: vscode.CompletionItem[] = [];

	constructor() {
		this.initializeKeywords();
		this.initializeBuiltinFunctions();
		this.initializeBuiltinClasses();
		this.initializeSnippets();
	}

	public provideCompletionItems(
		document: vscode.TextDocument,
		position: vscode.Position,
		token: vscode.CancellationToken,
		context: vscode.CompletionContext
	): vscode.CompletionItem[] {
		const config = vscode.workspace.getConfiguration("ssl");
		const intellisenseEnabled = config.get<boolean>("intellisense.enabled", true);

		if (!intellisenseEnabled) {
			return [];
		}

		const completions: vscode.CompletionItem[] = [];

		// Add keywords
		completions.push(...this.keywords);

		// Add built-in functions
		completions.push(...this.builtinFunctions);

		// Add built-in classes
		completions.push(...this.builtinClasses);

		// Add snippets
		completions.push(...this.snippets);

		return completions;
	}

	private initializeKeywords(): void {
		const keywords = [
			{ name: "IF", description: "Conditional statement" },
			{ name: "ELSE", description: "Alternative condition" },
			{ name: "ELSEIF", description: "Additional condition" },
			{ name: "ENDIF", description: "End conditional block" },
			{ name: "WHILE", description: "While loop" },
			{ name: "ENDWHILE", description: "End while loop" },
			{ name: "FOR", description: "For loop" },
			{ name: "TO", description: "Loop upper bound" },
			{ name: "STEP", description: "Loop increment" },
			{ name: "NEXT", description: "End for loop" },
			{ name: "FOREACH", description: "For each loop" },
			{ name: "IN", description: "Collection to iterate" },
			{ name: "BEGINCASE", description: "Case statement" },
			{ name: "CASE", description: "Case condition" },
			{ name: "OTHERWISE", description: "Default case" },
			{ name: "ENDCASE", description: "End case statement" },
			{ name: "TRY", description: "Try block for error handling" },
			{ name: "CATCH", description: "Catch errors" },
			{ name: "FINALLY", description: "Always execute block" },
			{ name: "ENDTRY", description: "End try-catch block" },
			{ name: "PROCEDURE", description: "Define a procedure" },
			{ name: "ENDPROC", description: "End procedure" },
			{ name: "PARAMETERS", description: "Procedure parameters" },
			{ name: "DEFAULT", description: "Default parameter value" },
			{ name: "RETURN", description: "Return from procedure" },
			{ name: "DECLARE", description: "Declare variables" },
			{ name: "PUBLIC", description: "Public variable declaration" },
			{ name: "INCLUDE", description: "Include external file" },
			{ name: "REGION", description: "Start code region" },
			{ name: "ENDREGION", description: "End code region" },
			{ name: "CLASS", description: "Define a class" },
			{ name: "INHERIT", description: "Inherit from base class" },
			{ name: "ERROR", description: "Error marker" },
			{ name: "LABEL", description: "Label for GOTO" },
			{ name: "EXITFOR", description: "Exit for loop" },
			{ name: "EXITWHILE", description: "Exit while loop" },
			{ name: "EXITCASE", description: "Exit case statement" },
			{ name: "LOOP", description: "Loop control" },
			{ name: "RESUME", description: "Resume after error" }
		];

		this.keywords = keywords.map(kw => {
			const item = new vscode.CompletionItem(`:${kw.name}`, vscode.CompletionItemKind.Keyword);
			item.detail = kw.description;
			item.insertText = `:${kw.name}`;
			return item;
		});
	}

	private initializeBuiltinFunctions(): void {
		const functions = [
			{ name: "SQLExecute", description: "Execute SQL query", params: "(query, dataset)" },
			{ name: "DOPROC", description: "Call a procedure", params: "(procName, params)" },
			{ name: "EXECFUNCTION", description: "Execute a function", params: "(funcName, params)" },
			{ name: "EMPTY", description: "Check if value is empty", params: "(value)" },
			{ name: "LEN", description: "Get length of string or array", params: "(value)" },
			{ name: "USRMES", description: "Display message to user", params: "(message)" },
			{ name: "CHR", description: "Get character from ASCII code", params: "(code)" },
			{ name: "AADD", description: "Add element to array", params: "(array, element)" },
			{ name: "ALLTRIM", description: "Trim whitespace from string", params: "(string)" },
			{ name: "AT", description: "Find substring position", params: "(needle, haystack)" },
			{ name: "NOW", description: "Get current date and time", params: "()" },
			{ name: "TODAY", description: "Get current date", params: "()" },
			{ name: "CREATEUDOBJECT", description: "Create UDO object", params: "(className, params)" },
			{ name: "BUILDSTRING", description: "Build formatted string", params: "(format, values)" },
			{ name: "ASCAN", description: "Search array", params: "(array, value)" },
			{ name: "ALEN", description: "Get array length", params: "(array)" },
			{ name: "ARRAYCALC", description: "Calculate array values", params: "(array, expression)" },
			{ name: "BUILDARRAY", description: "Build array from values", params: "(values)" },
			{ name: "DIRECTORY", description: "Get directory listing", params: "(path)" },
			{ name: "CREATEGUID", description: "Create GUID", params: "()" },
			{ name: "Left", description: "Get left portion of string", params: "(string, count)" },
			{ name: "Right", description: "Get right portion of string", params: "(string, count)" },
			{ name: "SubStr", description: "Get substring", params: "(string, start, length)" },
			{ name: "Upper", description: "Convert to uppercase", params: "(string)" },
			{ name: "Lower", description: "Convert to lowercase", params: "(string)" },
			{ name: "Val", description: "Convert string to number", params: "(string)" },
			{ name: "CtoD", description: "Convert string to date", params: "(dateString)" },
			{ name: "GetSetting", description: "Get system setting", params: "(settingName)" },
			{ name: "GetUserSetting", description: "Get user setting", params: "(settingName)" },
			{ name: "SetUserSetting", description: "Set user setting", params: "(settingName, value)" }
		];

		this.builtinFunctions = functions.map(func => {
			const item = new vscode.CompletionItem(func.name, vscode.CompletionItemKind.Function);
			item.detail = func.description;
			item.insertText = new vscode.SnippetString(`${func.name}($1)`);
			item.documentation = new vscode.MarkdownString(`**${func.name}**${func.params}\n\n${func.description}`);
			return item;
		});
	}

	private initializeBuiltinClasses(): void {
		const classes = [
			{
				name: "Email",
				description: "Built-in email class for sending emails",
				instantiation: "Email{}",
				usage: "oEmail := Email{}; oEmail:Subject := 'Test'; oEmail:Send();"
			},
			{
				name: "SSLRegex",
				description: "Built-in regular expression class for pattern matching",
				instantiation: "SSLRegex{}",
				usage: "oRegex := SSLRegex{}; result := oRegex:Match(pattern, text);"
			}
		];

		this.builtinClasses = classes.map(cls => {
			const item = new vscode.CompletionItem(cls.name, vscode.CompletionItemKind.Class);
			item.detail = cls.description;
			item.insertText = new vscode.SnippetString(`${cls.name}{}`);
			item.documentation = new vscode.MarkdownString(
				`**${cls.name}** - Built-in class\n\n` +
				`${cls.description}\n\n` +
				`**Instantiation:** \`${cls.instantiation}\`\n\n` +
				`**Example:** \`${cls.usage}\`\n\n` +
				`Use colon notation for methods and properties (e.g., \`object:method()\`, \`object:property\`)`
			);
			return item;
		});
	}

	private initializeSnippets(): void {
		// IF/ELSE snippet
		const ifSnippet = new vscode.CompletionItem("if", vscode.CompletionItemKind.Snippet);
		ifSnippet.insertText = new vscode.SnippetString(":IF ${1:condition};\n\t$0\n:ENDIF;");
		ifSnippet.detail = "IF statement";
		ifSnippet.documentation = "Create an IF conditional block";

		// IF/ELSE/ENDIF snippet
		const ifElseSnippet = new vscode.CompletionItem("ifelse", vscode.CompletionItemKind.Snippet);
		ifElseSnippet.insertText = new vscode.SnippetString(":IF ${1:condition};\n\t$2\n:ELSE;\n\t$3\n:ENDIF;");
		ifElseSnippet.detail = "IF/ELSE statement";
		ifElseSnippet.documentation = "Create an IF/ELSE conditional block";

		// FOR loop snippet
		const forSnippet = new vscode.CompletionItem("for", vscode.CompletionItemKind.Snippet);
		forSnippet.insertText = new vscode.SnippetString(":FOR ${1:i} := ${2:1};\n:TO ${3:10};\n:STEP ${4:1};\n\t$0\n:NEXT;");
		forSnippet.detail = "FOR loop";
		forSnippet.documentation = "Create a FOR loop";

		// WHILE loop snippet
		const whileSnippet = new vscode.CompletionItem("while", vscode.CompletionItemKind.Snippet);
		whileSnippet.insertText = new vscode.SnippetString(":WHILE ${1:condition};\n\t$0\n:ENDWHILE;");
		whileSnippet.detail = "WHILE loop";
		whileSnippet.documentation = "Create a WHILE loop";

		// PROCEDURE snippet
		const procSnippet = new vscode.CompletionItem("procedure", vscode.CompletionItemKind.Snippet);
		procSnippet.insertText = new vscode.SnippetString(
			"/************************************************************\nDescription.. : ${1:Brief description}\nParameters... : ${2:Parameters}\nReturns...... : ${3:Return value}\nAuthor....... : ${4:Initials}\nDate......... : ${5:YYYY-MM-DD}\n************************************************************/\n:PROCEDURE ${6:ProcedureName};\n:PARAMETERS ${7:params};\n:DEFAULT ${8:param}, ${9:value};\n\n\t$0\n\n:RETURN ${10:result};\n:ENDPROC;"
		);
		procSnippet.detail = "PROCEDURE with header";
		procSnippet.documentation = "Create a procedure with documentation header";

		// TRY/CATCH snippet
		const trySnippet = new vscode.CompletionItem("try", vscode.CompletionItemKind.Snippet);
		trySnippet.insertText = new vscode.SnippetString(":TRY;\n\t$1\n:CATCH;\n\t$2\n:ENDTRY;");
		trySnippet.detail = "TRY/CATCH block";
		trySnippet.documentation = "Create a TRY/CATCH error handling block";

		// REGION snippet
		const regionSnippet = new vscode.CompletionItem("region", vscode.CompletionItemKind.Snippet);
		regionSnippet.insertText = new vscode.SnippetString(":REGION ${1:Region Name};\n\t$0\n:ENDREGION;");
		regionSnippet.detail = "REGION block";
		regionSnippet.documentation = "Create a region for code organization";

		// CASE snippet
		const caseSnippet = new vscode.CompletionItem("case", vscode.CompletionItemKind.Snippet);
		caseSnippet.insertText = new vscode.SnippetString(":BEGINCASE;\n:CASE ${1:condition1};\n\t$2\n:CASE ${3:condition2};\n\t$4\n:OTHERWISE;\n\t$5\n:ENDCASE;");
		caseSnippet.detail = "CASE statement";
		caseSnippet.documentation = "Create a CASE statement";

		this.snippets = [
			ifSnippet,
			ifElseSnippet,
			forSnippet,
			whileSnippet,
			procSnippet,
			trySnippet,
			regionSnippet,
			caseSnippet
		];
	}
}
