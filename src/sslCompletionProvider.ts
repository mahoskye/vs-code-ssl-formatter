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

		const lineText = document.lineAt(position.line).text;
		const textBeforeCursor = lineText.substring(0, position.character);

		// Check if we're inside a DoProc call suggesting procedure names
		const doProcMatch = textBeforeCursor.match(/DoProc\s*\(\s*["']([^"']*)$/i);
		if (doProcMatch) {
			return this.getProcedureCompletions(document);
		}

		// Check if this is an object member completion (after ':')
		if (context.triggerCharacter === ':') {
			const textBeforeColon = lineText.substring(0, position.character - 1);

			// Get the object variable name before the colon
			const objectMatch = textBeforeColon.match(/(\w+)\s*$/);
			if (objectMatch) {
				const objectName = objectMatch[1];
				const memberCompletions = this.getObjectMemberCompletions(document, objectName);
				if (memberCompletions.length > 0) {
					return memberCompletions;
				}
			}
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
			{ name: "ELSE", description: "Alternative condition (use nested :IF for else-if logic)" },
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
			{ name: "BEGININLINECODE", description: "Begin inline code block" },
			{ name: "ENDINLINECODE", description: "End inline code block" },
			{ name: "ERROR", description: "Error marker" },
			{ name: "LABEL", description: "Label for GOTO" },
			{ name: "EXITFOR", description: "Exit for loop" },
			{ name: "EXITWHILE", description: "Exit while loop" },
			{ name: "EXITCASE", description: "Exit case statement" },
			{ name: "LOOP", description: "Loop control" },
			{ name: "RESUME", description: "Resume after error" }
		];

		this.keywords = keywords.map(kw => {
			const item = new vscode.CompletionItem(kw.name, vscode.CompletionItemKind.Keyword);
			item.detail = kw.description;
			item.insertText = kw.name;
			item.filterText = `:${kw.name}`;
			return item;
		});
	}

	private initializeBuiltinFunctions(): void {
		const functions = [
			{ name: "SQLExecute", description: "Execute SQL query", params: "(query, dataset)" },
			{ name: "DoProc", description: "Call a procedure", params: "(procName, params)" },
			{ name: "ExecFunction", description: "Execute a function", params: "(funcName, params)" },
			{ name: "Empty", description: "Check if value is empty", params: "(value)" },
			{ name: "Len", description: "Get length of string or array", params: "(value)" },
			{ name: "usrmes", description: "Display message to user", params: "(message1, message2)" },
			{ name: "Chr", description: "Get character from ASCII code", params: "(code)" },
			{ name: "aadd", description: "Add element to array", params: "(array, element)" },
			{ name: "AllTrim", description: "Trim whitespace from string", params: "(string)" },
			{ name: "At", description: "Find substring position", params: "(needle, haystack)" },
			{ name: "Now", description: "Get current date and time", params: "()" },
			{ name: "Today", description: "Get current date", params: "()" },
			{ name: "CreateUDObject", description: "Create UDO object", params: "(className, params)" },
			{ name: "buildstring", description: "Build formatted string", params: "(format, values)" },
			{ name: "ascan", description: "Search array", params: "(array, value)" },
			{ name: "alen", description: "Get array length", params: "(array)" },
			{ name: "arraycalc", description: "Calculate array values", params: "(array, expression)" },
			{ name: "buildarray", description: "Build array from values", params: "(values)" },
			{ name: "Directory", description: "Get directory listing", params: "(path)" },
			{ name: "CreateGUID", description: "Create GUID", params: "()" },
			{ name: "Left", description: "Get left portion of string", params: "(string, count)" },
			{ name: "Right", description: "Get right portion of string", params: "(string, count)" },
			{ name: "SubStr", description: "Get substring", params: "(string, start, length)" },
			{ name: "Upper", description: "Convert to uppercase", params: "(string)" },
			{ name: "Lower", description: "Convert to lowercase", params: "(string)" },
			{ name: "Val", description: "Convert string to number", params: "(string)" },
			{ name: "CToD", description: "Convert string to date", params: "(dateString)" },
			{ name: "GetSetting", description: "Get system setting", params: "(settingName)" },
			{ name: "GetUserData", description: "Get user data", params: "(key)" },
			{ name: "SetUserData", description: "Set user data", params: "(key, value)" }
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

	/**
	 * Get member completions for an object based on its type
	 */
	private getObjectMemberCompletions(
		document: vscode.TextDocument,
		objectName: string
	): vscode.CompletionItem[] {
		const text = document.getText();
		const lines = text.split('\n');

		// Find the object's instantiation to determine its type
		const objectType = this.findObjectType(lines, objectName);

		if (!objectType) {
			return [];
		}

		// Built-in classes
		if (objectType.type === 'builtin') {
			return this.getBuiltinClassMembers(objectType.className!);
		}

		// User-defined classes
		if (objectType.type === 'userclass') {
			return this.getUserDefinedClassMembers(lines, objectType.className!);
		}

		// Anonymous objects - scan for property assignments
		if (objectType.type === 'anonymous') {
			return this.getAnonymousObjectMembers(lines, objectName);
		}

		return [];
	}

	/**
	 * Determine the type of an object by finding its instantiation
	 */
	private findObjectType(
		lines: string[],
		objectName: string
	): { type: 'builtin' | 'userclass' | 'anonymous'; className?: string } | null {
		for (const line of lines) {
			// Check for built-in class instantiation: oEmail := Email{}
			const builtinMatch = line.match(new RegExp(`\\b${objectName}\\s*:=\\s*(\\w+)\\{\\}`, 'i'));
			if (builtinMatch) {
				const className = builtinMatch[1].toUpperCase();
				if (className === 'EMAIL' || className === 'SSLREGEX') {
					return { type: 'builtin', className };
				}
			}

			// Check for user-defined class: oHandler := CreateUDObject("ClassName")
			const userClassMatch = line.match(new RegExp(`\\b${objectName}\\s*:=\\s*CreateUDObject\\s*\\(\\s*["']([^"']+)["']`, 'i'));
			if (userClassMatch) {
				const fullClassName = userClassMatch[1];
				// Handle namespace: "Namespace.ClassName" -> "ClassName"
				const classParts = fullClassName.split('.');
				const className = classParts[classParts.length - 1];
				return { type: 'userclass', className };
			}

			// Check for anonymous object: oVar := CreateUDObject()
			const anonymousMatch = line.match(new RegExp(`\\b${objectName}\\s*:=\\s*CreateUDObject\\s*\\(\\s*\\)`, 'i'));
			if (anonymousMatch) {
				return { type: 'anonymous' };
			}
		}

		return null;
	}

	/**
	 * Get members for built-in classes (Email, SSLRegex)
	 */
	private getBuiltinClassMembers(className: string): vscode.CompletionItem[] {
		const members: vscode.CompletionItem[] = [];

		if (className === 'EMAIL') {
			// Methods
			const send = new vscode.CompletionItem('Send', vscode.CompletionItemKind.Method);
			send.detail = 'Send email';
			send.insertText = new vscode.SnippetString('Send($0)');
			members.push(send);

			const addAttachment = new vscode.CompletionItem('AddAttachment', vscode.CompletionItemKind.Method);
			addAttachment.detail = 'Add attachment to email';
			addAttachment.insertText = new vscode.SnippetString('AddAttachment($1)');
			members.push(addAttachment);

			const setRecipient = new vscode.CompletionItem('SetRecipient', vscode.CompletionItemKind.Method);
			setRecipient.detail = 'Set email recipient';
			setRecipient.insertText = new vscode.SnippetString('SetRecipient($1)');
			members.push(setRecipient);

			// Properties
			['Subject', 'Body', 'From', 'To'].forEach(prop => {
				const item = new vscode.CompletionItem(prop, vscode.CompletionItemKind.Property);
				item.detail = `Email ${prop.toLowerCase()}`;
				members.push(item);
			});
		} else if (className === 'SSLREGEX') {
			// Methods
			const match = new vscode.CompletionItem('Match', vscode.CompletionItemKind.Method);
			match.detail = 'Match pattern against text';
			match.insertText = new vscode.SnippetString('Match($1, $2)');
			members.push(match);

			const replace = new vscode.CompletionItem('Replace', vscode.CompletionItemKind.Method);
			replace.detail = 'Replace pattern in text';
			replace.insertText = new vscode.SnippetString('Replace($1, $2, $3)');
			members.push(replace);

			const test = new vscode.CompletionItem('Test', vscode.CompletionItemKind.Method);
			test.detail = 'Test if pattern matches';
			test.insertText = new vscode.SnippetString('Test($1, $2)');
			members.push(test);

			// Properties
			['Pattern', 'IgnoreCase', 'Multiline'].forEach(prop => {
				const item = new vscode.CompletionItem(prop, vscode.CompletionItemKind.Property);
				item.detail = `Regex ${prop}`;
				members.push(item);
			});
		}

		return members;
	}

	/**
	 * Get members for user-defined classes by scanning the class definition
	 */
	private getUserDefinedClassMembers(lines: string[], className: string): vscode.CompletionItem[] {
		const members: vscode.CompletionItem[] = [];
		const classPattern = new RegExp(`^\\s*:CLASS\\s+${className}\\b`, 'i');

		let inClass = false;
		for (const line of lines) {
			if (classPattern.test(line)) {
				inClass = true;
				continue;
			}

			if (inClass) {
				// Look for property/method definitions (simplified - just look for :PROCEDURE or variable patterns)
				const procMatch = line.match(/^\s*:PROCEDURE\s+(\w+)/i);
				if (procMatch) {
					const method = new vscode.CompletionItem(procMatch[1], vscode.CompletionItemKind.Method);
					method.detail = 'User-defined method';
					members.push(method);
				}

				// Exit class when we hit another :CLASS or end of likely class content
				if (/^\s*:(CLASS|PROCEDURE\s+\w+.*\s+:ENDPROC)/i.test(line) && !procMatch) {
					break;
				}
			}
		}

		return members;
	}

	/**
	 * Get procedure completions from the current document and workspace
	 */
	private getProcedureCompletions(document: vscode.TextDocument): vscode.CompletionItem[] {
		const completions: vscode.CompletionItem[] = [];
		const procedureNames = new Set<string>();
		const text = document.getText();
		const lines = text.split('\n');

		// Pattern to match :PROCEDURE ProcedureName
		const procedurePattern = /^\s*:PROCEDURE\s+(\w+)/i;

		for (const line of lines) {
			const match = line.match(procedurePattern);
			if (match) {
				const procName = match[1];
				if (!procedureNames.has(procName)) {
					procedureNames.add(procName);
					const item = new vscode.CompletionItem(procName, vscode.CompletionItemKind.Function);
					item.detail = 'User-defined procedure';
					item.insertText = procName;
					completions.push(item);
				}
			}
		}

		return completions;
	}

	/**
	 * Get members for anonymous objects by scanning for property assignments
	 */
	private getAnonymousObjectMembers(lines: string[], objectName: string): vscode.CompletionItem[] {
		const members: vscode.CompletionItem[] = [];
		const memberNames = new Set<string>();

		// Pattern: objectName:propertyName := value
		const memberPattern = new RegExp(`\\b${objectName}:(\\w+)\\s*:=`, 'i');

		for (const line of lines) {
			const match = line.match(memberPattern);
			if (match) {
				const memberName = match[1];
				if (!memberNames.has(memberName)) {
					memberNames.add(memberName);
					const item = new vscode.CompletionItem(memberName, vscode.CompletionItemKind.Property);
					item.detail = `Property of ${objectName}`;
					members.push(item);
				}
			}
		}

		return members;
	}
}
