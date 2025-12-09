import * as vscode from "vscode";
import {
	SSL_KEYWORDS,
	SSL_KEYWORD_DESCRIPTIONS,
	SSLClass,
	SSLFunction
} from "./constants/language";
import {
	CONFIG_KEYS,
	CONFIG_DEFAULTS
} from "./constants/config";
import {
	PATTERNS
} from "./constants/patterns";
import { ClassIndex, ClassMembers } from "./utils/classIndex";
import { ProcedureIndex } from "./utils/procedureIndex";
import { getConfiguredClasses, getConfiguredFunctions } from "./utils/intellisense";

interface DocumentAnalysis {
	variableTypes: Map<string, { type: 'builtin' | 'userclass' | 'anonymous', className?: string }>;
	localProcedures: Set<string>;
	anonymousMembers: Map<string, { methods: Set<string>, properties: Set<string> }>;
	userClasses: Map<string, ClassMembers>;
}

/**
 * SSL Completion Provider
 * Provides IntelliSense completion for SSL keywords, functions, and snippets
 */
export class SSLCompletionProvider implements vscode.CompletionItemProvider {

	private keywords: vscode.CompletionItem[] = [];
	private builtinFunctions: vscode.CompletionItem[] = [];
	private builtinClasses: vscode.CompletionItem[] = [];
	private snippets: vscode.CompletionItem[] = [];
	private builtinClassMetadata: Map<string, SSLClass> = new Map();
	private builtinClassNames: Set<string> = new Set();

	constructor(
		private readonly classIndex?: ClassIndex,
		private readonly procedureIndex?: ProcedureIndex
	) {
		this.initializeKeywords();
		const config = vscode.workspace.getConfiguration("ssl");
		this.refreshFromConfig(config);
		this.initializeSnippets();
	}

	public provideCompletionItems(
		document: vscode.TextDocument,
		position: vscode.Position,
		token: vscode.CancellationToken,
		context: vscode.CompletionContext
	): vscode.CompletionItem[] {
		const config = vscode.workspace.getConfiguration("ssl");
		this.refreshFromConfig(config);
		const intellisenseEnabled = config.get<boolean>(CONFIG_KEYS.INTELLISENSE_ENABLED, CONFIG_DEFAULTS[CONFIG_KEYS.INTELLISENSE_ENABLED]);

		if (!intellisenseEnabled) {
			return [];
		}

		const keywordsEnabled = config.get<boolean>(CONFIG_KEYS.INTELLISENSE_COMPLETION_KEYWORDS_ENABLED, CONFIG_DEFAULTS[CONFIG_KEYS.INTELLISENSE_COMPLETION_KEYWORDS_ENABLED]);
		const functionsEnabled = config.get<boolean>(CONFIG_KEYS.INTELLISENSE_COMPLETION_FUNCTIONS_ENABLED, CONFIG_DEFAULTS[CONFIG_KEYS.INTELLISENSE_COMPLETION_FUNCTIONS_ENABLED]);
		const classesEnabled = config.get<boolean>(CONFIG_KEYS.INTELLISENSE_COMPLETION_CLASSES_ENABLED, CONFIG_DEFAULTS[CONFIG_KEYS.INTELLISENSE_COMPLETION_CLASSES_ENABLED]);
		const snippetsEnabled = config.get<boolean>(CONFIG_KEYS.INTELLISENSE_COMPLETION_SNIPPETS_ENABLED, CONFIG_DEFAULTS[CONFIG_KEYS.INTELLISENSE_COMPLETION_SNIPPETS_ENABLED]);

		const lineText = document.lineAt(position.line).text;
		const textBeforeCursor = lineText.substring(0, position.character);

		// Single-pass analysis of document
		const analysis = this.analyzeDocument(document);

		// Check if we're inside a DoProc or ExecFunction call
		const doProcMatch = textBeforeCursor.match(PATTERNS.DOPROC.COMPLETION);
		const execFunctionMatch = textBeforeCursor.match(PATTERNS.EXEC_FUNCTION.COMPLETION);
		if (doProcMatch || execFunctionMatch) {
			const partialInput = doProcMatch ? doProcMatch[1] : (execFunctionMatch ? execFunctionMatch[1] : '');
			return this.getProcedureCompletions(document, analysis, partialInput);
		}

		// Check if this is an object member completion (after ':')
		if (context.triggerCharacter === ':') {
			const textBeforeColon = lineText.substring(0, position.character - 1);
			const objectMatch = textBeforeColon.match(/(\w+)\s*$/);

			if (objectMatch) {
				const objectName = objectMatch[1];
				const memberCompletions = this.getObjectMemberCompletions(objectName, analysis);
				if (memberCompletions.length > 0) {
					return memberCompletions;
				}
			}
		}

		const completions: vscode.CompletionItem[] = [];

		if (keywordsEnabled !== false) {
			completions.push(...this.keywords);
		}
		if (functionsEnabled !== false) {
			completions.push(...this.builtinFunctions);
		}
		if (classesEnabled !== false) {
			completions.push(...this.builtinClasses);
		}
		if (snippetsEnabled !== false) {
			completions.push(...this.snippets);
		}

		return completions;
	}

	/**
	 * Single-pass analysis of the document to extract types, members, and procedures.
	 */
	private analyzeDocument(document: vscode.TextDocument): DocumentAnalysis {
		const text = document.getText();
		const analysis: DocumentAnalysis = {
			variableTypes: new Map(),
			localProcedures: new Set(),
			anonymousMembers: new Map(),
			userClasses: new Map()
		};

		const lines = text.split(/\r?\n/);
		let currentClass: string | null = null;
		let inCommentBlock = false;

		for (const line of lines) {
			const trimmed = line.trim();
			if (!trimmed) { continue; }

			// Handle comments
			if (inCommentBlock) {
				if (trimmed.endsWith('*/') || trimmed.includes('*/')) {
					inCommentBlock = false;
				}
				continue; // Improve: handle code after */ on same line? Assume strict block for now
			}
			if (trimmed.startsWith('/*')) {
				if (!trimmed.endsWith('*/') && !trimmed.includes('*/')) {
					inCommentBlock = true;
				}
				// If it ends on same line, it's a one-line block comment, continue parsing?
				// For simplicity, skip line if it starts with comment block
				continue;
			}
			if (trimmed.startsWith('//')) {
				continue;
			}

			// Remove inline comments for parsing
			const codeLine = line.split('//')[0].split('/*')[0]; // Simple strip

			// 1. Procedure Definition
			const procMatch = codeLine.match(/^\s*:PROCEDURE\s+(\w+)/i);
			if (procMatch) {
				analysis.localProcedures.add(procMatch[1]);
				if (currentClass) {
					// Method of current class
					const method = procMatch[1];
					if (!analysis.userClasses.has(currentClass)) {
						analysis.userClasses.set(currentClass, { className: currentClass, methods: [], properties: [] });
					}
					analysis.userClasses.get(currentClass)!.methods.push(method);
				}
			}

			// 2. Class Definition
			const classMatch = codeLine.match(/^\s*:CLASS\s+(\w+)/i);
			if (classMatch) {
				currentClass = classMatch[1];
				continue;
			}
			if (/^\s*:ENDCLASS/i.test(codeLine)) {
				// Should not happen if strictly one class per file to end, but if user types it, ignore or treat as end?
				// User says "class declarations to the end of file". 
				// So we generally assume class continues.
				// However, maintaining ability to break out if user explicitly types invalid keyword might be good/bad?
				// Let's remove it as it's invalid.
				// currentClass = null; 
			}

			// 3. Variable Instantiation / Assignment
			// Builtin: var := Class{}
			const builtinMatch = codeLine.match(/(\w+)\s*:=\s*(\w+)\s*\{\s*\}/);
			if (builtinMatch) {
				const [_, varName, className] = builtinMatch;
				if (this.builtinClassNames.has(className.toUpperCase())) {
					analysis.variableTypes.set(varName, { type: 'builtin', className: className.toUpperCase() });
				}
			}

			// User Class: var := CreateUDObject("Class")
			const userClassMatch = codeLine.match(/(\w+)\s*:=\s*CreateUDObject\s*\(\s*["']([^"']+)["']/i);
			if (userClassMatch) {
				const [_, varName, fullClassName] = userClassMatch;
				const className = fullClassName.split('.').pop()!;
				analysis.variableTypes.set(varName, { type: 'userclass', className });
			}

			// Anonymous: var := CreateUDObject()
			const anonMatch = codeLine.match(/(\w+)\s*:=\s*CreateUDObject\s*\(\s*\)/i);
			if (anonMatch) {
				analysis.variableTypes.set(anonMatch[1], { type: 'anonymous' });
			}

			// 4. Object Member Assignment / Dynamic Add
			// obj:prop := val
			const propAssignMatch = codeLine.match(/(\w+):(\w+)\s*:=/);
			if (propAssignMatch) {
				const [_, objName, propName] = propAssignMatch;
				this.addAnonymousMember(analysis, objName, 'property', propName);
			}

			// obj:AddProperty("prop")
			const addPropMatch = codeLine.match(/(\w+):AddProperty\s*\(\s*["']([^"']+)["']/i);
			if (addPropMatch) {
				this.addAnonymousMember(analysis, addPropMatch[1], 'property', addPropMatch[2]);
			}

			// obj:AddMethod("method")
			const addMethodMatch = codeLine.match(/(\w+):AddMethod\s*\(\s*["']([^"']+)["']/i);
			if (addMethodMatch) {
				this.addAnonymousMember(analysis, addMethodMatch[1], 'method', addMethodMatch[2]);
			}
		}
		return analysis;
	}

	private addAnonymousMember(analysis: DocumentAnalysis, objName: string, type: 'method' | 'property', name: string) {
		if (!analysis.anonymousMembers.has(objName)) {
			analysis.anonymousMembers.set(objName, { methods: new Set(), properties: new Set() });
		}
		const members = analysis.anonymousMembers.get(objName)!;
		if (type === 'method') {
			members.methods.add(name);
		} else {
			members.properties.add(name);
		}
	}

	private initializeKeywords(): void {
		this.keywords = SSL_KEYWORDS.map(kw => {
			const item = new vscode.CompletionItem(kw, vscode.CompletionItemKind.Keyword);
			item.detail = SSL_KEYWORD_DESCRIPTIONS[kw];
			item.insertText = kw;
			item.filterText = `:${kw}`;
			return item;
		});
	}

	private initializeBuiltinFunctions(functions: SSLFunction[]): void {
		this.builtinFunctions = functions.map(func => {
			const item = new vscode.CompletionItem(func.name, vscode.CompletionItemKind.Function);
			item.detail = func.description || "Custom function";
			const signature = func.signature || `${func.name}${func.params || "()"}`;
			item.insertText = new vscode.SnippetString(`${func.name}($1)`);

			let documentation = `**${func.name}**\n\n${func.description || "Custom function"}`;
			if (func.signature) {
				documentation += `\n\n**Signature:** \`${func.signature}\``;
			}
			if (func.returnType) {
				documentation += `\n\n**Returns:** \`${func.returnType}\``;
			}
			if (func.category) {
				documentation += `\n\n**Category:** ${func.category}`;
			}
			if (func.frequency) {
				documentation += `\n\n**Usage:** ${func.frequency}`;
			}
			if (func.untypedSignature) {
				documentation += `\n\n**Untyped:** \`${func.untypedSignature}\``;
			}

			item.documentation = new vscode.MarkdownString(documentation);
			return item;
		});
	}

	private initializeBuiltinClasses(classes: SSLClass[]): void {
		this.builtinClasses = classes.map(cls => {
			const item = new vscode.CompletionItem(cls.name, vscode.CompletionItemKind.Class);
			item.detail = cls.description;
			item.insertText = new vscode.SnippetString(`${cls.name}{}`);
			const methods = cls.methods || [];
			const properties = cls.properties || [];
			item.documentation = new vscode.MarkdownString(
				`**${cls.name}** - Built-in class\n\n` +
				`${cls.description}\n\n` +
				`**Instantiation:** \`${cls.instantiation}\`\n\n` +
				`${cls.usage ? `**Example:** \`${cls.usage}\`\n\n` : ""}` +
				`${methods.length ? `**Methods:** ${methods.map(m => `\`${m}()\``).join(", ")}\n\n` : ""}` +
				`${properties.length ? `**Properties:** ${properties.map(p => `\`${p}\``).join(", ")}\n\n` : ""}` +
				`Use colon notation for methods and properties (e.g., \`object:method()\`, \`object:property\`)`
			);
			return item;
		});
	}

	private initializeSnippets(): void {
		// Basic snippets (implementation unchanged from original, just collapsed here for brevity in logic review)
		// ... (Re-implementing fully to ensure no data loss)
		const ifSnippet = new vscode.CompletionItem("if", vscode.CompletionItemKind.Snippet);
		ifSnippet.insertText = new vscode.SnippetString(":IF ${1:condition};\n\t$0\n:ENDIF;");
		ifSnippet.detail = "IF statement";

		const ifElseSnippet = new vscode.CompletionItem("ifelse", vscode.CompletionItemKind.Snippet);
		ifElseSnippet.insertText = new vscode.SnippetString(":IF ${1:condition};\n\t$2\n:ELSE;\n\t$3\n:ENDIF;");
		ifElseSnippet.detail = "IF/ELSE statement";

		const forSnippet = new vscode.CompletionItem("for", vscode.CompletionItemKind.Snippet);
		forSnippet.insertText = new vscode.SnippetString(":FOR ${1:i} := ${2:1};\n:TO ${3:10};\n:STEP ${4:1};\n\t$0\n:NEXT;");
		forSnippet.detail = "FOR loop";

		const whileSnippet = new vscode.CompletionItem("while", vscode.CompletionItemKind.Snippet);
		whileSnippet.insertText = new vscode.SnippetString(":WHILE ${1:condition};\n\t$0\n:ENDWHILE;");
		whileSnippet.detail = "WHILE loop";

		const procSnippet = new vscode.CompletionItem("procedure", vscode.CompletionItemKind.Snippet);
		procSnippet.insertText = new vscode.SnippetString(
			"/************************************************************\nDescription.. : ${1:Brief description}\nParameters... : ${2:Parameters}\nReturns...... : ${3:Return value}\nAuthor....... : ${4:Initials}\nDate......... : ${5:YYYY-MM-DD}\n************************************************************/\n:PROCEDURE ${6:ProcedureName};\n:PARAMETERS ${7:params};\n:DEFAULT ${8:param}, ${9:value};\n\n\t$0\n\n:RETURN ${10:result};\n:ENDPROC;"
		);
		procSnippet.detail = "PROCEDURE with header";

		const trySnippet = new vscode.CompletionItem("try", vscode.CompletionItemKind.Snippet);
		trySnippet.insertText = new vscode.SnippetString(":TRY;\n\t$1\n:CATCH;\n\t$2\n:ENDTRY;");
		trySnippet.detail = "TRY/CATCH block";

		const regionSnippet = new vscode.CompletionItem("region", vscode.CompletionItemKind.Snippet);
		regionSnippet.insertText = new vscode.SnippetString(":REGION ${1:Region Name};\n\t$0\n:ENDREGION;");
		regionSnippet.detail = "REGION block";

		const caseSnippet = new vscode.CompletionItem("case", vscode.CompletionItemKind.Snippet);
		caseSnippet.insertText = new vscode.SnippetString(":BEGINCASE;\n:CASE ${1:condition1};\n\t$2\n:CASE ${3:condition2};\n\t$4\n:OTHERWISE;\n\t$5\n:ENDCASE;");
		caseSnippet.detail = "CASE statement";

		this.snippets = [ifSnippet, ifElseSnippet, forSnippet, whileSnippet, procSnippet, trySnippet, regionSnippet, caseSnippet];
	}

	private getObjectMemberCompletions(
		objectName: string,
		analysis: DocumentAnalysis
	): vscode.CompletionItem[] {
		// Check analyzed local types first
		if (analysis.variableTypes.has(objectName)) {
			const typeInfo = analysis.variableTypes.get(objectName)!;
			if (typeInfo.type === 'builtin') {
				return this.getBuiltinClassMembers(typeInfo.className!);
			}
			if (typeInfo.type === 'userclass') {
				// Check if local class definition exists first
				if (analysis.userClasses.has(typeInfo.className!)) {
					const members = analysis.userClasses.get(typeInfo.className!)!;
					return this.createClassMemberItems(members);
				}
				// Fallback to index
				const indexedMembers = this.classIndex?.getClassMembers(typeInfo.className!);
				if (indexedMembers) {
					return this.createClassMemberItems(indexedMembers);
				}
				// Fallback to dynamic scan? No, analysis would have caught it if it was local.
			}
			if (typeInfo.type === 'anonymous') {
				return this.getAnonymousObjectMembers(objectName, analysis);
			}
		}

		return [];
	}

	private getBuiltinClassMembers(className: string): vscode.CompletionItem[] {
		const cls = this.builtinClassMetadata.get(className.toUpperCase());
		if (!cls) {
			return [];
		}

		const members: vscode.CompletionItem[] = [];
		(cls.methods || []).forEach(method => {
			const item = new vscode.CompletionItem(method, vscode.CompletionItemKind.Method);
			item.detail = `Method defined on ${cls.name}`;
			item.insertText = new vscode.SnippetString(`${method}($1)`);
			members.push(item);
		});
		(cls.properties || []).forEach(prop => {
			const item = new vscode.CompletionItem(prop, vscode.CompletionItemKind.Property);
			item.detail = `Property of ${cls.name}`;
			members.push(item);
		});
		return members;
	}

	private getProcedureCompletions(document: vscode.TextDocument, analysis: DocumentAnalysis, partialInput: string = ''): vscode.CompletionItem[] {
		const completions: vscode.CompletionItem[] = [];
		const addedKeys = new Set<string>();

		// Local procedures (from analysis)
		for (const procName of analysis.localProcedures) {
			// console.log(`[DEBUG] Processing local procedure: ${procName}`);
			const key = procName.toLowerCase();
			if (!addedKeys.has(key)) {
				// console.log(`[DEBUG] Adding local procedure to completions: ${procName}`);
				addedKeys.add(key);
				const item = new vscode.CompletionItem(procName, vscode.CompletionItemKind.Function);
				item.detail = 'Procedure (current file)';
				item.insertText = procName;
				item.sortText = `0_${procName}`;
				completions.push(item);
			}
		}

		// Workspace procedures
		if (this.procedureIndex) {
			const allProcedures = this.procedureIndex.getAllProcedures();
			const currentFileUri = document.uri.toString();

			for (const proc of allProcedures) {
				if (proc.uri.toString() === currentFileUri) {
					continue;
				}

				const qualifiedNames = this.buildQualifiedNames(proc);
				for (const qualifiedName of qualifiedNames) {
					const key = qualifiedName.toLowerCase();
					if (!addedKeys.has(key)) {
						if (partialInput && !key.startsWith(partialInput.toLowerCase())) {
							continue;
						}

						addedKeys.add(key);
						const item = new vscode.CompletionItem(qualifiedName, vscode.CompletionItemKind.Function);
						item.detail = `Procedure in ${proc.fileBaseName}.ssl`;
						item.insertText = qualifiedName;
						item.sortText = `1_${qualifiedName}`;

						let doc = `**${proc.name}**\n\nLocated in: \`${proc.fileBaseName}.ssl\``;
						if (proc.parameters && proc.parameters.length > 0) {
							doc += `\n\n**Parameters:** ${proc.parameters.join(', ')}`;
						}
						item.documentation = new vscode.MarkdownString(doc);
						completions.push(item);
					}
				}
			}
		}

		return completions;
	}

	private buildQualifiedNames(proc: {
		name: string;
		fileBaseName: string;
		scriptKeys: string[];
	}): string[] {
		const names: string[] = [];
		names.push(`${this.toPascalCase(proc.fileBaseName)}.${proc.name}`);
		for (const key of proc.scriptKeys) {
			if (key.includes('.')) {
				const segments = key.split('.').map(s => this.toPascalCase(s));
				const qualifiedName = `${segments.join('.')}.${proc.name}`;
				if (!names.includes(qualifiedName)) {
					names.push(qualifiedName);
				}
			}
		}
		return names;
	}

	private toPascalCase(str: string): string {
		return str.charAt(0).toUpperCase() + str.slice(1).toLowerCase();
	}

	private refreshFromConfig(config: vscode.WorkspaceConfiguration): void {
		const functions = getConfiguredFunctions(config);
		const classes = getConfiguredClasses(config);
		this.initializeBuiltinFunctions(functions);
		this.initializeBuiltinClasses(classes);
		this.builtinClassMetadata = new Map(classes.map(cls => [cls.name.toUpperCase(), cls]));
		this.builtinClassNames = new Set(this.builtinClassMetadata.keys());
	}

	private getAnonymousObjectMembers(objectName: string, analysis: DocumentAnalysis): vscode.CompletionItem[] {
		const members: vscode.CompletionItem[] = [];
		const memberNames = new Set<string>();

		// Built-in UDObject members
		const builtinMethods = [
			{ name: 'AddProperty', params: '(name, value)', description: 'Dynamically add a property' },
			{ name: 'IsProperty', params: '(name)', description: 'Check if a property exists' },
			{ name: 'AddMethod', params: '(name, procedureRef)', description: 'Dynamically add a method' },
			{ name: 'IsMethod', params: '(name)', description: 'Check if a method exists' },
			{ name: 'Clone', params: '()', description: 'Create a shallow copy' },
			{ name: 'Serialize', params: '()', description: 'Convert to string/XML' },
			{ name: 'Deserialize', params: '(data)', description: 'Restore from serialized data' }
		];

		for (const method of builtinMethods) {
			const item = new vscode.CompletionItem(method.name, vscode.CompletionItemKind.Method);
			item.detail = method.description;
			item.insertText = new vscode.SnippetString(`${method.name}($1)`);
			item.documentation = new vscode.MarkdownString(`**${method.name}**\n\n${method.description}\n\n*Built-in UDObject method*`);
			item.sortText = `0_${method.name}`;
			members.push(item);
			memberNames.add(method.name.toLowerCase());
		}

		const xmlTypeItem = new vscode.CompletionItem('xmltype', vscode.CompletionItemKind.Property);
		xmlTypeItem.detail = 'XML type identifier';
		xmlTypeItem.sortText = '0_xmltype';
		members.push(xmlTypeItem);
		memberNames.add('xmltype');

		// Dynamic members from analysis
		if (analysis.anonymousMembers.has(objectName)) {
			const dynamic = analysis.anonymousMembers.get(objectName)!;

			dynamic.properties.forEach(prop => {
				if (!memberNames.has(prop.toLowerCase())) {
					memberNames.add(prop.toLowerCase());
					const item = new vscode.CompletionItem(prop, vscode.CompletionItemKind.Property);
					item.detail = `User-defined property of ${objectName}`;
					item.sortText = `1_${prop}`;
					members.push(item);
				}
			});

			dynamic.methods.forEach(method => {
				if (!memberNames.has(method.toLowerCase())) {
					memberNames.add(method.toLowerCase());
					const item = new vscode.CompletionItem(method, vscode.CompletionItemKind.Method);
					item.detail = `User-defined method of ${objectName}`;
					item.insertText = new vscode.SnippetString(`${method}($1)`);
					item.sortText = `1_${method}`;
					members.push(item);
				}
			});
		}

		return members;
	}

	private createClassMemberItems(classInfo: ClassMembers): vscode.CompletionItem[] {
		const items: vscode.CompletionItem[] = [];
		classInfo.methods.forEach(method => {
			const item = new vscode.CompletionItem(method, vscode.CompletionItemKind.Method);
			item.detail = `Method defined on ${classInfo.className}`;
			item.insertText = new vscode.SnippetString(`${method}($0)`);
			items.push(item);
		});
		classInfo.properties.forEach(prop => {
			const item = new vscode.CompletionItem(prop, vscode.CompletionItemKind.Property);
			item.detail = `Property of ${classInfo.className}`;
			items.push(item);
		});
		return items;
	}
}
