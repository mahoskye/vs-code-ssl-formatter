import * as vscode from "vscode";
import {
    SSL_KEYWORD_DESCRIPTIONS,
    SSLFunction,
    SSLClass
} from "./constants/language";
import {
    PATTERNS
} from "./constants/patterns";
import { ProcedureIndex, ProcedureInfo } from "./utils/procedureIndex";
import { CONFIG_KEYS, CONFIG_DEFAULTS } from "./constants/config";
import { ClassIndex } from "./utils/classIndex";
import { getConfiguredClasses, getConfiguredFunctions } from "./utils/intellisense";

/**
 * SSL Hover Provider
 * Provides hover information for SSL keywords, functions, and symbols
 */
export class SSLHoverProvider implements vscode.HoverProvider {

	private functionDocs: Map<string, SSLFunction>;
	private keywordDocs: Map<string, string>;
	private builtInClassDocs: Map<string, { description: string; instantiation: string; commonMethods: string[]; commonProperties: string[] }>;

	constructor(
		private readonly classIndex?: ClassIndex,
		private readonly procedureIndex?: ProcedureIndex
	) {
		this.functionDocs = new Map();
		this.keywordDocs = new Map();
		this.builtInClassDocs = new Map();
		this.initializeKeywordDocs();
		const config = vscode.workspace.getConfiguration("ssl");
		this.refreshDocsFromConfig(config);
	}

	public provideHover(
		document: vscode.TextDocument,
		position: vscode.Position,
		token: vscode.CancellationToken
	): vscode.Hover | null {
		const lineText = document.lineAt(position.line).text;
		const config = vscode.workspace.getConfiguration("ssl");
		this.refreshDocsFromConfig(config);

		const locationContext = this.getLocationContext(document, position);

		if (locationContext.insideComment) {
			return null;
		}

		if (locationContext.insideString) {
			const placeholderHover = this.getSqlPlaceholderHover(document, position);
			if (placeholderHover) {
				return placeholderHover;
			}

			const stringProcHover = this.getUserDefinedProcedureHover(document, position, lineText);
			if (stringProcHover) {
				return stringProcHover;
			}

			const stringClassHover = this.getUserDefinedClassHover(document, position, lineText);
			if (stringClassHover) {
				return stringClassHover;
			}

			return null;
		}

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

			// Use signature if available, otherwise construct from name and params
			const signature = funcDoc.signature || `${word}${funcDoc.params}`;
			markdown.appendCodeblock(signature, "ssl");

			markdown.appendMarkdown(`\n**Description:** ${funcDoc.description}\n\n`);

			if (funcDoc.returnType) {
				markdown.appendMarkdown(`**Returns:** \`${funcDoc.returnType}\`\n\n`);
			} else if (funcDoc.returns) {
				markdown.appendMarkdown(`**Returns:** ${funcDoc.returns}\n\n`);
			}

			if (funcDoc.category) {
				markdown.appendMarkdown(`**Category:** ${funcDoc.category}\n\n`);
			}

			if (funcDoc.untypedSignature) {
				markdown.appendMarkdown(`**Untyped Signature:** \`${funcDoc.untypedSignature}\`\n\n`);
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

	private getLocationContext(document: vscode.TextDocument, position: vscode.Position): { insideString: boolean; insideComment: boolean } {
		let insideString = false;
		let stringChar: string | null = null;
		let insideComment = false;

		for (let line = 0; line <= position.line; line++) {
			const lineText = document.lineAt(line).text;
			const limit = line === position.line ? position.character : lineText.length;

			for (let i = 0; i < limit; i++) {
				const char = lineText[i];
				const nextChar = i + 1 < lineText.length ? lineText[i + 1] : '';

				if (!insideString && !insideComment && char === '/' && nextChar === '*') {
					insideComment = true;
					i++;
					continue;
				}

				if (!insideString && !insideComment && (char === '"' || char === '\'' || char === '[')) {
					insideString = true;
					stringChar = char === '[' ? ']' : char;
					continue;
				}

				if (insideString && char === stringChar) {
					insideString = false;
					stringChar = null;
					continue;
				}

				if (insideComment && char === ';') {
					insideComment = false;
				}
			}
		}

		return { insideString, insideComment };
	}

	private getSqlPlaceholderHover(document: vscode.TextDocument, position: vscode.Position): vscode.Hover | null {
		const lineText = document.lineAt(position.line).text;
		const charIndex = position.character;

		// Handle named placeholders like ?sPatientId? or ?sCountry[1]?
		const namedPlaceholderRegex = /\?([A-Za-z_][A-Za-z0-9_]*(?:\[[^\]]+\])?)\?/g;
		for (const match of lineText.matchAll(namedPlaceholderRegex)) {
			const start = match.index ?? 0;
			const end = start + match[0].length;
			if (charIndex >= start && charIndex < end) {
				const parameterExpr = match[1];
				const range = new vscode.Range(
					new vscode.Position(position.line, start),
					new vscode.Position(position.line, end)
				);

				// Parse the parameter expression for variable name and optional index
				const indexMatch = parameterExpr.match(/^([A-Za-z_][A-Za-z0-9_]*)(?:\[([^\]]+)\])?$/);
				const variableName = indexMatch ? indexMatch[1] : parameterExpr;
				const arrayIndex = indexMatch ? indexMatch[2] : null;

				// Try to find the variable declaration and value
				const varInfo = this.findVariableInfo(document, variableName, position.line);

				const markdown = new vscode.MarkdownString();
				markdown.isTrusted = true;

				if (arrayIndex) {
					markdown.appendMarkdown(`**Named SQL parameter** \`${variableName}[${arrayIndex}]\`\n\n`);
				} else {
					markdown.appendMarkdown(`**Named SQL parameter** \`${variableName}\`\n\n`);
				}

				if (varInfo) {
					if (varInfo.value) {
						if (arrayIndex && varInfo.isArray) {
							const indexValue = this.getArrayElementValue(varInfo.value, arrayIndex);
							if (indexValue) {
								markdown.appendMarkdown(`**Value:** \`${indexValue}\`\n\n`);
							} else {
								markdown.appendMarkdown(`**Value:** _(element ${arrayIndex} not found)_\n\n`);
							}
						} else {
							markdown.appendMarkdown(`**Value:** \`${varInfo.value}\`\n\n`);
						}
					}

					if (varInfo.declarationLine !== undefined) {
						markdown.appendMarkdown(`📍 Declared at line ${varInfo.declarationLine + 1}\n\n`);
					}
				} else {
					markdown.appendMarkdown(`⚠️ Variable \`${variableName}\` not found in current scope\n\n`);
				}

				markdown.appendMarkdown(`_Maps to SSL variable at query execution time_`);
				return new vscode.Hover(markdown, range);
			}
		}

		// Handle positional placeholders (standalone ?)
		const currentChar = lineText[charIndex];
		if (currentChar === '?') {
			const previousChar = charIndex > 0 ? lineText[charIndex - 1] : '';
			const nextChar = charIndex + 1 < lineText.length ? lineText[charIndex + 1] : '';

			// Ensure we're not inside a named placeholder (handled above)
			if (!/[A-Za-z0-9_]/.test(previousChar) && !/[A-Za-z0-9_]/.test(nextChar)) {
				const range = new vscode.Range(
					position,
					new vscode.Position(position.line, charIndex + 1)
				);

				// Count which positional placeholder this is
				const placeholderIndex = this.countPositionalPlaceholdersBefore(document, position);

				// Try to find the SQL function call and its parameters
				const sqlCallInfo = this.findSqlFunctionCall(document, position);

				const markdown = new vscode.MarkdownString();
				markdown.isTrusted = true;

				markdown.appendMarkdown(`**Positional SQL placeholder** \`?\` (Parameter ${placeholderIndex + 1})\n\n`);

				if (sqlCallInfo && sqlCallInfo.parameters.length > 0) {
					if (placeholderIndex < sqlCallInfo.parameters.length) {
						const paramValue = sqlCallInfo.parameters[placeholderIndex];
						markdown.appendMarkdown(`**Value:** \`${paramValue}\`\n\n`);
					} else {
						markdown.appendMarkdown(`⚠️ No matching parameter in array (only ${sqlCallInfo.parameters.length} provided)\n\n`);
					}
					markdown.appendMarkdown(`_Passed via ${sqlCallInfo.functionName}_\n\n`);
				} else if (sqlCallInfo) {
					markdown.appendMarkdown(`_Passed via ${sqlCallInfo.functionName} parameters array_\n\n`);
				} else {
					markdown.appendMarkdown(`_Value provided via parameters array argument_\n\n`);
				}

				return new vscode.Hover(markdown, range);
			}
		}

		return null;
	}

	/**
	 * Find variable declaration and value information
	 */
	private findVariableInfo(
		document: vscode.TextDocument,
		variableName: string,
		beforeLine: number
	): { declarationLine?: number; value?: string; isArray: boolean } | null {
		const text = document.getText();
		const lines = text.split('\n');
		let declarationLine: number | undefined;
		let value: string | undefined;
		let isArray = false;

		// Look for declaration
		const declarePattern = new RegExp(`^\\s*:DECLARE\\b[^;]*\\b${variableName}\\b`, 'i');
		const paramPattern = new RegExp(`^\\s*:PARAMETERS\\b[^;]*\\b${variableName}\\b`, 'i');

		for (let i = 0; i <= beforeLine && i < lines.length; i++) {
			if (declarePattern.test(lines[i]) || paramPattern.test(lines[i])) {
				declarationLine = i;
			}
		}

		// Look for most recent assignment before current line
		const assignPattern = new RegExp(`^\\s*${variableName}\\s*:=\\s*(.+?)\\s*;`, 'i');
		for (let i = 0; i <= beforeLine && i < lines.length; i++) {
			const match = lines[i].match(assignPattern);
			if (match) {
				value = match[1].trim();
				// Check if it's an array literal
				if (value.startsWith('{') && value.endsWith('}')) {
					isArray = true;
				}
			}
		}

		if (declarationLine === undefined && value === undefined) {
			return null;
		}

		return { declarationLine, value, isArray };
	}

	/**
	 * Extract a specific element from an array literal
	 */
	private getArrayElementValue(arrayLiteral: string, indexExpr: string): string | null {
		// Parse array literal like { "Germany", "France" }
		if (!arrayLiteral.startsWith('{') || !arrayLiteral.endsWith('}')) {
			return null;
		}

		const content = arrayLiteral.slice(1, -1).trim();
		const elements = this.parseArrayElements(content);

		// Handle numeric index (1-based in SSL)
		const numIndex = parseInt(indexExpr, 10);
		if (!isNaN(numIndex) && numIndex >= 1 && numIndex <= elements.length) {
			return elements[numIndex - 1];
		}

		return null;
	}

	/**
	 * Parse comma-separated array elements, respecting string quotes
	 */
	private parseArrayElements(content: string): string[] {
		const elements: string[] = [];
		let current = '';
		let inString = false;
		let stringChar = '';
		let depth = 0;

		for (const char of content) {
			if (!inString && (char === '"' || char === "'")) {
				inString = true;
				stringChar = char;
				current += char;
			} else if (inString && char === stringChar) {
				inString = false;
				current += char;
			} else if (!inString && char === '{') {
				depth++;
				current += char;
			} else if (!inString && char === '}') {
				depth--;
				current += char;
			} else if (!inString && depth === 0 && char === ',') {
				if (current.trim()) {
					elements.push(current.trim());
				}
				current = '';
			} else {
				current += char;
			}
		}

		if (current.trim()) {
			elements.push(current.trim());
		}

		return elements;
	}

	/**
	 * Count positional placeholders before the current position in the SQL string
	 */
	private countPositionalPlaceholdersBefore(document: vscode.TextDocument, position: vscode.Position): number {
		const lineText = document.lineAt(position.line).text;

		// Find the string containing the position
		const stringInfo = this.findContainingString(lineText, position.character);
		if (!stringInfo) {
			return 0;
		}

		// Count ? that are not part of named placeholders before current position
		const sqlContent = stringInfo.content.substring(0, position.character - stringInfo.start - 1);
		let count = 0;
		let i = 0;

		while (i < sqlContent.length) {
			if (sqlContent[i] === '?') {
				// Check if this is a named placeholder
				const rest = sqlContent.substring(i);
				const namedMatch = rest.match(/^\?[A-Za-z_][A-Za-z0-9_]*(?:\[[^\]]+\])?\?/);
				if (namedMatch) {
					i += namedMatch[0].length;
				} else {
					count++;
					i++;
				}
			} else {
				i++;
			}
		}

		return count;
	}

	/**
	 * Find the SQL function call containing the current position
	 */
	private findSqlFunctionCall(
		document: vscode.TextDocument,
		position: vscode.Position
	): { functionName: string; parameters: string[] } | null {
		const text = document.getText();
		const lines = text.split('\n');

		// Look for SQL function patterns that span current line
		// Common patterns: RunSQL(...), SQLExecute(...), LSearch(...), etc.
		const sqlFunctions = ['RunSQL', 'SQLExecute', 'LSearch', 'LSelect', 'LSelect1', 'LSelectC', 'GetDataSet', 'GetNetDataSet'];

		// Search backwards from current line to find the function call
		for (let searchLine = position.line; searchLine >= Math.max(0, position.line - 20); searchLine--) {
			const lineText = lines[searchLine];

			for (const funcName of sqlFunctions) {
				const funcIndex = lineText.indexOf(funcName + '(');
				if (funcIndex !== -1) {
					// Found a SQL function call - try to extract parameters
					const params = this.extractSqlFunctionParameters(lines, searchLine, funcIndex, funcName);
					if (params) {
						return { functionName: funcName, parameters: params };
					}
				}
			}
		}

		return null;
	}

	/**
	 * Extract the parameters array from a SQL function call
	 */
	private extractSqlFunctionParameters(
		lines: string[],
		startLine: number,
		startCol: number,
		funcName: string
	): string[] | null {
		// Build the full function call text (may span multiple lines)
		let callText = '';
		let parenDepth = 0;
		let started = false;

		for (let line = startLine; line < Math.min(startLine + 20, lines.length); line++) {
			const lineText = line === startLine ? lines[line].substring(startCol) : lines[line];

			for (let i = 0; i < lineText.length; i++) {
				const char = lineText[i];
				callText += char;

				if (char === '(') {
					parenDepth++;
					started = true;
				} else if (char === ')') {
					parenDepth--;
					if (started && parenDepth === 0) {
						// Found the complete call - now parse it
						return this.parseSqlFunctionParameters(callText, funcName);
					}
				}
			}
			callText += '\n';
		}

		return null;
	}

	/**
	 * Parse parameters from a complete SQL function call
	 */
	private parseSqlFunctionParameters(callText: string, funcName: string): string[] {
		// Different functions have parameters at different positions:
		// RunSQL(sql, connectionName, returnRecords, parameters) - params at index 3
		// SQLExecute(sql, param1, param2, ..., param10) - params are inline at indices 1-10
		// LSearch(sql, parameters) - params at index 1

		const argsMatch = callText.match(/^\w+\s*\(([\s\S]*)\)$/);
		if (!argsMatch) {
			return [];
		}

		const allArgs = this.splitFunctionArguments(argsMatch[1]);

		// Find the parameters array based on function type
		const funcUpper = funcName.toUpperCase();

		if (funcUpper === 'RUNSQL' && allArgs.length >= 4) {
			return this.parseArrayLiteral(allArgs[3]);
		}

		if (funcUpper === 'LSEARCH' && allArgs.length >= 2) {
			return this.parseArrayLiteral(allArgs[1]);
		}

		if (funcUpper === 'SQLEXECUTE') {
			// SQLExecute has inline parameters (not in array), indices 1-10
			return allArgs.slice(1).filter(arg => arg.trim() !== '').map(arg => arg.trim());
		}

		return [];
	}

	/**
	 * Split function arguments respecting nested structures
	 */
	private splitFunctionArguments(argsText: string): string[] {
		const args: string[] = [];
		let current = '';
		let depth = 0;
		let inString = false;
		let stringChar = '';

		for (const char of argsText) {
			if (!inString && (char === '"' || char === "'")) {
				inString = true;
				stringChar = char;
				current += char;
			} else if (inString && char === stringChar) {
				inString = false;
				current += char;
			} else if (!inString && (char === '(' || char === '{' || char === '[')) {
				depth++;
				current += char;
			} else if (!inString && (char === ')' || char === '}' || char === ']')) {
				depth--;
				current += char;
			} else if (!inString && depth === 0 && char === ',') {
				args.push(current.trim());
				current = '';
			} else {
				current += char;
			}
		}

		if (current.trim()) {
			args.push(current.trim());
		}

		return args;
	}

	/**
	 * Parse an array literal into its elements
	 */
	private parseArrayLiteral(arrayText: string): string[] {
		const trimmed = arrayText.trim();
		if (trimmed.startsWith('{') && trimmed.endsWith('}')) {
			return this.parseArrayElements(trimmed.slice(1, -1));
		}
		// If it's a variable reference, we can't resolve it easily
		return [];
	}

	/**
	 * Find the string literal containing the given position
	 */
	private findContainingString(lineText: string, charPosition: number): { content: string; start: number; end: number } | null {
		let inString = false;
		let stringStart = -1;
		let quoteChar = '';

		for (let i = 0; i < lineText.length; i++) {
			const char = lineText[i];

			if (!inString && (char === '"' || char === "'" || char === '[')) {
				inString = true;
				stringStart = i;
				quoteChar = char === '[' ? ']' : char;
			} else if (inString && char === quoteChar) {
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

	private initializeKeywordDocs(): void {
		Object.entries(SSL_KEYWORD_DESCRIPTIONS).forEach(([keyword, description]) => {
			this.keywordDocs.set(keyword, description);
		});
	}

	private initializeFunctionDocs(functions: SSLFunction[]): void {
		this.functionDocs = new Map();
		functions.forEach(func => {
			this.functionDocs.set(func.name.toUpperCase(), func);
		});
	}

	private initializeBuiltInClassDocs(classes: SSLClass[]): void {
		this.builtInClassDocs = new Map();
		classes.forEach(cls => {
			this.builtInClassDocs.set(cls.name.toUpperCase(), {
				description: cls.description,
				instantiation: cls.instantiation,
				commonMethods: cls.methods.map(m => `${cls.name.toLowerCase()}:${m}()`),
				commonProperties: cls.properties.map(p => `${cls.name.toLowerCase()}:${p}`)
			});
		});
	}

	private refreshDocsFromConfig(config: vscode.WorkspaceConfiguration): void {
		const functions = getConfiguredFunctions(config);
		const classes = getConfiguredClasses(config);

		this.initializeFunctionDocs(functions);
		this.initializeBuiltInClassDocs(classes);
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

		const procParts = procedureName.split('.');
		const actualProcName = procParts[procParts.length - 1];

		const localProcInfo = this.findProcedureDefinition(document, actualProcName);
		if (localProcInfo) {
			const markdown = new vscode.MarkdownString();
			markdown.appendCodeblock(`:PROCEDURE ${localProcInfo.name}${localProcInfo.params}`, "ssl");
			markdown.appendMarkdown(`\n**User-defined procedure**\n\n`);
			if (localProcInfo.params) {
				markdown.appendMarkdown(`**Parameters:** ${localProcInfo.params}\n\n`);
			}
			markdown.appendMarkdown(`_Called via ${doProcMatch[1]}_`);

			const range = new vscode.Range(
				position.line,
				stringInfo.start + 1,
				position.line,
				stringInfo.end - 1
			);

			return new vscode.Hover(markdown, range);
		}

		const workspaceProc = this.resolveWorkspaceProcedure(procedureName);
		if (!workspaceProc) {
			return null;
		}

		const markdown = new vscode.MarkdownString();
		const signatureLines = [workspaceProc.declarationText || `:PROCEDURE ${workspaceProc.name};`];
		if (workspaceProc.parameters && workspaceProc.parameters.length > 0) {
			signatureLines.push(`:PARAMETERS ${workspaceProc.parameters.join(", ")};`);
		}
		markdown.appendCodeblock(signatureLines.join("\n"), "ssl");
		markdown.appendMarkdown(`\n**User-defined procedure**\n\n`);
		const relativePath = vscode.workspace.asRelativePath(workspaceProc.uri, false);
		markdown.appendMarkdown(`Located in \`${relativePath}\`\n\n`);
		if (workspaceProc.parameters && workspaceProc.parameters.length > 0) {
			markdown.appendMarkdown(`**Parameters:** ${workspaceProc.parameters.join(", ")}\n\n`);
		}
		markdown.appendMarkdown(`_Called via ${doProcMatch[1]}_`);

		const range = new vscode.Range(
			position.line,
			stringInfo.start + 1,
			position.line,
			stringInfo.end - 1
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
