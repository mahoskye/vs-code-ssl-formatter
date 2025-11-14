import * as vscode from "vscode";
import {
    SSL_KEYWORD_DESCRIPTIONS,
    SSL_BUILTIN_FUNCTIONS,
    SSL_BUILTIN_CLASSES,
    SSLFunction
} from "./constants/language";
import {
    PATTERNS
} from "./constants/patterns";

/**
 * SSL Hover Provider
 * Provides hover information for SSL keywords, functions, and symbols
 */
export class SSLHoverProvider implements vscode.HoverProvider {

	private functionDocs: Map<string, SSLFunction>;
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

			if (funcDoc.frequency) {
				markdown.appendMarkdown(`**Usage Frequency:** ${funcDoc.frequency}\n\n`);
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

	private initializeKeywordDocs(): void {
		Object.entries(SSL_KEYWORD_DESCRIPTIONS).forEach(([keyword, description]) => {
			this.keywordDocs.set(keyword, description);
		});
	}

	private initializeFunctionDocs(): void {
		SSL_BUILTIN_FUNCTIONS.forEach(func => {
			this.functionDocs.set(func.name.toUpperCase(), func);
		});
	}

	private initializeBuiltInClassDocs(): void {
		SSL_BUILTIN_CLASSES.forEach(cls => {
			this.builtInClassDocs.set(cls.name.toUpperCase(), {
				description: cls.description,
				instantiation: cls.instantiation,
				commonMethods: cls.methods.map(m => `${cls.name.toLowerCase()}:${m}()`),
				commonProperties: cls.properties.map(p => `${cls.name.toLowerCase()}:${p}`)
			});
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
