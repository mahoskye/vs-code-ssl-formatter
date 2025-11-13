import * as vscode from "vscode";

/**
 * SSL Formatting Provider
 * Handles document and range formatting for SSL files according to the style guide
 */
export class SSLFormattingProvider implements vscode.DocumentFormattingEditProvider, vscode.DocumentRangeFormattingEditProvider {

	/**
	 * Format the entire document
	 */
	public provideDocumentFormattingEdits(
		document: vscode.TextDocument,
		options: vscode.FormattingOptions,
		token: vscode.CancellationToken
	): vscode.TextEdit[] {
		const config = vscode.workspace.getConfiguration("ssl");
		const fullRange = new vscode.Range(
			document.positionAt(0),
			document.positionAt(document.getText().length)
		);

		return this.formatText(document.getText(), config, options, fullRange);
	}

	/**
	 * Format a specific range of the document
	 */
	public provideDocumentRangeFormattingEdits(
		document: vscode.TextDocument,
		range: vscode.Range,
		options: vscode.FormattingOptions,
		token: vscode.CancellationToken
	): vscode.TextEdit[] {
		const config = vscode.workspace.getConfiguration("ssl");
		const text = document.getText(range);

		return this.formatText(text, config, options, range);
	}

	/**
	 * Format SSL code according to style guide rules
	 */
	private formatText(
		text: string,
		config: vscode.WorkspaceConfiguration,
		options: vscode.FormattingOptions,
		range: vscode.Range
	): vscode.TextEdit[] {
		let formatted = text;

		// Get configuration settings
		const indentStyle = config.get<string>("format.indentStyle", "tab");
		const indentWidth = config.get<number>("format.indentWidth", 1);
		const keywordCase = config.get<string>("format.keywordCase", "upper");
		const builtinFunctionCase = config.get<string>("format.builtinFunctionCase", "PascalCase");
		const trimTrailingWhitespace = config.get<boolean>("format.trimTrailingWhitespace", true);
		
		// Use editor's tab size for calculating visual positions
		const tabSize = options.tabSize || 4;

		// Normalize line endings to \n for processing
		formatted = formatted.replace(/\r\n/g, "\n");

		// Apply formatting rules
		formatted = this.normalizeKeywordCase(formatted, keywordCase);
		formatted = this.normalizeBuiltinFunctionCase(formatted, builtinFunctionCase);
		formatted = this.normalizeOperatorSpacing(formatted);
		formatted = this.normalizeIndentation(formatted, indentStyle, indentWidth, tabSize);

		if (trimTrailingWhitespace) {
			formatted = this.trimTrailingWhitespace(formatted);
		}

		// Ensure final newline
		if (!formatted.endsWith("\n")) {
			formatted += "\n";
		}

		return [vscode.TextEdit.replace(range, formatted)];
	}

	/**
	 * Normalize keyword casing (e.g., :IF, :WHILE, :PROCEDURE)
	 * Only processes keywords outside of strings and comments
	 */
	private normalizeKeywordCase(text: string, caseStyle: string): string {
		if (caseStyle === "preserve") {
			return text;
		}

		const keywords = [
			"IF", "ELSE", "ENDIF",
			"WHILE", "ENDWHILE",
			"FOR", "TO", "STEP", "NEXT", "EXITFOR",
			"EXITWHILE", "LOOP", "RESUME",
			"BEGINCASE", "CASE", "OTHERWISE", "ENDCASE", "EXITCASE",
			"TRY", "CATCH", "FINALLY", "ENDTRY",
			"DECLARE", "DEFAULT", "PARAMETERS", "PUBLIC",
			"INCLUDE", "PROCEDURE", "ENDPROC", "ENDPROCEDURE", "RETURN",
			"CLASS", "INHERIT",
			"REGION", "ENDREGION",
			"BEGININLINECODE", "ENDINLINECODE",
			"ERROR", "LABEL", "FOREACH", "IN"
		];

		const lines = text.split('\n');
		let inMultiLineComment = false;
		
		const formattedLines = lines.map(line => {
			const trimmed = line.trim();
			
			// Track multi-line comment state (SSL uses /* ... ; syntax)
			if (trimmed.startsWith('/*') && !trimmed.endsWith(';')) {
				inMultiLineComment = true;
				return line;
			}
			if (inMultiLineComment) {
				if (trimmed.endsWith(';')) {
					inMultiLineComment = false;
				}
				return line; // Don't format comment content
			}
			
			// Skip single-line comments
			if (trimmed.startsWith('/*') || trimmed.startsWith('*')) {
				return line;
			}
			
			let result = line;
			keywords.forEach(keyword => {
				const pattern = new RegExp(`:${keyword}\\b`, "gi");
				const replacement = caseStyle === "upper"
					? `:${keyword.toUpperCase()}`
					: `:${keyword.toLowerCase()}`;
				result = this.replaceOutsideStrings(result, pattern, replacement);
			});
			return result;
		});

		return formattedLines.join('\n');
	}

	/**
	 * Normalize built-in function casing
	 * Only processes function names outside of strings
	 */
	private normalizeBuiltinFunctionCase(text: string, caseStyle: string): string {
		if (caseStyle === "preserve") {
			return text;
		}

		const functions = [
			"SQLExecute", "DoProc", "ExecFunction", "Empty", "Len", "usrmes", "Chr",
			"aadd", "AllTrim", "At", "Now", "Today",
			"CreateUDObject", "buildstring", "ascan", "alen", "arraycalc", "buildarray",
			"Directory", "CreateGUID", "BuildStringForIn", "ascanexact",
			"Day", "arraynew", "Branch", "DateAdd", "DateDiff", "Abs",
			"Left", "Right", "SubStr", "StrTran", "Upper", "Lower", "Trim",
			"aeval", "RunSQL", "LSearch", "GetDataSet",
			"CToD", "InfoMes", "ErrorMes", "GetSetting", "GetUserData", "SetUserData",
			"GetLastSSLError", "ReturnLastSQLError", "FormatErrorMessage", "RaiseError",
			"Val", "LimsTypeEx"
		];

		const lines = text.split('\n');
		let inMultiLineComment = false;
		
		const formattedLines = lines.map(line => {
			const trimmed = line.trim();
			
			// Track multi-line comment state (SSL uses /* ... ; syntax)
			if (trimmed.startsWith('/*') && !trimmed.endsWith(';')) {
				inMultiLineComment = true;
				return line;
			}
			if (inMultiLineComment) {
				if (trimmed.endsWith(';')) {
					inMultiLineComment = false;
				}
				return line; // Don't format comment content
			}
			
			// Skip single-line comments
			if (trimmed.startsWith('/*') || trimmed.startsWith('*')) {
				return line;
			}
			
			let result = line;
			functions.forEach(func => {
				const pattern = new RegExp(`\\b${func}\\b`, "gi");
				let replacement: string;

				switch (caseStyle) {
					case "PascalCase":
						replacement = func;
						break;
					case "lowercase":
						replacement = func.toLowerCase();
						break;
					case "UPPERCASE":
						replacement = func.toUpperCase();
						break;
					default:
						replacement = func;
				}

				result = this.replaceOutsideStrings(result, pattern, replacement);
			});
			return result;
		});

		return formattedLines.join('\n');
	}

	/**
	 * Normalize operator spacing
	 * Only processes operators outside of strings
	 */
	private normalizeOperatorSpacing(text: string): string {
		const lines = text.split('\n');
		let inMultiLineComment = false;
		
		const formattedLines = lines.map(line => {
			const trimmed = line.trim();
			
			// Track multi-line comment state (SSL uses /* ... ; syntax)
			if (trimmed.startsWith('/*') && !trimmed.endsWith(';')) {
				inMultiLineComment = true;
				return line;
			}
			if (inMultiLineComment) {
				if (trimmed.endsWith(';')) {
					inMultiLineComment = false;
				}
				return line; // Don't format comment content
			}
			
			// Skip single-line comments
			if (trimmed.startsWith('/*') || trimmed.startsWith('*')) {
				return line;
			}
			
			let result = line;
			
			// Space around assignment operators
			result = this.replaceOutsideStrings(result, /\s*:=\s*/g, " := ");
			result = this.replaceOutsideStrings(result, /\s*\+=\s*/g, " += ");
			result = this.replaceOutsideStrings(result, /\s*-=\s*/g, " -= ");
			result = this.replaceOutsideStrings(result, /\s*\*=\s*/g, " *= ");
			result = this.replaceOutsideStrings(result, /\s*\/=\s*/g, " /= ");
			result = this.replaceOutsideStrings(result, /\s*\^=\s*/g, " ^= ");
			result = this.replaceOutsideStrings(result, /\s*%=\s*/g, " %= ");

			// Space around arithmetic operators - apply multiple times for chained operators
			let prev = '';
			while (prev !== result) {
				prev = result;
				result = this.replaceOutsideStrings(result, /([a-zA-Z0-9_\)])(\+|\*|\/|\^|%)([a-zA-Z0-9_\(])/g, "$1 $2 $3");
				result = this.replaceOutsideStrings(result, /([a-zA-Z0-9_\)])(-)([a-zA-Z0-9_\(])/g, "$1 $2 $3");
			}

			// Space around comparison operators
			result = this.replaceOutsideStrings(result, /\s*==\s*/g, " == ");
			result = this.replaceOutsideStrings(result, /\s*!=\s*/g, " != ");
			result = this.replaceOutsideStrings(result, /\s*<>\s*/g, " <> ");
			result = this.replaceOutsideStrings(result, /\s*<=\s*/g, " <= ");
			result = this.replaceOutsideStrings(result, /\s*>=\s*/g, " >= ");

			// Space around logical operators
			result = this.replaceOutsideStrings(result, /\s*\.AND\.\s*/gi, " .AND. ");
			result = this.replaceOutsideStrings(result, /\s*\.OR\.\s*/gi, " .OR. ");
			result = this.replaceOutsideStrings(result, /\s*\.NOT\.\s*/gi, " .NOT. ");

			// Space after commas
			result = this.replaceOutsideStrings(result, /,(\S)/g, ", $1");

			// No space before semicolons
			result = this.replaceOutsideStrings(result, /\s+;/g, ";");
			
			return result;
		});

		return formattedLines.join('\n');
	}
	
	/**
	 * Replace text only outside of string literals
	 */
	private replaceOutsideStrings(line: string, pattern: RegExp, replacement: string): string {
		const segments: { text: string; inString: boolean }[] = [];
		let current = '';
		let inString = false;
		let stringChar: string | null = null;
		
		for (let i = 0; i < line.length; i++) {
			const char = line[i];
			
			if (!inString && (char === '"' || char === "'")) {
				if (current) {
					segments.push({ text: current, inString: false });
					current = '';
				}
				inString = true;
				stringChar = char;
				current = char;
			} else if (inString && char === stringChar) {
				current += char;
				segments.push({ text: current, inString: true });
				current = '';
				inString = false;
				stringChar = null;
			} else {
				current += char;
			}
		}
		
		if (current) {
			segments.push({ text: current, inString });
		}
		
		// Apply replacements only to non-string segments
		return segments.map(seg => {
			if (seg.inString) {
				return seg.text;
			}
			return seg.text.replace(pattern, replacement);
		}).join('');
	}

	/**
	 * Normalize indentation
	 */
	private normalizeIndentation(text: string, indentStyle: string, indentWidth: number, tabSize: number): string {
		const lines = text.split("\n");
		let indentLevel = 0;
		let inMultiLineComment = false;
		let continuationIndent = 0; // Extra indent for continuation lines
		const indentChar = indentStyle === "tab" ? "\t" : " ".repeat(indentWidth);

		const blockStart = /^\s*:(IF|WHILE|FOR|FOREACH|BEGINCASE|TRY|PROCEDURE|CLASS|REGION)\b/i;
		const blockMiddle = /^\s*:(ELSE|CATCH|FINALLY)\b/i;
		const caseKeyword = /^\s*:(CASE|OTHERWISE)\b/i;
		const blockEnd = /^\s*:(ENDIF|ENDWHILE|NEXT|ENDCASE|ENDTRY|ENDPROC|ENDPROCEDURE|ENDREGION)\b/i;
		const caseExit = /^\s*:EXITCASE\b/i;

		const formatted = lines.map((line, index) => {
			const trimmed = line.trim();

			// Track multi-line comment state (SSL uses /* ... ; syntax)
			if (trimmed.startsWith('/*') && !trimmed.endsWith(';')) {
				inMultiLineComment = true;
			}
			if (inMultiLineComment) {
				if (trimmed.endsWith(';')) {
					inMultiLineComment = false;
				}
				return line; // Don't re-indent comment content
			}

			// Skip empty lines or return original comment lines
			if (!trimmed || trimmed.startsWith("/*") || trimmed.startsWith("*")) {
				return line;
			}

			// Check if this is a continuation line
			// Either: previous line ends with continuation operator OR current line starts with one
			let isContinuation = false;
			if (index > 0) {
				const prevLine = lines[index - 1].trim();
				const prevEndsWithOperator = /[+\-*/,]$/.test(prevLine);
				const currentStartsWithOperator = /^[+\-*/,]/.test(trimmed);
				
				if ((prevEndsWithOperator || currentStartsWithOperator) && !inMultiLineComment) {
					isContinuation = true;
					
					// Calculate continuation indent from the line with assignment if not already set
					if (continuationIndent === 0) {
						// Look backwards to find the line with the assignment
						for (let i = index - 1; i >= 0; i--) {
							const checkLine = lines[i].trim();
							const assignMatch = lines[i].match(/^(\s*)(\w+)\s*:=\s*(.+)/);
							
							if (assignMatch) {
								const baseIndent = assignMatch[1];
								const varName = assignMatch[2];
								const valueStart = assignMatch[3];
								
								// Convert tabs to spaces for accurate position calculation
								const expandedIndent = baseIndent.replace(/\t/g, ' '.repeat(tabSize));
								
								// Calculate base position after :=
								let alignPos = expandedIndent.length + varName.length + ' := '.length;
								
								// If the value starts with a string quote or other continuation operator,
								// we want to align with the actual content start
								// For leading operators on continuation lines, align with the operator
								// For trailing operators, align with the value itself
								const firstNonSpace = valueStart.match(/^(\s*)/);
								if (firstNonSpace) {
									alignPos += firstNonSpace[1].length;
								}
								
								continuationIndent = alignPos;
								break;
							}
							
							// Stop looking if we hit a line that doesn't end with continuation
							if (!/[+\-*/,]$/.test(checkLine) && !/^[+\-*/,]/.test(lines[i + 1]?.trim() || '')) {
								break;
							}
						}
					}
				} else if (!prevEndsWithOperator && !currentStartsWithOperator) {
					// Reset continuation indent when we're no longer in a continuation
					continuationIndent = 0;
				}
			}

			// Decrease indent for block end and middle keywords
			if (blockEnd.test(trimmed) || blockMiddle.test(trimmed)) {
				indentLevel = Math.max(0, indentLevel - 1);
				continuationIndent = 0; // Reset continuation on block keywords
			}

			// Decrease indent for :CASE and :OTHERWISE (they're at same level as :BEGINCASE)
			if (caseKeyword.test(trimmed)) {
				indentLevel = Math.max(0, indentLevel - 1);
				continuationIndent = 0; // Reset continuation on block keywords
			}

			// Apply indentation
			let indented;
			if (isContinuation && continuationIndent > 0) {
				// For continuation lines, we need to match the visual position
				// If using tabs, convert the continuation indent to tabs + spaces
				if (indentStyle === "tab") {
					const numTabs = Math.floor(continuationIndent / tabSize);
					const numSpaces = continuationIndent % tabSize;
					indented = '\t'.repeat(numTabs) + ' '.repeat(numSpaces) + trimmed;
				} else {
					// Use exact spacing for continuation lines when using spaces
					indented = ' '.repeat(continuationIndent) + trimmed;
				}
			} else {
				indented = indentChar.repeat(indentLevel) + trimmed;
			}

			// Increase indent after block start
			if (blockStart.test(trimmed)) {
				indentLevel++;
				continuationIndent = 0; // Reset continuation on block keywords
			}

			// Restore indent after middle keywords
			if (blockMiddle.test(trimmed)) {
				indentLevel++;
			}

			// Increase indent after :CASE and :OTHERWISE for their body
			if (caseKeyword.test(trimmed)) {
				indentLevel++;
			}

			// Reset continuation indent if this line doesn't participate in continuation
			const nextLine = lines[index + 1]?.trim() || '';
			if (!isContinuation && !/[+\-*/,]$/.test(trimmed) && !/^[+\-*/,]/.test(nextLine)) {
				continuationIndent = 0;
			}

			return indented;
		});

		return formatted.join("\n");
	}

	/**
	 * Trim trailing whitespace from each line
	 */
	private trimTrailingWhitespace(text: string): string {
		return text.split("\n").map(line => line.trimEnd()).join("\n");
	}
}
