import * as vscode from "vscode";
import {
    SSL_KEYWORDS,
    SSL_BUILTIN_FUNCTIONS,
    BLOCK_START_KEYWORDS,
    BLOCK_END_KEYWORDS,
    BLOCK_MIDDLE_KEYWORDS,
    CASE_KEYWORDS,
    PROCEDURE_LEVEL_KEYWORDS
} from "./constants/language";
import {
    CONFIG_KEYS,
    CONFIG_DEFAULTS
} from "./constants/config";
import {
	PATTERNS
} from "./constants/patterns";
import { normalizeOperatorSpacing, replaceOutsideStrings } from "./utils/formatters";

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
		const indentStyle = config.get<string>(CONFIG_KEYS.FORMAT_INDENT_STYLE, CONFIG_DEFAULTS[CONFIG_KEYS.FORMAT_INDENT_STYLE]);
		const indentWidth = config.get<number>(CONFIG_KEYS.FORMAT_INDENT_WIDTH, CONFIG_DEFAULTS[CONFIG_KEYS.FORMAT_INDENT_WIDTH]);
		const keywordCase = config.get<string>(CONFIG_KEYS.FORMAT_KEYWORD_CASE, CONFIG_DEFAULTS[CONFIG_KEYS.FORMAT_KEYWORD_CASE]);
		const builtinFunctionCase = config.get<string>(CONFIG_KEYS.FORMAT_BUILTIN_FUNCTION_CASE, CONFIG_DEFAULTS[CONFIG_KEYS.FORMAT_BUILTIN_FUNCTION_CASE]);
		const trimTrailingWhitespace = config.get<boolean>(CONFIG_KEYS.FORMAT_TRIM_TRAILING_WHITESPACE, CONFIG_DEFAULTS[CONFIG_KEYS.FORMAT_TRIM_TRAILING_WHITESPACE]);
		
		// Use editor's tab size for calculating visual positions
		const tabSize = options.tabSize || 4;

		// Normalize line endings to \n for processing
		formatted = formatted.replace(/\r\n/g, "\n");

		// Apply formatting rules
		formatted = this.splitMultipleStatements(formatted);
		formatted = this.normalizeKeywordCase(formatted, keywordCase);
		formatted = this.normalizeBuiltinFunctionCase(formatted, builtinFunctionCase);
	formatted = normalizeOperatorSpacing(formatted);
		formatted = this.normalizeIndentation(formatted, indentStyle, indentWidth, tabSize);
		formatted = this.normalizeBlankLines(formatted);

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
	 * Split multiple statements on the same line into separate lines
	 * Handles cases like: :ENDCASE; nProcessed := nProcessed + 1;
	 */
	private splitMultipleStatements(text: string): string {
		const lines = text.split('\n');
		let inMultiLineComment = false;
		let inMultiLineString = false;
		let stringDelimiter = "";

		const formattedLines = lines.flatMap(line => {
			const trimmed = line.trim();

			// Track multi-line string state
			if (!inMultiLineString) {
				const doubleQuoteCount = (line.match(/"/g) || []).length;
				const singleQuoteCount = (line.match(/'/g) || []).length;

				if (doubleQuoteCount % 2 !== 0) {
					inMultiLineString = true;
					stringDelimiter = '"';
				} else if (singleQuoteCount % 2 !== 0) {
					inMultiLineString = true;
					stringDelimiter = "'";
				}
			} else {
				const delimiterCount = (line.match(new RegExp(stringDelimiter === '"' ? '"' : "'", 'g')) || []).length;
				if (delimiterCount % 2 !== 0) {
					inMultiLineString = false;
					stringDelimiter = "";
				}
				return [line]; // Don't split string content
			}

			// Track multi-line comment state
			if (trimmed.startsWith('/*') && !trimmed.endsWith(';')) {
				inMultiLineComment = true;
			}
			if (inMultiLineComment) {
				if (trimmed.endsWith(';')) {
					inMultiLineComment = false;
				}
				return [line]; // Don't split comment content
			}

			// Skip comment lines
			if (trimmed.startsWith('/*') || trimmed.startsWith('*')) {
				return [line];
			}

			// Skip keywords and empty lines
			if (trimmed === '' || /^\s*:[A-Z]+\s*;?\s*$/i.test(trimmed)) {
				return [line];
			}

			// Split multiple statements on the same line
			// We need to find semicolons that aren't inside strings
			const leadingWhitespace = line.match(/^\s*/)?.[0] || '';
			const statements: string[] = [];
			let currentStatement = '';
			let inString = false;
			let stringChar = '';

			for (let i = 0; i < trimmed.length; i++) {
				const char = trimmed[i];
				const prevChar = i > 0 ? trimmed[i - 1] : '';

				// Track string boundaries
				if (!inString && (char === '"' || char === "'")) {
					inString = true;
					stringChar = char;
					currentStatement += char;
				} else if (inString && char === stringChar && prevChar !== '\\') {
					inString = false;
					stringChar = '';
					currentStatement += char;
				} else if (char === ';' && !inString) {
					// Found a statement terminator outside of strings
					currentStatement += char;
					const stmt = currentStatement.trim();
					if (stmt) {
						statements.push(leadingWhitespace + stmt);
					}
					currentStatement = '';
				} else {
					currentStatement += char;
				}
			}

			// Add any remaining content
			const remaining = currentStatement.trim();
			if (remaining) {
				statements.push(leadingWhitespace + remaining);
			}

			// If we only found one statement, return the original line
			if (statements.length <= 1) {
				return [line];
			}

			return statements;
		});

		return formattedLines.join('\n');
	}

	/**
	 * Normalize keyword casing (e.g., :IF, :WHILE, :PROCEDURE)
	 * Only processes keywords outside of strings and comments
	 */
	private normalizeKeywordCase(text: string, caseStyle: string): string {
		if (caseStyle === "preserve") {
			return text;
		}

		const keywords = SSL_KEYWORDS;

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
				result = replaceOutsideStrings(result, pattern, replacement);
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

		const functions = SSL_BUILTIN_FUNCTIONS.map(f => f.name);

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
				// Only match function names when followed by opening parenthesis
				// This prevents matching variable names that happen to match function names
				const pattern = new RegExp(`\\b${func}(?=\\s*\\()`, "gi");
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

				result = replaceOutsideStrings(result, pattern, replacement);
			});
			return result;
		});

		return formattedLines.join('\n');
	}

	/**
	 * Normalize operator spacing
	 * Only processes operators outside of strings
	 */
	// Use shared implementation from utils/formatters
	private normalizeOperatorSpacing(text: string): string {
		return normalizeOperatorSpacing(text);
	}
	
	/**
	 * Replace text only outside of string literals
	 */
	// replaceOutsideStrings functionality is provided by utils/formatters.replaceOutsideStrings

	/**
	 * Normalize indentation
	 */
	private normalizeIndentation(text: string, indentStyle: string, indentWidth: number, tabSize: number): string {
		// Two-pass approach:
		// Pass 1: Fix block-level indentation
		// Pass 2: Fix continuation line alignment based on corrected indentation
		
		let firstPass = this.normalizeBlockIndentation(text, indentStyle, indentWidth);
		let secondPass = this.normalizeContinuationIndentation(firstPass, indentStyle, tabSize);
		
		return secondPass;
	}

	/**
	 * First pass: Normalize block-level indentation (IF/WHILE/FOR/etc.)
	 */
	private normalizeBlockIndentation(text: string, indentStyle: string, indentWidth: number): string {
		const lines = text.split("\n");
		let indentLevel = 0;
		let inMultiLineComment = false;
		let inMultiLineString = false;
		let stringDelimiter = "";
		let procedureDepth = 0; // Track nesting depth of PROCEDURE blocks
		const indentChar = indentStyle === "tab" ? "\t" : " ".repeat(indentWidth);

		const blockStart = new RegExp(`^\\s*:(${BLOCK_START_KEYWORDS.join('|')})\\b`, 'i');
		const blockMiddle = new RegExp(`^\\s*:(${BLOCK_MIDDLE_KEYWORDS.join('|')})\\b`, 'i');
		const caseKeyword = new RegExp(`^\\s*:(${CASE_KEYWORDS.join('|')})\\b`, 'i');
		const blockEnd = new RegExp(`^\\s*:(${BLOCK_END_KEYWORDS.join('|')})\\b`, 'i');
		const procedureLevelKeyword = new RegExp(`^\\s*:(${PROCEDURE_LEVEL_KEYWORDS.join('|')})\\b`, 'i');
		const procedureKeyword = /^\s*:PROCEDURE\b/i;

		const formatted = lines.map((line) => {
			const trimmed = line.trim();

			// Track multi-line string state
			if (!inMultiLineString) {
				const doubleQuoteCount = (line.match(/"/g) || []).length;
				const singleQuoteCount = (line.match(/'/g) || []).length;
				
				if (doubleQuoteCount % 2 !== 0) {
					inMultiLineString = true;
					stringDelimiter = '"';
				} else if (singleQuoteCount % 2 !== 0) {
					inMultiLineString = true;
					stringDelimiter = "'";
				}
			} else {
				const delimiterCount = (line.match(new RegExp(stringDelimiter === '"' ? '"' : "'", 'g')) || []).length;
				if (delimiterCount % 2 !== 0) {
					inMultiLineString = false;
					stringDelimiter = "";
				}
				return line; // Don't re-indent string content
			}

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

			// Skip empty lines
			if (!trimmed) {
				return line;
			}

			// Handle single-line comments early (will use currentIndentLevel calculated below)
			// Skip this check for now - we'll handle comments along with regular code

			// Lines starting with * but not part of a multi-line comment are NOT comment lines
			// (they're likely continuation of something else), so skip them
			if (trimmed.startsWith("*")) {
				return line;
			}

			// Track PROCEDURE blocks
			if (procedureKeyword.test(trimmed)) {
				procedureDepth++;
			}

			// Decrease indent for block end and middle keywords
			if (blockEnd.test(trimmed) || blockMiddle.test(trimmed)) {
				indentLevel = Math.max(0, indentLevel - 1);
				// Decrease procedure depth when ending a procedure
				if (/^\s*:ENDPROC\b/i.test(trimmed)) {
					procedureDepth = Math.max(0, procedureDepth - 1);
				}
			}

			// Decrease indent for :CASE and :OTHERWISE (they're at same level as :BEGINCASE)
			if (caseKeyword.test(trimmed)) {
				indentLevel = Math.max(0, indentLevel - 1);
			}

			// Procedure-level keywords (PARAMETERS, DECLARE, DEFAULT, RETURN, PUBLIC)
			// should be at the same level as PROCEDURE (not indented)
			// Comments at procedure level should also be at column 0
			let currentIndentLevel = indentLevel;
			if (procedureLevelKeyword.test(trimmed) && procedureDepth > 0) {
				currentIndentLevel = Math.max(0, indentLevel - 1);
			}
			// Comments that are originally at column 0 (like doc comments) should stay at column 0
			// Only apply this if the original line starts with /* (not indented)
			if (trimmed.startsWith("/*") && line.trim() === trimmed && line.startsWith("/*")) {
				currentIndentLevel = 0;
			}

			// Handle comments with calculated indentation
			if (trimmed.startsWith("/*")) {
				const indented = indentChar.repeat(currentIndentLevel) + trimmed;
				return indented;
			}

			// Apply block indentation
			const indented = indentChar.repeat(currentIndentLevel) + trimmed;

			// Increase indent after block start
			if (blockStart.test(trimmed)) {
				indentLevel++;
			}

			// Restore indent after middle keywords
			if (blockMiddle.test(trimmed)) {
				indentLevel++;
			}

			// Increase indent after :CASE and :OTHERWISE for their body
			if (caseKeyword.test(trimmed)) {
				indentLevel++;
			}

			return indented;
		});

		return formatted.join("\n");
	}

	/**
	 * Second pass: Normalize continuation line indentation
	 */
	private normalizeContinuationIndentation(text: string, indentStyle: string, tabSize: number): string {
		const lines = text.split("\n");
		let inMultiLineComment = false;
		let inMultiLineString = false;
		let stringDelimiter = "";
		let continuationIndent = 0;

		const formatted = lines.map((line, index) => {
			const trimmed = line.trim();

			// Track multi-line string state
			if (!inMultiLineString) {
				const doubleQuoteCount = (line.match(/"/g) || []).length;
				const singleQuoteCount = (line.match(/'/g) || []).length;
				
				if (doubleQuoteCount % 2 !== 0) {
					inMultiLineString = true;
					stringDelimiter = '"';
				} else if (singleQuoteCount % 2 !== 0) {
					inMultiLineString = true;
					stringDelimiter = "'";
				}
			} else {
				const delimiterCount = (line.match(new RegExp(stringDelimiter === '"' ? '"' : "'", 'g')) || []).length;
				if (delimiterCount % 2 !== 0) {
					inMultiLineString = false;
					stringDelimiter = "";
				}
				return line; // Don't re-indent string content
			}

			// Track multi-line comment state
			if (trimmed.startsWith('/*') && !trimmed.endsWith(';')) {
				inMultiLineComment = true;
			}
			if (inMultiLineComment) {
				if (trimmed.endsWith(';')) {
					inMultiLineComment = false;
				}
				return line;
			}

			// Skip empty lines or comment lines
			if (!trimmed || trimmed.startsWith("/*") || trimmed.startsWith("*")) {
				return line;
			}

			// Check if this is a continuation line
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
				// Not a continuation line - keep the indentation from first pass
				return line;
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

	/**
	 * Normalize blank lines between procedures
	 * - Ensure exactly 1 blank line between procedures
	 * - Remove excessive blank lines (multiple blanks → single blank)
	 */
	private normalizeBlankLines(text: string): string {
		const lines = text.split('\n');
		const result: string[] = [];
		let lastWasEndProc = false;
		let blankLineCount = 0;

		for (let i = 0; i < lines.length; i++) {
			const line = lines[i];
			const trimmed = line.trim();

			// Track blank lines
			if (trimmed === '') {
				blankLineCount++;
				continue;
			}

			// If we hit a PROCEDURE after an ENDPROC, ensure exactly 1 blank line
			if (lastWasEndProc && /^\s*:PROCEDURE\b/i.test(line)) {
				if (blankLineCount === 0) {
					result.push(''); // Add missing blank line
				} else if (blankLineCount > 1) {
					result.push(''); // Normalize multiple blanks to 1
				} else {
					result.push(''); // Keep the 1 blank line
				}
				blankLineCount = 0;
			} else if (blankLineCount > 0) {
				// For other cases, preserve blank lines but collapse multiple into fewer
				// (keeping some blank lines for readability within procedures)
				const blanksToAdd = Math.min(blankLineCount, 2);
				for (let j = 0; j < blanksToAdd; j++) {
					result.push('');
				}
				blankLineCount = 0;
			}

			result.push(line);

			// Track if this line is ENDPROC
			lastWasEndProc = /^\s*:ENDPROC\b/i.test(line);
		}

		// Handle any trailing blank lines
		while (blankLineCount > 0 && result.length > 0) {
			result.push('');
			blankLineCount--;
		}

		return result.join('\n');
	}
}
