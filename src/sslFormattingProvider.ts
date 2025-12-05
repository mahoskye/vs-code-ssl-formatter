import * as vscode from "vscode";
import {
	SSL_KEYWORDS,
	BLOCK_START_KEYWORDS,
	BLOCK_END_KEYWORDS,
	BLOCK_MIDDLE_KEYWORDS,
	CASE_KEYWORDS,
	PROCEDURE_LEVEL_KEYWORDS,
	ALL_SQL_FUNCTIONS
} from "./constants/language";
import {
	CONFIG_KEYS,
	CONFIG_DEFAULTS
} from "./constants/config";
import {
	PATTERNS
} from "./constants/patterns";
import { normalizeOperatorSpacing, replaceOutsideStrings } from "./utils/formatters";
import { formatSqlWithStyleImpl, SqlFormattingStyle } from "./commands/formatSql";
import { getConfiguredFunctions } from "./utils/intellisense";

const SQL_CLAUSE_KEYWORDS = [
	"INNER JOIN",
	"LEFT JOIN",
	"RIGHT JOIN",
	"FULL JOIN",
	"GROUP BY",
	"ORDER BY",
	"UNION ALL",
	"UNION",
	"DELETE FROM",
	"INSERT INTO",
	"EXCEPT",
	"INTERSECT",
	"HAVING",
	"VALUES",
	"WHERE",
	"SET",
	"FROM",
	"JOIN",
	"UPDATE"
].sort((a, b) => b.length - a.length);

const SQL_GENERAL_KEYWORDS = [
	"SELECT",
	"DISTINCT",
	"TOP",
	"AS",
	"ON",
	"IN",
	"NOT",
	"BETWEEN",
	"LIKE",
	"IS",
	"NULL",
	"INNER",
	"LEFT",
	"RIGHT",
	"FULL",
	"JOIN",
	"INSERT",
	"INTO",
	"VALUES",
	"UPDATE",
	"SET",
	"DELETE",
	"FROM",
	"WHERE",
	"GROUP",
	"BY",
	"ORDER",
	"HAVING",
	"UNION",
	"ALL",
	"EXCEPT",
	"INTERSECT",
	"AND",
	"OR"
];

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
		const builtinFunctionCase = config.get<string>(CONFIG_KEYS.FORMAT_BUILTIN_FUNCTION_CASE, CONFIG_DEFAULTS[CONFIG_KEYS.FORMAT_BUILTIN_FUNCTION_CASE]);
		const trimTrailingWhitespace = config.get<boolean>(CONFIG_KEYS.FORMAT_TRIM_TRAILING_WHITESPACE, CONFIG_DEFAULTS[CONFIG_KEYS.FORMAT_TRIM_TRAILING_WHITESPACE]);
		const configuredFunctions = getConfiguredFunctions(config);

		// Use editor's tab size for calculating visual positions
		const tabSize = options.tabSize || 4;

		// Normalize line endings to \n for processing
		formatted = formatted.replace(/\r\n/g, "\n");

		// Apply formatting rules
		formatted = this.splitMultipleStatements(formatted);
		formatted = this.normalizeKeywordCase(formatted); // Always UPPER per style guide
		formatted = this.normalizeBuiltinFunctionCase(formatted, builtinFunctionCase, configuredFunctions.map(f => f.name));
		formatted = normalizeOperatorSpacing(formatted);
		formatted = this.normalizeIndentation(formatted, indentStyle, indentWidth, tabSize);
		formatted = this.normalizeBlankLines(formatted);
		formatted = this.formatSqlLiterals(formatted, config);

		// Wrap long lines if configured
		const wrapLength = config.get<number>(CONFIG_KEYS.FORMAT_WRAP_LENGTH, CONFIG_DEFAULTS[CONFIG_KEYS.FORMAT_WRAP_LENGTH]);
		if (wrapLength > 0) {
			formatted = this.wrapLongLines(formatted, wrapLength, indentStyle, tabSize);
		}

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
	 * Preserves inline comments after statements
	 */
	private splitMultipleStatements(text: string): string {
		const lines = text.split('\n');
		let inMultiLineComment = false;
		let inMultiLineString = false;
		let stringDelimiter = "";

		const formattedLines = lines.flatMap(line => {
			const trimmed = line.trim();

			// Track multi-line comment state before string handling
			if (trimmed.startsWith('/*') && !trimmed.endsWith(';')) {
				inMultiLineComment = true;
			}
			if (inMultiLineComment || trimmed.startsWith('/*') || trimmed.startsWith('*')) {
				if (inMultiLineComment && trimmed.endsWith(';')) {
					inMultiLineComment = false;
				}
				return [line]; // Don't split comment content
			}

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

			// Skip keywords and empty lines
			if (trimmed === '' || /^\s*:[A-Z]+\s*;?\s*$/i.test(trimmed)) {
				return [line];
			}

			// Split multiple statements on the same line
			// We need to find semicolons that aren't inside strings or followed by inline comments
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
				} else if (inString && char === stringChar) {
					inString = false;
					stringChar = '';
					currentStatement += char;
				} else if (char === ';' && !inString) {
					// Found a statement terminator outside of strings
					currentStatement += char;

					// Check if there's an inline comment following this semicolon
					// Look ahead to see if the rest of the line starts with a comment
					const restOfLine = trimmed.substring(i + 1).trim();
					const hasInlineComment = restOfLine.startsWith('/*');

					if (hasInlineComment) {
						// Keep the comment with the current statement, with normalized spacing
						currentStatement += ' ' + restOfLine;
						// Push the statement with its inline comment
						const stmt = currentStatement.trim();
						if (stmt) {
							statements.push(leadingWhitespace + stmt);
						}
						// Clear current statement to prevent duplication
						currentStatement = '';
						// We've consumed the rest of the line, so break
						break;
					} else {
						// No inline comment, split normally
						const stmt = currentStatement.trim();
						if (stmt) {
							statements.push(leadingWhitespace + stmt);
						}
						currentStatement = '';
					}
				} else {
					currentStatement += char;
				}
			}

			// Add any remaining content (only if we didn't break out early)
			const remaining = currentStatement.trim();
			if (remaining) {
				statements.push(leadingWhitespace + remaining);
			}

			// If we only found one statement, check if it needs inline comment spacing normalization
			if (statements.length <= 1) {
				// Normalize spacing before inline comments even for single statements
				const hasInlineComment = /;\s{2,}\/\*/.test(trimmed);
				if (hasInlineComment) {
					// Normalize multiple spaces after semicolon to single space
					const normalized = leadingWhitespace + trimmed.replace(/;\s+\/\*/g, '; /*');
					return [normalized];
				}
				return [line];
			}

			return statements;
		});

		return formattedLines.join('\n');
	}

	/**
	 * Normalize keyword casing to UPPERCASE (e.g., :IF, :WHILE, :PROCEDURE)
	 * Per SSL style guide, keywords are always UPPERCASE - this is not configurable.
	 * Only processes keywords outside of strings and comments.
	 */
	private normalizeKeywordCase(text: string): string {
		const keywords = SSL_KEYWORDS;

		const lines = text.split('\n');
		let inMultiLineComment = false;
		let inMultiLineString = false;
		let stringDelimiter = '';

		const formattedLines = lines.map(line => {
			const trimmed = line.trim();

			// Track multi-line comment state before any string detection
			if (trimmed.startsWith('/*') && !trimmed.endsWith(';')) {
				inMultiLineComment = true;
			}
			if (inMultiLineComment || trimmed.startsWith('/*') || trimmed.startsWith('*')) {
				if (inMultiLineComment && trimmed.endsWith(';')) {
					inMultiLineComment = false;
				}
				return line;
			}

			// Track multi-line string state
			if (!inMultiLineString) {
				const doubleQuoteCount = (line.match(/"/g) || []).length;
				const singleQuoteCount = (line.match(/'/g) || []).length;
				const bracketOpenCount = (line.match(/\[/g) || []).length;
				const bracketCloseCount = (line.match(/\]/g) || []).length;

				if (doubleQuoteCount % 2 !== 0) {
					inMultiLineString = true;
					stringDelimiter = '"';
				} else if (singleQuoteCount % 2 !== 0) {
					inMultiLineString = true;
					stringDelimiter = "'";
				} else if (bracketOpenCount !== bracketCloseCount) {
					inMultiLineString = true;
					stringDelimiter = ']';
				}
			} else {
				const delimiterCount = (line.match(new RegExp(stringDelimiter === '"' ? '"' : (stringDelimiter === "'" ? "'" : '\\]'), 'g')) || []).length;
				if (delimiterCount % 2 !== 0 || (stringDelimiter === ']' && delimiterCount > 0)) {
					inMultiLineString = false;
					stringDelimiter = '';
				}
			}

			if (inMultiLineString) {
				return line; // Don't format string content
			}

			let result = line;
			keywords.forEach(keyword => {
				const pattern = new RegExp(`:${keyword}\\b`, "gi");
				const replacement = `:${keyword.toUpperCase()}`;
				result = replaceOutsideStrings(result, pattern, replacement);
			});
			return result;
		});

		return formattedLines.join('\n');
	}

	/**
	 * Normalize built-in function casing to PascalCase (canonical form)
	 * Only processes function names outside of strings and comments.
	 * SSL functions are case-insensitive, but PascalCase is the canonical form.
	 */
	private normalizeBuiltinFunctionCase(text: string, caseStyle: string, functions: string[]): string {
		if (caseStyle === "preserve") {
			return text;
		}

		const lines = text.split('\n');
		let inMultiLineComment = false;
		let inMultiLineString = false;
		let stringDelimiter = '';

		const formattedLines = lines.map(line => {
			const trimmed = line.trim();

			// Track multi-line comment state before string detection
			if (trimmed.startsWith('/*') && !trimmed.endsWith(';')) {
				inMultiLineComment = true;
			}
			if (inMultiLineComment || trimmed.startsWith('/*') || trimmed.startsWith('*')) {
				if (inMultiLineComment && trimmed.endsWith(';')) {
					inMultiLineComment = false;
				}
				return line;
			}

			// If we're already inside a multi-line string, only check for its end
			if (inMultiLineString) {
				const delimiterCount = (line.match(new RegExp(stringDelimiter === '"' ? '"' : (stringDelimiter === "'" ? "'" : '\\]'), 'g')) || []).length;
				if (delimiterCount % 2 !== 0 || (stringDelimiter === ']' && delimiterCount > 0)) {
					inMultiLineString = false;
					stringDelimiter = '';
				}
				return line; // Skip formatting while inside string content
			}

			let result = line;
			functions.forEach(func => {
				// Only match function names when followed by opening parenthesis
				// This prevents matching variable names that happen to match function names
				const pattern = new RegExp(`\\b${func}(?=\\s*\\()`, "gi");
				// PascalCase uses the canonical name from SSL_BUILTIN_FUNCTIONS
				const replacement = func;
				result = replaceOutsideStrings(result, pattern, replacement);
			});

			// Track start of multi-line string for following lines
			const doubleQuoteCount = (line.match(/"/g) || []).length;
			const singleQuoteCount = (line.match(/'/g) || []).length;
			const bracketOpenCount = (line.match(/\[/g) || []).length;
			const bracketCloseCount = (line.match(/\]/g) || []).length;

			if (doubleQuoteCount % 2 !== 0) {
				inMultiLineString = true;
				stringDelimiter = '"';
			} else if (singleQuoteCount % 2 !== 0) {
				inMultiLineString = true;
				stringDelimiter = "'";
			} else if (bracketOpenCount !== bracketCloseCount) {
				inMultiLineString = true;
				stringDelimiter = ']';
			}

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
		let inProcedure = false; // Track if we're inside a procedure
		let groupingDepth = 0;
		const indentChar = indentStyle === "tab" ? "\t" : " ".repeat(indentWidth);

		const blockStart = new RegExp(`^\\s*:(${BLOCK_START_KEYWORDS.join('|')})\\b`, 'i');
		const blockMiddle = new RegExp(`^\\s*:(${BLOCK_MIDDLE_KEYWORDS.join('|')})\\b`, 'i');
		const caseKeyword = new RegExp(`^\\s*:(${CASE_KEYWORDS.join('|')})\\b`, 'i');
		const blockEnd = new RegExp(`^\\s*:(${BLOCK_END_KEYWORDS.join('|')})\\b`, 'i');
		const procedureLevelKeyword = new RegExp(`^\\s*:(${PROCEDURE_LEVEL_KEYWORDS.join('|')})\\b`, 'i');
		const procedureKeyword = /^\s*:PROCEDURE\b/i;
		const endProcKeyword = /^\s*:ENDPROC\b/i;

		const formatted = lines.map((line, index) => {
			const trimmed = line.trim();
			const startsBlockComment = trimmed.startsWith('/*') && !trimmed.endsWith(';');

			// Track multi-line comment boundaries without letting quotes toggle string state
			if (startsBlockComment) {
				inMultiLineComment = true;
			}

			if (inMultiLineComment) {
				if (trimmed.endsWith(';')) {
					inMultiLineComment = false;
				}
				return line; // Preserve multi-line comment content as-is
			}

			const isCommentLine = trimmed.startsWith('/*') || trimmed.startsWith('*');

			// Track multi-line string state only when not inside comments
			// We need to detect this BEFORE processing for indentation
			let lineStartsMultiLineString = false;
			let lineEndsMultiLineString = false;
			if (!isCommentLine) {
				if (!inMultiLineString) {
					const doubleQuoteCount = (line.match(/"/g) || []).length;
					const singleQuoteCount = (line.match(/'/g) || []).length;

					if (doubleQuoteCount % 2 !== 0) {
						inMultiLineString = true;
						lineStartsMultiLineString = true;
						stringDelimiter = '"';
					} else if (singleQuoteCount % 2 !== 0) {
						inMultiLineString = true;
						lineStartsMultiLineString = true;
						stringDelimiter = "'";
					}
				} else {
					const delimiterCount = (line.match(new RegExp(stringDelimiter === '"' ? '"' : "'", 'g')) || []).length;
					if (delimiterCount % 2 !== 0) {
						// String ends on this line - update grouping depth for brackets after the closing quote
						// but preserve the original indentation (it's continuation of string content)
						inMultiLineString = false;
						lineEndsMultiLineString = true;

						// We need to count any brackets after the string ends
						// Find portion after the closing delimiter
						const closingDelim = stringDelimiter;
						stringDelimiter = "";

						// Extract portion after the last occurrence of the closing quote
						const lastQuoteIndex = trimmed.lastIndexOf(closingDelim);
						if (lastQuoteIndex !== -1) {
							const afterString = trimmed.substring(lastQuoteIndex + 1);
							const openingBrackets = (afterString.match(/[\(\{\[]/g) || []).length;
							const closingBrackets = (afterString.match(/[\)\}\]]/g) || []).length;
							groupingDepth = Math.max(0, groupingDepth + openingBrackets - closingBrackets);
						}

						// Don't re-indent the line - preserve original indentation
						return line;
					} else {
						// Still inside multi-line string, don't re-indent
						return line;
					}
				}
			} else if (inMultiLineComment && trimmed.endsWith(';')) {
				inMultiLineComment = false;
			}

			// Skip empty lines
			if (!trimmed) {
				return line;
			}

			// Decrease indent for block end and middle keywords
			if (!isCommentLine && (blockEnd.test(trimmed) || blockMiddle.test(trimmed))) {
				indentLevel = Math.max(0, indentLevel - 1);
				// Exit procedure when we hit ENDPROC and reset indent to 0
				if (endProcKeyword.test(trimmed)) {
					inProcedure = false;
				}
			}

			// Decrease indent for :CASE and :OTHERWISE (they're at same level as :BEGINCASE)
			if (!isCommentLine && caseKeyword.test(trimmed)) {
				indentLevel = Math.max(0, indentLevel - 1);
			}

			// Calculate the current indentation level for this line
			let currentIndentLevel = indentLevel;

			// Procedure-level keywords should stay at column 0
			const isProcedureLevelKeyword = procedureLevelKeyword.test(trimmed);

			if (isProcedureLevelKeyword) {
				currentIndentLevel = 0;
			}
			// Single-line comments (/* ... ;) should match the indentation of the current code block
			else if (trimmed.startsWith("/*") && trimmed.endsWith(";")) {
				currentIndentLevel = indentLevel;
			}

			// Strip all string content from line for bracket counting
			// This handles both inline strings and the start of multi-line strings
			const sanitized = this.stripAllStringContent(trimmed);
			const groupingClosers = this.countLeadingClosers(sanitized);
			const groupingOffset = Math.max(0, groupingDepth - groupingClosers);

			let appliedIndentLevel = currentIndentLevel;
			if (!isProcedureLevelKeyword) {
				appliedIndentLevel += groupingOffset;
			}

			// Apply indentation
			const indented = indentChar.repeat(appliedIndentLevel) + trimmed;

			// Track PROCEDURE blocks - set indent to 1 for procedure body AFTER formatting the PROCEDURE line
			if (procedureKeyword.test(trimmed)) {
				inProcedure = true;
				indentLevel = 1;
			}

			// Increase indent after block start keywords (but NOT after PROCEDURE - handled above)
			if (!isCommentLine && blockStart.test(trimmed) && !procedureKeyword.test(trimmed)) {
				indentLevel++;
			}

			// Restore indent after middle keywords
			if (!isCommentLine && blockMiddle.test(trimmed)) {
				indentLevel++;
			}

			// Increase indent after :CASE and :OTHERWISE for their body
			if (!isCommentLine && caseKeyword.test(trimmed)) {
				indentLevel++;
			}

			// Count grouping brackets only from non-string code portions
			if (!isCommentLine) {
				const openingBrackets = (sanitized.match(/[\(\{\[]/g) || []).length;
				const closingBrackets = (sanitized.match(/[\)\}\]]/g) || []).length;
				groupingDepth = Math.max(0, groupingDepth + openingBrackets - closingBrackets);
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

			// Handle comment lines before string detection
			if (trimmed.startsWith('/*') && !trimmed.endsWith(';')) {
				inMultiLineComment = true;
			}
			if (inMultiLineComment || trimmed.startsWith("/*") || trimmed.startsWith("*")) {
				if (inMultiLineComment && trimmed.endsWith(';')) {
					inMultiLineComment = false;
				}
				return line;
			}

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

			// Skip empty lines or comment lines
			if (!trimmed) {
				return line;
			}

			// Check if this is a continuation line
			let isContinuation = false;
			if (index > 0) {
				const prevLine = lines[index - 1].trim();
				const prevEndsWithOperator = /[+\-*/]$/.test(prevLine);
				const currentStartsWithOperator = /^[+\-*/]/.test(trimmed);

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
				// For other cases, preserve blank lines but collapse excessive ones
				// Allow up to 10 consecutive blank lines for visual separation
				const blanksToAdd = Math.min(blankLineCount, 10);
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

	private formatSqlLiterals(
		text: string,
		config: vscode.WorkspaceConfiguration
	): string {
		const enabled = config.get<boolean>(CONFIG_KEYS.FORMAT_SQL_ENABLED, CONFIG_DEFAULTS[CONFIG_KEYS.FORMAT_SQL_ENABLED]);
		if (!enabled) {
			return text;
		}

		const keywordCase = config.get<string>(CONFIG_KEYS.FORMAT_SQL_KEYWORD_CASE, CONFIG_DEFAULTS[CONFIG_KEYS.FORMAT_SQL_KEYWORD_CASE]) || "upper";
		const indentSpaces = config.get<number>(CONFIG_KEYS.FORMAT_SQL_INDENT_SPACES, CONFIG_DEFAULTS[CONFIG_KEYS.FORMAT_SQL_INDENT_SPACES]) || 0;
		const normalizedIndent = Math.max(0, indentSpaces);
		const style = config.get<SqlFormattingStyle>(
			CONFIG_KEYS.FORMAT_SQL_STYLE,
			CONFIG_DEFAULTS[CONFIG_KEYS.FORMAT_SQL_STYLE] as SqlFormattingStyle
		);
		const functionPattern = ALL_SQL_FUNCTIONS.join("|");
		if (!functionPattern) {
			return text;
		}

		// Use a regex that matches SQL function calls with string literals, including multi-line
		// [\s\S]*? matches any character including newlines (non-greedy)
		const sqlLiteralRegex = new RegExp(`\\b(${functionPattern})\\s*\\(\\s*("([\\s\\S]*?)")`, "gi");

		let result = text;
		let match;

		// We need to process matches from end to start to preserve positions
		const matches: Array<{
			fullMatch: string;
			functionName: string;
			literal: string;
			content: string;
			index: number;
			quoteColumn: number;
		}> = [];

		while ((match = sqlLiteralRegex.exec(text)) !== null) {
			const functionName = match[1];
			const literal = match[2];
			const content = match[3];

			if (typeof literal !== "string" || typeof content !== "string") {
				continue;
			}

			// Calculate the column position of the opening quote
			// Find the start of the line containing this match
			const textBeforeMatch = text.substring(0, match.index);
			const lastNewlinePos = textBeforeMatch.lastIndexOf('\n');
			const lineStart = lastNewlinePos === -1 ? 0 : lastNewlinePos + 1;
			const lineBeforeQuote = text.substring(lineStart, match.index);

			// The quote is after the function name and opening paren
			const quoteColumn = lineBeforeQuote.length + functionName.length +
				text.substring(match.index + functionName.length).match(/^\s*\(\s*/)![0].length;

			matches.push({
				fullMatch: match[0],
				functionName,
				literal,
				content,
				index: match.index,
				quoteColumn
			});
		}

		// Process matches from end to start to preserve string positions
		for (let i = matches.length - 1; i >= 0; i--) {
			const m = matches[i];

			// Format the SQL content
			const formattedContent = formatSqlWithStyleImpl(
				m.content,
				style || "canonicalCompact",
				keywordCase,
				normalizedIndent
			);

			// Apply continuation indent for multi-line formatted SQL
			const continuationIndent = ' '.repeat(m.quoteColumn + 1);
			const sqlLines = formattedContent.split('\n');

			let formattedSql: string;
			if (sqlLines.length === 1) {
				formattedSql = formattedContent;
			} else {
				// First line stays as-is, continuation lines get indented
				formattedSql = sqlLines[0] + '\n' +
					sqlLines.slice(1).map(l => continuationIndent + l).join('\n');
			}

			const formattedLiteral = `"${formattedSql}"`;

			// Replace the literal in the result
			// Find the position of this literal in the current result
			const literalStart = result.indexOf(m.literal, m.index);
			if (literalStart !== -1) {
				result = result.substring(0, literalStart) +
					formattedLiteral +
					result.substring(literalStart + m.literal.length);
			}
		}

		return result;
	}

	private applySqlKeywordCase(keyword: string, style: string, original?: string): string {
		if (style === "lower") {
			return keyword.toLowerCase();
		}
		if (style === "upper") {
			return keyword.toUpperCase();
		}
		return original ?? keyword;
	}

	/**
	 * Strip all string content from a line for bracket counting.
	 * Handles both complete inline strings and strings that start but don't end (multi-line string starts).
	 */
	private stripAllStringContent(line: string): string {
		let result = '';
		let i = 0;
		let inString = false;
		let stringChar = '';

		while (i < line.length) {
			const char = line[i];

			if (!inString) {
				if (char === '"' || char === "'") {
					// Start of a string - skip it
					inString = true;
					stringChar = char;
				} else {
					result += char;
				}
			} else {
				// Inside a string - look for the closing quote
				if (char === stringChar) {
					inString = false;
					stringChar = '';
				}
				// Don't add string content to result
			}
			i++;
		}

		return result;
	}

	private countLeadingClosers(line: string): number {
		const trimmed = line.trimStart();
		const match = trimmed.match(/^([)\]\}]+)/);
		return match ? match[1].length : 0;
	}
	/**
	 * Wrap long lines that exceed the specified length
	 */
	private wrapLongLines(text: string, wrapLength: number, indentStyle: string, tabSize: number): string {
		const lines = text.split('\n');
		const result: string[] = [];
		let inMultiLineComment = false;
		let inMultiLineString = false;
		let stringDelimiter = '';

		for (let i = 0; i < lines.length; i++) {
			const line = lines[i];
			const trimmed = line.trim();

			// Track multi-line comment state (SSL uses /* ... ; syntax)
			if (trimmed.startsWith('/*') && !trimmed.endsWith(';')) {
				inMultiLineComment = true;
			}
			if (inMultiLineComment) {
				result.push(line);
				if (trimmed.endsWith(';')) {
					inMultiLineComment = false;
				}
				continue;
			}

			// Track multi-line string state
			if (!inMultiLineString) {
				const doubleQuotes = (line.match(/"/g) || []).length;
				const singleQuotes = (line.match(/'/g) || []).length;
				if (doubleQuotes % 2 !== 0) {
					inMultiLineString = true;
					stringDelimiter = '"';
				} else if (singleQuotes % 2 !== 0) {
					inMultiLineString = true;
					stringDelimiter = "'";
				}
			} else {
				const quotes = (line.match(new RegExp(stringDelimiter, 'g')) || []).length;
				if (quotes % 2 !== 0) {
					inMultiLineString = false;
					stringDelimiter = '';
				}
				result.push(line);
				continue;
			}

			// Skip lines that don't need wrapping
			if (line.length <= wrapLength) {
				result.push(line);
				continue;
			}

			// Skip single-line comments and empty lines
			if (!trimmed || trimmed.includes('/*')) {
				result.push(line);
				continue;
			}

			// Capture the original indentation
			const indentMatch = line.match(/^(\s*)/);
			const originalIndent = indentMatch ? indentMatch[1] : '';

			// Try to wrap based on different patterns
			let wrapped: string[] | null = null;

			// Pattern 1: Assignment with value
			const assignmentMatch = trimmed.match(/^([a-zA-Z_]\w*)\s*:=\s*(.+);$/);
			if (assignmentMatch) {
				const varName = assignmentMatch[1];
				const value = assignmentMatch[2];
				wrapped = this.wrapAssignment(varName, value, wrapLength, originalIndent);
			}

			// Pattern 2: Standalone function call (no assignment)
			if (!wrapped) {
				const funcCallMatch = trimmed.match(/^([a-zA-Z_]\w*)\s*\((.+)\)\s*;$/);
				if (funcCallMatch) {
					const funcName = funcCallMatch[1];
					const args = funcCallMatch[2];
					wrapped = this.wrapFunctionCall(funcName, args, wrapLength, originalIndent);
				}
			}

			// Pattern 3: Long string concatenation with + on the line
			if (!wrapped && /\+\s*["']/.test(trimmed) && (trimmed.startsWith('"') || trimmed.startsWith("'"))) {
				wrapped = this.wrapConcatenatedString(trimmed, wrapLength, originalIndent);
			}

			// Pattern 4: Line with logical operators .AND. .OR.
			if (!wrapped && /\.(?:AND|OR|NOT)\./.test(trimmed)) {
				wrapped = this.wrapAtLogicalOperators(trimmed, wrapLength, originalIndent);
			}

			if (wrapped && wrapped.length > 0) {
				// Apply original indentation to all wrapped lines
				result.push(...wrapped.map(l => originalIndent + l));
			} else {
				// If we couldn't wrap it, keep the original line
				result.push(line);
			}
		}

		return result.join('\n');
	}

	/**
	 * Wrap an assignment statement
	 */
	private wrapAssignment(varName: string, value: string, wrapLength: number, indent: string): string[] | null {
		let result: string[] | null = null;

		// Check if it's a string
		if (value.startsWith('"') || value.startsWith("'")) {
			result = this.wrapString(varName, value, wrapLength);
			if (result) { return result; }
		}

		// Check if it's a bracketed list (function call, array, etc.)
		if (/[\(\{]/.test(value)) {
			result = this.wrapBracketedList(varName, value, wrapLength);
			if (result) { return result; }
		}

		// Check if it's a logical expression
		if (/\.(?:AND|OR|NOT)\./i.test(value)) {
			result = this.wrapLogicalExpression(varName, value, wrapLength);
			if (result) { return result; }
		}

		return null;
	}
	/**
	 * Wrap a standalone function call
	 * NOTE: Does NOT add indentation - caller must prepend originalIndent to each returned line
	 */
	private wrapFunctionCall(funcName: string, args: string, wrapLength: number, indent: string): string[] | null {
		const prefix = `${funcName}(`;
		// Continuation lines align with the character after the opening paren
		// Since caller adds originalIndent, we just need spaces for the function name and paren
		const continuationIndent = ' '.repeat(prefix.length);

		// Try to split arguments at commas
		const argList = this.splitAtTopLevelCommas(args);
		if (argList.length <= 1) {
			return null;
		}

		// Build wrapped lines WITHOUT indentation (caller will add originalIndent)
		const result: string[] = [];
		let currentLine = prefix + argList[0];

		for (let i = 1; i < argList.length; i++) {
			const arg = argList[i];
			const testLine = currentLine + ', ' + arg.trim();

			// Check against wrapLength accounting for the indent that will be added
			if (testLine.length + indent.length > wrapLength || i === argList.length - 1) {
				// Line is too long or last argument, wrap at this point
				if (i < argList.length - 1) {
					result.push(currentLine + ',');
					currentLine = continuationIndent + arg.trim();
				} else {
					// Last argument, close the function call
					if (testLine.length + indent.length <= wrapLength) {
						result.push(testLine + ');');
					} else {
						result.push(currentLine + ',');
						result.push(continuationIndent + arg.trim() + ');');
					}
				}
			} else {
				currentLine = testLine;
			}
		}

		// If only one result line, return null to keep original
		if (result.length <= 1) {
			return null;
		}

		return result;
	}

	/**
	 * Wrap a concatenated string at + operators
	 * NOTE: Does NOT add indentation - caller must prepend originalIndent to each returned line
	 */
	private wrapConcatenatedString(content: string, wrapLength: number, indent: string): string[] | null {
		// Find + operators outside strings
		const parts: string[] = [];
		let current = '';
		let inString = false;
		let stringChar = '';
		const trimmedContent = content.trim();
		const hasTrailingPlus = trimmedContent.endsWith('+');

		for (let i = 0; i < content.length; i++) {
			const char = content[i];

			if (!inString && (char === '"' || char === "'")) {
				inString = true;
				stringChar = char;
				current += char;
			} else if (inString && char === stringChar) {
				inString = false;
				stringChar = '';
				current += char;
			} else if (!inString && char === '+') {
				// Only push if we have content (avoids pushing empty if string starts with + or has ++??)
				// Actually, if we have "A" + "B", finding + pushes "A".
				// If we have "A" + "B" +, finding last + pushes "B".
				if (current.trim()) {
					parts.push(current.trim());
				}
				current = '';
			} else {
				current += char;
			}
		}
		if (current.trim()) {
			parts.push(current.trim());
		}

		if (parts.length <= 1 && !hasTrailingPlus) {
			return null;
		}

		// Build wrapped lines
		const result: string[] = [];
		// Start with first part. Caller adds originalIndent.
		let currentLine = parts[0];
		// Standard continuation indent is 4 spaces relative to formatted indent
		const continuationIndent = '    ';

		for (let i = 1; i < parts.length; i++) {
			const part = parts[i];
			// Calculate length including the indent the caller will add
			const testLine = currentLine + ' + ' + part;

			if (testLine.length + indent.length > wrapLength) {
				result.push(currentLine + ' +');
				currentLine = continuationIndent + part;
			} else {
				currentLine = testLine;
			}
		}

		// Handle trailing plus if it existed in original
		if (hasTrailingPlus) {
			const testLine = currentLine + ' +';
			if (testLine.length + indent.length > wrapLength) {
				result.push(currentLine); // Push current line without the plus
				currentLine = continuationIndent + '+'; // Start new line with just the plus
			} else {
				currentLine += ' +';
			}
		}

		result.push(currentLine);

		return result.length > 1 ? result : null;
	}

	/**
	 * Wrap at logical operators (.AND., .OR., .NOT.)
	 */
	private wrapAtLogicalOperators(content: string, wrapLength: number, indent: string): string[] | null {
		// Split at .AND. or .OR. operators
		const parts = content.split(/(?=\.(?:AND|OR)\.)/i);

		if (parts.length <= 1) {
			return null;
		}

		const result: string[] = [];
		let currentLine = indent + parts[0].trimEnd();
		const continuationIndent = indent + '    '; // 4-space continuation

		for (let i = 1; i < parts.length; i++) {
			const part = parts[i];
			const testLine = currentLine + ' ' + part.trim();

			if (testLine.length > wrapLength) {
				result.push(currentLine);
				currentLine = continuationIndent + part.trim();
			} else {
				currentLine = testLine;
			}
		}
		result.push(currentLine);

		return result.length > 1 ? result : null;
	}

	/**
	 * Split a string at top-level commas (not inside brackets or strings)
	 */
	private splitAtTopLevelCommas(content: string): string[] {
		const parts: string[] = [];
		let current = '';
		let depth = 0;
		let inString = false;
		let stringChar = '';

		for (let i = 0; i < content.length; i++) {
			const char = content[i];

			if (!inString && (char === '"' || char === "'")) {
				inString = true;
				stringChar = char;
				current += char;
			} else if (inString && char === stringChar) {
				inString = false;
				stringChar = '';
				current += char;
			} else if (!inString && (char === '(' || char === '{' || char === '[')) {
				depth++;
				current += char;
			} else if (!inString && (char === ')' || char === '}' || char === ']')) {
				depth--;
				current += char;
			} else if (!inString && depth === 0 && char === ',') {
				parts.push(current.trim());
				current = '';
			} else {
				current += char;
			}
		}
		if (current.trim()) {
			parts.push(current.trim());
		}

		return parts;
	}

	/**
	 * Wrap a long string or string concatenation assignment across multiple lines
	 */
	private wrapString(varName: string, value: string, wrapLength: number): string[] | null {
		// Parse the string parts (might already be concatenated with +)
		let parts: string[] = [];
		let current = '';
		let inString = false;
		let stringChar = '';

		for (let i = 0; i < value.length; i++) {
			const char = value[i];

			if (!inString && (char === '"' || char === "'")) {
				inString = true;
				stringChar = char;
				current += char;
			} else if (inString && char === stringChar) {
				current += char;
				inString = false;
			} else if (!inString && char === '+') {
				// Found a concatenation operator
				if (current.trim()) {
					parts.push(current.trim());
				}
				current = '';
			} else {
				current += char;
			}
		}

		// Add the last part
		const lastPart = current.trim();
		if (lastPart && lastPart !== ';') {
			// Remove semicolon if it's at the end
			parts.push(lastPart.endsWith(';') ? lastPart.slice(0, -1) : lastPart);
		}

		// If only one part, try to split it
		if (parts.length === 1) {
			const singleString = parts[0];
			// Only split if it's a quoted string and it's too long
			if ((singleString.startsWith('"') || singleString.startsWith("'")) && singleString.length > 40) {
				parts = this.splitLongString(singleString, 60); // Reasonable chunk size
			}
		}

		// If still just one part or no parts, can't wrap
		if (parts.length <= 1) {
			return null;
		}

		// Build wrapped lines without indentation (caller will apply original line's indentation)
		const wrapped: string[] = [];

		// First line: varName := firstPart +
		wrapped.push(`${varName} := ${parts[0]} +`);

		// Middle lines: part +
		for (let i = 1; i < parts.length - 1; i++) {
			wrapped.push(`${parts[i]} +`);
		}

		// Last line: lastPart;
		wrapped.push(`${parts[parts.length - 1]};`);

		return wrapped;
	}

	/**
	 * Split a single long string into multiple parts at whitespace boundaries only
	 */
	private splitLongString(str: string, maxLength: number): string[] {
		// Remove quotes
		const quote = str[0];
		const content = str.slice(1, -1);

		if (content.length <= maxLength) {
			return [str];
		}

		const parts: string[] = [];
		let remaining = content;

		while (remaining.length > 0) {
			if (remaining.length <= maxLength) {
				parts.push(`${quote}${remaining}${quote}`);
				break;
			}

			// Find the last space within maxLength to break at a word boundary
			let breakPoint = remaining.lastIndexOf(' ', maxLength);

			// If no space found within maxLength, look for the first space after maxLength
			// This ensures we never break mid-word
			if (breakPoint === -1) {
				breakPoint = remaining.indexOf(' ', maxLength);
				// If still no space found, the remaining string has no spaces - keep it whole
				if (breakPoint === -1) {
					parts.push(`${quote}${remaining}${quote}`);
					break;
				}
			}

			const part = remaining.substring(0, breakPoint).trimEnd();
			parts.push(`${quote}${part}${quote}`);
			remaining = remaining.substring(breakPoint).trimStart();
		}

		return parts;
	}

	/**
	 * Wrap a long bracketed list (function call, array literal, etc.) across multiple lines
	 */
	private wrapBracketedList(varName: string, value: string, wrapLength: number): string[] | null {
		// Match bracketed patterns: FunctionName(...) or {...}
		const bracketMatch = value.match(/^(\w+)?\s*([\(\{])(.+)([\)\}]);?$/);
		if (!bracketMatch) {
			return null;
		}

		const prefix = bracketMatch[1] || ''; // Function name if it's a function call
		const openBracket = bracketMatch[2];
		const content = bracketMatch[3];
		const closeBracket = bracketMatch[4];

		// Parse items (comma-separated)
		const items: string[] = [];
		let current = '';
		let depth = 0;
		let inString = false;
		let stringChar = '';

		for (let i = 0; i < content.length; i++) {
			const char = content[i];

			if (!inString && (char === '"' || char === "'")) {
				inString = true;
				stringChar = char;
			} else if (inString && char === stringChar) {
				inString = false;
			}

			if (!inString) {
				if (char === '(' || char === '{' || char === '[') { depth++; }
				else if (char === ')' || char === '}' || char === ']') { depth--; }
				else if (char === ',' && depth === 0) {
					items.push(current.trim());
					current = '';
					continue;
				}
			}

			current += char;
		}

		if (current.trim()) {
			items.push(current.trim());
		}

		// If only one item or can't wrap, return null
		if (items.length <= 1) {
			return null;
		}

		// Decide wrapping strategy: condensed vs expanded
		// Use condensed if items are short (avg < 20 chars) and not too many (< 8 items)
		const avgItemLength = items.reduce((sum, item) => sum + item.length, 0) / items.length;
		const maxItemLength = Math.max(...items.map(i => i.length));
		const useCondensed = avgItemLength < 20 && items.length < 8 && maxItemLength < 40;

		if (useCondensed) {
			return this.wrapBracketedListCondensed(varName, prefix, openBracket, closeBracket, items, wrapLength);
		} else {
			return this.wrapBracketedListExpanded(varName, prefix, openBracket, closeBracket, items);
		}
	}

	/**
	 * Wrap bracketed list in condensed mode - multiple items per line
	 */
	private wrapBracketedListCondensed(varName: string, prefix: string, openBracket: string, closeBracket: string, items: string[], wrapLength: number): string[] {
		const wrapped: string[] = [];
		const firstLinePrefix = `${varName} := ${prefix}${openBracket}`;
		const targetLineLength = wrapLength - 10; // Leave some margin

		let currentLine = firstLinePrefix;

		for (let i = 0; i < items.length; i++) {
			const item = items[i];
			const isLast = i === items.length - 1;
			const separator = isLast ? closeBracket + ';' : ', ';
			const itemWithSep = item + separator;

			// Check if adding this item would exceed the line length
			if (currentLine.length + itemWithSep.length > targetLineLength && currentLine !== firstLinePrefix) {
				// Finish current line and start new one
				wrapped.push(currentLine);
				currentLine = itemWithSep;
			} else {
				// Add to current line
				if (currentLine === firstLinePrefix) {
					currentLine += item + (isLast ? closeBracket + ';' : ',');
				} else {
					currentLine += ' ' + item + (isLast ? closeBracket + ';' : ',');
				}
			}
		}

		// Add the last line if it has content
		if (currentLine !== firstLinePrefix) {
			wrapped.push(currentLine);
		}

		return wrapped;
	}

	/**
	 * Wrap bracketed list in expanded mode - one item per line
	 */
	private wrapBracketedListExpanded(varName: string, prefix: string, openBracket: string, closeBracket: string, items: string[]): string[] {
		const wrapped: string[] = [];

		// First line: varName := prefix(item1, or varName := {item1,
		if (prefix) {
			wrapped.push(`${varName} := ${prefix}${openBracket}${items[0]},`);
		} else {
			wrapped.push(`${varName} := ${openBracket}${items[0]},`);
		}

		// Middle lines: item,
		for (let i = 1; i < items.length - 1; i++) {
			wrapped.push(`${items[i]},`);
		}

		// Last line: lastItem);
		wrapped.push(`${items[items.length - 1]}${closeBracket};`);

		return wrapped;
	}

	/**
	 * Wrap a long logical expression across multiple lines
	 */
	private wrapLogicalExpression(varName: string, value: string, wrapLength: number): string[] | null {
		// Split by logical operators while preserving them
		const parts: string[] = [];
		let current = '';
		let inString = false;
		let stringChar = '';
		let i = 0;

		while (i < value.length) {
			const char = value[i];

			if (!inString && (char === '"' || char === "'")) {
				inString = true;
				stringChar = char;
				current += char;
				i++;
			} else if (inString && char === stringChar) {
				inString = false;
				current += char;
				i++;
			} else if (!inString && value.substring(i).match(/^\s*(\.(?:AND|OR|NOT)\.)/i)) {
				// Found a logical operator
				const match = value.substring(i).match(/^\s*(\.(?:AND|OR|NOT)\.)/i);
				if (match) {
					// Push current part
					if (current.trim()) {
						parts.push(current.trim());
					}
					// Start new part with the operator
					current = match[1] + ' ';
					i += match[0].length;
				}
			} else {
				current += char;
				i++;
			}
		}

		// Add the last part (remove trailing semicolon)
		const lastPart = current.trim();
		if (lastPart && lastPart !== ';') {
			parts.push(lastPart.endsWith(';') ? lastPart.slice(0, -1) : lastPart);
		}

		// If only one part, can't wrap
		if (parts.length <= 1) {
			return null;
		}

		// Build wrapped lines without indentation (caller will apply original line's indentation)
		const wrapped: string[] = [];

		// Check if first part starts with a logical operator
		if (parts[0].match(/^\.(AND|OR|NOT)\./i)) {
			// First line: varName := operator rest
			wrapped.push(`${varName} := ${parts[0]}`);
		} else {
			// First line: varName := firstPart
			wrapped.push(`${varName} := ${parts[0]}`);
		}

		// Remaining lines with operators
		for (let i = 1; i < parts.length - 1; i++) {
			wrapped.push(parts[i]);
		}

		// Last line with semicolon
		wrapped.push(`${parts[parts.length - 1]};`);

		return wrapped;
	}
}
