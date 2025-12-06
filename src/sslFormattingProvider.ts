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
		formatted = this.consolidateStatements(formatted); // Join split statements to allow proper reflow
		formatted = this.normalizeKeywordCase(formatted); // Always UPPER per style guide
		formatted = this.normalizeBuiltinFunctionCase(formatted, builtinFunctionCase, configuredFunctions.map(f => f.name));
		formatted = normalizeOperatorSpacing(formatted);
		formatted = this.normalizeIndentation(formatted, indentStyle, indentWidth, tabSize);
		formatted = this.normalizeBlankLines(formatted);
		formatted = this.formatSqlLiterals(formatted, config, tabSize);

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
	 * Consolidate split statements into single lines to allow for proper re-wrapping.
	 * Merges lines that do not end in a semicolon or are not comments.
	 */
	private consolidateStatements(text: string): string {
		const lines = text.split('\n');
		const result: string[] = [];
		let currentStatement = "";
		let inMultiLineComment = false;
		let inMultiLineString = false;
		let stringDelimiter = "";

		for (let i = 0; i < lines.length; i++) {
			const line = lines[i];
			const trimmed = line.trim();

			// Handle block comments logic
			if (trimmed.startsWith('/*') && !trimmed.endsWith(';')) {
				inMultiLineComment = true;
			}

			// If inside multi-line comment or line is a comment, don't merge
			if (inMultiLineComment || trimmed.startsWith('/*') || trimmed.startsWith('*') || trimmed.startsWith('//')) {
				if (currentStatement) {
					result.push(currentStatement);
					currentStatement = "";
				}
				result.push(line);

				if (inMultiLineComment && trimmed.endsWith(';')) {
					inMultiLineComment = false;
				}
				continue;
			}

			// Empty line -> break accumulation
			if (!trimmed) {
				if (currentStatement) {
					result.push(currentStatement);
					currentStatement = "";
				}
				result.push(line);
				continue;
			}

			// Track multi-line string state
			// If we are already in a multi-line string, we check for closure
			// If not, we check for opening
			let lineEndsInMultiLineString = inMultiLineString;

			// We need to count quotes carefully
			// Simplified check: count delimiters
			if (!inMultiLineString) {
				const doubleQuoteCount = (line.match(/"/g) || []).length;
				const singleQuoteCount = (line.match(/'/g) || []).length;
				// Check for odd number of quotes
				if (doubleQuoteCount % 2 !== 0) {
					lineEndsInMultiLineString = true;
					stringDelimiter = '"';
				} else if (singleQuoteCount % 2 !== 0) {
					lineEndsInMultiLineString = true;
					stringDelimiter = "'";
				}
			} else {
				// Check if enclosed
				const delimiterCount = (line.match(new RegExp(stringDelimiter === '"' ? '"' : "'", 'g')) || []).length;
				if (delimiterCount % 2 !== 0) {
					lineEndsInMultiLineString = false;
					stringDelimiter = "";
				}
			}

			// Accumulate (handle space separation)
			if (currentStatement) {
				// If we were in a multi-line string, we shouldn't have merged?
				// Actually, if we flushed, currentStatement is empty.
				// If we didn't flush, we add space.
				currentStatement += " " + trimmed;
			} else {
				currentStatement = line.trimEnd();
			}

			// Check if statement ends here
			let effectiveEnd = trimmed;
			if (trimmed.endsWith('*/')) {
				const lastCommentStart = trimmed.lastIndexOf('/*');
				if (lastCommentStart !== -1) {
					effectiveEnd = trimmed.substring(0, lastCommentStart).trim();
				}
			}

			// If line ends in a multi-line string, FLUSH immediately to preserve the line break
			if (lineEndsInMultiLineString) {
				result.push(currentStatement);
				currentStatement = "";
				inMultiLineString = true; // Set state for next interaction
				continue;
			}
			inMultiLineString = false; // Reset if we closed it

			if (effectiveEnd.endsWith(';') ||
				effectiveEnd.endsWith(':BEGINCASE') ||
				effectiveEnd.endsWith(':EXITCASE') ||
				/^:(CASE|DEFAULT|OTHERWISE)\b/i.test(effectiveEnd)
			) {
				result.push(currentStatement);
				currentStatement = "";
			}
		}

		if (currentStatement) {
			result.push(currentStatement);
		}

		return result.join('\n');
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
			const previousInMultiLineString = inMultiLineString;
			const nextState = this.getMultiLineStringState(line, inMultiLineString, stringDelimiter);
			inMultiLineString = nextState.inMultiLineString;
			stringDelimiter = nextState.stringDelimiter;

			if (inMultiLineString) {
				return line; // Don't format string content
			}

			let result = line;
			keywords.forEach(keyword => {
				const pattern = new RegExp(`:${keyword}\\b`, "gi");
				const replacement = `:${keyword.toUpperCase()}`;
				result = replaceOutsideStrings(result, pattern, replacement);
			});

			// Update multi-line string state for next line
			// We know we started with inMultiLineString=false if we reached here
			const state = this.getMultiLineStringState(result, false, '');
			inMultiLineString = state.inMultiLineString;
			stringDelimiter = state.stringDelimiter;

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

			// Update multi-line string state for next line
			// We know we started with inMultiLineString=false if we reached here
			const state = this.getMultiLineStringState(result, false, '');
			inMultiLineString = state.inMultiLineString;
			stringDelimiter = state.stringDelimiter;

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
		let finalPass = this.normalizeBlankLines(secondPass);

		return finalPass;
	}

	/**
	 * Third pass: Normalize vertical whitespace (blank lines)
	 * Enforce blank lines around block structures for readability
	 */
	private normalizeBlankLines(text: string): string {
		// Split into lines
		const lines = text.split('\n');
		const result: string[] = [];

		// Regex patterns for detecting block boundaries
		const blockStartPattern = new RegExp(`^\\s*:(${BLOCK_START_KEYWORDS.join('|')})\\b`, 'i');
		const blockEndPattern = new RegExp(`^\\s*:(${BLOCK_END_KEYWORDS.join('|')})\\b`, 'i');
		const blockMiddlePattern = new RegExp(`^\\s*:(${BLOCK_MIDDLE_KEYWORDS.join('|')})\\b`, 'i');
		const casePattern = new RegExp(`^\\s*:(${CASE_KEYWORDS.join('|')})\\b`, 'i');
		const exitCasePattern = /^\s*:EXITCASE\b/i;
		const parametersPattern = /^\s*:PARAMETERS\b/i;
		const defaultPattern = /^\s*:DEFAULT\b/i;
		const declarePattern = /^\s*:DECLARE\b/i;

		// Helper to check if a line is a comment
		const isComment = (line: string) => {
			const trimmed = line.trim();
			return trimmed.startsWith('/*') || trimmed.startsWith('*');
		};

		// Helper to check if a line is separate code (not start of this block or end of prev block)

		// Helper to check if a line is separate code (not start of this block or end of prev block)
		const isCode = (line: string) => {
			const trimmed = line.trim();
			return trimmed.length > 0 && !isComment(line);
		};

		let inMultiLineComment = false;

		for (let i = 0; i < lines.length; i++) {
			const line = lines[i];
			const trimmed = line.trim();

			// Handle multi-line comments state
			if (inMultiLineComment) {
				result.push(line);
				if (trimmed.endsWith(';')) {
					inMultiLineComment = false;
				}
				continue;
			}

			if (trimmed.startsWith('/*') && !trimmed.endsWith(';')) {
				inMultiLineComment = true;
			}

			// SKIP processing for empty lines (we handle insertion/removal)
			if (!trimmed) {
				// Simplified approach: Add existing blank line if previous wasn't blank
				if (result.length > 0 && result[result.length - 1].trim() !== '') {
					result.push('');
				}
				continue;
			}

			// Check if we need to insert a blank line BEFORE this line
			// feature: Comment Vertical Spacing
			// Enforce newline before comment unless it follows a block start/middle or another comment
			if (isComment(line)) {
				if (result.length > 0) {
					const lastLine = result[result.length - 1];
					const lastTrimmed = lastLine.trim();

					if (lastTrimmed) { // Last line was not blank
						const isLastComment = isComment(lastLine);
						const isLastBlockS = blockStartPattern.test(lastLine);
						const isLastBlockM = blockMiddlePattern.test(lastLine);
						const isLastCase = casePattern.test(lastLine);
						const isLastDocHeader = lastTrimmed.startsWith('/*region') || lastTrimmed.startsWith(':PARAMETERS'); // Heuristic

						if (!isLastComment && !isLastBlockS && !isLastBlockM && !isLastCase && !isLastDocHeader) {
							result.push('');
						}
					}
				}
			}
			// Condition: This line is a Block Start
			if (blockStartPattern.test(line)) {
				// Check strict predecessors (ignoring comments attached to this block)
				// We want to group comments with the block.
				// But we are iterating forward. If the previous lines were comments, 
				// we might have already output them.
				// If we already output comments, we missed the chance to insert BEFORE them unless we buffered.

				// If this line is a block start, we check if the LAST OUTPUT LINE needs separation.
				if (result.length > 0) {
					const lastLine = result[result.length - 1];
					const lastTrimmed = lastLine.trim();

					if (lastTrimmed) { // If last line was not blank
						// Don't insert if last line was:
						// 1. A block start (Nested)
						// 2. ELSE/CASE (Block middle)
						// 3. A comment? (See below)

						const isLastStart = blockStartPattern.test(lastLine);
						const isLastMiddle = blockMiddlePattern.test(lastLine) || casePattern.test(lastLine);
						const isLastComment = isComment(lastLine);
						const isLastDocHeader = lastTrimmed.startsWith('/*region') || lastTrimmed.startsWith(':PARAMETERS'); // Heuristic

						// For comments: If last was comment, assume it belongs to THIS block if no blank line exists.
						// The blank line provided by user would be in 'result' (as '') if we preserved it.
						// If user didn't have blank line, we assume comment belongs to Previous or This?
						// Usually comments immediately preceding block belong to block.
						// We can't easily move the blank line before the comments retrospectively without backtracking result.

						// Backtracking result approach:
						// Scan back `result` to find start of comment block.
						let insertIndex = result.length;
						let shouldInsert = true;

						// Walk back over comments
						let tempIdx = result.length - 1;
						while (tempIdx >= 0 && isComment(result[tempIdx])) {
							tempIdx--;
						}

						// Now tempIdx points to the code/blank line BEFORE the comments
						if (tempIdx >= 0) {
							const preCommentLine = result[tempIdx];
							const preCommentTrimmed = preCommentLine.trim();

							if (!preCommentTrimmed) {
								shouldInsert = false; // Already has blank line
							} else {
								// Check if the line BEFORE the comments is a start/middle
								if (blockStartPattern.test(preCommentLine) ||
									blockMiddlePattern.test(preCommentLine) ||
									casePattern.test(preCommentLine)) {
									shouldInsert = false;
								}
								insertIndex = tempIdx + 1; // Insert after that code line
							}
						} else {
							// Top of file
							shouldInsert = false;
						}

						if (shouldInsert) {
							result.splice(insertIndex, 0, '');
						}
					}
				}
			}

			// Add the current line
			result.push(line);

			// Check if we need to insert a blank line AFTER this line
			// Condition: This line is a Block End
			if (blockEndPattern.test(line)) {
				// We don't know the next line yet.
				// But we can check in the next iteration? 
				// Or easier: Just enforce that we will ensure separation when we hit the next line...
				// BUT: The "Next Line" logic (Start Block) checks Previous.
				// So if we have:
				// :ENDIF
				// stmt;
				// When we process 'stmt', we are not a block start. So 'stmt' logic needs to handle "If Prev was Block End".
			}
		}

		// Second Pass (or integrated): Ensure blank lines AFTER Block Ends
		// We can do this by post-processing 'result' or refining the loop.
		// Let's refine the loop to handle "non-block code following block end".

		const finalResult: string[] = [];
		for (let i = 0; i < result.length; i++) {
			const line = result[i];
			const trimmed = line.trim();

			if (i > 0) {
				const prevLine = result[i - 1];
				const prevTrimmed = prevLine.trim();

				// specific check: If Prev was Block END and Current is Code (and not Middle/End/Blank)
				if (prevTrimmed && blockEndPattern.test(prevLine)) {
					// Don't insert if current is:
					// 1. Block End (Cascading ends)
					// 2. Block Middle (ELSE/CATCH - though usually ENDIF followed by ELSE is invalid, except ENDCASE followed by... no)
					// 3. Blank (handled by !trimmed check above? No line is trimmed here, prevTrimmed is blank check)

					if (trimmed) {
						if (!blockEndPattern.test(line) &&
							!blockMiddlePattern.test(line) &&
							!casePattern.test(line)) {

							finalResult.push('');
						}
					}
				}

				// Enforce newline after :EXITCASE if followed by :CASE or :OTHERWISE
				if (prevTrimmed && exitCasePattern.test(prevLine)) {
					if (trimmed && casePattern.test(line)) {
						finalResult.push('');
					}
				}

				// Enforce blank lines between PARAMETERS, DEFAULT, and DECLARE
				const isPrevParams = prevTrimmed && parametersPattern.test(prevLine);
				const isPrevDefault = prevTrimmed && defaultPattern.test(prevLine);
				// const isPrevDeclare = prevTrimmed && declarePattern.test(prevLine); // Not needed for the current rules relative to *following* lines?
				// Rules:
				// PARAMETERS -> DEFAULT
				// PARAMETERS -> DECLARE
				// DEFAULT -> DECLARE

				if (isPrevParams) {
					if (trimmed && (defaultPattern.test(line) || declarePattern.test(line))) {
						finalResult.push('');
					}
				}

				if (isPrevDefault) {
					if (trimmed && declarePattern.test(line)) {
						finalResult.push('');
					}
				}
			}
			finalResult.push(line);
		}

		return finalResult.join('\n');
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
		const endCaseKeyword = /^\s*:ENDCASE\b/i;

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

				// Extra decrement for ENDCASE to align with BEGINCASE (since CASE indented 1 level deeper)
				if (endCaseKeyword.test(trimmed)) {
					indentLevel = Math.max(0, indentLevel - 1);
				}

				// Exit procedure when we hit ENDPROC and reset indent to 0
				if (endProcKeyword.test(trimmed)) {
					inProcedure = false;
				}
			}

			// Decrease indent for :CASE and :OTHERWISE (they're at same level as :BEGINCASE)
			if (!isCommentLine && caseKeyword.test(trimmed)) {
				// Only decrease indent if this is NOT the first case in the block
				// (First case should stay at reduced level of BEGINCASE block? No, stay at current level)

				// Check if previous code line was BEGINCASE
				let isFirstCase = false;
				let j = index - 1;
				while (j >= 0) {
					const prev = lines[j].trim();
					if (prev && !prev.startsWith('/*') && !prev.startsWith('*')) {
						if (/^\s*:BEGINCASE\b/i.test(prev)) {
							isFirstCase = true;
						}
						break;
					}
					j--;
				}

				if (!isFirstCase) {
					indentLevel = Math.max(0, indentLevel - 1);
				}
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



	private formatSqlLiterals(
		text: string,
		config: vscode.WorkspaceConfiguration,
		tabSize: number = 4
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
		// We only support double quotes for now as per certain SSL conventions or legacy regex behavior
		const sqlLiteralRegex = new RegExp(`\\b(${functionPattern})\\s*\\(\\s*("([\\s\\S]*?)")`, "gi");

		// Build a map of comment regions to exclude matches
		const commentRanges: { start: number; end: number }[] = [];
		let inCmt = false;
		let inStr = false;
		let strChar = '';
		let cmtStart = -1;

		for (let i = 0; i < text.length; i++) {
			const char = text[i];
			const next = i + 1 < text.length ? text[i + 1] : '';

			if (inCmt) {
				// Inside a comment, only look for semicolon to end it
				// Don't track strings inside comments
				if (char === ';') {
					inCmt = false;
					commentRanges.push({ start: cmtStart, end: i });
				}
				continue;
			}

			if (inStr) {
				if (char === strChar) {
					inStr = false;
				}
				continue;
			}

			// Check content - only process strings and comments when NOT in either
			if (char === '"' || char === '\'') {
				inStr = true;
				strChar = char;
			} else if (char === '/' && next === '*') {
				inCmt = true;
				cmtStart = i;
				i++; // Skip *
			}
		}

		// Create a version of text with comments masked (replaced with spaces)
		// This prevents unclosed strings in comments from affecting the regex matching
		let maskedText = text;
		for (let i = commentRanges.length - 1; i >= 0; i--) {
			const r = commentRanges[i];
			// Replace comment content with spaces (preserving length for position alignment)
			const replacement = ' '.repeat(r.end - r.start + 1);
			maskedText = maskedText.substring(0, r.start) + replacement + maskedText.substring(r.end + 1);
		}

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

		// Run regex on masked text to find match positions, but extract content from original text
		while ((match = sqlLiteralRegex.exec(maskedText)) !== null) {
			const idx = match.index;

			// Skip matches in comment ranges (shouldn't happen with masking, but safety check)
			const isCommented = commentRanges.some(r => idx >= r.start && idx <= r.end);
			if (isCommented) {
				continue;
			}

			// Extract the actual content from original text using match positions
			// The match object has positions relative to maskedText, but maskedText has same length as text
			const functionName = match[1];
			const matchEnd = idx + match[0].length;
			const actualMatch = text.substring(idx, matchEnd);
			// Re-extract literal and content from the actual text
			const literalMatch = actualMatch.match(/\(\s*("([\s\S]*?)")/);
			if (!literalMatch) {
				continue;
			}
			const literal = literalMatch[1];
			const content = literalMatch[2];

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
			// Calculate visual column taking tabs into account
			const preamble = lineBeforeQuote + functionName +
				text.substring(match.index + functionName.length).match(/^\s*\(\s*/)![0];
			const quoteColumn = this.getVisualLength(preamble, tabSize);

			matches.push({
				fullMatch: match[0],
				functionName,
				literal,
				content,
				index: match.index,
				quoteColumn
			});
		}

		// Use wrapLength if configured, otherwise 0
		const wrapLength = config.get<number>(CONFIG_KEYS.FORMAT_WRAP_LENGTH, CONFIG_DEFAULTS[CONFIG_KEYS.FORMAT_WRAP_LENGTH]);

		// Process matches from end to start to preserve string positions
		for (let i = matches.length - 1; i >= 0; i--) {
			const m = matches[i];

			// Apply continuation indent for multi-line formatted SQL
			const continuationIndent = ' '.repeat(m.quoteColumn + 1);

			// Adjust wrapLength to account for continuation indent
			const effectiveWrapLength = wrapLength > 0 ? Math.max(20, wrapLength - continuationIndent.length) : 0;

			// Format the SQL content
			const formattedContent = formatSqlWithStyleImpl(
				m.content,
				style || "canonicalCompact",
				keywordCase,
				normalizedIndent,
				effectiveWrapLength
			);

			// Check if the match is followed by string concatenation (+)
			const nextCharIndex = m.index + m.fullMatch.length;
			const isConcatenated = /^\s*\+/.test(text.substring(nextCharIndex, nextCharIndex + 20));
			// If concatenated, force the closing quote to a new line to avoid long lines
			const finalContent = isConcatenated ? formattedContent + "\n" : formattedContent;

			const sqlLines = finalContent.split('\n');

			let formattedSql: string;
			if (sqlLines.length === 1) {
				formattedSql = finalContent;
			} else {
				// First line stays as-is, continuation lines get indented
				// Also check if any line is too long and needs further wrapping
				const processedLines: string[] = [sqlLines[0]];

				for (let j = 1; j < sqlLines.length; j++) {
					const sqlLine = sqlLines[j];
					const fullLine = continuationIndent + sqlLine;

					if (fullLine.length > wrapLength) {
						// Try to wrap this line at a comma or before AND/OR
						const lineIndent = sqlLine.match(/^(\s*)/)?.[1] || '';
						const lineContent = sqlLine.trim();

						// Check if it's a SELECT list (comma-separated)
						if (lineContent.toUpperCase().startsWith('SELECT ')) {
							const selectPart = lineContent.match(/^(SELECT\s+)(.*)/i);
							if (selectPart) {
								const columns = this.splitAtTopLevelCommas(selectPart[2]);
								if (columns.length > 1) {
									// Split SELECT columns across lines
									processedLines.push(lineIndent + selectPart[1] + columns[0].trim() + ',');
									const colIndent = lineIndent + ' '.repeat(selectPart[1].length);
									for (let k = 1; k < columns.length; k++) {
										const isLast = k === columns.length - 1;
										processedLines.push(colIndent + columns[k].trim() + (isLast ? '' : ','));
									}
									continue;
								}
							}
						}

						// Check if it's a line with CASE/WHEN/THEN/ELSE that can be split
						if (/\bWHEN\s+\d/.test(lineContent) && /\bTHEN\b/.test(lineContent)) {
							// Line has "WHEN X THEN Y ELSE Z END" pattern - split at THEN and ELSE
							const caseMatch = lineContent.match(/^(.*?\bWHEN\s+\d+\s+THEN)\s+(.+?)\s+(ELSE\s+.+)$/i);
							if (caseMatch) {
								// Split into: WHEN X THEN, value, ELSE Z
								processedLines.push(lineIndent + caseMatch[1].trim());
								processedLines.push(lineIndent + '    ' + caseMatch[2].trim());
								processedLines.push(lineIndent + '    ' + caseMatch[3].trim());
								continue;
							}
							// Fallback: just split prefix and WHEN..THEN
							const simpleCaseMatch = lineContent.match(/^(.*?)\s*(\bWHEN\s+\d+\s+THEN\b.*)$/i);
							if (simpleCaseMatch && simpleCaseMatch[1].trim()) {
								processedLines.push(lineIndent + simpleCaseMatch[1].trim());
								processedLines.push(lineIndent + '    ' + simpleCaseMatch[2].trim());
								continue;
							}
						}

						// Check for long line with = (CASE pattern - break before = (CASE
						if (/=\s*\(CASE\s*\(/i.test(lineContent)) {
							const eqCaseMatch = lineContent.match(/^(.+?)\s*(=\s*\(CASE\s*\(.*)$/i);
							if (eqCaseMatch) {
								processedLines.push(lineIndent + eqCaseMatch[1].trim());
								processedLines.push(lineIndent + '    ' + eqCaseMatch[2].trim());
								continue;
							}
						}

						// For other long lines, just add as-is (SQL formatter should have handled it)
						processedLines.push(sqlLine);
					} else {
						processedLines.push(sqlLine);
					}
				}

				formattedSql = processedLines[0] + '\n' +
					processedLines.slice(1).map(l => continuationIndent + l).join('\n');
			}

			const formattedLiteral = `"${formattedSql}"`;

			// Replace the literal in the result
			// Find the position of this literal in the current result
			const literalStart = result.indexOf(m.literal, m.index);
			if (literalStart !== -1) {
				const newLiteral = formattedLiteral;
				result = result.substring(0, literalStart) +
					newLiteral +
					result.substring(literalStart + m.literal.length);

				// If we detected concatenation, fix the indentation of subsequent lines
				// to match the continuation indent (replacing tabs or wrong spaces)
				if (isConcatenated) {
					// Scan forward from the end of the new literal
					let currentPos = literalStart + newLiteral.length;

					// Limit scan to avoid going too far (e.g. 10 lines or until ;)
					let lineCount = 0;
					const maxLines = 10;

					while (lineCount < maxLines && currentPos < result.length) {
						const newlinePos = result.indexOf('\n', currentPos);
						if (newlinePos === -1) break;

						const lineStart = newlinePos + 1;
						const nextNewline = result.indexOf('\n', lineStart);
						const lineEnd = nextNewline !== -1 ? nextNewline : result.length;
						const line = result.substring(lineStart, lineEnd);

						// Stop if we look like we've reached a new statement or end of command
						// End of command: line contains `;`
						// We process this line then stop? 
						// Usually ); is on the last line of args.

						// Check if line is empty or just whitespace
						if (line.trim().length === 0) {
							currentPos = lineStart; // Move past empty line? No, move to next
							if (nextNewline === -1) break;
							continue;
						}

						// Fix indentation: Replace leading whitespace with continuationIndent
						const trimmed = line.trimStart();

						// Also perform basic cleanup on the content of the concatenated line
						// 1. Replace tabs with space
						let cleanedContent = trimmed.replace(/\t/g, ' ');
						// 2. Normalize spaces
						cleanedContent = cleanedContent.replace(/\s+/g, ' ');
						// 3. Uppercase common keywords if they appear (simple replace)
						cleanedContent = cleanedContent.replace(/\bwhere\b/gi, 'WHERE');
						cleanedContent = cleanedContent.replace(/\band\b/gi, 'AND');
						cleanedContent = cleanedContent.replace(/\bor\b/gi, 'OR');

						// 4. Force break on AND/OR - with extra indentation (4 spaces)
						cleanedContent = cleanedContent.replace(/\s+(AND|OR)\b/g, '\n' + continuationIndent + '    ' + '$1');

						// Check if the FIRST word is AND/OR and needs indent
						let linePrefix = continuationIndent;
						if (/^(AND|OR)\b/.test(cleanedContent)) {
							linePrefix += '    ';
						}

						const newLine = linePrefix + cleanedContent;

						// Apply replacement
						result = result.substring(0, lineStart) + newLine + result.substring(lineEnd);

						// Update position for next iteration
						currentPos = lineStart + newLine.length;
						lineCount++;

						// Check stop condition after processing
						if (trimmed.includes(';')) break;
					}
				}
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

	private getVisualLength(text: string, tabSize: number): number {
		let length = 0;
		for (let i = 0; i < text.length; i++) {
			if (text[i] === '\t') {
				length += tabSize - (length % tabSize);
			} else {
				length++;
			}
		}
		return length;
	}

	private isArrayAccess(line: string, index: number): boolean {
		if (index === 0) { return false; }
		let prevIndex = index - 1;
		while (prevIndex >= 0 && /\s/.test(line[prevIndex])) {
			prevIndex--;
		}
		if (prevIndex < 0) { return false; }
		const prevChar = line[prevIndex];
		return /[a-zA-Z0-9_\)\]]/.test(prevChar);
	}

	private getMultiLineStringState(line: string, inMultiLineString: boolean, stringDelimiter: string): { inMultiLineString: boolean; stringDelimiter: string } {
		let i = 0;
		let currentState = inMultiLineString;
		let currentDelim = stringDelimiter;
		let inInlineComment = false;

		while (i < line.length) {
			const char = line[i];
			const nextChar = i + 1 < line.length ? line[i + 1] : '';

			if (inInlineComment) {
				if (char === ';') { inInlineComment = false; }
				i++;
				continue;
			}

			// Check for comment start (if not in string)
			if (!currentState && char === '/' && nextChar === '*') {
				inInlineComment = true;
				i += 2;
				continue;
			}

			if (!currentState) {
				if (char === '"' || char === "'") {
					currentState = true;
					currentDelim = char;
				} else if (char === '[') {
					if (!this.isArrayAccess(line, i)) {
						currentState = true;
						currentDelim = ']';
					}
				}
			} else {
				if (char === currentDelim) {
					currentState = false;
					currentDelim = '';
				}
			}
			i++;
		}
		return { inMultiLineString: currentState, stringDelimiter: currentDelim };
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
				} else if (char === '[') {
					// Bracket is only a string start if NOT array access
					if (this.isArrayAccess(line, i)) {
						result += char;
					} else {
						inString = true;
						stringChar = ']';
					}
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
	 * Wrap a comma-separated list (e.g. :PARAMETERS, :DECLARE)
	 */
	/**
		 * Wrap a comma-separated list (e.g. :PARAMETERS, :DECLARE)
		 * Uses balanced wrapping to distribute items evenly
		 */
	private wrapList(keyword: string, content: string, wrapLength: number, indent: string = ''): string[] | null {
		// Handle trailing semicolon for splitting, then add it back to last item
		const trimmed = content.trim();
		const hasSemicolon = trimmed.endsWith(';');
		const contentToSplit = hasSemicolon ? trimmed.slice(0, -1) : trimmed;

		const items = this.splitAtTopLevelCommas(contentToSplit);
		if (items.length <= 1) { return null; }

		const firstLinePrefix = `${keyword} `;
		const continuationIndent = ' '.repeat(firstLinePrefix.length); // Visual alignment

		// Calculate visual width of indent
		const indentVisualWidth = this.getVisualLength(indent, 4);

		// Effective wrap length after accounting for indent
		const effectiveWrapLength = wrapLength - indentVisualWidth;

		// Simple approach: greedily fill each line, preferring minimum number of lines
		const wrapped: string[] = [];
		let currentLine = firstLinePrefix;

		for (let i = 0; i < items.length; i++) {
			const item = items[i];
			const isLast = i === items.length - 1;
			const separator = isLast ? (hasSemicolon ? ';' : '') : ', ';
			const itemWithSep = item + separator;

			// Check if adding this item would exceed the limit
			const lineWithNextItem = currentLine + itemWithSep;

			if (lineWithNextItem.length > effectiveWrapLength && currentLine !== firstLinePrefix) {
				// Wrap: start a new line
				wrapped.push(currentLine.trimEnd().replace(/, $/, ','));
				currentLine = continuationIndent + itemWithSep;
			} else {
				currentLine = lineWithNextItem;
			}
		}

		// Add the last line
		wrapped.push(currentLine);

		return wrapped.length > 1 ? wrapped : null;
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

			// Note: Multi-line string tracking was removed as it incorrectly skipped valid lines
			// when SQL string literals start on earlier lines. The pattern-based wrapping 
			// handles strings properly through regex matching.

			// Skip lines that don't need wrapping
			// Calculate visual length (tabs expand to tabSize spaces)
			const visualLength = line.split('').reduce((len, char) => {
				if (char === '\t') {
					return len + tabSize - (len % tabSize); // Tab stops at next multiple of tabSize
				}
				return len + 1;
			}, 0);

			if (visualLength <= wrapLength) {
				result.push(line);
				continue;
			}


			// Skip single-line comments and empty lines
			// Only skip if line STARTS with /* (block comment), not lines with trailing inline comments
			if (!trimmed || trimmed.startsWith('/*')) {
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

			// Pattern 5: :PARAMETERS or :DECLARE list
			// Keywords are already normalized to UPPERCASE
			const listMatch = trimmed.match(/^:(PARAMETERS|DECLARE)\b\s+(.+)$/);
			if (!wrapped && listMatch) {
				const keyword = ':' + listMatch[1]; // Already upper
				const content = listMatch[2];
				wrapped = this.wrapList(keyword, content, wrapLength, originalIndent);
			}

			// Pattern 6: Array literal continuation line (starts with { and ends with }); or })
			// These are often function call argument continuations
			if (!wrapped) {
				const arrayMatch = trimmed.match(/^\{(.+)\}(\);?)$/);
				if (arrayMatch) {
					const content = arrayMatch[1];
					const suffix = arrayMatch[2]; // ); or )
					wrapped = this.wrapArrayLiteral(content, wrapLength, originalIndent, suffix);
				}
			}

			// Pattern 7: :RETURN statement with array containing long string
			// e.g., :RETURN {-1, "Very long error message..."};
			// Also handles trailing inline comments: :RETURN {-1, "msg"}; /*comment
			if (!wrapped) {
				const returnMatch = trimmed.match(/^(:RETURN\s*)\{(.+)\}(;.*)?$/i);
				if (returnMatch) {
					const keyword = returnMatch[1];
					const arrayContent = returnMatch[2];
					const semicolon = returnMatch[3] || '';
					wrapped = this.wrapReturnArray(keyword, arrayContent, wrapLength, originalIndent, semicolon);
				}
			}

			// Pattern 8: Line ending with string + function call (like IIF)
			// e.g., ...?sQCType?" + IIF(METHODDEVELOPER == "Y", "", " and ss.STATUS = ?Released? ") );
			if (!wrapped) {
				// Match lines that have: ..." + FunctionCall(...) );
				const concatMatch = trimmed.match(/^(.+["\'])\s*\+\s*(\w+\s*\(.+\)\s*\)?;?)$/);
				if (concatMatch) {
					const beforePlus = concatMatch[1];
					const afterPlus = concatMatch[2];
					// Push directly to result with absolute positioning:
					// Line 1: originalIndent + beforePlus
					// Line 2: 27 spaces (column 28) + "+ " + afterPlus
					result.push(originalIndent + beforePlus);

					const col28Indent = ' '.repeat(27);
					const plusLine = col28Indent + '+ ' + afterPlus;

					// If the + line is too long, wrap the function arguments
					if (plusLine.length > wrapLength) {
						// Try to wrap IIF/function arguments
						const funcMatch = afterPlus.match(/^(\w+)\s*\((.+)\)\s*(\)?;?)$/);
						if (funcMatch) {
							const funcName = funcMatch[1];
							const args = funcMatch[2];
							const suffix = funcMatch[3];
							// Split arguments at top-level commas
							const argList = this.splitAtTopLevelCommas(args);
							if (argList.length > 1) {
								// First line: + FuncName(arg1,
								result.push(col28Indent + '+ ' + funcName + '(' + argList[0].trim() + ',');
								// Middle args with hanging indent at column 34
								const argIndent = ' '.repeat(33);
								for (let i = 1; i < argList.length - 1; i++) {
									result.push(argIndent + argList[i].trim() + ',');
								}
								// Last arg with closing
								result.push(argIndent + argList[argList.length - 1].trim() + ')' + suffix);
								continue;
							}
						}
					}
					result.push(plusLine);
					continue; // Skip the generic wrapped handling
				}
			}

			// Pattern 9: SQL string ending with trailing function arguments
			// e.g., ...= ?", "", "DATABASE", {sNewOrdNo, nTESTCODE, lotNo });
			if (!wrapped) {
				// Match lines that end with SQL parameter (like ?") then additional args
				// Look for pattern: ...?" (or similar), then comma-separated args ending in );
				// Use non-greedy match to find first quote that's followed by ", then args
				const sqlArgsMatch = trimmed.match(/^(.*\?\s*["\'])\s*,\s*(.+\)\s*;?)$/);
				if (sqlArgsMatch) {
					const sqlPart = sqlArgsMatch[1];
					const argsPart = sqlArgsMatch[2];
					const fullLine = originalIndent + trimmed;

					// Only wrap if line is too long
					if (this.getVisualLength(fullLine, 4) > wrapLength) {
						// Split arguments
						const args = this.splitAtTopLevelCommas(argsPart.replace(/\);?$/, ''));
						const suffix = argsPart.match(/(\)\s*;?)$/)?.[1] || ');';

						if (args.length >= 1) {
							// Line 1: SQL part with comma
							result.push(originalIndent + sqlPart + ',');
							// Remaining args at column 34
							const col34Indent = ' '.repeat(33);
							for (let i = 0; i < args.length; i++) {
								const isLast = i === args.length - 1;
								const argLine = col34Indent + args[i].trim() + (isLast ? suffix : ',');
								result.push(argLine);
							}
							continue;
						}
					}
				}
			}

			// Pattern 10: SQL FROM clause with comma-separated tables
			// e.g., FROM results r, spec_analytes sa, specschemas ss, specschemacalcs sc
			if (!wrapped) {
				const fromMatch = trimmed.match(/^(FROM\s+)(.+,\s*.+)$/i);
				if (fromMatch) {
					const keyword = fromMatch[1];
					const tableList = fromMatch[2];
					const fullLine = originalIndent + trimmed;

					// Only wrap if line is too long
					if (this.getVisualLength(fullLine, 4) > wrapLength) {
						const tables = this.splitAtTopLevelCommas(tableList);
						if (tables.length > 1) {
							// Calculate effective wrap length
							const indentVisualWidth = this.getVisualLength(originalIndent, 4);
							const effectiveWrapLength = wrapLength - indentVisualWidth;

							// Greedy fill: put as many tables as fit on each line
							const continuationIndent = ' '.repeat(keyword.length);
							const result_lines: string[] = [];
							let currentLine = keyword + tables[0].trim();

							for (let i = 1; i < tables.length; i++) {
								const table = tables[i].trim();
								const lineWithNext = currentLine + ', ' + table;

								if (lineWithNext.length > effectiveWrapLength) {
									result_lines.push(currentLine + ',');
									currentLine = continuationIndent + table;
								} else {
									currentLine = lineWithNext;
								}
							}
							result_lines.push(currentLine);

							wrapped = result_lines;
						}
					}
				}
			}

			if (wrapped && wrapped.length > 0) {
				// Apply original indentation and check for secondary wrapping
				for (const wLine of wrapped) {
					const fullLine = originalIndent + wLine;
					// If a wrapped line is STILL too long, try to wrap it further
					if (fullLine.length > wrapLength) {
						const trimmedW = wLine.trim();
						const wIndent = wLine.match(/^(\s*)/)?.[1] || '';
						const fullIndent = originalIndent + wIndent;

						// Try Pattern 6 for array literals
						const arrayMatchW = trimmedW.match(/^\{(.+)\}(\);?)$/);
						if (arrayMatchW) {
							const contentW = arrayMatchW[1];
							const suffixW = arrayMatchW[2];
							const secondaryWrapped = this.wrapArrayLiteral(contentW, wrapLength, fullIndent, suffixW);
							if (secondaryWrapped && secondaryWrapped.length > 1) {
								// wrapArrayLiteral returns: line 0 without indent (just {items}), lines 1+ with continuationIndent
								// We need to add fullIndent only to line 0
								result.push(fullIndent + secondaryWrapped[0]);
								for (let j = 1; j < secondaryWrapped.length; j++) {
									result.push(secondaryWrapped[j]); // Already has continuationIndent
								}
								continue;
							}
						}
					}
					result.push(fullLine);
				}
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
		// Continuation lines align with the start of the first argument
		const continuationIndent = ' '.repeat(prefix.length);

		// Try to split arguments at commas
		const argList = this.splitAtTopLevelCommas(args);
		if (argList.length <= 1) {
			// Check if single argument is a long concatenated string
			// We look for '+' and quotes
			if (argList.length === 1) {
				const arg = argList[0].trim();
				if (/\+/.test(arg) && (arg.includes('"') || arg.includes("'"))) {
					// Use wrapConcatenatedString but with custom continuation indent
					// The indent should align with the function call prefix
					// We need to account that wrapConcatenatedString returns lines where first line
					// starts with the content. We need to prepend prefix to the first line.
					const wrappedConcat = this.wrapConcatenatedString(
						arg,
						Math.max(20, wrapLength - prefix.length), // Adjust wrap length for prefix
						indent,
						continuationIndent // Use exact alignment indent
					);

					if (wrappedConcat && wrappedConcat.length > 0) {
						const result: string[] = [];
						result.push(prefix + wrappedConcat[0]);
						for (let k = 1; k < wrappedConcat.length; k++) {
							// wrappedConcat lines 1+ already have continuationIndent
							// However, wrapConcatenatedString adds continuationIndent relative to start of line
							// Our continuationIndent is relative to start of prefix.
							// Wait, wrapConcatenatedString:
							// currentLine = continuationIndent + part;
							// If we pass '           ' as continuationIndent, it uses it directly.
							// This is exactly what we want.
							result.push(wrappedConcat[k]);
						}
						// Add closing paren to last line
						const lastIndex = result.length - 1;
						result[lastIndex] = result[lastIndex] + ');';
						return result;
					}
				}
			}
			return null;
		}

		const result: string[] = [];
		let currentLine = prefix + argList[0];

		for (let i = 1; i < argList.length; i++) {
			const arg = argList[i];
			// Trailing comma style: put comma on previous line
			const lineWithNextArg = currentLine + ', ' + arg.trim();

			// Use visual length calculation
			const visualLen = this.getVisualLength(lineWithNextArg + indent, 4);
			if (visualLen > wrapLength) {
				// Wrap
				result.push(currentLine + ',');
				currentLine = continuationIndent + arg.trim();
			} else {
				currentLine = lineWithNextArg;
			}
		}

		// Check if the last line (currentLine) is still too long due to array literal
		const lastLineVisualLen = this.getVisualLength(indent + currentLine + ');', 4);
		if (lastLineVisualLen > wrapLength) {
			// Check if it's an array literal that can be further wrapped
			const arrayMatch = currentLine.match(/^(\s*)(\{.+\})$/);
			if (arrayMatch) {
				const arrayIndent = arrayMatch[1];
				const arrayContent = arrayMatch[2];
				// Extract array content (without { and })
				const contentMatch = arrayContent.match(/^\{(.+)\}$/);
				if (contentMatch) {
					const items = this.splitAtTopLevelCommas(contentMatch[1]);
					if (items.length > 1) {
						// Wrap the array into balanced lines
						// Calculate how many items per line to stay under wrapLength
						const availableWidth = wrapLength - this.getVisualLength(indent + arrayIndent, 4) - 5; // 5 for braces and comma

						// Try to balance: split roughly in half
						const midpoint = Math.ceil(items.length / 2);
						const line1Items = items.slice(0, midpoint);
						const line2Items = items.slice(midpoint);

						result.push(arrayIndent + '{' + line1Items.join(', ') + ',');
						result.push(arrayIndent + ' ' + line2Items.join(', ') + '});');
						return result;
					}
				}
			}
		}

		result.push(currentLine + ');');

		return result;
	}

	/**
	 * Wrap a concatenated string at + operators
	 * NOTE: Does NOT add indentation - caller must prepend originalIndent to each returned line
	 */
	private wrapConcatenatedString(content: string, wrapLength: number, indent: string, customContinuationIndent?: string): string[] | null {
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

		// Standard continuation indent is 4 spaces relative to formatted indent
		const continuationIndent = customContinuationIndent !== undefined ? customContinuationIndent : '    ';

		// Pre-process parts to split any individual string literals that are too long
		const effectiveWrapLength = wrapLength - (indent.length + continuationIndent.length + 3); // 3 for " + " approximation
		const expandedParts: string[] = [];

		for (const part of parts) {
			const trimmedPart = part.trim();
			// Check if it's a string literal
			if (trimmedPart.length > effectiveWrapLength &&
				((trimmedPart.startsWith('"') && trimmedPart.endsWith('"')) ||
					(trimmedPart.startsWith("'") && trimmedPart.endsWith("'")))) {

				// Use splitLongString to break it down
				// splitLongString expects the full quoted string and the max length for the CONTENT
				const splitParts = this.splitLongString(trimmedPart, effectiveWrapLength - 2);
				expandedParts.push(...splitParts);
				continue;
			}
			expandedParts.push(part);
		}

		// Use the expanded list
		const partsToUse = expandedParts;

		// Build wrapped lines
		const result: string[] = [];
		// Start with first part. Caller adds originalIndent.
		let currentLine = partsToUse[0];

		for (let i = 1; i < partsToUse.length; i++) {
			const part = partsToUse[i];
			// Calculate length including the indent the caller will add
			const testLine = currentLine + ' + ' + part;

			if (testLine.length + indent.length > wrapLength) {
				result.push(currentLine);
				currentLine = continuationIndent + '+ ' + part;
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
	 * Wrap an array literal continuation line
	 * Handles lines like {item1, item2, item3, ...});
	 */
	private wrapArrayLiteral(content: string, wrapLength: number, indent: string, suffix: string): string[] | null {
		const items = this.splitAtTopLevelCommas(content);
		if (items.length <= 1) {
			return null;
		}

		// Calculate visual width of indent (tabs expand to 4 spaces typically)
		const tabSize = 4;
		const indentVisualWidth = this.getVisualLength(indent, tabSize);

		// The continuation indent should align items with the content after the opening {
		// indent takes us to {, plus 1 space takes us to the content after {
		const continuationIndent = indent + ' ';

		const result: string[] = [];
		let currentLine = '{' + items[0];
		let isFirstOutputLine = true;

		for (let i = 1; i < items.length; i++) {
			const item = items[i];
			const isLast = i === items.length - 1;
			const lineWithNextItem = currentLine + ', ' + item;

			// Account for the visual indent and potential suffix on last item
			const effectiveLen = indentVisualWidth + lineWithNextItem.length + (isLast ? suffix.length + 1 : 0);

			if (effectiveLen > wrapLength) {
				// Wrap: put comma at end of current line
				result.push(currentLine + ',');
				// Continuation lines get the extra indent to align with first item
				currentLine = (isFirstOutputLine ? '' : '') + item;
				isFirstOutputLine = false;
			} else {
				currentLine = lineWithNextItem;
			}
		}

		// Add suffix (}); or }) to the last line
		result.push(currentLine + '}' + suffix);

		// Apply continuation indent to all lines except the first (which starts with {)
		if (result.length > 1) {
			for (let i = 1; i < result.length; i++) {
				result[i] = continuationIndent + result[i];
			}
		}

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

		// Calculate alignment indent for length checks
		const alignmentIndentLen = varName.length + 4; // " := "
		// Effective length for parts led by "+ "
		const effectiveWrapLength = wrapLength - (alignmentIndentLen + 2);

		// Split any parts that are long string literals
		const expandedParts: string[] = [];
		for (const part of parts) {
			const trimmedPart = part.trim();
			// Check if it's a string literal and is too long
			if (trimmedPart.length > effectiveWrapLength &&
				((trimmedPart.startsWith('"') && trimmedPart.endsWith('"')) ||
					(trimmedPart.startsWith("'") && trimmedPart.endsWith("'")))) {

				// splitLongString expects the full quoted string and the max length for the CONTENT
				// We subtract 2 from effectiveWrapLength to account for output quotes in the split parts
				const splitParts = this.splitLongString(trimmedPart, effectiveWrapLength - 2);
				expandedParts.push(...splitParts);
			} else {
				expandedParts.push(part);
			}
		}
		parts = expandedParts;

		// If still just one part or no parts, can't wrap (unless it was already short enough?)
		// Actually, if we have 1 part and it fits, we return null? 
		// wrapLongLines only calls us if line > wrapLength.
		// If we return null, wrapLongLines keeps original.
		// If we split successfully, we return wrapped.
		// If parts.length == 1 and it fits, we might want to return formatted single line?
		// But existing logic returns null.

		if (parts.length <= 1) {
			// Check if the single part is still too long? 
			// If it is, and we couldn't split it (e.g. function call?), then return null.
			return null;
		}

		// Build wrapped lines without indentation (caller will apply original line's indentation)
		const wrapped: string[] = [];

		// First line: varName := firstPart
		wrapped.push(`${varName} := ${parts[0]}`);

		// Calculate alignment indent: varName + " := "
		// " := " is 4 chars.
		const alignmentIndent = ' '.repeat(varName.length + 4);

		// Middle lines: + part
		for (let i = 1; i < parts.length - 1; i++) {
			wrapped.push(`${alignmentIndent}+ ${parts[i]}`);
		}

		// Last line: + lastPart;
		wrapped.push(`${alignmentIndent}+ ${parts[parts.length - 1]};`);

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

			// Keep the trailing space in the first part so concatenation preserves the word boundary
			const part = remaining.substring(0, breakPoint + 1); // Include the space
			parts.push(`${quote}${part}${quote}`);
			remaining = remaining.substring(breakPoint + 1); // Skip the space we included
		}

		return parts;
	}

	/**
	 * Wrap a :RETURN statement containing an array with long string elements
	 * e.g., :RETURN {-1, "Very long error message..."};
	 */
	private wrapReturnArray(
		keyword: string,
		arrayContent: string,
		wrapLength: number,
		indent: string,
		semicolon: string
	): string[] | null {
		// Parse array items (comma-separated, respecting strings)
		const items = this.splitAtTopLevelCommas(arrayContent);
		if (items.length < 1) {
			return null;
		}

		// Calculate effective wrap length for string content
		// String content should start at column 14 (0-indexed column 13)
		// Since caller adds originalIndent, we need to calculate spaces to reach column 14
		// Tab counts as 4 columns, spaces count as 1
		const indentVisualWidth = indent.split('').reduce((len, char) => {
			if (char === '\t') {
				return len + 4 - (len % 4);
			}
			return len + 1;
		}, 0);
		const targetColumn = 13; // 0-indexed column 13 = visual column 14
		const continuationSpaces = Math.max(0, targetColumn - indentVisualWidth);
		const continuationIndent = ' '.repeat(continuationSpaces);
		const effectiveWrapLength = wrapLength - (targetColumn + 6);

		// Expand any long string items
		const expandedItems: string[] = [];
		for (const item of items) {
			const trimmedItem = item.trim();
			// Check if it's a long string literal
			if (trimmedItem.length > effectiveWrapLength &&
				((trimmedItem.startsWith('"') && trimmedItem.endsWith('"')) ||
					(trimmedItem.startsWith("'") && trimmedItem.endsWith("'")))) {
				// Split the long string
				const splitParts = this.splitLongString(trimmedItem, effectiveWrapLength - 2);
				if (splitParts.length > 1) {
					// Join with newline + plus on following line
					// No extra spaces - continuation indent is applied when processing the lines
					expandedItems.push(splitParts.join('\n+ '));
				} else {
					expandedItems.push(trimmedItem);
				}
			} else {
				expandedItems.push(trimmedItem);
			}
		}

		// Reconstruct the :RETURN statement
		// If any item was expanded (contains newlines), we need multi-line format
		const hasMultiLine = expandedItems.some(item => item.includes('\n'));

		if (hasMultiLine) {
			const result: string[] = [];

			if (expandedItems.length === 1) {
				// Single item :RETURN
				const lines = (keyword + '{' + expandedItems[0] + '}' + semicolon).split('\n');
				return lines;
			}

			// Multiple items - put each on its own line
			result.push(keyword + '{' + expandedItems[0] + ',');
			for (let i = 1; i < expandedItems.length - 1; i++) {
				const itemLines = expandedItems[i].split('\n');
				for (let j = 0; j < itemLines.length; j++) {
					if (j === itemLines.length - 1) {
						result.push(continuationIndent + itemLines[j] + ',');
					} else {
						result.push(continuationIndent + itemLines[j]);
					}
				}
			}
			// Last item
			const lastItem = expandedItems[expandedItems.length - 1];
			const lastLines = lastItem.split('\n');
			for (let j = 0; j < lastLines.length; j++) {
				if (j === lastLines.length - 1) {
					result.push(continuationIndent + lastLines[j] + '}' + semicolon);
				} else {
					result.push(continuationIndent + lastLines[j]);
				}
			}

			return result;
		}

		// No multi-line items, just check if the whole thing needs wrapping
		const fullLine = keyword + '{' + expandedItems.join(', ') + '}' + semicolon;
		if (fullLine.length < wrapLength - indent.length) {
			return null; // Fits on one line, no wrapping needed
		}

		// Simple multi-line format
		const result: string[] = [];
		result.push(keyword + '{' + expandedItems[0] + ',');
		for (let i = 1; i < expandedItems.length - 1; i++) {
			result.push(continuationIndent + expandedItems[i] + ',');
		}
		result.push(continuationIndent + expandedItems[expandedItems.length - 1] + '}' + semicolon);

		return result;
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

		// Calculate continuation indent to align with first argument
		// e.g., for "Ordno := ExecFunction(" the indent aligns after the opening paren
		const firstLinePrefix = `${varName} := ${prefix}${openBracket}`;
		const continuationIndent = ' '.repeat(firstLinePrefix.length);

		// First line: varName := prefix(item1, or varName := {item1,
		if (prefix) {
			wrapped.push(`${varName} := ${prefix}${openBracket}${items[0]},`);
		} else {
			wrapped.push(`${varName} := ${openBracket}${items[0]},`);
		}

		// Middle lines: indented item,
		for (let i = 1; i < items.length - 1; i++) {
			wrapped.push(`${continuationIndent}${items[i]},`);
		}

		// Last line: indented lastItem);
		wrapped.push(`${continuationIndent}${items[items.length - 1]}${closeBracket};`);

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
