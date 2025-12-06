import * as vscode from "vscode";
import { CONFIG_KEYS, CONFIG_DEFAULTS } from "../constants/config";

/**
 * SQL formatting style presets
 */
export type SqlFormattingStyle =
	| "compact"           // Single-line clauses, compact SELECT
	| "canonicalCompact"  // Canonical compact + hanging operators (preferred default)
	| "expanded"          // Multi-line SELECT with vertical columns
	| "hangingOperators"  // AND/OR at line start with indent
	| "knr"               // K&R style with parentheses blocks
	| "knrCompact"        // K&R style with compact SELECT list
	| "ormFriendly";      // Inline JOINs, hanging operators, ORM-friendly

/**
 * SQL clause keywords that should start new lines
 * Sorted by length (longest first) to handle multi-word keywords correctly
 */
const SQL_CLAUSE_KEYWORDS = [
	"INNER JOIN",
	"LEFT JOIN",
	"RIGHT JOIN",
	"FULL JOIN",
	"CROSS JOIN",
	"GROUP BY",
	"ORDER BY",
	"UNION ALL",
	"INSERT INTO",
	"DELETE FROM",
	"EXCEPT",
	"INTERSECT",
	"HAVING",
	"VALUES",
	"WHERE",
	"UNION",
	"FROM",
	"JOIN",
	"UPDATE",
	"SET",
	"ON"
].sort((a, b) => b.length - a.length);

/**
 * All SQL keywords for case normalization
 */
const SQL_GENERAL_KEYWORDS = [
	"SELECT", "DISTINCT", "TOP", "AS", "ON", "IN", "NOT", "BETWEEN", "LIKE", "IS", "NULL",
	"INNER", "LEFT", "RIGHT", "FULL", "CROSS", "JOIN", "INSERT", "INTO", "VALUES",
	"UPDATE", "SET", "DELETE", "FROM", "WHERE", "GROUP", "BY", "ORDER", "HAVING",
	"UNION", "ALL", "EXCEPT", "INTERSECT", "AND", "OR", "ASC", "DESC", "LIMIT",
	"OFFSET", "EXISTS", "CASE", "WHEN", "THEN", "ELSE", "END", "CAST", "CONVERT",
	"COALESCE", "NULLIF", "COUNT", "SUM", "AVG", "MIN", "MAX"
];

/**
 * Style display names for the picker
 */
const STYLE_DISPLAY_NAMES: Record<SqlFormattingStyle, string> = {
	"compact": "Compact - Single-line clauses",
	"canonicalCompact": "Canonical Compact - Hanging operators + JOIN breaks",
	"expanded": "Expanded - Vertical columns",
	"hangingOperators": "Hanging Operators - AND/OR at line start",
	"knr": "K&R Style - Parenthesized blocks",
	"knrCompact": "K&R Compact - Parenthesized with compact SELECT",
	"ormFriendly": "No-Newline / ORM Friendly - Inline JOINs"
};

/**
 * Register the Format SQL commands
 */
export function registerFormatSqlCommand(context: vscode.ExtensionContext): void {
	// Main command - uses default style from config
	const formatSqlCommand = vscode.commands.registerCommand("ssl.formatSql", async () => {
		const config = vscode.workspace.getConfiguration("ssl");
		const defaultStyle = config.get<SqlFormattingStyle>(
			CONFIG_KEYS.FORMAT_SQL_STYLE,
			CONFIG_DEFAULTS[CONFIG_KEYS.FORMAT_SQL_STYLE] as SqlFormattingStyle
		);
		await formatSqlWithStyle(defaultStyle);
	});

	// Command to pick style from menu
	const formatSqlPickStyleCommand = vscode.commands.registerCommand("ssl.formatSqlPickStyle", async () => {
		const styles: { label: string; style: SqlFormattingStyle; description: string }[] = [
			{ label: "$(list-flat) Compact", style: "compact", description: "Single-line clauses, compact SELECT" },
			{ label: "$(pulse) Canonical Compact", style: "canonicalCompact", description: "Preferred: hanging operators + JOIN breaks" },
			{ label: "$(list-tree) Expanded", style: "expanded", description: "Multi-line SELECT with vertical columns" },
			{ label: "$(indent) Hanging Operators", style: "hangingOperators", description: "AND/OR at line start with indent" },
			{ label: "$(bracket) K&R Style", style: "knr", description: "Parenthesized blocks, one column per line" },
			{ label: "$(bracket-dot) K&R Compact", style: "knrCompact", description: "Parenthesized blocks, compact SELECT" },
			{ label: "$(circuit-board) No-Newline / ORM Friendly", style: "ormFriendly", description: "Inline JOINs, hanging operators" }
		];

		const selected = await vscode.window.showQuickPick(styles, {
			placeHolder: "Select SQL formatting style"
		});

		if (selected) {
			await formatSqlWithStyle(selected.style);
		}
	});

	context.subscriptions.push(formatSqlCommand, formatSqlPickStyleCommand);
}

/**
 * Format SQL with a specific style
 */
async function formatSqlWithStyle(style: SqlFormattingStyle): Promise<void> {
	const editor = vscode.window.activeTextEditor;
	if (!editor) {
		vscode.window.showErrorMessage("No active editor found.");
		return;
	}

	if (editor.document.languageId !== "ssl") {
		vscode.window.showErrorMessage("Format SQL is only available for SSL files.");
		return;
	}

	const config = vscode.workspace.getConfiguration("ssl");
	const keywordCase = config.get<string>(
		CONFIG_KEYS.FORMAT_SQL_KEYWORD_CASE,
		CONFIG_DEFAULTS[CONFIG_KEYS.FORMAT_SQL_KEYWORD_CASE]
	) || "upper";
	const indentSpaces = config.get<number>(
		CONFIG_KEYS.FORMAT_SQL_INDENT_SPACES,
		CONFIG_DEFAULTS[CONFIG_KEYS.FORMAT_SQL_INDENT_SPACES]
	) || 4;

	// Get selection or current line
	let selection = editor.selection;
	let text: string;
	let targetRange: vscode.Range;

	if (selection.isEmpty) {
		// No selection - try to find SQL string on current line
		const line = editor.document.lineAt(selection.active.line);
		const sqlRange = findSqlStringInLine(editor.document, line.lineNumber);

		if (!sqlRange) {
			vscode.window.showInformationMessage("No SQL string found on the current line. Select the SQL string to format.");
			return;
		}

		targetRange = sqlRange.range;
		text = sqlRange.content;
	} else {
		// Use selection
		targetRange = selection;
		text = editor.document.getText(selection);
	}

	// Extract content from string literal if it's a string
	const stringMatch = text.match(/^(["'\[])([\s\S]*)(["'\]])$/);
	let sqlContent: string;
	let quoteChar: string;
	let isStringLiteral = false;

	if (stringMatch) {
		quoteChar = stringMatch[1];
		sqlContent = stringMatch[2];
		isStringLiteral = true;
	} else {
		// Check if it's just SQL content without quotes
		sqlContent = text;
		quoteChar = '"';
	}

	// Check if content looks like SQL
	if (!looksLikeSql(sqlContent)) {
		vscode.window.showInformationMessage("Selected text doesn't appear to be SQL. Select a SQL string to format.");
		return;
	}

	// Format the SQL with the selected style
	const formattedSql = formatSqlWithStyleImpl(sqlContent, style, keywordCase, indentSpaces);

	// Wrap in quotes if it was a string literal
	let formattedOutput: string;
	if (isStringLiteral) {
		const closeQuote = quoteChar === '[' ? ']' : quoteChar;
		formattedOutput = `${quoteChar}${formattedSql}${closeQuote}`;
	} else {
		formattedOutput = formattedSql;
	}

	// Apply the edit
	// Preserve trailing newline if it existed in the original selection
	if (text.endsWith('\n') && !formattedOutput.endsWith('\n')) {
		formattedOutput += '\n';
	} else if (text.endsWith('\r\n') && !formattedOutput.endsWith('\r\n')) {
		formattedOutput += '\r\n';
	}

	// Apply the edit
	await editor.edit(editBuilder => {
		editBuilder.replace(targetRange, formattedOutput);
	});

	vscode.window.showInformationMessage(`SQL formatted with ${STYLE_DISPLAY_NAMES[style]} style.`);
}

/**
 * Find a SQL string literal in a line
 */
function findSqlStringInLine(
	document: vscode.TextDocument,
	lineNumber: number
): { range: vscode.Range; content: string } | null {
	const line = document.lineAt(lineNumber);
	const text = line.text;

	// Look for SQL function calls with string literals
	const sqlFunctionPattern = /\b(SqlExecute|RunSQL|LSearch|LSelect|LSelect1|LSelectC|GetDataSet|GetNetDataSet)\s*\(\s*(["'])([\s\S]*?)\2/gi;
	let match: RegExpExecArray | null;

	while ((match = sqlFunctionPattern.exec(text)) !== null) {
		const stringStart = match.index + match[0].indexOf(match[2]);
		const stringEnd = stringStart + match[2].length + match[3].length + 1;

		return {
			range: new vscode.Range(
				new vscode.Position(lineNumber, stringStart),
				new vscode.Position(lineNumber, stringEnd)
			),
			content: match[2] + match[3] + match[2]
		};
	}

	// Look for any string that contains SQL keywords
	const stringPattern = /(["'])([\s\S]*?)\1/g;
	while ((match = stringPattern.exec(text)) !== null) {
		const content = match[2];
		if (looksLikeSql(content)) {
			return {
				range: new vscode.Range(
					new vscode.Position(lineNumber, match.index),
					new vscode.Position(lineNumber, match.index + match[0].length)
				),
				content: match[0]
			};
		}
	}

	return null;
}

/**
 * Check if text looks like SQL
 * @public Exported for testing
 */
export function looksLikeSql(text: string): boolean {
	const normalizedText = text.toUpperCase();
	const sqlIndicators = ["SELECT", "INSERT", "UPDATE", "DELETE", "FROM", "WHERE"];
	return sqlIndicators.some(keyword => normalizedText.includes(keyword));
}

/**
 * Format SQL with a specific style
 * @public Exported for testing
 */
export function formatSqlWithStyleImpl(
	content: string,
	style: SqlFormattingStyle,
	keywordCase: string,
	indentSpaces: number,
	wrapLength: number = 0
): string {
	// Normalize the SQL first
	let sql = content.replace(/\r\n/g, "\n").trim();
	if (!sql) {
		return content.trim();
	}

	// Collapse multiple whitespace to single space
	sql = sql.replace(/\s+/g, " ");

	// Normalize comparison operator spacing
	// Handle multi-character operators first (to avoid double-processing)
	sql = sql.replace(/\s*<>\s*/g, " <> ");
	sql = sql.replace(/\s*<=\s*/g, " <= ");
	sql = sql.replace(/\s*>=\s*/g, " >= ");
	sql = sql.replace(/\s*!=\s*/g, " != ");
	// Now handle single = (but not part of <=, >=, !=, or := which shouldn't be in SQL)
	// Negative lookbehind for <, >, !, and negative lookahead for another =
	sql = sql.replace(/(?<![<>!=])=(?!=)/g, " = ");
	// Clean up any double spaces that may have been created
	sql = sql.replace(/\s+/g, " ");

	// Format expressions inside ?...? parameter placeholders
	// These contain SSL expressions that should have proper operator spacing
	// e.g., ?MAXCUPNO-1+i? -> ?MAXCUPNO - 1 + i?
	sql = sql.replace(/\?([^?]+)\?/g, (_match, expr: string) => {
		let formatted = expr;
		// Add spaces around arithmetic operators (+, -, *, /)
		// Handle + and - carefully to not break unary operators
		formatted = formatted.replace(/([A-Za-z0-9_\]\)])\s*\+\s*([A-Za-z0-9_\[\(])/g, '$1 + $2');
		formatted = formatted.replace(/([A-Za-z0-9_\]\)])\s*-\s*([A-Za-z0-9_\[\(])/g, '$1 - $2');
		formatted = formatted.replace(/([A-Za-z0-9_\]\)])\s*\*\s*([A-Za-z0-9_\[\(])/g, '$1 * $2');
		formatted = formatted.replace(/([A-Za-z0-9_\]\)])\s*\/\s*([A-Za-z0-9_\[\(])/g, '$1 / $2');
		return `?${formatted}?`;
	});


	// Mask literals and parameters to prevent casing changes inside them
	const masks: string[] = [];
	const maskLiteral = (s: string) => {
		masks.push(s);
		return `__MASK${masks.length - 1}__`;
	};

	// Mask string literals ('...' or "...")
	sql = sql.replace(/(["'])(?:(?=(\\?))\2[\s\S])*?\1/g, maskLiteral);

	// Mask parameters (?param?)
	sql = sql.replace(/\?([^?]+)\?/g, maskLiteral);

	// Apply casing to unmasked parts
	// Helper to determine token type and apply case
	const processToken = (match: string, offset: number, fullText: string): string => {
		const upper = match.toUpperCase();

		// 1. Check if it's a keyword
		if (SQL_GENERAL_KEYWORDS.includes(upper)) {
			return applySqlKeywordCase(upper, keywordCase, match);
		}

		// 2. Check if it's a function (followed by opening paren)
		// Look ahead for ( skipping whitespace
		const remainder = fullText.substring(offset + match.length);
		if (/^\s*\(/.test(remainder)) {
			// PascalCase: First letter upper, rest lowercase (unless already mixed/camel? No, standard is Pascal)
			// Actually, let's just title case it: Nvl, User, etc.
			// But if it is like 'MyFunction', we might want to preserve? 
			// User request: "functions should be pascal" -> 'Nvl', 'To_Char' (maybe TO_CHAR is keyword?)
			// Let's assume Capitalize First Letter.
			return match.charAt(0).toUpperCase() + match.slice(1).toLowerCase();
		}

		// 3. Identifiers (tables, columns) -> lowercase
		return match.toLowerCase();
	};

	// Tokenize: match words starting with char/underscore
	// We matched SQL_GENERAL_KEYWORDS regex before, but now we scan all words.
	// We must avoid touching numeric literals or masked placeholders.
	// Regex for identifiers: [a-zA-Z_][a-zA-Z0-9_]*
	sql = sql.replace(/\b[a-zA-Z_][a-zA-Z0-9_]*\b/g, (match, offset, fullText) => {
		// Ignore if it looks like a mask
		if (match.startsWith('__MASK') && match.endsWith('__')) {
			return match;
		}
		return processToken(match, offset, fullText);
	});

	// Unmask
	masks.forEach((original, i) => {
		sql = sql.replace(`__MASK${i}__`, original);
	});

	// Re-apply parameter formatting (spaces inside ?...?)
	// Since we masked params entirely above, the previous step that formatted expressions inside params 
	// (lines 292-304) is now effectively skipped/useless unless we did it BEFORE masking.
	// Wait, I masked at START of this block (line 292+ in original was formatting params).
	// I should do parameter formatting inside the Unmasking step or before masking?
	// The original code did: Normalize -> Operator Spacing -> Param Formatter -> Keyword Casing.
	// If I mask params *first*, I skip operator spacing inside them (if it was done via global regexes previously?).
	// The lines 292-304 in original were: `sql = sql.replace(/\?([^?]+)\?/g, ...)`
	// If I replace lines 308-309 (apply keyword casing), I am AFTER param formatting.
	// So params are already formatted "internally" (spaces added).
	// So my masking `?([^?]+)?` will capture the *formatted* param. That is fine. 
	// But `matchLiteral` takes the whole string.
	// So when I simple replace, I preserve the internal formatting.
	// CAUTION: The masking regex `\?([^?]+)\?` might fail if the param formatting added spaces?
	// `? MAXCUPNO - 1 + i ?` matches. Yes. Non-greedy `[^?]+` is fine.

	// Wait, I need to make sure I don't mask `?` placeholders that are NOT paired?
	// The previous code utilized `\?([^?]+)\?`.
	// What about SQL bind params like `?`? 
	// `WHERE DEPT = ?` <- this is not a match for `?...?`.
	// `WHERE DEPT = ?STARLIMSDEPT?` <- this IS a match.
	// My masking logic is consistent with the param formatter helper.

	// Implementation Note: I am replacing the `keywordRegex` block (lines 308-309).
	// So `sql` at this point usually has params formatted.
	// However, `maskLiteral` has to be robust. 
	// Also, string literals masking needs to handle escaped quotes?
	// SSL Strings: 'It''s' (double single quote). 
	// Regex `/(["'])(?:(?=(\\?))\2[\s\S])*?\1/g` might be for backslash escape. 
	// SSL uses `''`. 
	// Let's stick to simple quote pairing for now, assuming standard SQL.
	// Actually the user's string in line 45 is `'N/A'`.
	// The previous `findSqlStringInLine` used `/(["'])([\s\S]*?)\1/g`.
	// I'll use a safer regex for masking.


	// Format based on style
	switch (style) {
		case "compact":
			return formatCompactStyle(sql, keywordCase, indentSpaces);
		case "canonicalCompact":
			return formatCanonicalCompactStyle(sql, keywordCase, indentSpaces, wrapLength);
		case "expanded":
			return formatExpandedStyle(sql, keywordCase, indentSpaces);
		case "hangingOperators":
			return formatHangingOperatorsStyle(sql, keywordCase, indentSpaces);
		case "knr":
			return formatKnrStyle(sql, keywordCase, indentSpaces, false);
		case "knrCompact":
			return formatKnrStyle(sql, keywordCase, indentSpaces, true);
		case "ormFriendly":
			return formatOrmFriendlyStyle(sql, keywordCase, indentSpaces);
		default:
			return formatCompactStyle(sql, keywordCase, indentSpaces);
	}
}

/**
 * Compact Style - Single-line clauses
 * SELECT u.id, u.name FROM users u WHERE u.active = 1 AND u.deleted_at IS NULL
 */
function formatCompactStyle(sql: string, keywordCase: string, indentSpaces: number): string {
	const indent = " ".repeat(indentSpaces);

	// Break at major clause keywords
	SQL_CLAUSE_KEYWORDS.forEach(keyword => {
		if (keyword === "ON") { return; } // Keep ON on same line as JOIN in compact
		const pattern = keyword.replace(/\s+/g, "\\s+");
		const clauseRegex = new RegExp(`\\s+(${pattern})\\b`, "gi");
		sql = sql.replace(clauseRegex, (_match, clause) => {
			return `\n${applySqlKeywordCase(keyword, keywordCase, clause)}`;
		});
	});

	// Keep AND/OR inline in compact mode
	const lines = sql.split('\n').map(line => line.trim());
	return lines.map((line, index) => {
		if (index === 0) { return line; }
		return indent + line;
	}).join('\n');
}

/**
 * Expanded Style - Multi-line SELECT with vertical columns
 * SELECT
 *     u.id,
 *     u.name
 * FROM users u
 * WHERE
 *     u.active = 1
 */
function formatExpandedStyle(sql: string, keywordCase: string, indentSpaces: number): string {
	const indent = " ".repeat(indentSpaces);

	// Parse SELECT columns
	const selectMatch = sql.match(/^(SELECT\s+(?:DISTINCT\s+)?)(.*?)(?=\s+FROM\b)/i);
	let result = sql;

	if (selectMatch) {
		const selectKeyword = applySqlKeywordCase("SELECT", keywordCase, selectMatch[1].trim());
		const distinctMatch = selectMatch[1].match(/DISTINCT/i);
		const distinctPart = distinctMatch ? " " + applySqlKeywordCase("DISTINCT", keywordCase, distinctMatch[0]) : "";
		const columns = parseColumns(selectMatch[2]);

		const formattedSelect = selectKeyword + distinctPart + "\n" +
			columns.map(col => indent + col).join(",\n");

		result = sql.replace(selectMatch[0], formattedSelect + " ");
	}

	// Break at clause keywords
	SQL_CLAUSE_KEYWORDS.forEach(keyword => {
		const pattern = keyword.replace(/\s+/g, "\\s+");
		const clauseRegex = new RegExp(`\\s+(${pattern})\\b`, "gi");
		result = result.replace(clauseRegex, (_match, clause) => {
			return `\n${applySqlKeywordCase(keyword, keywordCase, clause)}`;
		});
	});

	// Handle WHERE with expanded conditions
	result = result.replace(/\bWHERE\b\s+/gi, (match) => {
		return applySqlKeywordCase("WHERE", keywordCase, match.trim()) + "\n" + indent;
	});

	// Handle AND/OR on new lines with indent
	result = result.replace(/\s+(AND|OR)\b/gi, (_match, op) => {
		return `\n${indent}${applySqlKeywordCase(op.toUpperCase(), keywordCase, op)}`;
	});

	// Handle GROUP BY and ORDER BY with expanded columns
	result = expandClauseColumns(result, "GROUP BY", keywordCase, indent);
	result = expandClauseColumns(result, "ORDER BY", keywordCase, indent);

	return result.trim();
}

/**
 * Canonical Compact Style - Hanging operators + JOIN breaks (default)
 * 
 * Format:
 * SELECT u.id, u.name, u.email,
 *        COUNT(o.id) AS order_count
 * FROM users u
 * JOIN orders o
 *   ON o.user_id = u.id
 * WHERE u.active = 1
 *   AND (u.deleted_at IS NULL
 *        OR u.deleted_at > NOW())
 * GROUP BY u.id, u.name, u.email
 * ORDER BY order_count DESC;
 */
function formatCanonicalCompactStyle(sql: string, keywordCase: string, indentSpaces: number, wrapLength: number = 0): string {
	let result = sql;
	const columnAlignIndent = 7; // "SELECT " length for column continuation alignment
	const hangingIndent = indentSpaces; // For ON, AND, OR (use configured indent)

	// Format SELECT with columns on same line where possible
	const selectMatch = result.match(/^(SELECT\s+(?:DISTINCT\s+)?)(.*?)(?=\s+FROM\b)/i);
	if (selectMatch) {
		const selectKeyword = applySqlKeywordCase("SELECT", keywordCase, selectMatch[1].trim());
		const distinctMatch = selectMatch[1].match(/DISTINCT/i);
		const distinctPart = distinctMatch ? " " + applySqlKeywordCase("DISTINCT", keywordCase, distinctMatch[0]) : "";
		const columns = parseColumns(selectMatch[2]);

		// Format columns with proper alignment
		// First columns on same line as SELECT, continuation aligned to 7 spaces
		const firstLinePrefix = selectKeyword + distinctPart + " ";
		const continuationPrefix = " ".repeat(columnAlignIndent);

		// Build column list with wrapping
		const formattedColumns: string[] = [];
		let currentLine = firstLinePrefix;
		const maxLineLength = 80;

		for (let i = 0; i < columns.length; i++) {
			const col = columns[i];
			const separator = i < columns.length - 1 ? ", " : "";
			const addition = col + separator;

			if (currentLine === firstLinePrefix) {
				// First column always goes on first line
				currentLine += addition;
			} else if (currentLine.length + addition.length <= maxLineLength) {
				// Fits on current line
				currentLine += addition;
			} else {
				// Need to wrap to next line
				formattedColumns.push(currentLine.trimEnd());
				currentLine = continuationPrefix + addition;
			}
		}
		if (currentLine.trim()) {
			formattedColumns.push(currentLine.trimEnd());
		}

		result = result.replace(selectMatch[0], formattedColumns.join("\n") + " ");
	}

	// Break at major clauses - each on its own line with no indent
	const majorClauses = [
		"UPDATE", "DELETE FROM",
		"FROM", "JOIN", "INNER JOIN", "LEFT JOIN", "RIGHT JOIN", "FULL JOIN", "CROSS JOIN",
		"WHERE", "GROUP BY", "ORDER BY", "HAVING", "UNION", "UNION ALL", "EXCEPT", "INTERSECT"
		// Removed SET, INSERT INTO, VALUES from majorClauses to handle them manually
	];

	majorClauses.forEach(keyword => {
		let pattern = keyword.replace(/\s+/g, "\\s+");

		// Use negative lookbehind for FROM to prevent breaking "DELETE FROM"
		if (keyword === "FROM") {
			pattern = `(?<!DELETE\\s+)FROM`;
		}

		const clauseRegex = new RegExp(`\\s+(${pattern})\\b`, "gi");
		result = result.replace(clauseRegex, (_match, clause) => {
			return `\n${applySqlKeywordCase(keyword, keywordCase, clause)}`;
		});
	});

	// Format SET clause assignments - UPDATE table SET ...
	// First normalize whitespace before SET (in case of "TABLE  SET" with double space)
	result = result.replace(/\s+SET\b/gi, ' SET');
	const setRegex = /(\bSET\b)([\s\S]+?)(?=\b(WHERE|FROM|JOIN|INNER|LEFT|RIGHT|FULL|CROSS|GROUP|ORDER|HAVING|UNION|EXCEPT|INTERSECT|$))/gi;
	result = result.replace(setRegex, (match, setKw, content, lookahead) => {
		const assignments = parseColumns(content);
		if (assignments.length > 0) {
			const indent = " ".repeat(indentSpaces); // Indent for assignments
			const formattedAssignments = assignments.map(a => indent + a.trim());

			// Result: SET \n    col1=val1,\n    col2=val2
			return `${applySqlKeywordCase("SET", keywordCase, "SET")}\n${formattedAssignments.join(',\n')}\n`;
		}
		return match;
	});

	/**
	 * Helper to wrap comma-separated list of items
	 */
	function wrapSqlList(items: string[], indent: string, wrapLength: number): string {
		if (items.length === 0) return "";
		if (wrapLength <= 0) return indent + items.join(", "); // No wrapping

		let currentLine = indent;
		const lines: string[] = [];

		for (let i = 0; i < items.length; i++) {
			const item = items[i];
			const separator = (i < items.length - 1) ? ", " : "";

			// If adding this item exceeds wrap length (and we aren't empty), push line
			if (currentLine.length + item.length + separator.length > wrapLength && currentLine.trim() !== "") {
				lines.push(currentLine.trimEnd());
				currentLine = indent + item + separator;
			} else {
				currentLine += item + separator;
			}
		}
		if (currentLine.trim()) {
			lines.push(currentLine.trimEnd());
		}
		return lines.join("\n");
	}

	// Format INSERT INTO ... VALUES/SELECT ...
	// Handles: INSERT INTO table (cols) VALUES (vals)
	// Handles: INSERT INTO table (cols) SELECT ...

	const insertRegex = /(\bINSERT\s+INTO\b[\s\S]+?\))\s*(\b(?:VALUES|SELECT)\b)([\s\S]+?)(?=(?:\b(UPDATE|DELETE|WHERE|GROUP|ORDER|HAVING|UNION|EXCEPT|INTERSECT)\b)|$|;)/gi;

	result = result.replace(insertRegex, (match, insertPart, keywordKw, contentPart) => {
		// 1. Process Insert Part
		let formattedInsert = insertPart.trim();
		const parenStart = formattedInsert.indexOf('(');
		let insertBlock = "";

		if (parenStart !== -1) {
			const insertIntoTable = formattedInsert.substring(0, parenStart).trim();
			const columnsContent = formattedInsert.substring(parenStart); // with parens

			// Strip parens for parsing
			const innerCols = columnsContent.replace(/^\s*\(\s*/, '').replace(/\s*\)\s*$/, '');
			const columns = parseColumns(innerCols);

			// Don't add manual indent spaces, let post-processing handle it via parens depth
			// But wrapSqlList needs to know availability.
			// Assume 4 spaces indent will be added by post-process.
			const wrapIndentVal = 0; // wrapSqlList generates clean lines, post-process adds indent
			const effectiveWrap = wrapLength > 0 ? Math.max(20, wrapLength - indentSpaces) : 0;

			// wrapSqlList should just return lines without indent prefix if we pass ""
			const wrappedCols = wrapSqlList(columns, "", effectiveWrap);

			const tablePart = insertIntoTable.replace(/^\s*INSERT\s+INTO\s*/i, '');
			const insertKw = applySqlKeywordCase("INSERT INTO", keywordCase, "INSERT INTO");

			insertBlock = `\n${insertKw} ${tablePart}\n(\n${wrappedCols}\n)`;
		} else {
			const insertKw = applySqlKeywordCase("INSERT INTO", keywordCase, "INSERT INTO");
			const remainder = formattedInsert.replace(/^\s*INSERT\s+INTO\s*/i, '');
			insertBlock = `\n${insertKw} ${remainder}`;
		}

		// 2. Process Values/Select Part
		let formattedContentPart = contentPart.trim();
		let contentBlock = "";

		const kwUpper = keywordKw.toUpperCase().trim();

		if (kwUpper === "VALUES") {
			const valParenStart = formattedContentPart.indexOf('(');

			if (valParenStart !== -1) {
				const valuesContent = formattedContentPart;
				const innerVals = valuesContent.replace(/^\s*\(\s*/, '').replace(/\s*\)\s*$/, '');
				const vals = parseColumns(innerVals);

				const effectiveWrap = wrapLength > 0 ? Math.max(20, wrapLength - indentSpaces) : 0;
				const wrappedVals = wrapSqlList(vals, "", effectiveWrap);

				contentBlock = `\n${applySqlKeywordCase("VALUES", keywordCase, "VALUES")}\n(\n${wrappedVals}\n)`;
			} else {
				contentBlock = `\n${applySqlKeywordCase("VALUES", keywordCase, "VALUES")} ${formattedContentPart}`;
			}
		} else if (kwUpper === "SELECT") {
			// Handle SELECT part
			// SELECT list might not have parens. 
			// Check if it starts with parens? Rare for standard INSERT SELECT.
			// Just treat as list of columns.

			// If formatting SELECT, we should wrap the list.
			// But unlike VALUES, we need manual indentation because there are no parens to trigger post-process indent.
			const indent = " ".repeat(indentSpaces);
			const effectiveWrap = wrapLength > 0 ? Math.max(20, wrapLength - indentSpaces) : 0;

			// We parseColumns just like values.
			// Note: SELECT list might contain function calls with parens, parseColumns handles that.
			const colList = parseColumns(formattedContentPart);

			// Should we force newline? Yes usually.
			// Use explicit indent for wrapSqlList because post-process won't add it (depth 0).
			const wrappedSelect = wrapSqlList(colList, indent, effectiveWrap);

			// Append newline to separate from subsequent clauses (like FROM) which might be immediately following
			contentBlock = `\n${applySqlKeywordCase("SELECT", keywordCase, "SELECT")}\n${wrappedSelect}\n`;
		} else {
			// Fallback
			contentBlock = `\n${applySqlKeywordCase(keywordKw, keywordCase, keywordKw)} ${formattedContentPart}`;
		}

		return `${insertBlock}${contentBlock}`;
	});

	// Move ON conditions to their own indented line after JOIN
	result = result.replace(/((?:INNER |LEFT |RIGHT |FULL )?JOIN[^\n]*?)\s+ON\s+(.+?)(?=\n|$)/gi,
		(_match, joinPart, condition) => {
			return `${joinPart}\n${" ".repeat(hangingIndent)}${applySqlKeywordCase("ON", keywordCase, "ON")} ${condition.trim()}`;
		}
	);

	// Handle AND/OR with hanging indent (no change needed)
	result = result.replace(/\s+(AND)\s+/gi, (_match, op) => {
		return `\n${" ".repeat(hangingIndent)}${applySqlKeywordCase("AND", keywordCase, op)} `;
	});

	result = result.replace(/\s+(OR)\s+/gi, (_match, op) => {
		return `\n${" ".repeat(columnAlignIndent)}${applySqlKeywordCase("OR", keywordCase, op)} `;
	});

	// Format subquery SELECTs - add space before ( and newline after ( before SELECT
	// This handles patterns like NOT EXISTS(SELECT, IN(SELECT, = (SELECT
	result = result.replace(/(EXISTS|IN)\s*(\()(\s*)(SELECT\b)/gi, (_match, keyword, paren, space, selectKw) => {
		return `${keyword.toUpperCase()} ${paren}\n${applySqlKeywordCase("SELECT", keywordCase, selectKw)}`;
	});

	// Also handle general (SELECT patterns not preceded by EXISTS/IN
	result = result.replace(/([^A-Z])(\()(\s*)(SELECT\b)/gi, (_match, before, paren, space, selectKw) => {
		return `${before}${paren}\n${applySqlKeywordCase("SELECT", keywordCase, selectKw)}`;
	});

	// Post-process for indentation based on nesting (Subquery alignment)
	// First pass: split closing ) to its own line when the opening ( was followed by newline
	// Pattern: if ( is at end of line (after optional whitespace), and later there's content ending with ), split the )
	// This handles subqueries and multi-line paren blocks
	let processedResult = result;

	// Match pattern: opening ( at end of line ... content ending with )
	// We need to be careful to match the CORRECT closing paren
	// Use a simpler approach: track depth and split ) when appropriate
	const preLines = result.split('\n');
	let openParenAtEndOfLine = 0; // Track depth of ( at end of line
	const processedLines: string[] = [];
	// Track whether opening ( had content before it (like "AND NOT EXISTS (")
	const openParenHadContent: boolean[] = [];

	for (let i = 0; i < preLines.length; i++) {
		const line = preLines[i];
		const trimmedEnd = line.trimEnd();
		const trimmed = line.trim();

		// Check if this line ends with ( only (not part of inline like IN (?x?))
		// Only count if the ( is truly at the end with nothing after it
		if (trimmedEnd.endsWith('(') && !trimmedEnd.match(/\([^)]*\)$/)) {
			openParenAtEndOfLine++;
			// Track if there was content before the ( (like "AND NOT EXISTS (")
			const hasContentBefore = trimmed !== '(';
			openParenHadContent.push(hasContentBefore);
		}

		// Check if line starts with ) - this closes one level
		if (trimmed.startsWith(')')) {
			openParenAtEndOfLine = Math.max(0, openParenAtEndOfLine - 1);
			openParenHadContent.pop();
			processedLines.push(line);
			continue;
		}

		// Check if this line ends with ) (not inline) and we have open parens at end of previous lines
		// Don't split if the ) is part of an inline paren pair like IN (?x?)
		const hasContentBeforeCloseParen = /[^(\s]+\)\s*["']?\s*$/.test(trimmedEnd);
		const isInlineParen = /\([^)]+\)\s*["']?\s*$/.test(trimmedEnd); // like IN (?x?)

		if (openParenAtEndOfLine > 0 && hasContentBeforeCloseParen && !isInlineParen) {
			// Split the ) to its own line
			const match = line.match(/^(.+?)(\)\s*["']?\s*)$/);
			if (match) {
				processedLines.push(match[1]);
				processedLines.push(match[2].trim());
				openParenAtEndOfLine--;
				openParenHadContent.pop();
			} else {
				processedLines.push(line);
			}
		} else {
			processedLines.push(line);
		}
	}

	processedResult = processedLines.join('\n');

	const lines = processedResult.split('\n');
	let currentDepth = 0;
	const subqueryIndent = " ".repeat(indentSpaces); // Matches basic indent
	// Track whether each level's opening ( had content before it
	const contentOpenedParens: boolean[] = [];

	const indentedLines = lines.map((line, lineIndex) => {
		// Count parens on this line to track depth changes
		let netChange = 0;
		let inString = false;
		let stringChar = '';

		for (let i = 0; i < line.length; i++) {
			const char = line[i];
			if (inString) {
				if (char === stringChar) inString = false;
			} else {
				if (char === "'" || char === '"' || char === '[') {
					inString = true;
					stringChar = char === '[' ? ']' : char;
				} else if (char === '(') {
					netChange++;
				} else if (char === ')') {
					netChange--;
				}
			}
		}

		const trimmed = line.trim();
		const trimmedEnd = line.trimEnd();

		// Track if this line ends with ( and whether it had content
		if (trimmedEnd.endsWith('(') && !trimmedEnd.match(/\([^)]*\)$/)) {
			const hasContent = trimmed !== '(';
			contentOpenedParens.push(hasContent);
		}

		// Lines starting with ) should use the depth before the closing
		let effectiveDepth = currentDepth;
		let closingContentParen = false;
		if (trimmed.startsWith(')')) {
			effectiveDepth = Math.max(0, currentDepth - 1);
			// Check if the matching opener had content
			if (contentOpenedParens.length > 0) {
				closingContentParen = contentOpenedParens.pop() || false;
			}
		}

		// Apply depth-based indent (4 spaces per nesting level)
		let indent = "";
		if (effectiveDepth > 0) {
			indent = subqueryIndent.repeat(effectiveDepth);
		}

		currentDepth += netChange;

		// Add extra indent for major clauses inside subqueries (depth > 0) 
		// to ensure visual nesting relative to outer clause
		let baseIndent = "";
		const upper = trimmed.toUpperCase();
		if (effectiveDepth > 0 && (
			upper.startsWith('SELECT ') ||
			upper.startsWith('FROM ') ||
			upper.startsWith('WHERE ') ||
			upper.startsWith('AND ') ||
			upper.startsWith('OR ')
		)) {
			// These clauses get extra indent to align inside subquery
			baseIndent = " ".repeat(indentSpaces);
		}

		// Closing parens that match a content-opened paren get hanging indent
		// to align with the content (like AND NOT EXISTS)
		// Closing parens for simple ( lines don't get extra indent
		if (trimmed.startsWith(')') && closingContentParen) {
			baseIndent = " ".repeat(hangingIndent);
		}

		// Add depth indent + base indent to existing line content
		return indent + baseIndent + line;
	});

	return indentedLines.join('\n').trim();
}

/**
 * Hanging Operators Style - AND/OR at line start with reduced indent
 * WHERE u.active = 1
 *   AND u.deleted_at IS NULL
 *   OR u.deleted_at > NOW()
 */
function formatHangingOperatorsStyle(sql: string, keywordCase: string, indentSpaces: number): string {
	const indent = " ".repeat(indentSpaces);
	const hangingIndent = "  "; // 2 spaces for hanging operators

	// Break at major clause keywords
	SQL_CLAUSE_KEYWORDS.forEach(keyword => {
		const pattern = keyword.replace(/\s+/g, "\\s+");
		const clauseRegex = new RegExp(`\\s+(${pattern})\\b`, "gi");
		sql = sql.replace(clauseRegex, (_match, clause) => {
			return `\n${applySqlKeywordCase(keyword, keywordCase, clause)}`;
		});
	});

	// Handle AND/OR with hanging indent
	sql = sql.replace(/\s+(AND|OR)\b/gi, (_match, op) => {
		return `\n${hangingIndent}${applySqlKeywordCase(op.toUpperCase(), keywordCase, op)}`;
	});

	// Handle ON clause with indent
	sql = sql.replace(/\n(ON)\b/gi, (_match, on) => {
		return `\n${hangingIndent}${applySqlKeywordCase("ON", keywordCase, on)}`;
	});

	return sql.trim();
}

/**
 * ORM-Friendly Style - Inline JOINs, hanging logical operators
 */
function formatOrmFriendlyStyle(sql: string, keywordCase: string, indentSpaces: number): string {
	const indent = " ".repeat(indentSpaces);
	const hangingIndent = "  ";
	let result = sql;

	// Format SELECT columns vertically (reuse expanded behavior)
	const selectMatch = result.match(/^(SELECT\s+(?:DISTINCT\s+)?)(.*?)(?=\s+FROM\b)/i);
	if (selectMatch) {
		const selectKeyword = applySqlKeywordCase("SELECT", keywordCase, selectMatch[1].trim());
		const distinctMatch = selectMatch[1].match(/DISTINCT/i);
		const distinctPart = distinctMatch ? " " + applySqlKeywordCase("DISTINCT", keywordCase, distinctMatch[0]) : "";
		const columns = parseColumns(selectMatch[2]);

		const formattedSelect = selectKeyword + distinctPart + "\n" +
			columns.map(col => indent + col).join(",\n");

		result = result.replace(selectMatch[0], formattedSelect + " ");
	}

	// Break before major clauses but keep JOIN inline with FROM
	result = result.replace(/\s+FROM\s+/gi, () => `\n${applySqlKeywordCase("FROM", keywordCase, "FROM")} `);
	result = result.replace(/\s+WHERE\s+/gi, () => `\n${applySqlKeywordCase("WHERE", keywordCase, "WHERE")} `);
	result = result.replace(/\s+GROUP BY\s+/gi, () => `\n${applySqlKeywordCase("GROUP BY", keywordCase, "GROUP BY")} `);
	result = result.replace(/\s+ORDER BY\s+/gi, () => `\n${applySqlKeywordCase("ORDER BY", keywordCase, "ORDER BY")} `);

	// Keep JOIN inline while normalizing case
	result = result.replace(/\b(INNER JOIN|LEFT JOIN|RIGHT JOIN|FULL JOIN|CROSS JOIN|JOIN)\b/gi,
		(_match, clause) => applySqlKeywordCase((clause as string).toUpperCase(), keywordCase, clause));

	// Hanging AND/OR in WHERE clauses
	result = result.replace(/\s+(AND|OR)\b/gi, (_match, op) => `\n${hangingIndent}${applySqlKeywordCase((op as string).toUpperCase(), keywordCase, op)}`);

	// Tidy whitespace
	const lines = result.split('\n').map(line => line.trimEnd());
	return lines.join('\n').trim();
}

/**
 * K&R Style - Parenthesized blocks
 * SELECT
 *     u.id,
 *     u.name
 * FROM users u
 * JOIN orders o
 * ON (
 *     o.user_id = u.id
 * )
 * WHERE (
 *     u.active = 1
 *     AND (
 *         u.deleted_at IS NULL
 *         OR u.deleted_at > NOW()
 *     )
 * )
 */
function formatKnrStyle(sql: string, keywordCase: string, indentSpaces: number, compactSelect: boolean): string {
	const indent = " ".repeat(indentSpaces);

	// Parse SELECT columns
	const selectMatch = sql.match(/^(SELECT\s+(?:DISTINCT\s+)?)(.*?)(?=\s+FROM\b)/i);
	let result = sql;

	if (selectMatch) {
		const selectKeyword = applySqlKeywordCase("SELECT", keywordCase, selectMatch[1].trim());
		const distinctMatch = selectMatch[1].match(/DISTINCT/i);
		const distinctPart = distinctMatch ? " " + applySqlKeywordCase("DISTINCT", keywordCase, distinctMatch[0]) : "";
		const columns = parseColumns(selectMatch[2]);

		let formattedSelect: string;
		if (compactSelect && columns.join(", ").length < 60) {
			// Compact: SELECT u.id, u.name, u.email
			formattedSelect = selectKeyword + distinctPart + "\n" + indent + columns.join(", ");
		} else {
			// Expanded: one column per line
			formattedSelect = selectKeyword + distinctPart + "\n" +
				columns.map(col => indent + col).join(",\n");
		}

		result = sql.replace(selectMatch[0], formattedSelect + " ");
	}

	// Break at major clause keywords (except ON which gets special handling)
	["FROM", "JOIN", "INNER JOIN", "LEFT JOIN", "RIGHT JOIN", "FULL JOIN",
		"GROUP BY", "ORDER BY", "HAVING", "UNION", "UNION ALL"].forEach(keyword => {
			const pattern = keyword.replace(/\s+/g, "\\s+");
			const clauseRegex = new RegExp(`\\s+(${pattern})\\b`, "gi");
			result = result.replace(clauseRegex, (_match, clause) => {
				return `\n${applySqlKeywordCase(keyword, keywordCase, clause)}`;
			});
		});

	// Handle ON with parentheses block
	result = result.replace(/\s+ON\s+([^)]+?)(?=\s+(?:WHERE|JOIN|INNER|LEFT|RIGHT|FULL|GROUP|ORDER|HAVING|UNION|$))/gi,
		(_match, condition) => {
			const onKeyword = applySqlKeywordCase("ON", keywordCase, "ON");
			return `\n${onKeyword} (\n${indent}${condition.trim()}\n)`;
		}
	);

	// Handle WHERE with parentheses block
	result = result.replace(/\s+WHERE\s+(.+?)(?=\s+(?:GROUP|ORDER|HAVING|UNION|$)|$)/gi,
		(_match, condition) => {
			const whereKeyword = applySqlKeywordCase("WHERE", keywordCase, "WHERE");
			const formattedCondition = formatKnrConditions(condition, keywordCase, indent);
			return `\n${whereKeyword} (\n${formattedCondition}\n)`;
		}
	);

	// Handle GROUP BY with parentheses
	result = result.replace(/\nGROUP BY\s+([^)]+?)(?=\s+(?:ORDER|HAVING|$)|$)/gi,
		(_match, columns) => {
			const groupByKeyword = applySqlKeywordCase("GROUP BY", keywordCase, "GROUP BY");
			const cols = parseColumns(columns);
			if (cols.length > 1) {
				return `\n${groupByKeyword} (\n${cols.map(c => indent + c).join(",\n")}\n)`;
			}
			return `\n${groupByKeyword} ${columns.trim()}`;
		}
	);

	// Handle ORDER BY with parentheses
	result = result.replace(/\nORDER BY\s+(.+?)$/gi,
		(_match, columns) => {
			const orderByKeyword = applySqlKeywordCase("ORDER BY", keywordCase, "ORDER BY");
			const cols = parseColumns(columns.replace(/;$/, ''));
			if (cols.length > 1) {
				return `\n${orderByKeyword} (\n${cols.map(c => indent + c).join(",\n")}\n)`;
			}
			return `\n${orderByKeyword} ${columns.trim()}`;
		}
	);

	return result.trim();
}

/**
 * Format conditions for K&R style with nested parentheses
 */
function formatKnrConditions(condition: string, keywordCase: string, indent: string): string {
	let result = condition.trim();

	// Handle AND/OR
	result = result.replace(/\s+(AND|OR)\s+/gi, (_match, op) => {
		return `\n${indent}${applySqlKeywordCase(op.toUpperCase(), keywordCase, op)} `;
	});

	// Add initial indent
	const lines = result.split('\n');
	return lines.map((line, index) => {
		if (index === 0) { return indent + line; }
		return line;
	}).join('\n');
}

/**
 * Parse comma-separated column list, handling nested parentheses
 */
function parseColumns(columnStr: string): string[] {
	const columns: string[] = [];
	let current = "";
	let depth = 0;

	for (const char of columnStr) {
		if (char === '(' || char === '[') {
			depth++;
			current += char;
		} else if (char === ')' || char === ']') {
			depth--;
			current += char;
		} else if (char === ',' && depth === 0) {
			if (current.trim()) {
				columns.push(current.trim());
			}
			current = "";
		} else {
			current += char;
		}
	}

	if (current.trim()) {
		columns.push(current.trim());
	}

	return columns;
}

/**
 * Expand columns in GROUP BY or ORDER BY clauses
 */
function expandClauseColumns(sql: string, clause: string, keywordCase: string, indent: string): string {
	const pattern = new RegExp(`\\b${clause}\\b\\s+([^\\n]+)`, "gi");
	return sql.replace(pattern, (_match, columns) => {
		const keyword = applySqlKeywordCase(clause, keywordCase, clause);
		const cols = parseColumns(columns);
		if (cols.length > 2) {
			return keyword + "\n" + cols.map(c => indent + c).join(",\n");
		}
		return keyword + " " + columns;
	});
}

/**
 * Apply keyword case preference
 */
function applySqlKeywordCase(keyword: string, style: string, original: string): string {
	if (style === "lower") {
		return keyword.toLowerCase();
	}
	if (style === "upper") {
		return keyword.toUpperCase();
	}
	return original;
}

// Legacy exports for backward compatibility with tests
export function formatSqlContent(content: string, keywordCase: string, indentSpaces: number): string {
	return formatSqlWithStyleImpl(content, "compact", keywordCase, indentSpaces);
}

export function formatAsMultilineString(sql: string, quoteChar: string, wasStringLiteral: boolean): string {
	if (!wasStringLiteral) {
		return sql;
	}
	const closeQuote = quoteChar === '[' ? ']' : quoteChar;
	return `${quoteChar}${sql}${closeQuote}`;
}

export function formatAsConcatenatedString(sql: string, quoteChar: string): string {
	const closeQuote = quoteChar === '[' ? ']' : quoteChar;
	const lines = sql.split('\n');

	if (lines.length === 1) {
		return `${quoteChar}${sql}${closeQuote}`;
	}

	const parts = lines.map((line, index) => {
		const isLast = index === lines.length - 1;
		const content = isLast ? line : line + " ";
		return `${quoteChar}${content}${closeQuote}`;
	});

	return parts.join(" +\n");
}
