import * as vscode from "vscode";
import { CONFIG_KEYS, CONFIG_DEFAULTS } from "../constants/config";

/**
 * SQL formatting style presets
 */
export type SqlFormattingStyle =
	| "compact"           // Single-line clauses, compact SELECT
	| "expanded"          // Multi-line SELECT with vertical columns
	| "hangingOperators"  // AND/OR at line start with indent
	| "knr"               // K&R style with parentheses blocks
	| "knrCompact";       // K&R style with compact SELECT list

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
	"expanded": "Expanded - Vertical columns",
	"hangingOperators": "Hanging Operators - AND/OR at line start",
	"knr": "K&R Style - Parenthesized blocks",
	"knrCompact": "K&R Compact - Parenthesized with compact SELECT"
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
			{ label: "$(list-tree) Expanded", style: "expanded", description: "Multi-line SELECT with vertical columns" },
			{ label: "$(indent) Hanging Operators", style: "hangingOperators", description: "AND/OR at line start with indent" },
			{ label: "$(bracket) K&R Style", style: "knr", description: "Parenthesized blocks, one column per line" },
			{ label: "$(bracket-dot) K&R Compact", style: "knrCompact", description: "Parenthesized blocks, compact SELECT" }
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
	indentSpaces: number
): string {
	// Normalize the SQL first
	let sql = content.replace(/\r\n/g, "\n").trim();
	if (!sql) {
		return content.trim();
	}

	// Collapse multiple whitespace to single space
	sql = sql.replace(/\s+/g, " ");

	// Apply keyword casing
	const keywordRegex = new RegExp(`\\b(${SQL_GENERAL_KEYWORDS.join("|")})\\b`, "gi");
	sql = sql.replace(keywordRegex, match => applySqlKeywordCase(match.toUpperCase(), keywordCase, match));

	// Format based on style
	switch (style) {
		case "compact":
			return formatCompactStyle(sql, keywordCase, indentSpaces);
		case "expanded":
			return formatExpandedStyle(sql, keywordCase, indentSpaces);
		case "hangingOperators":
			return formatHangingOperatorsStyle(sql, keywordCase, indentSpaces);
		case "knr":
			return formatKnrStyle(sql, keywordCase, indentSpaces, false);
		case "knrCompact":
			return formatKnrStyle(sql, keywordCase, indentSpaces, true);
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
		if (keyword === "ON") {return;} // Keep ON on same line as JOIN in compact
		const pattern = keyword.replace(/\s+/g, "\\s+");
		const clauseRegex = new RegExp(`\\s+(${pattern})\\b`, "gi");
		sql = sql.replace(clauseRegex, (_match, clause) => {
			return `\n${applySqlKeywordCase(keyword, keywordCase, clause)}`;
		});
	});

	// Keep AND/OR inline in compact mode
	const lines = sql.split('\n').map(line => line.trim());
	return lines.map((line, index) => {
		if (index === 0) {return line;}
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
		if (index === 0) {return indent + line;}
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
