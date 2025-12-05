import * as vscode from "vscode";
import { CONFIG_KEYS, CONFIG_DEFAULTS } from "../constants/config";

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
	"SET"
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
 * Output style for formatted SQL
 */
export type SqlOutputStyle = "multiline" | "concatenated";

/**
 * Register the Format SQL command
 */
export function registerFormatSqlCommand(context: vscode.ExtensionContext): void {
	const disposable = vscode.commands.registerCommand("ssl.formatSql", async () => {
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
		const outputStyle = config.get<SqlOutputStyle>(
			CONFIG_KEYS.FORMAT_SQL_OUTPUT_STYLE,
			CONFIG_DEFAULTS[CONFIG_KEYS.FORMAT_SQL_OUTPUT_STYLE] as SqlOutputStyle
		);

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

		// Format the SQL
		const formattedSql = formatSqlContent(sqlContent, keywordCase, indentSpaces);

		// Build the output based on style
		let formattedOutput: string;
		if (outputStyle === "concatenated") {
			formattedOutput = formatAsConcatenatedString(formattedSql, quoteChar);
		} else {
			// multiline - wrap in quotes with newlines
			formattedOutput = formatAsMultilineString(formattedSql, quoteChar, isStringLiteral);
		}

		// Apply the edit
		await editor.edit(editBuilder => {
			editBuilder.replace(targetRange, formattedOutput);
		});

		vscode.window.showInformationMessage("SQL formatted successfully.");
	});

	context.subscriptions.push(disposable);
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
		const stringEnd = stringStart + match[2].length + match[3].length + 1; // +1 for closing quote

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
 * Format SQL content with line breaks and indentation
 * @public Exported for testing
 */
export function formatSqlContent(content: string, keywordCase: string, indentSpaces: number): string {
	let sql = content.replace(/\r\n/g, "\n").trim();
	if (!sql) {
		return content.trim();
	}

	// Collapse multiple whitespace to single space
	sql = sql.replace(/\s+/g, " ");

	// Add line breaks before clause keywords
	SQL_CLAUSE_KEYWORDS.forEach(keyword => {
		const pattern = keyword.replace(/\s+/g, "\\s+");
		const clauseRegex = new RegExp(`\\s+(${pattern})\\b`, "gi");
		sql = sql.replace(clauseRegex, (_match, clause) => {
			return `\n${applySqlKeywordCase(keyword, keywordCase, clause)}`;
		});
	});

	// Add line breaks before AND/OR
	sql = sql.replace(/\s+(AND|OR)\b/gi, (_match, clause) => {
		return `\n${applySqlKeywordCase(clause.toUpperCase(), keywordCase, clause)}`;
	});

	// Apply case to all SQL keywords
	const keywordRegex = new RegExp(`\\b(${SQL_GENERAL_KEYWORDS.join("|")})\\b`, "gi");
	sql = sql.replace(keywordRegex, match => applySqlKeywordCase(match.toUpperCase(), keywordCase, match));

	// Apply indentation
	const indentUnit = indentSpaces > 0 ? " ".repeat(indentSpaces) : "";
	const doubleIndent = indentUnit + indentUnit;

	const lines = sql.split('\n').map(line => line.trim()).filter((line, index, arr) => {
		// Remove trailing empty lines
		return !(line === "" && index === arr.length - 1);
	});

	return lines.map((line, index) => {
		if (index === 0) {
			return line;
		}
		const upper = line.toUpperCase();
		// Double indent for AND/OR
		if (upper.startsWith("AND") || upper.startsWith("OR")) {
			return (doubleIndent || indentUnit) + line;
		}
		return indentUnit + line;
	}).join('\n');
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

/**
 * Format SQL as a multi-line string literal
 * @public Exported for testing
 */
export function formatAsMultilineString(sql: string, quoteChar: string, wasStringLiteral: boolean): string {
	if (!wasStringLiteral) {
		return sql;
	}

	const closeQuote = quoteChar === '[' ? ']' : quoteChar;

	// For bracket strings, just wrap the formatted SQL
	if (quoteChar === '[') {
		return `${quoteChar}${sql}${closeQuote}`;
	}

	// For quote strings, include the formatted multi-line content
	return `${quoteChar}${sql}${closeQuote}`;
}

/**
 * Format SQL as concatenated string parts
 * @public Exported for testing
 */
export function formatAsConcatenatedString(sql: string, quoteChar: string): string {
	const closeQuote = quoteChar === '[' ? ']' : quoteChar;
	const lines = sql.split('\n');

	if (lines.length === 1) {
		return `${quoteChar}${sql}${closeQuote}`;
	}

	// Build concatenated string with + operators
	const parts = lines.map((line, index) => {
		const isLast = index === lines.length - 1;
		// Add trailing space for proper SQL spacing when concatenated
		const content = isLast ? line : line + " ";
		return `${quoteChar}${content}${closeQuote}`;
	});

	return parts.join(" +\n");
}
