import * as vscode from "vscode";
import { CONFIG_KEYS, CONFIG_DEFAULTS } from "../constants/config";
import { SqlFormatter, SqlFormattingOptions } from "../formatting/sqlFormatter";
import { SqlLexer } from "../parsing/sqlLexer";
import { detectSqlContext, findSqlStringInLine, looksLikeSql } from "../parsing/sqlContext";
import { formatSqlWithStyleImpl, SqlFormattingStyle } from "../formatting/legacySqlFormatter";

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

	// Command to format SQL in a specific range (used by code action)
	const formatSqlRangeCommand = vscode.commands.registerCommand(
		"ssl.formatSqlRange",
		async (uri: vscode.Uri, range: vscode.Range) => {
			const editor = vscode.window.activeTextEditor;
			if (!editor || editor.document.uri.toString() !== uri.toString()) {
				return;
			}

			// Select the range and then format
			editor.selection = new vscode.Selection(range.start, range.end);

			// Get default style from config
			const config = vscode.workspace.getConfiguration("ssl");
			const defaultStyle = config.get<SqlFormattingStyle>(
				CONFIG_KEYS.FORMAT_SQL_STYLE,
				CONFIG_DEFAULTS[CONFIG_KEYS.FORMAT_SQL_STYLE] as SqlFormattingStyle
			);
			await formatSqlWithStyle(defaultStyle);
		}
	);

	context.subscriptions.push(formatSqlCommand, formatSqlPickStyleCommand, formatSqlRangeCommand);
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

	// Resolve indentation settings
	let indentSpaces = 4;
	let indentString = "    ";

	// Check if user explicitly set indentation settings in config
	const inspectIndent = config.inspect<number>(CONFIG_KEYS.FORMAT_SQL_INDENT_SPACES);
	const explicitIndent = inspectIndent && (inspectIndent.globalValue !== undefined || inspectIndent.workspaceValue !== undefined || inspectIndent.workspaceFolderValue !== undefined);

	if (explicitIndent) {
		indentSpaces = config.get<number>(CONFIG_KEYS.FORMAT_SQL_INDENT_SPACES) || 4;
		indentString = " ".repeat(indentSpaces);
	} else {
		// specific indent not set, use editor settings
		if (editor.options.insertSpaces) {
			indentSpaces = Number(editor.options.tabSize) || 4;
			indentString = " ".repeat(indentSpaces);
		} else {
			indentSpaces = Number(editor.options.tabSize) || 4; // fallback for calculation
			indentString = "\t";
		}
	}

	const wrapLength = config.get<number>(CONFIG_KEYS.FORMAT_WRAP_LENGTH, 90);

	// Get selection or current line
	let selection = editor.selection;
	let text: string;
	let targetRange: vscode.Range;
	let baseIndentColumn = 0;

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
		baseIndentColumn = sqlRange.range.start.character;
	} else {
		// Use selection
		targetRange = selection;
		text = editor.document.getText(selection);

		// If multiline selection, adjust base indent from first line
		const startLine = editor.document.lineAt(selection.start.line);
		baseIndentColumn = startLine.firstNonWhitespaceCharacterIndex;
	}

	// Detect Context
	const contextStr = detectSqlContext(text);
	const sqlContent = contextStr.content;

	// Check if content looks like SQL
	if (!looksLikeSql(sqlContent)) {
		vscode.window.showInformationMessage("Selected text doesn't appear to be SQL. Select a SQL string to format.");
		return;
	}

	let formattedSql: string;

	// Use robust formatting engine for default style (canonicalCompact)
	if (style === "canonicalCompact") {
		const options: SqlFormattingOptions = {
			wrapLength: wrapLength,
			insertSpaces: editor.options.insertSpaces as boolean, // respect editor settings
			tabSize: Number(editor.options.tabSize) || 4,
			keywordCase: keywordCase as any,
			indentSpaces: explicitIndent ? indentSpaces : undefined, // pass undefined to let formatter use tabs if applicable
			style: style
		};

		const formatter = new SqlFormatter(options);
		const lexer = new SqlLexer(sqlContent);
		const tokens = lexer.tokenize();

		// quoteChar="" so we get raw formatted SQL back, then we wrap it ourselves based on context
		formattedSql = formatter.formatSqlTokens(tokens, "", baseIndentColumn);
	} else {
		// Use legacy implementation for other styles
		// Pass indentString instead of just number
		formattedSql = formatSqlWithStyleImpl(sqlContent, style, keywordCase, indentSpaces, wrapLength, indentString);
		// formatSqlWithStyleImpl returns UNQUOTED sql.
	}

	// Reconstruction
	if (contextStr.type === 'quoted' || contextStr.type === 'function') {
		formattedSql = contextStr.prefix + formattedSql + contextStr.suffix;
	} else {
		// Raw SQL. Just return formatted.
		formattedSql = formattedSql;
	}

	// Apply the edit
	// Preserve trailing newline if it existed in the original selection
	if (text.endsWith('\n') && !formattedSql.endsWith('\n')) {
		formattedSql += '\n';
	} else if (text.endsWith('\r\n') && !formattedSql.endsWith('\r\n')) {
		formattedSql += '\r\n';
	}

	// Apply the edit
	await editor.edit(editBuilder => {
		editBuilder.replace(targetRange, formattedSql);
	});

	vscode.window.showInformationMessage(`SQL formatted with ${STYLE_DISPLAY_NAMES[style]} style.`);
}
