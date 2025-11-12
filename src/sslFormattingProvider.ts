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

		// Normalize line endings to \n for processing
		formatted = formatted.replace(/\r\n/g, "\n");

		// Apply formatting rules
		formatted = this.normalizeKeywordCase(formatted, keywordCase);
		formatted = this.normalizeBuiltinFunctionCase(formatted, builtinFunctionCase);
		formatted = this.normalizeOperatorSpacing(formatted);
		formatted = this.normalizeIndentation(formatted, indentStyle, indentWidth);

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
	 */
	private normalizeKeywordCase(text: string, caseStyle: string): string {
		if (caseStyle === "preserve") {
			return text;
		}

		const keywords = [
			"IF", "ELSE", "ELSEIF", "ENDIF",
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

		keywords.forEach(keyword => {
			const pattern = new RegExp(`:${keyword}\\b`, "gi");
			const replacement = caseStyle === "upper"
				? `:${keyword.toUpperCase()}`
				: `:${keyword.toLowerCase()}`;
			text = text.replace(pattern, replacement);
		});

		return text;
	}

	/**
	 * Normalize built-in function casing
	 */
	private normalizeBuiltinFunctionCase(text: string, caseStyle: string): string {
		if (caseStyle === "preserve") {
			return text;
		}

		const functions = [
			"SQLExecute", "DOPROC", "EXECFUNCTION", "EMPTY", "LEN", "USRMES", "CHR",
			"AADD", "ALLTRIM", "AT", "NOW", "TODAY",
			"CREATEUDOBJECT", "BUILDSTRING", "ASCAN", "ALEN", "ARRAYCALC", "BUILDARRAY",
			"DIRECTORY", "CREATEGUID", "BUILDSTRINGFORIN", "ASCANEXACT",
			"DAY", "ARRAYNEW", "BRANCH", "DATEADD", "DATEDIFF", "ABS",
			"Left", "Right", "SubStr", "StrTran", "Upper", "Lower", "Trim",
			"AEVAL", "RunSQL", "LSearch", "GetDataSet",
			"GetSystemDate", "GetSystemTime", "CtoD", "FormatDateTime",
			"DownloadFile", "UploadFile", "FileExists", "DeleteFile",
			"InfoMes", "ErrorMes", "GetSetting", "GetUserSetting", "SetUserSetting",
			"GetLastSSLError", "ReturnLastSqlError", "FormatErrorMessage", "RaiseError",
			"Val", "LimsTypeEx"
		];

		functions.forEach(func => {
			const pattern = new RegExp(`\\b${func}\\b`, "gi");
			let replacement: string;

			switch (caseStyle) {
				case "PascalCase":
					replacement = this.toPascalCase(func);
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

			text = text.replace(pattern, replacement);
		});

		return text;
	}

	/**
	 * Convert string to PascalCase
	 */
	private toPascalCase(str: string): string {
		return str.charAt(0).toUpperCase() + str.slice(1).toLowerCase().replace(/([A-Z])/g, (match, p1, offset) => {
			return offset > 0 ? p1.toUpperCase() : p1;
		});
	}

	/**
	 * Normalize operator spacing
	 */
	private normalizeOperatorSpacing(text: string): string {
		// Space around assignment operators
		text = text.replace(/\s*:=\s*/g, " := ");
		text = text.replace(/\s*\+=\s*/g, " += ");
		text = text.replace(/\s*-=\s*/g, " -= ");
		text = text.replace(/\s*\*=\s*/g, " *= ");
		text = text.replace(/\s*\/=\s*/g, " /= ");
		text = text.replace(/\s*\^=\s*/g, " ^= ");
		text = text.replace(/\s*%=\s*/g, " %= ");

		// Space around comparison operators
		text = text.replace(/\s*==\s*/g, " == ");
		text = text.replace(/\s*!=\s*/g, " != ");
		text = text.replace(/\s*<>\s*/g, " <> ");
		text = text.replace(/\s*<=\s*/g, " <= ");
		text = text.replace(/\s*>=\s*/g, " >= ");

		// Space around logical operators
		text = text.replace(/\s*\.AND\.\s*/gi, " .AND. ");
		text = text.replace(/\s*\.OR\.\s*/gi, " .OR. ");
		text = text.replace(/\s*\.NOT\.\s*/gi, " .NOT. ");

		// Space after commas
		text = text.replace(/,\s*/g, ", ");

		// No space before semicolons
		text = text.replace(/\s+;/g, ";");

		return text;
	}

	/**
	 * Normalize indentation
	 */
	private normalizeIndentation(text: string, indentStyle: string, indentWidth: number): string {
		const lines = text.split("\n");
		let indentLevel = 0;
		const indentChar = indentStyle === "tab" ? "\t" : " ".repeat(indentWidth);

		const blockStart = /^\s*:(IF|WHILE|FOR|FOREACH|BEGINCASE|TRY|PROCEDURE|CLASS|REGION)\b/i;
		const blockMiddle = /^\s*:(ELSE|ELSEIF|CATCH|FINALLY|CASE|OTHERWISE)\b/i;
		const blockEnd = /^\s*:(ENDIF|ENDWHILE|NEXT|ENDCASE|ENDTRY|ENDPROC|ENDPROCEDURE|ENDREGION)\b/i;

		const formatted = lines.map(line => {
			const trimmed = line.trim();

			if (!trimmed || trimmed.startsWith("/*")) {
				return trimmed;
			}

			// Decrease indent for block end and middle keywords
			if (blockEnd.test(trimmed) || blockMiddle.test(trimmed)) {
				indentLevel = Math.max(0, indentLevel - 1);
			}

			const indented = indentChar.repeat(indentLevel) + trimmed;

			// Increase indent after block start
			if (blockStart.test(trimmed)) {
				indentLevel++;
			}

			// Restore indent after middle keywords
			if (blockMiddle.test(trimmed)) {
				indentLevel++;
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
