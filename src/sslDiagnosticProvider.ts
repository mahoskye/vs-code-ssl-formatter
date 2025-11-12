import * as vscode from "vscode";

/**
 * SSL Diagnostic Provider
 * Provides real-time diagnostics for code quality and style guide enforcement
 */
export class SSLDiagnosticProvider {

	private diagnosticCollection: vscode.DiagnosticCollection;

	constructor() {
		this.diagnosticCollection = vscode.languages.createDiagnosticCollection("ssl");
	}

	public updateDiagnostics(document: vscode.TextDocument): void {
		const config = vscode.workspace.getConfiguration("ssl");
		const maxProblems = config.get<number>("maxNumberOfProblems", 100);
		const strictMode = config.get<boolean>("strictStyleGuideMode", false);

		const diagnostics: vscode.Diagnostic[] = [];
		const text = document.getText();
		const lines = text.split("\n");

		// Track nesting depth
		let blockDepth = 0;
		const maxBlockDepth = config.get<number>("styleGuide.limitBlockDepth", 4);

		// Track procedure info
		let inProcedure = false;
		let procedureParams = 0;
		const maxParams = config.get<number>("styleGuide.maxParamsPerProcedure", 8);

		// Track multi-line comment state
		let inMultiLineComment = false;

		// Hungarian notation settings
		const hungarianEnabled = config.get<boolean>("naming.hungarianNotation.enabled", true);
		const hungarianSeverity = config.get<string>("naming.hungarianNotation.severity", "warn");

		// Security settings
		const preventSqlInjection = config.get<boolean>("security.preventSqlInjection", true);
		const requireParameterized = config.get<boolean>("security.requireParameterizedQueries", true);

		// Style guide settings
		const enforceKeywordCase = config.get<boolean>("styleGuide.enforceKeywordCase", true);
		const enforceCommentSyntax = config.get<boolean>("styleGuide.enforceCommentSyntax", true);

		for (let i = 0; i < lines.length && diagnostics.length < maxProblems; i++) {
			const line = lines[i];
			const trimmed = line.trim();

			// Track multi-line comment state
			// Multi-line comments in SSL: /* ... ; (end with semicolon, not */)
			if (trimmed.startsWith("/*") && !trimmed.endsWith(";")) {
				inMultiLineComment = true;
			}
			if (inMultiLineComment && trimmed.endsWith(";")) {
				inMultiLineComment = false;
				continue; // Skip this line as it's the comment terminator
			}
			if (inMultiLineComment) {
				continue; // Skip all lines inside multi-line comments
			}

			// Check block depth
			if (this.isBlockStart(trimmed)) {
				blockDepth++;
				if (maxBlockDepth > 0 && blockDepth > maxBlockDepth) {
					const diagnostic = new vscode.Diagnostic(
						new vscode.Range(i, 0, i, line.length),
						`Block nesting depth (${blockDepth}) exceeds maximum (${maxBlockDepth})`,
						strictMode ? vscode.DiagnosticSeverity.Error : vscode.DiagnosticSeverity.Warning
					);
					diagnostic.code = "ssl-block-depth";
					diagnostics.push(diagnostic);
				}
			}

			if (this.isBlockEnd(trimmed)) {
				blockDepth = Math.max(0, blockDepth - 1);
			}

			// Check procedure parameters
			const procMatch = trimmed.match(/^:PROCEDURE\s+(\w+)/i);
			if (procMatch) {
				inProcedure = true;
				procedureParams = 0;
			}

			const paramsMatch = trimmed.match(/^:PARAMETERS\s+(.+?);/i);
			if (paramsMatch && inProcedure) {
				const params = paramsMatch[1].split(",");
				procedureParams = params.length;

				if (maxParams > 0 && procedureParams > maxParams) {
					const diagnostic = new vscode.Diagnostic(
						new vscode.Range(i, 0, i, line.length),
						`Procedure has ${procedureParams} parameters, exceeds maximum (${maxParams})`,
						strictMode ? vscode.DiagnosticSeverity.Error : vscode.DiagnosticSeverity.Warning
					);
					diagnostic.code = "ssl-max-params";
					diagnostics.push(diagnostic);
				}

				// Check Hungarian notation on parameters
				if (hungarianEnabled) {
					params.forEach(param => {
						const paramName = param.trim();
						if (!this.hasValidHungarianNotation(paramName)) {
							const severity = this.getSeverity(hungarianSeverity, strictMode);
							const diagnostic = new vscode.Diagnostic(
								new vscode.Range(i, line.indexOf(paramName), i, line.indexOf(paramName) + paramName.length),
								`Parameter '${paramName}' should use Hungarian notation (e.g., sName, nCount, aItems)`,
								severity
							);
							diagnostic.code = "ssl-hungarian-notation";
							diagnostics.push(diagnostic);
						}
					});
				}
			}

			const endProcMatch = trimmed.match(/^:(ENDPROC|ENDPROCEDURE)\b/i);
			if (endProcMatch) {
				inProcedure = false;
			}

			// Check for SQL injection risks
			if (preventSqlInjection) {
				const sqlMatch = trimmed.match(/(SQLExecute|RunSQL|LSearch)\s*\(/i);
				if (sqlMatch) {
					const hasParameter = /\?(\w+)?\?/.test(trimmed);
					if (!hasParameter && requireParameterized && /\+/.test(trimmed)) {
						const diagnostic = new vscode.Diagnostic(
							new vscode.Range(i, 0, i, line.length),
							"Potential SQL injection: Use parameterized queries (?PARAM?) instead of string concatenation",
							strictMode ? vscode.DiagnosticSeverity.Error : vscode.DiagnosticSeverity.Warning
						);
						diagnostic.code = "ssl-sql-injection";
						diagnostics.push(diagnostic);
					}
				}
			}

			// Check for missing semicolons
			// Skip: empty lines, comments, multi-line constructs, lines ending with semicolon
			// Skip: lines ending with operators (continuation lines)
			const isSingleLineComment = trimmed.startsWith("/*") && trimmed.endsWith(";");
			const isCommentLine = trimmed.startsWith("*") || trimmed.startsWith("/*");
			const isContinuationLine = /[+\-*/,]$/.test(trimmed); // Ends with operator or comma
			
			if (trimmed && !trimmed.endsWith(";") && !isCommentLine && !this.isMultilineConstruct(trimmed) && !isContinuationLine) {
				const diagnostic = new vscode.Diagnostic(
					new vscode.Range(i, line.length - 1, i, line.length),
					"Statement should end with semicolon",
					vscode.DiagnosticSeverity.Warning
				);
				diagnostic.code = "ssl-missing-semicolon";
				diagnostics.push(diagnostic);
			}

			// Check for nested ternaries (if we detect them)
			// Avoid false positives from SQL placeholders like ?PARAM?
			const ternaryPattern = /\?[^?]+\?[^?]*:[^?]*\?[^?]+\?/;
			if (ternaryPattern.test(trimmed) && !trimmed.includes("?PARAM?") && !/\?\w+\?/.test(trimmed)) {
				const diagnostic = new vscode.Diagnostic(
					new vscode.Range(i, 0, i, line.length),
					"Nested ternary expressions are discouraged for readability",
					vscode.DiagnosticSeverity.Information
				);
				diagnostic.code = "ssl-nested-ternary";
				diagnostics.push(diagnostic);
			}

			// Check variable declarations for Hungarian notation and invalid syntax
			if (hungarianEnabled) {
				const declareMatch = trimmed.match(/^:DECLARE\s+(.+?);/i);
				if (declareMatch) {
					const declString = declareMatch[1];
					
					// Check for invalid :DECLARE with assignment
					if (declString.includes(':=')) {
						const diagnostic = new vscode.Diagnostic(
							new vscode.Range(i, 0, i, line.length),
							"Invalid syntax: :DECLARE cannot initialize values. Use ':DECLARE var;' followed by 'var := value;' on separate lines",
							vscode.DiagnosticSeverity.Error
						);
						diagnostic.code = "ssl-invalid-declare";
						diagnostics.push(diagnostic);
						continue; // Skip Hungarian notation check for invalid syntax
					}
					
					// Check for invalid 'const' or 'CONST' keyword
					if (/\b(const|CONST)\b/.test(declString)) {
						const diagnostic = new vscode.Diagnostic(
							new vscode.Range(i, 0, i, line.length),
							"Invalid syntax: 'const' is not a valid SSL keyword. Remove 'const' and use proper Hungarian notation",
							vscode.DiagnosticSeverity.Error
						);
						diagnostic.code = "ssl-invalid-const";
						diagnostics.push(diagnostic);
						continue; // Skip Hungarian notation check for invalid syntax
					}
					
					const vars = declString.split(",");
					vars.forEach(varName => {
						const cleanName = varName.trim();
						if (!this.hasValidHungarianNotation(cleanName)) {
							const severity = this.getSeverity(hungarianSeverity, strictMode);
							const diagnostic = new vscode.Diagnostic(
								new vscode.Range(i, line.indexOf(cleanName), i, line.indexOf(cleanName) + cleanName.length),
								`Variable '${cleanName}' should use Hungarian notation (e.g., sName, nCount, aItems)`,
								severity
							);
							diagnostic.code = "ssl-hungarian-notation";
							diagnostics.push(diagnostic);
						}
					});
				}
			}

			// Check for CASE without OTHERWISE
			const beginCaseMatch = trimmed.match(/^:BEGINCASE\b/i);
			if (beginCaseMatch) {
				let hasOtherwise = false;
				let caseDepth = 1;
				for (let j = i + 1; j < lines.length && caseDepth > 0; j++) {
					const caseLine = lines[j].trim();
					if (/^:BEGINCASE\b/i.test(caseLine)) {
						caseDepth++;
					}
					if (/^:ENDCASE\b/i.test(caseLine)) {
						caseDepth--;
					}
					if (caseDepth === 1 && /^:OTHERWISE\b/i.test(caseLine)) {
						hasOtherwise = true;
						break;
					}
				}

				if (!hasOtherwise) {
					const diagnostic = new vscode.Diagnostic(
						new vscode.Range(i, 0, i, line.length),
						"CASE statement should include :OTHERWISE clause for completeness",
						vscode.DiagnosticSeverity.Information
					);
					diagnostic.code = "ssl-missing-otherwise";
					diagnostics.push(diagnostic);
				}
			}

			// Check keyword case (should be UPPERCASE)
			if (enforceKeywordCase) {
				const keywordMatch = trimmed.match(/^:([A-Za-z]+)\b/);
				if (keywordMatch) {
					const keyword = keywordMatch[1];
					if (keyword !== keyword.toUpperCase()) {
						const diagnostic = new vscode.Diagnostic(
							new vscode.Range(i, line.indexOf(`:${keyword}`), i, line.indexOf(`:${keyword}`) + keyword.length + 1),
							`Keyword should be UPPERCASE: :${keyword.toUpperCase()} (style guide requires UPPERCASE keywords)`,
							vscode.DiagnosticSeverity.Warning
						);
						diagnostic.code = "ssl-keyword-case";
						diagnostics.push(diagnostic);
					}
				}
			}

			// Check comment syntax (should be /* ... ; not /* ... */)
			if (enforceCommentSyntax) {
				const invalidCommentMatch = line.match(/\/\*.*\*\//);
				if (invalidCommentMatch && !line.includes(';')) {
					const diagnostic = new vscode.Diagnostic(
						new vscode.Range(i, 0, i, line.length),
						"Invalid SSL comment syntax: Comments should use /* ... ; (semicolon terminator, not */)",
						vscode.DiagnosticSeverity.Warning
					);
					diagnostic.code = "ssl-comment-syntax";
					diagnostics.push(diagnostic);
				}
			}
		}

		this.diagnosticCollection.set(document.uri, diagnostics);
	}

	public clear(document: vscode.TextDocument): void {
		this.diagnosticCollection.delete(document.uri);
	}

	public dispose(): void {
		this.diagnosticCollection.dispose();
	}

	private isBlockStart(line: string): boolean {
		return /^:(IF|WHILE|FOR|FOREACH|BEGINCASE|TRY|PROCEDURE|CLASS|REGION)\b/i.test(line);
	}

	private isBlockEnd(line: string): boolean {
		return /^:(ENDIF|ENDWHILE|NEXT|ENDCASE|ENDTRY|ENDPROC|ENDPROCEDURE|ENDREGION)\b/i.test(line);
	}

	private isMultilineConstruct(line: string): boolean {
		// Check if line is part of a multi-line construct that doesn't need semicolon on every line
		// Or is a control flow keyword that ends with the keyword itself
		return /^:(IF|ELSE|WHILE|FOR|TO|STEP|FOREACH|IN|BEGINCASE|CASE|OTHERWISE|EXITCASE|TRY|CATCH|FINALLY|PROCEDURE|PARAMETERS|DEFAULT|CLASS|INHERIT|REGION|ENDIF|ENDWHILE|NEXT|ENDCASE|ENDTRY|ENDPROC|ENDPROCEDURE|ENDREGION|LOOP|EXITWHILE|RETURN)\b/i.test(line);
	}

	private hasValidHungarianNotation(name: string): boolean {
		// Exceptions for loop counters and constants
		const exceptions = ["i", "j", "k", "NIL", ".T.", ".F.", "ID", "SQL", "URL", "XML", "HTML", "API", "UID", "GUID"];
		if (exceptions.includes(name)) {
			return true;
		}

		// Allow ALL_CAPS constants (global constants pattern)
		if (/^[A-Z][A-Z0-9_]+$/.test(name)) {
			return true;
		}

		if (name.length < 2) {
			return false;
		}

		const prefix = name[0].toLowerCase();
		const validPrefixes = ["s", "n", "b", "l", "d", "a", "o", "u"];

		// Check if first char is valid prefix and second char is uppercase
		return validPrefixes.includes(prefix) && name[1] === name[1].toUpperCase();
	}

	private getSeverity(severityString: string, strictMode: boolean): vscode.DiagnosticSeverity {
		if (strictMode) {
			return vscode.DiagnosticSeverity.Error;
		}

		switch (severityString.toLowerCase()) {
			case "error":
				return vscode.DiagnosticSeverity.Error;
			case "warn":
			case "warning":
				return vscode.DiagnosticSeverity.Warning;
			case "info":
			case "information":
				return vscode.DiagnosticSeverity.Information;
			default:
				return vscode.DiagnosticSeverity.Warning;
		}
	}
}
