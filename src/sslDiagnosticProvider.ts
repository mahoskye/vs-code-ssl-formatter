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
		const fileName = document.fileName.split('/').pop();
		console.log(`[SSL Debug] ========== Analyzing ${fileName} ==========`);
		
		const config = vscode.workspace.getConfiguration("ssl");
		const maxProblems = config.get<number>("maxNumberOfProblems", 100);
		const strictMode = config.get<boolean>("strictStyleGuideMode", false);

		const diagnostics: vscode.Diagnostic[] = [];
		const text = document.getText();
		const lines = text.split("\n");
		
		console.log(`[SSL Debug] Total lines: ${lines.length}`);

		// Track nesting depth
		let blockDepth = 0;
		const maxBlockDepth = config.get<number>("styleGuide.limitBlockDepth", 4);

		// Track procedure info
		let inProcedure = false;
		let procedureParams = 0;
		const maxParams = config.get<number>("styleGuide.maxParamsPerProcedure", 8);

		// Track multi-line comment state
		let inMultiLineComment = false;
		
		// Track multi-line string state
		let inMultiLineString = false;
		let stringDelimiter = "";

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

			// Validate SQL parameter placeholders BEFORE skipping strings
			// This catches parameters in query strings, including multi-line strings
			const paramPlaceholders = line.match(/\?(\w+)\?/g);
			if (paramPlaceholders && preventSqlInjection) {
				const declaredIdentifiers = this.getDeclaredIdentifiers(lines, i);
				
				paramPlaceholders.forEach(placeholder => {
					const paramName = placeholder.replace(/\?/g, '');
					
					console.log(`[SSL Debug] Line ${i + 1}: Checking SQL param '${paramName}', declared identifiers: [${Array.from(declaredIdentifiers).join(', ')}]`);
					
					// Check if it's a valid identifier (constant or variable)
					if (!declaredIdentifiers.has(paramName)) {
						const columnIndex = line.indexOf(placeholder);
						const diagnostic = new vscode.Diagnostic(
							new vscode.Range(i, columnIndex, i, columnIndex + placeholder.length),
							`SQL parameter '${paramName}' does not reference a valid variable or constant. Use lowercase variable names like '?sResult?' to pass through strings.`,
							vscode.DiagnosticSeverity.Error
						);
						diagnostic.code = "ssl-invalid-sql-param";
						diagnostics.push(diagnostic);
						console.log(`[SSL Debug] ERROR: Invalid SQL param '${paramName}' at line ${i + 1}`);
					}
				});
			}

			// Check for undeclared variable assignments BEFORE skipping multi-line strings
			// This ensures we catch assignments like: sQuery := "SELECT..."
			const assignmentMatch = trimmed.match(/^([a-z][a-zA-Z0-9_]*)\s*:=/);
			if (assignmentMatch && !trimmed.startsWith(':')) {
				const varName = assignmentMatch[1];
				const declaredIdentifiers = this.getDeclaredIdentifiers(lines, i);
				
				console.log(`[SSL Debug] Line ${i + 1}: Checking assignment '${varName}', declared identifiers: [${Array.from(declaredIdentifiers).join(', ')}]`);
				
				// Check if variable was declared before use
				if (!declaredIdentifiers.has(varName)) {
					const diagnostic = new vscode.Diagnostic(
						new vscode.Range(i, 0, i, varName.length),
						`Variable '${varName}' is used without being declared. Add ':DECLARE ${varName};' before first use.`,
						vscode.DiagnosticSeverity.Warning
					);
					diagnostic.code = "ssl-undeclared-variable";
					diagnostics.push(diagnostic);
					console.log(`[SSL Debug] WARNING: Undeclared variable '${varName}' at line ${i + 1}`);
				}
			}

			// Track multi-line string state
			// Check if line opens a string that doesn't close
			if (!inMultiLineString) {
				// Check for opening quote (double or single) that doesn't close on same line
				const doubleQuoteMatch = line.match(/"/g);
				const singleQuoteMatch = line.match(/'/g);
				
				if (doubleQuoteMatch && doubleQuoteMatch.length % 2 !== 0) {
					inMultiLineString = true;
					stringDelimiter = '"';
					continue; // Skip this line - it opens a multi-line string
				} else if (singleQuoteMatch && singleQuoteMatch.length % 2 !== 0) {
					inMultiLineString = true;
					stringDelimiter = "'";
					continue; // Skip this line - it opens a multi-line string
				}
			} else {
				// We're inside a multi-line string, check if it closes
				const delimiterCount = (line.match(new RegExp(stringDelimiter === '"' ? '"' : "'", 'g')) || []).length;
				if (delimiterCount % 2 !== 0) {
					// String closes on this line
					inMultiLineString = false;
					stringDelimiter = "";
				}
				// Skip all processing for lines inside strings
				continue;
			}

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

			// Check for undeclared variable usage (reading variables)
			// This catches cases like: infomes(sVariable); or DoProc("Test", {sVar});
			// Skip lines that are declarations, assignments, or keywords
			if (trimmed && 
				!trimmed.startsWith(':') && 
				!trimmed.startsWith('/*') && 
				!trimmed.startsWith('*') &&
				!/^[a-z][a-zA-Z0-9_]*\s*:=/.test(trimmed)) { // Skip assignments (already checked)
				
				const declaredIdentifiers = this.getDeclaredIdentifiers(lines, i);
				
				// Extract potential variable references (Hungarian notation variables)
				// Match lowercase-starting identifiers (sVar, nCount, etc.) but not function calls
				const varReferences = trimmed.match(/\b([a-z][a-zA-Z0-9_]*)\b/g);
				
				if (varReferences) {
					// Filter out known keywords and function names
					const sslKeywords = new Set([
						'if', 'else', 'endif', 'while', 'endwhile', 'for', 'to', 'step', 'next',
						'foreach', 'in', 'case', 'endcase', 'otherwise', 'exitcase', 'begincase',
						'try', 'catch', 'finally', 'endtry', 'return', 'loop', 'exitwhile',
						'and', 'or', 'not', 'nil', 'true', 'false'
					]);
					
					// Common SSL functions (lowercase versions)
					const sslFunctions = new Set([
						'usrmes', 'infomes', 'doproc', 'sqlexecute', 'runsql', 'lsearch',
						'alen', 'ascan', 'aadd', 'arraynew', 'empty', 'str', 'val', 'alltrim',
						'now', 'getsetting', 'createudobject', 'createguid', 'getlastsslerror'
					]);
					
					// Loop counter exceptions
					const loopCounters = new Set(['i', 'j', 'k', 'x', 'y', 'z']);
					
					varReferences.forEach(varRef => {
						const lowerRef = varRef.toLowerCase();
						
						// Skip keywords, functions, loop counters, and already declared identifiers
						if (!sslKeywords.has(lowerRef) && 
							!sslFunctions.has(lowerRef) &&
							!loopCounters.has(lowerRef) &&
							!declaredIdentifiers.has(varRef)) {
							
							// Check if this looks like a Hungarian notation variable
							if (/^[a-z][A-Z]/.test(varRef)) { // e.g., sVar, nCount
								const columnIndex = line.indexOf(varRef);
								if (columnIndex !== -1) {
									const diagnostic = new vscode.Diagnostic(
										new vscode.Range(i, columnIndex, i, columnIndex + varRef.length),
										`Variable '${varRef}' does not exist in the current scope. It may be declared in a different procedure or not declared at all.`,
										vscode.DiagnosticSeverity.Error
									);
									diagnostic.code = "ssl-undefined-variable";
									diagnostics.push(diagnostic);
									console.log(`[SSL Debug] ERROR: Undefined variable '${varRef}' at line ${i + 1}`);
								}
							}
						}
					});
				}
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
						diagnostic.code = "sql-sql-injection";
						diagnostics.push(diagnostic);
					}
				}
			}

			// Check DoProc calls for missing parameters
			// Match both DoProc("Name", {...}) and DoProc("Name") patterns
			const doProcWithBracesMatch = trimmed.match(/DoProc\s*\(\s*["']([^"']+)["']\s*,\s*\{([^}]*)\}/i);
			const doProcWithoutBracesMatch = trimmed.match(/DoProc\s*\(\s*["']([^"']+)["']\s*\)/i);
			
			if (doProcWithBracesMatch || doProcWithoutBracesMatch) {
				const procedureName = doProcWithBracesMatch ? doProcWithBracesMatch[1] : doProcWithoutBracesMatch![1];
				const argsString = doProcWithBracesMatch ? doProcWithBracesMatch[2].trim() : '';
				
				// Check if there's any non-whitespace, non-comma content in the braces
				const hasContent = /[^,\s]/.test(argsString);
				
				// Get expected parameters for this procedure
				const expectedParams = this.getProcedureParameters(lines, procedureName);
				
				// Only warn if procedure expects parameters and none were provided (empty braces or no braces)
				if (expectedParams.length > 0 && !hasContent) {
					const diagnostic = new vscode.Diagnostic(
						new vscode.Range(i, 0, i, line.length),
						`Procedure '${procedureName}' expects ${expectedParams.length} parameter${expectedParams.length > 1 ? 's' : ''} (${expectedParams.join(', ')}) but none were provided`,
						vscode.DiagnosticSeverity.Warning
					);
					diagnostic.code = "ssl-missing-params";
					diagnostics.push(diagnostic);
				}
			}

			// Check for missing semicolons
			// Skip: empty lines, comments, multi-line constructs, lines ending with semicolon
			// Skip: lines ending with operators (continuation lines)
			// Skip: lines starting with operators (continuation lines with leading operator)
			// Skip: lines where the next line starts with an operator (leading continuation style)
			const isSingleLineComment = trimmed.startsWith("/*") && trimmed.endsWith(";");
			const isCommentLine = trimmed.startsWith("*") || trimmed.startsWith("/*");
			const endsWithOperator = /[+\-*/,]$/.test(trimmed);
			const startsWithOperator = /^[+\-*/,]/.test(trimmed);
			const nextLine = i + 1 < lines.length ? lines[i + 1].trim() : "";
			const nextStartsWithOperator = /^[+\-*/,]/.test(nextLine);
			const isContinuationLine = endsWithOperator || startsWithOperator || nextStartsWithOperator;
			
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

		console.log(`[SSL Debug] Found ${diagnostics.length} diagnostic${diagnostics.length !== 1 ? 's' : ''}`);
		if (diagnostics.length > 0) {
			diagnostics.forEach((d, idx) => {
				console.log(`  ${idx + 1}. Line ${d.range.start.line + 1}: [${d.code}] ${d.message}`);
			});
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

	/**
	 * Get the list of parameters defined for a procedure
	 */
	private getProcedureParameters(lines: string[], procedureName: string): string[] {
		// Find the procedure definition
		const procedurePattern = new RegExp(`^\\s*:PROCEDURE\\s+${procedureName}\\b`, 'i');
		let procedureLineIndex = -1;

		for (let i = 0; i < lines.length; i++) {
			if (procedurePattern.test(lines[i])) {
				procedureLineIndex = i;
				break;
			}
		}

		if (procedureLineIndex === -1) {
			return []; // Procedure not found in document
		}

		// Look for :PARAMETERS line after :PROCEDURE
		for (let i = procedureLineIndex + 1; i < Math.min(procedureLineIndex + 20, lines.length); i++) {
			const line = lines[i].trim();

			// Check for :PARAMETERS
			const paramsMatch = line.match(/^:PARAMETERS\s+(.+?);/i);
			if (paramsMatch) {
				return paramsMatch[1].split(',').map(p => p.trim());
			}

			// Stop at next procedure or other block keyword
			if (/^:(PROCEDURE|ENDPROC|DECLARE)\b/i.test(line)) {
				break;
			}
		}

		return []; // No parameters found
	}

	/**
	 * Get all declared identifiers (variables, constants, parameters) visible at a given line
	 * This is scope-aware: only returns identifiers that are accessible at the given line
	 */
	private getDeclaredIdentifiers(lines: string[], currentLine: number): Set<string> {
		const globalIdentifiers = new Set<string>();
		const localIdentifiers = new Set<string>();
		let inProcedure = false;
		let currentProcedureStartLine = -1;
		let inMultiLineComment = false;

		// First pass: collect global declarations (before any procedure)
		for (let i = 0; i < lines.length; i++) {
			const line = lines[i].trim();

			// Skip comment lines
			if (line.startsWith('/*')) {
				if (!line.endsWith(';')) {
					inMultiLineComment = true;
					continue;
				}
				continue;
			}
			if (inMultiLineComment) {
				if (line.endsWith(';')) {
					inMultiLineComment = false;
				}
				continue;
			}
			if (line.startsWith('*')) {
				continue;
			}

			// Stop at first procedure - everything before is global scope
			if (/^:PROCEDURE\b/i.test(line)) {
				break;
			}

			// Collect global :DECLARE variables
			const declareMatch = line.match(/^:DECLARE\s+(.+?);/i);
			if (declareMatch) {
				const vars = declareMatch[1].split(',').map(v => v.trim());
				vars.forEach(varName => {
					const cleanName = varName.split(':=')[0].trim();
					if (cleanName) {
						globalIdentifiers.add(cleanName);
					}
				});
			}

			// Collect global constants (ALL_CAPS variables assigned at file level)
			if (/^[A-Z_][A-Z0-9_]*\s*:=/.test(line)) {
				const constMatch = line.match(/^([A-Z_][A-Z0-9_]*)\s*:=/);
				if (constMatch) {
					globalIdentifiers.add(constMatch[1]);
				}
			}
		}

		// Second pass: determine current scope and collect local identifiers
		inProcedure = false;
		currentProcedureStartLine = -1;
		inMultiLineComment = false;
		let procedureDepth = 0;

		for (let i = 0; i <= currentLine && i < lines.length; i++) {
			const line = lines[i].trim();

			// Skip comment lines
			if (line.startsWith('/*')) {
				if (!line.endsWith(';')) {
					inMultiLineComment = true;
					continue;
				}
				continue;
			}
			if (inMultiLineComment) {
				if (line.endsWith(';')) {
					inMultiLineComment = false;
				}
				continue;
			}
			if (line.startsWith('*')) {
				continue;
			}

			// Track procedure scope with nesting support
			if (/^:PROCEDURE\b/i.test(line)) {
				if (procedureDepth === 0) {
					// Entering the outermost procedure
					inProcedure = true;
					currentProcedureStartLine = i;
					localIdentifiers.clear(); // Clear previous procedure's locals
				}
				procedureDepth++;
			}

			if (/^:(ENDPROC|ENDPROCEDURE)\b/i.test(line)) {
				procedureDepth = Math.max(0, procedureDepth - 1);
				if (procedureDepth === 0) {
					// Exiting the outermost procedure
					inProcedure = false;
					currentProcedureStartLine = -1;
					localIdentifiers.clear();
				}
			}

			// If we're in a procedure, collect local declarations
			if (inProcedure && i >= currentProcedureStartLine) {
				// Collect :DECLARE variables in current procedure
				const declareMatch = line.match(/^:DECLARE\s+(.+?);/i);
				if (declareMatch) {
					const vars = declareMatch[1].split(',').map(v => v.trim());
					vars.forEach(varName => {
						const cleanName = varName.split(':=')[0].trim();
						if (cleanName) {
							localIdentifiers.add(cleanName);
						}
					});
				}

				// Collect :PARAMETERS in current procedure
				const paramsMatch = line.match(/^:PARAMETERS\s+(.+?);/i);
				if (paramsMatch) {
					const params = paramsMatch[1].split(',').map(p => p.trim());
					params.forEach(paramName => {
						if (paramName) {
							localIdentifiers.add(paramName);
						}
					});
				}
			}
		}

		// Combine global and local identifiers
		const allIdentifiers = new Set<string>([...globalIdentifiers, ...localIdentifiers]);
		return allIdentifiers;
	}
}

