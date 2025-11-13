import * as vscode from "vscode";
import {
    SSL_KEYWORDS,
    SSL_BUILTIN_FUNCTIONS,
    BLOCK_START_KEYWORDS,
    BLOCK_END_KEYWORDS,
    BLOCK_MIDDLE_KEYWORDS,
    CASE_KEYWORDS,
    MULTILINE_CONSTRUCT_KEYWORDS
} from "./constants/language";
import {
    CONFIG_KEYS,
    CONFIG_DEFAULTS
} from "./constants/config";
import {
    PATTERNS
} from "./constants/patterns";
import {
    DIAGNOSTIC_CODES,
    DIAGNOSTIC_MESSAGES,
    DIAGNOSTIC_SEVERITIES
} from "./constants/diagnostics";
import { hasValidHungarianNotation } from "./constants/hungarian";

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
		const maxProblems = config.get<number>(CONFIG_KEYS.MAX_NUMBER_OF_PROBLEMS, CONFIG_DEFAULTS[CONFIG_KEYS.MAX_NUMBER_OF_PROBLEMS]);
		const strictMode = config.get<boolean>(CONFIG_KEYS.STRICT_STYLE_GUIDE_MODE, CONFIG_DEFAULTS[CONFIG_KEYS.STRICT_STYLE_GUIDE_MODE]);

		const diagnostics: vscode.Diagnostic[] = [];
		const text = document.getText();
		const lines = text.split("\n");
		
		console.log(`[SSL Debug] Total lines: ${lines.length}`);

		// Track nesting depth
		let blockDepth = 0;
		const maxBlockDepth = config.get<number>(CONFIG_KEYS.STYLE_GUIDE_LIMIT_BLOCK_DEPTH, CONFIG_DEFAULTS[CONFIG_KEYS.STYLE_GUIDE_LIMIT_BLOCK_DEPTH]);

		// Track procedure info
		let inProcedure = false;
		let procedureParams = 0;
		const maxParams = config.get<number>(CONFIG_KEYS.STYLE_GUIDE_MAX_PARAMS_PER_PROCEDURE, CONFIG_DEFAULTS[CONFIG_KEYS.STYLE_GUIDE_MAX_PARAMS_PER_PROCEDURE]);

		// Track multi-line comment state
		let inMultiLineComment = false;
		
		// Track multi-line string state
		let inMultiLineString = false;
		let stringDelimiter = "";

		// Hungarian notation settings
		const hungarianEnabled = config.get<boolean>(CONFIG_KEYS.NAMING_HUNGARIAN_ENABLED, CONFIG_DEFAULTS[CONFIG_KEYS.NAMING_HUNGARIAN_ENABLED]);
		const hungarianSeverity = config.get<string>(CONFIG_KEYS.NAMING_HUNGARIAN_SEVERITY, CONFIG_DEFAULTS[CONFIG_KEYS.NAMING_HUNGARIAN_SEVERITY]);

		// Security settings
		const preventSqlInjection = config.get<boolean>(CONFIG_KEYS.SECURITY_PREVENT_SQL_INJECTION, CONFIG_DEFAULTS[CONFIG_KEYS.SECURITY_PREVENT_SQL_INJECTION]);
		const requireParameterized = config.get<boolean>(CONFIG_KEYS.SECURITY_REQUIRE_PARAMETERIZED_QUERIES, CONFIG_DEFAULTS[CONFIG_KEYS.SECURITY_REQUIRE_PARAMETERIZED_QUERIES]);

		// Style guide settings
		const enforceKeywordCase = config.get<boolean>(CONFIG_KEYS.STYLE_GUIDE_ENFORCE_KEYWORD_CASE, CONFIG_DEFAULTS[CONFIG_KEYS.STYLE_GUIDE_ENFORCE_KEYWORD_CASE]);
		const enforceCommentSyntax = config.get<boolean>(CONFIG_KEYS.STYLE_GUIDE_ENFORCE_COMMENT_SYNTAX, CONFIG_DEFAULTS[CONFIG_KEYS.STYLE_GUIDE_ENFORCE_COMMENT_SYNTAX]);

		for (let i = 0; i < lines.length && diagnostics.length < maxProblems; i++) {
			const line = lines[i];
			const trimmed = line.trim();

			// Validate SQL parameter placeholders BEFORE skipping strings
			// This catches parameters in query strings, including multi-line strings
			const paramPlaceholders = line.match(PATTERNS.SQL_PARAMETER_PLACEHOLDER);
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
							DIAGNOSTIC_MESSAGES.INVALID_SQL_PARAM(paramName),
							vscode.DiagnosticSeverity.Error
						);
						diagnostic.code = DIAGNOSTIC_CODES.INVALID_SQL_PARAM;
						diagnostics.push(diagnostic);
						console.log(`[SSL Debug] ERROR: Invalid SQL param '${paramName}' at line ${i + 1}`);
					}
				});
			}

			// Check for undeclared variable assignments BEFORE skipping multi-line strings
			// This ensures we catch assignments like: sQuery := "SELECT..."
			const assignmentMatch = trimmed.match(PATTERNS.VARIABLE_ASSIGNMENT);
			if (assignmentMatch && !trimmed.startsWith(':')) {
				const varName = assignmentMatch[1];
				const declaredIdentifiers = this.getDeclaredIdentifiers(lines, i);
				
				console.log(`[SSL Debug] Line ${i + 1}: Checking assignment '${varName}', declared identifiers: [${Array.from(declaredIdentifiers).join(', ')}]`);
				
				// Check if variable was declared before use
				if (!declaredIdentifiers.has(varName)) {
					const diagnostic = new vscode.Diagnostic(
						new vscode.Range(i, 0, i, varName.length),
						DIAGNOSTIC_MESSAGES.UNDECLARED_VARIABLE(varName),
						vscode.DiagnosticSeverity.Warning
					);
					diagnostic.code = DIAGNOSTIC_CODES.UNDECLARED_VARIABLE;
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
			// Also checks RHS of assignments: sVar := SomeFunc(sOtherVar);
			// Skip lines that are declarations or keywords
			if (trimmed && 
				!trimmed.startsWith(':') && 
				!trimmed.startsWith('/*') && 
				!trimmed.startsWith('*')) {
				
				const scopeInfo = this.getScopeInfo(lines, i);
				const declaredIdentifiers = scopeInfo.allIdentifiers;
				
				// For lines with assignments, only check the RHS for variable usage
				let textToCheck = trimmed;
				const assignmentMatch = trimmed.match(/^([a-z][a-zA-Z0-9_]*)\s*:=\s*(.+)$/);
				if (assignmentMatch) {
					// Only check the right-hand side of the assignment
					textToCheck = assignmentMatch[2];
				}
				
				// Extract potential variable references (Hungarian notation variables)
				// Match lowercase-starting identifiers (sVar, nCount, etc.) but not function calls
				const varReferences = textToCheck.match(/\b([a-z][a-zA-Z0-9_]*)\b/g);
				
				if (varReferences) {
					// Filter out known keywords and function names
					const sslKeywords = new Set(SSL_KEYWORDS.map(k => k.toLowerCase()));
					
					// Common SSL functions (lowercase versions)
					const sslFunctions = new Set(SSL_BUILTIN_FUNCTIONS.map(f => f.name.toLowerCase()));
					
					// Loop counter exceptions
					const loopCounters = new Set(['i', 'j', 'k', 'x', 'y', 'z']); // TODO: Move to constants
					
					varReferences.forEach(varRef => {
						const lowerRef = varRef.toLowerCase();
						
						// Skip keywords, functions, loop counters
						if (!sslKeywords.has(lowerRef) && 
							!sslFunctions.has(lowerRef) &&
							!loopCounters.has(lowerRef)) {
							
							// Check if this looks like a Hungarian notation variable
							if (/^[a-z][A-Z]/.test(varRef)) { // e.g., sVar, nCount
								const columnIndex = line.indexOf(varRef);
								if (columnIndex !== -1) {
									const isInLocalScope = scopeInfo.localIdentifiers.has(varRef);
									const isInGlobalScope = scopeInfo.globalIdentifiers.has(varRef);
									
									if (scopeInfo.inProcedure) {
										// Inside a procedure
										if (!isInLocalScope && !isInGlobalScope) {
											// Variable doesn't exist anywhere - ERROR
											const diagnostic = new vscode.Diagnostic(
												new vscode.Range(i, columnIndex, i, columnIndex + varRef.length),
												`Variable '${varRef}' is not declared. Add ':DECLARE ${varRef};' or pass it as a parameter.`,
												vscode.DiagnosticSeverity.Error
											);
											diagnostic.code = "ssl-undefined-variable";
											diagnostics.push(diagnostic);
											console.log(`[SSL Debug] ERROR: Undefined variable '${varRef}' at line ${i + 1}`);
										} else if (!isInLocalScope && isInGlobalScope) {
											// Variable exists globally but not declared locally - WARNING
											const diagnostic = new vscode.Diagnostic(
												new vscode.Range(i, columnIndex, i, columnIndex + varRef.length),
												`Procedure uses global variable '${varRef}' without declaring it locally. Declare it with ':DECLARE ${varRef};' or pass it as a parameter for better encapsulation.`,
												vscode.DiagnosticSeverity.Warning
											);
											diagnostic.code = "ssl-global-variable-in-procedure";
											diagnostics.push(diagnostic);
											console.log(`[SSL Debug] WARNING: Global variable used in procedure '${varRef}' at line ${i + 1}`);
										}
										// else: variable is in local scope, all good!
									} else {
										// Outside a procedure (global scope)
										if (!isInGlobalScope) {
											// Variable doesn't exist in global scope - ERROR
											const diagnostic = new vscode.Diagnostic(
												new vscode.Range(i, columnIndex, i, columnIndex + varRef.length),
												`Variable '${varRef}' is not declared in global scope. It may only exist inside a procedure.`,
												vscode.DiagnosticSeverity.Error
											);
											diagnostic.code = "ssl-undefined-variable";
											diagnostics.push(diagnostic);
											console.log(`[SSL Debug] ERROR: Undefined variable in global scope '${varRef}' at line ${i + 1}`);
										}
										// else: variable is in global scope, all good!
									}
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
						DIAGNOSTIC_MESSAGES.BLOCK_DEPTH_EXCEEDED(blockDepth, maxBlockDepth),
						strictMode ? vscode.DiagnosticSeverity.Error : vscode.DiagnosticSeverity.Warning
					);
					diagnostic.code = DIAGNOSTIC_CODES.BLOCK_DEPTH;
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
						DIAGNOSTIC_MESSAGES.MAX_PARAMS_EXCEEDED(procedureParams, maxParams),
						strictMode ? vscode.DiagnosticSeverity.Error : vscode.DiagnosticSeverity.Warning
					);
					diagnostic.code = DIAGNOSTIC_CODES.MAX_PARAMS;
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

			const endProcMatch = trimmed.match(/^:ENDPROC\b/i);
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
		return BLOCK_START_KEYWORDS.some(keyword => new RegExp(`^:${keyword}\\b`, 'i').test(line));
	}

	private isBlockEnd(line: string): boolean {
		return BLOCK_END_KEYWORDS.some(keyword => new RegExp(`^:${keyword}\\b`, 'i').test(line));
	}

	private isMultilineConstruct(line: string): boolean {
		// Check if line is part of a multi-line construct that doesn't need semicolon on every line
		// Or is a control flow keyword that ends with the keyword itself
		return MULTILINE_CONSTRUCT_KEYWORDS.some(keyword => new RegExp(`^:${keyword}\\b`, 'i').test(line));
	}

	private hasValidHungarianNotation(name: string): boolean {
		return hasValidHungarianNotation(name);
	}

	private getSeverity(severityString: string, strictMode: boolean): vscode.DiagnosticSeverity {
		if (strictMode) {
			return vscode.DiagnosticSeverity.Error;
		}

		switch (severityString.toLowerCase()) {
			case DIAGNOSTIC_SEVERITIES.ERROR:
				return vscode.DiagnosticSeverity.Error;
			case DIAGNOSTIC_SEVERITIES.WARNING:
				return vscode.DiagnosticSeverity.Warning;
			case DIAGNOSTIC_SEVERITIES.INFO:
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
			const paramsMatch = line.match(PATTERNS.PARAMETERS_DEFINITION);
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
			if (PATTERNS.PROCEDURE_DEFINITION.test(line)) {
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

			if (/^:ENDPROC\b/i.test(line)) {
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

	/**
	 * Get scope information at a given line including global/local identifiers and procedure state
	 */
	private getScopeInfo(lines: string[], currentLine: number): {
		globalIdentifiers: Set<string>;
		localIdentifiers: Set<string>;
		allIdentifiers: Set<string>;
		inProcedure: boolean;
	} {
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
			if (PATTERNS.PROCEDURE_DEFINITION.test(line)) {
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

			if (/^:ENDPROC\b/i.test(line)) {
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
		
		return {
			globalIdentifiers,
			localIdentifiers,
			allIdentifiers,
			inProcedure
		};
	}
}

