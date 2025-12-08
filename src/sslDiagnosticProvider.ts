import * as vscode from "vscode";
import {
	SSL_KEYWORDS,
	BLOCK_START_KEYWORDS,
	BLOCK_END_KEYWORDS,
	BLOCK_MIDDLE_KEYWORDS,
	CASE_KEYWORDS,
	LOOP_COUNTER_EXCEPTIONS
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
	DIAGNOSTIC_MESSAGES
} from "./constants/diagnostics";
import { hasValidHungarianNotation } from "./constants/hungarian";
import { Logger } from "./utils/logger";
import { getConfiguredFunctions } from "./utils/intellisense";

// --- Types & Interfaces ---

interface VariableDefinition {
	name: string;
	line: number;
	column: number;
	literalValue?: string; // For static analysis of content
}

interface ProcedureScope {
	name: string;
	startLine: number;
	endLine?: number; // Optional while parsing
	variables: Map<string, VariableDefinition>;
	parameters: VariableDefinition[];
}

interface BlockStart {
	keyword: string;
	line: number;
}

interface DocumentAnalysis {
	procedures: ProcedureScope[];
	globalVariables: Map<string, VariableDefinition>;
	lines: string[]; // Raw lines for easy access
}

interface ValidationConfig {
	maxProblems: number;
	strictMode: boolean;
	hungarianEnabled: boolean;
	hungarianSeverity: string;
	preventSqlInjection: boolean;
	requireParameterized: boolean;
	enforceKeywordCase: boolean;
	enforceCommentSyntax: boolean;
	maxBlockDepth: number;
	maxParams: number;
	configuredGlobals: Set<string>;
	configuredFunctions: Set<string>;
	namespaceAliases: Set<string>;
}

// --- Validator Class ---

export class SSLDiagnosticProvider {

	private diagnosticCollection: vscode.DiagnosticCollection;

	constructor() {
		this.diagnosticCollection = vscode.languages.createDiagnosticCollection("ssl");
	}

	public updateDiagnostics(document: vscode.TextDocument): void {
		const diagnostics: vscode.Diagnostic[] = [];
		const config = this.getConfiguration();

		// 1. Analyze Document (Single Pass)
		const analysis = this.analyzeDocument(document);

		// 2. Validate using Analysis Context
		this.validateBlocksAndStyle(analysis, config, diagnostics);
		this.validateVariables(analysis, config, diagnostics);
		this.validateSqlAndExec(analysis, config, diagnostics);

		this.diagnosticCollection.set(document.uri, diagnostics);
	}

	public dispose(): void {
		this.diagnosticCollection.dispose();
	}

	public clear(): void {
		this.diagnosticCollection.clear();
	}

	// --- Configuration Helper ---

	private getConfiguration(): ValidationConfig {
		const config = vscode.workspace.getConfiguration("ssl");
		// Using GLOBAL_VARIABLES as the correct key based on context, previously INTELLISENSE_GLOBAL_IDENTIFIERS was used incorrectly
		const globals = config.get<string[]>(CONFIG_KEYS.GLOBAL_VARIABLES) || [];
		const namespaceRoots = config.get<Record<string, string>>(CONFIG_KEYS.DOCUMENT_NAMESPACES) || {};
		const funcs = getConfiguredFunctions(config).map(f => f.name.toLowerCase());

		return {
			maxProblems: config.get(CONFIG_KEYS.MAX_NUMBER_OF_PROBLEMS, 100),
			strictMode: config.get(CONFIG_KEYS.STRICT_STYLE_GUIDE_MODE, false),
			hungarianEnabled: config.get(CONFIG_KEYS.NAMING_HUNGARIAN_ENABLED, false),
			hungarianSeverity: config.get(CONFIG_KEYS.NAMING_HUNGARIAN_SEVERITY, "warning"),
			preventSqlInjection: config.get(CONFIG_KEYS.SECURITY_PREVENT_SQL_INJECTION, true),
			requireParameterized: config.get(CONFIG_KEYS.SECURITY_REQUIRE_PARAMETERIZED_QUERIES, true),
			enforceKeywordCase: config.get(CONFIG_KEYS.STYLE_GUIDE_ENFORCE_KEYWORD_CASE, false),
			enforceCommentSyntax: config.get(CONFIG_KEYS.STYLE_GUIDE_ENFORCE_COMMENT_SYNTAX, true),
			maxBlockDepth: config.get(CONFIG_KEYS.STYLE_GUIDE_LIMIT_BLOCK_DEPTH, 0),
			maxParams: config.get(CONFIG_KEYS.STYLE_GUIDE_MAX_PARAMS_PER_PROCEDURE, 0),
			configuredGlobals: new Set(globals.map(g => g.toLowerCase())),
			configuredFunctions: new Set(funcs),
			namespaceAliases: new Set(Object.keys(namespaceRoots).map(k => k.toLowerCase()))
		};
	}

	// --- Analysis Phase ---

	private analyzeDocument(document: vscode.TextDocument): DocumentAnalysis {
		const lines = document.getText().split(/\r?\n/);
		const analysis: DocumentAnalysis = {
			procedures: [],
			globalVariables: new Map(),
			lines: lines
		};

		let currentProcedure: ProcedureScope | null = null;
		let inCommentBlock = false;

		for (let i = 0; i < lines.length; i++) {
			const line = lines[i];
			const trimmed = line.trim();

			// --- Comment State Tracking ---
			if (inCommentBlock) {
				// SSL uses /* ... ; syntax. The ; ends the comment block.
				if (trimmed.endsWith(';')) {
					inCommentBlock = false;
				}
				continue;
			}
			if (trimmed.startsWith('/*')) {
				if (!trimmed.endsWith(';')) {
					inCommentBlock = true;
				}
				// Even single-line /* ... ; is a comment, skip parsing content
				continue;
			}
			if (trimmed.startsWith('*') || trimmed.startsWith('//')) {
				continue;
			}
			// --- End Comment State Tracking ---

			// Procedure Detection
			const procMatch = trimmed.match(/^:PROCEDURE\s+(\w+)/i);
			if (procMatch) {
				if (currentProcedure) {
					currentProcedure.endLine = i - 1; // Implicit close
				}
				currentProcedure = {
					name: procMatch[1],
					startLine: i,
					variables: new Map(),
					parameters: []
				};
				analysis.procedures.push(currentProcedure);
			}
			else if (/^:ENDPROC\b/i.test(trimmed)) {
				if (currentProcedure) {
					currentProcedure.endLine = i;
					currentProcedure = null;
				}
			}

			// Variable Declaration detection (:DECLARE or :PARAMETERS)
			// Note: simple regex for now, handles basic cases
			const declMatch = trimmed.match(/^:(DECLARE|PARAMETERS)\s+(.+?);/i);
			if (declMatch) {
				const type = declMatch[1].toUpperCase();
				const varList = declMatch[2].split(',');

				varList.forEach(v => {
					const vTrim = v.trim();
					const name = vTrim.split(/\s+/)[0]; // simplistic parse
					if (name) {
						const def: VariableDefinition = {
							name: name,
							line: i,
							column: line.indexOf(name)
						};

						if (currentProcedure) {
							if (type === 'PARAMETERS') {
								currentProcedure.parameters.push(def);
							}
							// Parameters are also local variables
							currentProcedure.variables.set(name.toLowerCase(), def);
						} else {
							// Global if not in procedure (or PARAMETERS outside procedure is odd but syntactically possible?)
							// treating as global
							analysis.globalVariables.set(name.toLowerCase(), def);
						}
					}
				});
			}

			// Capture basic string assignments for SQL analysis
			const assignMatch = trimmed.match(/^([a-z][a-z0-9_]*)\s*:=\s*(["'].*["']);?$/i);
			if (assignMatch) {
				const varName = assignMatch[1];
				const value = assignMatch[2]; // Includes quotes

				// Helper to update definition
				const updateDef = (map: Map<string, VariableDefinition>) => {
					const def = map.get(varName.toLowerCase());
					if (def) {
						// Store literal without outer quotes (simplistic)
						def.literalValue = value.substring(1, value.length - 1);
					}
				};

				if (currentProcedure) {
					updateDef(currentProcedure.variables);
				} else {
					updateDef(analysis.globalVariables);
				}
			}
		}

		// Close last procedure if open
		if (currentProcedure && !currentProcedure.endLine) {
			currentProcedure.endLine = lines.length - 1;
		}

		return analysis;
	}

	// --- Validation Phase 1: Blocks & Style ---

	private validateBlocksAndStyle(analysis: DocumentAnalysis, config: ValidationConfig, diagnostics: vscode.Diagnostic[]) {
		const blockStack: BlockStart[] = [];
		let inCommentBlock = false;

		const BLOCK_PAIRS: Record<string, string> = {
			'IF': 'ENDIF', 'WHILE': 'ENDWHILE', 'FOR': 'NEXT',
			'BEGINCASE': 'ENDCASE', 'TRY': 'ENDTRY', 'PROCEDURE': 'ENDPROC',
			'REGION': 'ENDREGION', 'BEGININLINECODE': 'ENDINLINECODE'
		};
		const BLOCK_MIDDLE = new Set(['ELSE', 'CATCH', 'FINALLY', 'CASE', 'OTHERWISE', 'TO', 'STEP']);

		for (let i = 0; i < analysis.lines.length; i++) {
			if (diagnostics.length >= config.maxProblems) { break; }


			const line = analysis.lines[i];
			const trimmed = line.trim();
			if (!trimmed) {
				continue;
			}

			// Comment            if (inCommentBlock) {
			if (inCommentBlock) {
				if (trimmed.endsWith(';')) {
					inCommentBlock = false;
				}
				continue;
			}
			if (trimmed.startsWith('/*')) {
				if (!trimmed.endsWith(';')) {
					inCommentBlock = true;
				} else {
					// Single line block comment check style
					if (config.enforceCommentSyntax && PATTERNS.COMMENT.INVALID.test(trimmed) && !trimmed.includes(';')) {
						// This case (starts /* ends */ no ;) is handled by logic above?
						// Wait, logic above says: starts /*, checks endsWith ;
						// If user wrote /* ... */ it fails endsWith(';') check -> enters inCommentBlock = true.
						// So this validation needs to be smarter or run before state change.
					}
				}
				// Check for invalid syntax: /* ... */ without semicolon
				if (config.enforceCommentSyntax && PATTERNS.COMMENT.INVALID.test(line) && !line.includes(';')) {
					diagnostics.push(new vscode.Diagnostic(
						new vscode.Range(i, 0, i, line.length),
						"Invalid SSL comment syntax: Comments should use /* ... ; (semicolon terminator, not */)",
						vscode.DiagnosticSeverity.Warning
					));
				}
				continue;
			}
			if (trimmed.startsWith('*') || trimmed.startsWith('//')) {
				continue;
			}

			// Keyword Validation
			const keywordMatch = trimmed.match(/^:([A-Za-z]+)\b/);
			if (keywordMatch) {
				const rawKeyword = keywordMatch[1];
				const keyword = rawKeyword.toUpperCase();

				// Enforce Case
				if (config.enforceKeywordCase && rawKeyword !== keyword) {
					diagnostics.push(this.createDiagnostic(i, line.indexOf(rawKeyword), rawKeyword.length,
						`Keyword should be UPPERCASE: :${keyword}`, vscode.DiagnosticSeverity.Warning, "ssl-keyword-case"));
				}

				// Block Matching
				if (BLOCK_PAIRS[keyword]) {
					blockStack.push({ keyword, line: i });
				}
				else if (Object.values(BLOCK_PAIRS).includes(keyword)) {
					// It is an End Block
					const last = blockStack[blockStack.length - 1];
					if (!last) {
						diagnostics.push(this.createDiagnostic(i, 0, line.length,
							DIAGNOSTIC_MESSAGES.UNMATCHED_BLOCK_END(keyword, "START"), vscode.DiagnosticSeverity.Error, DIAGNOSTIC_CODES.UNMATCHED_BLOCK_END));
					} else if (BLOCK_PAIRS[last.keyword] !== keyword) {
						diagnostics.push(this.createDiagnostic(i, 0, line.length,
							DIAGNOSTIC_MESSAGES.MISMATCHED_BLOCK_END(keyword, BLOCK_PAIRS[last.keyword], last.keyword), vscode.DiagnosticSeverity.Error, DIAGNOSTIC_CODES.MISMATCHED_BLOCK_END));
					} else {
						blockStack.pop();
					}
				}

				// Max Depth
				if (config.maxBlockDepth > 0 && blockStack.length > config.maxBlockDepth) {
					diagnostics.push(this.createDiagnostic(i, 0, line.length,
						DIAGNOSTIC_MESSAGES.BLOCK_DEPTH_EXCEEDED(blockStack.length, config.maxBlockDepth), vscode.DiagnosticSeverity.Warning, DIAGNOSTIC_CODES.BLOCK_DEPTH));
				}
			}

			// Missing Semicolon Check
			const isStructure = blockStack.some(b => b.line === i) || (keywordMatch && BLOCK_MIDDLE.has(keywordMatch[1].toUpperCase()));

			let isContinuation = /[+\-*/,{(]$/.test(trimmed) ||
				/^[+\-*/,})]/.test(trimmed) ||
				/^(\.AND\.|\.OR\.|\.NOT\.)/i.test(trimmed);

			// Look ahead for continuation if not found
			if (!isContinuation && !trimmed.endsWith(';') && i + 1 < analysis.lines.length) {
				const nextLine = analysis.lines[i + 1].trim();
				if (/^[+\-*/,})]/.test(nextLine) || /^(\.AND\.|\.OR\.|\.NOT\.)/i.test(nextLine)) {
					isContinuation = true;
				}
			}

			if (!trimmed.endsWith(';') && !isStructure && !isContinuation && !trimmed.startsWith(':')) {
				diagnostics.push(this.createDiagnostic(i, line.length - 1, 1, "Statement should end with semicolon", vscode.DiagnosticSeverity.Warning, "ssl-missing-semicolon"));
			}
		}
	}

	// --- Validation Phase 2: Variables ---

	private validateVariables(analysis: DocumentAnalysis, config: ValidationConfig, diagnostics: vscode.Diagnostic[]) {
		const reported = new Set<string>();

		for (let i = 0; i < analysis.lines.length; i++) {
			if (diagnostics.length >= config.maxProblems) {
				break;
			}
			const line = analysis.lines[i];
			const trimmed = line.trim();
			if (!trimmed || trimmed.startsWith('//') || trimmed.startsWith('/*') || trimmed.startsWith(':')) {
				continue;
			}

			// Mask strings to avoid false positives for variables inside quotes
			// Replace with spaces to preserve indices
			const lineWithoutStrings = line.replace(/(["'])(?:(?=(\\?))\2.)*?\1/g, (m) => ' '.repeat(m.length));

			// Determine Scope
			const scope = analysis.procedures.find(p => i >= p.startLine && i <= (p.endLine || Infinity));
			const locals = scope ? scope.variables : new Map<string, VariableDefinition>();
			const globals = analysis.globalVariables;

			// Check Assignments & Usages
			// Regex to find variable-like identifiers
			const identifierPattern = /\b([a-z][a-zA-Z0-9_]*)\b/g;
			let match;
			while ((match = identifierPattern.exec(lineWithoutStrings)) !== null) {
				const varName = match[1];
				const lowerName = varName.toLowerCase();

				// Skip knowns
				if (SSL_KEYWORDS.includes(varName.toUpperCase()) ||
					config.configuredFunctions.has(lowerName) ||
					LOOP_COUNTER_EXCEPTIONS.includes(lowerName)) {
					continue;
				}

				// Skip if property access (Prop:Name or :Name)
				const charBefore = lineWithoutStrings.charAt(match.index - 1);
				if (charBefore === ':') {
					continue;
				}


				// Check Declaration
				const isDeclared = locals.has(lowerName) || globals.has(lowerName) || config.configuredGlobals.has(lowerName);

				if (!isDeclared && !reported.has(lowerName)) {
					diagnostics.push(this.createDiagnostic(i, match.index, varName.length,
						DIAGNOSTIC_MESSAGES.UNDEFINED_VARIABLE(varName), vscode.DiagnosticSeverity.Warning, DIAGNOSTIC_CODES.UNDEFINED_VARIABLE));
					reported.add(lowerName);
				}

				// Hungarian Notation
				if (config.hungarianEnabled && hasValidHungarianNotation && !hasValidHungarianNotation(varName)) {
					// Need access to wrapper or utility? Using imported one.
					// Note: imported hasValidHungarianNotation checks if it matches pattern.
					const severity = config.strictMode ? vscode.DiagnosticSeverity.Error : vscode.DiagnosticSeverity.Warning;
					diagnostics.push(this.createDiagnostic(i, match.index, varName.length,
						`Variable '${varName}' should use Hungarian notation`, severity, "ssl-hungarian-notation"));
				}
			}

			// Check specific assignments for Declarations?
			// The analyzer already parsed declarations. Here we check loose usage.
		}
	}

	// --- Validation Phase 3: SQL & Exec ---

	private validateSqlAndExec(analysis: DocumentAnalysis, config: ValidationConfig, diagnostics: vscode.Diagnostic[]) {
		for (let i = 0; i < analysis.lines.length; i++) {
			if (diagnostics.length >= config.maxProblems) {
				break;
			}
			const line = analysis.lines[i];

			// Check SQL Injection
			if (config.preventSqlInjection && /(SQLExecute|RunSQL|LSearch)/i.test(line)) {
				// Check for concatenations: "SELECT ... " + var
				if (/['"]\s*\+\s*\w+/.test(line) && !/\?[^?]+\?/.test(line)) {
					diagnostics.push(this.createDiagnostic(i, 0, line.length,
						"Potential SQL injection: Use parameterized queries", vscode.DiagnosticSeverity.Warning, "sql-positional-injection"));
				}

				// Check SQL Placeholder Style
				// RunSQL -> Positional '?' (legacy support for named exists but warning expected by tests)
				// Test 'warns when RunSQL uses named placeholders' implies named are invalid for RunSQL style preference
				// SQLExecute -> Named '?name?'

				// Check RunSQL calls
				const runSqlMatch = /RunSQL\s*\(\s*([^,]+)/i.exec(line);
				if (runSqlMatch) {
					this.validateSqlPlaceholders(i, runSqlMatch[1].trim(), 'RunSQL', analysis, diagnostics);
				}

				// Check SQLExecute calls
				const sqlExecMatch = /SQLExecute\s*\(\s*([^,]+)/i.exec(line);
				if (sqlExecMatch) {
					this.validateSqlPlaceholders(i, sqlExecMatch[1].trim(), 'SQLExecute', analysis, diagnostics);
				}


			}

			// Check for Invalid SQL Params (?Unknown?)
			// This applies to ANY string that looks like SQL
			// Simple heuristic: If string contains keywords SELECT/UPDATE/INSERT/DELETE
			// And we are NOT preventing SQL injection (or we are, but this is separate check?)
			// Ideally we always check this if strictly validating.
			// But maybe guard with `preventSqlInjection` or `strictMode`?
			// Tests expect it always?
			if (/(SELECT|UPDATE|INSERT|DELETE)\b/i.test(line)) {
				this.validateSqlParams(i, line, analysis, diagnostics);
			}

			// Check ExecFunction Targets

			// Check ExecFunction Targets
			const execMatch = /ExecFunction\s*\(\s*["']([^"']+)["']/i.exec(line);
			if (execMatch) {
				const target = execMatch[1];
				const parts = target.split('.');

				let isValid = false;
				if (parts.length >= 3) {
					isValid = true; // Namespace.Script.Proc
				} else if (parts.length === 2) {
					// Script.Proc or Namespace.Script (Invalid)
					// Check if first part is a known namespace alias
					if (config.namespaceAliases.has(parts[0].toLowerCase())) {
						isValid = false; // Missing Procedure (Alias.Script)
					} else {
						isValid = true; // Script.Proc
					}
				} else {
					isValid = false; // incomplete
				}

				if (!isValid) {
					// Usually requires Script.Proc
					diagnostics.push(this.createDiagnostic(i, execMatch.index!, execMatch[0].length,
						DIAGNOSTIC_MESSAGES.INVALID_EXEC_TARGET(target, "Script.Procedure"), vscode.DiagnosticSeverity.Error, DIAGNOSTIC_CODES.INVALID_EXEC_TARGET));
				}
			}
		}
	}

	// --- Helpers ---

	private validateSqlPlaceholders(lineIdx: number, arg: string, funcName: string, analysis: DocumentAnalysis, diagnostics: vscode.Diagnostic[]) {
		let sqlString = '';
		if (arg.startsWith('"') || arg.startsWith("'")) {
			sqlString = arg.substring(1, arg.length - 1);
		} else {
			// Resolve variable
			const scope = analysis.procedures.find(p => lineIdx >= p.startLine && lineIdx <= (p.endLine || Infinity));
			const locals = scope ? scope.variables : new Map<string, VariableDefinition>();
			const def = locals.get(arg.toLowerCase()) || analysis.globalVariables.get(arg.toLowerCase());
			if (def && def.literalValue) {
				sqlString = def.literalValue;
			}
		}

		if (!sqlString) { return; }

		// Heuristic: Check for placeholders
		const hasNamed = /\?[a-zA-Z0-9_]+\?/.test(sqlString);
		const hasPositional = /\?/.test(sqlString.replace(/\?[a-zA-Z0-9_]+\?/g, '')); // Check for ? NOT inside named

		if (funcName === 'RunSQL') {
			if (hasNamed) {
				diagnostics.push(this.createDiagnostic(lineIdx, 0, 1,
					"RunSQL should use positional placeholders '?'", vscode.DiagnosticSeverity.Warning, "ssl-invalid-sql-placeholder-style"));
			}
		} else if (funcName === 'SQLExecute') {
			if (hasPositional) {
				diagnostics.push(this.createDiagnostic(lineIdx, 0, 1,
					"SQLExecute should use named placeholders '?param?'", vscode.DiagnosticSeverity.Warning, "ssl-invalid-sql-placeholder-style"));
			}
		}
	}

	private validateSqlParams(lineIdx: number, line: string, analysis: DocumentAnalysis, diagnostics: vscode.Diagnostic[]) {
		const paramPattern = /\?([a-zA-Z0-9_]+)\?/g;
		let match;
		while ((match = paramPattern.exec(line)) !== null) {
			const paramName = match[1];
			// Check if declared
			const scope = analysis.procedures.find(p => lineIdx >= p.startLine && lineIdx <= (p.endLine || Infinity));
			const locals = scope ? scope.variables : new Map<string, VariableDefinition>();
			const lowerName = paramName.toLowerCase();

			const isDeclared = locals.has(lowerName) || analysis.globalVariables.has(lowerName);

			if (!isDeclared) {
				diagnostics.push(this.createDiagnostic(lineIdx, match.index, match[0].length,
					DIAGNOSTIC_MESSAGES.INVALID_SQL_PARAM(paramName), vscode.DiagnosticSeverity.Warning, DIAGNOSTIC_CODES.INVALID_SQL_PARAM));
			}
		}
	}

	private createDiagnostic(line: number, col: number, len: number, msg: string, severity: vscode.DiagnosticSeverity, code: string): vscode.Diagnostic {
		const d = new vscode.Diagnostic(new vscode.Range(line, col, line, col + len), msg, severity);
		d.code = code;
		return d;
	}

	private getSeverity(configSeverity: string, strict: boolean): vscode.DiagnosticSeverity {
		if (strict) { return vscode.DiagnosticSeverity.Error; }
		return configSeverity === 'error' ? vscode.DiagnosticSeverity.Error :
			configSeverity === 'information' ? vscode.DiagnosticSeverity.Information :
				vscode.DiagnosticSeverity.Warning;
	}
}
