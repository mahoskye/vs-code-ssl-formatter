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
			if (diagnostics.length >= config.maxProblems) break;
			if (diagnostics.length >= config.maxProblems) {
				break;
			}

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
			const isStructure = blockStack.some(b => b.line === i) || BLOCK_MIDDLE.has(keywordMatch?.[1].toUpperCase() || "");
			const isContinuation = /[+\-*/,]$/.test(trimmed) || /^[+\-*/,]/.test(trimmed);

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

			// Determine Scope
			const scope = analysis.procedures.find(p => i >= p.startLine && i <= (p.endLine || Infinity));
			const locals = scope ? scope.variables : new Map<string, VariableDefinition>();
			const globals = analysis.globalVariables;

			// Check Assignments & Usages
			// Regex to find variable-like identifiers
			const identifierPattern = /\b([a-z][a-zA-Z0-9_]*)\b/g;
			let match;
			while ((match = identifierPattern.exec(line)) !== null) {
				const varName = match[1];
				const lowerName = varName.toLowerCase();

				// Skip knowns
				if (SSL_KEYWORDS.includes(varName.toUpperCase()) ||
					config.configuredFunctions.has(lowerName) ||
					LOOP_COUNTER_EXCEPTIONS.includes(lowerName)) {
					continue;
				}

				// Skip if property access (Prop:Name or :Name) - simplistic check
				const charBefore = line.charAt(match.index - 1);
				if (charBefore === ':') {
					continue;
				}

				// Check Declaration
				const isDeclared = locals.has(lowerName) || globals.has(lowerName) || config.configuredGlobals.has(lowerName);

				if (!isDeclared && !reported.has(lowerName)) {
					diagnostics.push(this.createDiagnostic(i, match.index, varName.length,
						DIAGNOSTIC_MESSAGES.UNDECLARED_VARIABLE(varName), vscode.DiagnosticSeverity.Warning, DIAGNOSTIC_CODES.UNDECLARED_VARIABLE));
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
			}

			// Check ExecFunction Targets
			const execMatch = /ExecFunction\s*\(\s*["']([^"']+)["']/i.exec(line);
			if (execMatch) {
				const target = execMatch[1];
				const parts = target.split('.');
				// Check standard format: [Namespace.]Script.Proc
				const valid = parts.length >= 2 || (parts.length === 1 && config.namespaceAliases.size === 0); // relaxed check
				if (!valid && parts.length < 2) {
					// Usually requires Script.Proc
					diagnostics.push(this.createDiagnostic(i, execMatch.index!, execMatch[0].length,
						DIAGNOSTIC_MESSAGES.INVALID_EXEC_TARGET(target, "Script.Procedure"), vscode.DiagnosticSeverity.Error, DIAGNOSTIC_CODES.INVALID_EXEC_TARGET));
				}
			}
		}
	}

	// --- Helpers ---

	private createDiagnostic(line: number, col: number, len: number, msg: string, severity: vscode.DiagnosticSeverity, code: string): vscode.Diagnostic {
		const d = new vscode.Diagnostic(new vscode.Range(line, col, line, col + len), msg, severity);
		d.code = code;
		return d;
	}

	private getSeverity(configSeverity: string, strict: boolean): vscode.DiagnosticSeverity {
		if (strict) return vscode.DiagnosticSeverity.Error;
		return configSeverity === 'error' ? vscode.DiagnosticSeverity.Error :
			configSeverity === 'information' ? vscode.DiagnosticSeverity.Information :
				vscode.DiagnosticSeverity.Warning;
	}
}
