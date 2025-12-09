import * as vscode from "vscode";
import {
	SSL_KEYWORDS,
	BLOCK_START_KEYWORDS,
	BLOCK_END_KEYWORDS,
	BLOCK_MIDDLE_KEYWORDS,
	CASE_KEYWORDS,
	LOOP_COUNTER_EXCEPTIONS,
	SSL_BUILTIN_FUNCTIONS
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
	assignments: { line: number, value: string }[]; // Track history of assignments
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
		this.validateFunctionCalls(analysis, config, diagnostics);

		this.diagnosticCollection.set(document.uri, diagnostics);
	}

	public dispose(): void {
		this.diagnosticCollection.dispose();
	}

	public clear(): void {
		this.diagnosticCollection.clear();
	}

	public removeDiagnostics(uri: vscode.Uri): void {
		this.diagnosticCollection.delete(uri);
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
		let inDeclarationBlock = false;
		let currentDeclType = "";

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
			// // is NOT a valid comment in SSL, so we do not skip it
			// Next steps will handle it or flag it

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
			// Variable Declaration detection (:DECLARE or :PARAMETERS or :PUBLIC)
			// Handle multi-line declarations by accumulating until semicolon
			// First, check for start of declaration
			let declMatch = trimmed.match(/^:(DECLARE|PARAMETERS|PUBLIC)\s+(.*)/i);
			let isNewDecl = !!declMatch;

			if (isNewDecl || inDeclarationBlock) {
				let contentToParse = "";

				if (isNewDecl) {
					// fresh start
					currentDeclType = declMatch![1].toUpperCase();
					contentToParse = declMatch![2];
					inDeclarationBlock = true;
				} else {
					// continuation
					contentToParse = trimmed;
				}

				// Check for end of declaration
				if (contentToParse.includes(';')) {
					inDeclarationBlock = false;
					// Parse up to semicolon
					contentToParse = contentToParse.substring(0, contentToParse.indexOf(';'));
				}

				// Parse variables in this chunk
				const varList = contentToParse.split(',');
				varList.forEach(v => {
					const vTrim = v.trim();
					if (!vTrim) { return; }

					const name = vTrim.split(/\s+/)[0]; // simplistic parse
					if (name) {
						const def: VariableDefinition = {
							name: name,
							line: i,
							column: line.indexOf(name),
							assignments: []
						};

						if (currentProcedure) {
							if (currentDeclType === 'PARAMETERS') {
								currentProcedure.parameters.push(def);
							}
							currentProcedure.variables.set(name.toLowerCase(), def);
						} else {
							analysis.globalVariables.set(name.toLowerCase(), def);
						}
					}
				});
			}

			// Capture basic string assignments for SQL analysis
			// Capture string assignments (flow sensitive)
			const assignMatch = trimmed.match(/^([a-z][a-z0-9_]*)\s*:=\s*(.+);?$/i);
			if (assignMatch) {
				const varName = assignMatch[1];
				const rhs = assignMatch[2];

				// Extract all string literals from RHS
				let constructedString = "";
				const stringPattern = /(?:'((?:[^'\\]|\\.)*)')|(?:"((?:[^"\\]|\\.)*)")/g;
				let match;
				while ((match = stringPattern.exec(rhs)) !== null) {
					// Group 1 is single quoted, Group 2 is double quoted
					constructedString += match[1] !== undefined ? match[1] : match[2];
				}

				if (constructedString) {
					// Helper to update definition
					const updateDef = (map: Map<string, VariableDefinition>) => {
						const def = map.get(varName.toLowerCase());
						if (def) {
							if (!def.assignments) { def.assignments = []; }
							def.assignments.push({ line: i, value: constructedString });
							// Also update literalValue for legacy fallback (last assignment wins)
							def.literalValue = constructedString;
						}
					};

					if (currentProcedure) {
						updateDef(currentProcedure.variables);
					} else {
						updateDef(analysis.globalVariables);
					}
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
		let inStringQuote: string | null = null; // Track if we are inside a multiline string

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

			// --- Multiline String Tracking ---
			// We need to know if the line ends while still inside a string.
			// Simple parser for quotes.
			for (let charIndex = 0; charIndex < line.length; charIndex++) {
				const char = line[charIndex];
				if (inCommentBlock) { break; } // Don't parse quotes in comments (simplified)

				if (inStringQuote) {
					if (char === inStringQuote) {
						// Check for escaped quote? simple check for now
						// In SSL " "" " is escape? or \" ? Assuming standard or simple.
						// Lexer handles escapes. Here we do simple toggle.
						inStringQuote = null;
					}
				} else {
					if (char === '"' || char === "'") {
						inStringQuote = char;
					} else if (char === '/' && line[charIndex + 1] === '*') {
						// Start of comment block inside line? Handled by line check below usually
						// But strictly we should track it here too if mixing.
						// Keeping it simple: If line starts with /* we handle it.
					}
				}
			}

			// If we are inside a string at the end of the line, skip block/style checks for this line
			if (inStringQuote) {
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
			// Invalid Comment Check (Double Slash)
			if (trimmed.startsWith('//')) {
				diagnostics.push(new vscode.Diagnostic(
					new vscode.Range(i, 0, i, line.length),
					"Invalid syntax: '//' is not a valid comment. Use '/* ... ;'.",
					vscode.DiagnosticSeverity.Error
				));
				continue;
			}

			// Legacy Comment Check
			if (trimmed.startsWith('*')) {
				diagnostics.push(new vscode.Diagnostic(
					new vscode.Range(i, 0, i, line.length),
					"Invalid syntax: '*' at start of line is not a valid comment. Use '/* ... ;'.",
					vscode.DiagnosticSeverity.Error
				));
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
		let inCommentBlock = false;
		let inStringQuote: string | null = null;

		for (let i = 0; i < analysis.lines.length; i++) {
			if (diagnostics.length >= config.maxProblems) {
				break;
			}
			const line = analysis.lines[i];
			const trimmed = line.trim();

			// --- Comment Tracking ---
			if (inCommentBlock) {
				if (trimmed.endsWith(';')) {
					inCommentBlock = false;
				}
				continue;
			}
			if (trimmed.startsWith('/*')) {
				if (!trimmed.endsWith(';')) {
					inCommentBlock = true;
				}
				continue; // Skip comment start line
			}
			if (trimmed.startsWith('*') || trimmed.startsWith('//')) {
				continue;
			}
			if (!trimmed || trimmed.startsWith(':')) {
				continue;
			}

			// --- String Masking (Support Multiline) ---
			let maskedLine = "";
			for (let j = 0; j < line.length; j++) {
				const char = line[j];

				if (inStringQuote) {
					maskedLine += " "; // Mask content
					if (char === inStringQuote) {
						inStringQuote = null;
					}
				} else {
					if (char === '"' || char === "'") {
						inStringQuote = char;
						maskedLine += " "; // Mask quote too to avoid partial matches
					} else {
						maskedLine += char;
					}
				}
			}

			const lineWithoutStrings = maskedLine;

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

				const scopeKey = scope ? `${scope.name.toLowerCase()}:${lowerName}` : lowerName;

				if (!isDeclared && !reported.has(scopeKey)) {
					diagnostics.push(this.createDiagnostic(i, match.index, varName.length,
						DIAGNOSTIC_MESSAGES.UNDEFINED_VARIABLE(varName), vscode.DiagnosticSeverity.Warning, DIAGNOSTIC_CODES.UNDEFINED_VARIABLE));
					reported.add(scopeKey);
				}

				// Hungarian Notation
				if (config.hungarianEnabled && hasValidHungarianNotation && !hasValidHungarianNotation(varName)) {
					const severity = config.strictMode ? vscode.DiagnosticSeverity.Error : vscode.DiagnosticSeverity.Warning;
					diagnostics.push(this.createDiagnostic(i, match.index, varName.length,
						`Variable '${varName}' should use Hungarian notation`, severity, "ssl-hungarian-notation"));
				}
			}
		}
	}

	// --- Validation Phase 3: SQL & Exec ---

	private validateSqlAndExec(analysis: DocumentAnalysis, config: ValidationConfig, diagnostics: vscode.Diagnostic[]) {
		let inCommentBlock = false;

		for (let i = 0; i < analysis.lines.length; i++) {
			if (diagnostics.length >= config.maxProblems) {
				break;
			}
			const line = analysis.lines[i];
			const trimmed = line.trim();

			// --- Comment Tracking (Added) ---
			if (inCommentBlock) {
				if (trimmed.endsWith(';')) {
					inCommentBlock = false;
				}
				continue;
			}
			if (trimmed.startsWith('/*')) {
				if (!trimmed.endsWith(';')) {
					inCommentBlock = true;
				}
				continue;
			}
			if (trimmed.startsWith('*') || trimmed.startsWith('//')) {
				continue;
			}

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
				const runSqlCallMatch = /RunSQL\s*\((.+)\)/i.exec(line);
				if (runSqlCallMatch) {
					const args = this.parseArguments(runSqlCallMatch[1]);
					const sqlArg = args[0] || "";

					// Validate Style
					this.validateSqlPlaceholders(i, sqlArg.trim(), 'RunSQL', analysis, diagnostics);

					// Validate Params Count
					// Resolve SQL to check for placeholders
					const sqlString = this.resolveSqlString(i, sqlArg.trim(), analysis);
					if (sqlString && /\?/.test(sqlString)) {
						// If placeholders exist (? or ?name?), proper params array is required (3rd argument)
						// RunSQL(sql, name, params)
						if (args.length < 3) {
							diagnostics.push(this.createDiagnostic(i, runSqlCallMatch.index!, runSqlCallMatch[0].length,
								"RunSQL requires parameters argument when placeholders are used", vscode.DiagnosticSeverity.Error, "ssl-runsql-missing-params"));
						}
					}
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
			const execMatch = /ExecFunction\s*\(\s*["']([^"']*)["']/i.exec(line);
			if (execMatch) {
				const target = execMatch[1];
				const parts = target.split('.');

				let isValid = false;
				if (parts.length >= 2) {
					isValid = true; // Namespace.Script.Proc OR Script.Proc (Valid enough)
				} else if (parts.length === 1 && parts[0].length > 0) {
					isValid = true; // Procedure only (implicit current script/global)
				} else {
					isValid = false; // Empty?
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

	private isInsideString(line: string, index: number): boolean {
		let insideString = false;
		let quoteChar = '';

		for (let i = 0; i < line.length; i++) {
			if (i === index) {
				return insideString;
			}
			const char = line[i];
			if (insideString) {
				if (char === quoteChar && line[i - 1] !== '\\') {
					insideString = false;
				}
			} else {
				if (char === '"' || char === "'") {
					insideString = true;
					quoteChar = char;
				}
			}
		}
		return false;
	}

	private parseArguments(argsString: string): string[] {
		const args: string[] = [];
		let current = "";
		let depth = 0;
		let inQuote = false;
		let quoteChar = "";

		for (let i = 0; i < argsString.length; i++) {
			const char = argsString[i];

			if (inQuote) {
				current += char;
				if (char === quoteChar && argsString[i - 1] !== '\\') {
					inQuote = false;
				}
			} else {
				if (char === '"' || char === "'") {
					inQuote = true;
					quoteChar = char;
					current += char;
				} else if (char === '{' || char === '(') {
					depth++;
					current += char;
				} else if (char === '}' || char === ')') {
					depth--;
					current += char;
				} else if (char === ',' && depth === 0) {
					args.push(current.trim());
					current = "";
				} else {
					current += char;
				}
			}
		}
		if (current.trim()) {
			args.push(current.trim());
		}
		return args;
	}



	private resolveSqlString(lineIdx: number, arg: string, analysis: DocumentAnalysis): string {
		if (arg.startsWith('"') || arg.startsWith("'")) {
			// Check if it ends with the same quote
			const quote = arg.charAt(0);
			if (arg.endsWith(quote) && arg.length > 1) {
				return arg.substring(1, arg.length - 1);
			}
			// Incomplete or multi-line string: only strip the starting quote to preserve content at end of line
			return arg.substring(1);
		}
		// Resolve variable
		const scope = analysis.procedures.find(p => lineIdx >= p.startLine && lineIdx <= (p.endLine || Infinity));
		const locals = scope ? scope.variables : new Map<string, VariableDefinition>();
		const def = locals.get(arg.toLowerCase()) || analysis.globalVariables.get(arg.toLowerCase());

		if (def && def.assignments && def.assignments.length > 0) {
			// Find assignment closest to and before current line
			const relevantAssignment = def.assignments
				.filter(a => a.line < lineIdx)
				.sort((a, b) => b.line - a.line)[0];

			if (relevantAssignment) {
				return relevantAssignment.value;
			}
		}
		if (def && def.literalValue) {
			return def.literalValue;
		}
		return "";
	}

	private validateSqlPlaceholders(lineIdx: number, arg: string, funcName: string, analysis: DocumentAnalysis, diagnostics: vscode.Diagnostic[]) {
		const sqlString = this.resolveSqlString(lineIdx, arg, analysis);
		if (!sqlString) { return; }



		// Diagnostic checks based on function type
		if (funcName === 'SQLExecute') {
			// SQLExecute supports "Expression" substitution (?expr?)
			// It does NOT support bare positional parameters matching the broad pattern
			// Broad pattern from docs: ?[^?]+?
			const broadExpressionPattern = /\?[^?]+\?/g;
			const remainingString = sqlString.replace(broadExpressionPattern, '');

			if (/\?/.test(remainingString)) {
				// Highlight argument
				diagnostics.push(this.createDiagnostic(lineIdx, 0, arg.length,
					"SQLExecute should use named parameters '?var?'",
					vscode.DiagnosticSeverity.Warning, "ssl-invalid-sql-placeholder-style"));
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

	// --- Validation Phase 4: Function Calls ---

	private validateFunctionCalls(analysis: DocumentAnalysis, config: ValidationConfig, diagnostics: vscode.Diagnostic[]) {
		const validFunctionNames = new Set<string>();

		// 1. Add Built-ins
		SSL_BUILTIN_FUNCTIONS.forEach(f => validFunctionNames.add(f.name.toLowerCase()));

		// 2. Add Configured Functions
		config.configuredFunctions.forEach(f => validFunctionNames.add(f.toLowerCase()));

		// Also allow keywords that look like functions
		SSL_KEYWORDS.forEach(k => validFunctionNames.add(k.toLowerCase()));

		let inCommentBlock = false;
		let inString = false;
		let stringChar = '';

		for (let i = 0; i < analysis.lines.length; i++) {
			if (diagnostics.length >= config.maxProblems) { break; }

			let line = analysis.lines[i];
			const trimmed = line.trim();

			// Basic optimization
			if (!trimmed || trimmed.startsWith('//')) {
				continue;
			}

			// Handle block comments and strings across lines if necessary (though SSL strings are single line generally, 
			// except for some cases, but for safety lets assuming single line strings for now unless end is missing)

			// Masking Logic: Replace strings and comments with spaces
			let maskedLine = "";
			// inString and stringChar are persisted across lines


			for (let j = 0; j < line.length; j++) {
				const char = line[j];
				const next = line[j + 1] || '';

				if (inCommentBlock) {
					maskedLine += " ";
					if (char === '*' && next === '/') {
						inCommentBlock = false;
						maskedLine += " "; // consume /
						j++;
					}
					continue;
				}

				if (inString) {
					maskedLine += " ";
					if (char === stringChar && line[j - 1] !== '\\') {
						inString = false;
					}
					continue;
				}

				// Not in comment or string
				if (char === '/' && next === '*') {
					inCommentBlock = true;
					maskedLine += "  ";
					j++;
					continue;
				}

				if (char === '/' && next === '/') {
					// Start of single line comment -> ignore rest of line
					maskedLine += " ".repeat(line.length - j);
					break;
				}

				if (char === '"' || char === "'") {
					inString = true;
					stringChar = char;
					maskedLine += " ";
					continue;
				}

				maskedLine += char;
			}

			// Regex for function calls: Name(
			// Use maskedLine
			const functionCallRegex = /(?<![\.\:])\b([a-zA-Z][a-zA-Z0-9_]*)\s*\(/g;

			let match;
			while ((match = functionCallRegex.exec(maskedLine)) !== null) {
				const name = match[1];
				const nameLower = name.toLowerCase();

				// If it is NOT a valid function, check if it's a procedure in this file or just unknown
				if (!validFunctionNames.has(nameLower)) {
					// Check if it is a known procedure in this file
					const isLocalProcedure = analysis.procedures.some(p => p.name.toLowerCase() === nameLower);

					const range = new vscode.Range(i, match.index, i, match.index + name.length);
					const diag = new vscode.Diagnostic(
						range,
						`Invalid direct procedure call '${name}'. Procedures must be called using DoProc or ExecFunction.`,
						vscode.DiagnosticSeverity.Error
					);
					diag.code = "ssl-invalid-direct-call"; // Assign code for CodeAction
					diagnostics.push(diag);
				}
			}
		}
	}
}

