import * as vscode from "vscode";
import { getConfiguredFunctions } from "./utils/intellisense";
import { ProcedureIndex } from "./utils/procedureIndex";
import { CONFIG_KEYS, CONFIG_DEFAULTS } from "./constants/config";

interface FunctionSignature {
	label: string;
	parameters: string[];
	documentation: string;
}

/**
 * SSL Signature Help Provider
 * Provides parameter hints while typing function calls
 * Refactored to use ProcedureIndex and centralized function definitions.
 */
export class SSLSignatureHelpProvider implements vscode.SignatureHelpProvider {

	private functionSignatures: Map<string, FunctionSignature> = new Map();

	constructor(private readonly procedureIndex?: ProcedureIndex) {
		this.initializeFunctionSignatures();

		// Listen for config changes to reload signatures
		vscode.workspace.onDidChangeConfiguration(e => {
			if (e.affectsConfiguration("ssl.intellisense")) {
				this.initializeFunctionSignatures();
			}
		});
	}

	public provideSignatureHelp(
		document: vscode.TextDocument,
		position: vscode.Position,
		token: vscode.CancellationToken,
		context: vscode.SignatureHelpContext
	): vscode.SignatureHelp | null {
		// Fast path: Identify if we are in a function call
		// We scan backwards from cursor to find opening parenthesis.
		const lineText = document.lineAt(position.line).text;
		const offset = position.character;

		const callInfo = this.findFunctionCall(lineText, offset);
		if (!callInfo) {
			return null;
		}

		const funcName = callInfo.name;
		const paramIndex = callInfo.parameterIndex;

		// Special handling for DoProc/ExecFunction ("ProcName", ...)
		// Check if first arg is a string literal containing the procedure name
		if ((funcName.toUpperCase() === "DOPROC" || funcName.toUpperCase() === "EXECFUNCTION") && paramIndex > 0) {
			// If we are past the first argument, we might be providing help for the *target procedure's* parameters.
			// DoProc("TargetProc", {ArrayArgs}) ?? 
			// Wait, DoProc takes array of args usually: DoProc("Proc", {arg1, arg2})
			// Or variadic? SSL_BUILTIN_FUNCTIONS says DoProc(args) usually takes array.

			// Previous code logic:
			// DoProc("Name", { param1, param2 })
			// Logic:
			// 1. Extract "Name" from first arg.
			// 2. Determine if we are inside the array { ... } (which is usually the second arg or so, depending on implementation).

			// Let's refine parsing for DoProc context.
			// We need to know if we are inside the `{ ... }` array of arguments.

			const arrayContext = this.getDoProcArrayContext(lineText, offset);
			if (arrayContext) {
				// We are inside { ... } which is an argument to DoProc("TargetName", ...)
				const targetProc = this.resolveProcedure(arrayContext.procName);
				if (targetProc) {
					return this.createSignatureHelp(targetProc, arrayContext.paramIndex, true);
				}
			}
		}

		// Regular Function Call
		// Check built-ins first
		const builtIn = this.functionSignatures.get(funcName.toUpperCase());
		if (builtIn) {
			return this.createSignatureHelpFromDefinition(builtIn, paramIndex);
		}

		// Check Procedure Index for user-defined procedures called directly (if supported by language)
		// Does SSL support `MyProc(...)`? Usually yes if in same file or included.
		// Assuming direct calls are possible.
		if (this.procedureIndex) {
			const procs = this.procedureIndex.getProceduresByName(funcName);
			if (procs.length > 0) {
				// Pick best match? Usually just one or first.
				// Ideally we check import visibility but for now first match is good.
				const proc = procs[0];
				const sig = this.convertProcedureInfoToSignature(proc);
				return this.createSignatureHelpFromDefinition(sig, paramIndex);
			}
		}

		return null;
	}

	private initializeFunctionSignatures(): void {
		this.functionSignatures.clear();
		const config = vscode.workspace.getConfiguration("ssl");
		const allFunctions = getConfiguredFunctions(config);

		allFunctions.forEach(func => {
			// Parse params string "(p1, p2)" into array
			const paramStr = func.params.replace(/^\(|\)$/g, ""); // Remove parens
			const parameters = paramStr.split(",").map(p => p.trim()).filter(p => p.length > 0);

			this.functionSignatures.set(func.name.toUpperCase(), {
				label: func.signature || `${func.name}${func.params}`,
				parameters: parameters,
				documentation: func.description
			});
		});
	}

	private findFunctionCall(text: string, offset: number): { name: string; parameterIndex: number } | null {
		// Robust backward search for '('
		// Must handle nested parens '()' and strings/comments (simple check)

		let balance = 0;
		let braceBalance = 0;
		let paramCount = 0;

		for (let i = offset - 1; i >= 0; i--) {
			const char = text[i];

			if (char === ')') {
				balance++;
			} else if (char === '}') {
				braceBalance++;
			} else if (char === '{') {
				braceBalance--;
			} else if (char === '(') {
				if (balance > 0) {
					balance--;
				} else {
					// Found the opening parenthesis of OUR call
					// Extract name
					const prefix = text.substring(0, i).trimRight();
					const match = /([a-zA-Z_][a-zA-Z0-9_]*)$/.exec(prefix);
					if (match) {
						return { name: match[1], parameterIndex: paramCount };
					}
					return null; // found ( but no name? e.g. "if ("
				}
			} else if (char === ',') {
				if (balance === 0 && braceBalance === 0) { // Only count commas at current level
					paramCount++;
				}
			}
		}
		return null;
	}

	private getDoProcArrayContext(text: string, offset: number): { procName: string; paramIndex: number } | null {
		// Looking for pattern: DoProc ( "Name" , { ... cursor ... } )
		// We know we are inside DoProc (checked by caller partially).
		// But we need to be sure we are inside the array `{ ... }`.

		// Scan backwards.
		// Expect to find `{` before we find `DoProc`.
		let braceBalance = 0;
		let paramIndex = 0;

		for (let i = offset - 1; i >= 0; i--) {
			const char = text[i];

			if (char === '}') {
				braceBalance++;
			} else if (char === '{') {
				if (braceBalance > 0) {
					braceBalance--;
				} else {
					// Found the opening `{`
					// Now verify this `{` is an argument to `DoProc("Name",`
					// Scan back further
					const beforeBrace = text.substring(0, i).trimRight();
					// Expect comma, then string literal, then ( then DoProc
					// This regex is a bit expensive but robust enough for local context
					// Matches: DoProc\s*\(\s*["']([^"']+)["']\s*,\s*$
					const match = /DoProc\s*\(\s*["']([^"']+)["']\s*,\s*$/i.exec(beforeBrace);
					if (match) {
						return { procName: match[1], paramIndex: paramIndex };
					}
					return null;
				}
			}
			else if (char === ',') {
				if (braceBalance === 0) {
					paramIndex++;
				}
			}
		}
		return null;
	}

	private resolveProcedure(name: string): FunctionSignature | null {
		if (!this.procedureIndex) {
			return null;
		}

		// 1. Resolve using Index (handles namespaces if we added logic, but simpler here)
		// Using "resolveProcedureLiteral" logic without namespace root map for now?
		// Or better, just getProceduresByName if simple name.
		// sslSignatureHelpProvider previously had complex scanning.
		// resolveProcedureLiteral needs config.

		const config = vscode.workspace.getConfiguration("ssl");
		const namespaceRoots = config.get<Record<string, string>>(
			CONFIG_KEYS.DOCUMENT_NAMESPACES,
			CONFIG_DEFAULTS[CONFIG_KEYS.DOCUMENT_NAMESPACES] as Record<string, string>
		) || {};

		const procInfo = this.procedureIndex.resolveProcedureLiteral(name, namespaceRoots);

		if (procInfo) {
			return this.convertProcedureInfoToSignature(procInfo);
		}

		// Fallback: simple name lookup
		const simpleMatch = this.procedureIndex.getProceduresByName(name);
		if (simpleMatch.length > 0) {
			return this.convertProcedureInfoToSignature(simpleMatch[0]);
		}

		return null;
	}

	private convertProcedureInfoToSignature(proc: { parameters: string[]; name: string; declarationText: string }): FunctionSignature {
		return {
			label: `${proc.name}(${proc.parameters.join(", ")})`,
			parameters: proc.parameters, // these are formatted like "param" or "type param"? usually just names in SSL :PARAMETERS
			documentation: `User-defined procedure in ${proc.name}` // We could fetch more doc if available in ProcedureInfo
		};
	}

	private createSignatureHelpFromDefinition(def: FunctionSignature, currentParam: number): vscode.SignatureHelp {
		const sigInfo = new vscode.SignatureInformation(def.label, def.documentation);
		sigInfo.parameters = def.parameters.map(p => new vscode.ParameterInformation(p));

		const help = new vscode.SignatureHelp();
		help.signatures = [sigInfo];
		help.activeSignature = 0;
		help.activeParameter = currentParam;
		return help;
	}

	private createSignatureHelp(sig: FunctionSignature, currentParam: number, isDoProc: boolean): vscode.SignatureHelp {
		// Re-format label for DoProc context? 
		// DoProc("Name", { p1, p2 })
		// The signature help should probably show the *target* signature.

		const help = new vscode.SignatureHelp();
		const label = isDoProc ? `(In DoProc) ${sig.label}` : sig.label;

		const sigInfo = new vscode.SignatureInformation(label, sig.documentation);
		sigInfo.parameters = sig.parameters.map(p => new vscode.ParameterInformation(p));

		help.signatures = [sigInfo];
		help.activeSignature = 0;
		help.activeParameter = currentParam;
		return help;
	}
}
