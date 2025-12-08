import * as vscode from "vscode";
import * as path from "path";

export interface ProcedureInfo {
	name: string;
	uri: vscode.Uri;
	range: vscode.Range;
	fileBaseName: string;
	scriptKeys: string[];
	declarationText: string;
	parameters: string[];
}

export interface ProcedureIndex extends vscode.Disposable {
	initialize(): Promise<void>;
	getProceduresByName(name: string): ProcedureInfo[];
	getAllProcedures(): ProcedureInfo[];
	resolveProcedureLiteral(literal: string, namespaceRoots: Record<string, string>): ProcedureInfo | undefined;
}

export class WorkspaceProcedureIndex implements ProcedureIndex {
	private readonly disposables: vscode.Disposable[] = [];
	private readonly fileProcedures = new Map<string, ProcedureInfo[]>();
	private readonly procedureMap = new Map<string, ProcedureInfo[]>();
	private initialized = false;

	public async initialize(): Promise<void> {
		if (this.initialized) {
			return;
		}
		this.initialized = true;

		try {
			const files = await vscode.workspace.findFiles("**/*.{ssl,srvscr,ds}", "**/node_modules/**", 2000);
			await Promise.all(files.map(uri => this.updateFromUri(uri)));
		} catch {
			// Best effort – ignore errors indexing workspace
		}

		const watcher = vscode.workspace.createFileSystemWatcher("**/*.{ssl,srvscr,ds}");
		watcher.onDidCreate(uri => this.updateFromUri(uri));
		watcher.onDidChange(uri => this.updateFromUri(uri));
		watcher.onDidDelete(uri => this.removeFile(uri));
		this.disposables.push(watcher);

		this.disposables.push(
			vscode.workspace.onDidSaveTextDocument(document => {
				if (this.isSSLDocument(document)) {
					this.updateDocument(document);
				}
			})
		);

		vscode.workspace.textDocuments.forEach(document => {
			if (this.isSSLDocument(document)) {
				this.updateDocument(document);
			}
		});
	}

	public dispose(): void {
		this.disposables.forEach(d => d.dispose());
		this.fileProcedures.clear();
		this.procedureMap.clear();
	}

	public getProceduresByName(name: string): ProcedureInfo[] {
		const key = name.toLowerCase();
		return this.procedureMap.get(key) ?? [];
	}

	public getAllProcedures(): ProcedureInfo[] {
		const allProcs: ProcedureInfo[] = [];
		for (const procs of this.fileProcedures.values()) {
			allProcs.push(...procs);
		}
		return allProcs;
	}

	public resolveProcedureLiteral(literal: string, namespaceRoots: Record<string, string>): ProcedureInfo | undefined {
		const trimmed = literal.trim();
		if (!trimmed) {
			return undefined;
		}

		const parts = trimmed.split(".").map(part => part.trim()).filter(Boolean);
		if (parts.length === 0) {
			return undefined;
		}

		const procedureName = parts.pop()!;
		const candidates = this.getProceduresByName(procedureName);
		if (candidates.length === 0) {
			return undefined;
		}

		const namespaceSegments = parts.map(segment => segment.toLowerCase());
		const normalizedRoots = normalizeNamespaceRoots(namespaceRoots);

		if (namespaceSegments.length > 0) {
			const alias = namespaceSegments[0];
			const aliasPath = normalizedRoots.get(alias);
			if (aliasPath) {
				const combined = combineSegments(aliasPath, namespaceSegments.slice(1));
				const match = findByScriptKey(candidates, combined);
				if (match) {
					return match;
				}
			}

			const directKey = namespaceSegments.join(".");
			const directMatch = findByScriptKey(candidates, directKey);
			if (directMatch) {
				return directMatch;
			}

			const fileBase = namespaceSegments[namespaceSegments.length - 1];
			const baseMatch = candidates.find(proc => proc.fileBaseName === fileBase);
			if (baseMatch) {
				return baseMatch;
			}
		}

		if (candidates.length === 1) {
			return candidates[0];
		}

		return undefined;
	}

	private async updateFromUri(uri: vscode.Uri): Promise<void> {
		try {
			const document = await vscode.workspace.openTextDocument(uri);
			if (this.isSSLDocument(document)) {
				this.updateDocument(document);
			}
		} catch {
			this.removeFile(uri);
		}
	}

	private updateDocument(document: vscode.TextDocument): void {
		const fileKey = document.uri.toString();
		this.removeProcedures(fileKey);

		const procedures = parseProceduresFromDocument(document);
		const enhanced = procedures.map(proc => ({
			...proc,
			fileBaseName: path.basename(document.uri.fsPath, path.extname(document.uri.fsPath)).toLowerCase(),
			scriptKeys: computeScriptKeys(document.uri)
		}));

		this.fileProcedures.set(fileKey, enhanced);
		enhanced.forEach(info => {
			const key = info.name.toLowerCase();
			const existing = this.procedureMap.get(key);
			if (existing) {
				existing.push(info);
			} else {
				this.procedureMap.set(key, [info]);
			}
		});
	}

	private removeFile(uri: vscode.Uri): void {
		const fileKey = uri.toString();
		this.removeProcedures(fileKey);
		this.fileProcedures.delete(fileKey);
	}

	private removeProcedures(fileKey: string): void {
		const existing = this.fileProcedures.get(fileKey);
		if (!existing) {
			return;
		}

		existing.forEach(info => {
			const key = info.name.toLowerCase();
			const entries = this.procedureMap.get(key);
			if (!entries) {
				return;
			}
			const updated = entries.filter(entry => entry !== info);
			if (updated.length === 0) {
				this.procedureMap.delete(key);
			} else {
				this.procedureMap.set(key, updated);
			}
		});
	}

	private isSSLDocument(document: vscode.TextDocument): boolean {
		return ["ssl", "srvscr", "ds"].includes(document.languageId) ||
			/\.(ssl|srvscr|ds)$/i.test(document.uri.fsPath);
	}
}

function parseProceduresFromDocument(document: vscode.TextDocument): Omit<ProcedureInfo, "fileBaseName" | "scriptKeys">[] {
	const procedures: Omit<ProcedureInfo, "fileBaseName" | "scriptKeys">[] = [];
	for (let lineNumber = 0; lineNumber < document.lineCount; lineNumber++) {
		const lineText = document.lineAt(lineNumber).text;
		const match = lineText.match(/^\s*:PROCEDURE\s+(\w+)/i);
		if (!match) {
			continue;
		}
		const procName = match[1];
		const column = lineText.toUpperCase().indexOf(procName.toUpperCase());
		const range = new vscode.Range(
			new vscode.Position(lineNumber, column),
			new vscode.Position(lineNumber, column + procName.length)
		);
		const parameters = extractProcedureParameters(document, lineNumber);
		procedures.push({
			name: procName,
			uri: document.uri,
			range,
			declarationText: lineText.trim(),
			parameters
		});
	}
	return procedures;
}

function extractProcedureParameters(document: vscode.TextDocument, procLine: number): string[] {
	const parameters: string[] = [];
	for (let line = procLine + 1; line < Math.min(document.lineCount, procLine + 20); line++) {
		const text = document.lineAt(line).text.trim();
		if (!text) {
			continue;
		}
		const paramMatch = text.match(/^:PARAMETERS\s+(.+?);/i);
		if (paramMatch) {
			return paramMatch[1].split(",").map(param => param.trim()).filter(Boolean);
		}
		if (/^:(PROCEDURE|ENDPROC)\b/i.test(text)) {
			break;
		}
	}
	return parameters;
}

function computeScriptKeys(uri: vscode.Uri): string[] {
	const keys = new Set<string>();
	const fsPath = uri.fsPath;
	const ext = path.extname(fsPath);
	const baseName = path.basename(fsPath, ext).toLowerCase();
	keys.add(baseName);

	const relativePath = vscode.workspace.asRelativePath(uri, false);
	if (relativePath && relativePath !== uri.fsPath) {
		const normalized = normalizePath(relativePath);
		const withoutExt = removeExtension(normalized);
		if (withoutExt) {
			keys.add(withoutExt);
			const dotKey = withoutExt.replace(/\//g, ".");
			keys.add(dotKey);
			const segments = withoutExt.split("/");
			let accumulator = "";
			segments.forEach(segment => {
				const lowerSegment = segment.toLowerCase();
				keys.add(lowerSegment);
				accumulator = accumulator ? `${accumulator}.${lowerSegment}` : lowerSegment;
				keys.add(accumulator);
			});
		}
	}

	return Array.from(keys);
}

function normalizeNamespaceRoots(map: Record<string, string>): Map<string, string> {
	const normalized = new Map<string, string>();
	Object.entries(map || {}).forEach(([alias, folder]) => {
		if (!alias || !folder) {
			return;
		}
		const lowerAlias = alias.toLowerCase();
		const normalizedPath = normalizePath(folder).replace(/\//g, ".");
		normalized.set(lowerAlias, normalizedPath);
	});
	return normalized;
}

function findByScriptKey(candidates: ProcedureInfo[], key: string): ProcedureInfo | undefined {
	const normalizedKey = key.toLowerCase();
	return candidates.find(proc => proc.scriptKeys.some(scriptKey => scriptKey === normalizedKey));
}

function combineSegments(prefix: string, segments: string[]): string {
	if (!segments || segments.length === 0) {
		return prefix;
	}
	return `${prefix}.${segments.join(".")}`;
}

function normalizePath(value: string): string {
	return value
		.replace(/\\/g, "/")
		.replace(/^\.\/+/, "")
		.replace(/^\//, "")
		.toLowerCase();
}

function removeExtension(value: string): string {
	const index = value.lastIndexOf(".");
	if (index === -1) {
		return value;
	}
	return value.substring(0, index);
}
