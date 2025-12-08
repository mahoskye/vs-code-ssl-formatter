import * as vscode from "vscode";
import { PATTERNS } from "../constants/patterns";

export interface ClassMembers {
	className: string;
	methods: string[];
	properties: string[];
	range?: vscode.Range;
	uri?: vscode.Uri;
}

export interface ClassIndex {
	getClassMembers(className: string): ClassMembers | undefined;
	getAllClasses(): ClassMembers[];
}

interface ParsedClassInfo extends ClassMembers {
	fileKey: string;
}

const SSL_FILE_GLOB = "**/*.{ssl,srvscr,ds}";
const EXCLUDE_GLOB = "**/node_modules/**";

export class WorkspaceClassIndex implements ClassIndex, vscode.Disposable {
	private readonly disposables: vscode.Disposable[] = [];
	private readonly fileClassMap = new Map<string, ParsedClassInfo[]>();
	private initialized = false;

	public async initialize(): Promise<void> {
		if (this.initialized) {
			return;
		}
		this.initialized = true;

		// Seed from existing files
		try {
			const files = await vscode.workspace.findFiles(SSL_FILE_GLOB, EXCLUDE_GLOB, 2000);
			await Promise.all(files.map(uri => this.updateFromUri(uri)));
		} catch {
			// Ignore indexing errors – best-effort
		}

		// Update when documents are saved (captures active edits)
		this.disposables.push(
			vscode.workspace.onDidSaveTextDocument(document => {
				if (this.isSSLDocument(document)) {
					this.updateDocument(document);
				}
			})
		);

		// File system watcher for create/change/delete events
		const watcher = vscode.workspace.createFileSystemWatcher(SSL_FILE_GLOB);
		watcher.onDidCreate(uri => this.updateFromUri(uri));
		watcher.onDidChange(uri => this.updateFromUri(uri));
		watcher.onDidDelete(uri => this.removeFile(uri));
		this.disposables.push(watcher);

		// Index already open documents (unsaved buffers)
		vscode.workspace.textDocuments.forEach(document => {
			if (this.isSSLDocument(document)) {
				this.updateDocument(document);
			}
		});
	}

	public dispose(): void {
		this.disposables.forEach(d => d.dispose());
		this.fileClassMap.clear();
	}

	public getClassMembers(className: string): ClassMembers | undefined {
		const normalized = className.toUpperCase();
		const matches = Array.from(this.fileClassMap.values())
			.flat()
			.filter(cls => cls.className.toUpperCase() === normalized);

		if (matches.length === 0) {
			return undefined;
		}

		const methods = new Set<string>();
		const properties = new Set<string>();
		matches.forEach(cls => {
			cls.methods.forEach(method => methods.add(method));
			cls.properties.forEach(prop => properties.add(prop));
		});

		return {
			className,
			methods: Array.from(methods),
			properties: Array.from(properties)
		};
	}

	public getAllClasses(): ClassMembers[] {
		const result: ClassMembers[] = [];
		for (const list of this.fileClassMap.values()) {
			for (const item of list) {
				// reconstruct full info. URI is computed from fileKey.
				result.push({
					...item,
					uri: vscode.Uri.parse(item.fileKey)
				});
			}
		}
		return result;
	}

	private async updateFromUri(uri: vscode.Uri): Promise<void> {
		try {
			const document = await vscode.workspace.openTextDocument(uri);
			if (this.isSSLDocument(document)) {
				this.updateDocument(document);
			}
		} catch {
			// Ignore files we cannot open
		}
	}

	private updateDocument(document: vscode.TextDocument): void {
		const fileKey = document.uri.toString();
		const parsed = parseClassesFromText(document.getText(), document).map(cls => ({
			...cls,
			fileKey
		}));
		this.fileClassMap.set(fileKey, parsed);
	}

	private removeFile(uri: vscode.Uri): void {
		const fileKey = uri.toString();
		this.fileClassMap.delete(fileKey);
	}

	private isSSLDocument(document: vscode.TextDocument): boolean {
		return ["ssl", "srvscr", "ds"].includes(document.languageId)
			|| /\.(ssl|srvscr|ds)$/i.test(document.uri.fsPath);
	}
}

export function parseClassesFromText(text: string, document?: vscode.TextDocument): ClassMembers[] {
	const lines = text.split(/\r?\n/);
	const classes: ClassMembers[] = [];
	let current: ClassMembers | null = null;
	let currentStartLine = 0;

	const flushCurrent = (endLineFn: number) => {
		if (current) {
			current.methods = Array.from(new Set(current.methods));
			current.properties = Array.from(new Set(current.properties));
			if (document) {
				// If we have doc, we make range.
				// Assuming class ends at start of next class or EOF.
				// endLineFn is index of line that caused flush (e.g. next class decl).
				// Range is startLine to endLineFn - 1
				const endLine = Math.max(currentStartLine, endLineFn - 1);
				// Need character pos? 0 to line length.
				// We don't have line length readily without doc.
				current.range = new vscode.Range(currentStartLine, 0, endLine, 0);
			}
			classes.push(current);
			current = null;
		}
	};

	for (let i = 0; i < lines.length; i++) {
		const rawLine = lines[i];
		const line = rawLine.trim();

		// Use centralized pattern for CLASS
		const classMatch = line.match(PATTERNS.CLASS.DEFINITION);
		if (classMatch) {
			flushCurrent(i);
			current = {
				className: classMatch[1],
				methods: [],
				properties: []
			};
			currentStartLine = i;
			continue;
		}

		if (!current) {
			continue;
		}

		// Use centralized pattern for PROCEDURE
		const procedureMatch = line.match(PATTERNS.PROCEDURE.DEFINITION);
		if (procedureMatch) {
			current.methods.push(procedureMatch[1]);
			continue;
		}

		// Handles both :DECLARE and :PUBLIC (combined logic)
		// PATTERNS doesn't have a combined one, so we keep this specific regex or compose it.
		// "DECLARE_STATEMENT": /^\s*:DECLARE\s+(.+?);/i
		// We'll stick to the custom regex for this specific mixed use case to avoid complexity
		const declareMatch = line.match(/^:(DECLARE|PUBLIC)\s+(.+?);/i);
		if (declareMatch) {
			const identifiers = declareMatch[2]
				.split(',')
				.map(part => part.split(':=')[0].trim())
				.filter(Boolean);
			current.properties.push(...identifiers);
		}
	}

	flushCurrent(lines.length);
	return classes;
}
