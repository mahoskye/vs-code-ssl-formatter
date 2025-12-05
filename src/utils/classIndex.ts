import * as vscode from "vscode";

export interface ClassMembers {
	className: string;
	methods: string[];
	properties: string[];
}

export interface ClassIndex {
	getClassMembers(className: string): ClassMembers | undefined;
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
		const parsed = parseClassesFromText(document.getText()).map(cls => ({
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

export function parseClassesFromText(text: string): ClassMembers[] {
	const lines = text.split(/\r?\n/);
	const classes: ClassMembers[] = [];
	let current: ClassMembers | null = null;

	const flushCurrent = () => {
		if (current) {
			current.methods = Array.from(new Set(current.methods));
			current.properties = Array.from(new Set(current.properties));
			classes.push(current);
			current = null;
		}
	};

	for (const rawLine of lines) {
		const line = rawLine.trim();

		const classMatch = line.match(/^:CLASS\s+(\w+)/i);
		if (classMatch) {
			flushCurrent();
			current = {
				className: classMatch[1],
				methods: [],
				properties: []
			};
			continue;
		}

		if (!current) {
			continue;
		}

		// Start of a new class closes the previous one
		if (/^:CLASS\b/i.test(line)) {
			flushCurrent();
			continue;
		}

		const procedureMatch = line.match(/^:PROCEDURE\s+(\w+)/i);
		if (procedureMatch) {
			current.methods.push(procedureMatch[1]);
			continue;
		}

		const declareMatch = line.match(/^:(DECLARE|PUBLIC)\s+(.+?);/i);
		if (declareMatch) {
			const identifiers = declareMatch[2]
				.split(',')
				.map(part => part.split(':=')[0].trim())
				.filter(Boolean);
			current.properties.push(...identifiers);
		}
	}

	flushCurrent();
	return classes;
}
