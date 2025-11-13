/**
 * Mock VSCode API for unit testing
 * This allows us to test formatters and providers without a full VSCode environment
 */

export class MockPosition {
	constructor(public line: number, public character: number) {}

	isAfter(other: MockPosition): boolean {
		if (this.line > other.line) return true;
		if (this.line === other.line && this.character > other.character) return true;
		return false;
	}

	isBefore(other: MockPosition): boolean {
		if (this.line < other.line) return true;
		if (this.line === other.line && this.character < other.character) return true;
		return false;
	}

	isEqual(other: MockPosition): boolean {
		return this.line === other.line && this.character === other.character;
	}
}

export class MockRange {
	constructor(
		public start: MockPosition,
		public end: MockPosition
	) {}

	contains(positionOrRange: MockPosition | MockRange): boolean {
		if (positionOrRange instanceof MockPosition) {
			return !positionOrRange.isBefore(this.start) && !positionOrRange.isAfter(this.end);
		}
		return this.contains(positionOrRange.start) && this.contains(positionOrRange.end);
	}

	isEmpty(): boolean {
		return this.start.isEqual(this.end);
	}
}

export class MockTextLine {
	constructor(
		public text: string,
		public lineNumber: number
	) {}

	get range(): MockRange {
		return new MockRange(
			new MockPosition(this.lineNumber, 0),
			new MockPosition(this.lineNumber, this.text.length)
		);
	}

	get firstNonWhitespaceCharacterIndex(): number {
		const match = this.text.match(/\S/);
		return match ? match.index || 0 : 0;
	}

	get isEmptyOrWhitespace(): boolean {
		return this.text.trim().length === 0;
	}

	get rangeIncludingLineBreak(): MockRange {
		return new MockRange(
			new MockPosition(this.lineNumber, 0),
			new MockPosition(this.lineNumber + 1, 0)
		);
	}
}

export class MockUri {
	constructor(public fsPath: string) {}

	static file(path: string): MockUri {
		return new MockUri(path);
	}

	toString(): string {
		return this.fsPath;
	}
}

export enum MockEndOfLine {
	LF = 1,
	CRLF = 2
}

export class MockTextDocument {
	private lines: string[];

	constructor(
		public uri: MockUri,
		public languageId: string,
		private content: string,
		public version: number = 1
	) {
		this.lines = content.split('\n');
	}

	get fileName(): string {
		return this.uri.fsPath;
	}

	get isUntitled(): boolean {
		return false;
	}

	get isDirty(): boolean {
		return false;
	}

	get isClosed(): boolean {
		return false;
	}

	get eol(): MockEndOfLine {
		return MockEndOfLine.LF;
	}

	get lineCount(): number {
		return this.lines.length;
	}

	getText(range?: MockRange): string {
		if (!range) {
			return this.content;
		}

		const startOffset = this.offsetAt(range.start);
		const endOffset = this.offsetAt(range.end);
		return this.content.substring(startOffset, endOffset);
	}

	lineAt(lineOrPosition: number | MockPosition): MockTextLine {
		const lineNumber = typeof lineOrPosition === 'number'
			? lineOrPosition
			: lineOrPosition.line;

		if (lineNumber < 0 || lineNumber >= this.lines.length) {
			throw new Error(`Line number ${lineNumber} out of range`);
		}

		return new MockTextLine(this.lines[lineNumber], lineNumber);
	}

	offsetAt(position: MockPosition): number {
		let offset = 0;
		for (let i = 0; i < position.line && i < this.lines.length; i++) {
			offset += this.lines[i].length + 1; // +1 for newline
		}
		return offset + position.character;
	}

	positionAt(offset: number): MockPosition {
		const beforeOffset = this.content.substring(0, offset);
		const linesBefore = beforeOffset.split('\n');
		return new MockPosition(
			linesBefore.length - 1,
			linesBefore[linesBefore.length - 1].length
		);
	}

	getWordRangeAtPosition(position: MockPosition): MockRange | undefined {
		return undefined;
	}

	validateRange(range: MockRange): MockRange {
		return range;
	}

	validatePosition(position: MockPosition): MockPosition {
		return position;
	}

	save(): Promise<boolean> {
		return Promise.resolve(true);
	}
}

export class MockTextEdit {
	constructor(
		public range: MockRange,
		public newText: string
	) {}

	static replace(range: MockRange, newText: string): MockTextEdit {
		return new MockTextEdit(range, newText);
	}

	static insert(position: MockPosition, newText: string): MockTextEdit {
		return new MockTextEdit(new MockRange(position, position), newText);
	}

	static delete(range: MockRange): MockTextEdit {
		return new MockTextEdit(range, '');
	}
}

export interface MockFormattingOptions {
	tabSize: number;
	insertSpaces: boolean;
	[key: string]: boolean | number | string | undefined;
}

export class MockWorkspaceConfiguration {
	private values: Map<string, any> = new Map();

	constructor(defaults?: Record<string, any>) {
		if (defaults) {
			Object.entries(defaults).forEach(([key, value]) => {
				this.values.set(key, value);
			});
		}
	}

	get<T>(key: string, defaultValue?: T): T {
		if (this.values.has(key)) {
			return this.values.get(key) as T;
		}
		return defaultValue as T;
	}

	update(key: string, value: any): void {
		this.values.set(key, value);
	}

	has(key: string): boolean {
		return this.values.has(key);
	}
}

export enum MockDiagnosticSeverity {
	Error = 0,
	Warning = 1,
	Information = 2,
	Hint = 3
}

export class MockDiagnostic {
	constructor(
		public range: MockRange,
		public message: string,
		public severity: MockDiagnosticSeverity = MockDiagnosticSeverity.Error
	) {}

	public code?: string | number;
	public source?: string;
}

export class MockDiagnosticCollection {
	private diagnostics: Map<string, MockDiagnostic[]> = new Map();

	set(uri: MockUri, diagnostics: MockDiagnostic[]): void {
		this.diagnostics.set(uri.fsPath, diagnostics);
	}

	get(uri: MockUri): MockDiagnostic[] {
		return this.diagnostics.get(uri.fsPath) || [];
	}

	delete(uri: MockUri): void {
		this.diagnostics.delete(uri.fsPath);
	}

	clear(): void {
		this.diagnostics.clear();
	}

	dispose(): void {
		this.clear();
	}
}

/**
 * Create a mock document from text content
 */
export function createDocument(content: string, languageId: string = 'ssl'): MockTextDocument {
	return new MockTextDocument(
		MockUri.file('/test.ssl'),
		languageId,
		content
	);
}

/**
 * Apply text edits to content
 */
export function applyEdits(text: string, edits: MockTextEdit[]): string {
	// Sort edits in reverse order to apply from end to start
	const sortedEdits = edits.sort((a, b) => {
		const aStart = a.range.start;
		const bStart = b.range.start;
		if (aStart.line !== bStart.line) {
			return bStart.line - aStart.line;
		}
		return bStart.character - aStart.character;
	});

	let result = text;
	const lines = text.split('\n');

	for (const edit of sortedEdits) {
		const startOffset = getOffset(lines, edit.range.start);
		const endOffset = getOffset(lines, edit.range.end);
		result = result.substring(0, startOffset) + edit.newText + result.substring(endOffset);
	}

	return result;
}

function getOffset(lines: string[], position: MockPosition): number {
	let offset = 0;
	for (let i = 0; i < position.line && i < lines.length; i++) {
		offset += lines[i].length + 1; // +1 for newline
	}
	return offset + position.character;
}

/**
 * Create default formatting options
 */
export function createFormattingOptions(tabSize: number = 1, insertSpaces: boolean = false): MockFormattingOptions {
	return { tabSize, insertSpaces };
}

/**
 * Create a workspace configuration with SSL defaults
 */
export function createSSLConfig(overrides?: Record<string, any>): MockWorkspaceConfiguration {
	const defaults = {
		'ssl.format.indentStyle': 'tab',
		'ssl.format.indentWidth': 1,
		'ssl.format.keywordCase': 'upper',
		'ssl.format.builtinFunctionCase': 'PascalCase',
		'ssl.format.trimTrailingWhitespace': true,
		'ssl.maxNumberOfProblems': 100,
		'ssl.strictStyleGuideMode': false,
		'ssl.naming.hungarianNotation.enabled': true,
		'ssl.naming.hungarianNotation.severity': 'warn',
		'ssl.styleGuide.limitBlockDepth': 4,
		'ssl.styleGuide.maxParamsPerProcedure': 8,
		'ssl.security.preventSqlInjection': true,
		'ssl.security.requireParameterizedQueries': true,
		...overrides
	};

	return new MockWorkspaceConfiguration(defaults);
}
