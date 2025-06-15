/**
 * Mock implementation of VS Code API for testing
 */

export enum FoldingRangeKind {
    Comment = 1,
    Imports = 2,
    Region = 3,
}

export class Position {
    public line: number;
    public character: number;

    constructor(line: number, character: number) {
        this.line = line;
        this.character = character;
    }
}

export class Range {
    public start: Position;
    public end: Position;

    constructor(startLine: number, startCharacter: number, endLine: number, endCharacter: number) {
        this.start = new Position(startLine, startCharacter);
        this.end = new Position(endLine, endCharacter);
    }
}

export class FoldingRange {
    public start: number;
    public end: number;
    public kind?: FoldingRangeKind;

    constructor(start: number, end: number, kind?: FoldingRangeKind) {
        this.start = start;
        this.end = end;
        this.kind = kind;
    }
}

export class Uri {
    public scheme: string;
    public path: string;

    constructor(scheme: string, path: string) {
        this.scheme = scheme;
        this.path = path;
    }

    static file(path: string): Uri {
        return new Uri("file", path);
    }
}

export interface TextDocument {
    getText(): string;
    lineCount: number;
    lineAt(line: number): any;
    positionAt(offset: number): Position;
    offsetAt(position: Position): number;
    save(): Promise<boolean>;
    isClosed: boolean;
    isDirty: boolean;
    isUntitled: boolean;
    languageId: string;
    version: number;
    fileName: string;
    uri: Uri;
}

export interface FoldingContext {
    // Mock context - empty for testing
}

export interface CancellationToken {
    // Mock token - empty for testing
}

export type ProviderResult<T> = T | undefined | null | Thenable<T | undefined | null>;

export interface FoldingRangeProvider {
    provideFoldingRanges(
        document: TextDocument,
        context: FoldingContext,
        token: CancellationToken
    ): ProviderResult<FoldingRange[]>;
}

export const languages = {
    registerFoldingRangeProvider: jest.fn(),
    createDiagnosticCollection: jest.fn(() => ({
        dispose: jest.fn(),
        set: jest.fn(),
        clear: jest.fn(),
        delete: jest.fn(),
    })),
};

export const workspace = {
    // Mock workspace for testing
};

export const window = {
    // Mock window for testing
};

export enum DiagnosticSeverity {
    Error = 0,
    Warning = 1,
    Information = 2,
    Hint = 3,
}

export class Diagnostic {
    public range: Range;
    public message: string;
    public severity: DiagnosticSeverity;

    constructor(range: Range, message: string, severity?: DiagnosticSeverity) {
        this.range = range;
        this.message = message;
        this.severity = severity || DiagnosticSeverity.Error;
    }
}
