// Jest setup file for VS Code extension testing
// This file runs before all tests

// Mock VS Code API if needed
const mockVscode = {
    Position: class {
        constructor(public line: number, public character: number) {}
    },
    Range: class {
        constructor(public start: any, public end: any) {}
        isEqual(other: any): boolean {
            return (
                this.start.line === other.start.line &&
                this.start.character === other.start.character &&
                this.end.line === other.end.line &&
                this.end.character === other.end.character
            );
        }
    },
    Uri: {
        file: (path: string) => ({ fsPath: path, path }),
    },
    EndOfLine: {
        LF: 1,
        CRLF: 2,
    },
    CompletionItemKind: {
        Text: 1,
        Method: 2,
        Function: 3,
        Constructor: 4,
        Field: 5,
        Variable: 6,
        Class: 7,
        Interface: 8,
        Module: 9,
        Property: 10,
        Unit: 11,
        Value: 12,
        Enum: 13,
        Keyword: 14,
        Snippet: 15,
        Color: 16,
        File: 17,
        Reference: 18,
        Folder: 19,
        EnumMember: 20,
        Constant: 21,
        Struct: 22,
        Event: 23,
        Operator: 24,
        TypeParameter: 25,
    },
    CompletionTriggerKind: {
        Invoke: 0,
        TriggerCharacter: 1,
        TriggerForIncompleteCompletions: 2,
    },
    FoldingRange: class {
        constructor(public start: number, public end: number, public kind?: any) {}
    },
    FoldingRangeKind: {
        Comment: 1,
        Imports: 2,
        Region: 3,
    },
    MarkdownString: class {
        constructor(public value: string = "") {}
    },
    Hover: class {
        constructor(public contents: any, public range?: any) {}
    },
    CancellationTokenSource: class {
        token = {
            isCancellationRequested: false,
            onCancellationRequested: (() => {}) as any,
        };
        dispose() {}
    },
    SnippetString: class {
        constructor(public value: string = "") {}
    },
    workspace: {
        getConfiguration: jest.fn(),
    },
    extensions: {
        getExtension: jest.fn(),
    },
    TextEdit: {
        replace: (range: any, newText: string) => ({ range, newText }),
    },
};

// Make vscode available globally for tests
(global as any).vscode = mockVscode;

// Export the mock for module mapping
module.exports = mockVscode;

// Mock Node.js modules that might be used in tests
jest.mock("fs", () => ({
    existsSync: jest.fn(() => false),
    readFileSync: jest.fn(() => "{}"),
}));

jest.mock("path", () => ({
    resolve: jest.fn((...paths) => paths.join("/")),
    join: jest.fn((...paths) => paths.join("/")),
}));
