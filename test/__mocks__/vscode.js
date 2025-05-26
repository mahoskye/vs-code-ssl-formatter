// Mock implementation of VS Code API for Jest tests

// Position mock
class Position {
    constructor(line, character) {
        this.line = line;
        this.character = character;
    }

    isEqual(other) {
        return this.line === other.line && this.character === other.character;
    }

    isBefore(other) {
        return this.line < other.line || (this.line === other.line && this.character < other.character);
    }

    isAfter(other) {
        return this.line > other.line || (this.line === other.line && this.character > other.character);
    }

    translate(lineDelta = 0, characterDelta = 0) {
        return new Position(this.line + lineDelta, this.character + characterDelta);
    }

    with(line = this.line, character = this.character) {
        return new Position(line, character);
    }
}

// Range mock
class Range {
    constructor(startLine, startCharacter, endLine, endCharacter) {
        if (arguments.length === 2) {
            // Range(start: Position, end: Position)
            this.start = startLine;
            this.end = startCharacter;
        } else if (arguments.length === 4) {
            // Range(startLine: number, startCharacter: number, endLine: number, endCharacter: number)
            this.start = new Position(startLine, startCharacter);
            this.end = new Position(endLine, endCharacter);
        }
    }

    isEmpty() {
        return this.start.isEqual(this.end);
    }

    isSingleLine() {
        return this.start.line === this.end.line;
    }

    contains(positionOrRange) {
        if (positionOrRange.line !== undefined) {
            // Position
            return !positionOrRange.isBefore(this.start) && !positionOrRange.isAfter(this.end);
        } else {
            // Range
            return this.contains(positionOrRange.start) && this.contains(positionOrRange.end);
        }
    }

    intersection(range) {
        const start = this.start.isAfter(range.start) ? this.start : range.start;
        const end = this.end.isBefore(range.end) ? this.end : range.end;
        if (start.isAfter(end)) {
            return undefined;
        }
        return new Range(start, end);
    }

    union(other) {
        const start = this.start.isBefore(other.start) ? this.start : other.start;
        const end = this.end.isAfter(other.end) ? this.end : other.end;
        return new Range(start, end);
    }

    with(start = this.start, end = this.end) {
        return new Range(start, end);
    }
}

// Selection mock
class Selection extends Range {
    constructor(anchor, active) {
        if (arguments.length === 4) {
            // Selection(anchorLine: number, anchorCharacter: number, activeLine: number, activeCharacter: number)
            super(anchor, active, arguments[2], arguments[3]);
            this.anchor = new Position(anchor, active);
            this.active = new Position(arguments[2], arguments[3]);
        } else {
            // Selection(anchor: Position, active: Position)
            super(anchor, active);
            this.anchor = anchor;
            this.active = active;
        }
    }

    get isReversed() {
        return this.anchor.isAfter(this.active);
    }
}

// MarkdownString class
class MarkdownString {
    constructor(value, supportThemeIcons) {
        this.value = value || '';
        this.isTrusted = undefined;
        this.supportThemeIcons = supportThemeIcons;
        this.supportHtml = undefined;
        this.baseUri = undefined;
    }

    appendText(value) {
        this.value += value;
        return this;
    }

    appendMarkdown(value) {
        this.value += value;
        return this;
    }

    appendCodeblock(value, language) {
        this.value += '```' + (language || '') + '\n' + value + '\n```';
        return this;
    }
}

// Hover class
class Hover {
    constructor(contents, range) {
        this.contents = Array.isArray(contents) ? contents : [contents];
        this.range = range;
    }
}

// FoldingRange class
class FoldingRange {
    constructor(start, end, kind) {
        this.start = start;
        this.end = end;
        this.kind = kind;
    }
}

// CompletionItem class
class CompletionItem {
    constructor(label, kind) {
        this.label = label;
        this.kind = kind;
    }
}

const vscode = {
    // Uri mock
    Uri: {
        file: (path) => ({
            scheme: 'file',
            path,
            fsPath: path,
            toString: () => `file://${path}`
        }),
        parse: (uriString) => ({
            scheme: uriString.split(':')[0],
            path: uriString.split('://')[1] || uriString,
            toString: () => uriString
        })
    },

    // Classes
    Position,
    Range,
    Selection,
    MarkdownString,
    Hover,
    FoldingRange,
    CompletionItem,

    // EndOfLine enum
    EndOfLine: {
        LF: 1,
        CRLF: 2
    },

    // CompletionItemKind enum
    CompletionItemKind: {
        Text: 0,
        Method: 1,
        Function: 2,
        Constructor: 3,
        Field: 4,
        Variable: 5,
        Class: 6,
        Interface: 7,
        Module: 8,
        Property: 9,
        Unit: 10,
        Value: 11,
        Enum: 12,
        Keyword: 13,
        Snippet: 14,
        Color: 15,
        File: 16,
        Reference: 17,
        Folder: 18,
        EnumMember: 19,
        Constant: 20,
        Struct: 21,
        Event: 22,
        Operator: 23,
        TypeParameter: 24
    },

    // FoldingRangeKind enum
    FoldingRangeKind: {
        Comment: 1,
        Imports: 2,
        Region: 3
    },

    // Workspace mock
    workspace: {
        getConfiguration: jest.fn(() => ({
            get: jest.fn(),
            has: jest.fn(),
            inspect: jest.fn(),
            update: jest.fn()
        })),
        onDidChangeConfiguration: jest.fn(),
        workspaceFolders: [],
        getWorkspaceFolder: jest.fn(),
        asRelativePath: jest.fn(),
        findFiles: jest.fn(),
        openTextDocument: jest.fn(),
        applyEdit: jest.fn(),
        createFileSystemWatcher: jest.fn()
    },

    // Extensions mock
    extensions: {
        getExtension: jest.fn(() => ({
            extensionPath: '/mock/extension/path'
        })),
        all: []
    },

    // Languages mock
    languages: {
        registerCompletionItemProvider: jest.fn(),
        registerHoverProvider: jest.fn(),
        registerFoldingRangeProvider: jest.fn(),
        registerDocumentFormattingProvider: jest.fn(),
        registerDocumentRangeFormattingProvider: jest.fn(),
        setLanguageConfiguration: jest.fn()
    },

    // Window mock
    window: {
        showErrorMessage: jest.fn(),
        showWarningMessage: jest.fn(),
        showInformationMessage: jest.fn(),
        showQuickPick: jest.fn(),
        showInputBox: jest.fn(),
        createOutputChannel: jest.fn(() => ({
            appendLine: jest.fn(),
            show: jest.fn(),
            hide: jest.fn(),
            dispose: jest.fn()
        })),
        activeTextEditor: undefined,
        visibleTextEditors: [],
        onDidChangeActiveTextEditor: jest.fn(),
        onDidChangeVisibleTextEditors: jest.fn()
    },

    // Commands mock
    commands: {
        registerCommand: jest.fn(),
        executeCommand: jest.fn(),
        getCommands: jest.fn()
    },

    // CancellationToken mock
    CancellationToken: {
        None: {
            isCancellationRequested: false,
            onCancellationRequested: jest.fn()
        }
    }
};

module.exports = vscode;
