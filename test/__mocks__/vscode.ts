import * as jestMock from "jest-mock";

// Uri class defined earlier as it's used by other classes like MarkdownString
export class Uri {
    public readonly scheme: string;
    public readonly authority: string;
    public readonly path: string;
    public readonly query: string;
    public readonly fragment: string;
    public readonly fsPath: string;

    private constructor(
        scheme: string,
        authority: string,
        path: string,
        query: string,
        fragment: string
    ) {
        this.scheme = scheme;
        this.authority = authority;
        this.path = path;
        this.query = query;
        this.fragment = fragment;
        // In a real scenario, fsPath might need more complex logic, especially for non-'file' schemes
        this.fsPath = scheme === "file" ? path : "/[uri-not-a-file-path]/" + path;
    }

    static file(path: string): Uri {
        // Normalize path separators for consistency if needed, though mock might keep it simple
        return new Uri("file", "", path, "", "");
    }

    static parse(value: string, strict?: boolean): Uri {
        const schemeMatch = value.match(/^([^:]+):/);
        let scheme = "file";
        let pathPart = value;

        if (schemeMatch) {
            scheme = schemeMatch[1];
            pathPart = value.substring(scheme.length + 1);
            if (pathPart.startsWith("//")) {
                // very simplified authority and path extraction
                const authorityEnd = pathPart.indexOf("/", 2);
                if (authorityEnd !== -1) {
                    // this.authority = pathPart.substring(2, authorityEnd);
                    // pathPart = pathPart.substring(authorityEnd);
                    // For mock, keep it simple
                }
            }
        }
        // Simplified fsPath for mock
        const fsPath = pathPart.startsWith("//") ? pathPart.substring(2) : pathPart;
        return new Uri(scheme, "", fsPath, "", "");
    }

    with(change: {
        scheme?: string;
        authority?: string;
        path?: string;
        query?: string;
        fragment?: string;
    }): Uri {
        return new Uri(
            change.scheme !== undefined ? change.scheme : this.scheme,
            change.authority !== undefined ? change.authority : this.authority,
            change.path !== undefined ? change.path : this.path,
            change.query !== undefined ? change.query : this.query,
            change.fragment !== undefined ? change.fragment : this.fragment
        );
    }

    toString(skipEncoding?: boolean): string {
        let res = `${this.scheme}:`;
        if (this.authority || this.scheme === "file") {
            res += `//${this.authority}`;
        }
        res += this.path;
        if (this.query) {
            res += `?${this.query}`;
        }
        if (this.fragment) {
            res += `#${this.fragment}`;
        }
        return res;
    }

    toJSON(): any {
        return {
            scheme: this.scheme,
            authority: this.authority,
            path: this.path,
            query: this.query,
            fragment: this.fragment,
            fsPath: this.fsPath,
        };
    }
}

// Position mock
export class Position {
    constructor(public readonly line: number, public readonly character: number) {}

    isEqual(other: Position): boolean {
        return this.line === other.line && this.character === other.character;
    }

    isBefore(other: Position): boolean {
        return (
            this.line < other.line || (this.line === other.line && this.character < other.character)
        );
    }

    isAfter(other: Position): boolean {
        return (
            this.line > other.line || (this.line === other.line && this.character > other.character)
        );
    }

    translate(lineDelta: number = 0, characterDelta: number = 0): Position {
        return new Position(this.line + lineDelta, this.character + characterDelta);
    }

    with(line: number = this.line, character: number = this.character): Position {
        return new Position(line, character);
    }
}

// Range mock
export class Range {
    public readonly start: Position;
    public readonly end: Position;

    constructor(
        startLineOrPosition: number | Position,
        startCharacterOrPosition: number | Position,
        endLine?: number,
        endCharacter?: number
    ) {
        if (
            typeof startLineOrPosition === "number" &&
            typeof startCharacterOrPosition === "number" &&
            typeof endLine === "number" &&
            typeof endCharacter === "number"
        ) {
            this.start = new Position(startLineOrPosition, startCharacterOrPosition);
            this.end = new Position(endLine, endCharacter);
        } else if (
            startLineOrPosition instanceof Position &&
            startCharacterOrPosition instanceof Position
        ) {
            this.start = startLineOrPosition;
            this.end = startCharacterOrPosition;
        } else {
            throw new Error("Invalid Range constructor arguments");
        }
    }

    get isEmpty(): boolean {
        return this.start.isEqual(this.end);
    }

    get isSingleLine(): boolean {
        return this.start.line === this.end.line;
    }

    contains(positionOrRange: Position | Range): boolean {
        if (positionOrRange instanceof Position) {
            return !positionOrRange.isBefore(this.start) && !positionOrRange.isAfter(this.end);
        } else {
            return this.contains(positionOrRange.start) && this.contains(positionOrRange.end);
        }
    }

    intersection(range: Range): Range | undefined {
        const start = this.start.isAfter(range.start) ? this.start : range.start;
        const end = this.end.isBefore(range.end) ? this.end : range.end;
        if (start.isAfter(end)) {
            return undefined;
        }
        return new Range(start, end);
    }

    union(other: Range): Range {
        const start = this.start.isBefore(other.start) ? this.start : other.start;
        const end = this.end.isAfter(other.end) ? this.end : other.end;
        return new Range(start, end);
    }

    with(start: Position = this.start, end: Position = this.end): Range {
        return new Range(start, end);
    }
}

// Selection mock
export class Selection extends Range {
    public readonly anchor: Position;
    public readonly active: Position;

    constructor(
        anchorLineOrPosition: number | Position,
        anchorCharacterOrPosition: number | Position,
        activeLineOrPosition: number | Position,
        activeCharacter?: number
    ) {
        if (
            typeof anchorLineOrPosition === "number" &&
            typeof anchorCharacterOrPosition === "number" &&
            typeof activeLineOrPosition === "number" &&
            typeof activeCharacter === "number"
        ) {
            super(
                anchorLineOrPosition,
                anchorCharacterOrPosition,
                activeLineOrPosition,
                activeCharacter
            );
            this.anchor = new Position(anchorLineOrPosition, anchorCharacterOrPosition);
            this.active = new Position(activeLineOrPosition, activeCharacter);
        } else if (
            anchorLineOrPosition instanceof Position &&
            anchorCharacterOrPosition instanceof Position &&
            activeLineOrPosition instanceof Position
        ) {
            // The constructor signature is (anchor: Position, active: Position)
            super(anchorLineOrPosition, activeLineOrPosition); // Range constructor takes (start, end)
            this.anchor = anchorLineOrPosition;
            this.active = activeLineOrPosition; // Corrected: active should be the second Position argument
        } else {
            throw new Error("Invalid Selection constructor arguments");
        }
    }

    get isReversed(): boolean {
        return this.anchor.isAfter(this.active);
    }
}

// MarkdownString class
export class MarkdownString {
    public isTrusted?: boolean | { readonly enabledCommands: readonly string[] };
    public supportThemeIcons?: boolean;
    public supportHtml?: boolean;
    public baseUri?: Uri; // Correctly typed with Uri defined above
    constructor(public value: string = "", supportThemeIcons: boolean = false) {
        this.supportThemeIcons = supportThemeIcons;
    }

    appendText(value: string): MarkdownString {
        this.value += value;
        return this;
    }

    appendMarkdown(value: string): MarkdownString {
        this.value += value;
        return this;
    }
    appendCodeblock(value: string, language?: string): MarkdownString {
        this.value += `\`\`\`${language || ""}\n${value}\n\`\`\``;
        return this;
    }
}

// Hover class
export class Hover {
    public contents: MarkdownString[];
    constructor(
        contents:
            | MarkdownString
            | MarkdownString[]
            | string
            | { language: string; value: string }[],
        public range?: Range
    ) {
        if (Array.isArray(contents)) {
            this.contents = contents.map((c) => {
                if (typeof c === "string") {
                    return new MarkdownString(c);
                }
                if (c instanceof MarkdownString) {
                    return c;
                }
                const langVal = c as { language: string; value: string };
                return new MarkdownString(`\`\`\`${langVal.language}\n${langVal.value}\n\`\`\``);
            });
        } else if (typeof contents === "string") {
            this.contents = [new MarkdownString(contents)];
        } else if (contents instanceof MarkdownString) {
            this.contents = [contents];
        } else {
            // { language: string; value: string }
            const langVal = contents as { language: string; value: string };
            this.contents = [
                new MarkdownString(`\`\`\`${langVal.language}\n${langVal.value}\n\`\`\``),
            ];
        }
    }
}

// FoldingRange class
export class FoldingRange {
    constructor(public start: number, public end: number, public kind?: FoldingRangeKind) {}
}

// CompletionItem class
export class CompletionItem {
    public detail?: string;
    public documentation?: string | MarkdownString;
    public sortText?: string;
    public filterText?: string;
    public insertText?: string | SnippetString;
    public range?: Range | { inserting: Range; replacing: Range };
    public commitCharacters?: string[];
    public command?: Command;
    public additionalTextEdits?: TextEdit[];
    public preselect?: boolean;
    public keepWhitespace?: boolean;
    public label: string | CompletionItemLabel;

    constructor(label: string | CompletionItemLabel, public kind?: CompletionItemKind) {
        this.label = label;
    }
}

export interface CompletionItemLabel {
    label: string;
    detail?: string;
    description?: string;
}

export interface TextDocument {
    uri: Uri;
    fileName: string;
    isUntitled: boolean;
    languageId: string;
    version: number;
    isDirty: boolean;
    isClosed: boolean;
    save(): Thenable<boolean>;
    eol: EndOfLine;
    lineCount: number;
    lineAt(lineOrPosition: number | Position): TextLine;
    offsetAt(position: Position): number;
    positionAt(offset: number): Position;
    getText(range?: Range): string;
    getWordRangeAtPosition(position: Position, regex?: RegExp): Range | undefined;
    validateRange(range: Range): Range;
    validatePosition(position: Position): Position;
}

export interface Thenable<T> {
    then<TResult>(
        onfulfilled?: (value: T) => TResult | Thenable<TResult>,
        onrejected?: (reason: any) => TResult | Thenable<TResult>
    ): Thenable<TResult>;
    then<TResult>(
        onfulfilled?: (value: T) => TResult | Thenable<TResult>,
        onrejected?: (reason: any) => void
    ): Thenable<TResult>;
}

export interface TextLine {
    readonly lineNumber: number;
    readonly text: string;
    readonly range: Range;
    readonly rangeIncludingLineBreak: Range;
    readonly firstNonWhitespaceCharacterIndex: number;
    readonly isEmptyOrWhitespace: boolean;
}

export interface TextEditor {
    document: TextDocument;
    selection: Selection;
    selections: Selection[];
    readonly visibleRanges: Range[];
    options: TextEditorOptions;
    readonly viewColumn?: ViewColumn;
    edit(
        callback: (editBuilder: TextEditorEdit) => void,
        options?: { undoStopBefore: boolean; undoStopAfter: boolean }
    ): Thenable<boolean>;
    insertSnippet(
        snippet: SnippetString,
        location?: Range | Position | readonly Range[] | readonly Position[],
        options?: { undoStopBefore: boolean; undoStopAfter: boolean }
    ): Thenable<boolean>;
    setDecorations(
        decorationType: TextEditorDecorationType,
        rangesOrOptions: Range[] | DecorationOptions[]
    ): void;
    revealRange(range: Range, revealType?: TextEditorRevealType): void;
    show(column?: ViewColumn): void;
    hide(): void;
}

export interface TextEditorOptions {
    tabSize: number | string;
    insertSpaces: boolean | string;
    cursorStyle?: TextEditorCursorStyle;
    lineNumbers?: TextEditorLineNumbersType;
}

export enum TextEditorCursorStyle {
    Line = 1,
    Block = 2,
    Underline = 3,
    LineThin = 4,
    BlockOutline = 5,
    UnderlineThin = 6,
}

export enum TextEditorLineNumbersType {
    Off = 0,
    On = 1,
    Relative = 2,
}

export interface TextEditorEdit {
    replace(location: Position | Range | Selection, value: string): void;
    insert(location: Position, value: string): void;
    delete(location: Range | Selection): void;
    setEndOfLine(eol: EndOfLine): void;
}

export class SnippetString {
    constructor(public value: string) {}
}

export interface TextEditorDecorationType {}
export interface DecorationOptions {
    range: Range;
    hoverMessage?: MarkdownString | MarkdownString[];
    renderOptions?: DecorationRenderOptions;
}
export interface DecorationRenderOptions extends ThemableDecorationRenderOptions {
    isWholeLine?: boolean;
    overviewRulerLane?: OverviewRulerLane;
    light?: ThemableDecorationRenderOptions;
    dark?: ThemableDecorationRenderOptions;
}
export interface ThemableDecorationRenderOptions {
    backgroundColor?: string | ThemeColor;
    borderColor?: string | ThemeColor;
    color?: string | ThemeColor;
    cursor?: string;
    fontStyle?: string;
    fontWeight?: string;
    gutterIconPath?: string | Uri;
    gutterIconSize?: string;
    letterSpacing?: string;
    outline?: string;
    outlineColor?: string | ThemeColor;
    outlineStyle?: string;
    outlineWidth?: string;
    overviewRulerColor?: string | ThemeColor;
    textDecoration?: string;
}
export class ThemeColor {
    constructor(public id: string) {}
}
export enum OverviewRulerLane {
    Left = 1,
    Center = 2,
    Right = 4,
    Full = 7,
}
export enum TextEditorRevealType {
    Default = 0,
    InCenter = 1,
    InCenterIfOutsideViewport = 2,
    AtTop = 3,
}
export enum ViewColumn {
    Active = -1,
    Beside = -2,
    One = 1,
    Two = 2,
    Three = 3,
    Four = 4,
    Five = 5,
    Six = 6,
    Seven = 7,
    Eight = 8,
    Nine = 9,
}

export interface Disposable {
    dispose(): void;
}

export type Event<T> = (
    listener: (e: T) => any,
    thisArgs?: any,
    disposables?: Disposable[]
) => Disposable;

export class EventEmitter<T> implements Disposable {
    private _listeners: Array<{ listener: (e: T) => any; thisArgs?: any }> = [];
    private _event?: Event<T>;

    get event(): Event<T> {
        if (!this._event) {
            this._event = (
                listener: (e: T) => any,
                thisArgs?: any,
                disposables?: Disposable[]
            ): Disposable => {
                const item = { listener, thisArgs };
                this._listeners.push(item);

                const disposable: Disposable = {
                    dispose: () => {
                        const index = this._listeners.indexOf(item);
                        if (index !== -1) {
                            this._listeners.splice(index, 1);
                        }
                    },
                };

                if (disposables) {
                    disposables.push(disposable);
                }
                return disposable;
            };
        }
        return this._event;
    }

    fire(data: T): void {
        [...this._listeners].forEach((item) => item.listener.call(item.thisArgs, data));
    }

    dispose(): void {
        this._listeners = [];
        this._event = undefined;
    }
}

export interface QuickPickItem {
    label: string;
    description?: string;
    detail?: string;
    picked?: boolean;
    alwaysShow?: boolean;
}

export interface InputBoxOptions {
    value?: string;
    valueSelection?: [number, number];
    prompt?: string;
    placeHolder?: string;
    password?: boolean;
    ignoreFocusOut?: boolean;
    validateInput?(value: string): string | Thenable<string | null | undefined> | null | undefined;
}

export interface OpenDialogOptions {
    defaultUri?: Uri;
    openLabel?: string;
    canSelectFiles?: boolean;
    canSelectFolders?: boolean;
    canSelectMany?: boolean;
    filters?: { [name: string]: string[] };
}

export interface SaveDialogOptions {
    defaultUri?: Uri;
    saveLabel?: string;
    filters?: { [name: string]: string[] };
}

export interface OutputChannel {
    readonly name: string;
    append(value: string): void;
    appendLine(value: string): void;
    clear(): void;
    show(preserveFocusOrColumn?: boolean | ViewColumn, preserveFocus?: boolean): void;
    hide(): void;
    dispose(): void;
}

export interface WorkspaceConfiguration {
    get<T>(section: string): T | undefined;
    get<T>(section: string, defaultValue: T): T;
    has(section: string): boolean;
    inspect<T>(section: string):
        | {
              key: string;
              defaultValue?: T;
              globalValue?: T;
              workspaceValue?: T;
              workspaceFolderValue?: T;
              defaultLanguageValue?: T;
              globalLanguageValue?: T;
              workspaceLanguageValue?: T;
              workspaceFolderLanguageValue?: T;
              languageIds?: string[];
          }
        | undefined;
    update(
        section: string,
        value: any,
        configurationTarget?: ConfigurationTarget | boolean,
        overrideInLanguage?: boolean
    ): Thenable<void>;
}

export enum ConfigurationTarget {
    Global = 1,
    Workspace = 2,
    WorkspaceFolder = 3,
}

export interface FileSystemWatcher extends Disposable {
    ignoreCreateEvents: boolean;
    ignoreChangeEvents: boolean;
    ignoreDeleteEvents: boolean;
    onDidCreate: Event<Uri>;
    onDidChange: Event<Uri>;
    onDidDelete: Event<Uri>;
}

export interface FormattingOptions {
    tabSize: number;
    insertSpaces: boolean;
    [key: string]: boolean | number | string;
}

export interface Extension<T> {
    readonly id: string;
    readonly extensionUri: Uri;
    readonly extensionPath: string;
    readonly isActive: boolean;
    readonly packageJSON: any;
    readonly extensionKind: ExtensionKind;
    readonly exports: T;
    activate(): Thenable<T>;
}

export enum ExtensionKind {
    UI = 1,
    Workspace = 2,
}

export interface Command {
    title: string;
    command: string;
    tooltip?: string;
    arguments?: any[];
}

export interface TextEdit {
    range: Range;
    newText: string;
    newEol?: EndOfLine;
}
export class WorkspaceEdit implements Disposable {
    entries(): [Uri, TextEdit[]][] {
        return [];
    }
    replace(uri: Uri, range: Range, newText: string): void {}
    insert(uri: Uri, position: Position, newText: string): void {}
    delete(uri: Uri, range: Range): void {}
    createFile(uri: Uri, options?: { overwrite?: boolean; ignoreIfExists?: boolean }): void {}
    deleteFile(uri: Uri, options?: { recursive?: boolean; ignoreIfNotExists?: boolean }): void {}
    renameFile(
        oldUri: Uri,
        newUri: Uri,
        options?: { overwrite?: boolean; ignoreIfExists?: boolean }
    ): void {}
    get size(): number {
        return 0;
    }
    set(uri: Uri, edits: TextEdit[]): void {} // Simplified
    dispose(): void {}
}

export enum DiagnosticSeverity {
    Error = 0,
    Warning = 1,
    Information = 2,
    Hint = 3,
}

export enum SymbolKind {
    File = 0,
    Module = 1,
    Namespace = 2,
    Package = 3,
    Class = 4,
    Method = 5,
    Property = 6,
    Field = 7,
    Constructor = 8,
    Enum = 9,
    Interface = 10,
    Function = 11,
    Variable = 12,
    Constant = 13,
    String = 14,
    Number = 15,
    Boolean = 16,
    Array = 17,
    Object = 18,
    Key = 19,
    Null = 20,
    EnumMember = 21,
    Struct = 22,
    Event = 23,
    Operator = 24,
    TypeParameter = 25,
}

// EndOfLine enum
export enum EndOfLine {
    LF = 1,
    CRLF = 2,
}

// CompletionItemKind enum
export enum CompletionItemKind {
    Text = 0,
    Method = 1,
    Function = 2,
    Constructor = 3,
    Field = 4,
    Variable = 5,
    Class = 6,
    Interface = 7,
    Module = 8,
    Property = 9,
    Unit = 10,
    Value = 11,
    Enum = 12,
    Keyword = 13,
    Snippet = 14,
    Color = 15,
    File = 16,
    Reference = 17,
    Folder = 18,
    EnumMember = 19,
    Constant = 20,
    Struct = 21,
    Event = 22,
    Operator = 23,
    TypeParameter = 24,
    User = 25,
    Issue = 26,
}

// FoldingRangeKind enum
export enum FoldingRangeKind {
    Comment = 1,
    Imports = 2,
    Region = 3,
}

// CancellationToken mock
export interface CancellationToken {
    isCancellationRequested: boolean;
    onCancellationRequested: Event<any>; // Correctly typed with Event<any>
}

export const CancellationTokenNone: CancellationToken = {
    isCancellationRequested: false,
    onCancellationRequested: new EventEmitter<any>().event,
};

// CancellationTokenSource mock
export class CancellationTokenSource implements Disposable {
    private _isCancelled = false;
    private _onCancellationRequestedEmitter = new EventEmitter<any>();
    public readonly token: CancellationToken;

    constructor() {
        // Create a stable token reference
        const self = this;
        this.token = {
            get isCancellationRequested() {
                return self._isCancelled;
            },
            onCancellationRequested: this._onCancellationRequestedEmitter.event,
        };
    }

    cancel(): void {
        if (!this._isCancelled) {
            this._isCancelled = true;
            this._onCancellationRequestedEmitter.fire(undefined);
        }
    }

    dispose(): void {
        if (!this._isCancelled) {
            this.cancel();
        }
        this._onCancellationRequestedEmitter.dispose();
    }
}
// --- Mocked VSCode API Modules ---

export const workspace = {
    getConfiguration: jestMock.fn(() => ({
        get: jestMock.fn(),
        has: jestMock.fn(),
        inspect: jestMock.fn(),
        update: jestMock.fn(() => Promise.resolve()),
    })),
    onDidChangeConfiguration: new EventEmitter<void>().event,
    workspaceFolders: [] as { uri: Uri; name: string; index: number }[] | undefined,
    getWorkspaceFolder: jestMock.fn((_uri: Uri) => undefined),
    asRelativePath: jestMock.fn((pathOrUri: string | Uri) =>
        typeof pathOrUri === "string" ? pathOrUri : pathOrUri.fsPath
    ),
    findFiles: jestMock.fn(() => Promise.resolve([])),
    openTextDocument: jestMock.fn(
        async (
            uriOrPathOrContent?: Uri | string | { language?: string; content?: string }
        ): Promise<TextDocument> => {
            let uri: Uri;
            let content: string | undefined;
            let languageId = "plaintext";

            if (uriOrPathOrContent instanceof Uri) {
                uri = uriOrPathOrContent;
            } else if (typeof uriOrPathOrContent === "string") {
                uri = Uri.file(uriOrPathOrContent);
            } else if (uriOrPathOrContent && typeof uriOrPathOrContent === "object") {
                // Use backticks for template literals, not escaped
                uri = Uri.parse(`untitled:NewDocument-${Date.now()}`);
                content = uriOrPathOrContent.content;
                languageId = uriOrPathOrContent.language || languageId;
            } else {
                uri = Uri.parse(`untitled:NewDocument-${Date.now()}`);
            }
            const documentContent = content || "";
            return Promise.resolve({
                uri,
                fileName: uri.fsPath,
                isUntitled: uri.scheme === "untitled",
                languageId: languageId,
                version: 1,
                isDirty: false,
                isClosed: false,
                save: jestMock.fn(() => Promise.resolve(true)),
                eol: EndOfLine.LF,
                lineCount: documentContent.split("\n").length,
                lineAt: jestMock.fn((lineOrPosition: number | Position) => {
                    const lineNumber =
                        typeof lineOrPosition === "number" ? lineOrPosition : lineOrPosition.line;
                    const text = documentContent.split("\n")[lineNumber] || "";
                    return {
                        lineNumber,
                        text,
                        range: new Range(lineNumber, 0, lineNumber, text.length),
                        rangeIncludingLineBreak: new Range(
                            lineNumber,
                            0,
                            lineNumber,
                            text.length +
                                (lineNumber < documentContent.split("\n").length - 1 ? 1 : 0)
                        ),
                        firstNonWhitespaceCharacterIndex: text.search(/\S|$/),
                        isEmptyOrWhitespace: text.trim().length === 0,
                    };
                }),
                offsetAt: jestMock.fn((position: Position) => {
                    const lines = documentContent.split("\n");
                    let offset = 0;
                    for (let i = 0; i < position.line; i++) {
                        offset += lines[i].length + 1; // +1 for newline character
                    }
                    offset += position.character;
                    return offset;
                }),
                positionAt: jestMock.fn((offset: number) => {
                    const lines = documentContent.split("\n");
                    let currentOffset = 0;
                    for (let line = 0; line < lines.length; line++) {
                        const lineLength = lines[line].length + 1; // +1 for newline
                        if (currentOffset + lineLength > offset) {
                            return new Position(line, offset - currentOffset);
                        }
                        currentOffset += lineLength;
                    }
                    // Should not happen if offset is valid
                    return new Position(lines.length - 1, lines[lines.length - 1].length);
                }),
                getText: jestMock.fn((range?: Range) => {
                    if (!range) {
                        return documentContent;
                    }
                    const lines = documentContent.split("\n");
                    if (range.start.line === range.end.line) {
                        return lines[range.start.line].substring(
                            range.start.character,
                            range.end.character
                        );
                    }
                    let text = lines[range.start.line].substring(range.start.character);
                    for (let i = range.start.line + 1; i < range.end.line; i++) {
                        text += "\n" + lines[i];
                    }
                    text += "\n" + lines[range.end.line].substring(0, range.end.character);
                    return text;
                }),
                getWordRangeAtPosition: jestMock.fn(),
                validateRange: jestMock.fn((range: Range) => range),
                validatePosition: jestMock.fn((position: Position) => position),
            } as TextDocument);
        }
    ),
    applyEdit: jestMock.fn(() => Promise.resolve(true)),
    createFileSystemWatcher: jestMock.fn(
        (globPattern, ignoreCreateEvents, ignoreChangeEvents, ignoreDeleteEvents) => ({
            ignoreCreateEvents: !!ignoreCreateEvents,
            ignoreChangeEvents: !!ignoreChangeEvents,
            ignoreDeleteEvents: !!ignoreDeleteEvents,
            onDidCreate: new EventEmitter<Uri>().event,
            onDidChange: new EventEmitter<Uri>().event,
            onDidDelete: new EventEmitter<Uri>().event,
            dispose: jestMock.fn(),
        })
    ),
    onDidOpenTextDocument: new EventEmitter<TextDocument>().event,
    onDidCloseTextDocument: new EventEmitter<TextDocument>().event,
    onDidChangeTextDocument: new EventEmitter<any>().event, // Simplified: TextDocumentChangeEvent
    onDidSaveTextDocument: new EventEmitter<TextDocument>().event,
    onWillSaveTextDocument: new EventEmitter<any>().event, // Simplified: TextDocumentWillSaveEvent
    onWillCreateFiles: new EventEmitter<any>().event, // Simplified: FileWillCreateEvent
    onDidCreateFiles: new EventEmitter<any>().event, // Simplified: FileCreateEvent
    onWillDeleteFiles: new EventEmitter<any>().event, // Simplified: FileWillDeleteEvent
    onDidDeleteFiles: new EventEmitter<any>().event, // Simplified: FileDeleteEvent
    onWillRenameFiles: new EventEmitter<any>().event, // Simplified: FileWillRenameEvent
    onDidRenameFiles: new EventEmitter<any>().event, // Simplified: FileRenameEvent
    fs: {
        // Mock basic fs operations
        stat: jestMock.fn((uri: Uri) => Promise.resolve({ type: 0, ctime: 0, mtime: 0, size: 0 })), // FileType.Unknown = 0
        readDirectory: jestMock.fn((uri: Uri) => Promise.resolve([])), // [name: string, type: FileType][]
        readFile: jestMock.fn((uri: Uri) => Promise.resolve(new Uint8Array())),
        writeFile: jestMock.fn((uri: Uri, content: Uint8Array) => Promise.resolve()),
        delete: jestMock.fn((uri: Uri, options?: { recursive?: boolean; useTrash?: boolean }) =>
            Promise.resolve()
        ),
        createDirectory: jestMock.fn((uri: Uri) => Promise.resolve()),
        rename: jestMock.fn((source: Uri, target: Uri, options?: { overwrite?: boolean }) =>
            Promise.resolve()
        ),
        copy: jestMock.fn((source: Uri, target: Uri, options?: { overwrite?: boolean }) =>
            Promise.resolve()
        ),
        isWritableFileSystem: jestMock.fn((scheme: string) => undefined), // undefined means unknown
    },
};

export const extensions = {
    getExtension: jestMock.fn((extensionId: string) => {
        if (extensionId === "ms-vscode.test-provider") {
            // Example for testing
            return {
                id: extensionId,
                extensionUri: Uri.file("/mock/extension/path"),
                extensionPath: "/mock/extension/path",
                isActive: false,
                packageJSON: { name: "test-provider", version: "0.0.1" },
                extensionKind: ExtensionKind.Workspace,
                exports: {},
                activate: jestMock.fn(() => Promise.resolve({})),
            };
        }
        return undefined;
    }),
    all: [] as Extension<any>[],
    onDidChange: new EventEmitter<void>().event,
};

export const languages = {
    registerCompletionItemProvider: jestMock.fn(() => ({ dispose: jestMock.fn() })),
    registerHoverProvider: jestMock.fn(() => ({ dispose: jestMock.fn() })),
    registerFoldingRangeProvider: jestMock.fn(() => ({ dispose: jestMock.fn() })),
    registerDocumentFormattingEditProvider: jestMock.fn(() => ({ dispose: jestMock.fn() })),
    registerDocumentRangeFormattingEditProvider: jestMock.fn(() => ({ dispose: jestMock.fn() })),
    setLanguageConfiguration: jestMock.fn(() => ({ dispose: jestMock.fn() })),
    createDiagnosticCollection: jestMock.fn((name?: string) => ({
        name,
        set: jestMock.fn(),
        delete: jestMock.fn(),
        clear: jestMock.fn(),
        dispose: jestMock.fn(),
        get: jestMock.fn(() => []),
    })),
    getLanguages: jestMock.fn(() => Promise.resolve(["plaintext", "javascript", "typescript"])),
    onDidChangeDiagnostics: new EventEmitter<readonly Uri[]>().event,
    // ... other language features can be added here
};

export const window = {
    showErrorMessage: jestMock.fn((message: string, ...items: any[]) =>
        Promise.resolve(items.length > 0 ? items[0] : undefined)
    ),
    showWarningMessage: jestMock.fn((message: string, ...items: any[]) =>
        Promise.resolve(items.length > 0 ? items[0] : undefined)
    ),
    showInformationMessage: jestMock.fn((message: string, ...items: any[]) =>
        Promise.resolve(items.length > 0 ? items[0] : undefined)
    ),
    showQuickPick: jestMock.fn(
        (items: any[] | Thenable<any[]>, options?: any, token?: CancellationToken) =>
            Promise.resolve(Array.isArray(items) && items.length > 0 ? items[0] : undefined)
    ),
    showInputBox: jestMock.fn((options?: InputBoxOptions, token?: CancellationToken) =>
        Promise.resolve(options?.value)
    ),
    createOutputChannel: jestMock.fn((name: string) => ({
        name,
        append: jestMock.fn(),
        appendLine: jestMock.fn(),
        clear: jestMock.fn(),
        show: jestMock.fn(),
        hide: jestMock.fn(),
        dispose: jestMock.fn(),
    })),
    activeTextEditor: undefined as TextEditor | undefined,
    visibleTextEditors: [] as TextEditor[],
    onDidChangeActiveTextEditor: new EventEmitter<TextEditor | undefined>().event,
    onDidChangeVisibleTextEditors: new EventEmitter<readonly TextEditor[]>().event,
    onDidChangeTextEditorSelection: new EventEmitter<any>().event, // Simplified: TextEditorSelectionChangeEvent
    onDidChangeTextEditorVisibleRanges: new EventEmitter<any>().event, // Simplified: TextEditorVisibleRangesChangeEvent
    onDidChangeTextEditorOptions: new EventEmitter<any>().event, // Simplified: TextEditorOptionsChangeEvent
    onDidChangeTextEditorViewColumn: new EventEmitter<any>().event, // Simplified: TextEditorViewColumnChangeEvent
    showTextDocument: jestMock.fn(
        (
            documentOrUri: TextDocument | Uri,
            columnOrOptions?: ViewColumn | any,
            preserveFocus?: boolean
        ) => {
            // Simplified: returns a mock TextEditor
            const doc =
                documentOrUri instanceof Uri
                    ? workspace.openTextDocument(documentOrUri)
                    : Promise.resolve(documentOrUri);
            return doc.then(
                (d) =>
                    ({
                        document: d,
                        selection: new Selection(0, 0, 0, 0),
                        selections: [new Selection(0, 0, 0, 0)],
                        visibleRanges: [new Range(0, 0, d.lineCount, 0)],
                        options: { tabSize: 4, insertSpaces: true },
                        viewColumn:
                            typeof columnOrOptions === "number"
                                ? columnOrOptions
                                : ViewColumn.Active,
                        edit: jestMock.fn(),
                        insertSnippet: jestMock.fn(),
                        setDecorations: jestMock.fn(),
                        revealRange: jestMock.fn(),
                        show: jestMock.fn(),
                        hide: jestMock.fn(),
                    } as TextEditor)
            );
        }
    ),
    showOpenDialog: jestMock.fn((options: OpenDialogOptions) => Promise.resolve(undefined)), // Returns Uri[] | undefined
    showSaveDialog: jestMock.fn((options: SaveDialogOptions) => Promise.resolve(undefined)), // Returns Uri | undefined
    createStatusBarItem: jestMock.fn((alignment?: any, priority?: number) => ({
        // StatusBarAlignment
        alignment: alignment || 1, // StatusBarAlignment.Left
        priority: priority || 0,
        text: "",
        tooltip: "",
        color: "",
        command: "",
        show: jestMock.fn(),
        hide: jestMock.fn(),
        dispose: jestMock.fn(),
    })),
    createTreeView: jestMock.fn((viewId: string, options: { treeDataProvider: any }) => ({
        // TreeView<T>
        viewId,
        options,
        reveal: jestMock.fn(),
        dispose: jestMock.fn(),
        // ... other TreeView properties/methods
    })),
    // ... other window properties/methods
    // Example: Add a state mock for window state
    state: {
        focused: true,
        onDidChangeWindowState: new EventEmitter<{ focused: boolean }>().event,
    },
    // Terminal related mocks
    createTerminal: jestMock.fn((nameOrOptions?: string | any) => ({
        // TerminalOptions
        name:
            typeof nameOrOptions === "string"
                ? nameOrOptions
                : nameOrOptions?.name || "MockTerminal",
        processId: Promise.resolve(12345), // Mock process ID
        sendText: jestMock.fn(),
        show: jestMock.fn(),
        hide: jestMock.fn(),
        dispose: jestMock.fn(),
        creationOptions: typeof nameOrOptions === "object" ? nameOrOptions : {},
    })),
    activeTerminal: undefined as any | undefined, // Terminal
    terminals: [] as readonly any[], // Terminal[]
    onDidChangeActiveTerminal: new EventEmitter<any | undefined>().event, // Terminal
    onDidOpenTerminal: new EventEmitter<any>().event, // Terminal
    onDidCloseTerminal: new EventEmitter<any>().event, // Terminal
};

export const commands = {
    registerCommand: jestMock.fn(
        (command: string, callback: (...args: any[]) => any, thisArg?: any) => ({
            dispose: jestMock.fn(),
        })
    ),
    executeCommand: jestMock.fn((command: string, ...rest: any[]) => Promise.resolve(undefined)),
    getCommands: jestMock.fn((filterInternal?: boolean) => Promise.resolve([])),
};

// For things like Progress
export enum ProgressLocation {
    SourceControl = 1,
    Window = 10,
    Notification = 15,
}

export interface ProgressOptions {
    location: ProgressLocation | { viewId: string };
    title?: string;
    cancellable?: boolean;
}

export interface Progress<T> {
    report(value: T): void;
}

// Add a mock for withProgress
(window as any).withProgress = jestMock.fn(
    async <R>(
        options: ProgressOptions,
        task: (
            progress: Progress<{ message?: string; increment?: number }>,
            token: CancellationToken
        ) => Thenable<R>
    ): Promise<R> => {
        const progressMock: Progress<{ message?: string; increment?: number }> = {
            report: jestMock.fn(),
        };
        const cancellationTokenSource = new CancellationTokenSource();
        try {
            if (options.cancellable) {
                // Example: Simulate cancellation for testing purposes if needed
                // setTimeout(() => cancellationTokenSource.cancel(), 50);
            }
            const result = await task(progressMock, cancellationTokenSource.token);
            return result;
        } finally {
            cancellationTokenSource.dispose();
        }
    }
);

const vscode = {
    Uri,
    Position,
    Range,
    Selection,
    MarkdownString,
    Hover,
    FoldingRange,
    CompletionItem,
    EndOfLine,
    CompletionItemKind,
    FoldingRangeKind,
    DiagnosticSeverity,
    SymbolKind,
    CancellationTokenSource,
    CancellationToken: CancellationTokenNone,
    workspace,
    extensions,
    languages,
    window,
    commands,
    ProgressLocation,
    ThemeColor,
    OverviewRulerLane,
    TextEditorRevealType,
    ViewColumn,
    ConfigurationTarget,
    ExtensionKind,
    SnippetString,
    EventEmitter,
    WorkspaceEdit,
    version: "1.85.0",
    env: {
        appName: "VSCode Mock",
        appRoot: "/mock/app/root",
        language: "en",
        machineId: "mockMachineId",
        sessionId: "mockSessionId",
        shell: process.platform === "win32" ? "pwsh.exe" : "/bin/bash",
        uiKind: 1, // UIKind.Desktop
        uriScheme: "vscode",
        clipboard: {
            readText: jestMock.fn(() => Promise.resolve("")),
            writeText: jestMock.fn(() => Promise.resolve()),
        },
        openExternal: jestMock.fn(() => Promise.resolve(false)),
        asExternalUri: jestMock.fn(async (target: Uri) => target),
        appHost: "desktop",
        remoteName: undefined,
        isRemote: false,
        isNewAppInstall: false,
        isTelemetryEnabled: false,
        onDidChangeTelemetryEnabled: new EventEmitter<boolean>().event,
    },
    l10n: {
        t: jestMock.fn(
            (message: string, ...args: (string | number | boolean | Record<string, any>)[]) => {
                let result = message;
                if (args.length > 0) {
                    if (
                        typeof args[0] === "object" &&
                        args[0] !== null &&
                        !Array.isArray(args[0])
                    ) {
                        const replacements = args[0] as Record<string, any>;
                        for (const key in replacements) {
                            // Ensure RegExp is created correctly for `{${key}}` replacement
                            result = result.replace(
                                new RegExp(`\{\s*${key}\s*\}`, "g"),
                                String(replacements[key])
                            );
                        }
                    } else {
                        args.forEach((arg, index) => {
                            // Ensure RegExp is created correctly for `{${index}}` replacement
                            result = result.replace(
                                new RegExp(`\{\s*${index}\s*\}`, "g"),
                                String(arg)
                            );
                        });
                    }
                }
                return result;
            }
        ),
        bundle: undefined,
        uri: undefined,
    },
};

export default vscode;
