import * as assert from "assert";
import * as vscode from "vscode";
import { SSLCompletionProvider } from "../../src/sslCompletionProvider";
import { EOL } from "os";

// Helper function to create a mock TextDocument (can be shared or adapted from bracketMatchingProvider.test.ts)
function createMockDocument(lines: string[]): vscode.TextDocument {
    return {
        uri: vscode.Uri.file("test.ssl"),
        fileName: "test.ssl",
        isUntitled: false,
        languageId: "ssl",
        version: 1,
        isDirty: false,
        isClosed: false,
        save: () => Promise.resolve(true),
        eol: EOL === "\r\n" ? vscode.EndOfLine.CRLF : vscode.EndOfLine.LF,
        lineCount: lines.length,
        lineAt: (lineOrPosition: number | vscode.Position): vscode.TextLine => {
            const lineNumber =
                typeof lineOrPosition === "number" ? lineOrPosition : lineOrPosition.line;
            const text = lines[lineNumber];
            const firstNonWhitespaceCharacterIndex = text.search(/\S|$/);
            return {
                lineNumber,
                text,
                range: new vscode.Range(lineNumber, 0, lineNumber, text.length),
                rangeIncludingLineBreak: new vscode.Range(lineNumber, 0, lineNumber + 1, 0),
                firstNonWhitespaceCharacterIndex,
                isEmptyOrWhitespace: firstNonWhitespaceCharacterIndex === text.length,
            };
        },
        offsetAt: (position: vscode.Position): number => {
            let offset = 0;
            for (let i = 0; i < position.line; i++) {
                offset += lines[i].length + EOL.length;
            }
            return offset + position.character;
        },
        positionAt: (offset: number): vscode.Position => {
            let line = 0;
            let character = 0;
            for (let i = 0; i < lines.length; i++) {
                const lineLength = lines[i].length + EOL.length;
                if (offset >= lineLength) {
                    offset -= lineLength;
                    line++;
                } else {
                    character = offset;
                    break;
                }
            }
            return new vscode.Position(line, character);
        },
        getText: (range?: vscode.Range): string => {
            if (!range) {
                return lines.join(EOL);
            }
            const startLine = range.start.line;
            const endLine = range.end.line;
            if (startLine === endLine) {
                return lines[startLine].substring(range.start.character, range.end.character);
            }
            let text = lines[startLine].substring(range.start.character) + EOL;
            for (let i = startLine + 1; i < endLine; i++) {
                text += lines[i] + EOL;
            }
            text += lines[endLine].substring(0, range.end.character);
            return text;
        },
        getWordRangeAtPosition: (
            position: vscode.Position,
            regex?: RegExp
        ): vscode.Range | undefined => {
            const lineText = lines[position.line];
            const wordRegex = regex || /\w+/g; // Default to words
            let match;
            while ((match = wordRegex.exec(lineText)) !== null) {
                const start = match.index;
                const end = start + match[0].length;
                if (position.character >= start && position.character <= end) {
                    return new vscode.Range(position.line, start, position.line, end);
                }
            }
            return undefined;
        },
        validateRange: (range: vscode.Range): vscode.Range => range, // Simplified
        validatePosition: (position: vscode.Position): vscode.Position => position, // Simplified
    };
}

// Helper to check if a completion item exists
function assertCompletionItem(
    items: vscode.CompletionItem[],
    label: string,
    kind?: vscode.CompletionItemKind
) {
    const found = items.some(
        (item) => item.label === label && (kind === undefined || item.kind === kind)
    );
    assert.ok(
        found,
        `Completion item with label '${label}'${
            kind !== undefined ? " and kind " + vscode.CompletionItemKind[kind] : ""
        } not found.`
    );
}

// Helper to check if a snippet (by insertText) exists
function assertSnippet(items: vscode.CompletionItem[], insertTextStart: string) {
    const found = items.some(
        (item) =>
            item.kind === vscode.CompletionItemKind.Snippet &&
            (item.insertText as vscode.SnippetString)?.value.startsWith(insertTextStart)
    );
    assert.ok(found, `Snippet starting with '${insertTextStart}' not found.`);
}

describe("SSLCompletionProvider", () => {
    let provider: SSLCompletionProvider;
    let mockToken: vscode.CancellationTokenSource;

    beforeEach(async () => {
        // Ensure configurations are loaded before each test
        const mockConfiguration = {
            get: (section: string) => {
                if (section === "completions") {
                    return {
                        keywords: [
                            {
                                label: ":IF",
                                kind: "keyword",
                                insertText: ":IF ${1:condition};\n\t$0\n:ENDIF;",
                            },
                            { label: ":ELSE", kind: "keyword" },
                            { label: ":ENDIF", kind: "keyword" },
                            {
                                label: ":PROCEDURE",
                                kind: "keyword",
                                insertText:
                                    ":PROCEDURE ${1:ProcName};\n\t:PARAMETERS ${2:params};\n\t$0\n:ENDPROC;",
                            },
                            { label: ":ENDPROC", kind: "keyword" },
                            { label: ":DECLARE", kind: "keyword" },
                            { label: ":PARAMETERS", kind: "keyword" },
                            { label: ":RETURN", kind: "keyword" },
                            {
                                label: ":FOR",
                                kind: "keyword",
                                insertText: ":FOR ${1:i} := ${2:start} :TO ${3:end};\n\t$0\n:NEXT;",
                            },
                            { label: ":TO", kind: "keyword" },
                            { label: ":NEXT", kind: "keyword" },
                        ],
                        functions: [
                            {
                                label: "Len",
                                kind: "function",
                                detail: "Len(string | array)",
                                documentation: "Returns the length of a string or array.",
                            },
                            {
                                label: "SqlExecute",
                                kind: "function",
                                detail: "SqlExecute(query, params)",
                                documentation: "Executes an SQL query.",
                            },
                            {
                                label: "CreateUDObject",
                                kind: "function",
                                detail: "CreateUDObject(className)",
                                documentation: "Creates a User Defined Object.",
                            },
                        ],
                        variables: [
                            {
                                label: "sGlobalVar",
                                kind: "variable",
                                detail: "Global String Variable",
                            },
                        ],
                        literals: [
                            { label: ".T.", kind: "value", detail: "Boolean True" },
                            { label: ".F.", kind: "value", detail: "Boolean False" },
                            { label: "NIL", kind: "value", detail: "Null value" },
                        ],
                        snippets: [
                            {
                                label: "if-else",
                                kind: "snippet",
                                insertText:
                                    ":IF ${1:condition};\n\t${2:true_branch}\n:ELSE;\n\t${3:false_branch}\n:ENDIF;",
                                detail: "IF/ELSE block snippet",
                            },
                            {
                                label: "proc",
                                kind: "snippet",
                                insertText:
                                    ":PROCEDURE ${1:ProcedureName};\n\t:PARAMETERS ${2:params};\n\t/* Your code here */\n\t:RETURN ${3:result};\n:ENDPROC;",
                                detail: "Procedure snippet",
                            },
                        ],
                    };
                }
                return undefined;
            },
            has: () => true,
            inspect: () => undefined, // Simplified
            update: () => Promise.resolve(), // Simplified
        };

        const originalGetConfiguration = vscode.workspace.getConfiguration;
        (vscode.workspace as any).getConfiguration = (
            section?: string,
            scope?: vscode.ConfigurationScope
        ) => {
            if (section === "sslFormatter") {
                return mockConfiguration;
            }
            return originalGetConfiguration(section, scope);
        };

        provider = new SSLCompletionProvider(); // Provider will now use mocked config
        provider.loadCompletionItems(); // Explicitly call load to ensure mock config is used

        mockToken = new vscode.CancellationTokenSource();

        // Restore original getConfiguration after provider is initialized with mock
        (vscode.workspace as any).getConfiguration = originalGetConfiguration;
    });

    afterEach(() => {
        mockToken.dispose();
    });

    it("should provide keyword completions at the start of a line", async () => {
        const doc = createMockDocument([""]);
        const position = new vscode.Position(0, 0);
        const mockContext: vscode.CompletionContext = {
            triggerKind: vscode.CompletionTriggerKind.Invoke,
            triggerCharacter: undefined,
        };
        const items = (await provider.provideCompletionItems(
            doc,
            position,
            mockToken.token,
            mockContext
        )) as vscode.CompletionItem[];

        assert.ok(items, "Should return completion items");
        assertCompletionItem(items, ":IF", vscode.CompletionItemKind.Keyword);
        assertCompletionItem(items, ":PROCEDURE", vscode.CompletionItemKind.Keyword);
        assertCompletionItem(items, "if-else", vscode.CompletionItemKind.Snippet); // Check for snippet
    });

    it("should provide keyword completions after a colon at the start of a line", async () => {
        const doc = createMockDocument([":"]);
        const position = new vscode.Position(0, 1);
        const mockContext: vscode.CompletionContext = {
            triggerKind: vscode.CompletionTriggerKind.TriggerCharacter,
            triggerCharacter: ":",
        };
        const items = (await provider.provideCompletionItems(
            doc,
            position,
            mockToken.token,
            mockContext
        )) as vscode.CompletionItem[];

        assert.ok(items, "Should return completion items");
        assertCompletionItem(items, ":IF", vscode.CompletionItemKind.Keyword);
        assertCompletionItem(items, ":PROCEDURE", vscode.CompletionItemKind.Keyword);
    });

    it("should provide function completions after an assignment operator", async () => {
        const doc = createMockDocument(["myVar :="]);
        const position = new vscode.Position(0, 8);
        const mockContext: vscode.CompletionContext = {
            triggerKind: vscode.CompletionTriggerKind.Invoke,
            triggerCharacter: undefined,
        };
        const items = (await provider.provideCompletionItems(
            doc,
            position,
            mockToken.token,
            mockContext
        )) as vscode.CompletionItem[];

        assert.ok(items, "Should return completion items");
        assertCompletionItem(items, "Len", vscode.CompletionItemKind.Function);
        assertCompletionItem(items, "SqlExecute", vscode.CompletionItemKind.Function);
        assertCompletionItem(items, ".T.", vscode.CompletionItemKind.Value); // Literals
    });

    it("should provide function completions in a function call context", async () => {
        const doc = createMockDocument(["Result := MyFunction("]);
        const position = new vscode.Position(0, 20);
        const mockContext: vscode.CompletionContext = {
            triggerKind: vscode.CompletionTriggerKind.TriggerCharacter,
            triggerCharacter: "(",
        };
        const items = (await provider.provideCompletionItems(
            doc,
            position,
            mockToken.token,
            mockContext
        )) as vscode.CompletionItem[];

        assert.ok(items, "Should return completion items");
        assertCompletionItem(items, "Len", vscode.CompletionItemKind.Function);
    });

    it('should provide :TO keyword after ":FOR i := 1 "', async () => {
        const doc = createMockDocument([":FOR i := 1 "]);
        const position = new vscode.Position(0, 12);
        const mockContext: vscode.CompletionContext = {
            triggerKind: vscode.CompletionTriggerKind.TriggerCharacter,
            triggerCharacter: " ",
        };
        const items = (await provider.provideCompletionItems(
            doc,
            position,
            mockToken.token,
            mockContext
        )) as vscode.CompletionItem[];

        assert.ok(items, "Should return completion items");
        assert.strictEqual(items.length, 1, "Should only suggest :TO");
        assertCompletionItem(items, ":TO", vscode.CompletionItemKind.Keyword);
    });

    it("should provide snippets when typing their prefix", async () => {
        const doc = createMockDocument(["pro"]); // User starts typing 'proc'
        const position = new vscode.Position(0, 3);
        const mockContext: vscode.CompletionContext = {
            triggerKind: vscode.CompletionTriggerKind.Invoke,
            triggerCharacter: undefined,
        };
        const items = (await provider.provideCompletionItems(
            doc,
            position,
            mockToken.token,
            mockContext
        )) as vscode.CompletionItem[];

        assert.ok(items, "Should return completion items");
        const procSnippet = items.find(
            (item) => item.label === "proc" && item.kind === vscode.CompletionItemKind.Snippet
        );
        assert.ok(procSnippet, "Snippet with label 'proc' should be offered");
        assert.strictEqual(
            (procSnippet!.insertText as vscode.SnippetString).value.startsWith(
                ":PROCEDURE ${1:ProcedureName};"
            ),
            true
        );
    });

    it("should not provide SSL completions inside a string literal", async () => {
        const doc = createMockDocument(['sTest := ":IF";']);
        const position = new vscode.Position(0, 12); // Cursor inside ":IF"
        const mockContext: vscode.CompletionContext = {
            triggerKind: vscode.CompletionTriggerKind.Invoke,
            triggerCharacter: undefined,
        };
        const items = (await provider.provideCompletionItems(
            doc,
            position,
            mockToken.token,
            mockContext
        )) as vscode.CompletionItem[];

        const foundSSLKeyword = items.some(
            (item) => item.label === ":IF" && item.kind === vscode.CompletionItemKind.Keyword
        );
        assert.strictEqual(
            foundSSLKeyword,
            false,
            "Should not suggest SSL keywords inside string literals"
        );
    });

    it("should not provide SSL completions inside a comment", async () => {
        const doc = createMockDocument(["/* :IF this is a comment */;"]);
        const position = new vscode.Position(0, 6); // Cursor inside ":IF" in comment
        const mockContext: vscode.CompletionContext = {
            triggerKind: vscode.CompletionTriggerKind.Invoke,
            triggerCharacter: undefined,
        };
        const items = (await provider.provideCompletionItems(
            doc,
            position,
            mockToken.token,
            mockContext
        )) as vscode.CompletionItem[];

        const foundSSLKeyword = items.some(
            (item) => item.label === ":IF" && item.kind === vscode.CompletionItemKind.Keyword
        );
        assert.strictEqual(
            foundSSLKeyword,
            false,
            "Should not suggest SSL keywords inside comments"
        );
    });

    it("should load completions from configuration by checking output", async () => {
        // This test verifies that the mocked configuration was loaded
        // by checking for items defined in the mock in the output.
        const doc = createMockDocument([""]); // Empty document, invoke at start
        const position = new vscode.Position(0, 0);
        const mockContext: vscode.CompletionContext = {
            triggerKind: vscode.CompletionTriggerKind.Invoke,
            triggerCharacter: undefined,
        };
        const items = (await provider.provideCompletionItems(
            doc,
            position,
            mockToken.token,
            mockContext
        )) as vscode.CompletionItem[];

        assert.ok(items, "Should return completion items");
        assertCompletionItem(items, ":IF", vscode.CompletionItemKind.Keyword);
        // Functions are not typically suggested at the very start of a line unless triggered differently
        // assertCompletionItem(items, 'Len', vscode.CompletionItemKind.Function);
        assertCompletionItem(items, "proc", vscode.CompletionItemKind.Snippet);
    });

    it("should suggest :PROCEDURE when typing : at global scope (from thorough test file)", async () => {
        const doc = createMockDocument([":", ":DECLARE globalVar1;"]); // Simplified from test file
        const position = new vscode.Position(0, 1);
        const mockContext: vscode.CompletionContext = {
            triggerKind: vscode.CompletionTriggerKind.TriggerCharacter,
            triggerCharacter: ":",
        };
        const items = (await provider.provideCompletionItems(
            doc,
            position,
            mockToken.token,
            mockContext
        )) as vscode.CompletionItem[];

        assert.ok(items, "Should return completion items");
        assertCompletionItem(items, ":PROCEDURE", vscode.CompletionItemKind.Keyword);
        assertCompletionItem(items, ":DECLARE", vscode.CompletionItemKind.Keyword);
    });

    it("should suggest functions and literals after assignment operator `:=`", async () => {
        const doc = createMockDocument(["globalVar1 := "]);
        const position = new vscode.Position(0, 14);
        const mockContext: vscode.CompletionContext = {
            triggerKind: vscode.CompletionTriggerKind.Invoke,
            triggerCharacter: undefined,
        };
        const items = (await provider.provideCompletionItems(
            doc,
            position,
            mockToken.token,
            mockContext
        )) as vscode.CompletionItem[];

        assert.ok(items, "Should return completion items");
        assertCompletionItem(items, "Len", vscode.CompletionItemKind.Function);
        assertCompletionItem(items, ".T.", vscode.CompletionItemKind.Value);
        assertCompletionItem(items, "NIL", vscode.CompletionItemKind.Value);
    });

    // ... Add more tests based on ssl_examples/completion_provider_thorough_test.ssl ...
});
