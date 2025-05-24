import * as vscode from "vscode";
import * as fs from "fs";
import * as path from "path";

// Map of string keys to VSCode CompletionItemKind values
const completionKindMap: { [key: string]: vscode.CompletionItemKind } = {
    method: vscode.CompletionItemKind.Method,
    function: vscode.CompletionItemKind.Function,
    constructor: vscode.CompletionItemKind.Constructor,
    field: vscode.CompletionItemKind.Field,
    variable: vscode.CompletionItemKind.Variable,
    class: vscode.CompletionItemKind.Class,
    interface: vscode.CompletionItemKind.Interface,
    module: vscode.CompletionItemKind.Module,
    property: vscode.CompletionItemKind.Property,
    unit: vscode.CompletionItemKind.Unit,
    value: vscode.CompletionItemKind.Value,
    enum: vscode.CompletionItemKind.Enum,
    keyword: vscode.CompletionItemKind.Keyword,
    snippet: vscode.CompletionItemKind.Snippet,
    text: vscode.CompletionItemKind.Text,
    color: vscode.CompletionItemKind.Color,
    file: vscode.CompletionItemKind.File,
    reference: vscode.CompletionItemKind.Reference,
    customcolor: vscode.CompletionItemKind.Color,
};

/**
 * Provides completion items for SSL language
 */
export class SSLCompletionProvider implements vscode.CompletionItemProvider {
    private completionItems: vscode.CompletionItem[] = [];
    private keywordItems: vscode.CompletionItem[] = [];
    private functionItems: vscode.CompletionItem[] = [];
    private snippetItems: vscode.CompletionItem[] = [];

    constructor() {
        this.loadCompletionItems();
        this.loadPrivateConfig();
        this.createSnippets();
        // Reload completion items when configuration changes
        vscode.workspace.onDidChangeConfiguration(this.onConfigurationChanged.bind(this));
    }

    /**
     * Loads completion items from the extension's configuration
     */
    public loadCompletionItems() {
        const config = vscode.workspace.getConfiguration("sslFormatter");
        const userCompletions = config.get<any>("completions", {});
        this.completionItems = [];
        this.keywordItems = [];
        this.functionItems = [];

        for (const category in userCompletions) {
            const items = userCompletions[category];
            if (Array.isArray(items)) {
                items.forEach((item: any) => {
                    const completionItem = this.createCompletionItem(item);
                    this.completionItems.push(completionItem);

                    // Categorize items for context-sensitive completion
                    if (item.kind === "keyword") {
                        this.keywordItems.push(completionItem);
                    } else if (item.kind === "function") {
                        this.functionItems.push(completionItem);
                    }
                });
            }
        }
    }

    /**
     * Loads completion items from private configuration file
     */
    private loadPrivateConfig() {
        try {
            const extensionPath = vscode.extensions.getExtension(
                "mahoskye.vs-code-ssl-formatter"
            )?.extensionPath;
            if (!extensionPath) {
                return;
            }

            const configPath = path.join(extensionPath, "config", "config.private.json");
            if (fs.existsSync(configPath)) {
                const configContent = fs.readFileSync(configPath, "utf8");
                const config = JSON.parse(configContent);

                for (const category in config) {
                    const items = config[category];
                    if (Array.isArray(items)) {
                        items.forEach((item: any) => {
                            const completionItem = this.createCompletionItem(item);
                            this.completionItems.push(completionItem);

                            // Categorize items for context-sensitive completion
                            if (item.kind === "keyword") {
                                this.keywordItems.push(completionItem);
                            } else if (item.kind === "function") {
                                this.functionItems.push(completionItem);
                            }
                        });
                    }
                }
            }
        } catch (error) {
            console.warn("Could not load private configuration:", error);
        }
    }
    /**
     * Creates SSL-specific code snippets
     */
    private createSnippets() {
        this.snippetItems = [];

        // Common SSL patterns
        const snippets = [
            {
                label: "if-else",
                insertText:
                    ":IF ${1:condition};\n\t${2:// true branch}\n:ELSE;\n\t${3:// false branch}\n:ENDIF;",
                detail: "IF-ELSE statement",
                documentation: "Creates an IF-ELSE conditional statement",
            },
            {
                label: "while-loop",
                insertText: ":WHILE ${1:condition};\n\t${2:// loop body}\n:ENDWHILE;",
                detail: "WHILE loop",
                documentation: "Creates a WHILE loop",
            },
            {
                label: "for-loop",
                insertText: ":FOR ${1:i} := ${2:1} :TO ${3:10};\n\t${4:// loop body}\n:NEXT;",
                detail: "FOR loop",
                documentation: "Creates a FOR loop",
            },
            {
                label: "try-catch",
                insertText:
                    ":TRY;\n\t${1:// code that might throw}\n:CATCH;\n\t${2:// error handling}\n:ENDTRY;",
                detail: "TRY-CATCH block",
                documentation: "Creates a TRY-CATCH error handling block",
            },
            {
                label: "procedure",
                insertText:
                    ":PROCEDURE ${1:ProcName};\n:PARAMETERS ${2:param1, param2};\n\t${3:// procedure body}\n:RETURN ${4:result};\n:ENDPROC;",
                detail: "Procedure definition",
                documentation: "Creates a procedure definition",
            },
            {
                label: "case-switch",
                insertText:
                    ":BEGINCASE;\n:CASE ${1:condition1};\n\t${2:// case 1}\n:CASE ${3:condition2};\n\t${4:// case 2}\n:OTHERWISE;\n\t${5:// default case}\n:ENDCASE;",
                detail: "CASE statement",
                documentation: "Creates a CASE switch statement",
            },
            {
                label: "declare-vars",
                insertText: ":DECLARE ${1:variable1}, ${2:variable2};",
                detail: "Variable declaration",
                documentation: "Declares variables",
            },
            {
                label: "class-definition",
                insertText:
                    ":CLASS ${1:ClassName};\n:INHERIT ${2:ParentClass};\n\n:DECLARE ${3:field1}, ${4:field2};\n\n:PROCEDURE ${5:MethodName};\n:PARAMETERS ${6:param1};\n\t${7:// method body}\n:RETURN ${8:result};\n:ENDPROC;",
                detail: "Class definition",
                documentation: "Creates a class definition with inheritance and methods",
            },
            {
                label: "error-handling",
                insertText: ":ERROR;\n\t${1:// error handling code}\n:RETURN ${2:result};",
                detail: "Error handling block",
                documentation: "Creates an error handling block",
            },
            {
                label: "region",
                insertText: ":REGION ${1:RegionName};\n\t${2:// code here}\n:ENDREGION;",
                detail: "Code region",
                documentation: "Creates a collapsible code region",
            },
        ];

        snippets.forEach((snippet) => {
            const item = new vscode.CompletionItem(
                snippet.label,
                vscode.CompletionItemKind.Snippet
            );
            item.insertText = new vscode.SnippetString(snippet.insertText);
            item.detail = snippet.detail;
            item.documentation = new vscode.MarkdownString(snippet.documentation);
            this.snippetItems.push(item);
        });
    }

    /**
     * Creates a VSCode CompletionItem from a completion item configuration
     */
    private createCompletionItem(item: any): vscode.CompletionItem {
        const kind = this.getCompletionKind(item.kind);
        const completionItem = new vscode.CompletionItem(item.label, kind);

        if (item.detail) {
            completionItem.detail = item.detail;
        }
        if (item.documentation) {
            completionItem.documentation = new vscode.MarkdownString(item.documentation);
        }
        if (item.insertText) {
            completionItem.insertText = new vscode.SnippetString(item.insertText);
        }

        return completionItem;
    }
    /**
     * Maps a string kind to a VSCode CompletionItemKind
     */
    private getCompletionKind(kind: string): vscode.CompletionItemKind {
        return completionKindMap[kind] || vscode.CompletionItemKind.Text;
    }

    /**
     * Analyzes the current line and context to determine what completions to show
     */ private getContextualCompletions(
        document: vscode.TextDocument,
        position: vscode.Position,
        context: vscode.CompletionContext
    ): vscode.CompletionItem[] {
        const line = document.lineAt(position).text;
        const beforeCursor = line.substring(0, position.character);

        // Handle trigger characters
        if (context.triggerCharacter) {
            switch (context.triggerCharacter) {
                case ":":
                    // After colon, could be property access or statement start
                    if (/^\s*:$/.test(beforeCursor)) {
                        // Statement start - return keywords/statements
                        return this.keywordItems.filter((item) =>
                            item.label?.toString().startsWith(":")
                        );
                    } else {
                        // Property access (e.g., object:property) - return all completions
                        return this.completionItems;
                    }
                case "(":
                    // Function call - return functions
                    return this.functionItems;
                case ",":
                    // Parameter separator - return functions and variables
                    return this.functionItems;
                case " ":
                    // Context-dependent space handling
                    // Check for specific SSL patterns
                    if (/^\s*:FOR\s+\w+\s+:=\s+\w+\s+$/.test(beforeCursor)) {
                        // After :FOR variable := value, expect :TO
                        return this.keywordItems.filter((item) => item.label?.toString() === ":TO");
                    }
                    break;
            }
        }

        // SSL-specific contextual patterns

        // After assignment operator, suggest functions and literals
        if (/:=\s*$/.test(beforeCursor)) {
            return [
                ...this.functionItems,
                ...this.completionItems.filter(
                    (item) => item.kind === vscode.CompletionItemKind.Value
                ),
            ];
        }

        // After logical operators, suggest expressions
        if (/\.(AND|OR)\.\s*$/.test(beforeCursor.toUpperCase())) {
            return [
                ...this.functionItems,
                ...this.completionItems.filter(
                    (item) => item.kind === vscode.CompletionItemKind.Value
                ),
            ];
        }

        // In SQL parameter context (between ? marks)
        if (
            /\?\w*$/.test(beforeCursor) &&
            beforeCursor.lastIndexOf("?") > beforeCursor.lastIndexOf("?", beforeCursor.length - 2)
        ) {
            // Inside SQL parameter - return variable names
            return this.completionItems.filter(
                (item) => item.kind === vscode.CompletionItemKind.Variable
            );
        }

        // If we're at the start of a line or after whitespace, suggest keywords
        if (/^\s*$/.test(beforeCursor) || /^\s*:/.test(beforeCursor)) {
            return [...this.keywordItems, ...this.snippetItems];
        }

        // If we're in a function call context, suggest functions
        if (/[\w\s]*\(\s*$/.test(beforeCursor) || /,\s*$/.test(beforeCursor)) {
            return this.functionItems;
        }

        // If we're after a colon (property access), suggest appropriate completions
        if (/\w+:\s*$/.test(beforeCursor)) {
            // This could be object property access - return all items for now
            return this.completionItems;
        }

        // Default: return all items
        return this.completionItems;
    }
    /**
     * Handles configuration changes
     */
    private onConfigurationChanged(event: vscode.ConfigurationChangeEvent) {
        if (event.affectsConfiguration("sslFormatter.completions")) {
            this.loadCompletionItems();
            this.loadPrivateConfig();
        }
    }

    /**
     * Provides completion items for a given position in a document
     */
    provideCompletionItems(
        document: vscode.TextDocument,
        position: vscode.Position,
        token: vscode.CancellationToken,
        context: vscode.CompletionContext
    ): vscode.ProviderResult<vscode.CompletionItem[]> {
        // Get contextual completions based on cursor position
        const contextualItems = this.getContextualCompletions(document, position, context);

        // Always include snippets for quick access
        return [...contextualItems, ...this.snippetItems];
    }
}
