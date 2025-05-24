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
        // this.createSnippets(); // Removed: Snippets are now loaded from configuration
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
        this.snippetItems = []; // Initialize snippetItems

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
                    } else if (item.kind === "snippet") {
                        this.snippetItems.push(completionItem); // Ensure snippets are added to snippetItems
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
                            } else if (item.kind === "snippet") {
                                // Added: Handle snippets from private config
                                this.snippetItems.push(completionItem);
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
