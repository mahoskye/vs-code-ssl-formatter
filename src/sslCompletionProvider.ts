import * as vscode from "vscode";

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

    constructor() {
        this.loadCompletionItems();
        // Reload completion items when configuration changes
        vscode.workspace.onDidChangeConfiguration(this.onConfigurationChanged.bind(this));
    }

    /**
     * Loads completion items from the extension's configuration
     */
    public loadCompletionItems() {
        const config = vscode.workspace.getConfiguration("sslFormatter");
        const userCompletions = config.get<any[]>("completions", []);
        this.completionItems = userCompletions.map((item) => this.createCompletionItem(item));
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
     * Handles configuration changes
     */
    private onConfigurationChanged(event: vscode.ConfigurationChangeEvent) {
        if (event.affectsConfiguration("sslFormatter.completions")) {
            this.loadCompletionItems();
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
        return this.completionItems;
    }
}
