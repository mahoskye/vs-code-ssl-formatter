import * as vscode from "vscode";
import * as fs from "fs";
import * as path from "path";

/**
 * Interface for hover data items
 */
interface HoverDataItem {
    label: string;
    kind: string;
    detail?: string;
    documentation?: string;
    syntax?: string;
    examples?: string[];
    category?: string;
}

/**
 * Enhanced SSL Hover Provider with comprehensive language support
 */
export class SSLHoverProvider implements vscode.HoverProvider {
    private hoverCache: Map<string, vscode.Hover> = new Map();
    private hoverItems: { [key: string]: HoverDataItem } = {};
    private privateConfigItems: { [key: string]: HoverDataItem } = {};

    constructor() {
        this.loadHoverItems();
        this.loadPrivateConfig();
        // Reload hover items when configuration changes
        vscode.workspace.onDidChangeConfiguration(this.onConfigurationChanged.bind(this));
    }

    /**
     * Loads hover items from the extension's configuration
     */
    public loadHoverItems() {
        const config = vscode.workspace.getConfiguration("sslFormatter");
        const userCompletions = config.get<any>("completions", {});
        this.hoverItems = {};

        // Convert completion items to hover items
        for (const category in userCompletions) {
            const items = userCompletions[category];
            if (Array.isArray(items)) {
                items.forEach((item: any) => {
                    const hoverItem: HoverDataItem = {
                        label: item.label || item.insertText || "",
                        kind: item.kind || "function",
                        detail: item.detail || "",
                        documentation: item.documentation || "",
                        syntax: item.syntax || "",
                        examples: item.examples || [],
                        category: category,
                    };

                    // Store with multiple key variations for better matching
                    const baseLabel = hoverItem.label.replace(/\(\)$/, "");
                    this.hoverItems[baseLabel] = hoverItem;
                    this.hoverItems[baseLabel.toUpperCase()] = hoverItem;
                    this.hoverItems[baseLabel.toLowerCase()] = hoverItem;
                });
            }
        }
    }

    /**
     * Loads hover items from the private configuration file
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
            if (!fs.existsSync(configPath)) {
                return;
            }

            const configContent = fs.readFileSync(configPath, "utf8");
            const config = JSON.parse(configContent);

            this.privateConfigItems = {};

            for (const category in config) {
                const items = config[category];
                if (Array.isArray(items)) {
                    items.forEach((item: any) => {
                        const hoverItem: HoverDataItem = {
                            label: item.label || item.insertText || "",
                            kind: item.kind || "function",
                            detail: item.detail || "",
                            documentation: item.documentation || "",
                            syntax: item.syntax || "",
                            examples: item.examples || [],
                            category: category,
                        };

                        // Store with multiple key variations for better matching
                        const baseLabel = hoverItem.label.replace(/\(\)$/, "");
                        this.privateConfigItems[baseLabel] = hoverItem;
                        this.privateConfigItems[baseLabel.toUpperCase()] = hoverItem;
                        this.privateConfigItems[baseLabel.toLowerCase()] = hoverItem;
                    });
                }
            }
        } catch (error) {
            console.warn("Could not load private configuration for hover:", error);
        }
    }

    /**
     * Handles configuration changes
     */
    private onConfigurationChanged(event: vscode.ConfigurationChangeEvent) {
        if (event.affectsConfiguration("sslFormatter.completions")) {
            this.loadHoverItems();
            this.clearCache();
        }
    }

    public provideHover(
        document: vscode.TextDocument,
        position: vscode.Position,
        token: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.Hover> {
        try {
            const { word, context } = this.getWordAndContext(document, position);

            if (!word) {
                return undefined;
            }

            // Check cache first
            const cacheKey = `${word}:${context}`;
            if (this.hoverCache.has(cacheKey)) {
                return this.hoverCache.get(cacheKey);
            } // Try different hover sources in order of priority
            let hover =
                this.tryGetHoverFromBuiltIns(word, context) ||
                this.tryGetHoverFromContext(word, context, document, position);

            if (hover) {
                this.hoverCache.set(cacheKey, hover);
                return hover;
            }

            return undefined;
        } catch (error) {
            console.warn("SSL Hover Provider error:", error);
            return undefined;
        }
    }

    /**
     * Extracts the word at position and determines context
     */
    private getWordAndContext(
        document: vscode.TextDocument,
        position: vscode.Position
    ): { word: string; context: string } {
        const line = document.lineAt(position);
        const lineText = line.text;

        // Try to get a more intelligent word range for SSL syntax
        let wordRange = document.getWordRangeAtPosition(position);
        let word = "";
        let context = "";

        if (wordRange) {
            word = document.getText(wordRange);
        }

        // Handle special SSL constructs
        if (!word || word.length === 0) {
            // Try to capture operators and special symbols
            const char = lineText[position.character];
            const prevChar = position.character > 0 ? lineText[position.character - 1] : "";
            const nextChar =
                position.character < lineText.length - 1 ? lineText[position.character + 1] : "";

            // Handle compound operators
            if (char === "=" && prevChar === ":") {
                word = ":=";
            } else if (char === "=" && prevChar === "=") {
                word = "==";
            } else if (char === "=" && prevChar === "!") {
                word = "!=";
            } else if (
                char === "=" &&
                (prevChar === "+" || prevChar === "-" || prevChar === "*" || prevChar === "/")
            ) {
                word = prevChar + "=";
            }
        }

        // Handle SSL keywords that start with colon
        if (word && !word.startsWith(":")) {
            const colonPos =
                position.character > 0 && lineText[position.character - 1] === ":"
                    ? position.character - 1
                    : -1;
            if (colonPos >= 0) {
                // Extend range to include the colon
                const extendedRange = new vscode.Range(
                    position.line,
                    colonPos,
                    position.line,
                    wordRange?.end.character || position.character
                );
                word = document.getText(extendedRange);
            }
        }

        // Clean up word
        word = word.replace(/\(\)$/, ""); // Remove trailing parentheses

        // Determine context from surrounding text
        const beforeWord = lineText.substring(0, position.character);
        const afterWord = lineText.substring(position.character);

        if (/\s*(\/\*|;)\s*$/.test(beforeWord)) {
            context = "comment";
        } else if (/^s*:/.test(beforeWord.trim())) {
            context = "statement";
        } else if (/\w+:\s*$/.test(beforeWord)) {
            context = "property";
        } else if (/\(\s*$/.test(beforeWord)) {
            context = "function_call";
        } else if (/:=\s*$/.test(beforeWord)) {
            context = "assignment";
        } else {
            context = "general";
        }

        return { word, context };
    }
    /**
     * Try to get hover information from built-in SSL language constructs and configuration
     */
    private tryGetHoverFromBuiltIns(word: string, context: string): vscode.Hover | undefined {
        // First check our configuration-based hover items
        const hoverItem =
            this.hoverItems[word] ||
            this.hoverItems[word.toUpperCase()] ||
            this.hoverItems[word.toLowerCase()];
        if (hoverItem) {
            return this.createEnhancedHover(hoverItem);
        } // Then check private config items
        const privateItem =
            this.privateConfigItems[word] ||
            this.privateConfigItems[word.toUpperCase()] ||
            this.privateConfigItems[word.toLowerCase()];
        if (privateItem) {
            return this.createEnhancedHover(privateItem);
        }
        return undefined;
    }

    /**
     * Try to infer hover information from context
     */
    private tryGetHoverFromContext(
        word: string,
        context: string,
        document: vscode.TextDocument,
        position: vscode.Position
    ): vscode.Hover | undefined {
        // Provide basic context-aware information for unknown identifiers
        if (context === "function_call" && /^[a-zA-Z_][a-zA-Z0-9_]*$/.test(word)) {
            return new vscode.Hover(
                new vscode.MarkdownString(
                    `**${word}** *(User-defined function)*\n\nThis appears to be a function call. Add it to your completions configuration for detailed documentation.`
                )
            );
        }

        if (context === "property" && /^[a-zA-Z_][a-zA-Z0-9_]*$/.test(word)) {
            return new vscode.Hover(
                new vscode.MarkdownString(
                    `**${word}** *(Object property)*\n\nThis appears to be an object property access using SSL's colon syntax.`
                )
            );
        }

        // Handle Hungarian notation variables
        if (/^[snbaro][A-Z]/.test(word)) {
            const prefix = word[0];
            const typeMap: { [key: string]: string } = {
                s: "String variable",
                n: "Numeric variable",
                b: "Boolean variable",
                a: "Array variable",
                r: "Record/Object variable",
                o: "Object variable",
            };

            const type = typeMap[prefix] || "Variable";
            return new vscode.Hover(
                new vscode.MarkdownString(
                    `**${word}** *(${type})*\n\nSSL variable using Hungarian notation.`
                )
            );
        }

        return undefined;
    }

    /**
     * Creates an enhanced hover with rich formatting
     */
    private createEnhancedHover(item: HoverDataItem): vscode.Hover {
        const markdown = new vscode.MarkdownString();
        markdown.isTrusted = true;

        // Title with kind badge
        const kindBadge = this.getKindBadge(item.kind);
        markdown.appendMarkdown(`# ${item.label} ${kindBadge}\n\n`);

        // Detail
        if (item.detail) {
            markdown.appendMarkdown(`*${item.detail}*\n\n`);
        }

        // Category
        if (item.category) {
            markdown.appendMarkdown(`**Category:** ${item.category}\n\n`);
        }

        // Syntax
        if (item.syntax) {
            markdown.appendMarkdown(`**Syntax:**\n\`\`\`ssl\n${item.syntax}\n\`\`\`\n\n`);
        }

        // Documentation
        if (item.documentation) {
            markdown.appendMarkdown(`**Description:**\n${item.documentation}\n\n`);
        }

        // Examples
        if (item.examples && item.examples.length > 0) {
            markdown.appendMarkdown(`**Examples:**\n`);
            item.examples.forEach((example) => {
                markdown.appendMarkdown(`\`\`\`ssl\n${example}\n\`\`\`\n\n`);
            });
        }

        return new vscode.Hover(markdown);
    }

    /**
     * Gets a badge for the item kind
     */
    private getKindBadge(kind: string): string {
        const badges: { [key: string]: string } = {
            keyword: "![keyword](https://img.shields.io/badge/-Keyword-blue)",
            function: "![function](https://img.shields.io/badge/-Function-green)",
            operator: "![operator](https://img.shields.io/badge/-Operator-orange)",
            literal: "![literal](https://img.shields.io/badge/-Literal-purple)",
            statement: "![statement](https://img.shields.io/badge/-Statement-red)",
            variable: "![variable](https://img.shields.io/badge/-Variable-yellow)",
        };

        return badges[kind.toLowerCase()] || "";
    }

    /**
     * Clears the hover cache (useful for configuration reloads)
     */
    public clearCache(): void {
        this.hoverCache.clear();
    }
}
