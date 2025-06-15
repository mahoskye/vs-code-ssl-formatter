/**
 * SSL Provider Registry
 *
 * Centralized registry for all SSL language service providers.
 * This module provides a clean interface for registering and managing
 * VS Code language service providers for SSL, adhering to the SSL EBNF grammar.
 */

import * as vscode from "vscode";
import { SSLFoldingRangeProvider } from "./foldingRangeProvider";
import { SSLBracketMatchingProvider } from "./bracketMatchingProvider";
import { SSLDocumentFormattingProvider } from "./documentFormattingProvider";
import { SSLCompletionProvider } from "./completionProvider";
import { SSLHoverProvider } from "./hoverProvider";
import { SSLDocumentSymbolProvider } from "./documentSymbolProvider";
import { SSLDiagnosticsProvider } from "./diagnosticsProvider";

/**
 * Interface for provider registration result
 */
export interface ProviderRegistrationResult {
    disposables: vscode.Disposable[];
    providers: {
        foldingRangeProvider: SSLFoldingRangeProvider;
        bracketMatchingProvider: SSLBracketMatchingProvider;
        documentFormattingProvider: SSLDocumentFormattingProvider;
        completionProvider: SSLCompletionProvider;
        hoverProvider: SSLHoverProvider;
        documentSymbolProvider: SSLDocumentSymbolProvider;
        diagnosticsProvider: SSLDiagnosticsProvider;
    };
}

/**
 * Provider registration configuration
 */
export interface ProviderRegistrationConfig {
    /** Document selector for SSL files */
    documentSelector: vscode.DocumentSelector;
    /** Whether to register all providers (default: true) */
    registerAll?: boolean;
    /** Specific providers to register if not registering all */
    specificProviders?: {
        foldingRange?: boolean;
        documentFormatting?: boolean;
        completion?: boolean;
        hover?: boolean;
        documentSymbol?: boolean;
        diagnostics?: boolean;
        bracketMatching?: boolean;
    };
}

/**
 * SSL Provider Registry
 *
 * Manages registration and lifecycle of all SSL language service providers
 * according to the SSL EBNF grammar specification.
 */
export class SSLProviderRegistry {
    private static instance: SSLProviderRegistry;
    private registeredProviders: ProviderRegistrationResult | null = null;

    private constructor() {}

    /**
     * Get the singleton instance of the provider registry
     */
    public static getInstance(): SSLProviderRegistry {
        if (!SSLProviderRegistry.instance) {
            SSLProviderRegistry.instance = new SSLProviderRegistry();
        }
        return SSLProviderRegistry.instance;
    }

    /**
     * Register all SSL language service providers
     *
     * This method registers all providers needed for SSL language support:
     * - Folding Range Provider (for code folding of SSL constructs)
     * - Document Formatting Provider (for SSL code formatting)
     * - Completion Provider (for SSL keywords, functions, and constructs)
     * - Hover Provider (for SSL documentation and help)
     * - Document Symbol Provider (for SSL outline and navigation)
     * - Diagnostics Provider (for SSL syntax and semantic errors)
     * - Bracket Matching Provider (for SSL keyword pairs and brackets)
     *
     * @param config Provider registration configuration
     * @returns Registration result with disposables and provider instances
     */
    public registerProviders(config: ProviderRegistrationConfig): ProviderRegistrationResult {
        if (this.registeredProviders) {
            throw new Error("Providers are already registered. Call unregisterProviders() first.");
        }

        const disposables: vscode.Disposable[] = [];
        const providers = this.createProviders();

        // Register providers based on configuration
        if (config.registerAll !== false) {
            this.registerAllProviders(config.documentSelector, providers, disposables);
        } else if (config.specificProviders) {
            this.registerSpecificProviders(
                config.documentSelector,
                providers,
                disposables,
                config.specificProviders
            );
        }

        // Register SSL-specific commands
        this.registerSSLCommands(
            providers.bracketMatchingProvider,
            providers.diagnosticsProvider,
            disposables
        );

        // Set up document event listeners
        this.setupDocumentEventListeners(providers.diagnosticsProvider, disposables);

        this.registeredProviders = {
            disposables,
            providers,
        };

        return this.registeredProviders;
    }

    /**
     * Unregister all providers and clean up resources
     */
    public unregisterProviders(): void {
        if (this.registeredProviders) {
            this.registeredProviders.disposables.forEach((disposable) => disposable.dispose());
            this.registeredProviders = null;
        }
    }

    /**
     * Get currently registered providers
     */
    public getRegisteredProviders(): ProviderRegistrationResult | null {
        return this.registeredProviders;
    }

    /**
     * Create all provider instances
     */
    private createProviders() {
        return {
            foldingRangeProvider: new SSLFoldingRangeProvider(),
            bracketMatchingProvider: new SSLBracketMatchingProvider(),
            documentFormattingProvider: new SSLDocumentFormattingProvider(),
            completionProvider: new SSLCompletionProvider(),
            hoverProvider: new SSLHoverProvider(),
            documentSymbolProvider: new SSLDocumentSymbolProvider(),
            diagnosticsProvider: new SSLDiagnosticsProvider(),
        };
    }

    /**
     * Register all SSL language service providers
     */
    private registerAllProviders(
        selector: vscode.DocumentSelector,
        providers: any,
        disposables: vscode.Disposable[]
    ): void {
        // Register folding range provider for SSL code folding
        // Supports folding of procedures, conditionals, loops, classes, regions, etc.
        disposables.push(
            vscode.languages.registerFoldingRangeProvider(selector, providers.foldingRangeProvider)
        );

        // Register document formatting providers for SSL code formatting
        // Supports both full document and range formatting according to SSL EBNF grammar
        disposables.push(
            vscode.languages.registerDocumentFormattingEditProvider(
                selector,
                providers.documentFormattingProvider
            ),
            vscode.languages.registerDocumentRangeFormattingEditProvider(
                selector,
                providers.documentFormattingProvider
            )
        );

        // Register completion provider for SSL language constructs
        // Triggers on colon (:) for SSL keywords, parenthesis for functions, dot for logical operators
        disposables.push(
            vscode.languages.registerCompletionItemProvider(
                selector,
                providers.completionProvider,
                ":", // SSL keywords like :PROCEDURE, :IF, :WHILE, etc.
                "(", // Function calls
                "." // Logical operators like .AND., .OR., .T., .F.
            )
        );

        // Register hover provider for SSL documentation and help
        disposables.push(vscode.languages.registerHoverProvider(selector, providers.hoverProvider));

        // Register document symbol provider for SSL outline and navigation
        // Provides symbols for procedures, classes, variables, etc.
        disposables.push(
            vscode.languages.registerDocumentSymbolProvider(
                selector,
                providers.documentSymbolProvider
            )
        );

        // Register diagnostics provider collection
        disposables.push(providers.diagnosticsProvider.getDiagnosticCollection());
    }

    /**
     * Register specific providers based on configuration
     */
    private registerSpecificProviders(
        selector: vscode.DocumentSelector,
        providers: any,
        disposables: vscode.Disposable[],
        specificConfig: NonNullable<ProviderRegistrationConfig["specificProviders"]>
    ): void {
        if (specificConfig.foldingRange) {
            disposables.push(
                vscode.languages.registerFoldingRangeProvider(
                    selector,
                    providers.foldingRangeProvider
                )
            );
        }

        if (specificConfig.documentFormatting) {
            disposables.push(
                vscode.languages.registerDocumentFormattingEditProvider(
                    selector,
                    providers.documentFormattingProvider
                ),
                vscode.languages.registerDocumentRangeFormattingEditProvider(
                    selector,
                    providers.documentFormattingProvider
                )
            );
        }

        if (specificConfig.completion) {
            disposables.push(
                vscode.languages.registerCompletionItemProvider(
                    selector,
                    providers.completionProvider,
                    ":",
                    "(",
                    "."
                )
            );
        }

        if (specificConfig.hover) {
            disposables.push(
                vscode.languages.registerHoverProvider(selector, providers.hoverProvider)
            );
        }

        if (specificConfig.documentSymbol) {
            disposables.push(
                vscode.languages.registerDocumentSymbolProvider(
                    selector,
                    providers.documentSymbolProvider
                )
            );
        }

        if (specificConfig.diagnostics) {
            disposables.push(providers.diagnosticsProvider.getDiagnosticCollection());
        }
    }

    /**
     * Register SSL-specific commands
     */
    private registerSSLCommands(
        bracketMatchingProvider: SSLBracketMatchingProvider,
        diagnosticsProvider: SSLDiagnosticsProvider,
        disposables: vscode.Disposable[]
    ): void {
        // Command to show SSL bracket pair information
        disposables.push(
            vscode.commands.registerCommand("ssl-formatter.showBracketInfo", () => {
                const editor = vscode.window.activeTextEditor;
                if (editor && editor.document.languageId === "ssl") {
                    const info = bracketMatchingProvider.getBracketPairInfo(editor.document);
                    vscode.window.showInformationMessage(`SSL Bracket Pairs:\n${info}`);
                }
            })
        );

        // Command to show bracket information at cursor position
        disposables.push(
            vscode.commands.registerCommand("ssl-formatter.showBracketInfoAtCursor", () => {
                const editor = vscode.window.activeTextEditor;
                if (editor && editor.document.languageId === "ssl") {
                    const position = editor.selection.active;
                    const info = bracketMatchingProvider.getBracketInfoAtPosition(
                        editor.document,
                        position
                    );
                    vscode.window.showInformationMessage(info);
                }
            })
        );

        // Command to find matching SSL bracket/keyword
        disposables.push(
            vscode.commands.registerCommand("ssl-formatter.findMatchingBracket", () => {
                const editor = vscode.window.activeTextEditor;
                if (editor && editor.document.languageId === "ssl") {
                    const position = editor.selection.active;
                    const matchingPos = bracketMatchingProvider.findMatchingBracket(
                        editor.document,
                        position
                    );

                    if (matchingPos) {
                        // Move cursor to matching bracket
                        const newSelection = new vscode.Selection(matchingPos, matchingPos);
                        editor.selection = newSelection;
                        editor.revealRange(new vscode.Range(matchingPos, matchingPos));
                    } else {
                        vscode.window.showInformationMessage(
                            "No matching bracket found at cursor position."
                        );
                    }
                }
            })
        );
    }

    /**
     * Set up document event listeners for diagnostics and validation
     */
    private setupDocumentEventListeners(
        diagnosticsProvider: SSLDiagnosticsProvider,
        disposables: vscode.Disposable[]
    ): void {
        // Listen for document changes to update diagnostics
        disposables.push(
            vscode.workspace.onDidChangeTextDocument((e) => {
                if (e.document.languageId === "ssl") {
                    // Debounce diagnostics updates to avoid excessive processing
                    setTimeout(() => {
                        diagnosticsProvider.analyzeDiagnostics(e.document);
                    }, 250);
                }
            })
        );

        // Listen for document open events
        disposables.push(
            vscode.workspace.onDidOpenTextDocument((document) => {
                if (document.languageId === "ssl") {
                    diagnosticsProvider.analyzeDiagnostics(document);
                }
            })
        );

        // Listen for document close events to clean up diagnostics
        disposables.push(
            vscode.workspace.onDidCloseTextDocument((document) => {
                if (document.languageId === "ssl") {
                    diagnosticsProvider.clearDiagnostics(document);
                }
            })
        );

        // Analyze all currently open SSL documents
        vscode.workspace.textDocuments.forEach((document) => {
            if (document.languageId === "ssl") {
                diagnosticsProvider.analyzeDiagnostics(document);
            }
        });
    }

    /**
     * Get default registration configuration for SSL files
     */
    public static getDefaultConfig(): ProviderRegistrationConfig {
        return {
            documentSelector: { scheme: "file", language: "ssl" },
            registerAll: true,
        };
    }

    /**
     * Create a selective registration configuration
     */
    public static createSelectiveConfig(
        providers: Partial<NonNullable<ProviderRegistrationConfig["specificProviders"]>>
    ): ProviderRegistrationConfig {
        return {
            documentSelector: { scheme: "file", language: "ssl" },
            registerAll: false,
            specificProviders: {
                foldingRange: false,
                documentFormatting: false,
                completion: false,
                hover: false,
                documentSymbol: false,
                diagnostics: false,
                bracketMatching: false,
                ...providers,
            },
        };
    }
}
