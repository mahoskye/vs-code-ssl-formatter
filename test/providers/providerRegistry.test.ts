/**
 * Tests for SSL Provider Registry
 *
 * Tests the centralized registration and management of SSL language service providers.
 */

import { SSLProviderRegistry } from "../../src/providers/providerRegistry";
import * as vscode from "vscode";

// Mock VS Code module
jest.mock("vscode", () => ({
    languages: {
        registerFoldingRangeProvider: jest.fn(() => ({ dispose: jest.fn() })),
        registerDocumentFormattingEditProvider: jest.fn(() => ({ dispose: jest.fn() })),
        registerDocumentRangeFormattingEditProvider: jest.fn(() => ({ dispose: jest.fn() })),
        registerCompletionItemProvider: jest.fn(() => ({ dispose: jest.fn() })),
        registerHoverProvider: jest.fn(() => ({ dispose: jest.fn() })),
        registerDocumentSymbolProvider: jest.fn(() => ({ dispose: jest.fn() })),
        createDiagnosticCollection: jest.fn(() => ({
            dispose: jest.fn(),
            set: jest.fn(),
            clear: jest.fn(),
            delete: jest.fn(),
        })),
    },
    commands: {
        registerCommand: jest.fn(() => ({ dispose: jest.fn() })),
    },
    workspace: {
        onDidChangeTextDocument: jest.fn(() => ({ dispose: jest.fn() })),
        onDidOpenTextDocument: jest.fn(() => ({ dispose: jest.fn() })),
        onDidCloseTextDocument: jest.fn(() => ({ dispose: jest.fn() })),
        textDocuments: [],
    },
    window: {
        activeTextEditor: null,
        showInformationMessage: jest.fn(),
    },
    Range: jest.fn(),
    Selection: jest.fn(),
}));

describe("SSL Provider Registry", () => {
    let registry: SSLProviderRegistry;

    beforeEach(() => {
        // Reset the singleton instance for each test
        (SSLProviderRegistry as any).instance = undefined;
        registry = SSLProviderRegistry.getInstance();
        jest.clearAllMocks();
    });

    afterEach(() => {
        // Clean up after each test
        registry.unregisterProviders();
    });

    describe("Singleton Pattern", () => {
        test("should return the same instance", () => {
            const instance1 = SSLProviderRegistry.getInstance();
            const instance2 = SSLProviderRegistry.getInstance();
            expect(instance1).toBe(instance2);
        });

        test("should be the same as the registry instance", () => {
            const instance = SSLProviderRegistry.getInstance();
            expect(instance).toBe(registry);
        });
    });

    describe("Provider Registration - Default Configuration", () => {
        test("should register all providers with default configuration", () => {
            const config = SSLProviderRegistry.getDefaultConfig();
            const result = registry.registerProviders(config);
            expect(result).toBeDefined();
            expect(result.disposables).toHaveLength(13); // 6 provider registrations + 1 diagnostic collection + 3 commands + 3 event listeners
            expect(result.providers).toBeDefined();
            expect(result.providers.foldingRangeProvider).toBeDefined();
            expect(result.providers.documentFormattingProvider).toBeDefined();
            expect(result.providers.completionProvider).toBeDefined();
            expect(result.providers.hoverProvider).toBeDefined();
            expect(result.providers.documentSymbolProvider).toBeDefined();
            expect(result.providers.diagnosticsProvider).toBeDefined();
            expect(result.providers.bracketMatchingProvider).toBeDefined();
        });

        test("should register folding range provider", () => {
            const config = SSLProviderRegistry.getDefaultConfig();
            registry.registerProviders(config);

            expect(vscode.languages.registerFoldingRangeProvider).toHaveBeenCalledTimes(1);
            expect(vscode.languages.registerFoldingRangeProvider).toHaveBeenCalledWith(
                config.documentSelector,
                expect.any(Object)
            );
        });

        test("should register document formatting providers", () => {
            const config = SSLProviderRegistry.getDefaultConfig();
            registry.registerProviders(config);

            expect(vscode.languages.registerDocumentFormattingEditProvider).toHaveBeenCalledTimes(
                1
            );
            expect(
                vscode.languages.registerDocumentRangeFormattingEditProvider
            ).toHaveBeenCalledTimes(1);
        });

        test("should register completion provider with correct triggers", () => {
            const config = SSLProviderRegistry.getDefaultConfig();
            registry.registerProviders(config);

            expect(vscode.languages.registerCompletionItemProvider).toHaveBeenCalledTimes(1);
            expect(vscode.languages.registerCompletionItemProvider).toHaveBeenCalledWith(
                config.documentSelector,
                expect.any(Object),
                ":", // SSL keywords
                "(", // Function calls
                "." // Logical operators
            );
        });

        test("should register SSL commands", () => {
            const config = SSLProviderRegistry.getDefaultConfig();
            registry.registerProviders(config);

            expect(vscode.commands.registerCommand).toHaveBeenCalledTimes(3);
            expect(vscode.commands.registerCommand).toHaveBeenCalledWith(
                "ssl-formatter.showBracketInfo",
                expect.any(Function)
            );
            expect(vscode.commands.registerCommand).toHaveBeenCalledWith(
                "ssl-formatter.showBracketInfoAtCursor",
                expect.any(Function)
            );
            expect(vscode.commands.registerCommand).toHaveBeenCalledWith(
                "ssl-formatter.findMatchingBracket",
                expect.any(Function)
            );
        });

        test("should set up document event listeners", () => {
            const config = SSLProviderRegistry.getDefaultConfig();
            registry.registerProviders(config);

            expect(vscode.workspace.onDidChangeTextDocument).toHaveBeenCalledTimes(1);
            expect(vscode.workspace.onDidOpenTextDocument).toHaveBeenCalledTimes(1);
            expect(vscode.workspace.onDidCloseTextDocument).toHaveBeenCalledTimes(1);
        });
    });

    describe("Provider Registration - Selective Configuration", () => {
        test("should register only specified providers", () => {
            const config = SSLProviderRegistry.createSelectiveConfig({
                foldingRange: true,
                completion: true,
                diagnostics: true,
            });
            const result = registry.registerProviders(config);

            expect(result).toBeDefined();
            expect(result.disposables.length).toBeGreaterThan(0);

            // Should register folding range provider
            expect(vscode.languages.registerFoldingRangeProvider).toHaveBeenCalledTimes(1);

            // Should register completion provider
            expect(vscode.languages.registerCompletionItemProvider).toHaveBeenCalledTimes(1);

            // Should NOT register document formatting providers
            expect(vscode.languages.registerDocumentFormattingEditProvider).not.toHaveBeenCalled();
            expect(
                vscode.languages.registerDocumentRangeFormattingEditProvider
            ).not.toHaveBeenCalled();

            // Should NOT register hover provider
            expect(vscode.languages.registerHoverProvider).not.toHaveBeenCalled();

            // Should NOT register document symbol provider
            expect(vscode.languages.registerDocumentSymbolProvider).not.toHaveBeenCalled();
        });

        test("should create selective config correctly", () => {
            const config = SSLProviderRegistry.createSelectiveConfig({
                foldingRange: true,
                hover: true,
            });

            expect(config.registerAll).toBe(false);
            expect(config.specificProviders).toBeDefined();
            expect(config.specificProviders!.foldingRange).toBe(true);
            expect(config.specificProviders!.hover).toBe(true);
            expect(config.specificProviders!.completion).toBe(false);
            expect(config.specificProviders!.documentFormatting).toBe(false);
        });
    });

    describe("Provider Management", () => {
        test("should throw error when registering providers twice", () => {
            const config = SSLProviderRegistry.getDefaultConfig();
            registry.registerProviders(config);

            expect(() => {
                registry.registerProviders(config);
            }).toThrow("Providers are already registered. Call unregisterProviders() first.");
        });

        test("should return registered providers", () => {
            const config = SSLProviderRegistry.getDefaultConfig();
            const result = registry.registerProviders(config);
            const registeredProviders = registry.getRegisteredProviders();

            expect(registeredProviders).toBe(result);
        });

        test("should return null when no providers are registered", () => {
            const registeredProviders = registry.getRegisteredProviders();
            expect(registeredProviders).toBeNull();
        });

        test("should unregister providers successfully", () => {
            const config = SSLProviderRegistry.getDefaultConfig();
            const result = registry.registerProviders(config);

            // Mock dispose method
            const disposeMock = jest.fn();
            result.disposables.forEach((disposable) => {
                disposable.dispose = disposeMock;
            });

            registry.unregisterProviders();

            expect(disposeMock).toHaveBeenCalledTimes(result.disposables.length);
            expect(registry.getRegisteredProviders()).toBeNull();
        });

        test("should handle unregistering when no providers are registered", () => {
            expect(() => {
                registry.unregisterProviders();
            }).not.toThrow();
        });
    });

    describe("Default Configuration", () => {
        test("should return correct default configuration", () => {
            const config = SSLProviderRegistry.getDefaultConfig();

            expect(config.documentSelector).toEqual({ scheme: "file", language: "ssl" });
            expect(config.registerAll).toBe(true);
            expect(config.specificProviders).toBeUndefined();
        });
    });

    describe("SSL Language Support", () => {
        test("should configure for SSL file type", () => {
            const config = SSLProviderRegistry.getDefaultConfig();

            expect(config.documentSelector).toEqual({
                scheme: "file",
                language: "ssl",
            });
        });

        test("should support SSL EBNF grammar constructs", () => {
            // This test verifies that the registration supports all SSL constructs
            // mentioned in the EBNF grammar specification
            const config = SSLProviderRegistry.getDefaultConfig();
            const result = registry.registerProviders(config);

            // Verify that all provider types required for SSL support are available
            expect(result.providers.foldingRangeProvider).toBeTruthy(); // For procedure blocks, conditionals, loops, etc.
            expect(result.providers.bracketMatchingProvider).toBeTruthy(); // For SSL keyword pairs
            expect(result.providers.documentFormattingProvider).toBeTruthy(); // For SSL code formatting
            expect(result.providers.completionProvider).toBeTruthy(); // For SSL keywords and functions
            expect(result.providers.hoverProvider).toBeTruthy(); // For SSL documentation
            expect(result.providers.documentSymbolProvider).toBeTruthy(); // For SSL outline
            expect(result.providers.diagnosticsProvider).toBeTruthy(); // For SSL validation
        });

        test("should handle SSL-specific completion triggers", () => {
            const config = SSLProviderRegistry.getDefaultConfig();
            registry.registerProviders(config);

            // Verify completion provider is registered with SSL-specific triggers
            expect(vscode.languages.registerCompletionItemProvider).toHaveBeenCalledWith(
                expect.any(Object),
                expect.any(Object),
                ":", // For SSL keywords like :PROCEDURE, :IF, :WHILE
                "(", // For function calls
                "." // For logical operators like .AND., .OR., .T., .F.
            );
        });
    });
});
