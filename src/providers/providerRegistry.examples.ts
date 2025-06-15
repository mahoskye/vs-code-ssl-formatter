/**
 * SSL Provider Registry Usage Examples
 *
 * This file demonstrates various ways to use the SSL Provider Registry
 * for different extension scenarios and configurations.
 */

import * as vscode from "vscode";
import { SSLProviderRegistry, ProviderRegistrationConfig } from "../providers/providerRegistry";

/**
 * Example 1: Basic Extension Activation (Default Configuration)
 *
 * This is the most common usage - register all SSL providers with default settings.
 * Suitable for most SSL extension use cases.
 */
export function activateBasicSSLSupport(context: vscode.ExtensionContext): void {
    // Get the singleton registry instance
    const registry = SSLProviderRegistry.getInstance();

    // Register all providers with default configuration
    const registration = registry.registerProviders(SSLProviderRegistry.getDefaultConfig());

    // Add all disposables to extension context for cleanup
    context.subscriptions.push(...registration.disposables);

    console.log("SSL extension activated with full language support");
}

/**
 * Example 2: Minimal SSL Support (Only Essential Providers)
 *
 * Register only the most essential providers for basic SSL support.
 * Useful for lightweight extensions or when you want to minimize resource usage.
 */
export function activateMinimalSSLSupport(context: vscode.ExtensionContext): void {
    const registry = SSLProviderRegistry.getInstance();

    // Create selective configuration for minimal support
    const config = SSLProviderRegistry.createSelectiveConfig({
        foldingRange: true, // Code folding for SSL constructs
        completion: true, // Auto-completion for SSL keywords
        diagnostics: true, // Error detection and validation
    });

    const registration = registry.registerProviders(config);
    context.subscriptions.push(...registration.disposables);

    console.log("SSL extension activated with minimal language support");
}

/**
 * Example 3: Formatting-Focused SSL Support
 *
 * Register providers primarily focused on code formatting and structure.
 * Ideal for SSL formatter extensions.
 */
export function activateFormattingSSLSupport(context: vscode.ExtensionContext): void {
    const registry = SSLProviderRegistry.getInstance();

    const config = SSLProviderRegistry.createSelectiveConfig({
        foldingRange: true, // Code folding for structure
        documentFormatting: true, // Document formatting
        bracketMatching: true, // SSL keyword pair matching
        diagnostics: true, // Validation for formatting
    });

    const registration = registry.registerProviders(config);
    context.subscriptions.push(...registration.disposables);

    console.log("SSL extension activated with formatting-focused support");
}

/**
 * Example 4: Development and Analysis SSL Support
 *
 * Register providers focused on development assistance and code analysis.
 * Suitable for SSL development tools and IDEs.
 */
export function activateDevelopmentSSLSupport(context: vscode.ExtensionContext): void {
    const registry = SSLProviderRegistry.getInstance();

    const config = SSLProviderRegistry.createSelectiveConfig({
        completion: true, // Auto-completion
        hover: true, // Documentation on hover
        documentSymbol: true, // Code outline and navigation
        diagnostics: true, // Error detection
        bracketMatching: true, // Bracket/keyword matching
    });

    const registration = registry.registerProviders(config);
    context.subscriptions.push(...registration.disposables);

    console.log("SSL extension activated with development-focused support");
}

/**
 * Example 5: Custom Configuration
 *
 * Create a completely custom configuration for specific needs.
 */
export function activateCustomSSLSupport(context: vscode.ExtensionContext): void {
    const registry = SSLProviderRegistry.getInstance();

    // Custom configuration with specific document selector
    const customConfig: ProviderRegistrationConfig = {
        documentSelector: [
            { scheme: "file", language: "ssl" },
            { scheme: "file", pattern: "**/*.ssl" },
            { scheme: "file", pattern: "**/*.ssl.txt" },
        ],
        registerAll: false,
        specificProviders: {
            foldingRange: true,
            documentFormatting: true,
            completion: true,
            hover: false, // Disable hover
            documentSymbol: true,
            diagnostics: true,
            bracketMatching: true,
        },
    };

    const registration = registry.registerProviders(customConfig);
    context.subscriptions.push(...registration.disposables);

    console.log("SSL extension activated with custom configuration");
}

/**
 * Example 6: Conditional Provider Registration
 *
 * Register providers based on workspace configuration or user preferences.
 */
export function activateConditionalSSLSupport(context: vscode.ExtensionContext): void {
    const registry = SSLProviderRegistry.getInstance();

    // Get user configuration
    const config = vscode.workspace.getConfiguration("sslFormatter");
    const enableAdvancedFeatures = config.get<boolean>("enableAdvancedFeatures", true);
    const enableDiagnostics = config.get<boolean>("enableDiagnostics", true);

    // Create configuration based on user preferences
    const registrationConfig = enableAdvancedFeatures
        ? SSLProviderRegistry.getDefaultConfig() // Full support
        : SSLProviderRegistry.createSelectiveConfig({
              // Basic support
              foldingRange: true,
              documentFormatting: true,
              completion: true,
              diagnostics: enableDiagnostics,
          });

    const registration = registry.registerProviders(registrationConfig);
    context.subscriptions.push(...registration.disposables);

    console.log(
        `SSL extension activated with ${enableAdvancedFeatures ? "full" : "basic"} support`
    );
}

/**
 * Example 7: Access Registered Providers
 *
 * Show how to access and use individual providers after registration.
 */
export function demonstrateProviderAccess(): void {
    const registry = SSLProviderRegistry.getInstance();
    const registration = registry.getRegisteredProviders();

    if (registration) {
        // Access individual providers
        const foldingProvider = registration.providers.foldingRangeProvider;
        const formattingProvider = registration.providers.documentFormattingProvider;
        const diagnosticsProvider = registration.providers.diagnosticsProvider;

        // Use providers programmatically if needed
        console.log("Available providers:", Object.keys(registration.providers));

        // Example: Manually trigger diagnostics for a document
        const activeEditor = vscode.window.activeTextEditor;
        if (activeEditor && activeEditor.document.languageId === "ssl") {
            diagnosticsProvider.analyzeDiagnostics(activeEditor.document);
        }
    }
}

/**
 * Example 8: Extension Deactivation with Cleanup
 *
 * Proper cleanup when the extension is deactivated.
 */
export function deactivateSSLSupport(): void {
    const registry = SSLProviderRegistry.getInstance();

    // Clean up all registered providers
    registry.unregisterProviders();

    console.log("SSL extension deactivated and cleaned up");
}

/**
 * Example 9: Error Handling During Registration
 *
 * Handle potential errors during provider registration.
 */
export function activateWithErrorHandling(context: vscode.ExtensionContext): void {
    const registry = SSLProviderRegistry.getInstance();

    try {
        const registration = registry.registerProviders(SSLProviderRegistry.getDefaultConfig());

        context.subscriptions.push(...registration.disposables);
        console.log("SSL extension activated successfully");
    } catch (error) {
        console.error("Failed to activate SSL extension:", error);
        vscode.window.showErrorMessage(
            "Failed to activate SSL language support. Please check the extension logs."
        );
    }
}

/**
 * Example 10: Dynamic Provider Management
 *
 * Demonstrate how to dynamically change provider configuration during runtime.
 */
export function demonstrateDynamicProviderManagement(context: vscode.ExtensionContext): void {
    const registry = SSLProviderRegistry.getInstance();

    // Initial minimal configuration
    let registration = registry.registerProviders(
        SSLProviderRegistry.createSelectiveConfig({
            foldingRange: true,
            completion: true,
        })
    );

    context.subscriptions.push(...registration.disposables);

    // Register a command to upgrade to full support
    const upgradeCommand = vscode.commands.registerCommand(
        "ssl-formatter.upgradeToFullSupport",
        () => {
            try {
                // Unregister current providers
                registry.unregisterProviders();

                // Register full support
                registration = registry.registerProviders(SSLProviderRegistry.getDefaultConfig());

                // Note: In a real extension, you'd need to manage disposables properly
                // This is just for demonstration purposes
                vscode.window.showInformationMessage("Upgraded to full SSL language support!");
            } catch (error) {
                console.error("Failed to upgrade SSL support:", error);
                vscode.window.showErrorMessage("Failed to upgrade SSL language support.");
            }
        }
    );

    context.subscriptions.push(upgradeCommand);
}
