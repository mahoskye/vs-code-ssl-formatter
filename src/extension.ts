import * as vscode from "vscode";
import { SSLProviderRegistry } from "./providers";

/**
 * This function is called when the extension is activated.
 * It sets up the SSL language support using the centralized provider registry.
 */
export function activate(context: vscode.ExtensionContext) {
    // Get the provider registry instance
    const providerRegistry = SSLProviderRegistry.getInstance();

    // Register all SSL language service providers
    const registrationResult = providerRegistry.registerProviders(
        SSLProviderRegistry.getDefaultConfig()
    );

    // Add all disposables to the extension context for proper cleanup
    context.subscriptions.push(...registrationResult.disposables);

    // Log successful activation
    console.log("SSL Formatter extension is now active!");
    console.log("Registered SSL language providers:");
    console.log("- Folding Range Provider (supports SSL constructs per EBNF grammar)");
    console.log("- Document Formatting Provider (SSL code formatting)");
    console.log("- Completion Provider (SSL keywords, functions, operators)");
    console.log("- Hover Provider (SSL documentation and help)");
    console.log("- Document Symbol Provider (SSL outline and navigation)");
    console.log("- Diagnostics Provider (SSL syntax and semantic validation)");
    console.log("- Bracket Matching Commands (SSL keyword pairs)");
    console.log("- Document Event Listeners (real-time validation)");
}

/**
 * This function is called when the extension is deactivated.
 * It cleans up all registered providers and resources.
 */
export function deactivate() {
    // Clean up the provider registry
    const providerRegistry = SSLProviderRegistry.getInstance();
    providerRegistry.unregisterProviders();

    console.log("SSL Formatter extension has been deactivated and cleaned up.");
}
