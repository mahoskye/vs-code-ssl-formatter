/**
 * Provider module exports
 *
 * This module exports all VS Code language service providers for SSL.
 */

export { SSLFoldingRangeProvider } from "./foldingRangeProvider";
export { SSLBracketMatchingProvider } from "./bracketMatchingProvider";
export { SSLDocumentFormattingProvider } from "./documentFormattingProvider";
export { SSLCompletionProvider } from "./completionProvider";
export { SSLHoverProvider } from "./hoverProvider";
export { SSLDocumentSymbolProvider } from "./documentSymbolProvider";
export { SSLDiagnosticsProvider } from "./diagnosticsProvider";

// Provider Registry for centralized management
export {
    SSLProviderRegistry,
    type ProviderRegistrationResult,
    type ProviderRegistrationConfig,
} from "./providerRegistry";
