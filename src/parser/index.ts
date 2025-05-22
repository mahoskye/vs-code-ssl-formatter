/**
 * Main entry point for the SSL parser
 */

// Re-export specific elements to avoid naming conflicts
import { TokenType, SSLTokenizer, Token } from "./tokenizer";
import * as AST from "./sslAst";
import * as AdditionalStatements from "./additionalStatements";
import * as ExprHelpers from "./expressionHelpers";
import { SSLParser, DiagnosticSeverity, ParserError } from "./sslParser";
import { SSLDiagnosticProvider } from "./diagnostic";
import { SSLSemanticTokensProvider } from "./semanticHighlighting";

// Export types
export { TokenType, SSLTokenizer, Token };
export { SSLParser, DiagnosticSeverity, ParserError };
export { SSLDiagnosticProvider };
export { SSLSemanticTokensProvider };

// Export AST namespace to avoid naming conflicts
export { AST, AdditionalStatements, ExprHelpers };
