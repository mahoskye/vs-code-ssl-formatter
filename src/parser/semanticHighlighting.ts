/**
 * SSL Semantic Highlighting Provider
 * Provides semantic tokens for SSL code
 */

import * as vscode from "vscode";
import { SSLTokenizer, TokenType, Token } from "./tokenizer";

/**
 * Legend for semantic token types
 */
const tokenTypes = [
    "keyword",
    "function",
    "variable",
    "parameter",
    "property",
    "string",
    "number",
    "boolean",
    "comment",
    "operator",
    "type",
    "class",
    "namespace",
    "enum",
    "interface",
    "struct",
    "typeParameter",
    "event",
    "macro",
];

/**
 * Legend for semantic token modifiers
 */
const tokenModifiers = [
    "declaration",
    "definition",
    "readonly",
    "static",
    "deprecated",
    "abstract",
    "async",
    "modification",
    "documentation",
    "defaultLibrary",
];

/**
 * Provides semantic token information for SSL code
 */
export class SSLSemanticTokensProvider implements vscode.DocumentSemanticTokensProvider {
    private readonly legend: vscode.SemanticTokensLegend;
    private readonly tokenTypes: Record<string, number> = Object.create(null);
    private readonly tokenModifiers: Record<string, number> = Object.create(null);

    /**
     * Creates a new instance of the SSLSemanticTokensProvider
     */
    constructor() {
        this.legend = new vscode.SemanticTokensLegend(tokenTypes, tokenModifiers);

        // Initialize token type map
        tokenTypes.forEach((tokenType, index) => {
            this.tokenTypes[tokenType] = index;
        });

        // Initialize token modifier map
        tokenModifiers.forEach((tokenModifier, index) => {
            this.tokenModifiers[tokenModifier] = index;
        });
    }

    /**
     * Gets the semantic tokens legend
     * @returns The semantic tokens legend
     */
    public getLegend(): vscode.SemanticTokensLegend {
        return this.legend;
    }

    /**
     * Provides semantic tokens for a document
     * @param document The document to provide tokens for
     * @param token A cancellation token
     * @returns The semantic tokens data
     */
    public provideDocumentSemanticTokens(
        document: vscode.TextDocument,
        token: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.SemanticTokens> {
        const text = document.getText();
        const tokenizer = new SSLTokenizer(text);
        const tokens = tokenizer.tokenize();

        const builder = new vscode.SemanticTokensBuilder(this.legend);

        for (const sslToken of tokens) {
            // Skip whitespace
            if (sslToken.type === TokenType.whitespace) {
                continue;
            }

            const tokenType = this.getTokenType(sslToken);
            const tokenModifiers = this.getTokenModifiers(sslToken);

            if (tokenType !== undefined) {
                builder.push(
                    sslToken.range.start.line,
                    sslToken.range.start.character,
                    sslToken.range.end.character - sslToken.range.start.character,
                    this.tokenTypes[tokenType],
                    this.encodeTokenModifiers(tokenModifiers)
                );
            }
        }

        return builder.build();
    }

    /**
     * Gets the token type for a token
     * @param token The token
     * @returns The token type
     */ private getTokenType(token: Token): string | undefined {
        switch (token.type) {
            case TokenType.keyword:
                return "keyword";
            case TokenType.identifier:
                // Try to detect if this identifier is a function
                if (this.isFunctionIdentifier(token.value)) {
                    return "function";
                }
                return "variable";
            case TokenType.stringLiteral:
                return "string";
            case TokenType.numberLiteral:
                return "number";
            case TokenType.booleanLiteral:
                return "boolean";
            case TokenType.comment:
                return "comment";
            case TokenType.regionComment:
            case TokenType.endRegionComment:
                return "comment";
            case TokenType.assignmentOperator:
            case TokenType.arithmeticOperator:
            case TokenType.comparisonOperator:
            case TokenType.logicalOperator:
                return "operator";
            default:
                return undefined;
        }
    }

    /**
     * Gets the token modifiers for a token
     * @param token The token
     * @returns The token modifiers
     */ private getTokenModifiers(token: Token): string[] {
        const modifiers: string[] = [];

        // Detect declarations
        if (token.type === TokenType.identifier) {
            if (this.isDeclaration(token.value)) {
                modifiers.push("declaration");
            }
        }

        // Add documentation modifier for region comments
        if (token.type === TokenType.regionComment || token.type === TokenType.endRegionComment) {
            modifiers.push("documentation");
        }

        // Add more modifier logic as needed
        return modifiers;
    }

    /**
     * Encodes token modifiers into a numeric value
     * @param modifiers The token modifiers
     * @returns The encoded modifiers
     */
    private encodeTokenModifiers(modifiers: string[]): number {
        let result = 0;
        for (const modifier of modifiers) {
            const bit = this.tokenModifiers[modifier];
            if (bit !== undefined) {
                result |= 1 << bit;
            }
        }
        return result;
    }

    /**
     * Determines if an identifier is likely a function
     * This is a simplified heuristic - in a real implementation, you would
     * use symbol tables and scope analysis
     * @param name The identifier name
     * @returns True if the identifier is likely a function, false otherwise
     */
    private isFunctionIdentifier(name: string): boolean {
        // Common built-in functions in SSL
        const sslFunctions = [
            "DoProc",
            "ExecFunction",
            "SqlExecute",
            "LSearch",
            "CreateUDObject",
            "Branch",
            "ExecUDF",
            "Len",
            "CtoD",
            "Today",
            "Now",
        ];

        return sslFunctions.includes(name);
    }

    /**
     * Determines if an identifier is part of a declaration
     * This is a simplified heuristic - in a real implementation, you would
     * look at the context of the token
     * @param name The identifier name
     * @returns True if the identifier is likely a declaration, false otherwise
     */
    private isDeclaration(name: string): boolean {
        // This would need to be based on context
        // For example, following a :DECLARE, :PARAMETERS, etc.
        return false;
    }
}
