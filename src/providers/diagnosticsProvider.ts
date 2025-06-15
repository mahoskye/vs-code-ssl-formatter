/**
 * SSL Diagnostics Provider
 *
 * Provides diagnostic information (errors, warnings, hints) for SSL files.
 * Integrates with the tokenizer and parser to identify:
 * - Syntax errors
 * - Semantic errors
 * - Code style warnings
 * - Best practice suggestions
 */

import * as vscode from "vscode";
import { Tokenizer } from "../tokenizer";
import { Parser } from "../parser";
import { TokenType } from "../tokenizer/tokenType";
import {
    FormatterOptions,
    validateFormatterOptions,
    createFormatterOptionsFromConfig,
} from "../formatter/options";

/**
 * SSL Diagnostics Provider
 */
export class SSLDiagnosticsProvider {
    private diagnosticCollection: vscode.DiagnosticCollection;
    private readonly tokenizer: Tokenizer;

    constructor() {
        this.diagnosticCollection = vscode.languages.createDiagnosticCollection("ssl");
        this.tokenizer = new Tokenizer();
    }

    /**
     * Get the diagnostic collection
     */
    public getDiagnosticCollection(): vscode.DiagnosticCollection {
        return this.diagnosticCollection;
    }

    /**
     * Analyze a document and provide diagnostics
     */
    public analyzeDiagnostics(document: vscode.TextDocument): void {
        try {
            const diagnostics: vscode.Diagnostic[] = [];
            const text = document.getText(); // Tokenization diagnostics
            const tokenizeResult = this.tokenizer.tokenize(text);
            if (tokenizeResult.hasErrors && tokenizeResult.errors) {
                tokenizeResult.errors.forEach((error) => {
                    const diagnostic = new vscode.Diagnostic(
                        this.createRange(error.position.line, error.position.column, 1),
                        error.message,
                        vscode.DiagnosticSeverity.Error
                    );
                    diagnostic.source = "SSL Tokenizer";
                    diagnostic.code = error.code;
                    diagnostics.push(diagnostic);
                });
            }

            // Parsing diagnostics
            if (tokenizeResult.tokens && tokenizeResult.tokens.length > 0) {
                const parser = new Parser(tokenizeResult.tokens);
                const parseResult = parser.parse();
                if (!parseResult.success && parseResult.errors) {
                    parseResult.errors.forEach((error) => {
                        const diagnostic = new vscode.Diagnostic(
                            this.createRange(error.line, error.column, 1),
                            error.message,
                            vscode.DiagnosticSeverity.Error
                        );
                        diagnostic.source = "SSL Parser";
                        diagnostic.code = "parse-error";
                        diagnostics.push(diagnostic);
                    });
                }

                // Semantic analysis diagnostics
                if (parseResult.success && parseResult.ast) {
                    diagnostics.push(...this.getSemanticDiagnostics(parseResult.ast, document));
                }
            }

            // Style and best practice diagnostics
            diagnostics.push(...this.getStyleDiagnostics(document));

            // Formatter configuration diagnostics
            diagnostics.push(...this.getFormatterConfigDiagnostics());

            // Set diagnostics for the document
            this.diagnosticCollection.set(document.uri, diagnostics);
        } catch (error) {
            console.error("Error analyzing SSL diagnostics:", error);
            // Clear diagnostics on error to avoid stale information
            this.diagnosticCollection.set(document.uri, []);
        }
    }

    /**
     * Clear diagnostics for a document
     */
    public clearDiagnostics(document: vscode.TextDocument): void {
        this.diagnosticCollection.set(document.uri, []);
    }

    /**
     * Get semantic diagnostics from the AST
     */
    private getSemanticDiagnostics(ast: any, document: vscode.TextDocument): vscode.Diagnostic[] {
        const diagnostics: vscode.Diagnostic[] = [];

        // Check for unmatched control flow structures
        diagnostics.push(...this.checkControlFlowMatching(ast, document));

        // Check for unused variables
        diagnostics.push(...this.checkUnusedVariables(ast, document));

        // Check for Hungarian notation consistency
        diagnostics.push(...this.checkHungarianNotation(ast, document));

        // Check for SQL injection vulnerabilities
        diagnostics.push(...this.checkSqlInjectionRisks(ast, document));

        return diagnostics;
    }

    /**
     * Get style-related diagnostics
     */
    private getStyleDiagnostics(document: vscode.TextDocument): vscode.Diagnostic[] {
        const diagnostics: vscode.Diagnostic[] = [];
        const text = document.getText();
        const lines = text.split("\n");

        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            const lineRange = new vscode.Range(i, 0, i, line.length);

            // Check for trailing whitespace
            if (line.match(/\s+$/)) {
                const diagnostic = new vscode.Diagnostic(
                    lineRange,
                    "Line has trailing whitespace",
                    vscode.DiagnosticSeverity.Information
                );
                diagnostic.source = "SSL Style";
                diagnostic.code = "trailing-whitespace";
                diagnostic.tags = [vscode.DiagnosticTag.Unnecessary];
                diagnostics.push(diagnostic);
            }

            // Check for very long lines
            if (line.length > 120) {
                const diagnostic = new vscode.Diagnostic(
                    new vscode.Range(i, 120, i, line.length),
                    "Line exceeds recommended length (120 characters)",
                    vscode.DiagnosticSeverity.Warning
                );
                diagnostic.source = "SSL Style";
                diagnostic.code = "line-too-long";
                diagnostics.push(diagnostic);
            }

            // Check for tabs vs spaces consistency
            if (line.includes("\t") && line.includes("  ")) {
                const diagnostic = new vscode.Diagnostic(
                    lineRange,
                    "Mixed tabs and spaces for indentation",
                    vscode.DiagnosticSeverity.Warning
                );
                diagnostic.source = "SSL Style";
                diagnostic.code = "mixed-indentation";
                diagnostics.push(diagnostic);
            }

            // Check for missing semicolons after SSL statements
            if (this.isSSLStatement(line) && !line.trim().endsWith(";")) {
                const diagnostic = new vscode.Diagnostic(
                    lineRange,
                    "SSL statement should end with semicolon",
                    vscode.DiagnosticSeverity.Warning
                );
                diagnostic.source = "SSL Style";
                diagnostic.code = "missing-semicolon";
                diagnostics.push(diagnostic);
            }

            // Check for deprecated SSL syntax
            const deprecatedPatterns = [
                { pattern: /\b#\b/, message: "The '#' operator is deprecated, use '!=' instead" },
                { pattern: /\bTRUE\b|\bFALSE\b/, message: "Use .T. and .F. instead of TRUE/FALSE" },
            ];

            deprecatedPatterns.forEach(({ pattern, message }) => {
                const match = line.match(pattern);
                if (match) {
                    const startPos = line.indexOf(match[0]);
                    const diagnostic = new vscode.Diagnostic(
                        new vscode.Range(i, startPos, i, startPos + match[0].length),
                        message,
                        vscode.DiagnosticSeverity.Information
                    );
                    diagnostic.source = "SSL Style";
                    diagnostic.code = "deprecated-syntax";
                    diagnostic.tags = [vscode.DiagnosticTag.Deprecated];
                    diagnostics.push(diagnostic);
                }
            });
        }

        return diagnostics;
    }

    /**
     * Get formatter configuration diagnostics
     */
    private getFormatterConfigDiagnostics(): vscode.Diagnostic[] {
        const diagnostics: vscode.Diagnostic[] = [];

        try {
            // Get current formatter configuration
            const config = vscode.workspace.getConfiguration("sslFormatter");
            const formatterOptions = createFormatterOptionsFromConfig(config);

            // Validate formatter options
            const warnings = validateFormatterOptions(formatterOptions);

            warnings.forEach((warning) => {
                // Create a diagnostic for configuration warnings
                // Note: These don't have a specific location, so we'll use position 0,0
                const diagnostic = new vscode.Diagnostic(
                    new vscode.Range(0, 0, 0, 1),
                    `SSL Formatter Configuration: ${warning}`,
                    vscode.DiagnosticSeverity.Information
                );
                diagnostic.source = "SSL Formatter Config";
                diagnostic.code = "config-warning";
                diagnostics.push(diagnostic);
            });
        } catch (error) {
            console.error("Error validating formatter configuration:", error);
        }

        return diagnostics;
    }

    /**
     * Check for unmatched control flow structures
     */
    private checkControlFlowMatching(ast: any, document: vscode.TextDocument): vscode.Diagnostic[] {
        const diagnostics: vscode.Diagnostic[] = [];

        // This would require a more sophisticated AST traversal
        // For now, return empty array

        return diagnostics;
    }

    /**
     * Check for unused variables
     */
    private checkUnusedVariables(ast: any, document: vscode.TextDocument): vscode.Diagnostic[] {
        const diagnostics: vscode.Diagnostic[] = [];

        // This would require symbol table analysis
        // For now, return empty array

        return diagnostics;
    }

    /**
     * Check Hungarian notation consistency
     */
    private checkHungarianNotation(ast: any, document: vscode.TextDocument): vscode.Diagnostic[] {
        const diagnostics: vscode.Diagnostic[] = [];
        const text = document.getText();
        const lines = text.split("\n");

        // Check if Hungarian notation is being used consistently
        const variablePattern = /\b([nsbdao][A-Z][a-zA-Z0-9]*)\b/g;
        const inconsistentPattern = /\b([A-Z][a-zA-Z0-9]*)\b/g;

        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];

            // Skip comments and string literals
            if (line.trim().startsWith("/*") || line.includes('"') || line.includes("'")) {
                continue;
            }

            // Look for variables that don't follow Hungarian notation
            const matches = line.match(inconsistentPattern);
            if (matches) {
                matches.forEach((match) => {
                    // Skip SSL keywords and functions
                    if (this.isSSLKeywordOrFunction(match)) {
                        return;
                    }

                    const hungarianMatches = line.match(variablePattern);
                    if (hungarianMatches && !hungarianMatches.includes(match)) {
                        const startPos = line.indexOf(match);
                        const diagnostic = new vscode.Diagnostic(
                            new vscode.Range(i, startPos, i, startPos + match.length),
                            `Consider using Hungarian notation for variable '${match}' (e.g., n${match} for numbers, s${match} for strings)`,
                            vscode.DiagnosticSeverity.Hint
                        );
                        diagnostic.source = "SSL Style";
                        diagnostic.code = "hungarian-notation";
                        diagnostics.push(diagnostic);
                    }
                });
            }
        }

        return diagnostics;
    }

    /**
     * Check for potential SQL injection risks
     */
    private checkSqlInjectionRisks(ast: any, document: vscode.TextDocument): vscode.Diagnostic[] {
        const diagnostics: vscode.Diagnostic[] = [];
        const text = document.getText();
        const lines = text.split("\n");

        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];

            // Look for string concatenation in SQL statements
            if (line.includes("SqlExecute") && line.includes("+")) {
                const diagnostic = new vscode.Diagnostic(
                    new vscode.Range(i, 0, i, line.length),
                    "Potential SQL injection risk: Use parameterized queries instead of string concatenation",
                    vscode.DiagnosticSeverity.Warning
                );
                diagnostic.source = "SSL Security";
                diagnostic.code = "sql-injection-risk";
                diagnostics.push(diagnostic);
            }
        }

        return diagnostics;
    }

    /**
     * Check if a line contains an SSL statement
     */
    private isSSLStatement(line: string): boolean {
        const trimmed = line.trim();
        return trimmed.startsWith(":") && !trimmed.startsWith("/*");
    }

    /**
     * Check if a word is an SSL keyword or built-in function
     */
    private isSSLKeywordOrFunction(word: string): boolean {
        const sslKeywords = [
            "IF",
            "ELSE",
            "ENDIF",
            "WHILE",
            "ENDWHILE",
            "FOR",
            "NEXT",
            "PROCEDURE",
            "ENDPROC",
            "CLASS",
            "DECLARE",
            "PARAMETERS",
            "RETURN",
            "BEGINCASE",
            "CASE",
            "ENDCASE",
            "TRY",
            "CATCH",
            "ENDTRY",
            "ERROR",
            "REGION",
            "ENDREGION",
        ];

        const sslFunctions = [
            "SqlExecute",
            "LSearch",
            "Len",
            "SubStr",
            "Today",
            "Time",
            "Upper",
            "Lower",
            "LTrim",
            "RTrim",
            "AllTrim",
            "Str",
            "Val",
            "IIF",
            "IsNull",
            "Empty",
            "CreateUDObject",
            "DoProc",
            "ExecFunction",
        ];

        return sslKeywords.includes(word.toUpperCase()) || sslFunctions.includes(word);
    }

    /**
     * Create a VS Code Range from line/column information
     */
    private createRange(line: number, column: number, length: number): vscode.Range {
        return new vscode.Range(
            new vscode.Position(line, column),
            new vscode.Position(line, column + length)
        );
    }

    /**
     * Dispose of resources
     */
    public dispose(): void {
        this.diagnosticCollection.dispose();
    }
}
