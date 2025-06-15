/**
 * SSL Document Symbol Provider
 *
 * Provides document outline and symbol navigation for SSL files.
 * Extracts and presents symbols like:
 * - Classes and their members
 * - Procedures and their parameters
 * - Variables and declarations
 * - Regions and code sections
 */

import * as vscode from "vscode";
import { Parser } from "../parser";
import { Tokenizer } from "../tokenizer";
import { ASTNode, ProgramNode } from "../parser/ast";
import { ASTNodeType } from "../parser/ast/base";

/**
 * SSL Document Symbol Provider
 */
export class SSLDocumentSymbolProvider implements vscode.DocumentSymbolProvider {
    /**
     * Provide document symbols for the given document
     */
    public provideDocumentSymbols(
        document: vscode.TextDocument,
        token: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.DocumentSymbol[]> {
        try {
            // Parse the document
            const text = document.getText();
            const tokenizer = new Tokenizer();
            const tokenizeResult = tokenizer.tokenize(text);

            if (tokenizeResult.hasErrors || tokenizeResult.tokens.length === 0) {
                return [];
            }

            const parser = new Parser(tokenizeResult.tokens);
            const parseResult = parser.parse();

            if (!parseResult.success || !parseResult.ast) {
                return [];
            }

            // Extract symbols from the AST
            return this.extractSymbolsFromAST(parseResult.ast, document);
        } catch (error) {
            console.error("Error providing SSL document symbols:", error);
            return [];
        }
    }

    /**
     * Extract symbols from the AST
     */
    private extractSymbolsFromAST(
        ast: ProgramNode,
        document: vscode.TextDocument
    ): vscode.DocumentSymbol[] {
        const symbols: vscode.DocumentSymbol[] = [];

        if (!ast.body) {
            return symbols;
        }

        for (const node of ast.body) {
            const symbol = this.createSymbolFromNode(node, document);
            if (symbol) {
                symbols.push(symbol);
            }
        }

        return symbols;
    }

    /**
     * Create a document symbol from an AST node
     */
    private createSymbolFromNode(
        node: ASTNode,
        document: vscode.TextDocument
    ): vscode.DocumentSymbol | null {
        if (!node || !node.startToken || !node.endToken) {
            return null;
        }

        const range = this.getRange(node, document);
        const selectionRange = this.getSelectionRange(node, document);

        switch (node.kind) {
            case ASTNodeType.ClassDefinition:
                return this.createClassSymbol(node as any, document, range, selectionRange);

            case ASTNodeType.ProcedureStatement:
                return this.createProcedureSymbol(node as any, document, range, selectionRange);

            case ASTNodeType.DeclarationStatement:
            case ASTNodeType.ParametersStatement:
            case ASTNodeType.PublicStatement:
                return this.createDeclarationSymbol(node as any, document, range, selectionRange);

            case ASTNodeType.RegionBlock:
                return this.createRegionSymbol(node as any, document, range, selectionRange);

            case ASTNodeType.IfStatement:
            case ASTNodeType.WhileLoop:
            case ASTNodeType.ForLoop:
            case ASTNodeType.SwitchStatement:
            case ASTNodeType.TryBlock:
                return this.createControlFlowSymbol(node, document, range, selectionRange);

            default:
                return null;
        }
    }

    /**
     * Create symbol for class definitions
     */
    private createClassSymbol(
        node: any,
        document: vscode.TextDocument,
        range: vscode.Range,
        selectionRange: vscode.Range
    ): vscode.DocumentSymbol {
        const className = node.declaration?.name?.value || "UnnamedClass";
        const symbol = new vscode.DocumentSymbol(
            className,
            "SSL Class",
            vscode.SymbolKind.Class,
            range,
            selectionRange
        );

        // Add class members as children
        if (node.members && Array.isArray(node.members)) {
            for (const member of node.members) {
                const memberSymbol = this.createSymbolFromNode(member, document);
                if (memberSymbol) {
                    symbol.children.push(memberSymbol);
                }
            }
        }

        // Add inheritance information to detail
        if (node.inheritance?.className) {
            symbol.detail = `extends ${node.inheritance.className.value}`;
        }

        return symbol;
    }

    /**
     * Create symbol for procedure definitions
     */
    private createProcedureSymbol(
        node: any,
        document: vscode.TextDocument,
        range: vscode.Range,
        selectionRange: vscode.Range
    ): vscode.DocumentSymbol {
        const procName = node.name?.value || "UnnamedProcedure";
        const symbol = new vscode.DocumentSymbol(
            procName,
            "SSL Procedure",
            vscode.SymbolKind.Function,
            range,
            selectionRange
        );

        // Add parameter information to detail
        const parameters = this.extractParameterNames(node);
        if (parameters.length > 0) {
            symbol.detail = `(${parameters.join(", ")})`;
        }

        // Add local variables and nested constructs as children
        if (node.body && Array.isArray(node.body)) {
            for (const statement of node.body) {
                const childSymbol = this.createSymbolFromNode(statement, document);
                if (childSymbol) {
                    symbol.children.push(childSymbol);
                }
            }
        }

        return symbol;
    }

    /**
     * Create symbol for variable declarations
     */
    private createDeclarationSymbol(
        node: any,
        document: vscode.TextDocument,
        range: vscode.Range,
        selectionRange: vscode.Range
    ): vscode.DocumentSymbol | null {
        let symbolKind: vscode.SymbolKind;
        let symbolName: string;
        let symbolDetail: string;

        switch (node.kind) {
            case ASTNodeType.ParametersStatement:
                symbolKind = vscode.SymbolKind.Variable;
                symbolName = "Parameters";
                symbolDetail = "Procedure parameters";
                break;
            case ASTNodeType.PublicStatement:
                symbolKind = vscode.SymbolKind.Variable;
                symbolName = "Public Variables";
                symbolDetail = "Public variable declarations";
                break;
            case ASTNodeType.DeclarationStatement:
            default:
                symbolKind = vscode.SymbolKind.Variable;
                symbolName = "Variables";
                symbolDetail = "Local variable declarations";
                break;
        }

        // Extract variable names
        const variableNames = this.extractVariableNames(node);
        if (variableNames.length === 0) {
            return null;
        }

        // Create a parent symbol for the declaration group
        const symbol = new vscode.DocumentSymbol(
            symbolName,
            symbolDetail,
            symbolKind,
            range,
            selectionRange
        );

        // Add individual variables as children
        variableNames.forEach((varName) => {
            const varSymbol = new vscode.DocumentSymbol(
                varName,
                this.getVariableTypeHint(varName),
                vscode.SymbolKind.Variable,
                range, // Use same range as parent for simplicity
                selectionRange
            );
            symbol.children.push(varSymbol);
        });

        return symbol;
    }

    /**
     * Create symbol for region blocks
     */
    private createRegionSymbol(
        node: any,
        document: vscode.TextDocument,
        range: vscode.Range,
        selectionRange: vscode.Range
    ): vscode.DocumentSymbol {
        const regionName = node.name?.value || "Code Region";
        return new vscode.DocumentSymbol(
            regionName,
            "SSL Region",
            vscode.SymbolKind.Namespace,
            range,
            selectionRange
        );
    }

    /**
     * Create symbol for control flow constructs
     */
    private createControlFlowSymbol(
        node: ASTNode,
        document: vscode.TextDocument,
        range: vscode.Range,
        selectionRange: vscode.Range
    ): vscode.DocumentSymbol {
        let symbolName: string;
        let symbolDetail: string;

        switch (node.kind) {
            case ASTNodeType.IfStatement:
                symbolName = "IF Statement";
                symbolDetail = "Conditional block";
                break;
            case ASTNodeType.WhileLoop:
                symbolName = "WHILE Loop";
                symbolDetail = "Loop block";
                break;
            case ASTNodeType.ForLoop:
                symbolName = "FOR Loop";
                symbolDetail = "For loop block";
                break;
            case ASTNodeType.SwitchStatement:
                symbolName = "BEGINCASE Statement";
                symbolDetail = "Switch-case block";
                break;
            case ASTNodeType.TryBlock:
                symbolName = "TRY Block";
                symbolDetail = "Error handling block";
                break;
            default:
                symbolName = "Control Flow";
                symbolDetail = "Control flow construct";
                break;
        }

        return new vscode.DocumentSymbol(
            symbolName,
            symbolDetail,
            vscode.SymbolKind.Method, // Use Method for control flow blocks
            range,
            selectionRange
        );
    }

    /**
     * Get the full range of a node
     */ private getRange(node: ASTNode, document: vscode.TextDocument): vscode.Range {
        if (!node.startToken || !node.endToken || !node.startToken.range || !node.endToken.range) {
            return new vscode.Range(0, 0, 0, 0);
        }

        const startPos = new vscode.Position(
            node.startToken.range.start.line,
            node.startToken.range.start.column
        );

        const endPos = new vscode.Position(
            node.endToken.range.end.line,
            node.endToken.range.end.column
        );

        return new vscode.Range(startPos, endPos);
    }

    /**
     * Get the selection range (typically the identifier/name)
     */
    private getSelectionRange(node: ASTNode, document: vscode.TextDocument): vscode.Range {
        // For most nodes, the selection range is the same as the full range
        // In a more sophisticated implementation, you would identify the specific
        // token that represents the "name" of the construct
        return this.getRange(node, document);
    }

    /**
     * Extract parameter names from a procedure node
     */
    private extractParameterNames(procedureNode: any): string[] {
        const parameters: string[] = [];

        // Look for parameter declarations in the procedure body
        if (procedureNode.body && Array.isArray(procedureNode.body)) {
            for (const statement of procedureNode.body) {
                if (statement.kind === ASTNodeType.ParametersStatement) {
                    const paramNames = this.extractVariableNames(statement);
                    parameters.push(...paramNames);
                }
            }
        }

        return parameters;
    }

    /**
     * Extract variable names from a declaration node
     */
    private extractVariableNames(node: any): string[] {
        const variables: string[] = [];

        // This is simplified - in a real implementation, you'd traverse the
        // identifier list properly
        if (node.identifiers && Array.isArray(node.identifiers)) {
            for (const identifier of node.identifiers) {
                if (identifier.value) {
                    variables.push(identifier.value);
                }
            }
        }

        return variables;
    }

    /**
     * Get type hint for a variable based on Hungarian notation
     */
    private getVariableTypeHint(variableName: string): string {
        const prefix = variableName.charAt(0).toLowerCase();

        switch (prefix) {
            case "n":
                return "Number";
            case "s":
                return "String";
            case "b":
                return "Boolean";
            case "d":
                return "Date";
            case "a":
                return "Array";
            case "o":
                return "Object";
            default:
                return "Variable";
        }
    }
}
