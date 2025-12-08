import * as vscode from "vscode";
import { Lexer } from "./parsing/lexer";
import { Parser, Node, NodeType } from "./parsing/parser";

/**
 * Provides folding ranges for SSL files using the centralized Parser.
 */
export class SSLFoldingProvider implements vscode.FoldingRangeProvider {

    public provideFoldingRanges(
        document: vscode.TextDocument,
        context: vscode.FoldingContext,
        token: vscode.CancellationToken
    ): vscode.FoldingRange[] {
        const text = document.getText();
        const lexer = new Lexer(text);
        const tokens = lexer.tokenize();

        if (token && token.isCancellationRequested) {
            return [];
        }

        const parser = new Parser(tokens);
        const root = parser.parse();

        const foldingRanges: vscode.FoldingRange[] = [];
        const regionStack: Node[] = [];

        this.traverse(root, foldingRanges, regionStack);

        return foldingRanges;
    }

    private traverse(node: Node, ranges: vscode.FoldingRange[], regionStack: Node[]): void {
        // Handle Block Blocks (IF, WHILE, etc.)
        if (node.type === NodeType.Block) {
            // Only add folding range if it spans multiple lines
            if (node.endLine > node.startLine) {
                // VS Code uses 0-based line numbers, Parser uses 1-based
                ranges.push(new vscode.FoldingRange(node.startLine - 1, node.endLine - 1));
            }
        }
        else if (node.type === NodeType.RegionStart) {
            regionStack.push(node);
        }
        else if (node.type === NodeType.RegionEnd) {
            const startNode = regionStack.pop();
            if (startNode) {
                ranges.push(new vscode.FoldingRange(startNode.startLine - 1, node.endLine - 1, vscode.FoldingRangeKind.Region));
            }
        }

        // Recursively handle children
        for (const child of node.children) {
            this.traverse(child, ranges, regionStack);
        }
    }
}
