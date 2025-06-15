/**
 * SSL Code Folding Range Provider
 *
 * Provides folding ranges for SSL language constructs based on the EBNF grammar.
 * Supports folding of:
 * - Procedure blocks (:PROCEDURE/:ENDPROC)
 * - Conditional blocks (:IF/:ENDIF, :ELSE)
 * - Loop blocks (:WHILE/:ENDWHILE, :FOR/:NEXT)
 * - Switch case blocks (:BEGINCASE/:ENDCASE, :CASE, :OTHERWISE)
 * - Try-catch blocks (:TRY/:ENDTRY, :CATCH, :FINALLY)
 * - Error handling blocks (:ERROR)
 * - Class definitions (:CLASS to end, :INHERIT)
 * - Region blocks (:REGION/:ENDREGION)
 * - Inline code blocks (:BEGININLINECODE/:ENDINLINECODE)
 * - Comment regions (/* region / /* endregion)
 * - Multi-line comments and comment blocks
 * - Array literals ({...})
 * - Code block literals ({|...|...})
 * - SQL statements (multi-line)
 */

import * as vscode from "vscode";
import { Tokenizer } from "../tokenizer";
import { Parser } from "../parser";
import { Token } from "../tokenizer/token";
import { TokenType } from "../tokenizer/tokenType";
import {
    ASTNode,
    ASTNodeType,
    ProgramNode,
    ProcedureStatementNode,
    IfStatementNode,
    WhileLoopNode,
    ForLoopNode,
    SwitchStatementNode,
    TryBlockNode,
    ClassDefinitionNode,
    RegionBlockNode,
    BlockCommentNode,
    RegionCommentNode,
    ErrorBlockStanzaNode,
    InlineCodeBlockNode,
    ArrayLiteralNode,
    CodeBlockLiteralNode,
} from "../parser/ast";

/**
 * Folding range types supported by VS Code
 */
enum FoldingRangeKind {
    Comment = vscode.FoldingRangeKind.Comment,
    Imports = vscode.FoldingRangeKind.Imports,
    Region = vscode.FoldingRangeKind.Region,
}

/**
 * SSL-specific folding range provider
 */
export class SSLFoldingRangeProvider implements vscode.FoldingRangeProvider {
    /**
     * Provide folding ranges for the given document
     */
    public provideFoldingRanges(
        document: vscode.TextDocument,
        context: vscode.FoldingContext,
        token: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.FoldingRange[]> {
        try {
            const ranges: vscode.FoldingRange[] = [];

            // Parse the document to get tokens
            const tokenizer = new Tokenizer();
            const tokenizationResult = tokenizer.tokenize(document.getText());

            if (tokenizationResult.hasErrors || tokenizationResult.tokens.length === 0) {
                return ranges;
            }

            // Get folding ranges from AST (when parsing succeeds)
            const parser = new Parser(tokenizationResult.tokens);
            const parseResult = parser.parse();

            if (parseResult.success) {
                this.extractFoldingRangesFromAST(parseResult.ast, ranges);
            }

            // Always get folding ranges from token-based analysis
            // This provides fallback when AST parsing fails
            this.extractTokenBasedFoldingRanges(tokenizationResult.tokens, ranges, document);

            // Sort ranges by start line
            ranges.sort((a, b) => a.start - b.start);

            return ranges;
        } catch (error) {
            console.error("Error providing folding ranges:", error);
            return [];
        }
    }

    /**
     * Extract folding ranges from the AST
     */
    private extractFoldingRangesFromAST(ast: ProgramNode, ranges: vscode.FoldingRange[]): void {
        this.traverseASTForFolding(ast, ranges);
    }
    /**
     * Recursively traverse AST nodes to find foldable constructs
     */
    private traverseASTForFolding(node: ASTNode, ranges: vscode.FoldingRange[]): void {
        if (!node) {
            return;
        }

        const startLine = node.startToken.range.start.line; // Use 0-based lines as VS Code expects
        const endLine = node.endToken.range.end.line;

        // Only create folding range if it spans multiple lines
        if (endLine > startLine) {
            switch (node.kind) {
                // Core language constructs
                case ASTNodeType.ProcedureStatement:
                    ranges.push(new vscode.FoldingRange(startLine, endLine));
                    break;

                // Conditional statements
                case ASTNodeType.IfStatement:
                case ASTNodeType.ConditionalStatement:
                    ranges.push(new vscode.FoldingRange(startLine, endLine));
                    break;

                // Loop statements
                case ASTNodeType.WhileLoop:
                case ASTNodeType.ForLoop:
                    ranges.push(new vscode.FoldingRange(startLine, endLine));
                    break;

                // Switch case statements
                case ASTNodeType.SwitchStatement:
                case ASTNodeType.BeginCaseStatement:
                    ranges.push(new vscode.FoldingRange(startLine, endLine));
                    break;

                // Individual case blocks
                case ASTNodeType.CaseBlock:
                case ASTNodeType.OtherwiseBlock:
                    ranges.push(new vscode.FoldingRange(startLine, endLine));
                    break;

                // Error handling statements
                case ASTNodeType.TryBlock:
                case ASTNodeType.ErrorHandlingStatement:
                case ASTNodeType.ErrorBlockStanza:
                    ranges.push(new vscode.FoldingRange(startLine, endLine));
                    break;

                case ASTNodeType.CatchBlock:
                case ASTNodeType.FinallyBlock:
                    ranges.push(new vscode.FoldingRange(startLine, endLine));
                    break;

                // Class definitions
                case ASTNodeType.ClassDefinition:
                    ranges.push(new vscode.FoldingRange(startLine, endLine));
                    break;

                // Region blocks
                case ASTNodeType.RegionBlock:
                    ranges.push(
                        new vscode.FoldingRange(startLine, endLine, FoldingRangeKind.Region)
                    );
                    break;

                // Inline code blocks
                case ASTNodeType.InlineCodeBlock:
                    ranges.push(new vscode.FoldingRange(startLine, endLine));
                    break;

                // Comments
                case ASTNodeType.BlockComment:
                    ranges.push(
                        new vscode.FoldingRange(startLine, endLine, FoldingRangeKind.Comment)
                    );
                    break;

                case ASTNodeType.RegionComment:
                    ranges.push(
                        new vscode.FoldingRange(startLine, endLine, FoldingRangeKind.Region)
                    );
                    break;

                // Array literals and code blocks
                case ASTNodeType.ArrayLiteral:
                    if (endLine > startLine) {
                        // Only for multi-line arrays
                        ranges.push(new vscode.FoldingRange(startLine, endLine));
                    }
                    break;

                case ASTNodeType.CodeBlockLiteral:
                    ranges.push(new vscode.FoldingRange(startLine, endLine));
                    break;

                // SQL statements (when they span multiple lines)
                case ASTNodeType.SqlExecute:
                case ASTNodeType.LSearch:
                case ASTNodeType.SqlStatement:
                    if (endLine > startLine) {
                        // Only for multi-line SQL
                        ranges.push(new vscode.FoldingRange(startLine, endLine));
                    }
                    break;
            }
        }

        // Recursively traverse child nodes
        this.traverseChildNodes(node, ranges);
    }
    /**
     * Traverse child nodes based on node type
     */
    private traverseChildNodes(node: ASTNode, ranges: vscode.FoldingRange[]): void {
        switch (node.kind) {
            case ASTNodeType.Program:
                const programNode = node as ProgramNode;
                programNode.body.forEach((child) => this.traverseASTForFolding(child, ranges));
                break;

            case ASTNodeType.ProcedureStatement:
                const procNode = node as ProcedureStatementNode;
                if (procNode.body) {
                    procNode.body.forEach((stmt) => this.traverseASTForFolding(stmt, ranges));
                }
                break;

            case ASTNodeType.IfStatement:
                const ifNode = node as IfStatementNode;
                if (ifNode.thenBranch) {
                    ifNode.thenBranch.forEach((stmt) => this.traverseASTForFolding(stmt, ranges));
                }
                if (ifNode.elseBranch) {
                    this.traverseASTForFolding(ifNode.elseBranch, ranges);
                }
                break;

            case ASTNodeType.WhileLoop:
                const whileNode = node as WhileLoopNode;
                if (whileNode.body) {
                    whileNode.body.forEach((stmt) => this.traverseASTForFolding(stmt, ranges));
                }
                break;

            case ASTNodeType.ForLoop:
                const forNode = node as ForLoopNode;
                if (forNode.body) {
                    forNode.body.forEach((stmt) => this.traverseASTForFolding(stmt, ranges));
                }
                break;

            case ASTNodeType.SwitchStatement:
                const switchNode = node as SwitchStatementNode;
                if (switchNode.cases) {
                    switchNode.cases.forEach((caseBlock) =>
                        this.traverseASTForFolding(caseBlock, ranges)
                    );
                }
                if (switchNode.otherwiseBlock) {
                    this.traverseASTForFolding(switchNode.otherwiseBlock, ranges);
                }
                break;

            case ASTNodeType.TryBlock:
                const tryNode = node as TryBlockNode;
                if (tryNode.tryStatements) {
                    tryNode.tryStatements.forEach((stmt) =>
                        this.traverseASTForFolding(stmt, ranges)
                    );
                }
                if (tryNode.catchBlock) {
                    this.traverseASTForFolding(tryNode.catchBlock, ranges);
                }
                if (tryNode.finallyBlock) {
                    this.traverseASTForFolding(tryNode.finallyBlock, ranges);
                }
                break;
            case ASTNodeType.ClassDefinition:
                const classNode = node as ClassDefinitionNode;
                if (classNode.members) {
                    classNode.members.forEach((member) =>
                        this.traverseASTForFolding(member, ranges)
                    );
                }
                break;
            case ASTNodeType.RegionBlock:
                const regionNode = node as RegionBlockNode;
                if (regionNode.statements) {
                    regionNode.statements.forEach((stmt) =>
                        this.traverseASTForFolding(stmt, ranges)
                    );
                }
                break;

            case ASTNodeType.InlineCodeBlock:
                const inlineCodeNode = node as InlineCodeBlockNode;
                if (inlineCodeNode.statements) {
                    inlineCodeNode.statements.forEach((stmt) =>
                        this.traverseASTForFolding(stmt, ranges)
                    );
                }
                break;

            case ASTNodeType.ErrorBlockStanza:
                const errorNode = node as ErrorBlockStanzaNode;
                if (errorNode.statements) {
                    errorNode.statements.forEach((stmt) =>
                        this.traverseASTForFolding(stmt, ranges)
                    );
                }
                break;

            case ASTNodeType.InlineCodeBlock:
                const inlineNode = node as InlineCodeBlockNode;
                // Inline code blocks typically contain raw code, not AST nodes
                // But if they have structured content, traverse it
                break;

            case ASTNodeType.ArrayLiteral:
                const arrayNode = node as ArrayLiteralNode;
                if (arrayNode.elements) {
                    arrayNode.elements.forEach((element) =>
                        this.traverseASTForFolding(element, ranges)
                    );
                }
                break;

            case ASTNodeType.CodeBlockLiteral:
                const codeBlockNode = node as CodeBlockLiteralNode;
                // Code block literals have their own internal structure
                // Traverse if they contain nested expressions
                break;

            case ASTNodeType.Assignment:
                const assignmentNode = node as any; // Using any since AssignmentNode interface is incomplete
                // Traverse the right-hand side expression of the assignment
                if (assignmentNode.right) {
                    this.traverseASTForFolding(assignmentNode.right, ranges);
                }
                // Traverse the left-hand side if it's complex (e.g., property access)
                if (assignmentNode.left) {
                    this.traverseASTForFolding(assignmentNode.left, ranges);
                }
                break;

            // Add support for other container nodes as needed
            default:
                // For nodes we don't explicitly handle, try to traverse any child arrays
                // This is a fallback for future extensibility
                this.traverseGenericNode(node, ranges);
                break;
        }
    }

    /**
     * Generic traversal for nodes we don't explicitly handle
     */
    private traverseGenericNode(node: any, ranges: vscode.FoldingRange[]): void {
        if (!node || typeof node !== "object") {
            return;
        }

        // Look for common array properties that might contain child nodes
        const commonArrayProps = [
            "body",
            "statements",
            "members",
            "elements",
            "cases",
            "parameters",
        ];

        for (const prop of commonArrayProps) {
            if (Array.isArray(node[prop])) {
                node[prop].forEach((child: any) => {
                    if (child && typeof child === "object" && child.kind) {
                        this.traverseASTForFolding(child, ranges);
                    }
                });
            }
        }

        // Look for single child node properties
        const commonNodeProps = ["elseBranch", "catchBlock", "finallyBlock", "otherwiseBlock"];

        for (const prop of commonNodeProps) {
            if (node[prop] && typeof node[prop] === "object" && node[prop].kind) {
                this.traverseASTForFolding(node[prop], ranges);
            }
        }
    }
    /**
     * Extract folding ranges from tokens for constructs not well-represented in AST
     */
    private extractTokenBasedFoldingRanges(
        tokens: Token[],
        ranges: vscode.FoldingRange[],
        document: vscode.TextDocument
    ): void {
        const keywordPairs = this.findKeywordPairs(tokens);
        // Add folding ranges for keyword pairs
        keywordPairs.forEach((pair) => {
            const startLine = pair.start.range.start.line;
            const endLine = pair.end.range.end.line;

            if (endLine > startLine) {
                let kind: vscode.FoldingRangeKind | undefined;

                // Determine folding range kind based on keyword type
                if (
                    pair.start.type === TokenType.REGION ||
                    pair.start.type === TokenType.REGION_COMMENT
                ) {
                    kind = FoldingRangeKind.Region;
                } else if (
                    pair.start.type === TokenType.BLOCK_COMMENT ||
                    pair.start.type === TokenType.SINGLE_LINE_COMMENT
                ) {
                    kind = FoldingRangeKind.Comment;
                }

                ranges.push(new vscode.FoldingRange(startLine, endLine, kind));
            }
        }); // Find multi-line comments
        this.findMultiLineComments(tokens, ranges);

        // Find multi-line SQL strings
        this.findMultiLineSqlStrings(tokens, ranges);

        // Find multi-line arrays
        this.findMultiLineArrays(tokens, ranges);

        // Find code block literals
        this.findCodeBlockLiterals(tokens, ranges);
    }
    /**
     * Find matching keyword pairs for folding
     */ private findKeywordPairs(tokens: Token[]): Array<{ start: Token; end: Token }> {
        const pairs: Array<{ start: Token; end: Token }> = [];
        const stack: Array<{ token: Token; type: string }> = [];

        for (let i = 0; i < tokens.length; i++) {
            const token = tokens[i];

            // Skip if not a colon token
            if (token.type !== TokenType.COLON) {
                // Handle non-colon array and code block literals
                if (token.value === "{") {
                    // Check if this is a code block start {|
                    if (token.type === TokenType.CODE_BLOCK_START) {
                        stack.push({ token, type: "CODEBLOCK" });
                    } else if (token.type === TokenType.LBRACE) {
                        stack.push({ token, type: "ARRAY" });
                    }
                } else if (token.value === "}" && token.type === TokenType.RBRACE) {
                    // Try to match with either ARRAY or CODEBLOCK
                    this.matchAndPopMultiple(stack, pairs, ["CODEBLOCK", "ARRAY"], token);
                }

                // Handle comment regions
                if (
                    token.type === TokenType.BLOCK_COMMENT ||
                    token.type === TokenType.SINGLE_LINE_COMMENT
                ) {
                    const commentText = token.value.toLowerCase();
                    if (commentText.includes("region ")) {
                        stack.push({ token, type: "COMMENT_REGION" });
                    } else if (commentText.includes("endregion")) {
                        this.matchAndPop(stack, pairs, "COMMENT_REGION", token);
                    }
                }

                // Handle specific region comment types
                if (token.type === TokenType.REGION_COMMENT) {
                    stack.push({ token, type: "COMMENT_REGION" });
                } else if (token.type === TokenType.ENDREGION_COMMENT) {
                    this.matchAndPop(stack, pairs, "COMMENT_REGION", token);
                }

                continue;
            }

            // Look for the next token after the colon
            const nextToken = i + 1 < tokens.length ? tokens[i + 1] : null;
            if (!nextToken) {
                continue;
            }

            const nextTokenValue = nextToken.value.toUpperCase();

            // Handle opening keywords (: followed by keyword)
            switch (nextTokenValue) {
                case "PROCEDURE":
                    stack.push({ token, type: "PROCEDURE" });
                    break;
                case "IF":
                    stack.push({ token, type: "IF" });
                    break;
                case "WHILE":
                    stack.push({ token, type: "WHILE" });
                    break;
                case "FOR":
                    stack.push({ token, type: "FOR" });
                    break;
                case "BEGINCASE":
                    stack.push({ token, type: "CASE" });
                    break;
                case "TRY":
                    stack.push({ token, type: "TRY" });
                    break;
                case "CLASS":
                    stack.push({ token, type: "CLASS" });
                    break;
                case "REGION":
                    stack.push({ token, type: "REGION" });
                    break;
                case "BEGININLINECODE":
                    stack.push({ token, type: "INLINECODE" });
                    break;
                case "ERROR":
                    stack.push({ token, type: "ERROR" });
                    break;
            }

            // Handle closing keywords (: followed by keyword)
            switch (nextTokenValue) {
                case "ENDPROC":
                    this.matchAndPop(stack, pairs, "PROCEDURE", token);
                    break;
                case "ENDIF":
                    this.matchAndPop(stack, pairs, "IF", token);
                    break;
                case "ENDWHILE":
                    this.matchAndPop(stack, pairs, "WHILE", token);
                    break;
                case "NEXT":
                    this.matchAndPop(stack, pairs, "FOR", token);
                    break;
                case "ENDCASE":
                    this.matchAndPop(stack, pairs, "CASE", token);
                    break;
                case "ENDTRY":
                    this.matchAndPop(stack, pairs, "TRY", token);
                    break;
                case "ENDREGION":
                    this.matchAndPop(stack, pairs, "REGION", token);
                    break;
                case "ENDINLINECODE":
                    this.matchAndPop(stack, pairs, "INLINECODE", token);
                    break;
            }
        }

        return pairs;
    }
    /**
     * Match and pop from stack to create keyword pairs
     */
    private matchAndPop(
        stack: Array<{ token: Token; type: string }>,
        pairs: Array<{ start: Token; end: Token }>,
        expectedType: string,
        endToken: Token
    ): void {
        // Find the most recent matching opening token
        for (let i = stack.length - 1; i >= 0; i--) {
            if (stack[i].type === expectedType) {
                const startItem = stack.splice(i, 1)[0];
                pairs.push({ start: startItem.token, end: endToken });
                break;
            }
        }
    }

    /**
     * Match and pop from stack with multiple possible types
     */
    private matchAndPopMultiple(
        stack: Array<{ token: Token; type: string }>,
        pairs: Array<{ start: Token; end: Token }>,
        expectedTypes: string[],
        endToken: Token
    ): void {
        // Find the most recent matching opening token from any of the expected types
        for (let i = stack.length - 1; i >= 0; i--) {
            if (expectedTypes.includes(stack[i].type)) {
                const startItem = stack.splice(i, 1)[0];
                pairs.push({ start: startItem.token, end: endToken });
                break;
            }
        }
    }
    /**
     * Find multi-line comments for folding
     */
    private findMultiLineComments(tokens: Token[], ranges: vscode.FoldingRange[]): void {
        let commentStart: Token | null = null;
        let commentBlockStart: Token | null = null;

        for (let i = 0; i < tokens.length; i++) {
            const token = tokens[i];

            if (
                token.type === TokenType.BLOCK_COMMENT ||
                token.type === TokenType.SINGLE_LINE_COMMENT
            ) {
                if (!commentStart) {
                    commentStart = token;
                    commentBlockStart = token;
                } else {
                    // Check if this comment is reasonably close to the previous comment
                    // Allow for some flexibility in line numbering (e.g., tokenizer issues)
                    const prevLine = commentStart.range.start.line;
                    const currentLine = token.range.start.line;

                    // Consider comments consecutive if they're within a reasonable gap
                    // This accounts for potential tokenizer line numbering issues
                    if (currentLine > prevLine + 3) {
                        // Significant gap in comments, end previous block and start new one
                        this.addCommentBlockRange(commentBlockStart, commentStart, ranges);
                        commentBlockStart = token;
                    }
                    commentStart = token;
                }
            } else if (commentStart && token.type !== TokenType.NEWLINE) {
                // End of comment block - ignore newlines to allow consecutive single-line comments
                this.addCommentBlockRange(commentBlockStart, commentStart, ranges);
                commentStart = null;
                commentBlockStart = null;
            }
        }

        // Handle comment block that goes to end of file
        if (commentStart && commentBlockStart) {
            this.addCommentBlockRange(commentBlockStart, commentStart, ranges);
        }
    }

    /**
     * Add a comment block range if it spans multiple lines
     */
    private addCommentBlockRange(
        startToken: Token | null,
        endToken: Token | null,
        ranges: vscode.FoldingRange[]
    ): void {
        if (!startToken || !endToken) {
            return;
        }

        const startLine = startToken.range.start.line;
        const endLine = endToken.range.end.line;

        if (endLine > startLine) {
            ranges.push(new vscode.FoldingRange(startLine, endLine, FoldingRangeKind.Comment));
        }
    }

    /**
     * Find multi-line SQL strings for folding
     */ private findMultiLineSqlStrings(tokens: Token[], ranges: vscode.FoldingRange[]): void {
        for (const token of tokens) {
            // Check for SQL_STRING tokens or multi-line STRING tokens that might contain SQL
            if (
                token.type === TokenType.SQL_STRING ||
                (token.type === TokenType.STRING && this.isMultiLineString(token))
            ) {
                const startLine = token.range.start.line;
                const endLine = token.range.end.line;

                // Only fold multi-line SQL strings
                if (endLine > startLine) {
                    ranges.push(new vscode.FoldingRange(startLine, endLine));
                }
            }
        }
    }

    /**
     * Helper method to determine if a string token spans multiple lines
     */
    private isMultiLineString(token: Token): boolean {
        return token.range.end.line > token.range.start.line;
    }

    /**
     * Find multi-line array literals for folding
     */
    private findMultiLineArrays(tokens: Token[], ranges: vscode.FoldingRange[]): void {
        const stack: Token[] = [];

        for (const token of tokens) {
            if (token.type === TokenType.LBRACE) {
                stack.push(token);
            } else if (token.type === TokenType.RBRACE && stack.length > 0) {
                const startToken = stack.pop()!;
                const startLine = startToken.range.start.line;
                const endLine = token.range.end.line;

                // Only fold multi-line arrays
                if (endLine > startLine) {
                    ranges.push(new vscode.FoldingRange(startLine, endLine));
                }
            }
        }
    }

    /**
     * Find code block literals for folding
     */
    private findCodeBlockLiterals(tokens: Token[], ranges: vscode.FoldingRange[]): void {
        const stack: Token[] = [];

        for (const token of tokens) {
            if (token.type === TokenType.CODE_BLOCK_START) {
                stack.push(token);
            } else if (token.type === TokenType.RBRACE && stack.length > 0) {
                const startToken = stack.pop()!;
                const startLine = startToken.range.start.line;
                const endLine = token.range.end.line;

                // Fold code block literals that span multiple lines
                if (endLine > startLine) {
                    ranges.push(new vscode.FoldingRange(startLine, endLine));
                }
            }
        }
    }
}
