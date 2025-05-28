/**
 * SSL Formatting Provider - Document formatting for SSL (STARLIMS Scripting Language)
 * Based on the SSL EBNF grammar specification and modern parsing techniques
 */

import * as vscode from "vscode";
import { SSLTokenizer, Token, TokenType } from "../core/tokenizer"; // Changed from tokenizeSSL, Added Token, TokenType
import { SSLParser, ASTNode, ASTNodeType } from "../core/parser"; // Changed from parseTokens and added ASTNode, ASTNodeType
import { CommentFormattingRule } from "./rules/commentFormattingRule";
import { IndentationRule } from "./rules/indentationRule";
import { OperatorSpacingRule } from "./rules/operatorSpacingRule";
import { BlockAlignmentRule } from "./rules/blockAlignmentRule";
import { ColonSpacingRule } from "./rules/colonSpacingRule";
import { CommaSpacingRule } from "./rules/commaSpacingRule";
import { getIndentationString } from "../utils/indentationUtils"; // Added import

export interface FormattingOptions extends vscode.FormattingOptions {
    // Potentially add custom options here if needed beyond vscode.FormattingOptions
    maxLineLength: number;
    indentStyle: "space" | "tab";
}

export interface FormattingRule {
    name: string;
    description: string;
    apply(line: string, context: FormattingContext): string;
}

export interface FormattingContext {
    indentLevel: number;
    blockType: string | null; // Type of the keyword on the current line (e.g., "IF", "ELSE") or null
    previousLine: string | null;
    nextLine: string | null;
    lineNumber: number;
    options: FormattingOptions;
    ast?: ASTNode; // The AST for the entire document
    lineTokens: Token[]; // Tokens for the current line
    blockDepth: number; // Anticipated depth for the *next* line, based on current line's keywords
    inMultiLineConstruct: boolean;
    constructType: string | null; // Type of multi-line construct (e.g., "array", "functionCall")
    enclosingASTBlockType?: ASTNodeType | null; // Type of the AST block node enclosing the current line
}

/**
 * SSL Document Formatting Provider
 */
export class SSLFormattingProvider implements vscode.DocumentFormattingEditProvider {
    private rules: FormattingRule[];
    constructor() {
        this.rules = [
            new CommentFormattingRule(), // Apply comment formatting first
            new OperatorSpacingRule(),
            new CommaSpacingRule(),
            new BlockAlignmentRule(),
            new ColonSpacingRule(),
            new IndentationRule(), // Apply indentation last
        ];
    }

    public async provideDocumentFormattingEdits(
        // Added async
        document: vscode.TextDocument,
        options: FormattingOptions, // Use our extended options type
        token: vscode.CancellationToken
    ): Promise<vscode.TextEdit[]> {
        // Changed return type
        if (token.isCancellationRequested) {
            return [];
        }

        const fullText = document.getText();

        try {
            const text = document.getText();

            const formattingOptions: FormattingOptions = {
                tabSize: options.tabSize,
                insertSpaces: options.insertSpaces,
                maxLineLength: 90, // Default SSL line length
                indentStyle: options.insertSpaces ? "space" : "tab",
            };
            const formattedText = await this.formatText(text, formattingOptions, token); // Defensive check: ensure formattedText is a string
            if (typeof formattedText !== "string" || formattedText === undefined) {
                console.error(
                    "formatText returned invalid result:",
                    typeof formattedText,
                    formattedText
                );
                return []; // Return no edits if formatting failed
            }

            // Return a single edit that replaces the entire document
            const lastLineId = document.lineCount - 1;
            const lineAtLast = document.lineAt(lastLineId); // Get the TextLine object
            const lastLineLength = lineAtLast ? lineAtLast.text.length : 0; // Ensure lineAtLast is not undefined

            // console.log(
            //     `SSLFormattingProvider: lineCount=<span class="math-inline">\{document\.lineCount\}, lastLineId\=</span>{lastLineId}, lastLineLength=${lastLineLength}`
            // );

            const range = new vscode.Range(0, 0, lastLineId, lastLineLength);
            // console.log("SSLFormattingProvider: Created range:", JSON.stringify(range));
            const edit = vscode.TextEdit.replace(range, formattedText);
            return [edit];
        } catch (error) {
            console.error("SSL Formatting error:", error);
            vscode.window.showErrorMessage(
                `SSL Formatting error: ${error instanceof Error ? error.message : String(error)}`
            );
            return [];
        }
    }
    private async formatText(
        text: string,
        options: FormattingOptions,
        token: vscode.CancellationToken
    ): Promise<string> {
        const startTime = Date.now();
        const MAX_PROCESSING_TIME = 5000; // 5 second timeout

        let allTokens: Token[];
        let ast: ASTNode;

        try {
            // Check cancellation before expensive operations
            if (token && token.isCancellationRequested) {
                return text;
            }

            // Tokenize the input
            const tokenizer = new SSLTokenizer(text);
            allTokens = tokenizer.tokenize();

            // Check cancellation and timeout
            if (token && token.isCancellationRequested) {
                return text;
            }
            if (Date.now() - startTime > MAX_PROCESSING_TIME) {
                console.warn("SSL Formatting timeout during tokenization");
                return text;
            }

            // Parse into AST for structural understanding
            const parser = new SSLParser(allTokens); // Use allTokens for parsing
            ast = parser.parse();

            if (Date.now() - startTime > MAX_PROCESSING_TIME) {
                console.warn("SSL Formatting timeout during parsing");
                return text;
            }
        } catch (error) {
            console.error("SSL Tokenizer/Parser error, proceeding with minimal context:", error);
            // Create a minimal AST to avoid crashes if parsing fails
            ast = { type: ASTNodeType.program, children: [] };
            allTokens = []; // Avoid using potentially incomplete tokens
        }

        // Process line by line with context from AST
        const lines = text.split(/\r?\n/);
        const formattedLines: string[] = [];
        let currentBlockDepth = 0;

        const MAX_LINES = 10000;
        const actualLines = Math.min(lines.length, MAX_LINES);

        if (lines.length > MAX_LINES) {
            console.warn(
                `SSL Formatting limiting processing to ${MAX_LINES} lines (original: ${lines.length})`
            );
        }

        for (let i = 0; i < actualLines; i++) {
            if (Date.now() - startTime > MAX_PROCESSING_TIME) {
                console.warn(`SSL Formatting timeout at line ${i + 1}/${lines.length}`);
                return formattedLines.length > 0 ? formattedLines.join("\n") : text;
            }

            if (token && token.isCancellationRequested) {
                return formattedLines.length > 0 ? formattedLines.join("\n") : text;
            }

            const originalLine = lines[i];
            const lineTokens = allTokens.filter((t) => t.position.line === i + 1);

            // Determine indentation and block context BEFORE applying rules
            const lineAnalysisResult = this.analyzeLineContext(
                originalLine,
                ast,
                i + 1,
                currentBlockDepth,
                lineTokens
            );

            const indentLevelForCurrentLine = lineAnalysisResult.indentLevel;

            let currentLineText = originalLine;
            const context: FormattingContext = {
                indentLevel: indentLevelForCurrentLine,
                blockType: lineAnalysisResult.blockType,
                previousLine: i > 0 ? lines[i - 1] : null,
                nextLine: i < lines.length - 1 ? lines[i + 1] : null,
                lineNumber: i + 1,
                options,
                ast,
                lineTokens,
                blockDepth: lineAnalysisResult.blockDepth,
                constructType: this.getConstructType(
                    originalLine,
                    lineTokens,
                    lineAnalysisResult.blockType,
                    ast, // Added ast
                    i + 1 // Added lineNumber (1-based)
                ),
                inMultiLineConstruct: false,
                enclosingASTBlockType: lineAnalysisResult.enclosingASTBlockType, // Added
            };
            context.inMultiLineConstruct = this.isMultiLineConstruct(
                originalLine,
                lineTokens,
                i, // 0-based index for lines array
                lines,
                context.constructType,
                ast, // Added ast
                options // Added options
            );

            // Apply general formatting rules
            for (const rule of this.rules) {
                if (token && token.isCancellationRequested) {
                    return formattedLines.length > 0 ? formattedLines.join("\n") : text;
                }
                if (rule.name !== "IndentationRule") {
                    currentLineText = rule.apply(currentLineText, context);
                }
            }

            // Apply indentation based on the calculated level
            const indentationString = getIndentationString(
                indentLevelForCurrentLine,
                options.tabSize,
                options.insertSpaces
            );
            // Preserve leading whitespace if the line is empty or already all whitespace
            if (currentLineText.trim().length === 0) {
                // Line is empty or all whitespace, keep it as is, but ensure it's just one line (no newlines from rules)
                currentLineText = currentLineText.split(/\r?\n/)[0];
            } else {
                currentLineText = indentationString + currentLineText.trimStart();
            }

            formattedLines.push(currentLineText);

            currentBlockDepth = lineAnalysisResult.blockDepth;
        }
        const result = formattedLines.join("\n");

        if (typeof result !== "string") {
            console.error("Formatted result is not a string:", result);
            return text;
        }

        return result;
    }

    private analyzeLineContext(
        line: string,
        ast: ASTNode,
        lineNumber: number,
        currentBlockDepth: number, // This is the keyword-driven depth
        lineTokens: Token[]
    ): {
        indentLevel: number;
        blockType: string | null;
        blockDepth: number; // Keyword-driven depth for *next* line
        enclosingASTBlockType: ASTNodeType | null;
    } {
        let keywordDrivenBlockDepth = currentBlockDepth;
        let indentLevelForThisLine = currentBlockDepth;
        let newBlockType: string | null = null;

        let enclosingAstBlock: ASTNode | null = null;
        let astBasedIndentLevel = currentBlockDepth;

        if (ast && ast.children.length > 0) {
            enclosingAstBlock = this.findEnclosingASTBlockNode(ast, lineNumber);
            if (enclosingAstBlock) {
                astBasedIndentLevel = this.getASTNodeDepth(ast, enclosingAstBlock);
            }
        }

        if (lineTokens.length > 0 && lineTokens[0].type === TokenType.colon) {
            const keywordToken = lineTokens.length > 1 ? lineTokens[1] : null;

            if (keywordToken) {
                const keywordTokenType = keywordToken.type;
                newBlockType = this.getBlockTypeFromToken(keywordTokenType);

                const startTokens = [
                    TokenType.if,
                    TokenType.while,
                    TokenType.for,
                    TokenType.procedure,
                    TokenType.begincase,
                    TokenType.try,
                    TokenType.class,
                    TokenType.region,
                    TokenType.error,
                    TokenType.begininlinecode,
                ];
                const endTokens = [
                    TokenType.endif,
                    TokenType.endwhile,
                    TokenType.next,
                    TokenType.endproc,
                    TokenType.endcase,
                    TokenType.endtry,
                    TokenType.endregion,
                    TokenType.endinlinecode,
                    TokenType.endclass,
                ];
                const intermediateTokens = [
                    TokenType.else,
                    TokenType.case,
                    TokenType.otherwise,
                    TokenType.catch,
                    TokenType.finally,
                ];

                if (endTokens.includes(keywordTokenType)) {
                    keywordDrivenBlockDepth = Math.max(0, currentBlockDepth - 1);
                    indentLevelForThisLine = keywordDrivenBlockDepth;
                } else if (intermediateTokens.includes(keywordTokenType)) {
                    indentLevelForThisLine = Math.max(0, currentBlockDepth - 1);
                    keywordDrivenBlockDepth = currentBlockDepth;
                } else if (startTokens.includes(keywordTokenType)) {
                    indentLevelForThisLine = currentBlockDepth;
                    keywordDrivenBlockDepth = currentBlockDepth + 1;
                } else {
                    // Colon-prefixed line, but not a block control keyword (e.g., :DEFINE, :INCLUDE)
                    // Indent based on AST if possible, otherwise maintain current keyword-driven depth.
                    indentLevelForThisLine = enclosingAstBlock
                        ? astBasedIndentLevel
                        : currentBlockDepth;
                    // keywordDrivenBlockDepth remains currentBlockDepth as these keywords don't define new block levels.
                }
            } else {
                // Line starts with a colon but no subsequent keyword token (e.g., just ":") - unlikely but handle defensively.
                indentLevelForThisLine = enclosingAstBlock
                    ? astBasedIndentLevel
                    : currentBlockDepth;
                // keywordDrivenBlockDepth remains currentBlockDepth.
            }
        } else {
            // Not a colon-prefixed line (e.g., assignment, expression, or empty line)
            // For non-keyword lines (including empty ones), AST-based indentation is preferred if available.
            // Otherwise, fall back to the current keyword-driven depth from the previous line.
            indentLevelForThisLine = enclosingAstBlock ? astBasedIndentLevel : currentBlockDepth;
            // keywordDrivenBlockDepth remains currentBlockDepth as this line doesn't change keyword-based block structure.
            // newBlockType remains null.
        }

        return {
            indentLevel: indentLevelForThisLine,
            blockType: newBlockType,
            blockDepth: keywordDrivenBlockDepth,
            enclosingASTBlockType: enclosingAstBlock ? enclosingAstBlock.type : null,
        };
    }

    private getASTNodeDepth(programNode: ASTNode, targetNode: ASTNode): number {
        let depth = 0;
        let found = false;

        const findDepth = (currentNode: ASTNode, currentDepth: number): void => {
            if (found) {
                return;
            }
            if (currentNode === targetNode) {
                depth = currentDepth;
                found = true;
                return;
            }

            let nextDepth = currentDepth;
            // Check if the current node is a block type using `this.isASTBlockType`
            if (currentNode.type === ASTNodeType.program || this.isASTBlockType(currentNode.type)) {
                nextDepth = currentDepth + 1;
            }

            for (const child of currentNode.children) {
                findDepth(child, nextDepth);
                if (found) {
                    return;
                }
            }
        }; // Added semicolon
        findDepth(programNode, -1);
        return Math.max(0, depth);
    }

    private isASTBlockType(type: ASTNodeType): boolean {
        return [
            ASTNodeType.program, // The root is a block
            ASTNodeType.procedure,
            ASTNodeType.ifStatement,
            ASTNodeType.whileStatement,
            ASTNodeType.forStatement,
            ASTNodeType.beginCaseStatement,
            ASTNodeType.caseStatement, // Each CASE within BEGINCASE acts as a block scope
            ASTNodeType.tryStatement,
            ASTNodeType.regionStatement,
            ASTNodeType.classStatement,
            ASTNodeType.blockStatement, // Generic block, e.g., body of an if/while
        ].includes(type);
    }

    private findEnclosingASTBlockNode(
        currentNode: ASTNode,
        targetLineNumber: number
    ): ASTNode | null {
        let candidateFromChildren: ASTNode | null = null;

        for (const child of currentNode.children) {
            const childResult = this.findEnclosingASTBlockNode(child, targetLineNumber);
            if (childResult) {
                // If childResult is a valid block for targetLine, it's preferred over an ancestor.
                // Pick the one that starts latest (closest to targetLine) but not after, if multiple children qualify.
                if (
                    !candidateFromChildren ||
                    (childResult.line !== undefined &&
                        candidateFromChildren.line !== undefined &&
                        childResult.line > candidateFromChildren.line)
                ) {
                    candidateFromChildren = childResult;
                }
            }
        }

        // If a candidate was found in children that starts on or before the target line, it's more specific.
        if (
            candidateFromChildren &&
            candidateFromChildren.line !== undefined &&
            candidateFromChildren.line <= targetLineNumber
        ) {
            return candidateFromChildren;
        }

        // Otherwise, check if the current node itself is a valid enclosing block.
        // It must be a block type and start on or before the target line.
        if (
            this.isASTBlockType(currentNode.type) &&
            currentNode.line !== undefined &&
            currentNode.line <= targetLineNumber
        ) {
            // This node is a candidate. If no deeper candidate was found, this is the one.
            return currentNode;
        }

        return null;
    }

    private getBlockTypeFromToken(tokenType: TokenType): string | null {
        switch (tokenType) {
            case TokenType.if:
                return "IF";
            case TokenType.else:
                return "ELSE";
            case TokenType.while:
                return "WHILE";
            case TokenType.for:
                return "FOR";
            case TokenType.procedure:
                return "PROCEDURE";
            case TokenType.begincase:
                return "BEGINCASE";
            case TokenType.case:
                return "CASE";
            case TokenType.otherwise:
                return "OTHERWISE";
            case TokenType.try:
                return "TRY";
            case TokenType.catch:
                return "CATCH";
            case TokenType.finally:
                return "FINALLY";
            case TokenType.class:
                return "CLASS";
            case TokenType.region:
                return "REGION";
            case TokenType.error:
                return "ERROR";
            case TokenType.begininlinecode:
                return "BEGININLINECODE"; // Added
            case TokenType.endinlinecode:
                return "ENDINLINECODE"; // Added
            case TokenType.endclass:
                return "CLASS"; // Added for :ENDCLASS
            default:
                return null;
        }
    }

    // TODO: Re-evaluate isMultiLineConstruct and getConstructType.
    // These should ideally be derived from AST or more robust token analysis.
    private isMultiLineConstruct(
        line: string, // Keep original line for underscore check
        lineTokens: Token[],
        lineNumber: number, // 0-based index
        lines: string[],
        currentLineConstructType: string | null,
        ast: ASTNode | undefined, // Add AST
        options: FormattingOptions // Add options for tabSize/insertSpaces for potential advanced logic
    ): boolean {
        const trimmedLine = line.trim();

        // 1. Explicit line continuation (most reliable)
        if (trimmedLine.endsWith("_")) {
            return true;
        }
        // Check if the previous line ended with a line continuation character
        if (lineNumber > 0 && lines[lineNumber - 1].trim().endsWith("_")) {
            return true;
        }

        // 2. AST-based check for unclosed constructs (more robust)
        // Note: ASTNode currently lacks endLine, so this check is limited.
        if (ast) {
            // Using more generic types due to parser limitations
            const enclosingNode = this.findEnclosingASTNodeForLine(ast, lineNumber + 1, [
                ASTNodeType.literal,
                ASTNodeType.callExpression,
                ASTNodeType.expression,
            ]);
            if (enclosingNode) {
                // If an AST node starts on or before this line and is of a type that *can* be multi-line,
                // and there's no clear closing token on this line, assume multi-line.
                // This is a heuristic due to lack of endLine in AST.
                if (
                    enclosingNode.type === ASTNodeType.literal &&
                    currentLineConstructType === "array"
                ) {
                    // Check if it's an array
                    if (!lineTokens.some((t) => t.type === TokenType.rbrace)) {
                        return true;
                    }
                }
                if (
                    enclosingNode.type === ASTNodeType.callExpression &&
                    currentLineConstructType === "functionCall"
                ) {
                    if (!lineTokens.some((t) => t.type === TokenType.rparen)) {
                        return true;
                    }
                }
                // SQL statements are harder to delimit with single tokens, rely on underscore or token heuristics.
            }
        }

        // 3. Token-based heuristic (fallback if AST is not precise enough or for constructs not well-represented in AST)
        let openBraces = 0;
        let openParens = 0;
        for (const token of lineTokens) {
            if (token.type === TokenType.lbrace) {
                openBraces++;
            } else if (token.type === TokenType.rbrace) {
                openBraces--;
            } else if (token.type === TokenType.lparen) {
                openParens++;
            } else if (token.type === TokenType.rparen) {
                openParens--;
            }
        }

        if (currentLineConstructType === "array" && openBraces > 0) {
            return true;
        }
        if (currentLineConstructType === "functionCall" && openParens > 0) {
            return true;
        }
        // For SQL, openParens is a weak indicator due to subqueries, etc. AST or underscore is better.
        // if (currentLineConstructType === "sqlStatement" && openParens > 0) return true;

        return false;
    }

    private getConstructType(
        line: string, // Keep original line if needed for future heuristics
        lineTokens: Token[],
        analyzedBlockType: string | null, // Keyword-based block type from analyzeLineContext
        ast: ASTNode | undefined, // Add AST
        lineNumber: number // Add lineNumber (1-based)
    ): string | null {
        // 1. AST-based determination (most reliable with current AST structure)
        if (ast) {
            const enclosingConstructNode = this.findEnclosingASTNodeForLine(ast, lineNumber, [
                // Using ASTNodeType.literal for arrays as they are parsed as literals containing braces.
                ASTNodeType.literal,
                ASTNodeType.callExpression, // For function calls and potentially SQL via SQLEXECUTE
                ASTNodeType.expression, // Generic fallback for other multi-line expressions
            ]);
            if (enclosingConstructNode) {
                switch (enclosingConstructNode.type) {
                    case ASTNodeType.literal:
                        // Further check if this literal looks like an array (contains braces)
                        if (
                            lineTokens.some(
                                (t) => t.type === TokenType.lbrace || t.type === TokenType.rbrace
                            ) ||
                            (enclosingConstructNode.raw &&
                                (enclosingConstructNode.raw.includes("{") ||
                                    enclosingConstructNode.raw.includes("}")))
                        ) {
                            return "array";
                        }
                        break;
                    case ASTNodeType.callExpression:
                        // Check if it's an SQL execute or LSearch
                        if (
                            lineTokens.some(
                                (t) =>
                                    t.type === TokenType.sqlExecute || t.type === TokenType.lSearch
                            )
                        ) {
                            return "sqlStatement";
                        }
                        return "functionCall";
                    // No specific ASTNodeType for SQL statements yet, SQLEXECUTE is often a callExpression.
                    // If parser is enhanced, add specific SQL ASTNodeType here.
                }
            }
        }

        // 2. Token-based heuristics (fallback or for constructs not in AST types above)
        // Check for array literals opening
        if (lineTokens.some((t) => t.type === TokenType.lbrace)) {
            return "array";
        }

        // Check for function calls (identifier followed by lparen or specific keywords)
        for (let i = 0; i < lineTokens.length - 1; i++) {
            if (
                lineTokens[i].type === TokenType.identifier &&
                lineTokens[i + 1].type === TokenType.lparen
            ) {
                return "functionCall";
            }
        }
        const funcCallKeywords = [
            TokenType.doProc,
            TokenType.execFunction,
            TokenType.execUDF,
            TokenType.createUDObject,
            TokenType.sqlExecute, // Can be SQL or a function call wrapper
            TokenType.lSearch,
        ];
        if (lineTokens.length > 0 && funcCallKeywords.includes(lineTokens[0].type)) {
            // If it's sqlExecute or lSearch, prefer sqlStatement if it also looks like one
            if (
                lineTokens.some(
                    (t) => t.type === TokenType.sqlExecute || t.type === TokenType.lSearch
                )
            ) {
                // Further check if it's a typical SQL structure (e.g. string literal follows)
                if (
                    lineTokens.length > 1 &&
                    (lineTokens[1].type === TokenType.stringLiteral ||
                        lineTokens[1].type === TokenType.lparen)
                ) {
                    return "sqlStatement";
                }
            }
            return "functionCall";
        }

        // Check for SQL statements specifically
        if (
            lineTokens.some((t) => t.type === TokenType.sqlExecute || t.type === TokenType.lSearch)
        ) {
            return "sqlStatement";
        }

        return null; // Prefer null if no specific multi-line construct is identified
    }

    // Helper to find an AST node of specific types that encloses a given line number
    private findEnclosingASTNodeForLine(
        currentNode: ASTNode,
        targetLineNumber: number,
        nodeTypes: ASTNodeType[]
    ): ASTNode | null {
        // Check if the current node itself is a candidate
        // Lacks endLine, so can only check if it starts on or before targetLineNumber
        let selfIsCandidate = false;
        if (
            nodeTypes.includes(currentNode.type) &&
            currentNode.line !== undefined &&
            currentNode.line <= targetLineNumber
        ) {
            // We cannot reliably check endLine, so if it starts at/before and is of the right type,
            // it's a candidate. The caller will need to use other heuristics (like token checks)
            // to confirm if the construct is still "open" on the targetLine.
            selfIsCandidate = true;
        }

        // Recursively search in children
        for (const child of currentNode.children) {
            // Only search children if their line range could include the targetLineNumber
            if (child.line !== undefined && child.line <= targetLineNumber) {
                const childResult = this.findEnclosingASTNodeForLine(
                    child,
                    targetLineNumber,
                    nodeTypes
                );
                if (childResult) {
                    // A more specific child match is preferred
                    return childResult;
                }
            }
        }

        return selfIsCandidate ? currentNode : null;
    }
}
