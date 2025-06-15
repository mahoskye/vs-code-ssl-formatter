/**
 * SSL Bracket Matching Provider
 *
 * Provides bracket matching functionality for SSL keyword pairs based on the EBNF grammar.
 * This provider uses the tokenizer to identify SSL keyword pairs and provides their positions
 * to VS Code for highlighting and navigation.
 *
 * Supports matching for:
 * - Procedure blocks (:PROCEDURE/:ENDPROC)
 * - Conditional blocks (:IF/:ENDIF, :ELSE)
 * - Loop blocks (:WHILE/:ENDWHILE, :FOR/:NEXT)
 * - Switch case blocks (:BEGINCASE/:ENDCASE, :CASE, :OTHERWISE)
 * - Try-catch blocks (:TRY/:ENDTRY, :CATCH, :FINALLY)
 * - Error handling blocks (:ERROR)
 * - Class definitions (:CLASS to end, :INHERIT)
 * - Region blocks (:REGION/:ENDREGION)
 * - Inline code blocks (:BEGININLINECODE/:ENDINLINECODE)
 * - Array literals ({...})
 * - Code block literals ({|...|...})
 * - Standard brackets (), [], {}
 */

import * as vscode from "vscode";
import { Tokenizer } from "../tokenizer";
import { Token } from "../tokenizer/token";
import { TokenType } from "../tokenizer/tokenType";

/**
 * Represents a bracket pair with positions
 */
interface BracketPair {
    /** The opening bracket/keyword position */
    opening: vscode.Position;
    /** The closing bracket/keyword position */
    closing: vscode.Position;
    /** The type of bracket pair */
    type: BracketPairType;
}

/**
 * Types of bracket pairs supported
 */
enum BracketPairType {
    // Standard brackets
    Parentheses = "parentheses",
    SquareBrackets = "square-brackets",
    CurlyBraces = "curly-braces",

    // SSL keyword pairs - Control Flow
    Procedure = "procedure",
    If = "if",
    While = "while",
    For = "for",
    Switch = "switch",
    Try = "try",
    Class = "class",
    Region = "region",
    InlineCode = "inline-code",
    Error = "error",

    // SSL intermediate keywords (can appear within blocks)
    Else = "else",
    Case = "case",
    Otherwise = "otherwise",
    Catch = "catch",
    Finally = "finally",

    // Code blocks
    ArrayLiteral = "array-literal",
    CodeBlock = "code-block",
}

/**
 * SSL-specific bracket matching provider
 */
export class SSLBracketMatchingProvider {
    private readonly tokenizer: Tokenizer;

    constructor() {
        this.tokenizer = new Tokenizer();
    }

    /**
     * Find the matching bracket for the given position
     */
    public findMatchingBracket(
        document: vscode.TextDocument,
        position: vscode.Position
    ): vscode.Position | null {
        try {
            // Tokenize the document
            const tokenizationResult = this.tokenizer.tokenize(document.getText());
            if (tokenizationResult.hasErrors || tokenizationResult.tokens.length === 0) {
                return null;
            }

            // Find all bracket pairs
            const bracketPairs = this.findAllBracketPairs(tokenizationResult.tokens);

            // Find the bracket pair that contains the given position
            const matchingPair = this.findBracketPairAtPosition(bracketPairs, position);
            if (matchingPair) {
                // Return the opposite bracket position
                if (this.isPositionWithinRange(position, matchingPair.opening)) {
                    return matchingPair.closing;
                } else if (this.isPositionWithinRange(position, matchingPair.closing)) {
                    return matchingPair.opening;
                }
            }

            return null;
        } catch (error) {
            console.error("Error finding matching bracket:", error);
            return null;
        }
    }
    /**
     * Get all bracket pairs in the document
     */ public getAllBracketPairs(document: vscode.TextDocument): BracketPair[] {
        try {
            const tokenizationResult = this.tokenizer.tokenize(document.getText());

            if (tokenizationResult.hasErrors || tokenizationResult.tokens.length === 0) {
                return [];
            }

            return this.findAllBracketPairs(tokenizationResult.tokens);
        } catch (error) {
            console.error("Error getting bracket pairs:", error);
            return [];
        }
    }
    /**
     * Find all bracket pairs in the token stream
     */
    private findAllBracketPairs(tokens: Token[]): BracketPair[] {
        const pairs: BracketPair[] = [];
        const stack: Array<{ token: Token; type: BracketPairType }> = [];

        for (let i = 0; i < tokens.length; i++) {
            const token = tokens[i];

            // Handle standard brackets first
            if (this.handleStandardBrackets(token, stack, pairs)) {
                continue;
            }

            // Handle SSL keyword pairs
            if (token.type === TokenType.COLON) {
                const nextToken = i + 1 < tokens.length ? tokens[i + 1] : null;
                if (nextToken) {
                    this.handleSSLKeywords(token, nextToken, stack, pairs);
                }
            }

            // Handle code block literals
            if (token.type === TokenType.CODE_BLOCK_START) {
                stack.push({ token, type: BracketPairType.CodeBlock });
            } else if (token.type === TokenType.CODE_BLOCK_END) {
                this.matchAndPop(stack, pairs, BracketPairType.CodeBlock, token);
            }

            // Handle comment regions - both EBNF specified comment types
            this.handleCommentRegions(token, stack, pairs);
        }

        // Special handling for class definitions that don't have explicit end keywords
        this.handleClassEndHandling(tokens, stack, pairs);

        return pairs;
    }

    /**
     * Handle standard bracket pairs (), [], {}
     */
    private handleStandardBrackets(
        token: Token,
        stack: Array<{ token: Token; type: BracketPairType }>,
        pairs: BracketPair[]
    ): boolean {
        switch (token.type) {
            case TokenType.LPAREN:
                stack.push({ token, type: BracketPairType.Parentheses });
                return true;
            case TokenType.RPAREN:
                this.matchAndPop(stack, pairs, BracketPairType.Parentheses, token);
                return true;
            case TokenType.LBRACKET:
                stack.push({ token, type: BracketPairType.SquareBrackets });
                return true;
            case TokenType.RBRACKET:
                this.matchAndPop(stack, pairs, BracketPairType.SquareBrackets, token);
                return true;
            case TokenType.LBRACE:
                stack.push({ token, type: BracketPairType.CurlyBraces });
                return true;
            case TokenType.RBRACE:
                // Try to match with either curly braces or array literal
                this.matchAndPopMultiple(
                    stack,
                    pairs,
                    [BracketPairType.CurlyBraces, BracketPairType.ArrayLiteral],
                    token
                );
                return true;
            default:
                return false;
        }
    }
    /**
     * Handle SSL keyword pairs
     */
    private handleSSLKeywords(
        colonToken: Token,
        keywordToken: Token,
        stack: Array<{ token: Token; type: BracketPairType }>,
        pairs: BracketPair[]
    ): void {
        const keyword = keywordToken.value.toUpperCase();

        // Handle opening keywords
        switch (keyword) {
            // Procedure definitions
            case "PROCEDURE":
                stack.push({ token: colonToken, type: BracketPairType.Procedure });
                break;

            // Conditional statements
            case "IF":
                stack.push({ token: colonToken, type: BracketPairType.If });
                break;

            // Loop statements
            case "WHILE":
                stack.push({ token: colonToken, type: BracketPairType.While });
                break;
            case "FOR":
                stack.push({ token: colonToken, type: BracketPairType.For });
                break;

            // Switch case statements
            case "BEGINCASE":
                stack.push({ token: colonToken, type: BracketPairType.Switch });
                break;

            // Error handling statements
            case "TRY":
                stack.push({ token: colonToken, type: BracketPairType.Try });
                break;
            case "ERROR":
                stack.push({ token: colonToken, type: BracketPairType.Error });
                break;

            // Class definitions
            case "CLASS":
                stack.push({ token: colonToken, type: BracketPairType.Class });
                break;

            // Region blocks
            case "REGION":
                stack.push({ token: colonToken, type: BracketPairType.Region });
                break;

            // Inline code blocks
            case "BEGININLINECODE":
                stack.push({ token: colonToken, type: BracketPairType.InlineCode });
                break;
        } // Handle closing keywords
        switch (
            keyword // Procedure end
        ) {
            case "ENDPROC":
                this.matchAndPop(stack, pairs, BracketPairType.Procedure, colonToken);
                break;

            // Conditional end
            case "ENDIF":
                this.matchAndPop(stack, pairs, BracketPairType.If, colonToken);
                break;

            // Loop ends
            case "ENDWHILE":
                this.matchAndPop(stack, pairs, BracketPairType.While, colonToken);
                break;
            case "NEXT":
                this.matchAndPop(stack, pairs, BracketPairType.For, colonToken);
                break;

            // Switch case end
            case "ENDCASE":
                this.matchAndPop(stack, pairs, BracketPairType.Switch, colonToken);
                break;

            // Error handling ends
            case "ENDTRY":
                this.matchAndPop(stack, pairs, BracketPairType.Try, colonToken);
                break;

            // Region end
            case "ENDREGION":
                this.matchAndPop(stack, pairs, BracketPairType.Region, colonToken);
                break;

            // Inline code end
            case "ENDINLINECODE":
                this.matchAndPop(stack, pairs, BracketPairType.InlineCode, colonToken);
                break;
        }

        // Handle intermediate keywords that don't create new pairs but may affect matching
        switch (keyword) {
            case "ELSE":
                // Create a pair for the IF..ELSE portion if there's an IF on the stack
                this.createIntermediatePair(
                    stack,
                    pairs,
                    BracketPairType.If,
                    colonToken,
                    BracketPairType.Else
                );
                break;
            case "CASE":
            case "OTHERWISE":
                // These are intermediate keywords within switch statements
                // They don't close the BEGINCASE but create sub-pairs
                break;
            case "CATCH":
            case "FINALLY":
                // These are intermediate keywords within try statements
                // They don't close the TRY but create sub-pairs
                break;
        }
    }
    /**
     * Match and pop from stack to create bracket pairs
     */
    private matchAndPop(
        stack: Array<{ token: Token; type: BracketPairType }>,
        pairs: BracketPair[],
        expectedType: BracketPairType,
        endToken: Token
    ): void {
        // Find the most recent matching opening token
        for (let i = stack.length - 1; i >= 0; i--) {
            if (stack[i].type === expectedType) {
                const startItem = stack.splice(i, 1)[0];
                pairs.push({
                    opening: this.tokenToPosition(startItem.token),
                    closing: this.tokenToPosition(endToken),
                    type: expectedType,
                });
                break;
            }
        }
    }

    /**
     * Match and pop from stack with multiple possible types
     */ private matchAndPopMultiple(
        stack: Array<{ token: Token; type: BracketPairType }>,
        pairs: BracketPair[],
        expectedTypes: BracketPairType[],
        endToken: Token
    ): void {
        // Find the most recent matching opening token from any of the expected types
        for (let i = stack.length - 1; i >= 0; i--) {
            if (expectedTypes.includes(stack[i].type)) {
                const startItem = stack.splice(i, 1)[0];
                pairs.push({
                    opening: this.tokenToPosition(startItem.token),
                    closing: this.tokenToPosition(endToken),
                    type: startItem.type,
                });
                break;
            }
        }
    }
    /**
     * Convert token position to VS Code Position
     */
    private tokenToPosition(token: Token): vscode.Position {
        if (token.range && token.range.start) {
            return new vscode.Position(token.range.start.line, token.range.start.column);
        }

        // Fallback: return 0,0 - this should not happen in production
        return new vscode.Position(0, 0);
    }
    /**
     * Find the bracket pair at the given position
     */
    private findBracketPairAtPosition(
        bracketPairs: BracketPair[],
        position: vscode.Position
    ): BracketPair | null {
        for (const pair of bracketPairs) {
            // Check if position is within the opening or closing bracket range
            if (
                this.isPositionWithinRange(position, pair.opening) ||
                this.isPositionWithinRange(position, pair.closing)
            ) {
                return pair;
            }
        }

        return null;
    }

    /**
     * Check if position is within a bracket range (allowing for multi-character keywords)
     */ private isPositionWithinRange(
        position: vscode.Position,
        bracketPosition: vscode.Position
    ): boolean {
        // For SSL keywords, we need to check if the cursor is on the keyword, not just exact position
        // This allows matching when cursor is anywhere on ":PROCEDURE", ":ENDPROC", etc.
        return (
            position.line === bracketPosition.line &&
            Math.abs(position.character - bracketPosition.character) <= 15 // Allow up to 15 char keyword length
        );
    }

    /**
     * Check if two positions are equal
     */
    private isPositionEqual(pos1: vscode.Position, pos2: vscode.Position): boolean {
        return pos1.line === pos2.line && pos1.character === pos2.character;
    }
    /**
     * Get bracket pair information for diagnostics or debugging
     */
    public getBracketPairInfo(document: vscode.TextDocument): string {
        const pairs = this.getAllBracketPairs(document);
        if (pairs.length === 0) {
            return "No bracket pairs found";
        }

        const groupedPairs = this.groupPairsByType(pairs);
        const info: string[] = [];

        for (const [type, pairList] of groupedPairs.entries()) {
            info.push(`${type.toUpperCase()} blocks (${pairList.length}):`);
            for (const pair of pairList) {
                info.push(
                    `  Line ${pair.opening.line + 1}:${pair.opening.character + 1} -> Line ${
                        pair.closing.line + 1
                    }:${pair.closing.character + 1}`
                );
            }
        }

        return info.join("\n");
    }

    /**
     * Group bracket pairs by their type for better organization
     */
    private groupPairsByType(pairs: BracketPair[]): Map<string, BracketPair[]> {
        const grouped = new Map<string, BracketPair[]>();

        for (const pair of pairs) {
            if (!grouped.has(pair.type)) {
                grouped.set(pair.type, []);
            }
            grouped.get(pair.type)!.push(pair);
        }

        return grouped;
    }

    /**
     * Validate bracket pairs and find unmatched brackets
     */
    public validateBracketPairs(document: vscode.TextDocument): vscode.Diagnostic[] {
        const diagnostics: vscode.Diagnostic[] = [];

        try {
            const tokenizationResult = this.tokenizer.tokenize(document.getText());
            if (tokenizationResult.hasErrors) {
                return diagnostics;
            }

            const stack: Array<{ token: Token; type: BracketPairType }> = [];
            const tokens = tokenizationResult.tokens;

            // Track unmatched opening brackets
            for (let i = 0; i < tokens.length; i++) {
                const token = tokens[i];

                // Check standard brackets
                if (this.isOpeningBracket(token)) {
                    stack.push({ token, type: this.getBracketType(token) });
                } else if (this.isClosingBracket(token)) {
                    const expectedType = this.getBracketType(token);
                    const matched = this.findAndRemoveMatchingBracket(stack, expectedType);
                    if (!matched) {
                        // Unmatched closing bracket
                        const range = new vscode.Range(
                            token.range.start.line,
                            token.range.start.column,
                            token.range.end.line,
                            token.range.end.column
                        );
                        diagnostics.push(
                            new vscode.Diagnostic(
                                range,
                                `Unmatched closing bracket: ${token.value}`,
                                vscode.DiagnosticSeverity.Error
                            )
                        );
                    }
                }

                // Check SSL keyword pairs
                if (token.type === TokenType.COLON) {
                    const nextToken = i + 1 < tokens.length ? tokens[i + 1] : null;
                    if (nextToken) {
                        this.validateSSLKeywordPair(token, nextToken, stack, diagnostics);
                    }
                }
            } // Report unmatched opening brackets
            for (const item of stack) {
                // Skip items without proper range information
                if (!item.token.range || !item.token.range.start || !item.token.range.end) {
                    continue;
                }

                const range = new vscode.Range(
                    item.token.range.start.line,
                    item.token.range.start.column,
                    item.token.range.end.line,
                    item.token.range.end.column
                );
                diagnostics.push(
                    new vscode.Diagnostic(
                        range,
                        `Unmatched opening bracket: ${item.token.value}`,
                        vscode.DiagnosticSeverity.Error
                    )
                );
            }
        } catch (error) {
            console.error("Error validating bracket pairs:", error);
        }

        return diagnostics;
    }

    /**
     * Check if token is an opening bracket
     */
    private isOpeningBracket(token: Token): boolean {
        return [TokenType.LPAREN, TokenType.LBRACKET, TokenType.LBRACE].includes(token.type);
    }

    /**
     * Check if token is a closing bracket
     */
    private isClosingBracket(token: Token): boolean {
        return [TokenType.RPAREN, TokenType.RBRACKET, TokenType.RBRACE].includes(token.type);
    }

    /**
     * Get bracket type for a token
     */
    private getBracketType(token: Token): BracketPairType {
        switch (token.type) {
            case TokenType.LPAREN:
            case TokenType.RPAREN:
                return BracketPairType.Parentheses;
            case TokenType.LBRACKET:
            case TokenType.RBRACKET:
                return BracketPairType.SquareBrackets;
            case TokenType.LBRACE:
            case TokenType.RBRACE:
                return BracketPairType.CurlyBraces;
            default:
                return BracketPairType.CurlyBraces; // Default fallback
        }
    }

    /**
     * Find and remove matching bracket from stack
     */
    private findAndRemoveMatchingBracket(
        stack: Array<{ token: Token; type: BracketPairType }>,
        expectedType: BracketPairType
    ): boolean {
        for (let i = stack.length - 1; i >= 0; i--) {
            if (stack[i].type === expectedType) {
                stack.splice(i, 1);
                return true;
            }
        }
        return false;
    }
    /**
     * Validate SSL keyword pairs
     */
    private validateSSLKeywordPair(
        colonToken: Token,
        keywordToken: Token,
        stack: Array<{ token: Token; type: BracketPairType }>,
        diagnostics: vscode.Diagnostic[]
    ): void {
        const keyword = keywordToken.value.toUpperCase();

        // Handle opening keywords - add to stack
        const openingKeywords = new Map([
            ["PROCEDURE", BracketPairType.Procedure],
            ["IF", BracketPairType.If],
            ["WHILE", BracketPairType.While],
            ["FOR", BracketPairType.For],
            ["BEGINCASE", BracketPairType.Switch],
            ["TRY", BracketPairType.Try],
            ["CLASS", BracketPairType.Class],
            ["REGION", BracketPairType.Region],
            ["BEGININLINECODE", BracketPairType.InlineCode],
            ["ERROR", BracketPairType.Error],
        ]);

        // Handle closing keywords - find matching opening
        const closingKeywords = new Map([
            ["ENDPROC", BracketPairType.Procedure],
            ["ENDIF", BracketPairType.If],
            ["ENDWHILE", BracketPairType.While],
            ["NEXT", BracketPairType.For],
            ["ENDCASE", BracketPairType.Switch],
            ["ENDTRY", BracketPairType.Try],
            ["ENDREGION", BracketPairType.Region],
            ["ENDINLINECODE", BracketPairType.InlineCode],
        ]);

        // Handle intermediate keywords that should be within specific blocks
        const intermediateKeywords = new Map([
            ["ELSE", [BracketPairType.If]],
            ["CASE", [BracketPairType.Switch]],
            ["OTHERWISE", [BracketPairType.Switch]],
            ["CATCH", [BracketPairType.Try]],
            ["FINALLY", [BracketPairType.Try]],
            ["EXITWHILE", [BracketPairType.While]],
            ["EXITFOR", [BracketPairType.For]],
            ["EXITCASE", [BracketPairType.Switch]],
            ["LOOP", [BracketPairType.While, BracketPairType.For]], // Can be in while or for loops
        ]);

        if (openingKeywords.has(keyword)) {
            stack.push({ token: colonToken, type: openingKeywords.get(keyword)! });
        } else if (closingKeywords.has(keyword)) {
            const expectedType = closingKeywords.get(keyword)!;
            const matched = this.findAndRemoveMatchingBracket(stack, expectedType);
            if (!matched) {
                // Unmatched closing keyword
                const range = new vscode.Range(
                    colonToken.range.start.line,
                    colonToken.range.start.column,
                    keywordToken.range.end.line,
                    keywordToken.range.end.column
                );
                diagnostics.push(
                    new vscode.Diagnostic(
                        range,
                        `Unmatched closing keyword: :${keyword}`,
                        vscode.DiagnosticSeverity.Error
                    )
                );
            }
        } else if (intermediateKeywords.has(keyword)) {
            // Validate that intermediate keywords are within appropriate blocks
            const expectedTypes = intermediateKeywords.get(keyword)!;
            const hasMatchingParent = stack.some((item) => expectedTypes.includes(item.type));

            if (!hasMatchingParent) {
                const range = new vscode.Range(
                    colonToken.range.start.line,
                    colonToken.range.start.column,
                    keywordToken.range.end.line,
                    keywordToken.range.end.column
                );
                const expectedTypeNames = expectedTypes.map((t) => t.toUpperCase()).join(" or ");
                diagnostics.push(
                    new vscode.Diagnostic(
                        range,
                        `Keyword :${keyword} must be within a ${expectedTypeNames} block`,
                        vscode.DiagnosticSeverity.Error
                    )
                );
            }
        }
    }

    /**
     * Create intermediate pairs for keywords that appear within blocks (like :ELSE within :IF blocks)
     */
    private createIntermediatePair(
        stack: Array<{ token: Token; type: BracketPairType }>,
        pairs: BracketPair[],
        expectedParentType: BracketPairType,
        intermediateToken: Token,
        intermediateType: BracketPairType
    ): void {
        // Find the most recent matching parent token without removing it from stack
        for (let i = stack.length - 1; i >= 0; i--) {
            if (stack[i].type === expectedParentType) {
                // Create a pair from the parent to this intermediate keyword
                pairs.push({
                    opening: this.tokenToPosition(stack[i].token),
                    closing: this.tokenToPosition(intermediateToken),
                    type: intermediateType,
                });
                break;
            }
        }
    }

    /**
     * Handle comment region pairs as specified in the EBNF grammar
     */
    private handleCommentRegions(
        token: Token,
        stack: Array<{ token: Token; type: BracketPairType }>,
        pairs: BracketPair[]
    ): void {
        // Handle comment regions - both keyword-based and comment-based regions
        if (token.type === TokenType.REGION_COMMENT) {
            stack.push({ token, type: BracketPairType.Region });
        } else if (token.type === TokenType.ENDREGION_COMMENT) {
            this.matchAndPop(stack, pairs, BracketPairType.Region, token);
        }

        // Handle general comments that contain region markers
        if (
            token.type === TokenType.BLOCK_COMMENT ||
            token.type === TokenType.SINGLE_LINE_COMMENT
        ) {
            const commentText = token.value.toLowerCase();
            if (commentText.includes("region ")) {
                stack.push({ token, type: BracketPairType.Region });
            } else if (commentText.includes("endregion")) {
                this.matchAndPop(stack, pairs, BracketPairType.Region, token);
            }
        }
    }

    /**
     * Handle special case for class definitions which don't have explicit end keywords
     * According to the EBNF, classes end with the script, not with :ENDCLASS
     */
    private handleClassEndHandling(
        tokens: Token[],
        stack: Array<{ token: Token; type: BracketPairType }>,
        pairs: BracketPair[]
    ): void {
        // Find any remaining CLASS tokens in the stack and close them at the end of the document
        const classItems = stack.filter((item) => item.type === BracketPairType.Class);

        if (classItems.length > 0 && tokens.length > 0) {
            const lastToken = tokens[tokens.length - 1];

            // Close all open class definitions at the end of the document
            for (const classItem of classItems) {
                pairs.push({
                    opening: this.tokenToPosition(classItem.token),
                    closing: this.tokenToPosition(lastToken),
                    type: BracketPairType.Class,
                });
            }

            // Remove class items from stack to avoid duplicate error reporting
            for (let i = stack.length - 1; i >= 0; i--) {
                if (stack[i].type === BracketPairType.Class) {
                    stack.splice(i, 1);
                }
            }
        }
    }

    /**
     * Get bracket information at a specific cursor position
     */
    public getBracketInfoAtPosition(
        document: vscode.TextDocument,
        position: vscode.Position
    ): string {
        try {
            const tokenizationResult = this.tokenizer.tokenize(document.getText());
            if (tokenizationResult.hasErrors || tokenizationResult.tokens.length === 0) {
                return "No bracket information available";
            }

            const bracketPairs = this.findAllBracketPairs(tokenizationResult.tokens);
            const matchingPair = this.findBracketPairAtPosition(bracketPairs, position);

            if (matchingPair) {
                const partnerPosition = this.isPositionWithinRange(position, matchingPair.opening)
                    ? matchingPair.closing
                    : matchingPair.opening;

                return `SSL ${matchingPair.type.toUpperCase()} block: Line ${
                    partnerPosition.line + 1
                }:${partnerPosition.character + 1}`;
            }

            return "No matching bracket found at cursor position";
        } catch (error) {
            console.error("Error getting bracket info at position:", error);
            return "Error retrieving bracket information";
        }
    }

    /**
     * Get all SSL keyword pairs for navigation
     */
    public getSSLKeywordPairs(
        document: vscode.TextDocument
    ): Array<{ type: string; opening: vscode.Position; closing: vscode.Position }> {
        const pairs = this.getAllBracketPairs(document);
        return pairs
            .filter(
                (pair) =>
                    ![
                        "parentheses",
                        "square-brackets",
                        "curly-braces",
                        "array-literal",
                        "code-block",
                    ].includes(pair.type)
            )
            .map((pair) => ({
                type: pair.type,
                opening: pair.opening,
                closing: pair.closing,
            }));
    }
}
