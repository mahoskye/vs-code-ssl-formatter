import { FormattingRule, FormattingContext, SSLFormattingProvider } from "../formattingProvider";
import { Token, TokenType } from "../../core/tokenizer";
import { ASTNode, ASTNodeType } from "../../core/parser";
import { getIndentationString } from "../../utils/indentationUtils";

/**
 * Handles consistent indentation for SSL blocks
 */
export class IndentationRule implements FormattingRule {
    name = "IndentationRule";
    description = "Ensures consistent indentation for SSL blocks";

    private provider: SSLFormattingProvider;

    constructor(provider: SSLFormattingProvider) {
        this.provider = provider;
    }

    apply(line: string, context: FormattingContext): string {
        const trimmedLine = line.trim();
        if (trimmedLine === "") {
            // Let the provider handle the final state of empty lines (e.g., truly empty or just whitespace for indent level).
            // For now, returning "" means it might be stripped if other rules don't add whitespace.
            // Or, provider could ensure it's at least `getIndentationString(context.currentLineBaseIndentLevel, ...)`
            return "";
        }

        // Labels should have zero indent, overriding other calculations.
        if (context.lineTokens.length > 0 && context.lineTokens[0].type === TokenType.colon) {
            return trimmedLine; // No leading space for labels
        }

        let indentLevelForThisLine = context.currentLineBaseIndentLevel;

        // --- AST-based indentation logic ---
        if (context.ast && context.ast.children.length > 0) {
            const enclosingAstBlock = this.provider.findEnclosingASTBlockNode(
                context.ast,
                context.lineNumber
            );
            if (enclosingAstBlock) {
                const astBlockDepth = this.provider.getASTNodeDepth(context.ast, enclosingAstBlock);

                if (line.includes("NIL .AND.")) {
                    console.log(
                        `[IndentationRule] NIL Line ${context.lineNumber}: enclosingAstBlock type=${enclosingAstBlock.type}, line=${enclosingAstBlock.line}, astBlockDepth=${astBlockDepth}, currentLineBaseIndentLevel=${context.currentLineBaseIndentLevel}, enclosingASTBlockTypeFromContext=${context.enclosingASTBlockType}`
                    );
                }

                if (enclosingAstBlock.type === ASTNodeType.program) {
                    // Directly under program node.
                    // Top-level statements and top-level blocks (PROCEDURE, CLASS, IF at root) should generally be indent 0.
                    // currentLineBaseIndentLevel should be 0 if we are at the global scope.
                    indentLevelForThisLine = 0;
                } else {
                    // Enclosed in a non-program AST block
                    if (
                        enclosingAstBlock.line === context.lineNumber &&
                        this.provider.isASTBlockType(enclosingAstBlock.type)
                    ) {
                        // An AST block (e.g. IF, WHILE body) starts on this line. Its indent is currentLineBaseIndentLevel.
                        indentLevelForThisLine = context.currentLineBaseIndentLevel;
                    } else {
                        // Line is inside an existing AST block. Its indent should be astBlockDepth.
                        indentLevelForThisLine = astBlockDepth;
                    }
                }
            }
        }
        // --- End AST-based indentation logic ---

        // --- Keyword-based adjustments for the current line ---
        // Adjust for block-ending or intermediate keywords on the *current* line (e.g. ELSE, ENDIF should be outdented)
        const firstSignificantToken = context.lineTokens.find(
            (t) =>
                t.type !== TokenType.whitespace &&
                t.type !== TokenType.singleLineComment &&
                t.type !== TokenType.blockComment &&
                t.type !== TokenType.regionComment &&
                t.type !== TokenType.endregionComment
        );
        if (firstSignificantToken) {
            if (
                this.isBlockEndToken(firstSignificantToken, context.lineTokens) ||
                this.isMiddleBlockToken(firstSignificantToken, context.lineTokens)
            ) {
                indentLevelForThisLine = Math.max(0, context.currentLineBaseIndentLevel - 1);
            }
            // Note: isBlockStartToken is primarily for the *next* line's indent, handled by provider's analyzeLineContext.
            // However, if AST logic didn't place it correctly (e.g. a :IF at top level), this could be a fallback.
            // But the AST logic for program children (setting to 0) should handle top-level blocks.
        }
        // --- End Keyword-based adjustments ---

        if (line.includes("NIL .AND.")) {
            console.log(
                `[IndentationRule] NIL Line ${context.lineNumber}: Final indentLevelForThisLine = ${indentLevelForThisLine}, from base ${context.currentLineBaseIndentLevel}`
            );
        }

        // Ensure indentLevel is not negative
        indentLevelForThisLine = Math.max(0, indentLevelForThisLine);

        const indentationString = getIndentationString(
            indentLevelForThisLine,
            context.options.indentStyle === "tab" ? 1 : context.options.tabSize,
            context.options.indentStyle === "tab"
        );
        return indentationString + trimmedLine;
    }

    // Helper methods for token-based keyword checks
    public isBlockStartToken(token: Token, allLineTokens: Token[]): boolean {
        const keyword = token.value.toUpperCase();
        // Keywords that open a new indentation level for subsequent lines
        const startKeywords = [
            ":IF",
            ":WHILE",
            ":FOR",
            ":PROCEDURE",
            ":CLASS",
            ":BEGINCASE",
            ":TRY",
            ":REGION",
            ":BEGININLINECODE",
            // Note: :SWITCH is handled by :BEGINCASE in some SSL versions
        ];
        if (startKeywords.includes(keyword)) {
            // Special case: "IF ... THEN" on a single line. "THEN" doesn't start a new block itself for the *next* line's indent.
            // This is more about next line's depth, which provider handles.
            // For this rule, we are mostly concerned with current line's placement.
            return true;
        }
        return false;
    }

    public isBlockEndToken(token: Token, allLineTokens: Token[]): boolean {
        const keyword = token.value.toUpperCase();
        const endKeywords = [
            ":ENDIF",
            ":ENDWHILE",
            ":NEXT",
            /* for :FOR */ ":ENDPROC",
            ":ENDCLASS",
            ":ENDCASE",
            ":ENDTRY",
            ":ENDREGION",
            ":ENDINLINECODE",
        ];
        return endKeywords.includes(keyword);
    }

    public isMiddleBlockToken(token: Token, allLineTokens: Token[]): boolean {
        const keyword = token.value.toUpperCase();
        const middleKeywords = [
            ":ELSE",
            ":CASE",
            ":OTHERWISE", // for :BEGINCASE
            ":CATCH",
            ":FINALLY", // for :TRY
        ];
        return middleKeywords.includes(keyword);
    }

    // Original helper methods (can be removed or adapted if fully superseded)
    // private shouldNotIndent(line: string): boolean { ... }
    // private calculateIndentLevel(line: string, context: FormattingContext): number { ... }
    // private isBlockEnd(line: string): boolean { ... } // Superseded by token version
    // private isMiddleBlock(line: string): boolean { ... } // Superseded by token version
}
