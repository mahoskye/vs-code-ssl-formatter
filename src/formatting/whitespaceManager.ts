import { Node, NodeType } from '../parsing/parser';
import { Token, TokenType } from '../parsing/lexer';
import {
    BLOCK_START_KEYWORDS,
    BLOCK_END_KEYWORDS,
    PROCEDURE_LEVEL_KEYWORDS,
    BLOCK_MIDDLE_KEYWORDS,
    CASE_KEYWORDS,
    MAJOR_BLOCK_START_KEYWORDS,
    MAJOR_BLOCK_END_KEYWORDS
} from '../constants/language';

export class WhitespaceManager {
    /**
     * Determine usage of blank lines between statements
     */
    public getVerticalWhitespace(prev: Node, curr: Node): number {
        const currToken = this.getFirstSignificantToken(curr);
        const prevToken = this.getFirstSignificantToken(prev);
        const currText = currToken ? currToken.text.toUpperCase() : "";
        const prevText = prevToken ? prevToken.text.toUpperCase() : "";

        // Inline Comments - append to same line
        if (curr.type === NodeType.Comment && prev.endLine === curr.startLine) {
            return -1;
        }

        // Definition Blocks (PARAMETERS, DEFAULT, DECLARE)
        if (this.isDefinition(currToken) && this.isDefinition(prevToken)) {
            if (currText === prevText) { return 0; }
            return 1;
        }

        // Comments - give them room
        const currIsComment = curr.tokens.some(t => t.type === TokenType.Comment);
        const prevIsComment = prev.tokens.some(t => t.type === TokenType.Comment);

        if (this.isRegionComment(curr) && this.isEndRegionComment(prev)) {
            return 1;
        }

        if (currIsComment && !prevIsComment && !this.isDefinition(prevToken)) {
            return 1;
        }

        // Control Structures (IF, WHILE, FOR, etc.)
        if (this.isControlStart(currToken) && !this.isControlStart(prevToken)) {
            return 1;
        }

        // Force blank line before :CASE, :OTHERWISE
        if (this.isCaseKeyword(currToken)) {
            return 1;
        }

        // Add blank line after control block end
        if (this.isControlEnd(prevToken)) {
            // Don't add blank if: next node has no token, or followed by block middle/case/control end/major block end
            if (currToken && !this.isBlockMiddle(currToken) && !this.isCaseKeyword(currToken) && !this.isControlEnd(currToken) && !this.isMajorBlockEnd(currToken)) {
                return 1;
            }
        }

        // Major Blocks (Procedure, Class) - Always separate
        if (this.isMajorBlockStart(currToken)) {
            if (prev.type === NodeType.Comment) { return 0; }
            return 1;
        }
        if (this.isMajorBlockEnd(prevToken)) { return 1; }

        return 0;
    }

    // --- Private Helpers ---

    private getFirstSignificantToken(node: Node): Token | undefined {
        if (!node || node.tokens.length === 0) { return undefined; }
        for (const t of node.tokens) {
            if (t.type !== TokenType.Whitespace) { return t; }
        }
        return undefined;
    }

    /** Normalize keyword text by uppercasing and removing legacy colon prefix */
    private normalizeKeyword(token: Token | undefined): string {
        if (!token) { return ''; }
        return token.text.toUpperCase().replace(/^:/, '');
    }

    private isDefinition(token: Token | undefined): boolean {
        if (!token || token.type !== TokenType.Keyword) { return false; }
        return (PROCEDURE_LEVEL_KEYWORDS as readonly string[]).includes(this.normalizeKeyword(token));
    }

    private isControlStart(token: Token | undefined): boolean {
        if (!token || token.type !== TokenType.Keyword) { return false; }
        const text = this.normalizeKeyword(token);
        return (BLOCK_START_KEYWORDS as readonly string[]).includes(text) &&
            !(MAJOR_BLOCK_START_KEYWORDS as readonly string[]).includes(text);
    }

    private isControlEnd(token: Token | undefined): boolean {
        if (!token || token.type !== TokenType.Keyword) { return false; }
        const text = this.normalizeKeyword(token);
        return (BLOCK_END_KEYWORDS as readonly string[]).includes(text) &&
            !(MAJOR_BLOCK_END_KEYWORDS as readonly string[]).includes(text);
    }

    private isBlockMiddle(token: Token | undefined): boolean {
        if (!token || token.type !== TokenType.Keyword) { return false; }
        return (BLOCK_MIDDLE_KEYWORDS as readonly string[]).includes(this.normalizeKeyword(token));
    }

    private isCaseKeyword(token: Token | undefined): boolean {
        if (!token) { return false; }
        return (CASE_KEYWORDS as readonly string[]).includes(this.normalizeKeyword(token));
    }

    private isMajorBlockStart(token: Token | undefined): boolean {
        if (!token || token.type !== TokenType.Keyword) { return false; }
        return (MAJOR_BLOCK_START_KEYWORDS as readonly string[]).includes(this.normalizeKeyword(token));
    }

    private isMajorBlockEnd(token: Token | undefined): boolean {
        if (!token || token.type !== TokenType.Keyword) { return false; }
        return (MAJOR_BLOCK_END_KEYWORDS as readonly string[]).includes(this.normalizeKeyword(token));
    }

    private isRegionComment(node: Node): boolean {
        return node.type === NodeType.RegionStart;
    }

    private isEndRegionComment(node: Node): boolean {
        return node.type === NodeType.RegionEnd;
    }
}

