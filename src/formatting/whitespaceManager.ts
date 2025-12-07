import { Node, NodeType } from './parser';
import { Token, TokenType } from './lexer';
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

        // 0. Inline Comments
        // If current is a comment and starts on same line as previous ends -> Inline
        // Must return -1 to signal appending
        if (curr.type === NodeType.Comment && prev.endLine === curr.startLine) {
            return -1;
        }

        let res = 0;
        // console.log(`WS check: ${prev.type}(${prevText}) -> ${curr.type}(${currText})`);

        // 1. Definition Blocks (PARAMETERS, DEFAULT, DECLARE)
        if (this.isDefinition(currToken) && this.isDefinition(prevToken)) {
            // Group only if same type (e.g. DEFAULT followed by DEFAULT)
            if (currText === prevText) {
                return 0;
            }
            return 1;
        }

        // 2. Comments - give them room
        const currIsComment = curr.tokens.some(t => t.type === TokenType.Comment);
        const prevIsComment = prev.tokens.some(t => t.type === TokenType.Comment);

        // Check for specific comment regions
        if (this.isRegionComment(curr) && this.isEndRegionComment(prev)) {
            return 1;
        }

        if (currIsComment && !prevIsComment && !this.isDefinition(prevToken)) {
            // If current is a comment block and previous was code definitions
            return 1;
        }

        // 3. Control Structures (IF, WHILE, FOR, etc.)
        // Add blank line before control block start
        if (this.isControlStart(currToken)) {
            if (!this.isControlStart(prevToken)) {
                return 1;
            }
        }

        // Force blank line before :CASE, :OTHERWISE
        // (Removing isBlockMiddle check to avoid forcing blank line before :ELSE)
        if (this.isCaseKeyword(currToken)) {
            return 1;
        }

        // Add blank line after control block end
        if (this.isControlEnd(prevToken)) {
            // Unless followed by ELSE or another END (nested closing)
            if (!this.isBlockMiddle(currToken) && !this.isCaseKeyword(currToken) && !this.isControlEnd(currToken)) {
                return 1;
            }
        }

        // 4. Major Blocks (Procedure, Class) - Always separate
        if (this.isMajorBlockStart(currToken)) {
            if (prev.type === NodeType.Comment) { return 0; }
            return 1;
        }
        if (this.isMajorBlockEnd(prevToken)) { return 1; }

        return 0;
    }

    private getFirstSignificantToken(node: Node): Token | undefined {
        if (!node || node.tokens.length === 0) { return undefined; }
        for (const t of node.tokens) {
            if (t.type !== TokenType.Whitespace) { return t; }
        }
        return undefined;
    }

    private isDefinition(token: Token | undefined): boolean {
        if (!token || token.type !== TokenType.Keyword) { return false; }
        const text = token.text.toUpperCase().replace(/^:/, ''); // Handle legacy colon
        return PROCEDURE_LEVEL_KEYWORDS.includes(text);
    }

    private isControlStart(token: Token | undefined): boolean {
        if (!token || token.type !== TokenType.Keyword) { return false; }
        const text = token.text.toUpperCase().replace(/^:/, '');
        // Exclude Procedure/Class/Region from "Control Start" spacing logic if handled separately
        return BLOCK_START_KEYWORDS.includes(text) && !MAJOR_BLOCK_START_KEYWORDS.includes(text);
    }

    private isControlEnd(token: Token | undefined): boolean {
        if (!token || token.type !== TokenType.Keyword) { return false; }
        const text = token.text.toUpperCase().replace(/^:/, '');
        return BLOCK_END_KEYWORDS.includes(text) && !MAJOR_BLOCK_END_KEYWORDS.includes(text);
    }

    // Middle keywords like ELSE, CATCH, FINALLY
    private isBlockMiddle(token: Token | undefined): boolean {
        if (!token || token.type !== TokenType.Keyword) { return false; }
        const text = token.text.toUpperCase().replace(/^:/, '');
        return BLOCK_MIDDLE_KEYWORDS.includes(text);
    }

    private isCaseKeyword(token: Token | undefined): boolean {
        if (!token) { return false; }
        const text = token.text.toUpperCase().replace(/^:/, '');
        return CASE_KEYWORDS.includes(text);
    }

    private isMajorBlockStart(token: Token | undefined): boolean {
        if (!token || token.type !== TokenType.Keyword) { return false; }
        const text = token.text.toUpperCase().replace(/^:/, '');
        return MAJOR_BLOCK_START_KEYWORDS.includes(text);
    }

    private isMajorBlockEnd(token: Token | undefined): boolean {
        if (!token || token.type !== TokenType.Keyword) { return false; }
        const text = token.text.toUpperCase().replace(/^:/, '');
        return MAJOR_BLOCK_END_KEYWORDS.includes(text);
    }

    private isRegionComment(node: Node): boolean {
        return node.type === NodeType.RegionStart;
    }

    private isEndRegionComment(node: Node): boolean {
        return node.type === NodeType.RegionEnd;
    }
}
