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
    private maxConsecutiveBlankLines: number;

    constructor(maxConsecutiveBlankLines: number = 2) {
        this.maxConsecutiveBlankLines = maxConsecutiveBlankLines;
    }

    /**
     * Determine usage of blank lines between statements
     */
    public getVerticalWhitespace(prev: Node, curr: Node): number {
        const currToken = this.getFirstSignificantToken(curr);
        const prevToken = this.getFirstSignificantToken(prev);
        const currText = currToken ? currToken.text.toUpperCase() : "";
        const prevText = prevToken ? prevToken.text.toUpperCase() : "";

        // Calculate existing blank lines (preservation of intent)
        // startLine is 1-based, so if prev ends on 10 and curr starts on 12, there is 1 blank line (line 11).
        // Calculate existing blank lines (preservation of intent)
        // startLine is 1-based, so if prev ends on 10 and curr starts on 12, there is 1 blank line (line 11).
        const existingBlankLines = Math.max(0, curr.startLine - prev.endLine - 1);
        const preservedLines = Math.min(existingBlankLines, this.maxConsecutiveBlankLines);

        // Inline Comments - append to same line
        if (curr.type === NodeType.Comment && prev.endLine === curr.startLine) {
            return -1;
        }

        // Definition Blocks (PARAMETERS, DEFAULT, DECLARE)
        // Same definition type = grouped together (no blank)
        // Different definition types = blank line between
        if (this.isDefinition(currToken) && this.isDefinition(prevToken)) {
            if (currText === prevText) { return 0; }  // e.g., DEFAULT followed by DEFAULT
            return 1;  // e.g., PARAMETERS → DEFAULT, or DEFAULT → DECLARE
        }

        // Special Case: Header Comment -> Definition
        // If the previous node was a header comment (lines 1-x), and we are now at the first definition (PARAMETERS usually),
        // we want exactly 1 blank line, not preservedLines (which might be 2 or more if the user went wild).
        if (this.isDefinition(currToken) && prev.type === NodeType.Comment && prev.startLine === 1) {
            return 1;
        }

        // Add blank line after definition block before body code
        // (style guide section 1.4.1: blank line between declarations and code)
        if (this.isDefinition(prevToken) && !this.isDefinition(currToken)) {
            return 1;
        }

        // Comments - give them room
        const currIsComment = curr.tokens.some(t => t.type === TokenType.Comment);
        const prevIsComment = prev.tokens.some(t => t.type === TokenType.Comment);

        if (this.isRegionComment(curr) && this.isEndRegionComment(prev)) {
            return 1;
        }

        if (currIsComment && !prevIsComment && !this.isDefinition(prevToken)) {
            // Special case: If prev was a block of comments at the very start of the file (likely header),
            // and we are now at the first code/definition, limit blank lines to 1 (or 0 if user had none).
            // We want to avoid "double gaping" the header.
            // A simple heuristic: if prev.startLine is 1 (or close to it) and it's a comment block.
            if (prev.type === NodeType.Comment && prev.startLine === 1) {
                return 1; // Enforce distinct separation but no more than 1 blank line for standard header
            }

            // Preserve user intent for comments, but ensure at least 1 blank line if previously handled
            // Actually, for comments, we often want to respect the user's spacing more strictly.
            // If the user put a comment immediately after code, maybe they want it there (handled above).
            // But generally, comments introducing a section get a blank line.
            return Math.max(1, preservedLines);
        }

        // Control Structures (IF, WHILE, FOR, etc.)
        if (this.isControlStart(currToken) && !this.isControlStart(prevToken)) {
            return Math.max(1, preservedLines);
        }

        // Force blank line before :CASE, :OTHERWISE
        if (this.isCaseKeyword(currToken)) {
            return 1;
        }

        // Add blank line after control block end
        if (this.isControlEnd(prevToken)) {
            // Don't add blank if: next node has no token, or followed by block middle/case/control end/major block end
            if (currToken && !this.isBlockMiddle(currToken) && !this.isCaseKeyword(currToken) && !this.isControlEnd(currToken) && !this.isMajorBlockEnd(currToken)) {
                return Math.max(1, preservedLines);
            }
        }

        // Major Blocks (Procedure, Class) - Always separate
        if (this.isMajorBlockStart(currToken)) {
            if (prev.type === NodeType.Comment) { return 0; }
            return Math.max(1, preservedLines);
        }
        if (this.isMajorBlockEnd(prevToken)) {
            return Math.max(1, preservedLines);
        }

        // Default: Return preserved lines (up to max) instead of 0
        return preservedLines;
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

