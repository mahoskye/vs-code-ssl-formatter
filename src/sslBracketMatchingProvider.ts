/**
 * SSL Bracket/Block Matching Provider
 * Highlights matching SSL block pairs like :IF/:ENDIF, :WHILE/:ENDWHILE, etc.
 */

import * as vscode from "vscode";

/**
 * Represents a block matching pair in SSL
 */
interface SSLBlockPair {
    /** The opening keyword (e.g., "if", "while", "procedure") */
    start: string;
    /** The closing keyword (e.g., "endif", "endwhile", "endproc") */
    end: string;
    /** Intermediate keywords that are part of this block (e.g., "else", "case") */
    intermediates?: string[];
    /** Whether this block supports nesting (most do, except simple ones like ERROR) */
    allowsNesting?: boolean;
}

/**
 * Context for tracking nested blocks during parsing
 */
interface BlockContext {
    /** The type of block (e.g., "IF", "WHILE") */
    type: string;
    /** Line number where the block starts */
    startLine: number;
    /** Character position where the block starts */
    startChar: number;
    /** Length of the opening keyword */
    keywordLength: number;
}

/**
 * Provides document highlighting for SSL block matching
 */
export class SSLBracketMatchingProvider implements vscode.DocumentHighlightProvider {
    private readonly blockPairs: SSLBlockPair[] = [
        // Conditional blocks
        {
            start: "if",
            end: "endif",
            intermediates: ["else"],
            allowsNesting: true,
        },

        // Loop blocks
        {
            start: "while",
            end: "endwhile",
            allowsNesting: true,
        },
        {
            start: "for",
            end: "next",
            allowsNesting: true,
        },

        // Switch case blocks
        {
            start: "begincase",
            end: "endcase",
            intermediates: ["case", "otherwise"],
            allowsNesting: true,
        },

        // Error handling blocks
        {
            start: "try",
            end: "endtry",
            intermediates: ["catch", "finally"],
            allowsNesting: true,
        },
        {
            start: "error",
            end: "resume",
            allowsNesting: false,
        },

        // Procedure and class blocks
        {
            start: "procedure",
            end: "endproc",
            allowsNesting: true,
        },
        {
            start: "class",
            end: "endclass", // Note: Classes auto-close at end of file in SSL v11
            allowsNesting: true,
        },

        // Region blocks
        {
            start: "region",
            end: "endregion",
            allowsNesting: true,
        },

        // Inline code blocks
        {
            start: "begininlinecode",
            end: "endinlinecode",
            allowsNesting: true,
        },
    ];

    /**
     * Updates string state for a line, handling SSL's three string delimiter types
     */
    private updateStringState(
        line: string,
        currentlyInString: boolean,
        currentDelimiter: string
    ): { inString: boolean; delimiter: string } {
        let inString = currentlyInString;
        let delimiter = currentDelimiter;

        if (!inString) {
            // Look for string start
            for (let i = 0; i < line.length; i++) {
                const char = line[i];
                if (char === '"' || char === "'" || char === "[") {
                    inString = true;
                    delimiter = char === "[" ? "]" : char;
                    break;
                }
            }
        } else {
            // Look for string end
            const endIndex = line.indexOf(delimiter);
            if (endIndex !== -1) {
                inString = false;
                delimiter = "";
            }
        }

        return { inString, delimiter };
    }

    /**
     * Provides document highlights for bracket/block matching
     */
    public provideDocumentHighlights(
        document: vscode.TextDocument,
        position: vscode.Position,
        token: vscode.CancellationToken
    ): vscode.DocumentHighlight[] | null {
        const line = document.lineAt(position.line);
        const lineText = line.text.toLowerCase().trim();

        // Check if cursor is on an SSL keyword
        const keywordMatch = this.getKeywordAtPosition(document, position);
        if (!keywordMatch) {
            return null;
        }

        const { keyword, range } = keywordMatch;

        // Find the block pair this keyword belongs to
        const blockPair = this.findBlockPair(keyword);
        if (!blockPair) {
            return null;
        }

        // Find all matching keywords in the document
        const matches = this.findMatchingKeywords(document, position, blockPair, keyword);

        if (matches.length === 0) {
            return null;
        }

        // Convert matches to document highlights
        return matches.map(
            (match) => new vscode.DocumentHighlight(match.range, vscode.DocumentHighlightKind.Text)
        );
    }

    /**
     * Gets the SSL keyword at the given position
     */
    private getKeywordAtPosition(
        document: vscode.TextDocument,
        position: vscode.Position
    ): { keyword: string; range: vscode.Range } | null {
        const line = document.lineAt(position.line);
        const lineText = line.text;

        // Check for comment regions first (/* region, /* endregion)
        const commentRegionMatch = lineText.match(/\/\*\s*(region|endregion)\b/i);
        if (commentRegionMatch) {
            const start = lineText.indexOf(commentRegionMatch[0]);
            const end = start + commentRegionMatch[0].length;

            if (position.character >= start && position.character <= end) {
                const keyword = commentRegionMatch[1].toLowerCase();
                return {
                    keyword: keyword === "region" ? "region" : "endregion",
                    range: new vscode.Range(position.line, start, position.line, end),
                };
            }
        }

        // Check for SSL keywords (starting with :)
        const keywordPattern = /:([a-zA-Z][a-zA-Z0-9]*)\b/gi;
        let match;

        while ((match = keywordPattern.exec(lineText)) !== null) {
            const keywordStart = match.index;
            const keywordEnd = match.index + match[0].length;

            // Check if cursor is within this keyword
            if (position.character >= keywordStart && position.character <= keywordEnd) {
                const keyword = match[1].toLowerCase();

                return {
                    keyword,
                    range: new vscode.Range(position.line, keywordStart, position.line, keywordEnd),
                };
            }
        }

        return null;
    }

    /**
     * Finds the block pair definition for a given keyword
     */
    private findBlockPair(keyword: string): SSLBlockPair | null {
        for (const pair of this.blockPairs) {
            if (
                pair.start === keyword ||
                pair.end === keyword ||
                (pair.intermediates && pair.intermediates.includes(keyword))
            ) {
                return pair;
            }
        }
        return null;
    }

    /**
     * Finds all matching keywords for the given block pair and current keyword
     */
    private findMatchingKeywords(
        document: vscode.TextDocument,
        currentPosition: vscode.Position,
        blockPair: SSLBlockPair,
        currentKeyword: string
    ): { range: vscode.Range; keyword: string }[] {
        const matches: { range: vscode.Range; keyword: string }[] = [];
        const stack: BlockContext[] = [];
        let foundCurrentKeyword = false;
        let inString = false;
        let stringDelimiter = "";
        let inComment = false;

        // Process document line by line
        for (let lineIndex = 0; lineIndex < document.lineCount; lineIndex++) {
            const line = document.lineAt(lineIndex);
            const lineText = line.text;

            // Handle string state
            const stringState: { inString: boolean; delimiter: string } = this.updateStringState(
                lineText,
                inString,
                stringDelimiter
            );
            inString = stringState.inString;
            stringDelimiter = stringState.delimiter;

            // Handle comment state
            if (lineText.trim().startsWith("/*")) {
                inComment = true;
            }
            if (lineText.includes(";") && inComment) {
                inComment = false;
            }

            // Skip processing if inside string or comment (except for region comments)
            if (inString || (inComment && !lineText.match(/\/\*\s*(region|endregion)/i))) {
                continue;
            }

            // Find SSL keywords in this line
            const keywordsInLine = this.findKeywordsInLine(line, blockPair);

            for (const keywordInfo of keywordsInLine) {
                const { keyword, range } = keywordInfo;

                // Check if this is the current keyword position
                const isCurrentKeyword =
                    lineIndex === currentPosition.line &&
                    range.start.character <= currentPosition.character &&
                    range.end.character >= currentPosition.character;

                if (isCurrentKeyword) {
                    foundCurrentKeyword = true;
                    matches.push({ range, keyword });
                }

                // Handle block structure
                if (keyword === blockPair.start) {
                    // Opening keyword
                    if (blockPair.allowsNesting !== false) {
                        stack.push({
                            type: blockPair.start.toUpperCase(),
                            startLine: lineIndex,
                            startChar: range.start.character,
                            keywordLength: keyword.length + 1, // +1 for the colon
                        });
                    }

                    if (!isCurrentKeyword && foundCurrentKeyword) {
                        matches.push({ range, keyword });
                    }
                } else if (keyword === blockPair.end) {
                    // Closing keyword
                    if (blockPair.allowsNesting !== false && stack.length > 0) {
                        const context = stack.pop();
                        if (context && !isCurrentKeyword && foundCurrentKeyword) {
                            matches.push({ range, keyword });
                        }
                    } else if (!isCurrentKeyword && foundCurrentKeyword) {
                        matches.push({ range, keyword });
                    }
                } else if (blockPair.intermediates && blockPair.intermediates.includes(keyword)) {
                    // Intermediate keyword (like :ELSE, :CASE)
                    if (!isCurrentKeyword && foundCurrentKeyword && stack.length > 0) {
                        matches.push({ range, keyword });
                    }
                }

                // If we found current keyword and it's a simple non-nesting block, look for its pair
                if (isCurrentKeyword && blockPair.allowsNesting === false) {
                    return this.findSimpleBlockPair(
                        document,
                        currentPosition,
                        blockPair,
                        currentKeyword
                    );
                }
            }
        }

        return matches;
    }

    /**
     * Finds keywords in a single line that belong to the given block pair
     */
    private findKeywordsInLine(
        line: vscode.TextLine,
        blockPair: SSLBlockPair
    ): { keyword: string; range: vscode.Range }[] {
        const results: { keyword: string; range: vscode.Range }[] = [];
        const lineText = line.text;

        // Check for comment regions (/* region, /* endregion)
        if (blockPair.start === "region") {
            const commentRegionPattern = /\/\*\s*(region|endregion)\b/gi;
            let match;

            while ((match = commentRegionPattern.exec(lineText)) !== null) {
                const keyword = match[1].toLowerCase();
                if (keyword === "region" || keyword === "endregion") {
                    results.push({
                        keyword: keyword === "region" ? "region" : "endregion",
                        range: new vscode.Range(
                            line.lineNumber,
                            match.index,
                            line.lineNumber,
                            match.index + match[0].length
                        ),
                    });
                }
            }
        }

        // Check for SSL keywords (starting with :)
        const allKeywords = [blockPair.start, blockPair.end, ...(blockPair.intermediates || [])];

        for (const keyword of allKeywords) {
            const pattern = new RegExp(`:${keyword}\\b`, "gi");
            let match;

            while ((match = pattern.exec(lineText)) !== null) {
                results.push({
                    keyword: keyword.toLowerCase(),
                    range: new vscode.Range(
                        line.lineNumber,
                        match.index,
                        line.lineNumber,
                        match.index + match[0].length
                    ),
                });
            }
        }

        return results;
    }

    /**
     * Handles simple block pairs that don't support nesting (like :ERROR)
     */
    private findSimpleBlockPair(
        document: vscode.TextDocument,
        currentPosition: vscode.Position,
        blockPair: SSLBlockPair,
        currentKeyword: string
    ): { range: vscode.Range; keyword: string }[] {
        const matches: { range: vscode.Range; keyword: string }[] = [];
        const isStartKeyword = currentKeyword === blockPair.start;
        const targetKeyword = isStartKeyword ? blockPair.end : blockPair.start;
        const searchDirection = isStartKeyword ? 1 : -1; // 1 for forward, -1 for backward

        // Add current keyword to matches
        const currentRange = this.getKeywordAtPosition(document, currentPosition);
        if (currentRange) {
            matches.push({ range: currentRange.range, keyword: currentKeyword });
        }

        // Search for the matching keyword
        let startLine = currentPosition.line + searchDirection;
        const endLine = searchDirection > 0 ? document.lineCount : -1;

        for (let lineIndex = startLine; lineIndex !== endLine; lineIndex += searchDirection) {
            const line = document.lineAt(lineIndex);
            const pattern = new RegExp(`:${targetKeyword}\\b`, "i");
            const match = pattern.exec(line.text);

            if (match) {
                matches.push({
                    keyword: targetKeyword,
                    range: new vscode.Range(
                        lineIndex,
                        match.index,
                        lineIndex,
                        match.index + match[0].length
                    ),
                });
                break; // Found the pair, stop searching
            }
        }

        return matches;
    }
}
