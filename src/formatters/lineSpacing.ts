/**
 * Handles line spacing formatting for STARLIMS code files
 */

// Interface for processed individual lines
interface ProcessedLine {
    originalString: string; // The unmodified line
    leadingWhitespace: string; // Leading whitespace from the original line
    trimmedContent: string; // Line with whitespace trimmed
}

// Interface for content blocks
interface ContentBlock {
    lines: ProcessedLine[]; // Array of lines in this block
    followingSpaces: number; // Number of spaces after this block
    nextBlockFirstLine?: ProcessedLine; // First line of the next block (if needed)
}

// Common regex patterns for rule matching
export const patterns = {
    comments: {
        blockComment: /^\/\*\*/,
        singleLineComment: /^\/\*/,
    },
    lines: {
        blankLine: /^\s*$/,
        newLine: /\?r\n/,
        multiLine: /[^;]\s*$/,
    },
    operators: {
        semicolon: /;/,
        assignment: /:=/,
    },
    regions: {
        regionStart: /^\/\*\s*region\b/i,
        regionEnd: /^\/\*\s*endregion\b/i,
    },
    spacing: {
        double: {
            after: /:(ENDPROC|ENDCASE|ENDWHILE|ENDIF|ENDREGION|RESUME|ERROR)\b/i,
            before: /:(PROCEDURE|ERROR)\b/i,
        },
        single: {
            after: /:(BEGINCASE|ERROR|EXITCASE)\b/i,
            before: /:(ELSE|ENDIF|ENDCASE|EXITCASE|RESUME)\b/i,
        },
        nospace: {
            after: null,
            before: /^:(CASE|OTHERWISE)\b/i,
        },
    },
    statements: {
        declarations: /^:(?:DECLARE|INCLUDE|DEFAULT|PARAMETERS)\b/i,
        logic: /:(IF|ELSE|WHILE|FOR|BEGINCASE|CASE|OTHERWISE|ENDCASE|ENDIF|ENDWHILE|EXITCASE|EXITWHILE)\b/i,
        return: /^:RETURN\b/i,
        procedureStart: /:(?:CLASS|PROCEDURE)\b/i,
        procedureEnd: /:ENDPROC\b/i,
        errorStart: /:ERROR\b/i,
        errorEnd: /:RESUME\b/i,
        caseStart: /:BEGINCASE\b/i,
        caseEnd: /:ENDCASE\b/i,
    },
} as const;

// Main formatter function
export function lineSpacingFormatter(text: string): string {
    // Split text into blocks and process lines
    const blocks = splitIntoBlocks(text);

    // Determine spacing requirements
    const spacedBlocks = determineSpacing(blocks);

    // Apply spacing and join blocks
    return formatBlocks(spacedBlocks);
}

function processLine(line: string): ProcessedLine {
    const trimmed = line.trim();
    const leadingWhitespace = line.substring(0, line.indexOf(trimmed));

    return {
        originalString: line,
        leadingWhitespace,
        trimmedContent: trimmed,
    };
}

function splitIntoBlocks(text: string): ContentBlock[] {
    const lines = text.split(patterns.lines.newLine);
    const blocks: ContentBlock[] = [];
    let currentBlock: ProcessedLine[] = [];

    for (let i = 0; i < lines.length; i++) {
        const line = processLine(lines[i]);
        currentBlock.push(line);

        // Check if this is a block comment
        if (patterns.comments.blockComment.test(line.trimmedContent)) {
            // Get the first line of the next block if it exists
            const nextLine = i + 1 < lines.length ? processLine(lines[i + 1]) : undefined;

            blocks.push({
                lines: [...currentBlock],
                followingSpaces: 0, // TBD later
                nextBlockFirstLine: nextLine,
            });
            currentBlock = [];
            continue;
        }

        // Check if this line ends a block
        if (patterns.operators.semicolon.test(line.originalString)) {
            // Get the first line of th enext block if it exists
            const nextLine = i + 1 < lines.length ? processLine(lines[i + 1]) : undefined;

            blocks.push({
                lines: [...currentBlock],
                followingSpaces: 0, // TBD later
                nextBlockFirstLine: nextLine,
            });
            currentBlock = [];
        }
    }

    // Handle any remaining lines
    if (currentBlock.length > 0) {
        blocks.push({
            lines: currentBlock,
            followingSpaces: 0, // TBD later
            nextBlockFirstLine: undefined, // This is the last block
        });
    }

    return blocks;
}

function determineSpacing(blocks: ContentBlock[]): ContentBlock[] {
    // TODO: Implement spacing logic
    return blocks;
}

function formatBlocks(blocks: ContentBlock[]): string {
    // TODO: Implement block formatting and joining
    return "";
}
