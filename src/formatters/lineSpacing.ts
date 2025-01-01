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
        newLine: /\r?\n/,
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
    let debug = false;

    // Split text into blocks and process lines
    const blocks = splitIntoBlocks(text);

    // Determine spacing requirements
    const spacedBlocks = determineSpacing(blocks);

    // Apply spacing and join blocks
    return debug ? formatBlocksDebug(spacedBlocks) : formatBlocks(spacedBlocks);
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
    let isBlockComment = false;

    for (let i = 0; i < lines.length; i++) {
        const line = processLine(lines[i]);
        currentBlock.push(line);

        // Check if start of block comment
        if (patterns.comments.blockComment.test(line.trimmedContent)) {
            isBlockComment = true;
        }

        // Count blank lines after this block
        let blankCount = 0;
        let nextIndex = i + 1;
        while (nextIndex < lines.length && patterns.lines.blankLine.test(lines[nextIndex].trim())) {
            blankCount++;
            nextIndex++;
        }

        // Get the next non-blank line
        const nextContentLine = nextIndex < lines.length ? processLine(lines[nextIndex]) : undefined;

        // Check if this line ends a block
        if (
            (!isBlockComment && patterns.operators.semicolon.test(line.originalString)) || // Normal line ending
            (!isBlockComment && patterns.comments.blockComment.test(line.trimmedContent)) || // Start of new block comment
            (isBlockComment && patterns.operators.semicolon.test(line.originalString)) // End of block comment
        ) {
            blocks.push({
                lines: [...currentBlock],
                followingSpaces: Math.min(blankCount, 2),
                nextBlockFirstLine: nextContentLine,
            });
            currentBlock = [];
            isBlockComment = false; // Reset flag
            i += blankCount; // Skip the blank lines we've already counted
        }
    }

    // Handle any remaining lines
    if (currentBlock.length > 0) {
        blocks.push({
            lines: currentBlock,
            followingSpaces: 0,
            nextBlockFirstLine: undefined, // This is the last block
        });
    }

    return blocks;
}

function determineSpacing(blocks: ContentBlock[]): ContentBlock[] {
    return blocks;
}

function formatBlocks(blocks: ContentBlock[]): string {
    let result = "";

    blocks.forEach((block, index) => {
        // Add all lines in the block with their original whitespace
        block.lines.forEach((line) => {
            result += line.originalString + "\n";
        });

        // Add following spaces if this isn't the last block
        if (index < blocks.length - 1) {
            for (let i = 0; i < block.followingSpaces; i++) {
                result += "\n";
            }
        }
    });

    // Ensure exactly one trailing newline
    return result.trimEnd() + "\n";
}

// Formatting function for debugging
function formatBlocksDebug(blocks: ContentBlock[]): string {
    return (
        blocks
            .map((block, index) => {
                const firstLine = block.lines[0].trimmedContent;
                const nextLine = block.nextBlockFirstLine?.trimmedContent || "END";

                // Truncate long lines for readability
                const truncateLength = 30;
                const truncatedFirst =
                    firstLine.length > truncateLength ? firstLine.substring(0, truncateLength) + "..." : firstLine;
                const truncatedNext =
                    nextLine.length > truncateLength ? nextLine.substring(0, truncateLength) + "..." : nextLine;

                return (
                    `Block ${index + 1}: lines: ${block.lines.length} | ` +
                    `blanks: ${block.followingSpaces} | firstline: ${truncatedFirst} | ` +
                    `nextline: ${truncatedNext}`
                );
            })
            .join("\n")
            .trimEnd() + "\n"
    );
}
