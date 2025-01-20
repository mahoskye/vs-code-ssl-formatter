import { CodeFormatter, TypedBlock, ProcessedLine, ContentSegment } from "./formattingPipeline";

export class SemicolonNewlineFormatter implements CodeFormatter {
    public async format(blocks: TypedBlock[]): Promise<void> {
        for (const block of blocks) {
            // Skip empty blocks
            if (block.lines.length === 0) {
                continue;
            }

            // Process each line in the block
            for (let i = 0; i < block.lines.length; i++) {
                const line = block.lines[i];

                // Skip empty lines
                if (line.trimmedContent === "") {
                    continue;
                }

                // Find lines that need to be split
                const newLines = this.splitLineOnSemicolons(line);
                if (newLines.length > 1) {
                    // Replace current line with split lines
                    block.lines.splice(i, 1, ...newLines);
                    // Adjust index to account for new lines
                    i += newLines.length - 1;
                }
            }
        }
    }

    /**
     * Find valid semicolon positions that should trigger line splits
     */
    private findSplitPositions(line: ProcessedLine): number[] {
        const splitPositions: number[] = [];
        let lastFoundPosition = -1;

        // Find all semicolons in the line
        for (let i = 0; i < line.originalString.length; i++) {
            if (line.originalString[i] === ";") {
                // Find which segment contains this semicolon
                const containingSegment = line.segments.find(
                    (seg) => i >= seg.startIndex && i < seg.endIndex
                );

                if (!containingSegment) {
                    continue;
                }

                // Skip semicolons in string literals
                if (containingSegment.type === "string") {
                    continue;
                }

                // Get the rest of the line after this semicolon
                const restOfLine = line.originalString.substring(i + 1).trim();

                // Don't split if:
                // 1. This is just a closing parenthesis or semicolon
                // 2. There's no content after this point
                // 3. We've already processed this position
                const isClosing = restOfLine === ")" || restOfLine === ");" || restOfLine === "";

                if (i > lastFoundPosition && !isClosing) {
                    splitPositions.push(i);
                    lastFoundPosition = i;
                }
            }
        }

        return splitPositions;
    }

    /**
     * Split a line on semicolons and preserve segment information
     */
    private splitLineOnSemicolons(line: ProcessedLine): ProcessedLine[] {
        const splitPositions = this.findSplitPositions(line);

        // If no valid split positions, return original line
        if (splitPositions.length === 0) {
            return [line];
        }

        const result: ProcessedLine[] = [];
        let currentStart = 0;
        let newLineNumber = line.lineNumber;

        // Create new lines for each split position
        for (const splitPos of splitPositions) {
            // Create a new line up to and including the semicolon
            const newLine = this.createProcessedLine(
                line,
                currentStart,
                splitPos + 1, // Include the semicolon
                newLineNumber++,
                line.leadingWhitespace
            );
            result.push(newLine);
            currentStart = splitPos + 1;
        }

        // Add remaining content as the last line if there is any
        if (currentStart < line.originalString.length) {
            const finalLine = this.createProcessedLine(
                line,
                currentStart,
                line.originalString.length,
                newLineNumber,
                line.leadingWhitespace
            );
            result.push(finalLine);
        }

        return result;
    }

    /**
     * Create a new ProcessedLine with updated segment information
     */
    private createProcessedLine(
        originalLine: ProcessedLine,
        startPos: number,
        endPos: number,
        lineNumber: number,
        leadingWhitespace: string
    ): ProcessedLine {
        const content = originalLine.originalString.substring(startPos, endPos);
        const trimmedContent = content.trim();

        // Create new segments that fall within this line's range
        const segments: ContentSegment[] = [];

        for (const segment of originalLine.segments) {
            // Skip segments that don't overlap with our range
            if (segment.endIndex <= startPos || segment.startIndex >= endPos) {
                continue;
            }

            // Calculate the overlapping portion
            const overlapStart = Math.max(segment.startIndex, startPos);
            const overlapEnd = Math.min(segment.endIndex, endPos);

            // Calculate relative positions for the new segment
            const relativeStart = overlapStart - startPos;
            const relativeEnd = overlapEnd - startPos;

            segments.push({
                type: segment.type,
                content: content.substring(relativeStart, relativeEnd),
                startIndex: relativeStart,
                endIndex: relativeEnd,
                tokenType: segment.tokenType,
                priority: segment.priority,
            });
        }

        return {
            originalString: leadingWhitespace + trimmedContent,
            leadingWhitespace,
            trimmedContent,
            lineNumber,
            segments,
        };
    }
}
