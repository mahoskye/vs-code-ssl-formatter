import {
    CodeFormatter,
    FormatterInsight,
    TypedBlock,
    ProcessedLine,
    ContentSegment,
    BlockOperation,
    FormatterPipeline,
    BlockIdentifier,
} from "./formattingPipeline";

export class SemicolonNewlineFormatter implements CodeFormatter {
    private insights: FormatterInsight[] = [];
    private pipeline: FormatterPipeline;

    constructor(pipeline: FormatterPipeline) {
        this.pipeline = pipeline;
    }

    public getName(): string {
        return "SemicolonNewline";
    }

    public getInsights(): FormatterInsight[] {
        return this.insights;
    }

    public async format(blocks: TypedBlock[]): Promise<void> {
        this.insights = [];
        const operations: BlockOperation[] = [];

        // Process each block
        for (const [blockIndex, block] of blocks.entries()) {
            const splitOperations = this.processBlock(block, blockIndex);
            operations.push(...splitOperations);
        }

        // Register all operations with the pipeline
        operations.forEach((operation) => {
            this.pipeline.registerBlockOperation(operation);
        });
    }

    private processBlock(block: TypedBlock, blockIndex: number): BlockOperation[] {
        const operations: BlockOperation[] = [];
        let lineOffset = 0;

        // Process each line in the block
        for (let lineIndex = 0; lineIndex < block.lines.length; lineIndex++) {
            const line = block.lines[lineIndex];
            const splitLines = this.splitLineOnSemicolons(line);

            if (splitLines.length > 1) {
                // Create new blocks for each split line
                const newBlocks = this.createBlocksFromSplitLines(splitLines, block);

                // Create an update operation for the current block
                operations.push({
                    type: "update",
                    blockIndex: blockIndex + lineOffset,
                    blocks: newBlocks,
                });

                // Track how many new blocks we're adding
                lineOffset += newBlocks.length - 1;

                // Record insights
                this.insights.push({
                    formatterName: this.getName(),
                    sourceLineNumber: line.originalLineNumber,
                    changeType: "splitting",
                    description: `Split into ${splitLines.length} lines`,
                    before: line.originalString,
                    after: splitLines.map((l) => l.originalString).join("\n"),
                });
            }
        }

        return operations;
    }

    private createBlocksFromSplitLines(
        lines: ProcessedLine[],
        originalBlock: TypedBlock
    ): TypedBlock[] {
        const blocks: TypedBlock[] = [];
        let currentLines: ProcessedLine[] = [];

        // Group split lines into new blocks
        for (const line of lines) {
            currentLines.push(line);

            // If this line ends with a semicolon, create a new block
            if (line.trimmedContent.endsWith(";")) {
                const newBlock = this.createBlockFromLines(currentLines, originalBlock);
                blocks.push(newBlock);
                currentLines = [];
            }
        }

        // Handle any remaining lines
        if (currentLines.length > 0) {
            blocks.push(this.createBlockFromLines(currentLines, originalBlock));
        }

        return blocks;
    }

    private createBlockFromLines(lines: ProcessedLine[], originalBlock: TypedBlock): TypedBlock {
        // Use BlockIdentifier to determine block type and metadata
        const identification = BlockIdentifier.identify(lines);

        return {
            lines,
            blockType: identification.blockType,
            metadata: identification.metadata,
            context: {
                ...originalBlock.context,
                isPartOfChain: false,
                parentBlockType: originalBlock.context.parentBlockType,
                depth: originalBlock.context.depth,
            },
            startLineNumber: lines[0].lineNumber,
            endLineNumber: lines[lines.length - 1].lineNumber,
        };
    }

    private splitLineOnSemicolons(line: ProcessedLine): ProcessedLine[] {
        const splitPositions = this.findSplitPositions(line);
        if (splitPositions.length === 0) {
            return [line];
        }

        const result: ProcessedLine[] = [];
        let currentStart = 0;

        for (const splitPos of splitPositions) {
            const newLine = this.createProcessedLine(
                line,
                currentStart,
                splitPos + 1,
                line.lineNumber,
                line.originalLineNumber,
                currentStart === 0 ? line.leadingWhitespace : line.leadingWhitespace + "    "
            );
            result.push(newLine);
            currentStart = splitPos + 1;
        }

        // Add remaining content if any and not empty
        if (currentStart < line.originalString.length) {
            const remainingContent = line.originalString.substring(currentStart).trim();
            if (remainingContent) {
                result.push(
                    this.createProcessedLine(
                        line,
                        currentStart,
                        line.originalString.length,
                        line.lineNumber,
                        line.originalLineNumber,
                        line.leadingWhitespace + "    "
                    )
                );
            }
        }

        return result;
    }

    private createProcessedLine(
        originalLine: ProcessedLine,
        startPos: number,
        endPos: number,
        lineNumber: number,
        originalLineNumber: number,
        leadingWhitespace: string
    ): ProcessedLine {
        // Extract code content
        const content = originalLine.originalString.substring(startPos, endPos);
        const trimmedContent = content.trim();

        // Split segments by type
        const segments = this.createSegmentsForSplit(originalLine.segments, startPos, endPos);
        const codeSegments = segments.filter((s) => s.type === "code");
        const commentSegments = segments.filter((s) => s.type === "comment");

        // Build formatted content
        const formattedContent = `${leadingWhitespace}${codeSegments
            .map((s) => s.content)
            .join(" ")
            .trim()}${
            commentSegments.length > 0 ? " " + commentSegments.map((s) => s.content).join(" ") : ""
        }`;

        return {
            originalString: originalLine.originalString,
            formattedString: formattedContent,
            leadingWhitespace,
            trimmedContent: codeSegments
                .map((s) => s.content)
                .join(" ")
                .trim(),
            lineNumber,
            originalLineNumber,
            segments,
        };
    }

    private createSegmentsForSplit(
        originalSegments: ContentSegment[],
        startPos: number,
        endPos: number
    ): ContentSegment[] {
        const segments: ContentSegment[] = [];

        for (const segment of originalSegments) {
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
                content: segment.content.substring(
                    overlapStart - segment.startIndex,
                    overlapEnd - segment.startIndex
                ),
                startIndex: relativeStart,
                endIndex: relativeEnd,
                tokenType: segment.tokenType,
                priority: segment.priority,
            });
        }

        return segments;
    }

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

                // Skip if semicolon is in a string or comment
                if (
                    !containingSegment ||
                    containingSegment.type === "string" ||
                    containingSegment.type === "comment"
                ) {
                    continue;
                }

                // Get the rest of the line after this semicolon
                const restOfLine = line.originalString.substring(i + 1).trim();
                const isClosing = restOfLine === ")" || restOfLine === ");" || restOfLine === "";

                if (i > lastFoundPosition && !isClosing) {
                    splitPositions.push(i);
                    lastFoundPosition = i;
                }
            }
        }

        return splitPositions;
    }
}
