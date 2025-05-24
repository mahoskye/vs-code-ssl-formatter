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
                const newBlocks = this.createBlocksFromSplitLines(splitLines, block);

                operations.push({
                    type: "update",
                    blockIndex: blockIndex + lineOffset,
                    blocks: newBlocks,
                });

                lineOffset += newBlocks.length - 1;

                this.insights.push({
                    formatterName: this.getName(),
                    sourceLineNumber: line.originalLineNumber,
                    changeType: "splitting",
                    description: `Split into ${splitLines.length} lines`,
                    before: line.originalString,
                    after: splitLines.map((l) => l.formattedString).join("\n"),
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

        // Process each line and create blocks
        for (const line of lines) {
            currentLines.push(line);

            if (line.trimmedContent.endsWith(";")) {
                const newBlock = this.createBlockFromLines(currentLines, originalBlock);
                blocks.push(newBlock);
                currentLines = [];
            }
        }

        if (currentLines.length > 0) {
            blocks.push(this.createBlockFromLines(currentLines, originalBlock));
        }

        return blocks;
    }

    private createBlockFromLines(lines: ProcessedLine[], originalBlock: TypedBlock): TypedBlock {
        const identification = BlockIdentifier.identify(lines);

        return {
            lines: lines.map((line) => ({
                ...line,
                originalString: line.formattedString,
            })),
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
                currentStart === 0 ? line.leadingWhitespace : line.leadingWhitespace
            );

            result.push(newLine);
            currentStart = splitPos + 1;
        }

        if (currentStart < line.originalString.length) {
            const remainingContent = line.originalString.substring(currentStart).trim();
            if (remainingContent) {
                const finalLine = this.createProcessedLine(
                    line,
                    currentStart,
                    line.originalString.length,
                    line.lineNumber,
                    line.originalLineNumber,
                    line.leadingWhitespace
                );

                result.push(finalLine);
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
        const content = originalLine.originalString.substring(startPos, endPos);
        const trimmedContent = content.trim();
        const segments = this.createSegmentsForSplit(originalLine.segments, startPos, endPos);
        const orderedSegments = segments.sort((a, b) => a.startIndex - b.startIndex);

        let formattedContent = leadingWhitespace;
        let lastEndIndex = 0;

        orderedSegments.forEach((segment, index) => {
            if (index > 0 && segment.startIndex > lastEndIndex) {
                formattedContent += " ";
            }

            formattedContent += segment.content;
            lastEndIndex = segment.endIndex;
        });

        return {
            originalString: originalLine.originalString,
            formattedString: formattedContent,
            leadingWhitespace,
            trimmedContent,
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
            if (segment.endIndex <= startPos || segment.startIndex >= endPos) {
                continue;
            }

            const overlapStart = Math.max(segment.startIndex, startPos);
            const overlapEnd = Math.min(segment.endIndex, endPos);

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

        for (let i = 0; i < line.originalString.length; i++) {
            if (line.originalString[i] === ";") {
                const containingSegment = line.segments.find(
                    (seg) => i >= seg.startIndex && i < seg.endIndex
                );

                if (containingSegment?.type === "comment") {
                    splitPositions.push(i);
                    lastFoundPosition = i;
                    continue;
                }

                if (containingSegment?.type === "string") {
                    continue;
                }

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
