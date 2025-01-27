import {
    CodeFormatter,
    FormatterInsight,
    TypedBlock,
    ContentSegment,
    ProcessedLine,
} from "./formattingPipeline";

export class LongLineBreaker implements CodeFormatter {
    private insights: FormatterInsight[] = [];
    private readonly maxLength: number;
    private readonly continuationIndent: number = 11; // Standard SSL continuation indent

    constructor(maxLength: number = 90) {
        this.maxLength = maxLength;
    }

    public getName(): string {
        return "LongLineBreaker";
    }

    public getInsights(): FormatterInsight[] {
        return this.insights;
    }

    public async format(blocks: TypedBlock[]): Promise<void> {
        this.insights = [];

        blocks.forEach((block) => {
            block.lines.forEach((line, lineIndex) => {
                if (line.formattedString.length > this.maxLength) {
                    // Skip lines that are single comments
                    if (this.isSingleComment(line)) {
                        return;
                    }

                    const breakPoints = this.findBreakPoints(line.segments);
                    if (breakPoints.length > 0) {
                        const newLines = this.breakLine(line, breakPoints);
                        line.formattedString = newLines.join("\n");

                        this.insights.push({
                            formatterName: this.getName(),
                            sourceLineNumber: line.originalLineNumber,
                            changeType: "splitting",
                            description: "Long line broken into multiple lines",
                            before: line.originalString,
                            after: newLines.join("\n"),
                        });
                    }
                }
            });
        });
    }

    private isSingleComment(line: ProcessedLine): boolean {
        return line.segments.length === 1 && line.segments[0].type === "comment";
    }

    private findBreakPoints(segments: ContentSegment[]): number[] {
        const breakPoints: number[] = [];
        let currentLength = 0;
        let lastGoodBreakPoint = -1;
        let inString = false;
        let stringStart = -1;
        let functionCallDepth = 0;

        // Prioritize operators over function calls
        const breakOpportunities = [
            { type: "operator", content: "+", priority: 1, breakBefore: true },
            { type: "separator", content: ",", priority: 2, breakBefore: false },
            { type: "operator", content: ":=", priority: 3, breakBefore: false },
        ];

        segments.forEach((segment, index) => {
            // Track function call depth
            if (segment.type === "code" && segment.tokenType.type === "methodCall") {
                functionCallDepth++;
            } else if (segment.content === ")") {
                functionCallDepth = Math.max(0, functionCallDepth - 1);
            }

            // Track string segments
            if (segment.type === "string") {
                if (!inString) {
                    inString = true;
                    stringStart = index;
                }
            } else {
                if (inString) {
                    inString = false;
                }
            }

            currentLength += segment.content.length;

            // Don't break inside strings or function calls unless necessary
            if (
                (inString &&
                    currentLength - segments[stringStart].content.length < this.maxLength) ||
                (functionCallDepth > 0 && currentLength < this.maxLength)
            ) {
                return;
            }

            // Check for break opportunities based on priority
            for (const opportunity of breakOpportunities) {
                if (
                    segment.type === "code" &&
                    segment.tokenType.type === opportunity.type &&
                    segment.content.includes(opportunity.content)
                ) {
                    // For operators like '+', break before if specified
                    if (opportunity.breakBefore) {
                        lastGoodBreakPoint = index - 1;
                    } else {
                        lastGoodBreakPoint = index;
                    }
                    break;
                }
            }

            // Only consider method calls as break points if we have no other options
            if (
                currentLength > this.maxLength &&
                lastGoodBreakPoint === -1 &&
                segment.tokenType.type === "methodCall"
            ) {
                lastGoodBreakPoint = index;
            }

            if (currentLength > this.maxLength && lastGoodBreakPoint !== -1) {
                breakPoints.push(lastGoodBreakPoint);
                currentLength = this.calculateRemainingLength(segments, lastGoodBreakPoint);
            }
        });

        return breakPoints;
    }

    private calculateRemainingLength(segments: ContentSegment[], breakPoint: number): number {
        return segments
            .slice(breakPoint + 1)
            .reduce((length, segment) => length + segment.content.length, 0);
    }

    private breakLine(line: ProcessedLine, breakPoints: number[]): string[] {
        const lines: string[] = [];
        let currentLine = line.leadingWhitespace;
        let lastEndIndex = 0;
        let isFirstLine = true;

        line.segments.forEach((segment, index) => {
            if (breakPoints.includes(index - 1)) {
                lines.push(currentLine.trimEnd());
                // Calculate alignment based on first opening construct
                const continuationIndent = isFirstLine
                    ? this.calculateFirstLineIndent(line)
                    : this.calculateContinuationIndent(line, false);
                currentLine = " ".repeat(continuationIndent);
                isFirstLine = false;
            }

            // Add proper spacing around operators and separators
            if (index > 0 && segment.startIndex > lastEndIndex) {
                currentLine += " ";
            }

            currentLine += segment.content;
            lastEndIndex = segment.endIndex;
        });

        if (currentLine.trim()) {
            lines.push(currentLine);
        }

        return lines;
    }

    private calculateFirstLineIndent(line: ProcessedLine): number {
        // Find the first opening construct (like SQLExecute()
        const firstSegment = line.segments[0];
        if (firstSegment && firstSegment.type === "code") {
            // Align with opening parenthesis or first content
            return firstSegment.content.length + line.leadingWhitespace.length;
        }
        return line.leadingWhitespace.length + this.continuationIndent;
    }

    private calculateContinuationIndent(line: ProcessedLine, isFirstLine: boolean): number {
        // Calculate base indentation from the original line
        const baseIndent = line.leadingWhitespace.length;

        // For first line breaks, align with the standard SSL continuation indent
        if (isFirstLine) {
            return baseIndent + this.continuationIndent;
        }

        // For subsequent breaks, maintain the same indentation as the first continuation
        return baseIndent + this.continuationIndent;
    }
}
