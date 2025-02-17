import {
    CodeFormatter,
    FormatterInsight,
    TypedBlock,
    ContentSegment,
    ProcessedLine,
} from "./formattingPipeline";

export class LineSplitter implements CodeFormatter {
    private insights: FormatterInsight[] = [];
    private maxLineLength: number;

    constructor(maxLineLength: number) {
        this.maxLineLength = maxLineLength;
    }

    public getName(): string {
        return "LineSplitter";
    }

    public getInsights(): FormatterInsight[] {
        return this.insights;
    }

    public async format(blocks: TypedBlock[]): Promise<void> {
        this.insights = [];
        console.log(`\n[LineSplitter] Starting format with maxLineLength: ${this.maxLineLength}`);

        for (const block of blocks) {
            console.log(`\n[LineSplitter] Processing block of type: ${block.blockType}`);

            for (let i = 0; i < block.lines.length; i++) {
                const line = block.lines[i];
                console.log(
                    `\n[LineSplitter] Line ${line.lineNumber}: Length ${line.formattedString.length}`
                );
                console.log(`Content: "${line.trimmedContent}"`);

                if (this.shouldBreakLine(line)) {
                    console.log(
                        `Line exceeds max length (${this.maxLineLength}), searching for break points...`
                    );
                    const breakPoints = this.findBreakPoints(line);

                    if (breakPoints.length > 0) {
                        console.log(
                            `Found ${
                                breakPoints.length
                            } break points at positions: ${breakPoints.join(", ")}`
                        );
                        const newLines = this.breakLine(line, breakPoints);
                        console.log("Breaking into new lines:");
                        newLines.forEach((l, idx) =>
                            console.log(`  ${idx + 1}: "${l.formattedString}"`)
                        );

                        block.lines.splice(i, 1, ...newLines);
                        i += newLines.length - 1;

                        this.insights.push({
                            formatterName: this.getName(),
                            sourceLineNumber: line.originalLineNumber,
                            changeType: "splitting",
                            description: `Long line split into ${newLines.length} lines`,
                            before: line.originalString,
                            after: newLines.map((l) => l.formattedString).join("\n"),
                        });
                    } else {
                        console.log("No suitable break points found, leaving line unchanged");
                    }
                } else {
                    console.log("Line does not need breaking");
                }
            }
        }
    }

    private shouldBreakLine(line: ProcessedLine): boolean {
        if (line.segments.some((seg) => seg.type === "comment")) {
            console.log("[shouldBreakLine] Skipping comment line");
            return false;
        }

        if (line.trimmedContent === "") {
            console.log("[shouldBreakLine] Skipping empty line");
            return false;
        }

        const shouldBreak = line.formattedString.length > this.maxLineLength;
        console.log(
            `[shouldBreakLine] Line length ${line.formattedString.length} > ${this.maxLineLength}? ${shouldBreak}`
        );
        return shouldBreak;
    }

    private findBreakPoints(line: ProcessedLine): number[] {
        console.log("\n[findBreakPoints] Analyzing segments for break points:");
        const breakPoints: number[] = [];
        const segments = line.segments;
        let currentLength = line.leadingWhitespace.length;

        console.log(`Starting with leading whitespace length: ${currentLength}`);

        // Check if we're in a list context
        const isListContext = this.isInsideList(segments, segments.length - 1);
        if (isListContext) {
            console.log("Detected list context - using list-specific breaking rules");
            return this.findListBreakPoints(line);
        }

        for (let i = 0; i < segments.length; i++) {
            const segment = segments[i];
            currentLength += segment.content.length;

            console.log(`\nSegment ${i + 1}:`);
            console.log(`  Type: ${segment.tokenType.type}`);
            console.log(`  Content: "${segment.content}"`);
            console.log(`  Current total length: ${currentLength}`);

            if (currentLength > this.maxLineLength) {
                console.log("  Length exceeds maximum, searching for break point...");
                const breakPoint = this.findBestBreakPoint(segments, i);

                if (breakPoint !== -1) {
                    console.log(`  Found break point at position ${breakPoint}`);
                    breakPoints.push(breakPoint);
                    currentLength = line.leadingWhitespace.length + 4;
                    console.log(`  Reset length to ${currentLength} (indent + 4)`);
                } else {
                    console.log("  No suitable break point found");
                }
            }
        }

        return breakPoints;
    }

    private findBestBreakPoint(segments: ContentSegment[], currentIndex: number): number {
        console.log("\n[findBestBreakPoint] Searching for break point before index:", currentIndex);

        // Check for list context first
        const isInList = this.isInsideList(segments, currentIndex);
        if (isInList) {
            console.log("  Context: Inside list structure");
            // For lists, break after each comma
            for (let i = currentIndex; i >= 0; i--) {
                const segment = segments[i];
                if (segment.tokenType.type === "separator" && segment.content === ",") {
                    console.log(`    → Breaking after comma at ${segment.endIndex}`);
                    return segment.endIndex;
                }
            }
            // If no comma found but we're in a list, try to break after opening brace
            for (let i = currentIndex; i >= 0; i--) {
                const segment = segments[i];
                if (segment.tokenType.type === "separator" && segment.content === "{") {
                    console.log(`    → Breaking after opening brace at ${segment.endIndex}`);
                    return segment.endIndex;
                }
            }
        }

        // Original priority-based breaking logic
        const breakPriorities = [
            { type: "operator", content: ["+", "-", "*", "/", ":="] },
            { type: "separator", content: [","] },
            { type: "methodCall", content: [] },
            { type: "propertyAccess", content: [] },
        ];

        for (const priority of breakPriorities) {
            console.log(`\nChecking priority type: ${priority.type}`);
            if (priority.content.length) {
                console.log(`Looking for content: ${priority.content.join(", ")}`);
            }

            for (let i = currentIndex; i >= 0; i--) {
                const segment = segments[i];
                console.log(`  Checking segment ${i}:`);
                console.log(`    Type: ${segment.tokenType.type}`);
                console.log(`    Content: "${segment.content.trim()}"`);

                if (segment.tokenType.type === priority.type) {
                    if (
                        priority.content.length === 0 ||
                        priority.content.includes(segment.content.trim())
                    ) {
                        // Check the current segment's type for break point placement
                        console.log(`    Found candidate segment:`);
                        console.log(`      Type: ${segment.tokenType.type}`);
                        console.log(`      Content: "${segment.content.trim()}"`);

                        if (segment.tokenType.type === "operator") {
                            // For operators, break before the operator
                            console.log(`    → Breaking before operator at ${segment.startIndex}`);
                            return segment.startIndex;
                        } else if (segment.tokenType.type === "separator") {
                            // For separators, break after the separator
                            console.log(`    → Breaking after separator at ${segment.endIndex}`);
                            return segment.endIndex;
                        }

                        // For other types (method calls, property access), use default
                        console.log(`    ✓ Using default break point at ${segment.endIndex}`);
                        return segment.endIndex;
                    }
                }
            }
        }

        console.log("  × No suitable break point found");
        return -1;
    }

    private isInsideList(segments: ContentSegment[], currentIndex: number): boolean {
        let braceCount = 0;
        let hasCommas = false;
        let hasAssignment = false;

        // First check if this is an assignment with a brace
        for (let i = 0; i < segments.length; i++) {
            const segment = segments[i];
            if (segment.tokenType.type === "operator" && segment.content.includes(":=")) {
                hasAssignment = true;
            }
            if (
                hasAssignment &&
                segment.tokenType.type === "separator" &&
                segment.content === "{"
            ) {
                return true; // Definitely a list if we have := followed by {
            }
        }

        // More detailed list structure check
        for (let i = 0; i <= currentIndex; i++) {
            const segment = segments[i];
            if (segment.tokenType.type === "separator") {
                if (segment.content === "{") {
                    braceCount++;
                } else if (segment.content === "}") {
                    braceCount--;
                } else if (segment.content === ",") {
                    if (braceCount > 0) {
                        hasCommas = true;
                    }
                }
            }
        }

        return braceCount > 0 && hasCommas;
    }

    private findListBreakPoints(line: ProcessedLine): number[] {
        const breakPoints: number[] = [];
        const segments = line.segments;

        // Find assignment operator and opening brace
        const assignmentIndex = segments.findIndex(
            (seg) => seg.tokenType.type === "operator" && seg.content.includes(":=")
        );
        const openBraceIndex = segments.findIndex(
            (seg, index) => seg.tokenType.type === "separator" && seg.content === "{"
        );

        if (assignmentIndex !== -1) {
            // Break after assignment operator
            breakPoints.push(segments[assignmentIndex].endIndex);
        }

        if (openBraceIndex !== -1) {
            // Break after opening brace
            breakPoints.push(segments[openBraceIndex].endIndex);
        }

        // Break after each comma
        for (let i = openBraceIndex + 1; i < segments.length; i++) {
            const segment = segments[i];
            if (segment.tokenType.type === "separator" && segment.content === ",") {
                breakPoints.push(segment.endIndex);
            }
        }

        // Find and add closing brace position
        const closingBraceIndex = segments.findIndex(
            (seg) => seg.tokenType.type === "separator" && seg.content === "}"
        );
        if (closingBraceIndex !== -1) {
            breakPoints.push(segments[closingBraceIndex].startIndex);
        }

        console.log(`Found ${breakPoints.length} break points in list structure`);
        return breakPoints;
    }

    private breakLine(line: ProcessedLine, breakPoints: number[]): ProcessedLine[] {
        const newLines: ProcessedLine[] = [];
        let startIndex = 0;
        const baseIndent = line.leadingWhitespace;

        // Find the opening string position for alignment
        const stringStart = line.segments.find(
            (seg) => seg.tokenType.type === "stringLiteral" || seg.tokenType.type === "methodCall"
        );

        // Calculate alignment based on the opening string position
        const alignmentColumn = stringStart
            ? baseIndent.length +
              line.originalString.indexOf(stringStart.content) +
              (stringStart.tokenType.type === "methodCall" ? stringStart.content.length : 0)
            : baseIndent.length + 4;

        const continuationIndent = " ".repeat(alignmentColumn);

        console.log(`Alignment info:`);
        console.log(`  Base indent: ${baseIndent.length} spaces`);
        console.log(
            `  String start position: ${
                stringStart ? line.originalString.indexOf(stringStart.content) : "not found"
            }`
        );
        console.log(`  Alignment column: ${alignmentColumn}`);

        const isListContext = this.isInsideList(line.segments, line.segments.length - 1);

        // Adjust list indentation based on context
        const listIndent = isListContext
            ? line.leadingWhitespace + "    ".repeat(2) // Double indent for list items
            : continuationIndent;

        for (let i = 0; i < breakPoints.length; i++) {
            const endIndex = breakPoints[i];
            const content = line.originalString.substring(startIndex, endIndex).trim();
            const segments = this.sliceSegments(line.segments, startIndex, endIndex);

            // Use special indentation for list items
            let indent;
            if (isListContext) {
                if (content.includes(":=")) {
                    indent = baseIndent; // Assignment line
                } else if (content === "{") {
                    indent = baseIndent + "    "; // Opening brace gets single indent
                } else if (content === "}") {
                    indent = baseIndent + "    "; // Closing brace gets single indent
                } else {
                    indent = baseIndent + "    ".repeat(2); // List items get double indent
                }
            } else {
                indent = i === 0 ? baseIndent : continuationIndent;
            }

            const newLine: ProcessedLine = {
                originalString: line.originalString,
                formattedString: indent + content,
                leadingWhitespace: indent,
                trimmedContent: content,
                lineNumber: line.lineNumber,
                originalLineNumber: line.originalLineNumber,
                segments: segments,
            };
            newLines.push(newLine);
            startIndex = endIndex;
        }

        // Handle final segment (which might be the closing brace)
        const finalContent = line.originalString.substring(startIndex).trim();
        let finalIndent;
        if (isListContext) {
            if (finalContent === "}") {
                finalIndent = baseIndent + "    "; // Single indent for closing brace
            } else if (finalContent.endsWith("};")) {
                finalIndent = baseIndent + "    "; // Single indent for closing brace with semicolon
            } else {
                finalIndent = baseIndent + "    ".repeat(2); // Double indent for list items
            }
        } else {
            finalIndent = continuationIndent;
        }

        newLines.push({
            originalString: line.originalString,
            formattedString: finalIndent + finalContent,
            leadingWhitespace: finalIndent,
            trimmedContent: finalContent,
            lineNumber: line.lineNumber,
            originalLineNumber: line.originalLineNumber,
            segments: this.sliceSegments(line.segments, startIndex, line.originalString.length),
        });

        return newLines;
    }

    private sliceSegments(
        segments: ContentSegment[],
        start: number,
        end: number
    ): ContentSegment[] {
        return segments
            .filter((seg) => seg.startIndex < end && seg.endIndex > start)
            .map((seg) => ({
                ...seg,
                startIndex: Math.max(seg.startIndex - start, 0),
                endIndex: Math.min(seg.endIndex - start, end - start),
            }));
    }
}
