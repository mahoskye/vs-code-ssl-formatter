import { CodeFormatter, FormatterInsight, TypedBlock } from "./formattingPipeline";

export class KeywordCasingFormatter implements CodeFormatter {
    private insights: FormatterInsight[] = [];

    public getName(): string {
        return "KeywordCasing";
    }

    public getInsights(): FormatterInsight[] {
        return this.insights;
    }

    public async format(blocks: TypedBlock[]): Promise<void> {
        this.insights = [];

        for (const block of blocks) {
            if (block.blockType === "comment") {
                continue;
            }

            for (const line of block.lines) {
                // 🔴 Breakpoint 1: Start of line processing
                console.log("\nProcessing line:", line.trimmedContent);
                console.log(
                    "Original segments:",
                    line.segments.map((s) => ({
                        content: s.content,
                        type: s.tokenType?.type,
                        start: s.startIndex,
                        end: s.endIndex,
                    }))
                );

                let modified = false;
                const originalContent = line.trimmedContent;

                // Process each keyword segment and preserve other segments exactly
                for (const segment of line.segments) {
                    if (segment.type === "code" && segment.tokenType?.type === "keyword") {
                        const content = segment.content;
                        if (content.startsWith(":")) {
                            console.log("Processing keyword:", {
                                original: content,
                                keywordPart: content.slice(1).toUpperCase(),
                                replacement: `:${content.slice(1).toUpperCase()}`,
                            });
                            const keywordPart = content.slice(1).toUpperCase();
                            const replacement = `:${keywordPart}`;

                            if (replacement !== content) {
                                segment.content = replacement;
                                modified = true;
                            }
                        }
                    }
                }

                if (modified) {
                    // Reconstruct the line exactly as it was, only modifying keyword segments
                    let formattedContent = "";
                    let currentPos = 0;

                    console.log("\nReconstructing line:");
                    for (const segment of line.segments) {
                        const gapSize = segment.startIndex - currentPos;
                        console.log("Segment:", {
                            content: segment.content,
                            type: segment.tokenType?.type,
                            start: segment.startIndex,
                            end: segment.endIndex,
                            gapSize,
                            currentPos,
                        });

                        if (gapSize > 0) {
                            formattedContent += " ".repeat(gapSize);
                        }

                        formattedContent += segment.content;
                        currentPos = segment.endIndex;
                    }

                    console.log("Line reconstruction result:", {
                        before: originalContent,
                        after: formattedContent,
                    });

                    line.formattedString = line.leadingWhitespace + formattedContent;
                    line.trimmedContent = formattedContent;

                    this.insights.push({
                        formatterName: this.getName(),
                        sourceLineNumber: line.lineNumber,
                        changeType: "casing",
                        description: "Converted keyword to uppercase",
                        before: originalContent,
                        after: formattedContent,
                    });
                }
            }
        }
    }
}
