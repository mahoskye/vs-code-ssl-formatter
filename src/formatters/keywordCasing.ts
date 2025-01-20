import { CodeFormatter, TypedBlock } from "./formattingPipeline";

export class KeywordCasingFormatter implements CodeFormatter {
    public async format(blocks: TypedBlock[]): Promise<void> {
        for (const block of blocks) {
            if (block.blockType === "comment") {
                continue;
            }

            for (const line of block.lines) {
                let modified = false;
                let newContent = line.originalString;

                line.segments.forEach((segment) => {
                    // Only process keyword tokens
                    if (segment.type === "code" && segment.tokenType?.type === "keyword") {
                        const content = segment.content;
                        if (content.startsWith(":")) {
                            const keywordPart = content.slice(1).toUpperCase();
                            const replacement = `:${keywordPart}`;

                            newContent =
                                newContent.substring(0, segment.startIndex) +
                                replacement +
                                newContent.substring(segment.endIndex);

                            modified = true;
                        }
                    }
                });

                if (modified) {
                    line.originalString = newContent;
                    line.trimmedContent = newContent.trim();
                }
            }
        }
    }
}
