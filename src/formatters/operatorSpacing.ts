import { CodeFormatter, FormatterInsight, TypedBlock, ContentSegment } from "./formattingPipeline";

interface SpacingRule {
    before: boolean;
    after: boolean;
}

interface ProcessedSegment extends ContentSegment {
    isCompound?: boolean;
    originalSegments?: ContentSegment[];
}

export class OperatorSpacingFormatter implements CodeFormatter {
    private insights: FormatterInsight[] = [];

    private readonly spacingRules = new Map<string, SpacingRule>([
        // Keep all the existing spacing rules unchanged
        ["===", { before: true, after: true }],
        ["!==", { before: true, after: true }],
        ["++", { before: false, after: false }],
        ["--", { before: false, after: false }],
        [">=", { before: true, after: true }],
        ["<=", { before: true, after: true }],
        ["==", { before: true, after: true }],
        ["!=", { before: true, after: true }],
        [":=", { before: true, after: true }],
        ["+=", { before: true, after: true }],
        ["-=", { before: true, after: true }],
        ["*=", { before: true, after: true }],
        ["/=", { before: true, after: true }],
        ["&&", { before: true, after: true }],
        ["||", { before: true, after: true }],
        ["+", { before: true, after: true }],
        ["-", { before: true, after: true }],
        ["*", { before: true, after: true }],
        ["/", { before: true, after: true }],
        ["<", { before: true, after: true }],
        [">", { before: true, after: true }],
        ["=", { before: true, after: true }],
        ["&", { before: true, after: true }],
        ["|", { before: true, after: true }],
        ["!", { before: true, after: true }],
        ["~", { before: true, after: true }],
        ["^", { before: true, after: true }],
        ["%", { before: true, after: true }],
        [",", { before: false, after: true }],
        [";", { before: false, after: false }],
        ["(", { before: false, after: false }],
        [")", { before: false, after: false }],
        ["[", { before: false, after: false }],
        ["]", { before: false, after: false }],
        ["{", { before: true, after: false }],
        ["}", { before: false, after: false }],
        [".", { before: false, after: false }],
    ]);

    private readonly compoundOperators = new Set([
        "++",
        "--",
        "+=",
        "-=",
        "*=",
        "/=",
        ">=",
        "<=",
        "==",
        "===",
        "!=",
        "!==",
        "&&",
        "||",
        ":=",
    ]);

    private readonly incrementDecrementOperators = new Set(["++", "--"]);

    public getName(): string {
        return "OperatorSpacingFormatter";
    }

    public getInsights(): FormatterInsight[] {
        return this.insights;
    }

    private combineAdjacentOperators(segments: ContentSegment[]): ProcessedSegment[] {
        const processed: ProcessedSegment[] = [];
        let i = 0;

        while (i < segments.length) {
            const current = segments[i];

            // Handle method calls by splitting the parenthesis
            if (current.tokenType?.type === "methodCall") {
                const content = current.content;
                const methodName = content.slice(0, -1); // Remove the (

                // Add the method name
                processed.push({
                    ...current,
                    content: methodName,
                    endIndex: current.endIndex - 1,
                    tokenType: {
                        ...current.tokenType,
                        type: "methodCall",
                    },
                });

                // Add the opening parenthesis as a separator
                processed.push({
                    ...current,
                    content: "(",
                    startIndex: current.endIndex - 1,
                    endIndex: current.endIndex,
                    tokenType: {
                        type: "separator",
                        breakable: false,
                    },
                });

                i++;
                continue;
            }

            const next = i + 1 < segments.length ? segments[i + 1] : null;

            if (current.tokenType?.type === "operator" && next?.tokenType?.type === "operator") {
                const combined = current.content + next.content;
                const nextIsIdentifier =
                    i + 2 < segments.length && segments[i + 2].tokenType?.type === "identifier";
                const prevIsIdentifier = i > 0 && segments[i - 1].tokenType?.type === "identifier";

                const isIncrementDecrement = combined === "++" || combined === "--";
                if (
                    this.compoundOperators.has(combined) ||
                    (isIncrementDecrement && (nextIsIdentifier || prevIsIdentifier))
                ) {
                    processed.push({
                        ...current,
                        content: combined,
                        endIndex: next.endIndex,
                        isCompound: true,
                        originalSegments: [current, next],
                        tokenType: {
                            ...current.tokenType,
                            type: "operator",
                            breakable: false,
                        },
                    });
                    i += 2;
                    continue;
                }
            }

            processed.push({ ...current });
            i++;
        }

        return processed;
    }

    private isUnaryOperator(
        segment: ProcessedSegment,
        prevSegment: ProcessedSegment | null,
        nextSegment: ProcessedSegment | null
    ): boolean {
        // Skip increment/decrement operators - they're handled separately
        if (this.incrementDecrementOperators.has(segment.content)) {
            return false;
        }

        if (segment.content !== "+" && segment.content !== "-") {
            return false;
        }

        // Unary if at start or after operator/separator
        if (!prevSegment) {
            return true;
        }

        const prevType = prevSegment.tokenType?.type;
        return (
            prevType === "operator" ||
            prevType === "separator" ||
            (prevType === "keyword" && !prevSegment.content.includes(":"))
        );
    }

    private isLogicalLiteral(segments: ProcessedSegment[], index: number): boolean {
        // Check for .T. or .F. pattern
        if (index + 2 >= segments.length) {
            return false;
        }

        const current = segments[index];
        const next = segments[index + 1];
        const afterNext = segments[index + 2];

        return (
            current.content === "." &&
            (next.content === "T" || next.content === "F") &&
            afterNext.content === "."
        );
    }

    private isLogicalOperator(
        segment: ProcessedSegment,
        segments: ProcessedSegment[],
        index: number
    ): boolean {
        if (segment.content !== ".") {
            return false;
        }

        // Look ahead to see if this forms a .T. or .F. pattern
        const next = index + 1 < segments.length ? segments[index + 1] : null;
        const afterNext = index + 2 < segments.length ? segments[index + 2] : null;

        return (
            next?.content === "T" ||
            next?.content === "F" ||
            next?.content === "t" ||
            next?.content === "f"
        );
    }

    private handleLogicalLiteral(segments: ProcessedSegment[], index: number): number {
        // Returns the number of segments to skip
        if (index + 2 >= segments.length) {
            return 0;
        }

        const current = segments[index];
        const next = segments[index + 1];
        const afterNext = segments[index + 2];

        if (
            current.content === "." &&
            (next.content.toUpperCase() === "T" || next.content.toUpperCase() === "F") &&
            afterNext.content === "."
        ) {
            return 2; // Skip the next two segments
        }
        return 0;
    }

    private isFunctionCall(segments: ProcessedSegment[], currentIndex: number): boolean {
        // Find the enclosing method call
        let methodCallStart = -1;
        let methodCallEnd = -1;

        // First look for a methodCall type that contains our current position
        for (let i = 0; i < segments.length; i++) {
            const segment = segments[i];
            if (segment.tokenType?.type === "methodCall") {
                // Check if our current index falls within this method call's range
                if (i <= currentIndex) {
                    methodCallStart = i;
                    // Find matching closing paren
                    for (let j = i + 1; j < segments.length; j++) {
                        if (segments[j].content === ")") {
                            methodCallEnd = j;
                            break;
                        }
                    }
                    break;
                }
            }
        }

        // If we're between the method call start and end, we're in a function call
        return currentIndex >= methodCallStart && currentIndex <= methodCallEnd;
    }

    private getSpacingRule(
        segment: ProcessedSegment,
        prevSegment: ProcessedSegment | null,
        nextSegment: ProcessedSegment | null,
        segments: ProcessedSegment[],
        currentIndex: number
    ): SpacingRule {
        const isMethodCallContext = this.isFunctionCall(segments, currentIndex);

        if (
            prevSegment?.content === ":RETURN" &&
            (segment.content === "{" || segment.content === "}")
        ) {
            if (segment.content === "{") {
                return { before: true, after: false };
            }
            return { before: false, after: false };
        }

        if (isMethodCallContext) {
            if (segment.content === "," || segment.content === ")" || segment.content === "(") {
                return {
                    before: false,
                    after: segment.content === "," ? true : false,
                };
            }
        }

        // Handle increment/decrement operators
        if (this.incrementDecrementOperators.has(segment.content)) {
            const isPreIncDec = nextSegment?.tokenType?.type === "identifier";
            const isPostIncDec = prevSegment?.tokenType?.type === "identifier";

            if (isPreIncDec || isPostIncDec) {
                return { before: false, after: false };
            }
        }

        // Handle unary operators
        const isUnary = this.isUnaryOperator(segment, prevSegment, nextSegment);
        const rule = this.spacingRules.get(segment.content) || { before: false, after: false };

        if (isUnary) {
            return { before: false, after: false };
        }

        // Handle property access colons
        if (
            segment.content === ":" &&
            prevSegment?.tokenType?.type === "identifier" &&
            nextSegment?.tokenType?.type === "identifier"
        ) {
            return { before: false, after: false };
        }

        // Handle logical literals (.T. and .F.)
        if (segment.content === ".") {
            const isPartOfLogical = this.isLogicalLiteral(segments, currentIndex);
            if (isPartOfLogical) {
                return { before: false, after: false };
            }
        }

        // Handle logical operators (.T. and .F.)
        if (segment.content === ".") {
            if (this.isLogicalOperator(segment, segments, currentIndex)) {
                return { before: true, after: false };
            }
        }

        // Special handling for function calls
        if (prevSegment?.tokenType?.type === "methodCall" && segment.content === "(") {
            return { before: false, after: false };
        }

        // Check if we're inside a function call once and store the result
        const isInsideFunctionCall =
            this.isFunctionCall(segments, currentIndex) ||
            prevSegment?.tokenType?.type === "methodCall";

        // If we're inside a function call (between the parentheses)
        if (isInsideFunctionCall) {
            if (segment.content === "," || segment.content === ")") {
                return { before: false, after: segment.content === "," ? true : false };
            }
        }

        // Handle method calls - this needs to come first
        if (prevSegment?.tokenType?.type === "methodCall") {
            // If this is a parenthesis or part of the method call
            if (segment.content === "(" || segment.tokenType?.type === "separator") {
                return { before: false, after: false };
            }
        }

        // Handle function call parameters - reuse the earlier check
        if (isInsideFunctionCall) {
            if (segment.content === "," || segment.content === ")" || segment.content === "(") {
                return {
                    before: false,
                    after: segment.content === "," ? true : false,
                };
            }
        }

        return rule;
    }

    private processLogicalOperator(segments: ProcessedSegment[], index: number): string {
        // Process .T. or .F. sequence
        const current = segments[index];
        const next = index + 1 < segments.length ? segments[index + 1] : null;
        const afterNext = index + 2 < segments.length ? segments[index + 2] : null;

        if (
            next &&
            afterNext &&
            current.content === "." &&
            (next.content === "T" || next.content === "F") &&
            afterNext.content === "."
        ) {
            return `.${next.content}.`; // Combine without spaces
        }

        return current.content;
    }

    // Rest of the class implementation remains the same
    public async format(blocks: TypedBlock[]): Promise<void> {
        this.insights = [];

        for (const block of blocks) {
            if (block.blockType === "comment" && !this.hasMixedContent(block)) {
                continue;
            }

            for (const line of block.lines) {
                if (!line.trimmedContent) {
                    continue;
                }

                const originalContent = line.trimmedContent;
                let newContent = "";
                let lastWasSpace = true;

                // Process any leading comments first
                let startProcessingFromIndex = 0;
                let commentContent = "";

                for (let i = 0; i < line.segments.length; i++) {
                    if (line.segments[i].tokenType?.type === "comment") {
                        commentContent += line.segments[i].content;
                        startProcessingFromIndex = i + 1;
                    } else {
                        break;
                    }
                }

                if (commentContent) {
                    newContent += commentContent;
                    lastWasSpace = false;
                }

                // Process remaining segments with proper operator handling
                const remainingSegments = line.segments.slice(startProcessingFromIndex);
                const processedSegments = this.combineAdjacentOperators(remainingSegments);

                for (let i = 0; i < processedSegments.length; i++) {
                    const segment = processedSegments[i];
                    const prevSegment = i > 0 ? processedSegments[i - 1] : null;
                    const nextSegment =
                        i < processedSegments.length - 1 ? processedSegments[i + 1] : null;

                    if (
                        segment.tokenType?.type === "operator" ||
                        segment.tokenType?.type === "separator"
                    ) {
                        // Special handling for .T. and .F.
                        if (segment.content === ".") {
                            const logicalOperator = this.processLogicalOperator(
                                processedSegments,
                                i
                            );
                            if (logicalOperator.length > 1) {
                                // If we got a combined .T. or .F.
                                newContent += logicalOperator;
                                i += 2; // Skip the next two segments
                                lastWasSpace = false;
                                continue;
                            }
                        }

                        // Handle increment/decrement operators
                        const isIncDec = this.incrementDecrementOperators.has(segment.content);
                        if (isIncDec) {
                            const isPreIncDec = nextSegment?.tokenType?.type === "identifier";
                            const isPostIncDec = prevSegment?.tokenType?.type === "identifier";
                            if (isPreIncDec || isPostIncDec) {
                                newContent += segment.content;
                                lastWasSpace = false;
                                continue;
                            }
                        }

                        // Normal operator spacing rules
                        const rule = this.getSpacingRule(
                            segment,
                            prevSegment,
                            nextSegment,
                            processedSegments,
                            i
                        );

                        if (rule.before && !lastWasSpace) {
                            newContent += " ";
                        }
                        newContent += segment.content;
                        if (rule.after) {
                            newContent += " ";
                            lastWasSpace = true;
                        } else {
                            lastWasSpace = false;
                        }
                    } else if (segment.tokenType?.type === "keyword") {
                        if (!lastWasSpace && newContent.length > 0) {
                            newContent += " ";
                        }
                        newContent += segment.content;
                        if (
                            nextSegment?.tokenType?.type !== "separator" ||
                            nextSegment.content !== ";"
                        ) {
                            newContent += " ";
                            lastWasSpace = true;
                        } else {
                            lastWasSpace = false;
                        }
                    } else {
                        const previousIsOpenToken =
                            prevSegment?.content === "{" ||
                            prevSegment?.content === "(" ||
                            prevSegment?.content === "[" ||
                            prevSegment?.content === "<";

                        if (!previousIsOpenToken && !lastWasSpace && newContent.length > 0) {
                            newContent += " ";
                        }
                        newContent += segment.content;
                        lastWasSpace = false;
                    }
                }

                newContent = newContent.trimEnd();
                if (newContent !== originalContent) {
                    line.formattedString = line.leadingWhitespace + newContent;
                    this.insights.push({
                        formatterName: this.getName(),
                        sourceLineNumber: line.originalLineNumber,
                        changeType: "spacing",
                        description: "Adjusted operator spacing",
                        before: originalContent,
                        after: newContent,
                    });
                }
            }
        }
    }

    private hasMixedContent(block: TypedBlock): boolean {
        return block.lines.some((line) => {
            const hasComment = line.segments.some((seg) => seg.type === "comment");
            const hasCode = line.segments.some(
                (seg) => seg.type === "code" || seg.type === "string"
            );
            return hasComment && hasCode;
        });
    }
}
