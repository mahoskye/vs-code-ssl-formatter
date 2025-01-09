/**
 * Handles line spacing formatting for STARLIMS code files
 */

// Types and interfaces for the formatter
export type BlockType =
    | "procedure"
    | "conditional"
    | "loop"
    | "switch"
    | "errorHandling"
    | "comment"
    | "declaration"
    | "logic";

type FlowKeyword =
    | "procedureStart"
    | "procedureEnd"
    | "ifStart"
    | "ifEnd"
    | "else"
    | "whileStart"
    | "whileEnd"
    | "exitWhile"
    | "forStart"
    | "forEnd"
    | "loop"
    | "caseStart"
    | "caseBranch"
    | "caseDefault"
    | "caseEnd"
    | "exitCase"
    | "tryStart"
    | "catch"
    | "finally"
    | "tryEnd"
    | "error"
    | "resume"
    | "multiLineOpen"
    | "multiLineClose"
    | "other";

interface ProcessedLine {
    originalString: string;
    leadingWhitespace: string;
    trimmedContent: string;
    lineNumber: number;
}

interface BlockContext {
    parentBlock?: TypedBlock;
    isPartOfChain: boolean;
    parentBlockType: BlockType | null;
}

interface BlockMetadata {
    flowType: FlowKeyword;
    isStart: boolean;
    isMiddle: boolean;
    isEnd: boolean;
    isCaseContent?: boolean;
}

interface TypedBlock {
    lines: ProcessedLine[];
    followingSpaces: number;
    originalSpaces: number;
    nextBlockFirstLine?: ProcessedLine;
    blockType: BlockType;
    metadata: BlockMetadata;
    context: BlockContext;
    startLineNumber: number;
    endLineNumber: number;
}

// Pattern definitions
const patterns = {
    flow: {
        procedure: {
            start: /:(?:CLASS|PROCEDURE)\b/i,
            end: /:RETURN\b/i,
        },
        conditional: {
            ifStart: /:IF\b/i,
            ifEnd: /:ENDIF\b/i,
            else: /:ELSE\b/i,
        },
        loop: {
            whileStart: /:WHILE\b/i,
            whileEnd: /:ENDWHILE\b/i,
            exitWhile: /:EXITWHILE\b/i,
            forStart: /:FOR\b/i,
            forEnd: /:NEXT\b/i,
        },
        switch: {
            start: /:BEGINCASE\b/i,
            branch: /:CASE\b/i,
            default: /:OTHERWISE\b/i,
            end: /:ENDCASE\b/i,
            exit: /:EXITCASE\b/i,
        },
        errorHandling: {
            tryStart: /:TRY\b/i,
            catch: /:CATCH\b/i,
            finally: /:FINALLY\b/i,
            tryEnd: /:ENDTRY\b/i,
            error: /:ERROR\b/i,
            resume: /:RESUME\b/i,
        },
    },
    declaration: {
        types: {
            include: /:INCLUDE\b/i,
            declare: /:DECLARE\b/i,
            public: /:PUBLIC\b/i,
            parameters: /:PARAMETERS\b/i,
            assignment: /:=/,
            default: /:DEFAULT\b/i,
        },
        group: /^:(?:PARAMETERS|DEFAULT|DECLARE)\b/i,
    },
    comment: {
        block: /^\/\*\*/,
        single: /^\/\*/,
        region: {
            start: /^\/\*\s*region\b/i,
            end: /^\/\*\s*endregion\b/i,
        },
    },
    structure: {
        semicolon: /;/,
        multiLine: /[^;]\s*$/,
        blankLine: /^\s*$/,
        newLine: /\r?\n/,
    },
};

// Block processor class
class BlockProcessor {
    private blocks: TypedBlock[] = [];
    private debug: boolean = false;

    constructor(private text: string) {}

    public process(): string {
        this.blocks = this.splitIntoBlocks();
        this.updateBlockRelationships();
        this.detectChains();
        this.applySpacingRules();
        return this.debug ? this.formatDebug() : this.format();
    }

    private splitIntoBlocks(): TypedBlock[] {
        const lines = this.text.split(patterns.structure.newLine);
        const blocks: TypedBlock[] = [];
        let currentBlock: ProcessedLine[] = [];
        let isBlockComment = false;

        for (let i = 0; i < lines.length; i++) {
            const line = this.processLine(lines[i], i);

            // Skip empty lines at the start of file
            if (currentBlock.length === 0 && line.trimmedContent === "") {
                continue;
            }

            currentBlock.push(line);

            if (patterns.comment.block.test(line.trimmedContent)) {
                isBlockComment = true;
            }

            // Check for block end or end of file
            if (this.isBlockEnd(line, isBlockComment) || i === lines.length - 1) {
                const block = this.createBlock(currentBlock, lines, i);

                // Check if this block should be merged with the previous one
                if (blocks.length > 0 && this.shouldMergeBlocks(blocks[blocks.length - 1], block)) {
                    const prevBlock = blocks[blocks.length - 1];
                    this.mergeBlocks(prevBlock, block);
                } else {
                    blocks.push(block);
                }

                currentBlock = [];
                isBlockComment = false;
            }
        }

        if (this.debug) {
            this.validateBlocks(blocks);
        }

        return blocks;
    }

    private processLine(line: string, lineIndex: number): ProcessedLine {
        const trimmed = line.trim();
        const leadingWhitespace = trimmed === "" ? "" : line.substring(0, line.length - line.trimStart().length);

        return {
            originalString: line,
            leadingWhitespace,
            trimmedContent: trimmed,
            lineNumber: lineIndex + 1, // Convert to 1-based line numbers
        };
    }

    private isBlockEnd(line: ProcessedLine, isBlockComment: boolean): boolean {
        return (
            (!isBlockComment && patterns.structure.semicolon.test(line.trimmedContent)) ||
            (isBlockComment && patterns.structure.semicolon.test(line.trimmedContent))
        );
    }

    private createBlock(lines: ProcessedLine[], allLines: string[], currentIndex: number): TypedBlock {
        const startLine = lines[0].lineNumber;
        const endLine = lines[lines.length - 1].lineNumber;

        const blankCount = this.countFollowingBlankLines(allLines, currentIndex);
        const nextLine = this.getNextContentLine(allLines, currentIndex + blankCount + 1);

        const blockIdentification = BlockIdentifier.identify(lines);

        const block = {
            lines,
            originalSpaces: blankCount,
            followingSpaces: Math.min(blankCount, 2),
            nextBlockFirstLine: nextLine,
            ...blockIdentification,
            context: {
                isPartOfChain: false,
                parentBlockType: blockIdentification.blockType,
            },
            startLineNumber: startLine,
            endLineNumber: endLine,
        };

        if (this.debug) {
            console.log(
                `Created block: ${startLine}-${endLine}, type=${blockIdentification.blockType}, originalSpaces=${blankCount}`
            );
        }

        return block;
    }

    private countFollowingBlankLines(lines: string[], currentIndex: number): number {
        let count = 0;
        let index = currentIndex + 1;
        while (index < lines.length && patterns.structure.blankLine.test(lines[index])) {
            count++;
            index++;
        }
        return count;
    }

    private getNextContentLine(lines: string[], index: number): ProcessedLine | undefined {
        while (index < lines.length) {
            if (!patterns.structure.blankLine.test(lines[index])) {
                return this.processLine(lines[index], index);
            }
            index++;
        }
        return undefined;
    }

    private shouldMergeBlocks(prevBlock: TypedBlock, currentBlock: TypedBlock): boolean {
        // Handle switch case statements specifically
        if (prevBlock.blockType === "switch") {
            // Merge CASE statements with their content
            const isCaseStatement =
                prevBlock.metadata.flowType === "caseBranch" || prevBlock.metadata.flowType === "caseDefault";

            if (isCaseStatement) {
                // Merge with any content that follows the case statement
                if (!["caseStart", "caseEnd", "caseBranch", "caseDefault"].includes(currentBlock.metadata.flowType)) {
                    if (this.debug) {
                        console.log(`Merging case content with case statement`);
                    }
                    return true;
                }
            }

            // Always merge consecutive case branches
            if (currentBlock.blockType === "switch") {
                if (this.debug) {
                    console.log(`Merging consecutive case branches`);
                }
                return true;
            }
        }

        // Merge declarations that should be grouped
        if (prevBlock.blockType === "declaration" && currentBlock.blockType === "declaration") {
            const currentLine = prevBlock.lines[0].trimmedContent;
            const nextLine = currentBlock.lines[0].trimmedContent;
            const declarationTypes = patterns.declaration.group;

            const shouldMergeDeclarations =
                declarationTypes.test(currentLine) &&
                declarationTypes.test(nextLine) &&
                currentLine.substring(1, currentLine.indexOf(" ")) === nextLine.substring(1, nextLine.indexOf(" "));

            const shouldMergeAssignments =
                patterns.declaration.types.assignment.test(currentLine) &&
                patterns.declaration.types.assignment.test(nextLine);

            if ((shouldMergeDeclarations || shouldMergeAssignments) && this.debug) {
                console.log(`Merging declarations: ${currentLine} with ${nextLine}`);
            }

            return shouldMergeDeclarations || shouldMergeAssignments;
        }

        // Handle control flow blocks
        if (
            ["conditional", "loop", "switch", "errorHandling"].includes(prevBlock.blockType) &&
            prevBlock.blockType === currentBlock.blockType
        ) {
            const shouldMerge =
                prevBlock.metadata.flowType === currentBlock.metadata.flowType || currentBlock.context.isPartOfChain;

            if (shouldMerge && this.debug) {
                console.log(`Merging control flow blocks`);
            }

            return shouldMerge;
        }

        return false;
    }

    private mergeBlocks(target: TypedBlock, source: TypedBlock): void {
        target.lines.push(...source.lines);
        target.endLineNumber = source.endLineNumber;
        target.nextBlockFirstLine = source.nextBlockFirstLine;
        target.originalSpaces = source.originalSpaces;
        target.followingSpaces = source.followingSpaces;

        if (target.blockType === "switch" && source.metadata.isCaseContent) {
            target.metadata = {
                ...target.metadata,
                isCaseContent: true,
            };
        }

        if (this.debug) {
            console.log(`Merged blocks: ${target.startLineNumber}-${target.endLineNumber}`);
        }
    }

    private validateBlocks(blocks: TypedBlock[]): void {
        for (let i = 0; i < blocks.length - 1; i++) {
            const currentBlock = blocks[i];
            const nextBlock = blocks[i + 1];

            if (
                currentBlock.nextBlockFirstLine &&
                currentBlock.nextBlockFirstLine.lineNumber !== nextBlock.lines[0].lineNumber
            ) {
                console.warn(
                    `Block transition mismatch at line ${currentBlock.endLineNumber}:\n` +
                        `Expected next line: ${currentBlock.nextBlockFirstLine.lineNumber}\n` +
                        `Actual next line: ${nextBlock.lines[0].lineNumber}`
                );
            }

            const expectedNextLine = currentBlock.endLineNumber + currentBlock.followingSpaces + 1;
            if (nextBlock.startLineNumber > expectedNextLine) {
                console.warn(
                    `Line number gap detected between blocks:\n` +
                        `Block ${i} ends at line ${currentBlock.endLineNumber}\n` +
                        `Block ${i + 1} starts at line ${nextBlock.startLineNumber}\n` +
                        `Gap: ${nextBlock.startLineNumber - expectedNextLine} lines`
                );
            }
        }
    }

    private updateBlockRelationships(): void {
        this.blocks.forEach((block, index) => {
            if (index > 0) {
                const prevBlock = this.blocks[index - 1];
                if (prevBlock.metadata.isStart) {
                    block.context.parentBlock = prevBlock;
                    block.context.parentBlockType = prevBlock.blockType;
                }
            }
        });
    }

    private detectChains(): void {
        this.blocks.forEach((block, index) => {
            if (index > 0) {
                const prevBlock = this.blocks[index - 1];
                block.context.isPartOfChain = this.isChainable(prevBlock, block);
            }
        });
    }

    private isChainable(prev: TypedBlock, current: TypedBlock): boolean {
        // Handle if/else chains
        if (prev.blockType === "conditional" && current.blockType === "conditional") {
            return current.metadata.flowType === "else";
        }

        // Handle case chains
        if (prev.blockType === "switch" && current.blockType === "switch") {
            return ["caseBranch", "caseDefault"].includes(current.metadata.flowType);
        }

        return false;
    }

    private applySpacingRules(): void {
        this.blocks.forEach((block, index) => {
            if (index < this.blocks.length - 1) {
                block.followingSpaces = SpacingRulesEngine.determineSpacing(block, this.blocks[index + 1]);
            }
        });
    }

    private format(): string {
        let result = "";

        this.blocks.forEach((block, index) => {
            // Add each line exactly as it was in the original file
            block.lines.forEach((line, lineIndex) => {
                result += line.originalString;
                if (!(index === this.blocks.length - 1 && lineIndex === block.lines.length - 1)) {
                    result += "\n";
                }
            });

            // Add block spacing only between blocks
            if (index < this.blocks.length - 1) {
                result += "\n".repeat(block.followingSpaces);
            }
        });

        // Always ensure exactly one trailing newline
        return result.trimEnd() + "\n";
    }

    private formatDebug(): string {
        return (
            this.blocks
                .map((block, index) => {
                    const firstLine = block.lines[0].trimmedContent;
                    const nextLine = block.nextBlockFirstLine?.trimmedContent || "END";
                    const truncateLength = 30;

                    return (
                        `Block ${index + 1}: ` +
                        `lines=${block.startLineNumber}-${block.endLineNumber} | ` +
                        `type=${block.blockType} | ` +
                        `spaces=${block.followingSpaces} | ` +
                        `originalSpaces=${block.originalSpaces} | ` +
                        `first=${firstLine.substring(0, truncateLength)}${
                            firstLine.length > truncateLength ? "..." : ""
                        } | ` +
                        `next=${nextLine.substring(0, truncateLength)}${nextLine.length > truncateLength ? "..." : ""}`
                    );
                })
                .join("\n") + "\n"
        );
    }
}

// Block identifier class
class BlockIdentifier {
    static identify(lines: ProcessedLine[]): { blockType: BlockType; metadata: BlockMetadata } {
        const firstLine = lines[0].trimmedContent;

        // Try each identifier in order of specificity
        return (
            this.identifyComment(firstLine) ||
            this.identifyProcedure(lines) ||
            this.identifyControlFlow(firstLine) ||
            this.identifyDeclaration(firstLine) ||
            this.createLogicBlock(firstLine)
        );
    }

    private static identifyProcedure(lines: ProcessedLine[]): { blockType: BlockType; metadata: BlockMetadata } | null {
        const firstLine = lines[0].trimmedContent;

        if (patterns.flow.procedure.start.test(firstLine)) {
            return {
                blockType: "procedure",
                metadata: {
                    flowType: "procedureStart",
                    isStart: true,
                    isMiddle: false,
                    isEnd: false,
                },
            };
        }

        if (patterns.flow.procedure.end.test(firstLine)) {
            return {
                blockType: "procedure",
                metadata: {
                    flowType: "procedureEnd",
                    isStart: false,
                    isMiddle: false,
                    isEnd: true,
                },
            };
        }

        return null;
    }

    private static identifyDeclaration(line: string): { blockType: BlockType; metadata: BlockMetadata } | null {
        for (const [key, pattern] of Object.entries(patterns.declaration.types)) {
            if (pattern.test(line)) {
                return {
                    blockType: "declaration",
                    metadata: {
                        flowType: "other",
                        isStart: false,
                        isMiddle: false,
                        isEnd: false,
                    },
                };
            }
        }
        return null;
    }

    private static identifyComment(line: string): { blockType: BlockType; metadata: BlockMetadata } | null {
        if (
            patterns.comment.block.test(line) ||
            patterns.comment.single.test(line) ||
            patterns.comment.region.start.test(line) ||
            patterns.comment.region.end.test(line)
        ) {
            return {
                blockType: "comment",
                metadata: {
                    flowType: "other",
                    isStart: false,
                    isMiddle: false,
                    isEnd: false,
                },
            };
        }
        return null;
    }

    private static identifyControlFlow(line: string): { blockType: BlockType; metadata: BlockMetadata } | null {
        // Check conditional flow
        for (const [key, pattern] of Object.entries(patterns.flow.conditional)) {
            if (pattern.test(line)) {
                return {
                    blockType: "conditional",
                    metadata: {
                        flowType: key as FlowKeyword,
                        isStart: key === "ifStart",
                        isMiddle: key === "else",
                        isEnd: key === "ifEnd",
                    },
                };
            }
        }

        // Check loop flow
        for (const [key, pattern] of Object.entries(patterns.flow.loop)) {
            if (pattern.test(line)) {
                return {
                    blockType: "loop",
                    metadata: {
                        flowType: key as FlowKeyword,
                        isStart: key.endsWith("Start"),
                        isMiddle: key === "exitWhile" || key === "loop",
                        isEnd: key.endsWith("End"),
                    },
                };
            }
        }

        // Check switch flow
        for (const [key, pattern] of Object.entries(patterns.flow.switch)) {
            if (pattern.test(line)) {
                const isCase = key === "branch" || key === "default" || key === "exit";
                const isCaseContent =
                    !patterns.flow.switch.branch.test(line) &&
                    !patterns.flow.switch.default.test(line) &&
                    !patterns.flow.switch.start.test(line) &&
                    !patterns.flow.switch.end.test(line);
                return {
                    blockType: "switch",
                    metadata: {
                        flowType: key as FlowKeyword,
                        isStart: key === "start",
                        isMiddle: isCase,
                        isEnd: key === "end",
                        isCaseContent: isCaseContent,
                    },
                };
            }
        }

        // Check error handling flow
        for (const [key, pattern] of Object.entries(patterns.flow.errorHandling)) {
            if (pattern.test(line)) {
                return {
                    blockType: "errorHandling",
                    metadata: {
                        flowType: key as FlowKeyword,
                        isStart: key === "tryStart",
                        isMiddle: ["catch", "finally"].includes(key),
                        isEnd: key === "tryEnd",
                    },
                };
            }
        }

        return null;
    }

    private static createLogicBlock(line: string): { blockType: BlockType; metadata: BlockMetadata } {
        return {
            blockType: "logic",
            metadata: {
                flowType: "other",
                isStart: false,
                isMiddle: false,
                isEnd: false,
            },
        };
    }
}

// Spacing rules engine
class SpacingRulesEngine {
    static determineSpacing(block: TypedBlock, nextBlock: TypedBlock): number {
        // Apply rules in order of precedence
        return (
            this.applyProcedureRules(block, nextBlock) ??
            this.applyDeclarationRules(block, nextBlock) ??
            this.applyControlFlowRules(block, nextBlock) ??
            this.applyCommentRules(block, nextBlock) ??
            this.applyLogicRules(block, nextBlock) ??
            1
        ); // Default spacing
    }

    private static applyProcedureRules(block: TypedBlock, nextBlock: TypedBlock): number | null {
        if (block.blockType === "procedure") {
            if (block.metadata.isStart) {
                // No space between procedure start and its declarations
                return nextBlock.blockType === "declaration" ? 0 : 1;
            }

            if (block.metadata.isEnd) {
                return 2; // Double space after procedure end
            }
        }
        return null;
    }

    private static applyDeclarationRules(block: TypedBlock, nextBlock: TypedBlock): number | null {
        if (block.blockType === "declaration") {
            // Group declarations of the same type together
            const currentLine = block.lines[0].trimmedContent;
            const nextLine = nextBlock.lines[0].trimmedContent;

            if (
                nextBlock.blockType === "declaration" &&
                patterns.declaration.group.test(currentLine) &&
                patterns.declaration.group.test(nextLine) &&
                currentLine.substring(1, currentLine.indexOf(" ")) === nextLine.substring(1, nextLine.indexOf(" "))
            ) {
                return 0;
            }

            return 1;
        }
        return null;
    }

    private static applyControlFlowRules(block: TypedBlock, nextBlock: TypedBlock): number | null {
        // Handle control flow blocks (if, while, switch, etc.)
        if (["conditional", "loop", "switch", "errorHandling"].includes(block.blockType)) {
            // No space for chained blocks or related blocks
            if (
                nextBlock.context.isPartOfChain ||
                (block.metadata.flowType === nextBlock.metadata.flowType && !block.metadata.isEnd)
            ) {
                return 0;
            }

            // One space after end of control structure unless followed by another control structure
            if (block.metadata.isEnd) {
                return ["conditional", "loop", "switch", "errorHandling"].includes(nextBlock.blockType) ? 0 : 1;
            }

            // No space between control structure elements that are related
            if (block.metadata.isStart && block.metadata.flowType === nextBlock.metadata.flowType) {
                return 0;
            }
        }
        return null;
    }

    private static applyCommentRules(block: TypedBlock, nextBlock: TypedBlock): number | null {
        if (block.blockType === "comment") {
            const commentLine = block.lines[0].trimmedContent;

            // Single space after comments unless part of a group
            if (patterns.comment.block.test(commentLine) || patterns.comment.single.test(commentLine)) {
                return nextBlock.blockType === "comment" ? 0 : 1;
            }
        }
        return null;
    }

    private static applyLogicRules(block: TypedBlock, nextBlock: TypedBlock): number | null {
        if (block.blockType === "logic") {
            // Group related logic statements
            if (nextBlock.blockType === "logic") {
                return 0;
            }
            return 1;
        }
        return null;
    }
}

// Main formatter function
export function lineSpacingFormatter(text: string): string {
    const processor = new BlockProcessor(text);
    return processor.process();
}

// Error types
export class FormatterError extends Error {
    constructor(message: string) {
        super(message);
        this.name = "FormatterError";
    }
}

export class BlockProcessingError extends FormatterError {
    constructor(message: string) {
        super(`Block processing error: ${message}`);
        this.name = "BlockProcessingError";
    }
}

export class PatternMatchError extends FormatterError {
    constructor(message: string) {
        super(`Pattern matching error: ${message}`);
        this.name = "PatternMatchError";
    }
}
