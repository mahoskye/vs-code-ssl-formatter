/**
 * Core formatter pipeline for STARLIMS code
 */

import { start } from "repl";

// Core block and content types
export type BlockType =
    | "procedure"
    | "conditional"
    | "loop"
    | "switch"
    | "errorHandling"
    | "comment"
    | "declaration"
    | "logic";

export type FlowKeyword =
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
    | "assignment"
    | "functionCall"
    | "other";

export type DeclarationType = "parameters" | "declare" | "default" | "public" | "include" | "other";
export type CommentType = "block" | "single" | "regionStart" | "regionEnd" | "separator";

// First we need a type for the different kinds of content
export type ContentType = "code" | "string" | "comment" | "empty";

// Define what a segment looks like
export interface ContentSegment {
    type: ContentType;
    content: string;
    startIndex: number;
    endIndex: number;
    tokenType: TokenType;
    priority?: number; // For determining preferred break points
}

// Update ProcessedLine to include segments
export interface ProcessedLine {
    originalString: string;
    leadingWhitespace: string;
    trimmedContent: string;
    lineNumber: number;
    segments: ContentSegment[]; // Add segments array
}

// Configuration interface
export interface FormatterConfig {
    debug?: boolean;
    useSpacingContext?: boolean;
    useSpacingPostProcessing?: boolean;
    preserveUserSpacing?: boolean;
    maxConsecutiveBlankLines?: number;
    maxLineLength?: number;
    tabSize?: number;
    indentStyle?: "tab" | "space";
    showSegmentDetails?: boolean; // Add this line
}

export const DEFAULT_FORMATTER_CONFIG: FormatterConfig = {
    debug: false,
    useSpacingContext: true,
    useSpacingPostProcessing: true,
    preserveUserSpacing: false,
    maxConsecutiveBlankLines: 2,
    maxLineLength: 90,
    tabSize: 4,
    indentStyle: "space",
    showSegmentDetails: false, // Add this line
};

// Line and block structure interfaces
export interface ProcessedLine {
    originalString: string;
    leadingWhitespace: string;
    trimmedContent: string;
    lineNumber: number;
    segments: ContentSegment[]; // Add segments array
}

export interface BlockContext {
    parentBlock?: TypedBlock;
    previousBlock?: TypedBlock;
    isPartOfChain: boolean;
    parentBlockType: BlockType | null;
    depth: number;
    chainPosition?: "first" | "middle" | "last";
}

export interface BlockMetadata {
    flowType: FlowKeyword;
    isStart: boolean;
    isMiddle: boolean;
    isEnd: boolean;
    isCaseContent?: boolean;
    declarationType?: DeclarationType;
    commentType?: CommentType;
}

export interface TypedBlock {
    lines: ProcessedLine[];
    blockType: BlockType;
    metadata: BlockMetadata;
    context: BlockContext;
    startLineNumber: number;
    endLineNumber: number;
    nextBlockFirstLine?: ProcessedLine;
}

export interface DebugResult {
    tableView: string;
    detailedView: string;
    blocks: TypedBlock[];
}

export type TokenTypeName =
    | "keyword"
    | "separator"
    | "identifier"
    | "operator"
    | "number"
    | "stringLiteral"
    | "comment"
    | "empty"
    | "methodCall" // Add this
    | "propertyAccess"; // Add this

interface TokenType {
    type: TokenTypeName;
    breakable: boolean;
    breakBefore?: boolean;
    breakAfter?: boolean;
    isMethod?: boolean; // Add this
    isProperty?: boolean; // Add this
    isKeyword?: boolean; // Add this
}

// Types for token processing
interface Token {
    type: TokenTypeName;
    content: string;
    startIndex: number;
    endIndex: number;
    breakable: boolean;
    isMethod?: boolean;
    isProperty?: boolean;
    isKeyword?: boolean;
}

interface ProcessingResult {
    token: Token | null;
    newIndex: number;
}

// Map token types to content types
export const TOKEN_TYPE_TO_CONTENT_TYPE: Record<TokenTypeName, ContentType> = {
    keyword: "code",
    separator: "code",
    identifier: "code",
    operator: "code",
    number: "code",
    stringLiteral: "string",
    comment: "comment",
    empty: "empty",
    methodCall: "code",
    propertyAccess: "code",
};

// Define which token types should never be merged
export const NON_MERGEABLE_TYPES: TokenTypeName[] = [
    "keyword",
    "separator",
    "operator",
    "stringLiteral",
    "number",
    "methodCall",
    "propertyAccess",
];

// Configuration for the segment processor
export interface SegmentConfig {
    maxLineLength?: number;
    debug?: boolean;
    preserveComments?: boolean; // New option
}

export const DEFAULT_SEGMENT_CONFIG: SegmentConfig = {
    maxLineLength: 90,
    debug: false,
    preserveComments: true,
};

// Core syntax patterns
export const patterns = {
    flow: {
        procedure: {
            start: /:(?:CLASS|PROCEDURE)\b/i,
            end: /:(?:RETURN|ENDPROC)\b/i,
            endProc: /:ENDPROC\b/i,
            return: /:RETURN\b/i,
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
            default: /:DEFAULT\b/i,
        },
        group: /^:(?:PARAMETERS|DEFAULT|DECLARE)\b/i,
    },
    logic: {
        assignment: /[:+-]=/,
        functionCall: /\w+\s*\(/,
    },
    comment: {
        block: /^\/\*\*/,
        single: /^\/\*/,
        region: {
            start: /^\/\*\s*region\b/i,
            end: /^\/\*\s*endregion\b/i,
        },
        separator: /^\/\*\s*[=*-]{20}/,
    },
    structure: {
        semicolon: /;/,
        multiLine: /[^;]\s*$/,
        blankLine: /^\s*$/,
        newLine: /\r?\n/,
    },
    keyword: {
        // Full keyword includes the colon
        pattern: /:[A-Za-z]+\b/i,
        find: /:[A-Za-z]+\b/gi, // Global version for finding all matches
    },
};

function createDefaultTokenType(type: TokenTypeName): TokenType {
    return {
        type,
        breakable: !NON_MERGEABLE_TYPES.includes(type),
    };
}

// Base formatter error
export class FormatterError extends Error {
    constructor(message: string) {
        super(message);
        this.name = "FormatterError";
    }
}

// Interface that all formatters must implement
export interface CodeFormatter {
    format(blocks: TypedBlock[]): Promise<void>;
}

/**
 * Class responsible for identifying block types and their metadata in code.
 */
export class BlockIdentifier {
    /**
     * Identify block type and metadata from processed lines
     */
    public static identify(lines: ProcessedLine[]): {
        blockType: BlockType;
        metadata: BlockMetadata;
    } {
        const firstLine = lines[0].trimmedContent;

        // Try each identifier in order of specificity
        return (
            this.identifyComment(firstLine) ||
            this.identifyProcedure(firstLine) ||
            this.identifyControlFlow(firstLine) ||
            this.identifyDeclaration(firstLine) ||
            this.identifyLogic(firstLine)
        );
    }

    /**
     * Check if blocks should be merged
     */
    public static shouldMergeWithPrevious(
        prevBlock: TypedBlock,
        currentBlock: TypedBlock
    ): boolean {
        // Handle multi-line statements that are incomplete(no semicolon)
        if (
            patterns.structure.multiLine.test(
                prevBlock.lines[prevBlock.lines.length - 1].trimmedContent
            )
        ) {
            if (currentBlock.blockType === "comment") {
                return false;
            }
            return true;
        }

        // Handle blocks that are explicitly joined (like a statement broken across lines)
        if (this.isExplicitContinuation(currentBlock.lines[0].trimmedContent)) {
            return true;
        }

        return false;
    }

    /**
     * Check if line continues a previous statement
     */
    private static isExplicitContinuation(line: string): boolean {
        // Don't consider comments as continuations
        if (line.trimStart().startsWith("/*")) {
            return false;
        }

        // Check for lines that are clear continuations
        const continuationPattern = /^[\s]*(?:[+\-*/%,.]|(?:and|or|&&|\|\|))\s/i;
        return continuationPattern.test(line);
    }

    private static identifyProcedure(
        line: string
    ): { blockType: BlockType; metadata: BlockMetadata } | null {
        if (patterns.flow.procedure.start.test(line)) {
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

        if (patterns.flow.procedure.end.test(line)) {
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

    private static identifyDeclaration(
        line: string
    ): { blockType: BlockType; metadata: BlockMetadata } | null {
        for (const [key, pattern] of Object.entries(patterns.declaration.types)) {
            if (pattern.test(line)) {
                return {
                    blockType: "declaration",
                    metadata: {
                        flowType: "other",
                        isStart: false,
                        isMiddle: true,
                        isEnd: false,
                        declarationType: key as DeclarationType,
                    },
                };
            }
        }
        return null;
    }

    private static identifyComment(
        line: string
    ): { blockType: BlockType; metadata: BlockMetadata } | null {
        if (patterns.comment.region.start.test(line)) {
            return {
                blockType: "comment",
                metadata: {
                    flowType: "other",
                    isStart: true,
                    isMiddle: false,
                    isEnd: false,
                    commentType: "regionStart",
                },
            };
        }

        if (patterns.comment.region.end.test(line)) {
            return {
                blockType: "comment",
                metadata: {
                    flowType: "other",
                    isStart: false,
                    isMiddle: false,
                    isEnd: true,
                    commentType: "regionEnd",
                },
            };
        }

        if (patterns.comment.separator.test(line) && patterns.structure.semicolon.test(line)) {
            return {
                blockType: "comment",
                metadata: {
                    flowType: "other",
                    isStart: false,
                    isMiddle: true,
                    isEnd: false,
                    commentType: "separator",
                },
            };
        }

        if (patterns.comment.block.test(line)) {
            return {
                blockType: "comment",
                metadata: {
                    flowType: "other",
                    isStart: false,
                    isMiddle: true,
                    isEnd: false,
                    commentType: "block",
                },
            };
        }

        if (patterns.comment.single.test(line)) {
            return {
                blockType: "comment",
                metadata: {
                    flowType: "other",
                    isStart: false,
                    isMiddle: true,
                    isEnd: false,
                    commentType: "single",
                },
            };
        }

        return null;
    }

    private static identifyControlFlow(
        line: string
    ): { blockType: BlockType; metadata: BlockMetadata } | null {
        const conditional = this.identifyConditional(line);
        if (conditional) {
            return conditional;
        }

        const loop = this.identifyLoop(line);
        if (loop) {
            return loop;
        }

        const switchBlock = this.identifySwitch(line);
        if (switchBlock) {
            return switchBlock;
        }

        const errorHandling = this.identifyErrorHandling(line);
        if (errorHandling) {
            return errorHandling;
        }

        return null;
    }

    private static identifyConditional(
        line: string
    ): { blockType: BlockType; metadata: BlockMetadata } | null {
        if (patterns.flow.conditional.ifStart.test(line)) {
            return {
                blockType: "conditional",
                metadata: {
                    flowType: "ifStart",
                    isStart: true,
                    isMiddle: false,
                    isEnd: false,
                },
            };
        }

        if (patterns.flow.conditional.ifEnd.test(line)) {
            return {
                blockType: "conditional",
                metadata: {
                    flowType: "ifEnd",
                    isStart: false,
                    isMiddle: false,
                    isEnd: true,
                },
            };
        }

        if (patterns.flow.conditional.else.test(line)) {
            return {
                blockType: "conditional",
                metadata: {
                    flowType: "else",
                    isStart: false,
                    isMiddle: true,
                    isEnd: false,
                },
            };
        }

        return null;
    }

    private static identifyLoop(
        line: string
    ): { blockType: BlockType; metadata: BlockMetadata } | null {
        if (patterns.flow.loop.whileStart.test(line)) {
            return {
                blockType: "loop",
                metadata: {
                    flowType: "whileStart",
                    isStart: true,
                    isMiddle: false,
                    isEnd: false,
                },
            };
        }

        if (patterns.flow.loop.whileEnd.test(line)) {
            return {
                blockType: "loop",
                metadata: {
                    flowType: "whileEnd",
                    isStart: false,
                    isMiddle: false,
                    isEnd: true,
                },
            };
        }

        if (patterns.flow.loop.exitWhile.test(line)) {
            return {
                blockType: "loop",
                metadata: {
                    flowType: "exitWhile",
                    isStart: false,
                    isMiddle: true,
                    isEnd: false,
                },
            };
        }

        if (patterns.flow.loop.forStart.test(line)) {
            return {
                blockType: "loop",
                metadata: {
                    flowType: "forStart",
                    isStart: true,
                    isMiddle: false,
                    isEnd: false,
                },
            };
        }

        if (patterns.flow.loop.forEnd.test(line)) {
            return {
                blockType: "loop",
                metadata: {
                    flowType: "forEnd",
                    isStart: false,
                    isMiddle: false,
                    isEnd: true,
                },
            };
        }

        return null;
    }

    private static identifySwitch(
        line: string
    ): { blockType: BlockType; metadata: BlockMetadata } | null {
        if (patterns.flow.switch.start.test(line)) {
            return {
                blockType: "switch",
                metadata: {
                    flowType: "caseStart",
                    isStart: true,
                    isMiddle: false,
                    isEnd: false,
                },
            };
        }

        if (patterns.flow.switch.branch.test(line)) {
            return {
                blockType: "switch",
                metadata: {
                    flowType: "caseBranch",
                    isStart: false,
                    isMiddle: true,
                    isEnd: false,
                },
            };
        }

        if (patterns.flow.switch.default.test(line)) {
            return {
                blockType: "switch",
                metadata: {
                    flowType: "caseDefault",
                    isStart: false,
                    isMiddle: true,
                    isEnd: false,
                },
            };
        }

        if (patterns.flow.switch.end.test(line)) {
            return {
                blockType: "switch",
                metadata: {
                    flowType: "caseEnd",
                    isStart: false,
                    isMiddle: false,
                    isEnd: true,
                },
            };
        }

        if (patterns.flow.switch.exit.test(line)) {
            return {
                blockType: "switch",
                metadata: {
                    flowType: "exitCase",
                    isStart: false,
                    isMiddle: true,
                    isEnd: false,
                },
            };
        }

        return null;
    }

    private static identifyErrorHandling(
        line: string
    ): { blockType: BlockType; metadata: BlockMetadata } | null {
        if (patterns.flow.errorHandling.tryStart.test(line)) {
            return {
                blockType: "errorHandling",
                metadata: {
                    flowType: "tryStart",
                    isStart: true,
                    isMiddle: false,
                    isEnd: false,
                },
            };
        }

        if (patterns.flow.errorHandling.catch.test(line)) {
            return {
                blockType: "errorHandling",
                metadata: {
                    flowType: "catch",
                    isStart: false,
                    isMiddle: true,
                    isEnd: false,
                },
            };
        }

        if (patterns.flow.errorHandling.finally.test(line)) {
            return {
                blockType: "errorHandling",
                metadata: {
                    flowType: "finally",
                    isStart: false,
                    isMiddle: true,
                    isEnd: false,
                },
            };
        }

        if (patterns.flow.errorHandling.tryEnd.test(line)) {
            return {
                blockType: "errorHandling",
                metadata: {
                    flowType: "tryEnd",
                    isStart: false,
                    isMiddle: false,
                    isEnd: true,
                },
            };
        }

        if (patterns.flow.errorHandling.error.test(line)) {
            return {
                blockType: "errorHandling",
                metadata: {
                    flowType: "error",
                    isStart: false,
                    isMiddle: true,
                    isEnd: false,
                },
            };
        }

        if (patterns.flow.errorHandling.resume.test(line)) {
            return {
                blockType: "errorHandling",
                metadata: {
                    flowType: "resume",
                    isStart: false,
                    isMiddle: true,
                    isEnd: false,
                },
            };
        }

        return null;
    }

    private static identifyLogic(line: string): { blockType: BlockType; metadata: BlockMetadata } {
        // Check if it's an assignment
        if (patterns.logic.assignment.test(line)) {
            return {
                blockType: "logic",
                metadata: {
                    flowType: "assignment",
                    isStart: false,
                    isMiddle: true,
                    isEnd: false,
                },
            };
        }

        // Check if it's a function call
        if (patterns.logic.functionCall.test(line)) {
            return {
                blockType: "logic",
                metadata: {
                    flowType: "functionCall",
                    isStart: false,
                    isMiddle: true,
                    isEnd: false,
                },
            };
        }

        // Default logic block
        return {
            blockType: "logic",
            metadata: {
                flowType: "other",
                isStart: false,
                isMiddle: true,
                isEnd: false,
            },
        };
    }
}
/**
 * Handles processing of code blocks for formatting
 */
interface CommentState {
    inBlockComment: boolean;
    blockCommentDepth: number;
}

export class BlockProcessor {
    private segmentProcessor: SegmentProcessor;
    private commentState: CommentState;

    constructor(private config: FormatterConfig) {
        // Convert formatter config to segment config
        const segmentConfig: SegmentConfig = {
            maxLineLength: this.config.maxLineLength,
            debug: this.config.debug,
            preserveComments: true,
        };

        this.segmentProcessor = new SegmentProcessor(segmentConfig);
        this.commentState = {
            inBlockComment: false,
            blockCommentDepth: 0,
        };
    }

    /**
     * Process input text into blocks
     */
    public processText(text: string): TypedBlock[] {
        try {
            return this.splitIntoBlocks(text);
        } catch (error) {
            throw new FormatterError(
                `Error processing blocks: ${error instanceof Error ? error.message : String(error)}`
            );
        }
    }

    /**
     * Convert blocks back to text
     */
    public blocksToText(blocks: TypedBlock[]): string {
        let result = "";

        blocks.forEach((block, index) => {
            // Add each line
            block.lines.forEach((line, lineIndex) => {
                result += line.originalString;
                if (!(index === blocks.length - 1 && lineIndex === block.lines.length - 1)) {
                    result += "\n";
                }
            });
        });

        // Ensure exactly one trailing newline
        return result.trimEnd() + "\n";
    }

    /**
     * Split input text into logical blocks
     */
    private splitIntoBlocks(text: string): TypedBlock[] {
        const lines = text.split(patterns.structure.newLine);
        const blocks: TypedBlock[] = [];
        let currentBlock: ProcessedLine[] = [];
        let isMultiLineStatement = false;

        for (let i = 0; i < lines.length; i++) {
            const line = this.processLine(lines[i], i);

            // Skip empty lines at start of file
            if (currentBlock.length === 0 && line.trimmedContent === "") {
                continue;
            }

            // Handle multi-line statements
            if (patterns.structure.multiLine.test(line.trimmedContent)) {
                isMultiLineStatement = true;
            }

            currentBlock.push(line);

            // Check for block end
            if (this.isBlockEnd(line)) {
                const block = this.createBlock(currentBlock, lines, i);

                // Check if this block should be merged with previous
                if (
                    blocks.length > 0 &&
                    BlockIdentifier.shouldMergeWithPrevious(blocks[blocks.length - 1], block)
                ) {
                    this.mergeBlocks(blocks[blocks.length - 1], block);
                } else {
                    blocks.push(block);
                }

                currentBlock = [];
                isMultiLineStatement = false;
            }
        }

        // Handle any remaining lines
        if (currentBlock.length > 0) {
            const block = this.createBlock(currentBlock, lines, lines.length - 1);
            blocks.push(block);
        }

        return blocks;
    }

    /**
     * Process a single line of code
     */
    private processLine(line: string, lineIndex: number): ProcessedLine {
        const trimmed = line.trim();
        const leadingWhitespace =
            trimmed === "" ? "" : line.substring(0, line.length - line.trimStart().length);

        if (trimmed === "") {
            return {
                originalString: line,
                leadingWhitespace,
                trimmedContent: trimmed,
                lineNumber: lineIndex + 1,
                segments: [
                    {
                        type: "empty",
                        content: "",
                        startIndex: 0,
                        endIndex: line.length,
                        tokenType: createDefaultTokenType("empty"),
                    },
                ],
            };
        }

        // Check if line is part of a block comment
        if (this.isBlockComment(line) || this.commentState.inBlockComment) {
            return {
                originalString: line,
                leadingWhitespace,
                trimmedContent: trimmed,
                lineNumber: lineIndex + 1,
                segments: [
                    {
                        type: "comment",
                        content: line,
                        startIndex: 0,
                        endIndex: line.length,
                        tokenType: {
                            type: "comment",
                            breakable: true,
                        },
                    },
                ],
            };
        }

        if (trimmed.startsWith("/*")) {
            return {
                originalString: line,
                leadingWhitespace,
                trimmedContent: trimmed,
                lineNumber: lineIndex + 1,
                segments: [
                    {
                        type: "comment",
                        content: line,
                        startIndex: 0,
                        endIndex: line.length,
                        tokenType: {
                            type: "comment",
                            breakable: false,
                        },
                    },
                ],
            };
        }

        // Get segments from the processor
        const segments = this.segmentProcessor.processLine(line);

        return {
            originalString: line,
            leadingWhitespace,
            trimmedContent: trimmed,
            lineNumber: lineIndex + 1,
            segments,
        };
    }

    /**
     * Check if current line ends a block
     */
    private isBlockEnd(line: ProcessedLine): boolean {
        const content = line.trimmedContent;

        // Check for semi-colons at the end of statement
        if (patterns.structure.semicolon.test(content)) {
            return true;
        }

        return false;
    }

    /**
     * Create a new block from processed lines
     */
    private createBlock(
        lines: ProcessedLine[],
        allLines: string[],
        currentIndex: number
    ): TypedBlock {
        const startLine = lines[0].lineNumber;
        const endLine = lines[lines.length - 1].lineNumber;
        const nextLine = this.getNextContentLine(allLines, currentIndex + 1);

        const blockIdentification = BlockIdentifier.identify(lines);

        return {
            lines,
            nextBlockFirstLine: nextLine,
            ...blockIdentification,
            context: {
                isPartOfChain: false,
                parentBlockType: null,
                previousBlock: undefined,
                depth: 0,
            },
            startLineNumber: startLine,
            endLineNumber: endLine,
        };
    }

    /**
     * Get the next non-empty line
     */
    private getNextContentLine(lines: string[], index: number): ProcessedLine | undefined {
        while (index < lines.length) {
            if (!patterns.structure.blankLine.test(lines[index])) {
                return this.processLine(lines[index], index);
            }
            index++;
        }
        return undefined;
    }

    /**
     * Merge two blocks together
     */
    private mergeBlocks(target: TypedBlock, source: TypedBlock): void {
        target.lines.push(...source.lines);
        target.endLineNumber = source.endLineNumber;
        target.nextBlockFirstLine = source.nextBlockFirstLine;

        if (target.blockType === "switch" && source.metadata.isCaseContent) {
            target.metadata = {
                ...target.metadata,
                isCaseContent: true,
            };
        }
    }

    /**
     * Check if line is part of a block comment
     */
    private isBlockComment(line: string): boolean {
        const trimmed = line.trim();

        // Handle block comment start
        if (trimmed.startsWith("/*")) {
            this.commentState.blockCommentDepth++;
            this.commentState.inBlockComment = true;
        }

        // Check for comment endings (semicolon)
        if (this.commentState.inBlockComment) {
            if (trimmed.endsWith(";")) {
                this.commentState.blockCommentDepth--;
                if (this.commentState.blockCommentDepth === 0) {
                    this.commentState.inBlockComment = false;
                }
            }
            return true;
        }

        return false;
    }
}
/**
 * Main formatter pipeline that coordinates the formatting process
 */
export class FormatterPipeline {
    private formatters: CodeFormatter[] = [];
    private blockProcessor: BlockProcessor;

    constructor(private config: FormatterConfig = DEFAULT_FORMATTER_CONFIG) {
        this.blockProcessor = new BlockProcessor(config);
    }

    /**
     * Add a formatter to the pipeline
     */
    public addFormatter(formatter: CodeFormatter): void {
        this.formatters.push(formatter);
    }

    /**
     * Remove a formatter from the pipeline
     */
    public removeFormatter(formatter: CodeFormatter): void {
        const index = this.formatters.indexOf(formatter);
        if (index !== -1) {
            this.formatters.splice(index, 1);
        }
    }

    /**
     * Process text through the formatting pipeline
     */
    public async process(text: string): Promise<string | DebugResult> {
        try {
            // Convert text to blocks
            let blocks = this.blockProcessor.processText(text);

            // Establish block relationships
            this.establishBlockRelationships(blocks);

            // If in debug mode, return debug analysis
            if (this.config.debug) {
                return {
                    tableView: this.formatTableView(blocks),
                    detailedView: this.formatDetailedView(blocks),
                    blocks: blocks,
                };
            }

            // Run each formatter
            for (const formatter of this.formatters) {
                await formatter.format(blocks);
            }

            // Convert blocks back to text
            return this.blockProcessor.blocksToText(blocks);
        } catch (error) {
            throw new FormatterError(
                `Formatting failed: ${error instanceof Error ? error.message : String(error)}`
            );
        }
    }

    /**
     * Establish relationships between blocks
     */
    private establishBlockRelationships(blocks: TypedBlock[]): void {
        let currentProcedure: TypedBlock | null = null;
        let currentControlBlock: TypedBlock | null = null;
        let blockStack: TypedBlock[] = [];
        let depth = 0;

        blocks.forEach((block, index) => {
            // Set up basic relationships
            const previousBlock = index > 0 ? blocks[index - 1] : undefined;
            const nextBlock = index < blocks.length - 1 ? blocks[index + 1] : undefined;

            // Update depth
            if (block.metadata.isStart) {
                depth++;
            }

            // Update context
            block.context = {
                parentBlock: currentProcedure || currentControlBlock || undefined,
                previousBlock,
                isPartOfChain: false,
                parentBlockType:
                    currentProcedure?.blockType || currentControlBlock?.blockType || null,
                depth,
            };

            // Track procedure blocks
            if (block.blockType === "procedure") {
                if (block.metadata.isStart) {
                    currentProcedure = block;
                    blockStack.push(block);
                } else if (block.metadata.isEnd) {
                    currentProcedure =
                        blockStack.length > 1 ? blockStack[blockStack.length - 2] : null;
                    blockStack.pop();
                }
            }

            // Track control structure blocks
            if (this.isControlStructure(block)) {
                if (block.metadata.isStart) {
                    currentControlBlock = block;
                    blockStack.push(block);
                } else if (block.metadata.isEnd) {
                    currentControlBlock =
                        blockStack.length > 1 ? blockStack[blockStack.length - 2] : null;
                    blockStack.pop();
                    depth = Math.max(0, depth - 1);
                }
            }

            // Handle chainable blocks
            if (previousBlock) {
                const chainPosition = this.determineChainPosition(block, previousBlock, nextBlock);
                if (chainPosition) {
                    block.context.isPartOfChain = true;
                    block.context.chainPosition = chainPosition;
                }
            }
        });
    }

    private isControlStructure(block: TypedBlock): boolean {
        return ["conditional", "loop", "switch", "errorHandling"].includes(block.blockType);
    }

    private determineChainPosition(
        block: TypedBlock,
        previousBlock: TypedBlock,
        nextBlock?: TypedBlock
    ): "first" | "middle" | "last" | undefined {
        // Handle if/else chains
        if (block.blockType === "conditional" && block.metadata.flowType === "else") {
            const hasNext =
                nextBlock?.blockType === "conditional" && nextBlock.metadata.flowType === "else";
            if (!hasNext) {
                return "last";
            }
            return previousBlock.metadata.flowType === "else" ? "middle" : "first";
        }

        // Handle case chains
        if (
            block.blockType === "switch" &&
            ["caseBranch", "caseDefault"].includes(block.metadata.flowType)
        ) {
            const hasNext =
                nextBlock?.blockType === "switch" &&
                ["caseBranch", "caseDefault"].includes(nextBlock.metadata.flowType);
            if (!hasNext) {
                return "last";
            }
            return previousBlock.metadata.flowType === "caseBranch" ? "middle" : "first";
        }

        return undefined;
    }

    private formatTableView(blocks: TypedBlock[]): string {
        const output: string[] = [];

        output.push("Block Analysis Table:");
        output.push("=====================");

        // Build header
        const headers = [
            "Block",
            "StartLine",
            "EndLine",
            "Type",
            "Flow",
            "Depth",
            "Chain",
            "Segments",
            "FirstLine",
            "NextLine",
        ];
        output.push("`" + headers.join("`|`") + "`");

        // Add each block
        blocks.forEach((block, index) => {
            const firstLine = this.truncateString(block.lines[0].trimmedContent, 30);
            const nextLine = block.nextBlockFirstLine
                ? this.truncateString(block.nextBlockFirstLine.trimmedContent, 30)
                : "END";

            // Count segments by token type
            const segmentCounts = block.lines.reduce((acc, line) => {
                line.segments.forEach((segment) => {
                    if (segment.tokenType) {
                        acc[segment.tokenType.type] = (acc[segment.tokenType.type] || 0) + 1;
                    }
                });
                return acc;
            }, {} as Record<string, number>);

            const segmentSummary = Object.entries(segmentCounts)
                .map(([type, count]) => `${type}:${count}`)
                .join(", ");

            const debugInfo = [
                `${index + 1}`,
                `${block.startLineNumber}`,
                `${block.endLineNumber}`,
                `${block.blockType}`,
                `${block.metadata.flowType}`,
                `${block.context.depth}`,
                `${block.context.isPartOfChain}${
                    block.context.chainPosition ? `(${block.context.chainPosition})` : ""
                }`,
                `${segmentSummary}`,
                `${firstLine}`,
                `${nextLine}`,
            ];

            output.push("`" + debugInfo.join("`|`") + "`");
        });

        return output.join("\n");
    }

    private formatDetailedView(blocks: TypedBlock[]): string {
        const output: string[] = [];

        output.push("Detailed Block Analysis:");
        output.push("========================");

        // Add each block
        blocks.forEach((block, index) => {
            output.push(`\nBlock ${index + 1}:`);
            output.push(`Type: ${block.blockType}`);
            output.push(`Flow: ${block.metadata.flowType}`);
            output.push(`Lines: ${block.startLineNumber}-${block.endLineNumber}`);
            output.push(`Depth: ${block.context.depth}`);

            // Add more details for the block if relevant
            if (block.context.isPartOfChain) {
                output.push(`Chain Position: ${block.context.chainPosition}`);
            }

            if (block.metadata.declarationType) {
                output.push(`Declaration Type: ${block.metadata.declarationType}`);
            }

            if (block.metadata.commentType) {
                output.push(`Comment Type: ${block.metadata.commentType}`);
            }

            if (block.context.parentBlockType) {
                output.push(`Parent Block: ${block.context.parentBlockType}`);
            }

            output.push("\nTrimmed Content:");
            block.lines.forEach((line) => {
                output.push(`    ${line.trimmedContent}`);
            });

            // Show segments for each line
            if (block.lines.some((line) => line.segments.length > 1)) {
                output.push("\nSegments:");

                // Display truncated segments on a single line before going into details
                block.lines.forEach((line) => {
                    const segments = line.segments.map((seg) =>
                        this.truncateString(seg.content, 20)
                    );
                    output.push(`    ${segments.join(" | ")}`);
                });

                // Add segment details if enabled
                if (this.config.showSegmentDetails) {
                    output.push("\nSegment Details:");
                    block.lines.forEach((line, lineIndex) => {
                        if (lineIndex > 0) {
                            output.push(""); // Add spacing between lines
                        }

                        output.push(`    Line ${lineIndex + 1}:`);

                        line.segments.forEach((segment, segIndex) => {
                            const content = this.truncateString(segment.content.trim(), 20);
                            output.push(`      Segment ${segIndex + 1}:`);
                            output.push(`        Content: ${content}`);
                            output.push(`        Type: ${segment.type}`);

                            if (segment.tokenType) {
                                output.push(`        Token Type: ${segment.tokenType.type}`);

                                // Only show breaking properties if they're defined
                                if (segment.tokenType.breakable !== undefined) {
                                    output.push(
                                        `        Breakable: ${segment.tokenType.breakable}`
                                    );
                                }

                                if (segment.tokenType.breakBefore !== undefined) {
                                    output.push(
                                        `        Break Before: ${segment.tokenType.breakBefore}`
                                    );
                                }

                                if (segment.tokenType.breakAfter !== undefined) {
                                    output.push(
                                        `        Break After: ${segment.tokenType.breakAfter}`
                                    );
                                }
                            }

                            output.push(
                                `        Position: ${segment.startIndex}-${segment.endIndex}`
                            );
                        });
                    });
                }
            }
        });

        return output.join("\n");
    }

    private truncateString(str: string, maxLength: number): string {
        if (str.length <= maxLength) {
            return str;
        }
        return str.substring(0, maxLength) + "...";
    }

    /**
     * Get current formatter configuration
     */
    public getConfig(): FormatterConfig {
        return { ...this.config };
    }

    // Make patterns available to formatters
    public readonly patterns = patterns;
}

/**
 * Handles segmentation of code lines into meaningful tokens
 */
export class SegmentProcessor {
    private readonly config: SegmentConfig;

    // Core token patterns
    private readonly patterns = {
        whitespace: /^\s+/,
        // Split method calls into two types
        colonMethodCall: /^[A-Za-z_]\w*:[A-Za-z_]\w*\s*\(/, // s:format(
        simpleMethodCall: /^[A-Za-z_]\w*\s*\(/, // usrmes(
        propertyAccess: /^[A-Za-z_]\w*:[A-Za-z_]\w*(?!\s*\()/,
        keyword: /^(?:^|[\s;]):(?![A-Za-z_]\w*:)[A-Za-z]+\b/i,
        identifier: /^[A-Za-z_]\w*/,
        number: /^-?\d*\.?\d+/,
        stringStart: /^["']/,
        commentStart: /^\/\*/,
        operator: /^(?::=|==|===|!=|>=|<=|&&|\|\||[+\-*/%=])/,
        separator: /^[,;()[\]{}]/,
        colon: /^:/, // Add the missing colon pattern
    };

    constructor(config: SegmentConfig = DEFAULT_SEGMENT_CONFIG) {
        this.config = { ...DEFAULT_SEGMENT_CONFIG, ...config };
    }

    /**
     * Process a line into segments
     */
    public processLine(line: string): ContentSegment[] {
        const segments: ContentSegment[] = [];
        let currentIndex = 0;

        while (currentIndex < line.length) {
            const result = this.processNextToken(line, currentIndex);

            if (!result || result.token === null) {
                // Skip any unrecognized character
                currentIndex++;
                continue;
            }

            const segment = this.convertTokenToSegment(result.token);
            if (segment) {
                segments.push(segment);
            }

            currentIndex = result.newIndex;
        }

        return this.mergeAdjacentSegments(segments);
    }

    /**
     * Process the next token in the line
     */
    private processNextToken(line: string, startIndex: number): ProcessingResult | null {
        const remaining = line.slice(startIndex);

        // Skip whitespace
        const whitespaceMatch = remaining.match(this.patterns.whitespace);
        if (whitespaceMatch) {
            return {
                token: null,
                newIndex: startIndex + whitespaceMatch[0].length,
            };
        }

        // Look for inline comments first since they need to capture until semicolon
        if (remaining.startsWith("/*")) {
            return this.processComment(line, startIndex);
        }

        // Try other token types in order
        return (
            this.processColonMethodCall(line, startIndex) || // Try colon methods first
            this.processSimpleMethodCall(line, startIndex) || // Then simple methods
            this.processPropertyAccess(line, startIndex) || // Then property access
            this.processKeyword(line, startIndex) || // Then keywords
            this.processOperator(line, startIndex) || // Then other tokens
            this.processString(line, startIndex) ||
            this.processSeparator(line, startIndex) ||
            this.processNumber(line, startIndex) ||
            this.processIdentifier(line, startIndex)
        );
    }

    /**
     * Process string literals
     */
    private processString(line: string, startIndex: number): ProcessingResult | null {
        const match = line.slice(startIndex).match(this.patterns.stringStart);
        if (!match) {
            return null;
        }

        const quote = match[0];
        let endIndex = startIndex + 1;
        let escaped = false;

        while (endIndex < line.length) {
            const char = line[endIndex];

            if (char === "\\") {
                escaped = !escaped;
                endIndex++;
                continue;
            }

            if (char === quote && !escaped) {
                return {
                    token: {
                        type: "stringLiteral",
                        content: line.slice(startIndex, endIndex + 1),
                        startIndex,
                        endIndex: endIndex + 1,
                        breakable: this.isBreakableString(line.slice(startIndex, endIndex + 1)),
                    },
                    newIndex: endIndex + 1,
                };
            }

            escaped = false;
            endIndex++;
        }

        // Handle unterminated string
        return null;
    }

    /**
     * Process comments
     */
    private processComment(line: string, startIndex: number): ProcessingResult | null {
        const remaining = line.slice(startIndex);
        const match = remaining.match(this.patterns.commentStart);
        if (!match) {
            return null;
        }

        // Find the comment part and the semicolon that ends it
        let endIndex = startIndex;
        while (endIndex < line.length) {
            if (line[endIndex] === ";") {
                endIndex++;
                break;
            }
            endIndex++;
        }

        return {
            token: {
                type: "comment",
                content: line.slice(startIndex, endIndex),
                startIndex,
                endIndex,
                breakable: true,
            },
            newIndex: endIndex,
        };
    }

    /**
     * Process keywords
     */
    private processKeyword(line: string, startIndex: number): ProcessingResult | null {
        const match = line.slice(startIndex).match(this.patterns.keyword);
        if (match) {
            return {
                token: {
                    type: "keyword",
                    content: match[0],
                    startIndex,
                    endIndex: startIndex + match[0].length,
                    breakable: false,
                    isKeyword: true,
                },
                newIndex: startIndex + match[0].length,
            };
        }

        return null;
    }

    /**
     * Process operators
     */
    private processOperator(line: string, startIndex: number): ProcessingResult | null {
        const match = line.slice(startIndex).match(this.patterns.operator);
        if (!match) {
            return null;
        }

        return {
            token: {
                type: "operator",
                content: match[0],
                startIndex,
                endIndex: startIndex + match[0].length,
                breakable: false,
            },
            newIndex: startIndex + match[0].length,
        };
    }

    /**
     * Process colon separately from other operators
     */
    private processColon(line: string, startIndex: number): ProcessingResult | null {
        const match = line.slice(startIndex).match(this.patterns.colon);
        if (!match) {
            return null;
        }

        return {
            token: {
                type: "operator",
                content: match[0],
                startIndex,
                endIndex: startIndex + match[0].length,
                breakable: false,
            },
            newIndex: startIndex + match[0].length,
        };
    }

    /**
     * Process separators
     */
    private processSeparator(line: string, startIndex: number): ProcessingResult | null {
        const match = line.slice(startIndex).match(this.patterns.separator);
        if (!match) {
            return null;
        }

        return {
            token: {
                type: "separator",
                content: match[0],
                startIndex,
                endIndex: startIndex + match[0].length,
                breakable: true,
            },
            newIndex: startIndex + match[0].length,
        };
    }

    /**
     * Process identifiers
     */
    private processIdentifier(line: string, startIndex: number): ProcessingResult | null {
        const match = line.slice(startIndex).match(this.patterns.identifier);
        if (!match) {
            return null;
        }

        return {
            token: {
                type: "identifier",
                content: match[0],
                startIndex,
                endIndex: startIndex + match[0].length,
                breakable: true,
            },
            newIndex: startIndex + match[0].length,
        };
    }

    /**
     * Process numbers
     */
    private processNumber(line: string, startIndex: number): ProcessingResult | null {
        const match = line.slice(startIndex).match(this.patterns.number);
        if (!match) {
            return null;
        }

        return {
            token: {
                type: "number",
                content: match[0],
                startIndex,
                endIndex: startIndex + match[0].length,
                breakable: false,
            },
            newIndex: startIndex + match[0].length,
        };
    }

    private processMethodCall(line: string, startIndex: number): ProcessingResult | null {
        const match = line.slice(startIndex).match(this.patterns.colonMethodCall);
        if (!match) {
            return null;
        }

        return {
            token: {
                type: "methodCall",
                content: match[0],
                startIndex,
                endIndex: startIndex + match[0].length,
                breakable: true,
            },
            newIndex: startIndex + match[0].length,
        };
    }

    private processPropertyAccess(line: string, startIndex: number): ProcessingResult | null {
        const match = line.slice(startIndex).match(this.patterns.propertyAccess);
        if (!match) {
            return null;
        }

        return {
            token: {
                type: "propertyAccess",
                content: match[0],
                startIndex,
                endIndex: startIndex + match[0].length,
                breakable: true,
            },
            newIndex: startIndex + match[0].length,
        };
    }

    private processColonMethodCall(line: string, startIndex: number): ProcessingResult | null {
        const match = line.slice(startIndex).match(this.patterns.colonMethodCall);
        if (!match) {
            return null;
        }

        return {
            token: {
                type: "methodCall",
                content: match[0],
                startIndex,
                endIndex: startIndex + match[0].length,
                breakable: true,
                isMethod: true,
            },
            newIndex: startIndex + match[0].length,
        };
    }

    private processSimpleMethodCall(line: string, startIndex: number): ProcessingResult | null {
        const match = line.slice(startIndex).match(this.patterns.simpleMethodCall);
        if (!match) {
            return null;
        }

        return {
            token: {
                type: "methodCall",
                content: match[0],
                startIndex,
                endIndex: startIndex + match[0].length,
                breakable: true,
                isMethod: true,
            },
            newIndex: startIndex + match[0].length,
        };
    }

    /**
     * Convert a token to a content segment
     */
    private convertTokenToSegment(token: Token): ContentSegment {
        return {
            type: TOKEN_TYPE_TO_CONTENT_TYPE[token.type],
            content: token.content,
            startIndex: token.startIndex,
            endIndex: token.endIndex,
            tokenType: {
                type: token.type,
                breakable: token.breakable,
            },
        };
    }

    /**
     * Determine if a string is breakable based on its content
     */
    private isBreakableString(content: string): boolean {
        // Remove quotes
        const innerContent = content.slice(1, -1);

        // Short strings are not breakable
        if (innerContent.length < 20) {
            return false;
        }

        // Don't break special strings
        if (/^[A-Z_]+$/.test(innerContent)) {
            return false;
        } // All caps with underscores
        if (/^\w+$/.test(innerContent)) {
            return false;
        } // Single word
        if (/^[$?#@]\w*$/.test(innerContent)) {
            return false;
        } // Special prefixes

        return true;
    }

    /**
     * Merge adjacent segments of the same type when appropriate
     */
    private mergeAdjacentSegments(segments: ContentSegment[]): ContentSegment[] {
        const merged: ContentSegment[] = [];
        let current: ContentSegment | null = null;

        for (const segment of segments) {
            if (!current) {
                current = segment;
                continue;
            }

            if (this.canMergeSegments(current, segment)) {
                current = this.mergeSegments(current, segment);
            } else {
                merged.push(current);
                current = segment;
            }
        }

        if (current) {
            merged.push(current);
        }

        return merged;
    }

    /**
     * Check if two segments can be merged
     */
    private canMergeSegments(a: ContentSegment, b: ContentSegment): boolean {
        // Only merge segments of the same type
        if (a.type !== b.type) {
            return false;
        }
        if (a.tokenType?.type !== b.tokenType?.type) {
            return false;
        }

        // Don't merge across gaps
        if (a.endIndex !== b.startIndex) {
            return false;
        }

        return !NON_MERGEABLE_TYPES.includes(a.tokenType.type);
    }

    /**
     * Merge two segments together
     */
    private mergeSegments(a: ContentSegment, b: ContentSegment): ContentSegment {
        return {
            type: a.type,
            content: a.content + b.content,
            startIndex: a.startIndex,
            endIndex: b.endIndex,
            tokenType: a.tokenType,
        };
    }
}
