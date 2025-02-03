/**
 * Core formatter pipeline for STARLIMS code
 */

import { start } from "repl";

// Core block and content types
/**
 * Represents the different types of code blocks that can be formatted.
 * - `procedure`: A block representing a procedure or function.
 * - `conditional`: A block representing a conditional statement (e.g., if-else).
 * - `loop`: A block representing a loop (e.g., for, while).
 * - `switch`: A block representing a switch-case statement.
 * - `errorHandling`: A block representing error handling (e.g., try-catch).
 * - `comment`: A block representing a comment.
 * - `declaration`: A block representing a variable or constant declaration.
 * - `logic`: A block representing logical operations or expressions.
 */
export type BlockType =
    | "procedure"
    | "conditional"
    | "loop"
    | "switch"
    | "errorHandling"
    | "comment"
    | "declaration"
    | "logic";

/**
 * Represents the various flow control keywords used in the formatting pipeline.
 *
 * The possible values are:
 * - `"procedureStart"`: Indicates the start of a procedure.
 * - `"procedureEnd"`: Indicates the end of a procedure.
 * - `"ifStart"`: Indicates the start of an if statement.
 * - `"ifEnd"`: Indicates the end of an if statement.
 * - `"else"`: Indicates an else statement.
 * - `"whileStart"`: Indicates the start of a while loop.
 * - `"whileEnd"`: Indicates the end of a while loop.
 * - `"exitWhile"`: Indicates an exit from a while loop.
 * - `"forStart"`: Indicates the start of a for loop.
 * - `"forEnd"`: Indicates the end of a for loop.
 * - `"loop"`: Indicates a loop statement.
 * - `"caseStart"`: Indicates the start of a case statement.
 * - `"caseBranch"`: Indicates a branch within a case statement.
 * - `"caseDefault"`: Indicates the default branch within a case statement.
 * - `"caseEnd"`: Indicates the end of a case statement.
 * - `"exitCase"`: Indicates an exit from a case statement.
 * - `"tryStart"`: Indicates the start of a try block.
 * - `"catch"`: Indicates a catch block.
 * - `"finally"`: Indicates a finally block.
 * - `"tryEnd"`: Indicates the end of a try block.
 * - `"error"`: Indicates an error statement.
 * - `"resume"`: Indicates a resume statement.
 * - `"assignment"`: Indicates an assignment statement.
 * - `"functionCall"`: Indicates a function call.
 * - `"other"`: Indicates any other type of statement not covered by the above keywords.
 */
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

/**
 * Represents the different types of declarations that can be used in the formatting pipeline.
 *
 * - `parameters`: Represents a parameter declaration.
 * - `declare`: Represents a declare statement.
 * - `default`: Represents a default declaration.
 * - `public`: Represents a public declaration.
 * - `include`: Represents an include statement.
 * - `other`: Represents any other type of declaration.
 */
export type DeclarationType = "parameters" | "declare" | "default" | "public" | "include" | "other";

/**
 * Represents the different types of comments that can be used in the formatting pipeline.
 *
 * - `block`: A block comment, typically spanning multiple lines.
 * - `single`: A single-line comment.
 * - `regionStart`: A comment indicating the start of a region.
 * - `regionEnd`: A comment indicating the end of a region.
 * - `separator`: A comment used as a separator.
 */
export type CommentType = "block" | "single" | "regionStart" | "regionEnd" | "separator";

/**
 * Represents the type of content that can be processed by the formatting pipeline.
 *
 * - `code`: Represents a block of code.
 * - `string`: Represents a string literal.
 * - `comment`: Represents a comment.
 * - `empty`: Represents an empty line or whitespace.
 */
export type ContentType = "code" | "string" | "comment" | "empty";

/**
 * Represents a segment of content within a line.
 */
export interface ContentSegment {
    /**
     * The type of content (code, string, comment, empty).
     */
    type: ContentType;

    /**
     * The actual content text.
     */
    content: string;

    /**
     * Starting index in the original line.
     */
    startIndex: number;

    /**
     * Ending index in the original line.
     */
    endIndex: number;

    /**
     * Type information about the token.
     */
    tokenType: TokenType;

    /**
     * Priority for determining break points.
     * Optional.
     */
    priority?: number;
}

/**
 * Represents a processed line in the formatting pipeline.
 */
export interface ProcessedLine {
    /**
     * The original unmodified line.
     */
    originalString: string;

    /**
     * The formatted version of the line.
     */
    formattedString: string;

    /**
     * Leading whitespace from the original line.
     */
    leadingWhitespace: string;

    /**
     * Content with whitespace trimmed.
     */
    trimmedContent: string;

    /**
     * Current line number.
     */
    lineNumber: number;

    /**
     * Original line number before any transformations.
     */
    originalLineNumber: number;

    /**
     * Array of content segments in the line.
     */
    segments: ContentSegment[];
}

/**
 * Represents an operation to be performed on a block of data.
 */
export interface BlockOperation {
    /**
     * The type of operation to be performed.
     * - `insert`: Insert a new block.
     * - `update`: Update an existing block.
     * - `delete`: Delete an existing block.
     */
    type: "insert" | "update" | "delete";
    /**
     * The index of the block on which the operation is to be performed.
     * - For `insert`, the index is the position where the new block should be inserted.
     * - For `update` and `delete`, the index is the position of the block to be updated or deleted.
     */
    blockIndex: number;
    /**
     * The blocks involved in the operation.
     */
    blocks: TypedBlock[];
}

/**
 * Configuration options for the formatter.
 */
export interface FormatterConfig {
    /**
     * Enables or disables debug mode.
     * @default false
     */
    debug?: boolean;

    /**
     * Determines whether to use spacing context.
     * @default false
     */
    useSpacingContext?: boolean;

    /**
     * Determines whether to use post-processing for spacing.
     * @default false
     */
    useSpacingPostProcessing?: boolean;

    /**
     * Preserves user-defined spacing.
     * @default false
     */
    preserveUserSpacing?: boolean;

    /**
     * The maximum number of consecutive blank lines allowed.
     * @default 1
     */
    maxConsecutiveBlankLines?: number;

    /**
     * The maximum length of a line.
     * @default 80
     */
    maxLineLength?: number;

    /**
     * The number of spaces or tabs to use for indentation.
     * @default 4
     */
    tabSize?: number;

    /**
     * The style of indentation to use: "tab" or "space".
     * @default "space"
     */
    indentStyle?: "tab" | "space";

    /**
     * Determines whether to show segment details.
     * @default false
     */
    showSegmentDetails?: boolean;
}

/**
 * Default configuration for the formatter.
 *
 * @property {boolean} debug - Enables or disables debug mode.
 * @property {boolean} useSpacingContext - Determines if spacing context should be used.
 * @property {boolean} useSpacingPostProcessing - Determines if spacing post-processing should be used.
 * @property {boolean} preserveUserSpacing - Preserves user-defined spacing if set to true.
 * @property {number} maxConsecutiveBlankLines - Maximum number of consecutive blank lines allowed.
 * @property {number} maxLineLength - Maximum length of a line before wrapping.
 * @property {number} tabSize - Number of spaces per tab.
 * @property {"space" | "tab"} indentStyle - Style of indentation, either "space" or "tab".
 * @property {boolean} showSegmentDetails - Shows segment details if set to true.
 */
export const DEFAULT_FORMATTER_CONFIG: FormatterConfig = {
    debug: false,
    useSpacingContext: true,
    useSpacingPostProcessing: true,
    preserveUserSpacing: false,
    maxConsecutiveBlankLines: 2,
    maxLineLength: 90,
    tabSize: 4,
    indentStyle: "space",
    showSegmentDetails: false,
};

/**
 * Represents a line that has been processed by the formatting pipeline.
 *
 * @interface ProcessedLine
 *
 * @property {string} originalString - The original string of the line before any formatting.
 * @property {string} formattedString - The string of the line after formatting.
 * @property {string} leadingWhitespace - The leading whitespace characters of the line.
 * @property {string} trimmedContent - The content of the line with leading and trailing whitespace removed.
 * @property {number} lineNumber - The line number in the original document.
 * @property {ContentSegment[]} segments - An array of content segments within the line.
 */
export interface ProcessedLine {
    originalString: string;
    formattedString: string;
    leadingWhitespace: string;
    trimmedContent: string;
    lineNumber: number;
    segments: ContentSegment[];
}

/**
 * Represents the context of a block within a formatting pipeline.
 */
export interface BlockContext {
    /**
     * The parent block of the current block, if any.
     */
    parentBlock?: TypedBlock;

    /**
     * The previous block in the sequence, if any.
     */
    previousBlock?: TypedBlock;

    /**
     * Indicates whether the block is part of a chain.
     */
    isPartOfChain: boolean;

    /**
     * The type of the parent block, or null if there is no parent block.
     */
    parentBlockType: BlockType | null;

    /**
     * The depth of the block within the hierarchy.
     */
    depth: number;

    /**
     * The position of the block within the chain, if it is part of one.
     * Can be "first", "middle", or "last".
     */
    chainPosition?: "first" | "middle" | "last";
}

/**
 * Represents metadata for a block of code in the formatting pipeline.
 */
export interface BlockMetadata {
    /**
     * The type of flow control keyword associated with the block.
     */
    flowType: FlowKeyword;

    /**
     * Indicates whether the block is the start of a flow control structure.
     */
    isStart: boolean;

    /**
     * Indicates whether the block is in the middle of a flow control structure.
     */
    isMiddle: boolean;

    /**
     * Indicates whether the block is the end of a flow control structure.
     */
    isEnd: boolean;

    /**
     * Optional. Indicates whether the block is part of a case content.
     */
    isCaseContent?: boolean;

    /**
     * Optional. The type of declaration associated with the block.
     */
    declarationType?: DeclarationType;

    /**
     * Optional. The type of comment associated with the block.
     */
    commentType?: CommentType;
}

/**
 * Represents a block of processed lines with associated metadata and context.
 */
export interface TypedBlock {
    /**
     * The lines that make up the block.
     */
    lines: ProcessedLine[];

    /**
     * The type of the block.
     */
    blockType: BlockType;

    /**
     * Metadata associated with the block.
     */
    metadata: BlockMetadata;

    /**
     * Context information for the block.
     */
    context: BlockContext;

    /**
     * The starting line number of the block.
     */
    startLineNumber: number;

    /**
     * The ending line number of the block.
     */
    endLineNumber: number;

    /**
     * The first line of the next block, if any.
     */
    nextBlockFirstLine?: ProcessedLine;

    /**
     * Insights provided by the formatter for this block, if any.
     */
    formatterInsights?: FormatterInsight[];
}

/**
 * Represents the result of a debugging operation.
 */
export interface DebugResult {
    tableView: string;
    detailedView: string;
    blocks: TypedBlock[];
}

/**
 * Represents the different types of tokens that can be identified in the formatting pipeline.
 *
 * @typedef {("keyword" | "separator" | "identifier" | "operator" | "number" | "stringLiteral" | "comment" | "empty" | "methodCall" | "propertyAccess" | "logicalOperator" | "incrementDecrement")} TokenTypeName
 *
 * @property {"keyword"} keyword - Represents a language keyword.
 * @property {"separator"} separator - Represents a separator such as a comma or semicolon.
 * @property {"identifier"} identifier - Represents an identifier such as a variable or function name.
 * @property {"operator"} operator - Represents an operator such as +, -, *, /.
 * @property {"number"} number - Represents a numeric literal.
 * @property {"stringLiteral"} stringLiteral - Represents a string literal.
 * @property {"comment"} comment - Represents a comment in the code.
 * @property {"empty"} empty - Represents an empty token.
 * @property {"methodCall"} methodCall - Represents a method call.
 * @property {"propertyAccess"} propertyAccess - Represents property access.
 * @property {"logicalOperator"} logicalOperator - Represents a logical operator such as && or ||.
 * @property {"incrementDecrement"} incrementDecrement - Represents increment or decrement operators (++ or --).
 */
export type TokenTypeName =
    | "keyword"
    | "separator"
    | "identifier"
    | "operator"
    | "number"
    | "stringLiteral"
    | "comment"
    | "empty"
    | "methodCall"
    | "propertyAccess"
    | "logicalOperator"
    | "incrementDecrement";

/**
 * Represents a token type used in the formatting pipeline.
 *
 * @interface TokenType
 * @property {TokenTypeName} type - The name of the token type.
 * @property {boolean} breakable - Indicates if the token can be broken.
 * @property {boolean} [breakBefore] - Indicates if a break should occur before the token.
 * @property {boolean} [breakAfter] - Indicates if a break should occur after the token.
 * @property {boolean} [isMethod] - Indicates if the token represents a method.
 * @property {boolean} [isProperty] - Indicates if the token represents a property.
 * @property {boolean} [isKeyword] - Indicates if the token represents a keyword.
 */
interface TokenType {
    type: TokenTypeName;
    breakable: boolean;
    breakBefore?: boolean;
    breakAfter?: boolean;
    isMethod?: boolean;
    isProperty?: boolean;
    isKeyword?: boolean;
}

// Types for token processing
/**
 * Represents a token in the formatting pipeline.
 *
 * @interface Token
 *
 * @property {TokenTypeName} type - The type of the token.
 * @property {string} content - The content of the token.
 * @property {number} startIndex - The starting index of the token in the source text.
 * @property {number} endIndex - The ending index of the token in the source text.
 * @property {boolean} breakable - Indicates if the token can be broken into smaller parts.
 * @property {boolean} [isMethod] - Optional. Indicates if the token represents a method.
 * @property {boolean} [isProperty] - Optional. Indicates if the token represents a property.
 * @property {boolean} [isKeyword] - Optional. Indicates if the token represents a keyword.
 */
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

/**
 * Represents the result of a processing operation.
 *
 * @interface ProcessingResult
 *
 * @property {Token | null} token - The processed token, or null if no token was processed.
 * @property {number} newIndex - The new index position after processing.
 */
interface ProcessingResult {
    token: Token | null;
    newIndex: number;
}

/**
 * Represents insights about a specific formatter's changes.
 */
export interface FormatterInsight {
    /**
     * The name of the formatter that made the change.
     */
    formatterName: string;

    /**
     * The line number in the source code where the change was made.
     */
    sourceLineNumber: number;

    /**
     * The type of change made by the formatter.
     * - "spacing": Changes related to spacing.
     * - "casing": Changes related to letter casing.
     * - "splitting": Changes related to splitting code elements.
     * - "other": Any other type of change.
     */
    changeType: "spacing" | "casing" | "splitting" | "other";

    /**
     * A description of the change made by the formatter.
     */
    description: string;

    /**
     * The code before the change was made.
     */
    before: string;

    /**
     * The code after the change was made.
     */
    after: string;
}

/**
 * A mapping from token type names to content types.
 * This is used to determine the content type for a given token type.
 *
 * @constant
 * @type {Record<TokenTypeName, ContentType>}
 * @property {ContentType} keyword - Represents code content type for keywords.
 * @property {ContentType} separator - Represents code content type for separators.
 * @property {ContentType} identifier - Represents code content type for identifiers.
 * @property {ContentType} operator - Represents code content type for operators.
 * @property {ContentType} number - Represents code content type for numbers.
 * @property {ContentType} stringLiteral - Represents string content type for string literals.
 * @property {ContentType} comment - Represents comment content type for comments.
 * @property {ContentType} empty - Represents empty content type for empty tokens.
 * @property {ContentType} methodCall - Represents code content type for method calls.
 * @property {ContentType} propertyAccess - Represents code content type for property accesses.
 * @property {ContentType} logicalOperator - Represents code content type for logical operators.
 * @property {ContentType} incrementDecrement - Represents code content type for increment/decrement operators.
 */
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
    logicalOperator: "code",
    incrementDecrement: "code",
};

/**
 * A list of token type names that are considered non-mergeable.
 * These token types should not be merged during the formatting process.
 *
 * @constant
 * @type {TokenTypeName[]}
 * @default
 * - "keyword"
 * - "separator"
 * - "operator"
 * - "stringLiteral"
 * - "number"
 * - "methodCall"
 * - "propertyAccess"
 * - "logicalOperator"
 * - "incrementDecrement"
 */
export const NON_MERGEABLE_TYPES: TokenTypeName[] = [
    "keyword",
    "separator",
    "operator",
    "stringLiteral",
    "number",
    "methodCall",
    "propertyAccess",
    "logicalOperator",
    "incrementDecrement",
];

/**
 * Configuration options for formatting segments.
 *
 * @property {number} [maxLineLength] - The maximum length of a line.
 * @property {boolean} [debug] - Flag to enable or disable debug mode.
 * @property {boolean} [preserveComments] - Flag to preserve comments during formatting.
 */
export interface SegmentConfig {
    maxLineLength?: number;
    debug?: boolean;
    preserveComments?: boolean; // New option
}

/**
 * Default configuration for segment formatting.
 *
 * @constant
 * @type {SegmentConfig}
 * @property {number} maxLineLength - The maximum length of a line before it gets wrapped.
 * @property {boolean} debug - Flag to enable or disable debug mode.
 * @property {boolean} preserveComments - Flag to indicate whether comments should be preserved.
 */
export const DEFAULT_SEGMENT_CONFIG: SegmentConfig = {
    maxLineLength: 90,
    debug: false,
    preserveComments: true,
};

/**
 * A collection of regular expression patterns used for formatting and parsing code.
 */
export const patterns = {
    flow: {
        procedure: {
            /**
             * Matches the start of a procedure or class definition.
             * Example: `:CLASS` or `:PROCEDURE`
             */
            start: /:(?:CLASS|PROCEDURE)\b/i,

            /**
             * Matches the end of a procedure.
             * Example: `:RETURN` or `:ENDPROC`
             */
            end: /:(?:RETURN|ENDPROC)\b/i,

            /**
             * Matches the end of a procedure.
             * Example: `:ENDPROC`
             */
            endProc: /:ENDPROC\b/i,

            /**
             * Matches a return statement.
             * Example: `:RETURN`
             */
            return: /:RETURN\b/i,
        },
        conditional: {
            /**
             * Matches the start of an if statement.
             * Example: `:IF`
             */
            ifStart: /:IF\b/i,

            /**
             * Matches the end of an if statement.
             * Example: `:ENDIF`
             */
            ifEnd: /:ENDIF\b/i,

            /**
             * Matches an else statement.
             * Example: `:ELSE`
             */
            else: /:ELSE\b/i,
        },
        loop: {
            /**
             * Matches the start of a while loop.
             * Example: `:WHILE`
             */
            whileStart: /:WHILE\b/i,

            /**
             * Matches the continuation of a loop.
             * Example: `:LOOP`
             */
            continue: /:LOOP\b/i,

            /**
             * Matches the end of a while loop.
             * Example: `:ENDWHILE`
             */
            whileEnd: /:ENDWHILE\b/i,

            /**
             * Matches an exit while statement.
             * Example: `:EXITWHILE`
             */
            exitWhile: /:EXITWHILE\b/i,

            /**
             * Matches the start of a for loop.
             * Example: `:FOR`
             */
            forStart: /:FOR\b/i,

            /**
             * Matches the end of a for loop.
             * Example: `:NEXT`
             */
            forEnd: /:NEXT\b/i,
        },
        switch: {
            /**
             * Matches the start of a switch case block.
             * Example: `:BEGINCASE`
             */
            start: /:BEGINCASE\b/i,

            /**
             * Matches a case branch in a switch case block.
             * Example: `:CASE`
             */
            branch: /:CASE\b/i,

            /**
             * Matches the default branch in a switch case block.
             * Example: `:OTHERWISE`
             */
            default: /:OTHERWISE\b/i,

            /**
             * Matches the end of a switch case block.
             * Example: `:ENDCASE`
             */
            end: /:ENDCASE\b/i,

            /**
             * Matches an exit case statement.
             * Example: `:EXITCASE`
             */
            exit: /:EXITCASE\b/i,
        },
        errorHandling: {
            /**
             * Matches the start of a try block.
             * Example: `:TRY`
             */
            tryStart: /:TRY\b/i,

            /**
             * Matches a catch block.
             * Example: `:CATCH`
             */
            catch: /:CATCH\b/i,

            /**
             * Matches a finally block.
             * Example: `:FINALLY`
             */
            finally: /:FINALLY\b/i,

            /**
             * Matches the end of a try block.
             * Example: `:ENDTRY`
             */
            tryEnd: /:ENDTRY\b/i,

            /**
             * Matches an error statement.
             * Example: `:ERROR`
             */
            error: /:ERROR\b/i,

            /**
             * Matches a resume statement.
             * Example: `:RESUME`
             */
            resume: /:RESUME\b/i,
        },
    },
    declaration: {
        types: {
            /**
             * Matches an include statement.
             * Example: `:INCLUDE`
             */
            include: /:INCLUDE\b/i,

            /**
             * Matches a declare statement.
             * Example: `:DECLARE`
             */
            declare: /:DECLARE\b/i,

            /**
             * Matches a public statement.
             * Example: `:PUBLIC`
             */
            public: /:PUBLIC\b/i,

            /**
             * Matches a parameters statement.
             * Example: `:PARAMETERS`
             */
            parameters: /:PARAMETERS\b/i,

            /**
             * Matches a default statement.
             * Example: `:DEFAULT`
             */
            default: /:DEFAULT\b/i,
        },
        /**
         * Matches a group of declaration statements.
         * Example: `:PARAMETERS`, `:DEFAULT`, or `:DECLARE`
         */
        group: /^:(?:PARAMETERS|DEFAULT|DECLARE)\b/i,
    },
    logic: {
        /**
         * Matches an assignment operation.
         * Example: `:=`, `+=`, or `-=`
         */
        assignment: /[:+-]=/,

        /**
         * Matches a function call.
         * Example: `functionName(`
         */
        functionCall: /\w+\s*\(/,
    },
    /**
     * Basic comment structure.
     * Example: `/* This is a comment;`
     */
    comment: {
        /**
         * Matches the start of a block comment.
         * Example: `/**`
         */
        block: /^\/\*\*/,

        /**
         * Matches a single line comment.
         * Example: `/*`
         */
        single: /^\/\*/,

        /**
         * Matches the end of a comment.
         * Example: `;`
         */
        endComment: /;/,

        region: {
            /**
             * Matches the start of a region comment.
             * Example: `/* region`
             */
            start: /^\/\*\s*region\b/i,

            /**
             * Matches the end of a region comment.
             * Example: `/* endregion`
             */
            end: /^\/\*\s*endregion\b/i,
        },

        /**
         * Matches a separator comment.
         * Example: `/* ====================`
         */
        separator: /^\/\*\s*[=*-]{20}/,
    },
    structure: {
        /**
         * Matches a semicolon.
         * Example: `;`
         */
        semicolon: /;/,

        /**
         * Matches a multi-line statement.
         * Example: Any line that does not end with a semicolon.
         */
        multiLine: /[^;]\s*$/,

        /**
         * Matches a blank line.
         * Example: Any line that contains only whitespace.
         */
        blankLine: /^\s*$/,

        /**
         * Matches a new line character.
         * Example: `\n` or `\r\n`
         */
        newLine: /\r?\n/,
    },
    keyword: {
        /**
         * Matches a keyword with a colon prefix.
         * Example: `:KEYWORD`
         */
        pattern: /:[A-Za-z]+\b/i,

        /**
         * Matches all occurrences of keywords with a colon prefix.
         * Example: `:KEYWORD`
         */
        find: /:[A-Za-z]+\b/gi,
    },
};

/**
 * Creates a default token type object.
 *
 * @param type - The name of the token type.
 * @returns An object representing the token type with its properties.
 */
function createDefaultTokenType(type: TokenTypeName): TokenType {
    return {
        type,
        breakable: !NON_MERGEABLE_TYPES.includes(type),
    };
}

/**
 * Custom error class for formatting-related errors.
 *
 * @extends {Error}
 */
export class FormatterError extends Error {
    constructor(message: string) {
        super(message);
        this.name = "FormatterError";
    }
}

/**
 * Interface that all code formatters must implement
 */
export interface CodeFormatter {
    /**
     * Formats an array of code blocks
     * @param blocks The blocks to format
     */
    format(blocks: TypedBlock[]): Promise<void>;

    /**
     * Gets the name of the formatter
     * @returns Formatter name
     */
    getName(): string;

    /**
     * Gets formatting insights from the last format operation
     * @returns Array of formatter insights
     */
    getInsights(): FormatterInsight[];
}

/**
 * The `BlockIdentifier` class provides methods to identify and categorize different types of code blocks
 * from an array of processed lines. It includes functionality to determine if blocks should be merged
 * and to identify specific types of blocks such as comments, procedures, control flow statements, declarations,
 * logic, loops, switch cases, and error handling blocks.
 */
export class BlockIdentifier {
    /**
     * Identifies the type of block and its metadata from the given lines of processed content.
     *
     * @param lines - An array of `ProcessedLine` objects representing the lines of code to be identified.
     * @returns An object containing the `blockType` and `metadata` of the identified block.
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
     * Determines whether the current block should be merged with the previous block.
     *
     * @param prevBlock - The previous block of code.
     * @param currentBlock - The current block of code.
     * @returns `true` if the current block should be merged with the previous block, otherwise `false`.
     *
     * This method handles multi-line statements that are incomplete (no semicolon) and blocks that are explicitly joined (like a statement broken across lines).
     * It returns `false` if the current block is a comment.
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
     * Determines if a given line of code is an explicit continuation of the previous line.
     *
     * This method checks if the line is a comment or matches a pattern that indicates
     * it is a continuation of the previous line. Continuation patterns include lines
     * that start with operators or logical connectors.
     *
     * @param line - The line of code to check.
     * @returns `true` if the line is an explicit continuation, `false` otherwise.
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

    /**
     * Identifies the type of procedure block in a given line of text.
     *
     * This method checks if the provided line matches the start or end pattern
     * of a procedure block. If a match is found, it returns an object containing
     * the block type and metadata about the procedure block. If no match is found,
     * it returns null.
     *
     * @param line - The line of text to be checked.
     * @returns An object containing the block type and metadata if a match is found, or null if no match is found.
     */
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

    /**
     * Identifies if a given line matches any of the declaration patterns.
     *
     * @param line - The line of text to be checked against declaration patterns.
     * @returns An object containing the block type and metadata if a match is found, otherwise null.
     */
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

    /**
     * Identifies the type of comment in a given line of code.
     *
     * @param line - The line of code to analyze.
     * @returns An object containing the block type and metadata if a comment is identified, or `null` if no comment is found.
     *
     * The returned metadata includes:
     * - `flowType`: The flow type of the comment (always "other").
     * - `isStart`: Indicates if the comment marks the start of a region.
     * - `isMiddle`: Indicates if the comment is in the middle of a region or a standalone comment.
     * - `isEnd`: Indicates if the comment marks the end of a region.
     * - `commentType`: The specific type of comment (e.g., "regionStart", "regionEnd", "separator", "block", "single").
     */
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

    /**
     * Identifies the type of control flow statement in a given line of code.
     *
     * This method analyzes the provided line of code to determine if it contains
     * a conditional statement, loop, switch block, or error handling block. It
     * returns an object containing the block type and associated metadata if a
     * control flow statement is identified, or `null` if no control flow statement
     * is found.
     *
     * @param line - The line of code to analyze.
     * @returns An object containing the block type and metadata if a control flow
     * statement is identified, or `null` if no control flow statement is found.
     */
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

    /**
     * Identifies the type of conditional block in a given line of code.
     *
     * @param line - The line of code to be analyzed.
     * @returns An object containing the block type and metadata if a conditional block is identified, or null otherwise.
     *
     * The returned metadata includes:
     * - `flowType`: The specific type of conditional flow (e.g., "ifStart", "ifEnd", "else").
     * - `isStart`: Indicates if the line marks the start of a conditional block.
     * - `isMiddle`: Indicates if the line is in the middle of a conditional block.
     * - `isEnd`: Indicates if the line marks the end of a conditional block.
     */
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

    /**
     * Identifies the type of loop and its metadata based on the given line of code.
     *
     * @param line - The line of code to be analyzed.
     * @returns An object containing the block type and metadata if a loop pattern is matched, otherwise null.
     *
     * The returned metadata object contains:
     * - `flowType`: The type of loop flow (e.g., "whileStart", "whileEnd", "exitWhile", "forStart", "forEnd").
     * - `isStart`: A boolean indicating if the line marks the start of the loop.
     * - `isMiddle`: A boolean indicating if the line is in the middle of the loop.
     * - `isEnd`: A boolean indicating if the line marks the end of the loop.
     */
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

    /**
     * Identifies the type of switch block and its metadata based on the given line of code.
     *
     * @param line - The line of code to be analyzed.
     * @returns An object containing the block type and metadata if a match is found, otherwise null.
     *
     * The returned object has the following structure:
     * - blockType: "switch"
     * - metadata: An object containing:
     *   - flowType: The type of switch flow (e.g., "caseStart", "caseBranch", "caseDefault", "caseEnd", "exitCase").
     *   - isStart: A boolean indicating if this is the start of the switch block.
     *   - isMiddle: A boolean indicating if this is a middle part of the switch block.
     *   - isEnd: A boolean indicating if this is the end of the switch block.
     */
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

    /**
     * Identifies the type of error handling block in a given line of code.
     *
     * @param line - The line of code to analyze.
     * @returns An object containing the block type and metadata if an error handling block is identified, or `null` if no match is found.
     *
     * The returned metadata includes:
     * - `flowType`: The specific type of error handling flow (e.g., "tryStart", "catch", "finally", "tryEnd", "error", "resume").
     * - `isStart`: Indicates if the block is the start of an error handling flow.
     * - `isMiddle`: Indicates if the block is in the middle of an error handling flow.
     * - `isEnd`: Indicates if the block is the end of an error handling flow.
     */
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

    /**
     * Identifies the type of logic present in a given line of code.
     *
     * This method analyzes the provided line of code and determines whether it represents
     * an assignment, a function call, or some other type of logic. It returns an object
     * containing the block type and metadata about the flow type and position within the block.
     *
     * @param line - The line of code to analyze.
     * @returns An object containing the block type and metadata about the flow type and position.
     *
     * @example
     * ```typescript
     * const result = identifyLogic("let x = 10;");
     * // result: { blockType: "logic", metadata: { flowType: "assignment", isStart: false, isMiddle: true, isEnd: false } }
     * ```
     */
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
 * Represents the state of comment parsing within a formatting pipeline.
 */
interface CommentState {
    inBlockComment: boolean;
    blockCommentDepth: number;
}

/**
 * The `BlockProcessor` class is responsible for processing text into structured blocks,
 * handling comments, and formatting the blocks. It uses a `SegmentProcessor` to process
 * individual lines and manages the state of block comments.
 */
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
     * Processes the given text and splits it into typed blocks.
     *
     * @param text - The text to be processed.
     * @returns An array of TypedBlock objects.
     * @throws FormatterError - If an error occurs while processing the text.
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
     * Converts an array of `TypedBlock` objects into a single formatted text string.
     * Each block's lines are concatenated, using the `formattedString` property if available,
     * otherwise falling back to the `originalString` property. Lines are separated by newline characters.
     * Ensures that the resulting string has exactly one trailing newline.
     *
     * @param {TypedBlock[]} blocks - The array of `TypedBlock` objects to be converted to text.
     * @returns {string} The concatenated and formatted text string.
     */
    public blocksToText(blocks: TypedBlock[]): string {
        let result = "";

        blocks.forEach((block, index) => {
            // Add each line, using formattedString instead of originalString
            block.lines.forEach((line, lineIndex) => {
                result += line.formattedString || line.originalString; // Use formattedString with fallback
                if (!(index === blocks.length - 1 && lineIndex === block.lines.length - 1)) {
                    result += "\n";
                }
            });
        });

        // Ensure exactly one trailing newline
        return result.trimEnd() + "\n";
    }

    /**
     * Splits the given text into blocks of lines based on specific patterns and rules.
     *
     * @param text - The input text to be split into blocks.
     * @returns An array of TypedBlock objects representing the split blocks.
     *
     * The function processes the input text line by line, grouping lines into blocks.
     * It handles multi-line statements and merges blocks when necessary.
     * Empty lines at the start of the file are skipped.
     *
     * The function uses the following steps:
     * 1. Splits the input text into lines.
     * 2. Iterates through each line, processing it and adding it to the current block.
     * 3. Checks for the end of a block and creates a new block if necessary.
     * 4. Merges blocks if they should be merged.
     * 5. Handles any remaining lines after the iteration.
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
     * Processes a single line of text and returns a `ProcessedLine` object containing
     * various details about the line, such as its original and formatted content,
     * leading whitespace, trimmed content, line numbers, and segments.
     *
     * @param line - The line of text to process.
     * @param lineIndex - The index of the line in the original text.
     * @returns A `ProcessedLine` object with details about the processed line.
     */
    private processLine(line: string, lineIndex: number): ProcessedLine {
        const trimmed = line.trim();
        const leadingWhitespace =
            trimmed === "" ? "" : line.substring(0, line.length - line.trimStart().length);

        if (trimmed === "") {
            return {
                originalString: line,
                formattedString: line, // Initialize with original
                leadingWhitespace,
                trimmedContent: trimmed,
                lineNumber: lineIndex + 1,
                originalLineNumber: lineIndex + 1,
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

        // Check if this line is part of a block comment first
        const isCommentLine = this.isBlockComment(line);

        // If it's a comment line or we're in a block comment, handle it accordingly
        if (isCommentLine || this.commentState.inBlockComment) {
            return {
                originalString: line,
                formattedString: line, // Initialize with original
                leadingWhitespace,
                trimmedContent: trimmed,
                lineNumber: lineIndex + 1,
                originalLineNumber: lineIndex + 1,
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

        // Get segments from the processor for all other lines
        const segments = this.segmentProcessor.processLine(line);

        return {
            originalString: line,
            formattedString: line, // Initialize with original
            leadingWhitespace,
            trimmedContent: trimmed,
            lineNumber: lineIndex + 1,
            originalLineNumber: lineIndex + 1,
            segments,
        };
    }

    /**
     * Determines if the given line marks the end of a block.
     *
     * This method checks if the trimmed content of the provided line
     * matches the pattern for a semi-colon at the end of a statement.
     *
     * @param line - The processed line to check.
     * @returns `true` if the line marks the end of a block, otherwise `false`.
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
     * Creates a `TypedBlock` object from the given lines and context.
     *
     * @param lines - An array of `ProcessedLine` objects representing the lines to be included in the block.
     * @param allLines - An array of all lines in the document as strings.
     * @param currentIndex - The current index in the `allLines` array.
     * @returns A `TypedBlock` object containing the lines, block identification, context, and line numbers.
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
     * Applies a series of block operations (insert, update, delete) to an array of TypedBlock objects.
     *
     * @param {TypedBlock[]} blocks - The array of blocks to which the operations will be applied.
     * @param {BlockOperation[]} operations - The array of operations to apply to the blocks.
     * @returns {TypedBlock[]} The updated array of blocks after all operations have been applied.
     *
     * The operations are sorted by block index in reverse order to handle deletions correctly.
     * Each operation is then applied in sequence:
     * - "insert": Inserts new blocks at the specified index.
     * - "update": Replaces the block at the specified index with new blocks.
     * - "delete": Removes the block at the specified index.
     *
     * After applying all operations, the line numbers and relationships of the blocks are updated.
     */
    public applyBlockOperation(blocks: TypedBlock[], operations: BlockOperation[]): TypedBlock[] {
        // Sort operations by block index in reverse order to handle deletions
        operations.sort((a, b) => b.blockIndex - a.blockIndex);

        // Apply each operation
        for (const op of operations) {
            switch (op.type) {
                case "insert":
                    // Insert new blocks at the specified index
                    blocks.splice(op.blockIndex, 0, ...op.blocks);
                    break;
                case "update":
                    // Replace the block at the specified index
                    blocks.splice(op.blockIndex, 1, ...op.blocks);
                    break;
                case "delete":
                    // Remove the block at the specified index
                    blocks.splice(op.blockIndex, 1);
                    break;
            }
        }

        // Update line numbers and relationships
        this.updateblockLineNumbers(blocks);
        return blocks;
    }

    /**
     * Updates the line numbers for each block and its lines.
     *
     * This method iterates through the provided blocks and assigns
     * line numbers starting from `1`. Each block's `startLineNumber` is set
     * to the current line number before iterating through its lines.
     * Each line within the block is then assigned a line number sequentially.
     * After processing all lines in a block, the block's `endLineNumber` is set
     * to the last line number assigned.
     *
     * @param blocks - An array of `TypedBlock` objects to update with line numbers.
     */
    private updateblockLineNumbers(blocks: TypedBlock[]): void {
        let currentLineNumber = 1;

        blocks.forEach((block) => {
            block.startLineNumber = currentLineNumber;
            block.lines.forEach((line) => {
                line.lineNumber = currentLineNumber++;
            });

            block.endLineNumber = currentLineNumber - 1;
        });
    }

    /**
     * Retrieves the next non-blank line from the given array of lines starting from the specified index.
     *
     * @param lines - An array of strings representing the lines to be processed.
     * @param index - The starting index from which to search for the next non-blank line.
     * @returns A `ProcessedLine` object representing the processed line and its index, or `undefined` if no non-blank line is found.
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
     * Merges the lines and metadata of the source block into the target block.
     *
     * @param target - The block that will receive the merged content.
     * @param source - The block whose content will be merged into the target block.
     *
     * The function performs the following operations:
     * - Appends all lines from the source block to the target block.
     * - Updates the target block's end line number to match the source block's end line number.
     * - Updates the target block's next block first line to match the source block's next block first line.
     * - If the target block is of type "switch" and the source block's metadata indicates it is case content,
     *   the target block's metadata is updated to reflect that it contains case content.
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
     * Determines if a given line is part of a block comment.
     *
     * This method checks if the provided line is within a block comment or starts a new block comment.
     * It also updates the internal state of the comment handling to track the depth of nested block comments.
     *
     * @param line - The line of code to check.
     * @returns `true` if the line is part of a block comment, `false` otherwise.
     */
    private isBlockComment(line: string): boolean {
        const trimmed = line.trim();

        // If we're already in a block comment, check for ending
        if (this.commentState.inBlockComment) {
            if (trimmed.endsWith(";")) {
                this.commentState.blockCommentDepth--;
                if (this.commentState.blockCommentDepth === 0) {
                    this.commentState.inBlockComment = false;
                }
            }
            return true;
        }

        // Handle block comment start
        if (trimmed.startsWith("/*")) {
            // Check if comment ends on the same line
            if (trimmed.endsWith(";")) {
                // This is an inline comment - split it into segments
                return false;
            }
            this.commentState.blockCommentDepth++;
            this.commentState.inBlockComment = true;
        }

        return false;
    }
}

/**
 * Represents a pipeline for formatting code, consisting of multiple formatters and block operations.
 */
export class FormatterPipeline {
    private formatters: CodeFormatter[] = [];
    private blockProcessor: BlockProcessor;
    private allInsights: FormatterInsight[] = [];
    private blockOperations: BlockOperation[] = [];

    /**
     * Initializes a new instance of the `FormatterPipeline` class.
     *
     * @param config - The configuration for the formatter pipeline. Defaults to `DEFAULT_FORMATTER_CONFIG`.
     */
    constructor(private config: FormatterConfig = DEFAULT_FORMATTER_CONFIG) {
        this.blockProcessor = new BlockProcessor(config);
    }

    /**
     * Adds a new formatter to the formatting pipeline.
     *
     * @param formatter - The formatter to be added to the pipeline.
     */
    public addFormatter(formatter: CodeFormatter): void {
        this.formatters.push(formatter);
    }

    /**
     * Removes the specified formatter from the list of formatters.
     *
     * @param formatter - The formatter to be removed.
     */
    public removeFormatter(formatter: CodeFormatter): void {
        const index = this.formatters.indexOf(formatter);
        if (index !== -1) {
            this.formatters.splice(index, 1);
        }
    }

    /**
     * Processes the given text through a series of formatters, converting it to blocks,
     * establishing relationships, and applying insights and operations.
     *
     * @param text - The input text to be processed.
     * @returns A promise that resolves to the formatted text or a debug result.
     * @throws {FormatterError} If the formatting process fails.
     */
    public async process(text: string): Promise<string | DebugResult> {
        try {
            // Convert text to blocks
            let blocks = this.blockProcessor.processText(text);

            // Establish block relationships
            this.establishBlockRelationships(blocks);

            // Reset insights and operations
            this.allInsights = [];
            this.blockOperations = [];

            // Run each formatter
            for (const formatter of this.formatters) {
                await formatter.format(blocks);
                const insights = formatter.getInsights();
                this.allInsights.push(...insights);

                // Apply block operations after each formatter
                if (this.blockOperations.length > 0) {
                    blocks = this.blockProcessor.applyBlockOperation(blocks, this.blockOperations);
                    this.blockOperations = [];

                    // Re-establish relationships after structural changes
                    this.establishBlockRelationships(blocks);
                }

                // Add insights to their respective blocks
                const blockContainsLine = (block: TypedBlock, lineNumber: number) => {
                    return block.lines.some((line) => line.lineNumber === lineNumber);
                };
                insights.forEach((insight) => {
                    const block = blocks.find((b) =>
                        blockContainsLine(b, insight.sourceLineNumber)
                    );
                    if (block) {
                        block.formatterInsights = block.formatterInsights || [];
                        block.formatterInsights.push(insight);
                    }
                });
            }

            // If in debug mode, return debug analysis
            if (this.config.debug) {
                return {
                    tableView: this.formatTableView(blocks),
                    detailedView: this.formatDetailedView(blocks),
                    blocks: blocks,
                };
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
     * Registers a block operation to the formatting pipeline.
     *
     * @param operation - The block operation to be added to the pipeline.
     */
    public registerBlockOperation(operation: BlockOperation): void {
        this.blockOperations.push(operation);
    }

    /**
     * Establishes relationships between blocks in a given array of `TypedBlock`.
     * This method sets up parent-child relationships, tracks procedure and control structure blocks,
     * updates block context with depth and chain information, and manages a stack to handle nested blocks.
     *
     * @param blocks - An array of `TypedBlock` objects to process and establish relationships for.
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

    /**
     * Determines if the given block is a control structure.
     *
     * @param block - The block to check.
     * @returns `true` if the block is a control structure (conditional, loop, switch, or error handling), otherwise `false`.
     */
    private isControlStructure(block: TypedBlock): boolean {
        return ["conditional", "loop", "switch", "errorHandling"].includes(block.blockType);
    }

    /**
     * Determines the position of a block within a chain of blocks.
     *
     * @param block - The current block being evaluated.
     * @param previousBlock - The block that precedes the current block.
     * @param nextBlock - The block that follows the current block (optional).
     * @returns The position of the block within the chain: "first", "middle", "last", or undefined if the block is not part of a recognized chain.
     *
     * The function handles two types of chains:
     * 1. If/else chains:
     *    - If the block is a conditional block with a flow type of "else":
     *      - If there is no next block or the next block is not a conditional block with a flow type of "else", it returns "last".
     *      - If the previous block has a flow type of "else", it returns "middle".
     *      - Otherwise, it returns "first".
     * 2. Case chains:
     *    - If the block is a switch block with a flow type of "caseBranch" or "caseDefault":
     *      - If there is no next block or the next block is not a switch block with a flow type of "caseBranch" or "caseDefault", it returns "last".
     *      - If the previous block has a flow type of "caseBranch", it returns "middle".
     *      - Otherwise, it returns "first".
     */
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

    /**
     * Generates a formatted table view of block analysis.
     *
     * @param blocks - An array of `TypedBlock` objects to be analyzed and formatted.
     * @returns A string representing the formatted table view of the block analysis.
     *
     * The table includes the following columns:
     * - Block: The index of the block.
     * - StartLine: The starting line number of the block.
     * - EndLine: The ending line number of the block.
     * - Type: The type of the block.
     * - Flow: The flow type metadata of the block.
     * - Depth: The depth context of the block.
     * - Chain: Indicates if the block is part of a chain and its position in the chain.
     * - Segments: A summary of segment counts by token type within the block.
     * - FirstLine: The truncated content of the first line of the block.
     * - NextLine: The truncated content of the first line of the next block or "END" if there is no next block.
     */
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

    /**
     * Formats a detailed view of the provided blocks, including insights and segment details.
     *
     * @param {TypedBlock[]} blocks - The blocks to format.
     * @returns {string} The formatted detailed view as a string.
     *
     * The output includes:
     * - A header for detailed block analysis.
     * - Grouped insights by source line number.
     * - Detailed information for each block, including type, flow, lines, depth, chain position, declaration type, comment type, and parent block type.
     * - Formatter insights for lines within each block, sorted by formatter and change type.
     * - Content comparison for each line, showing original and formatted strings.
     * - Segments for each line, with truncated content displayed on a single line.
     * - Optional segment details, including content, type, token type, breakable properties, and position.
     */
    private formatDetailedView(blocks: TypedBlock[]): string {
        const output: string[] = [];

        output.push("Detailed Block Analysis:");
        output.push("========================");

        // Group insights by source line number
        const insightsByLine = new Map<number, FormatterInsight[]>();
        this.allInsights.forEach((insight) => {
            // Ensure we're using the original line number from the insight
            const insights = insightsByLine.get(insight.sourceLineNumber) || [];
            insights.push(insight);
            insightsByLine.set(insight.sourceLineNumber, insights);
        });

        // Add each block
        blocks.forEach((block, index) => {
            output.push(`\nBlock ${index + 1}:`);
            output.push(`Type: ${block.blockType}`);
            output.push(`Flow: ${block.metadata.flowType}`);
            // Use original line numbers for block range
            output.push(
                `Lines: ${block.lines[0].originalLineNumber}-${
                    block.lines[block.lines.length - 1].originalLineNumber
                }`
            );
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

            // Add formatter insights for lines in this block
            block.lines.forEach((line) => {
                // Use originalLineNumber for insights
                const lineInsights = insightsByLine.get(line.originalLineNumber);
                if (lineInsights?.length) {
                    output.push(`\nFormatter Changes for Line ${line.originalLineNumber}:`);
                    // Sort insights by formatter and change type to ensure consistent ordering
                    const sortedInsights = lineInsights.sort((a, b) => {
                        if (a.formatterName === b.formatterName) {
                            return a.changeType.localeCompare(b.changeType);
                        }
                        return a.formatterName.localeCompare(b.formatterName);
                    });
                    sortedInsights.forEach((insight) => {
                        output.push(`  ${insight.formatterName}:`);
                        output.push(`    ${insight.description}`);
                        output.push(`    Before: "${insight.before}"`);
                        output.push(`    After:  "${insight.after}"`);
                    });
                }
            });

            output.push("\nContent Comparison:");
            block.lines.forEach((line) => {
                output.push(`    Line ${line.originalLineNumber}:`);
                output.push(`       Original: ${line.originalString.trim()}`);
                output.push(`      Formatted: ${line.formattedString.trim()}`);
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

    /**
     * Truncates a string to a specified maximum length and appends an ellipsis ("...") if the string exceeds the maximum length.
     *
     * @param str - The string to be truncated.
     * @param maxLength - The maximum length of the string after truncation.
     * @returns The truncated string with an ellipsis appended if it exceeds the maximum length.
     */
    private truncateString(str: string, maxLength: number): string {
        if (str.length <= maxLength) {
            return str;
        }
        return str.substring(0, maxLength) + "...";
    }

    /**
     * Retrieves the current formatter configuration.
     *
     * @returns {FormatterConfig} A copy of the current configuration object.
     */
    public getConfig(): FormatterConfig {
        return { ...this.config };
    }

    // Make patterns available to formatters
    public readonly patterns = patterns;
}

/**
 * A class responsible for processing segments of text based on a given configuration.
 * It uses various regular expression patterns to identify and format different types of tokens.
 */
export class SegmentProcessor {
    /**
     * Configuration for the segment processor.
     */
    private readonly config: SegmentConfig;

    /**
     * State information for handling comments.
     */
    private commentState: CommentState = {
        inBlockComment: false,
        blockCommentDepth: 0,
    };

    /**
     * A collection of regular expression patterns used for formatting.
     *
     * @property whitespace - Matches leading whitespace characters.
     * @property incrementDecrement - Matches increment (++) or decrement (--) operators attached to a word.
     * @property colonMethodCall - Matches method calls with a colon (e.g., `s:format(`).
     * @property simpleMethodCall - Matches simple method calls (e.g., `usrmes(`).
     * @property propertyAccess - Matches property access with a colon (e.g., `object:property`).
     * @property keyword - Matches keywords that start with a colon and are not followed by another colon (e.g., `:keyword`).
     * @property identifier - Matches valid identifiers (e.g., variable names).
     * @property number - Matches numbers, including negative and decimal numbers.
     * @property stringStart - Matches the start of a string, either single or double quotes.
     * @property commentStart - Matches the start of a comment block (e.g., `/*`).
     * @property operator - Matches various operators (e.g., `:=`, `==`, `===`, `!=`, `>=`, `<=`, `&&`, `||`, `+`, `-`, `*`, `/`, `%`, `<`, `>`).
     * @property separator - Matches separators such as commas, semicolons, parentheses, brackets, and braces.
     * @property colon - Matches a single colon character.
     * @property logicalOperator - Matches logical operators in the form of `.T.`, `.F.`, `.AND.`, `.OR.`.
     */
    private readonly patterns = {
        whitespace: /^\s+/,
        incrementDecrement: /^(?:\+\+|--)\w+|^\w+(?:\+\+|--)/, // Moved up in the patterns list
        // Split method calls into two types
        colonMethodCall: /^[A-Za-z_]\w*:[A-Za-z_]\w*\s*\(/, // s:format(
        simpleMethodCall: /^[A-Za-z_]\w*\s*\(/, // usrmes(
        propertyAccess: /^[A-Za-z_]\w*:[A-Za-z_]\w*(?!\s*\()/,
        keyword: /^(?:^|[\s;]):(?![A-Za-z_]\w*:)[A-Za-z]+\b/i,
        identifier: /^[A-Za-z_]\w*/,
        number: /^-?\d*\.?\d+/,
        stringStart: /^["']/,
        commentStart: /^\/\*/,
        operator: /^(?::=|==|===|!=|>=|<=|&&|\|\||[+\-*/%=<>])/,
        separator: /^[,;()[\]{}]/,
        colon: /^:/, // Add the missing colon pattern
        logicalOperator: /^\.(?:T|F|AND|OR)\./i, // Add pattern for .T., .F., .AND., .OR.
    };

    constructor(config: SegmentConfig = DEFAULT_SEGMENT_CONFIG) {
        this.config = { ...DEFAULT_SEGMENT_CONFIG, ...config };
    }

    /**
     * Processes a line of text and returns an array of content segments.
     *
     * This method iterates through the given line, processes each token, and converts
     * recognized tokens into content segments. Unrecognized characters are skipped.
     * Finally, it merges adjacent segments before returning the result.
     *
     * @param line - The line of text to process.
     * @returns An array of content segments derived from the processed line.
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
     * Processes the next token in the given line starting from the specified index.
     *
     * The method follows a specific order of processing:
     * 1. Skips whitespace.
     * 2. Processes comments if they start with `/*`.
     * 3. Processes increment/decrement patterns.
     * 4. Processes keywords (e.g., `:parameters`, `:if`, etc.).
     * 5. Processes method calls and property access.
     * 6. Processes separators and operators.
     * 7. Processes values (strings, numbers, identifiers).
     * 8. Processes logical operators (e.g., `.T.`, `.F.`, etc.).
     *
     * If none of the above patterns match, it skips any unrecognized character.
     *
     * @param line - The line of code to process.
     * @param startIndex - The index to start processing from.
     * @returns A `ProcessingResult` object containing the token and the new index, or `null` if no token is recognized.
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

        // Process in correct order:
        // 1. Comments (only if they start with /*)
        if (remaining.startsWith("/*")) {
            return this.processComment(line, startIndex);
        }

        // 2. Increment/Decrement patterns (moved up in priority)
        const incDecMatch = remaining.match(this.patterns.incrementDecrement);
        if (incDecMatch) {
            return {
                token: {
                    type: "incrementDecrement",
                    content: incDecMatch[0],
                    startIndex,
                    endIndex: startIndex + incDecMatch[0].length,
                    breakable: false,
                },
                newIndex: startIndex + incDecMatch[0].length,
            };
        }

        // 3. Keywords (includes :parameters, :if, etc)
        const keywordResult = this.processKeyword(line, startIndex);
        if (keywordResult) {
            return keywordResult;
        }

        // 4. Method calls and property access
        const methodResult =
            this.processColonMethodCall(line, startIndex) ||
            this.processSimpleMethodCall(line, startIndex) ||
            this.processPropertyAccess(line, startIndex);
        if (methodResult) {
            return methodResult;
        }

        // 5. Separators and operators
        const operatorResult =
            this.processSeparator(line, startIndex) || this.processOperator(line, startIndex);
        if (operatorResult) {
            return operatorResult;
        }

        // 6. Values
        const valueResult =
            this.processString(line, startIndex) ||
            this.processNumber(line, startIndex) ||
            this.processIdentifier(line, startIndex);
        if (valueResult) {
            return valueResult;
        }

        // Check for logical operators first (.T., .F., etc)
        const logicalMatch = remaining.match(this.patterns.logicalOperator);
        if (logicalMatch) {
            return {
                token: {
                    type: "logicalOperator",
                    content: logicalMatch[0],
                    startIndex,
                    endIndex: startIndex + logicalMatch[0].length,
                    breakable: false,
                },
                newIndex: startIndex + logicalMatch[0].length,
            };
        }

        // Skip any unrecognized character
        return null;
    }

    /**
     * Processes a substring starting from a given index to identify and extract a string literal.
     *
     * @param line - The input string to process.
     * @param startIndex - The index at which to start processing the string.
     * @returns A `ProcessingResult` object containing the identified string literal and its metadata,
     *          or `null` if no string literal is found.
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
     * Processes a comment in the given line starting from the specified index.
     *
     * @param line - The line of text to process.
     * @param startIndex - The index in the line where the comment starts.
     * @returns A `ProcessingResult` object containing the comment token and the new index,
     *          or `null` if no comment start pattern is matched.
     */
    private processComment(line: string, startIndex: number): ProcessingResult | null {
        const remaining = line.slice(startIndex);
        const match = remaining.match(this.patterns.commentStart);
        if (!match) {
            return null;
        }

        // Find the comment part and the semicolon that ends it
        let endIndex = startIndex;
        let foundSemicolon = false;

        while (endIndex < line.length) {
            if (line[endIndex] === ";") {
                endIndex++;
                foundSemicolon = true;
                // Reset block comment state when we find the ending semicolon
                this.commentState.inBlockComment = false;
                this.commentState.blockCommentDepth = 0;
                break;
            }
            endIndex++;
        }

        // If no semicolon found, the comment continues
        if (!foundSemicolon) {
            this.commentState.inBlockComment = true;
            this.commentState.blockCommentDepth++;
            return {
                token: {
                    type: "comment",
                    content: line.slice(startIndex),
                    startIndex,
                    endIndex: line.length,
                    breakable: true,
                },
                newIndex: line.length,
            };
        }

        return {
            token: {
                type: "comment",
                content: line.slice(startIndex, endIndex),
                startIndex,
                endIndex,
                breakable: false, // Don't break inline comments
            },
            newIndex: endIndex,
        };
    }

    /**
     * Processes a keyword in the given line starting from the specified index.
     *
     * @param line - The line of text to process.
     * @param startIndex - The index in the line to start processing from.
     * @returns A `ProcessingResult` object containing the keyword token and the new index after the keyword,
     *          or `null` if no keyword is found.
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
     * Processes an operator in the given line starting from the specified index.
     *
     * @param line - The line of text to process.
     * @param startIndex - The index in the line where processing should start.
     * @returns A `ProcessingResult` object containing the operator token and the new index, or `null` if no operator is found.
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
     * Processes a colon operator in the given line starting from the specified index.
     *
     * @param line - The line of text to process.
     * @param startIndex - The index in the line to start processing from.
     * @returns A `ProcessingResult` object containing the token information and the new index,
     *          or `null` if no colon operator is found.
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
     * Processes a separator in the given line starting from the specified index.
     *
     * @param line - The line of text to process.
     * @param startIndex - The index in the line to start processing from.
     * @returns A `ProcessingResult` object containing the separator token and the new index,
     *          or `null` if no separator is found.
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
     * Processes an identifier in the given line starting from the specified index.
     *
     * @param line - The line of text to process.
     * @param startIndex - The index in the line where the identifier processing should start.
     * @returns A `ProcessingResult` object containing the identifier token and the new index,
     *          or `null` if no identifier is found.
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
     * Processes a number token from the given line starting at the specified index.
     *
     * @param line - The line of text to process.
     * @param startIndex - The index to start processing from.
     * @returns A `ProcessingResult` object containing the number token and the new index,
     *          or `null` if no number token is found.
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

    /**
     * Processes a method call in a given line of text starting from a specified index.
     *
     * @param line - The line of text to process.
     * @param startIndex - The index in the line where the method call starts.
     * @returns A `ProcessingResult` object containing details about the method call if a match is found, otherwise `null`.
     */
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

    /**
     * Processes a line of text to identify and extract a property access pattern starting from a given index.
     *
     * @param line - The line of text to process.
     * @param startIndex - The index in the line from which to start processing.
     * @returns A `ProcessingResult` object containing the identified property access token and the new index after processing,
     *          or `null` if no property access pattern is found.
     */
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

    /**
     * Processes a line of text to identify and extract a method call that uses a colon syntax.
     *
     * @param line - The line of text to process.
     * @param startIndex - The index in the line where the search for the method call should start.
     * @returns A `ProcessingResult` object containing details about the identified method call, or `null` if no method call is found.
     */
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

    /**
     * Processes a simple method call in a given line of code starting from a specified index.
     *
     * @param line - The line of code to process.
     * @param startIndex - The index in the line from which to start processing.
     * @returns A `ProcessingResult` object containing details about the method call if a match is found, otherwise `null`.
     */
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
     * Converts a given token into a content segment.
     *
     * @param token - The token to be converted.
     * @returns An object representing the content segment.
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
     * Determines if a given string is breakable based on certain criteria.
     *
     * @param content - The string to be evaluated.
     * @returns `true` if the string is breakable, `false` otherwise.
     *
     * The function performs the following checks:
     * - Removes the quotes from the string.
     * - Returns `false` if the string length (excluding quotes) is less than 20 characters.
     * - Returns `false` if the string consists of all uppercase letters and underscores.
     * - Returns `false` if the string is a single word (alphanumeric characters only).
     * - Returns `false` if the string starts with special characters ($, ?, #, @) followed by alphanumeric characters.
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
     * Merges adjacent content segments if they can be merged.
     *
     * This method iterates through the provided segments and merges adjacent segments
     * that satisfy the merging criteria defined by `canMergeSegments`. The merged segments
     * are then returned as a new array.
     *
     * @param segments - An array of content segments to be merged.
     * @returns An array of merged content segments.
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
     * Determines whether two content segments can be merged.
     *
     * @param a - The first content segment.
     * @param b - The second content segment.
     * @returns `true` if the segments can be merged, `false` otherwise.
     *
     * The segments can be merged if:
     * - They are of the same type.
     * - They have the same token type.
     * - They are contiguous (i.e., the end index of the first segment is the start index of the second segment).
     * - Their token type is not in the list of non-mergeable types.
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
     * Merges two content segments into one.
     *
     * @param a - The first content segment.
     * @param b - The second content segment.
     * @returns A new content segment that combines the content and end index of the two segments.
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
