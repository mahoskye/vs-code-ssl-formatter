/**
 * Handles line spacing formatting for STARLIMS code files
 */

// Interface for processed individual lines
interface ProcessedLine {
    originalString: string; // The unmodified line
    leadingWhitespace: string; // Leading whitespace from the original line
    trimmedContent: string; // Line with whitespace trimmed
}

// Interface for content blocks
interface ContentBlock {
    lines: ProcessedLine[]; // Array of lines in this block
    contentType: ContentType; // Type of content this block represents
}

// Interface for logic groups
interface LogicGroup {
    contentBlock: ContentBlock;
    blankLineCount: number; // Number of blank lines before next block
    nextBlockType?: ContentType; // Type of the next block (if any)
    nextFirstLine?: string; // First line of next block (if needed)
}

// Common regex patterns for rule matching
export const patterns = {
    comments: {
        blockComment: /^\/\*\*/,
        singleLineComment: /^\/\*/,
    },
    lines: {
        blankLine: /^\s*$/,
        newLine: /\?r\n/,
        multiLine: /[^;]\s*$/,
    },
    operators: {
        semicolon: /;/,
        assignment: /:=/,
    },
    regions: {
        regionStart: /^\/\*\s*region\b/i,
        regionEnd: /^\/\*\s*endregion\b/i,
    },
    spacing: {
        double: {
            after: /:(ENDPROC|ENDCASE|ENDWHILE|ENDIF|ENDREGION|RESUME|ERROR)\b/i,
            before: /:(PROCEDURE|ERROR)\b/i,
        },
        single: {
            after: /:(BEGINCASE|ERROR|EXITCASE)\b/i,
            before: /:(ELSE|ENDIF|ENDCASE|EXITCASE|RESUME)\b/i,
        },
        nospace: {
            after: null,
            before: /^:(CASE|OTHERWISE)\b/i,
        },
    },
    statements: {
        declarations: /^:(?:DECLARE|INCLUDE|DEFAULT|PARAMETERS)\b/i,
        logic: /:(IF|ELSE|WHILE|FOR|BEGINCASE|CASE|OTHERWISE|ENDCASE|ENDIF|ENDWHILE|EXITCASE|EXITWHILE)\b/i,
        return: /^:RETURN\b/i,
        procedureStart: /:(?:CLASS|PROCEDURE)\b/i,
        procedureEnd: /:ENDPROC\b/i,
        errorStart: /:ERROR\b/i,
        errorEnd: /:RESUME\b/i,
        caseStart: /:BEGINCASE\b/i,
        caseEnd: /:ENDCASE\b/i,
    },
} as const;

// Main formatter function
export function lineSpacingFormatter(text: string): string {
    // Split text into lines
    const lines = text.split(patterns.lines.newLine);

    // Process lines into content blocks
    const contentBlocks = createContentBlocks(lines);

    // Determine spacing requirements
    const spacedBlocks = determineSpacing(blocks);

    // Apply spacing and join blocks
    return formatBlocks(spacedBlocks);
}

function processLine(line: string): ProcessedLine {
    const trimmed = line.trim();
    const leadingWhitespace = line.substring(0, line.indexOf(trimmed));

    return {
        originalString: line,
        leadingWhitespace,
        trimmedContent: trimmed,
    };
}

function determineLineType(trimmedLine: string): LineType {
    if (patterns.lines.blankLine.test(trimmedLine)) {
        return LineType.blankLine;
    }

    if (patterns.comments.blockComment.test(trimmedLine)) {
        return LineType.blockComment;
    }

    if (patterns.comments.singleLineComment.test(trimmedLine)) {
        return LineType.singleLineComment;
    }

    if (patterns.statements.procedureStart.test(trimmedLine)) {
        return LineType.procedureStart;
    }

    if (patterns.statements.procedureEnd.test(trimmedLine)) {
        return LineType.procedureEnd;
    }

    if (patterns.statements.errorStart.test(trimmedLine)) {
        return LineType.errorStart;
    }

    if (patterns.statements.errorEnd.test(trimmedLine)) {
        return LineType.errorResume;
    }

    if (patterns.statements.caseStart.test(trimmedLine)) {
        return LineType.caseStart;
    }

    if (patterns.statements.caseEnd.test(trimmedLine)) {
        return LineType.caseEnd;
    }

    if (patterns.statements.declarations.test(trimmedLine)) {
        return LineType.declaration;
    }

    if (patterns.statements.logic.test(trimmedLine)) {
        return LineType.logic;
    }

    if (patterns.statements.return.test(trimmedLine)) {
        return LineType.return;
    }

    return LineType.code;
}

function determineContentType(lines: ProcessedLine[]): ContentType {
    const firstContentLine = lines.find((line) => line.lineType !== LineType.blankLine);

    if (!firstContentLine) {
        return ContentType.unknown;
    }

    switch (firstContentLine.lineType) {
        case LineType.blockComment:
        case LineType.singleLineComment:
            return ContentType.comment;

        case LineType.procedureStart:
        case LineType.procedureEnd:
            return ContentType.procedure;

        case LineType.errorStart:
        case LineType.errorResume:
            return ContentType.error;

        case LineType.caseStart:
        case LineType.caseEnd:
        case LineType.caseStatement:
        case LineType.exitCase:
        case LineType.elseStatement:
            return ContentType.controlStructure;

        default:
            if (patterns.statements.declarations.test(firstContentLine.trimmedContent)) {
                return ContentType.declaration;
            }
            return ContentType.statement;
    }
}

// Helper function to create logic groups from content blocks
function createLogicGroups(blocks: ContentBlock[]): LogicGroup[] {
    const groups: LogicGroup[] = [];

    // TODO: Implement grouping logic

    return groups;
}

function determineSpacing(blocks: ContentBlock[]): ContentBlock[] {
    // TODO: Implement spacing logic
    return blocks;
}

// Helper function to convert logic groups back to text
function convertToText(groups: LogicGroup[]): string {
    // TODO: Implement conversion logic
    return "";
}
