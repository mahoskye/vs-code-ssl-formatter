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

// Flow keyword definitions
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

// Declaration type definitions
export type DeclarationType = "parameters" | "declare" | "default" | "public" | "include" | "other";

// Comment type definitions
export type CommentType = "block" | "single" | "regionStart" | "regionEnd";

export interface FormatterConfig {
  useContext?: boolean;
  debug?: boolean;
  preserveUser?: boolean;
  maxConsecutiveBlank?: number;
}

export const DEFAULT_FORMATTER_CONFIG: FormatterConfig = {
  useContext: true,
  debug: true,
  preserveUser: false,
  maxConsecutiveBlank: 2,
};

export interface SpacingState {
  originalSpaces: number;
  standardSpaces: number;
  contextSpaces?: number;
  postProcessSpaces?: number;
  followingSpaces: number;
}

// Processed line information
export interface ProcessedLine {
  originalString: string;
  leadingWhitespace: string;
  trimmedContent: string;
  lineNumber: number;
}

// Block context information
export interface BlockContext {
  parentBlock?: TypedBlock;
  previousBlock?: TypedBlock;
  isPartOfChain: boolean;
  parentBlockType: BlockType | null;
  depth?: number;
  chainPosition?: "first" | "middle" | "last";
}

// Block metadata information
export interface BlockMetadata {
  flowType: FlowKeyword;
  isStart: boolean;
  isMiddle: boolean;
  isEnd: boolean;
  isCaseContent?: boolean;
  declarationType?: DeclarationType;
  commentType?: CommentType;
}

// Full block information
export interface TypedBlock {
  lines: ProcessedLine[];
  spacing: SpacingState;
  nextBlockFirstLine?: ProcessedLine;
  blockType: BlockType;
  metadata: BlockMetadata;
  context: BlockContext;
  startLineNumber: number;
  endLineNumber: number;
}

export interface ValidationResult {
  valid: boolean;
  warnings: Array<{ blockIndex: number; message: string; severity: "warning" | "error" }>;
}

export type FlowTransition = {
  type: FlowKeyword;
  requiresSpace: boolean;
  preserveOriginal: boolean;
  contextDependent: boolean;
};

/**
 * Pattern definitions for STARLIMS syntax
 */
export const patterns = {
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
      default: /:DEFAULT\b/i,
    },
    group: /^:(?:PARAMETERS|DEFAULT|DECLARE)\b/i,
  },
  logic: {
    assignment: /[:+-]=/,
    funtionCall: /\w+\s*\(/,
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

/**
 * Type guards for runtime type checking
 */
export const typeGuards = {
  isProcessedLine(obj: any): obj is ProcessedLine {
    return (
      typeof obj === "object" &&
      typeof obj.originalString === "string" &&
      typeof obj.leadingWhitespace === "string" &&
      typeof obj.trimmedContent === "string" &&
      typeof obj.lineNumber === "number"
    );
  },

  isTypedBlock(obj: any): obj is TypedBlock {
    return (
      typeof obj === "object" &&
      Array.isArray(obj.lines) &&
      typeof obj.spacing === "object" &&
      typeof obj.blockType === "string" &&
      typeof obj.metadata === "object" &&
      typeof obj.context === "object"
    );
  },

  isBlockMetadata(obj: any): obj is BlockMetadata {
    return (
      typeof obj === "object" &&
      typeof obj.flowType === "string" &&
      typeof obj.isStart === "boolean" &&
      typeof obj.isMiddle === "boolean" &&
      typeof obj.isEnd === "boolean"
    );
  },

  isBlockContext(obj: any): obj is BlockContext {
    return (
      typeof obj === "object" &&
      typeof obj.isPartOfChain === "boolean" &&
      (obj.parentBlockType === null || typeof obj.parentBlockType === "string")
    );
  },

  isSpacingState(obj: any): obj is SpacingState {
    return (
      typeof obj === "object" &&
      typeof obj.originalSpaces === "number" &&
      typeof obj.standardSpaces === "number" &&
      typeof obj.followingSpaces === "number"
    );
  },
};

// Main formatter function
export function lineSpacingFormatter(
  text: string,
  config: Partial<FormatterConfig> = {}
): string | ValidationResult {
  // Merge provided config with defaults
  const finalConfig = { ...DEFAULT_FORMATTER_CONFIG, ...config };

  const processor = new BlockProcessor(text, finalConfig);
  return processor.process();
}

// Block identifier class
/**
 * Class responsible for identifying block types and their metadata in code.
 */
class BlockIdentifier {
  static identify(lines: ProcessedLine[]): { blockType: BlockType; metadata: BlockMetadata } {
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
    if (patterns.logic.funtionCall.test(line)) {
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

  static shouldMergeWithPrevious(prevBlock: TypedBlock, currentBlock: TypedBlock): boolean {
    // Handle switch case statements
    if (prevBlock.blockType === "switch") {
      if (
        prevBlock.metadata.flowType === "caseBranch" ||
        prevBlock.metadata.flowType === "caseDefault"
      ) {
        return !this.isCaseOrCaseEnd(currentBlock);
      }

      if (currentBlock.blockType === "switch") {
        return true;
      }
    }

    // Handle declarations that should be grouped
    if (this.areMergeableDeclarations(prevBlock, currentBlock)) {
      return true;
    }

    // Handle control flow blocks
    if (this.areMergeableControlBlocks(prevBlock, currentBlock)) {
      return true;
    }

    return false;
  }

  private static isCaseOrCaseEnd(block: TypedBlock): boolean {
    return (
      block.blockType === "switch" &&
      ["caseBranch", "caseDefault", "caseEnd"].includes(block.metadata.flowType)
    );
  }

  private static areMergeableDeclarations(
    prevBlock: TypedBlock,
    currentBlock: TypedBlock
  ): boolean {
    if (prevBlock.blockType === "declaration" && currentBlock.blockType === "declaration") {
      const prevLine = prevBlock.lines[0].trimmedContent;
      const currentLine = currentBlock.lines[0].trimmedContent;

      // Check if declarations are of the same type
      const shouldMergeDeclarations =
        patterns.declaration.group.test(prevLine) &&
        patterns.declaration.group.test(currentLine) &&
        prevBlock.metadata.declarationType === currentBlock.metadata.declarationType;

      // Check if they're assignment declarations
      const shouldMergeAssignments =
        patterns.logic.assignment.test(prevLine) && patterns.logic.assignment.test(currentLine);

      return shouldMergeDeclarations || shouldMergeAssignments;
    }
    return false;
  }

  private static areMergeableControlBlocks(
    prevBlock: TypedBlock,
    currentBlock: TypedBlock
  ): boolean {
    const controlTypes = ["conditional", "loop", "switch", "errorHandling"];

    if (
      controlTypes.includes(prevBlock.blockType) &&
      prevBlock.blockType === currentBlock.blockType
    ) {
      return (
        prevBlock.metadata.flowType === currentBlock.metadata.flowType ||
        currentBlock.context.isPartOfChain
      );
    }
    return false;
  }
}

// Base class for all rules
/**
 * Abstract base class for spacing rules
 */
abstract class SpacingRule {
  constructor(protected config: FormatterConfig) {}

  /**
   * Apply spacing rules to a block of code
   * @param block The current block being processed
   * @param nextBlock Optional next block in the sequence
   */
  abstract applyRule(block: TypedBlock, nextBlock?: TypedBlock): void;

  /**
   * Update block spacing while preserving state
   */
  protected updateSpacing(block: TypedBlock, spaces: number): void {
    block.spacing.followingSpaces = spaces;
  }

  /**
   * Check if a line contains specific keywords
   */
  protected containsKeywords(line: string, keywords: string[]): boolean {
    const normalizedLine = line.toLowerCase();
    return keywords.some((keyword) => normalizedLine.includes(keyword.toLowerCase()));
  }

  /**
   * Count the number of non-whitespace lines in a block
   */
  protected getContentLineCount(block: TypedBlock): number {
    return block.lines.filter((line) => line.trimmedContent.length > 0).length;
  }

  /**
   * Check if a block is empty (contains only whitespace)
   */
  protected isEmptyBlock(block: TypedBlock): boolean {
    return block.lines.every((line) => line.trimmedContent.length === 0);
  }

  /**
   * Check if a block contains only a single statement
   */
  protected isSingleStatement(block: TypedBlock): boolean {
    const contentLines = block.lines.filter((line) => line.trimmedContent.length > 0);
    return contentLines.length === 1;
  }

  /**
   * Check if a block opens a new scope
   */
  protected isBlockStart(block: TypedBlock): boolean {
    return block.metadata.isStart;
  }

  /**
   * Check if a block closes a scope
   */
  protected isBlockEnd(block: TypedBlock): boolean {
    return block.metadata.isEnd;
  }

  /**
   * Check if a block is in the middle of a control structure
   */
  protected isBlockMiddle(block: TypedBlock): boolean {
    return block.metadata.isMiddle;
  }

  /**
   * Check if a block is a block comment
   */
  protected isBlockComment(block: TypedBlock): boolean {
    return block.blockType === "comment" && block.metadata.commentType === "block";
  }

  /**
   * Check if a block is a region comment
   */
  protected isRegionComment(block: TypedBlock): boolean {
    return (
      block.blockType === "comment" &&
      (block.metadata.commentType === "regionStart" || block.metadata.commentType === "regionEnd")
    );
  }

  /**
   * Check if a block is a control structure
   */
  protected isControlStructure(block: TypedBlock): boolean {
    return ["conditional", "loop", "switch", "errorHandling"].includes(block.blockType);
  }

  /**
   * Check if a block is part of a chain (like IF-ELSE or CASE)
   */
  protected isPartOfChain(block: TypedBlock): boolean {
    return block.context.isPartOfChain;
  }

  /**
   * Check if a block should preserve its original spacing
   */
  protected shouldPreserveSpacing(block: TypedBlock): boolean {
    if (!this.config.preserveUser) {
      return false;
    }

    // Preserve spacing in block comments
    if (this.isBlockComment(block)) {
      return true;
    }

    // Preserve spacing in explicitly separated declarations
    if (block.blockType === "declaration" && block.spacing.originalSpaces > 0) {
      return true;
    }

    return false;
  }

  /**
   * Check if a block is a section separator
   */
  // ! We don't really have a convention for section separators, but I like it enough to keep it
  // ! We should set it up with a 2 before 2 after rule
  protected isSectionSeparator(block: TypedBlock): boolean {
    if (block.blockType !== "comment") {
      return false;
    }

    const content = block.lines[0].trimmedContent;
    return (
      content.includes("=".repeat(20)) ||
      content.includes("-".repeat(20)) ||
      content.includes("*".repeat(20))
    );
  }
}

// Standard rules that don't require context
/**
 * Handles standard spacing rules that don't require broader context
 */
class StandardSpacingRules extends SpacingRule {
  constructor(config: FormatterConfig) {
    super(config);
  }

  applyRule(block: TypedBlock, nextBlock?: TypedBlock): void {
    // Store original spacing for later reference
    const originalSpacing = block.spacing.followingSpaces;

    // Apply rules in sequence
    this.applyFileStartRule(block);
    this.applyConsecutiveBlankLinesRule(block);
    this.applyRegionSpacingRule(block);
    this.applyErrorBlockSpacingRule(block);
    this.applyBasicControlStructureRule(block);
    this.applyBasicCommentRule(block, nextBlock);
    this.applyFileEndRule(block, nextBlock);

    // Preserve original spacing if necessary
    if (this.shouldPreserveSpacing(block)) {
      this.updateSpacing(block, originalSpacing);
    }
  }

  /**
   * Remove blank lines at start of file
   */
  private applyFileStartRule(block: TypedBlock): void {
    if (!block.context.previousBlock) {
      while (block.lines[0]?.trimmedContent === "") {
        block.lines.shift();
      }
    }
  }

  /**
   * Enforce maximum of two consecutive blank lines
   */
  private applyConsecutiveBlankLinesRule(block: TypedBlock): void {
    this.updateSpacing(block, Math.min(block.spacing.followingSpaces, 2));
  }

  /**
   * Apply region comment spacing rules
   */
  private applyRegionSpacingRule(block: TypedBlock): void {
    if (block.blockType !== "comment") {
      return;
    }

    if (block.metadata.commentType === "regionStart") {
      if (block.context.previousBlock) {
        this.updateSpacing(block.context.previousBlock, 1);
      }
      this.updateSpacing(block, 1);
    } else if (block.metadata.commentType === "regionEnd") {
      if (block.context.previousBlock) {
        this.updateSpacing(block.context.previousBlock, 1);
      }
      this.updateSpacing(block, 2);
    }
  }

  /**
   * Apply error handling block spacing rules
   */
  private applyErrorBlockSpacingRule(block: TypedBlock): void {
    if (block.blockType !== "errorHandling") {
      return;
    }

    if (block.metadata.flowType === "error") {
      if (block.context.previousBlock) {
        this.updateSpacing(block.context.previousBlock, 2);
      }
      this.updateSpacing(block, 1);
    } else if (block.metadata.flowType === "resume") {
      if (block.context.previousBlock) {
        this.updateSpacing(block.context.previousBlock, 1);
      }
      this.updateSpacing(block, 1);
    }
  }

  /**
   * Apply basic control structure spacing rules
   */
  private applyBasicControlStructureRule(block: TypedBlock): void {
    if (block.blockType === "switch") {
      this.applySwitchBlockRules(block);
    } else if (block.blockType === "conditional") {
      this.applyConditionalBlockRules(block);
    }
  }

  /**
   * Apply switch block spacing rules
   */
  private applySwitchBlockRules(block: TypedBlock): void {
    if (block.metadata.flowType === "caseStart") {
      this.updateSpacing(block, 1);
    } else if (block.metadata.flowType === "caseEnd" && block.context.previousBlock) {
      this.updateSpacing(block.context.previousBlock, 1);
    }
  }

  /**
   * Apply conditional block spacing rules
   */
  private applyConditionalBlockRules(block: TypedBlock): void {
    const isEndOfBlock = block.metadata.flowType === "else" || block.metadata.flowType === "ifEnd";

    if (isEndOfBlock && block.context.previousBlock) {
      this.updateSpacing(block.context.previousBlock, 1);
    }
  }

  /**
   * Apply basic comment spacing rules
   */
  private applyBasicCommentRule(block: TypedBlock, nextBlock?: TypedBlock): void {
    if (block.blockType !== "comment") {
      return;
    }

    const isPreviousComment = block.context.previousBlock?.blockType === "comment";

    // One blank line before any comment unless it follows another comment
    if (!isPreviousComment && block.context.previousBlock) {
      this.updateSpacing(block.context.previousBlock, 1);
    }

    // Block comments always get spacing
    if (block.metadata.commentType === "block") {
      // Block comments followed by two blank lines
      this.updateSpacing(block, 2);
    } else {
      // Single line comments followed by one blank line unless
      // followed by another comment
      const spacing = block.spacing.originalSpaces === 0 ? 1 : block.spacing.originalSpaces;
      this.updateSpacing(block, spacing);
    }
  }

  /**
   * Ensure exactly one blank line at end of file
   */
  private applyFileEndRule(block: TypedBlock, nextBlock?: TypedBlock): void {
    if (!nextBlock) {
      this.updateSpacing(block, 1);
    }
  }
}

// Context-aware rules that require surrounding content knowledge
/**
 * Handles context-dependent spacing rules that require awareness of surrounding blocks
 */
class ContextualSpacingRules extends SpacingRule {
  constructor(config: FormatterConfig) {
    super(config);
  }

  applyRule(block: TypedBlock, nextBlock?: TypedBlock): void {
    // Store original spacing for potential preservation
    const originalSpacing = block.spacing.followingSpaces;

    // Apply contextual rules
    this.applyDeclarationSpacingRule(block, nextBlock);
    this.applyProcedureBlockSpacingRule(block, nextBlock);
    this.applyAdvancedControlStructureRule(block, nextBlock);
    this.applyContextualCommentRule(block, nextBlock);
    this.applyChainedBlockRule(block, nextBlock);

    // Preserve original spacing if necessary
    if (this.shouldPreserveSpacing(block)) {
      this.updateSpacing(block, originalSpacing);
    }
  }

  /**
   * Apply declaration spacing rules
   */
  private applyDeclarationSpacingRule(block: TypedBlock, nextBlock?: TypedBlock): void {
    if (block.blockType !== "declaration") {
      return;
    }

    const currentType = block.metadata.declarationType;
    const nextType = nextBlock?.metadata.declarationType;
    const isExplicitlySeparated = block.spacing.originalSpaces > 0;

    if (nextBlock?.blockType === "declaration") {
      if (currentType === nextType && !isExplicitlySeparated) {
        // No blank lines between same type declarations unless explicitly separated
        this.updateSpacing(block, 0);
      } else if (currentType !== nextType) {
        // One blank line between different declaration types
        this.updateSpacing(block, 1);
      }
    } else if (nextBlock && !this.isDeclarationRelated(nextBlock)) {
      // Two blank lines after last declaration before main code
      this.updateSpacing(block, 2);
    }
  }

  /**
   * Apply procedure block spacing rules
   */
  private applyProcedureBlockSpacingRule(block: TypedBlock, nextBlock?: TypedBlock): void {
    if (block.blockType !== "procedure") {
      return;
    }

    if (block.metadata.isStart) {
      // Two blank lines before procedure unless it's first or after separator
      if (block.context.previousBlock && !this.isSectionSeparator(block.context.previousBlock)) {
        this.updateSpacing(block.context.previousBlock, 2);
      }
      // One blank line after procedure declaration
      this.updateSpacing(block, 1);
    } else if (block.metadata.isEnd) {
      // One blank line before return
      if (block.context.previousBlock) {
        this.updateSpacing(block.context.previousBlock, 1);
      }
      // Two blank lines after procedure end unless it's last
      this.updateSpacing(block, nextBlock ? 2 : 1);
    }
  }

  /**
   * Apply advanced control structure spacing rules
   */
  private applyAdvancedControlStructureRule(block: TypedBlock, nextBlock?: TypedBlock): void {
    // Check for control structure start
    if (block.metadata.isStart || block.metadata.flowType === "ifStart") {
      if (
        block.context.previousBlock &&
        !this.isConsecutiveComment(block.context.previousBlock, block)
      ) {
        this.updateSpacing(block.context.previousBlock, 1);
      }
    }

    if (block.blockType === "switch") {
      this.applySwitchCaseRules(block, nextBlock);
    } else if (block.blockType === "conditional") {
      this.applyConditionalRules(block, nextBlock);
    }
  }

  /**
   * Apply switch case spacing rules
   */
  private applySwitchCaseRules(block: TypedBlock, nextBlock?: TypedBlock): void {
    const isCaseOrOtherwise =
      block.metadata.flowType === "caseBranch" || block.metadata.flowType === "caseDefault";

    if (isCaseOrOtherwise && nextBlock?.blockType !== "switch") {
      // No blank line after CASE/OTHERWISE when followed by code
      this.updateSpacing(block, 0);
    } else if (block.metadata.flowType === "exitCase") {
      // One blank line before and after EXITCASE
      if (block.context.previousBlock) {
        this.updateSpacing(block.context.previousBlock, 1);
      }
      this.updateSpacing(block, 1);
    }
  }

  /**
   * Apply conditional statement spacing rules
   */
  private applyConditionalRules(block: TypedBlock, nextBlock?: TypedBlock): void {
    if (block.metadata.flowType === "else" && this.hasNestedControl(nextBlock)) {
      // Add space when ELSE is followed by nested control
      this.updateSpacing(block, 1);
    }
  }

  /**
   * Apply contextual comment spacing rules
   */
  private applyContextualCommentRule(block: TypedBlock, nextBlock?: TypedBlock): void {
    if (block.blockType !== "comment") {
      return;
    }

    if (this.isRelatedCodeComment(block, nextBlock)) {
      // No blank line after comments describing following code
      this.updateSpacing(block, 0);
    } else if (this.isConsecutiveComment(block, nextBlock)) {
      // No blank line between consecutive comments
      this.updateSpacing(block, 0);
    }
  }

  /**
   * Apply chained block spacing rules
   */
  private applyChainedBlockRule(block: TypedBlock, nextBlock?: TypedBlock): void {
    if (!block.context.isPartOfChain || !nextBlock) {
      return;
    }

    if (block.blockType === "conditional" && nextBlock.metadata.flowType === "else") {
      this.updateSpacing(block, 0);
    } else if (block.blockType === "switch" && this.isCaseStatement(nextBlock)) {
      this.updateSpacing(block, 0);
    }
  }

  /**
   * Helper method to check if block is declaration-related
   */
  private isDeclarationRelated(block: TypedBlock): boolean {
    return (
      block.blockType === "declaration" ||
      (block.blockType === "comment" && this.isDeclarationComment(block))
    );
  }

  /**
   * Helper method to check if a comment is declaration-related
   */
  // ! We should be checking to see if the type that follows the comment is a declaration or logic
  private isDeclarationComment(block: TypedBlock): boolean {
    const commentText = block.lines[0].trimmedContent.toLowerCase();
    return (
      commentText.includes("parameter") ||
      commentText.includes("declare") ||
      commentText.includes("variable") ||
      commentText.includes("constant")
    );
  }

  /**
   * Helper method to check if a block has nested control structures
   */
  // ! We should probably be using the pattern to check for nested control structures
  private hasNestedControl(block?: TypedBlock): boolean {
    if (!block) {
      return false;
    }

    const content = block.lines[0].trimmedContent.toLowerCase();
    return content.includes(":if") || content.includes(":while") || content.includes(":for");
  }

  /**
   * Helper method to check if a comment is related to following code
   */
  // ! This is complicating the logic too much. Block comments should always have space
  // ! after them but single line comments can be followed by code of any type
  // ! Maybe this is more of a post-processing step?
  // ! do a single space after a single line always, unless the user chose to have no space
  private isRelatedCodeComment(block: TypedBlock, nextBlock?: TypedBlock): boolean {
    if (!nextBlock) {
      return false;
    }

    const commentText = block.lines[0].trimmedContent.toLowerCase();
    const nextCode = nextBlock.lines[0].trimmedContent.toLowerCase();

    return (
      !block.metadata.commentType?.startsWith("block") &&
      ((commentText.includes("initialize") && nextCode.includes(":=")) ||
        (commentText.includes("check") &&
          (nextCode.includes(":if") || nextCode.includes(":while"))) ||
        (commentText.includes("loop") &&
          (nextCode.includes(":for") || nextCode.includes(":while"))) ||
        (commentText.includes("handle") && nextCode.includes(":try")))
    );
  }

  /**
   * Helper method to check if comments are consecutive
   */
  private isConsecutiveComment(block: TypedBlock, nextBlock?: TypedBlock): boolean {
    return (
      nextBlock?.blockType === "comment" &&
      block.metadata.commentType !== "block" &&
      block.metadata.commentType !== "regionStart" &&
      block.metadata.commentType !== "regionEnd" &&
      nextBlock.metadata.commentType !== "block" &&
      nextBlock.metadata.commentType !== "regionStart" &&
      nextBlock.metadata.commentType !== "regionEnd"
    );
  }

  /**
   * Helper method to check if a block is a case statement
   */
  private isCaseStatement(block: TypedBlock): boolean {
    return block.metadata.flowType === "caseBranch" || block.metadata.flowType === "caseDefault";
  }
}

// Spacing rules engine with updated implementation
/**
 * Coordinates the application of spacing rules
 */
export class SpacingRulesProcessor {
  private standardRules: StandardSpacingRules;
  private contextualRules: ContextualSpacingRules;
  private validationResult: ValidationResult = {
    valid: true,
    warnings: [],
  };

  constructor(private config: FormatterConfig) {
    this.standardRules = new StandardSpacingRules(config);
    this.contextualRules = new ContextualSpacingRules(config);
  }

  /**
   * Process blocks and apply spacing rules
   */
  processBlocks(blocks: TypedBlock[]): ValidationResult {
    if (blocks.length === 0) {
      return this.validationResult;
    }

    // First pass: Set up block relationships
    this.establishBlockRelationships(blocks);

    // Second pass: Apply spacing rules
    this.applySpacingRules(blocks);

    // Final pass: Validate and normalize spacing
    this.validateAndNormalizeSpacing(blocks);

    return this.validationResult;
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

      // Update the block depth
      if (block.metadata.isStart) {
        depth++;
      }

      // Determine block context
      const context = this.determineBlockContext(
        block,
        previousBlock,
        currentProcedure,
        currentControlBlock,
        depth
      );

      // Update block relationships with null safety
      const isChainable = previousBlock ? this.isChainable(previousBlock, block) : false;
      const chainPosition = this.determineChainPosition(block, previousBlock, nextBlock);

      // Update block relationships
      block.context = {
        ...context,
        previousBlock,
        isPartOfChain: isChainable,
        chainPosition: chainPosition,
      };

      // Track procedure blocks
      if (block.blockType === "procedure") {
        if (block.metadata.isStart) {
          currentProcedure = block;
          blockStack.push(block);
        } else if (block.metadata.isEnd) {
          currentProcedure = blockStack.length > 1 ? blockStack[blockStack.length - 2] : null;
          blockStack.pop();
        }
      }

      // Track control structure blocks
      if (this.isControlStructure(block)) {
        if (block.metadata.isStart) {
          currentControlBlock = block;
          blockStack.push(block);
        } else if (block.metadata.isEnd) {
          currentControlBlock = blockStack.length > 1 ? blockStack[blockStack.length - 2] : null;
          blockStack.pop();
          depth = Math.max(0, depth - 1);
        }
      }

      // Set up chainable relationships
      if (previousBlock && this.isChainable(previousBlock, block)) {
        this.establishChainRelationship(previousBlock, block);
      }
    });
  }

  private determineChainPosition(
    block: TypedBlock,
    previousBlock?: TypedBlock,
    nextBlock?: TypedBlock
  ): "first" | "middle" | "last" | undefined {
    // First check if any block is undefined
    if (!block || (!previousBlock && !nextBlock)) {
      return undefined;
    }

    // Check chainability with null safety
    const isPrevChainable = previousBlock ? this.isChainable(previousBlock, block) : false;
    const isNextChainable = nextBlock ? this.isChainable(block, nextBlock) : false;

    if (!isPrevChainable && !isNextChainable) {
      return undefined;
    }
    if (!isPrevChainable) {
      return "first";
    }
    if (!isNextChainable) {
      return "last";
    }
    return "middle";
  }

  /**
   * Determine context for a block
   */
  private determineBlockContext(
    block: TypedBlock,
    previousBlock?: TypedBlock,
    currentProcedure?: TypedBlock | null,
    currentControlBlock?: TypedBlock | null,
    depth?: number
  ): BlockContext {
    return {
      parentBlock: currentProcedure || currentControlBlock || undefined,
      isPartOfChain: false,
      parentBlockType: currentProcedure?.blockType || currentControlBlock?.blockType || null,
      previousBlock: previousBlock,
      depth: depth || 0,
    };
  }

  /**
   * Check if a block is a control structure
   */
  private isControlStructure(block: TypedBlock): boolean {
    return ["conditional", "loop", "switch", "errorHandling"].includes(block.blockType);
  }

  /**
   * Apply spacing rules to blocks
   */
  private applySpacingRules(blocks: TypedBlock[]): void {
    blocks.forEach((block, index) => {
      const nextBlock = index < blocks.length - 1 ? blocks[index + 1] : undefined;

      // Store the original spacing before any rules are applied
      block.spacing.originalSpaces = block.spacing.followingSpaces;

      // Apply standard rules first
      this.standardRules.applyRule(block, nextBlock);
      block.spacing.standardSpaces = block.spacing.followingSpaces;

      // Then apply contextual rules if enabled
      if (this.config.useContext) {
        this.contextualRules.applyRule(block, nextBlock);
        block.spacing.contextSpaces = block.spacing.followingSpaces;
      }

      // TODO: Post Processing
    });
  }

  /**
   * Validate and normalize block spacing
   */
  private validateAndNormalizeSpacing(blocks: TypedBlock[]): void {
    blocks.forEach((block, index) => {
      const originalSpacing = block.spacing.followingSpaces;

      // Ensure no negative spacing
      block.spacing.followingSpaces = Math.max(0, originalSpacing);

      // Apply maximum consecutive blank lines rule
      if (index > 0) {
        const prevBlock = blocks[index - 1];
        const combinedSpacing =
          prevBlock.spacing.followingSpaces + (block.lines[0].trimmedContent === "" ? 1 : 0);

        if (combinedSpacing > this.config.maxConsecutiveBlank!) {
          const reduction = combinedSpacing - this.config.maxConsecutiveBlank!;
          prevBlock.spacing.followingSpaces = Math.max(
            0,
            prevBlock.spacing.followingSpaces - reduction
          );
        }
      }

      // Special handling for file boundaries
      if (index === 0 || index === blocks.length - 1) {
        this.handleFileBoundaries(block, index === 0, index === blocks.length - 1);
      }

      // Validate final spacing matches expectations
      this.validateBlockSpacing(block, index);
    });
  }

  private validateBlockSpacing(block: TypedBlock, index: number): void {
    // Check for inconsistencies between spacing states
    if (
      this.config.useContext &&
      block.spacing.contextSpaces !== undefined &&
      block.spacing.followingSpaces !== block.spacing.contextSpaces
    ) {
      this.validationResult.warnings.push({
        blockIndex: index,
        message:
          `Block ${index}: Final spacing (${block.spacing.followingSpaces}) ` +
          `doesn't match contextual spacing (${block.spacing.contextSpaces})`,
        severity: "warning",
      });
    }

    // Check for unexpected large gaps
    if (block.spacing.followingSpaces > this.config.maxConsecutiveBlank!) {
      this.validationResult.warnings.push({
        blockIndex: index,
        message:
          `Block ${index}: Spacing (${block.spacing.followingSpaces}) ` +
          `exceeds maximum (${this.config.maxConsecutiveBlank})`,
        severity: "warning",
      });
    }
  }

  /**
   * Handle file boundary spacing
   */
  private handleFileBoundaries(block: TypedBlock, isFirst: boolean, isLast: boolean): void {
    if (isFirst) {
      // Remove leading blank lines at start of file
      while (block.lines[0]?.trimmedContent === "") {
        block.lines.shift();
      }
    }

    if (isLast) {
      // Ensure exactly one trailing blank line
      block.spacing.followingSpaces = 1;
    }
  }

  /**
   * Check if blocks can be chained
   */
  private isChainable(prev: TypedBlock | undefined, current: TypedBlock | undefined): boolean {
    // Return false if either block is undefined
    if (!prev || !current) {
      return false;
    }

    // Handle if/else chains
    if (prev.blockType === "conditional" && current.blockType === "conditional") {
      return current.metadata.flowType === "else";
    }

    // Handle case chains
    if (prev.blockType === "switch" && current.blockType === "switch") {
      return ["caseBranch", "caseDefault"].includes(current.metadata.flowType);
    }

    // Handle declaration chains
    if (prev.blockType === "declaration" && current.blockType === "declaration") {
      const prevType = prev.metadata.declarationType;
      const currentType = current.metadata.declarationType;
      return prevType === currentType && prev.spacing.originalSpaces === 0;
    }

    return false;
  }

  /**
   * Establish chain relationship between blocks
   */
  private establishChainRelationship(prev: TypedBlock, current: TypedBlock): void {
    if (prev.blockType === "conditional" && current.metadata.flowType === "else") {
      prev.spacing.followingSpaces = 0;
      current.context.isPartOfChain = true;
    } else if (
      prev.blockType === "switch" &&
      ["caseBranch", "caseDefault"].includes(current.metadata.flowType)
    ) {
      prev.spacing.followingSpaces = 0;
      current.context.isPartOfChain = true;
    }
  }
}

// Block processor class
/**
 * Handles processing of code blocks for formatting
 */
class BlockProcessor {
  private blocks: TypedBlock[] = [];
  private rulesProcessor: SpacingRulesProcessor;
  private config: FormatterConfig;
  private validationResult: ValidationResult = {
    valid: true,
    warnings: [],
  };

  constructor(private text: string, config: FormatterConfig) {
    this.config = config;
    this.rulesProcessor = new SpacingRulesProcessor(config);
  }

  /**
   * Process input text and apply formatting rules
   */
  public process(): string | ValidationResult {
    try {
      this.blocks = this.splitIntoBlocks();
      this.rulesProcessor.processBlocks(this.blocks);

      // If in debug mode, return validation result
      if (this.config.debug) {
        return this.formatDebug();
      }
      return this.format();
    } catch (error: unknown) {
      this.validationResult.valid = false;
      this.validationResult.warnings.push({
        blockIndex: -1,
        message: error instanceof Error ? error.message : String(error),
        severity: "error",
      });

      if (this.config.debug) {
        return this.validationResult;
      }

      throw error;
    }
  }

  private createSpacingState(followingSpaces: number): SpacingState {
    return {
      originalSpaces: followingSpaces,
      standardSpaces: followingSpaces,
      followingSpaces: followingSpaces,
    };
  }

  /**
   * Split input text into logical blocks
   */
  private splitIntoBlocks(): TypedBlock[] {
    const lines = this.text.split(patterns.structure.newLine);
    const blocks: TypedBlock[] = [];
    let currentBlock: ProcessedLine[] = [];
    let isBlockComment = false;
    let isMultiLineStatement = false;

    for (let i = 0; i < lines.length; i++) {
      const line = this.processLine(lines[i], i);

      // Skip empty lines at start of file
      if (currentBlock.length === 0 && line.trimmedContent === "") {
        continue;
      }

      // Handle block comments
      if (patterns.comment.block.test(line.trimmedContent)) {
        isBlockComment = true;
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
        isBlockComment = false;
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

    return {
      originalString: line,
      leadingWhitespace,
      trimmedContent: trimmed,
      lineNumber: lineIndex + 1,
    };
  }

  /**
   * Check if current line ends a block
   */
  private isBlockEnd(line: ProcessedLine): boolean {
    const content = line.trimmedContent;

    // Just check for semi-colons at the end of statement
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
    const blankCount = this.countFollowingBlankLines(allLines, currentIndex);
    const nextLine = this.getNextContentLine(allLines, currentIndex + blankCount + 1);

    const blockIdentification = BlockIdentifier.identify(lines);

    return {
      lines,
      spacing: this.createSpacingState(blankCount),
      nextBlockFirstLine: nextLine,
      ...blockIdentification,
      context: {
        isPartOfChain: false,
        parentBlockType: null,
        previousBlock: undefined,
      },
      startLineNumber: startLine,
      endLineNumber: endLine,
    };
  }

  /**
   * Count blank lines following a block
   */
  private countFollowingBlankLines(lines: string[], currentIndex: number): number {
    let count = 0;
    let index = currentIndex + 1;

    while (index < lines.length && patterns.structure.blankLine.test(lines[index])) {
      count++;
      index++;
    }

    return count;
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
    target.spacing.originalSpaces = source.spacing.originalSpaces;
    target.spacing.followingSpaces = source.spacing.followingSpaces;

    if (target.blockType === "switch" && source.metadata.isCaseContent) {
      target.metadata = {
        ...target.metadata,
        isCaseContent: true,
      };
    }
  }

  /**
   * Format the processed blocks into final output
   */
  private format(): string {
    let result = "";

    this.blocks.forEach((block, index) => {
      // Add each line
      block.lines.forEach((line, lineIndex) => {
        result += line.originalString;
        if (!(index === this.blocks.length - 1 && lineIndex === block.lines.length - 1)) {
          result += "\n";
        }
      });

      // Add block spacing
      if (index < this.blocks.length - 1) {
        result += "\n".repeat(block.spacing.followingSpaces);
      }
    });

    // Ensure exactly one trailing newline
    return result.trimEnd() + "\n";
  }
  private formatDebug(): string {
    const output: string[] = [];

    output.push("Block Analysis:");
    output.push("==============");
    output.push(
      "`block`|`type`|`flow`|`origSpacing`|`stdSpacing`|`ctxSpacing`|`currSpacing`" +
        "|`startLine`|`endLine`|`firstLine`|`isChain`"
    );

    this.blocks.forEach((block, index) => {
      const firstLine = block.lines[0].trimmedContent;
      const nextLine = block.nextBlockFirstLine?.trimmedContent || "END";
      const truncateLength = 30;

      // Build the debug info string
      const debugInfo = [
        `${index + 1}`,
        `${block.blockType}`,
        `${block.metadata.flowType}`,
        `${block.spacing.originalSpaces}`,
        `${block.spacing.standardSpaces}`,
        `${block.spacing.contextSpaces}`,
        `${block.spacing.followingSpaces}`,
        `${block.startLineNumber}`,
        `${block.endLineNumber}`,
        `${this.truncateString(firstLine, truncateLength)}`,
        `${this.truncateString(nextLine, truncateLength)}`,
        `${block.context.isPartOfChain}`,
      ].join("`|`");

      output.push("`" + debugInfo + "`");
    });

    return output.join("\n");
  }

  private truncateString(str: string, maxLength: number): string {
    if (str.length <= maxLength) {
      return str;
    }
    return str.substring(0, maxLength) + "...";
  }
}

// Error types
/**
 * Base class for all formatter-related errors
 */
export class FormatterError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "FormatterError";
  }
}

/**
 * Error thrown during block processing operations
 */
export class BlockProcessingError extends FormatterError {
  constructor(
    message: string,
    public readonly lineNumber?: number,
    public readonly blockType?: BlockType
  ) {
    super(`Block processing error${lineNumber ? ` at line ${lineNumber}` : ""}: ${message}`);
    this.name = "BlockProcessingError";
  }
}

/**
 * Error thrown during pattern matching operations
 */
export class PatternMatchError extends FormatterError {
  constructor(message: string, public readonly pattern: RegExp, public readonly text: string) {
    super(`Pattern matching error: ${message}\nPattern: ${pattern}\nText: ${text}`);
    this.name = "PatternMatchError";
  }
}

/**
 * Error thrown during rule application
 */
export class RuleApplicationError extends FormatterError {
  constructor(
    message: string,
    public readonly ruleName: string,
    public readonly blockInfo?: {
      type: BlockType;
      lineNumber: number;
      content: string;
    }
  ) {
    super(`Rule application error in ${ruleName}: ${message}`);
    this.name = "RuleApplicationError";
  }
}

/**
 * Error thrown when block merging fails
 */
export class BlockMergeError extends BlockProcessingError {
  constructor(
    message: string,
    public readonly sourceBlock: {
      type: BlockType;
      startLine: number;
      endLine: number;
    },
    public readonly targetBlock: {
      type: BlockType;
      startLine: number;
      endLine: number;
    }
  ) {
    super(
      `Failed to merge blocks: ${message}\n` +
        `Source block (${sourceBlock.type}): lines ${sourceBlock.startLine}-${sourceBlock.endLine}\n` +
        `Target block (${targetBlock.type}): lines ${targetBlock.startLine}-${targetBlock.endLine}`
    );
    this.name = "BlockMergeError";
  }
}

/**
 * Helper function to create appropriate error instance
 */
export function createFormatterError(
  type: "block" | "pattern" | "rule" | "merge",
  message: string,
  details?: any
): FormatterError {
  switch (type) {
    case "block":
      return new BlockProcessingError(message, details?.lineNumber, details?.blockType);
    case "pattern":
      return new PatternMatchError(message, details?.pattern, details?.text);
    case "rule":
      return new RuleApplicationError(message, details?.ruleName, details?.blockInfo);
    case "merge":
      return new BlockMergeError(message, details?.sourceBlock, details?.targetBlock);
    default:
      return new FormatterError(message);
  }
}
