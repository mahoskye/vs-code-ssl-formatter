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

  private static identifyProcedure(
    lines: ProcessedLine[]
  ): { blockType: BlockType; metadata: BlockMetadata } | null {
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
            isMiddle: false,
            isEnd: false,
          },
        };
      }
    }
    return null;
  }

  private static identifyComment(
    line: string
  ): { blockType: BlockType; metadata: BlockMetadata } | null {
    // Handle region comments specifically
    if (patterns.comment.region.start.test(line)) {
      return {
        blockType: "comment",
        metadata: {
          flowType: "other",
          isStart: true,
          isMiddle: false,
          isEnd: false,
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
        },
      };
    }

    // Handle other comments
    if (patterns.comment.block.test(line) || patterns.comment.single.test(line)) {
      return {
        blockType: "comment",
        metadata: {
          flowType: "other",
          isStart: false,
          isMiddle: true,
          isEnd: false,
        },
      };
    }
    return null;
  }

  private static identifyControlFlow(
    line: string
  ): { blockType: BlockType; metadata: BlockMetadata } | null {
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

// Standard rules processor class
class StandardRulesProcessor {
  constructor(private blocks: TypedBlock[]) {}

  public applyStandardRules(): void {
    this.ensureFileStartsCorrectly();
    this.limitConsecutiveBlankLines();
    this.applySqlStatementSpacing();
    this.applyRegionSpacing();
    this.applyErrorBlockSpacing();
    this.applyBasicControlStructureSpacing();
    this.applyBasicCommentSpacing();
    this.ensureFileEndsCorrectly();
  }

  private ensureFileStartsCorrectly(): void {
    // Remove any leading blank lines by adjusting the first block
    if (this.blocks.length > 0) {
      const firstBlock = this.blocks[0];
      while (firstBlock.lines[0]?.trimmedContent === "") {
        firstBlock.lines.shift();
      }
    }
  }

  private limitConsecutiveBlankLines(): void {
    this.blocks.forEach((block) => {
      block.followingSpaces = Math.min(block.followingSpaces, 2);
    });
  }

  private applySqlStatementSpacing(): void {
    this.blocks.forEach((block, index) => {
      if (index > 0 && index < this.blocks.length - 1) {
        const currentLine = block.lines[0].trimmedContent.toUpperCase();
        if (
          currentLine.startsWith("SELECT ") ||
          currentLine.startsWith("INSERT ") ||
          currentLine.startsWith("UPDATE ") ||
          currentLine.startsWith("DELETE ")
        ) {
          // Ensure one blank line before and after SQL statements
          this.blocks[index - 1].followingSpaces = 1;
          block.followingSpaces = 1;
        }
      }
    });
  }

  private applyRegionSpacing(): void {
    this.blocks.forEach((block, index) => {
      const firstLine = block.lines[0].trimmedContent;

      if (patterns.comment.region.start.test(firstLine)) {
        // One blank line before region start
        if (index > 0) {
          this.blocks[index - 1].followingSpaces = 1;
        }
        // One blank line after region start
        block.followingSpaces = 1;
      } else if (patterns.comment.region.end.test(firstLine)) {
        // One blank line before region end
        if (index > 0) {
          this.blocks[index - 1].followingSpaces = 1;
        }
        // Two blank lines after region end
        block.followingSpaces = 2;
      }
    });
  }

  private applyErrorBlockSpacing(): void {
    this.blocks.forEach((block, index) => {
      if (block.blockType === "errorHandling") {
        const metadata = block.metadata;

        if (metadata.flowType === "error") {
          // Two blank lines before ERROR block
          if (index > 0) {
            this.blocks[index - 1].followingSpaces = 2;
          }
          // One blank line after ERROR declaration
          block.followingSpaces = 1;
        } else if (metadata.flowType === "resume") {
          // One blank line before RESUME
          if (index > 0) {
            this.blocks[index - 1].followingSpaces = 1;
          }
          // One blank line after RESUME
          block.followingSpaces = 1;
        }
      }
    });
  }

  private applyBasicControlStructureSpacing(): void {
    this.blocks.forEach((block, index) => {
      if (block.blockType === "switch" || block.blockType === "conditional") {
        const metadata = block.metadata;

        // Handle BEGINCASE
        if (metadata.flowType === "caseStart" && block.blockType === "switch") {
          block.followingSpaces = 1;
        }
        // Handle ENDCASE
        else if (metadata.flowType === "caseEnd" && block.blockType === "switch") {
          if (index > 0) {
            this.blocks[index - 1].followingSpaces = 1;
          }
        }
        // Handle ELSE or ENDIF
        else if (
          block.blockType === "conditional" &&
          (metadata.flowType === "else" || metadata.flowType === "ifEnd")
        ) {
          if (index > 0) {
            this.blocks[index - 1].followingSpaces = 1;
          }
        }
      }
    });
  }

  private applyBasicCommentSpacing(): void {
    this.blocks.forEach((block, index) => {
      if (block.blockType === "comment") {
        const firstLine = block.lines[0].trimmedContent;

        // One blank line before any comment
        if (index > 0 && !this.isConsecutiveComment(this.blocks[index - 1])) {
          this.blocks[index - 1].followingSpaces = 1;
        }

        // Block comments must be followed by two blank lines
        if (patterns.comment.block.test(firstLine)) {
          block.followingSpaces = 2;
        }
      }
    });
  }

  private isConsecutiveComment(block: TypedBlock): boolean {
    return block.blockType === "comment";
  }

  private ensureFileEndsCorrectly(): void {
    // Ensure exactly one blank line at the end of file
    if (this.blocks.length > 0) {
      const lastBlock = this.blocks[this.blocks.length - 1];
      lastBlock.followingSpaces = 1;
    }
  }
}

// Spacing rules engine with updated implementation
class SpacingRulesEngine {
  static determineSpacing(block: TypedBlock, nextBlock: TypedBlock): number {
    // Apply standard rules first
    const processor = new StandardRulesProcessor([block, nextBlock]);
    processor.applyStandardRules();

    // Get the baseline spacing from standard rules
    const standardSpacing = block.followingSpaces;

    // Apply context-specific rules that may override standard rules
    return this.applyContextualRules(block, nextBlock, standardSpacing);
  }

  private static applyContextualRules(
    block: TypedBlock,
    nextBlock: TypedBlock,
    standardSpacing: number
  ): number {
    // Only override standard rules when specific context requires it

    // Procedure-specific overrides
    if (block.blockType === "procedure") {
      if (block.metadata.isStart && nextBlock.blockType === "declaration") {
        // Force no space between procedure start and its declarations
        return 0;
      }
    }

    // Declaration grouping overrides
    if (block.blockType === "declaration" && nextBlock.blockType === "declaration") {
      const currentLine = block.lines[0].trimmedContent;
      const nextLine = nextBlock.lines[0].trimmedContent;

      if (this.shouldGroupDeclarations(currentLine, nextLine)) {
        // Force no space between grouped declarations
        return 0;
      }
    }

    // Control structure overrides
    if (["conditional", "loop", "switch", "errorHandling"].includes(block.blockType)) {
      // Force no space for chained or related blocks
      if (nextBlock.context.isPartOfChain || this.areRelatedControlBlocks(block, nextBlock)) {
        return 0;
      }
    }

    // Comment handling overrides
    if (block.blockType === "comment") {
      const firstLine = block.lines[0].trimmedContent;

      // Handle region comments
      if (patterns.comment.region.start.test(firstLine)) {
        return 1; // One blank line after region start
      }
      if (patterns.comment.region.end.test(firstLine)) {
        return 2; // Two blank lines after region end
      }

      // Handle consecutive comments in a group
      if (nextBlock.blockType === "comment") {
        if (this.areConsecutiveComments(block, nextBlock)) {
          return 0;
        }
      }
    }

    // Logic block grouping overrides
    if (block.blockType === "logic" && nextBlock.blockType === "logic") {
      // Force no space between related logic blocks
      if (this.areRelatedLogicBlocks(block, nextBlock)) {
        return 0;
      }
    }

    // If no contextual rules apply, use the standard spacing
    return standardSpacing;
  }

  private static shouldGroupDeclarations(currentLine: string, nextLine: string): boolean {
    if (
      !patterns.declaration.group.test(currentLine) ||
      !patterns.declaration.group.test(nextLine)
    ) {
      return false;
    }

    // Compare declaration types (e.g., PARAMETERS, DECLARE, DEFAULT)
    const currentType = currentLine.substring(1, currentLine.indexOf(" "));
    const nextType = nextLine.substring(1, nextLine.indexOf(" "));
    return currentType === nextType;
  }

  private static areRelatedControlBlocks(block: TypedBlock, nextBlock: TypedBlock): boolean {
    return block.metadata.flowType === nextBlock.metadata.flowType && !block.metadata.isEnd;
  }

  private static areConsecutiveComments(block: TypedBlock, nextBlock: TypedBlock): boolean {
    const currentLine = block.lines[0].trimmedContent;
    const nextLine = nextBlock.lines[0].trimmedContent;

    // Don't group region comments
    if (
      patterns.comment.region.start.test(currentLine) ||
      patterns.comment.region.end.test(currentLine) ||
      patterns.comment.region.start.test(nextLine) ||
      patterns.comment.region.end.test(nextLine)
    ) {
      return false;
    }

    // Don't group block comments
    if (patterns.comment.block.test(currentLine) || patterns.comment.block.test(nextLine)) {
      return false;
    }

    // Group consecutive single-line comments
    return patterns.comment.single.test(currentLine) && patterns.comment.single.test(nextLine);
  }

  private static areRelatedLogicBlocks(block: TypedBlock, nextBlock: TypedBlock): boolean {
    // Logic blocks are related if they're consecutive and not separated by comments
    return true;
  }
}

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
    const leadingWhitespace =
      trimmed === "" ? "" : line.substring(0, line.length - line.trimStart().length);

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
        `Created block: ${startLine}-${endLine}, ` +
          `type=${blockIdentification.blockType}, ` +
          `originalSpaces=${blankCount}`
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
        prevBlock.metadata.flowType === "caseBranch" ||
        prevBlock.metadata.flowType === "caseDefault";

      if (isCaseStatement) {
        // Merge with any content that follows the case statement
        if (
          !["caseStart", "caseEnd", "caseBranch", "caseDefault"].includes(
            currentBlock.metadata.flowType
          )
        ) {
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
        currentLine.substring(1, currentLine.indexOf(" ")) ===
          nextLine.substring(1, nextLine.indexOf(" "));

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
        prevBlock.metadata.flowType === currentBlock.metadata.flowType ||
        currentBlock.context.isPartOfChain;

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
            `next=${nextLine.substring(0, truncateLength)}${
              nextLine.length > truncateLength ? "..." : ""
            }`
          );
        })
        .join("\n") + "\n"
    );
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
