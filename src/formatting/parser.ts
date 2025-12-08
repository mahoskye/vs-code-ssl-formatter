import { SqlLexer } from './sqlLexer';
import { Token, TokenType } from './lexer';
import {
    BLOCK_START_KEYWORDS,
    BLOCK_END_KEYWORDS,
    BLOCK_MIDDLE_KEYWORDS,
    CASE_KEYWORDS,
    REGION_START_PATTERN,
    REGION_END_PATTERN,
    SQL_CONTEXT_FUNCTIONS
} from '../constants/language';

export enum NodeType {
    Program,
    Block,
    Statement,
    Comment,
    RegionStart,
    RegionEnd,
    Unknown
}

export interface Node {
    type: NodeType;
    tokens: Token[];
    children: Node[];
    parent?: Node;
    startLine: number;
    endLine: number;
}

export class Parser {
    private tokens: Token[];

    constructor(tokens: Token[]) {
        this.tokens = tokens;
    }

    public parse(): Node {
        const root: Node = {
            type: NodeType.Program,
            tokens: [],
            children: [],
            startLine: 0,
            endLine: 0
        };

        const statements = this.groupStatements(this.tokens);
        let currentNode: Node = root;
        const stack: Node[] = [root];

        for (const stmt of statements) {
            const blockStart = this.isBlockStart(stmt);
            const blockEnd = this.isBlockEnd(stmt);
            const blockMiddle = this.isBlockMiddle(stmt);

            if (blockMiddle) {
                currentNode = this.handleBlockMiddle(stmt, currentNode, stack);
                continue;
            }

            if (blockEnd) {
                currentNode = this.handleBlockEnd(stmt, currentNode, stack);
            }

            // Add statement to current node
            currentNode.children.push(stmt);

            if (blockStart) {
                currentNode = this.handleBlockStart(stmt, currentNode, stack);
            }
        }

        return root;
    }

    // --- Block Handlers ---

    private handleBlockMiddle(stmt: Node, currentNode: Node, stack: Node[]): Node {
        // :ELSE, :CASE, :CATCH
        // These end the *current* block and start a *new* block.
        // Special logic for nested CASE/BEGINCASE interaction.

        let shouldPop = true;

        // CASE logic: check if this :CASE should close the current block
        // It SHOULD close a previous :CASE block, but NOT a :BEGINCASE block.
        const firstToken = this.getFirstSignificantToken(stmt);
        const isCase = firstToken && (CASE_KEYWORDS as readonly string[]).includes(this.getNormalizedText(firstToken));

        if (isCase && stack.length > 1) {
            const currentBlock = stack[stack.length - 1];
            const starter = this.getBlockStarter(currentBlock);
            if (starter) {
                const starterToken = this.getFirstSignificantToken(starter);
                if (starterToken && this.getNormalizedText(starterToken) === 'BEGINCASE') {
                    shouldPop = false; // Don't pop the BEGINCASE block, just append to it?
                    // Actually, :CASE inside BEGINCASE usually implies:
                    // BEGINCASE (starts block) -> ... -> CASE (ends previous case-block?! No, it starts a case-block inside main block?)
                    // SSL Logic: BEGINCASE ... CASE ... CASE ... ENDCASE
                    // We treat BEGINCASE as a block. The things inside are children.
                    // If we treat CASE as a block starter?
                    // Actually, usually CASE is treated as a label or a block switcher.
                    // Current logic assumes it switches blocks.

                    // If we are directly inside BEGINCASE, we shouldn't Pop.
                    // But if we are inside a previous CASE block, we SHOuld Pop?
                }
            }
        }

        // 1. Close current block (if valid and should pop)
        let activeNode = currentNode;
        if (shouldPop && stack.length > 1) {
            stack.pop();
            activeNode = stack[stack.length - 1];
        }

        // 2. Add the middle statement (e.g. :ELSE) to the parent
        activeNode.children.push(stmt);

        // 3. Start a new block for the body following this middle statement
        const newBlock: Node = {
            type: NodeType.Block,
            tokens: [],
            children: [],
            parent: activeNode,
            startLine: stmt.endLine + 1,
            endLine: 0
        };
        activeNode.children.push(newBlock);
        stack.push(newBlock);

        return newBlock; // Return the new current node
    }

    private handleBlockEnd(stmt: Node, currentNode: Node, stack: Node[]): Node {
        // Standard pop
        let activeNode = currentNode;
        if (stack.length > 1) {
            stack.pop();
            activeNode = stack[stack.length - 1];
        }

        // Special handling for ENDCASE: It blindly closes the BEGINCASE block as well if we just popped a CASE block
        // Or if we are now at BEGINCASE block, we need to pop that too.
        const first = this.getFirstSignificantToken(stmt);
        if (first && this.getNormalizedText(first) === 'ENDCASE') {
            if (stack.length > 1) {
                const currentBlock = stack[stack.length - 1];
                const starter = this.getBlockStarter(currentBlock);
                if (starter) {
                    const starterToken = this.getFirstSignificantToken(starter);
                    if (starterToken && this.getNormalizedText(starterToken) === 'BEGINCASE') {
                        stack.pop();
                        activeNode = stack[stack.length - 1];
                    }
                }
            }
        }
        return activeNode;
    }

    private handleBlockStart(stmt: Node, currentNode: Node, stack: Node[]): Node {
        // :IF, :WHILE, :PROCEDURE
        // Start a new block
        const newBlock: Node = {
            type: NodeType.Block,
            tokens: [],
            children: [],
            parent: currentNode,
            startLine: stmt.startLine,
            endLine: stmt.endLine // Will be updated? No, usually not used for Blocks until closed.
        };

        currentNode.children.push(newBlock);
        stack.push(newBlock);
        return newBlock;
    }

    // --- Statement Grouping ---

    private groupStatements(tokens: Token[]): Node[] {
        const statements: Node[] = [];
        let currentTokens: Token[] = [];

        for (let i = 0; i < tokens.length; i++) {
            const token = tokens[i];

            // Check for SQL Context
            if (token.type === TokenType.String) {
                this.checkSqlContext(tokens, i);
            }

            if (token.type === TokenType.EOF) { break; }

            currentTokens.push(token);

            if (token.type === TokenType.Punctuation && token.text === ';') {
                statements.push(this.createNode(currentTokens));
                currentTokens = [];
            } else if (token.type === TokenType.Whitespace && token.text.includes('\n')) {
                // Implicit semicolon check
                if (this.isStatementContinuation(tokens, i, currentTokens)) {
                    continue;
                }
                statements.push(this.createNode(currentTokens));
                currentTokens = [];
            } else if (token.type === TokenType.Comment) {
                // Comments usually stand alone if they are single lines. 
                // If we have accumulated tokens (code), comment might be inline?
                // But groupStatements pushes comments as separate nodes if they appear?
                // Original logic: if token is Comment, push immediately.
                // This implies comments break statements? 
                // If we have `x = 1 /* comment */`, `currentTokens` has `x=1`.
                // Then we push `/* comment */`. 
                // Then next might be `\n`.
                // Original logic pushed comment ONLY if it was the token.
                // Actually original loop logic was: if comment, statements.push(createNode([token])), clear currentTokens.
                // This splits `x=1 /*comment*/` into `x=1` and `/*comment*/`. Correct.
                statements.push(this.createNode([token]));
                currentTokens = [];
            }
        }

        if (currentTokens.length > 0) {
            statements.push(this.createNode(currentTokens));
        }

        return statements;
    }

    private isStatementContinuation(tokens: Token[], currentIndex: number, currentStatementTokens: Token[]): boolean {
        // Look ahead for next non-whitespace
        let j = currentIndex + 1;
        while (j < tokens.length && tokens[j].type === TokenType.Whitespace) { j++; }

        if (j >= tokens.length) { return false; } // End of file, so yes, end of statement.

        const next = tokens[j];

        // Retrieve last significant token of current statement
        let k = currentStatementTokens.length - 2; // -1 is the current Newline token we just pushed
        while (k >= 0 && currentStatementTokens[k].type === TokenType.Whitespace) { k--; }

        if (k < 0) { return false; } // No significant tokens in current statement yet

        const last = currentStatementTokens[k];

        const lastIsContinuation = last.type === TokenType.Operator || last.text === ',' || last.text === '(' || last.text === '[' || last.text === '{';
        const nextIsContinuation = next.type === TokenType.Operator || next.text === ',' || next.text === '.' || next.text === ')' || next.text === ']' || next.text === '}' || next.text === ';';
        const isFunctionCall = next.text === '(' && (last.type === TokenType.Identifier || last.type === TokenType.Keyword);

        return lastIsContinuation || nextIsContinuation || isFunctionCall;
    }

    private checkSqlContext(tokens: Token[], index: number) {
        if (index < 2) { return; }

        let lookbackIndex = index - 1;
        while (lookbackIndex >= 0 && (tokens[lookbackIndex].type === TokenType.Whitespace || tokens[lookbackIndex].type === TokenType.Comment)) {
            lookbackIndex--;
        }

        if (lookbackIndex < 0) { return; }
        const prev = tokens[lookbackIndex];

        if (prev.text === '(') {
            lookbackIndex--;
            while (lookbackIndex >= 0 && (tokens[lookbackIndex].type === TokenType.Whitespace || tokens[lookbackIndex].type === TokenType.Comment)) {
                lookbackIndex--;
            }

            if (lookbackIndex >= 0) {
                const func = tokens[lookbackIndex];
                if (func.type === TokenType.Identifier) {
                    if (SQL_CONTEXT_FUNCTIONS.some(f => f.toLowerCase() === func.text.toLowerCase())) {
                        const token = tokens[index];
                        if (token.type === TokenType.String) {
                            let content = token.text;
                            if (content.startsWith('"') || content.startsWith("'")) {
                                content = content.substring(1, content.length - 1);
                            } else if (content.startsWith('[')) {
                                content = content.substring(1, content.length - 1);
                            }

                            const sqlLexer = new SqlLexer(content);
                            token.sqlTokens = sqlLexer.tokenize();
                        }
                    }
                }
            }
        }
    }

    private createNode(tokens: Token[]): Node {
        if (tokens.length === 0) { return { type: NodeType.Statement, tokens: [], children: [], startLine: 0, endLine: 0 }; }

        let type = NodeType.Statement;
        if (tokens.length === 1 && tokens[0].type === TokenType.Comment) {
            const text = tokens[0].text;
            if (this.isRegionStartText(text)) {
                type = NodeType.RegionStart;
            } else if (this.isRegionEndText(text)) {
                type = NodeType.RegionEnd;
            } else {
                type = NodeType.Comment;
            }
        }

        return {
            type: type,
            tokens: tokens,
            children: [],
            startLine: tokens[0].line,
            endLine: tokens[tokens.length - 1].line
        };
    }

    // --- Helpers ---

    private getNormalizedText(token: Token): string {
        return token.text.toUpperCase().replace(':', '');
    }

    private isRegionStartText(text: string): boolean {
        return REGION_START_PATTERN.test(text);
    }

    private isRegionEndText(text: string): boolean {
        return REGION_END_PATTERN.test(text);
    }

    private isBlockStart(node: Node): boolean {
        const first = this.getFirstSignificantToken(node);
        if (!first || first.type !== TokenType.Keyword) { return false; }
        return (BLOCK_START_KEYWORDS as readonly string[]).includes(this.getNormalizedText(first));
    }

    private isBlockEnd(node: Node): boolean {
        const first = this.getFirstSignificantToken(node);
        if (!first || first.type !== TokenType.Keyword) { return false; }
        return (BLOCK_END_KEYWORDS as readonly string[]).includes(this.getNormalizedText(first));
    }

    private isBlockMiddle(node: Node): boolean {
        const first = this.getFirstSignificantToken(node);
        if (!first || first.type !== TokenType.Keyword) { return false; }
        const text = this.getNormalizedText(first);
        return (BLOCK_MIDDLE_KEYWORDS as readonly string[]).includes(text) || (CASE_KEYWORDS as readonly string[]).includes(text);
    }

    private getFirstSignificantToken(node: Node): Token | undefined {
        for (const t of node.tokens) {
            if (t.type !== TokenType.Whitespace && t.type !== TokenType.Comment) { return t; }
        }
        return undefined;
    }

    private getBlockStarter(blockNode: Node): Node | undefined {
        if (!blockNode.parent) { return undefined; }
        const siblings = blockNode.parent.children;
        const index = siblings.indexOf(blockNode);
        if (index > 0) {
            return siblings[index - 1];
        }
        return undefined;
    }
}

