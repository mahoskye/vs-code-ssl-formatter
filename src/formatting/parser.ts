import { SqlLexer } from './sqlLexer';
import { Token, TokenType, Lexer } from './lexer';
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
    private pos: number = 0;

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

        let currentNode: Node = root;

        const statements = this.groupStatements(this.tokens);

        const stack: Node[] = [root];

        for (const stmt of statements) {
            // Determine if statement starts/ends a block
            const blockStart = this.isBlockStart(stmt);
            const blockEnd = this.isBlockEnd(stmt);
            const blockMiddle = this.isBlockMiddle(stmt);

            if (blockMiddle) {
                // :ELSE, :CASE, :CATCH
                // These end the *current* block (e.g. the IF body) and start a *new* block (the ELSE body).

                // Special handling for CASE/OTHERWISE
                // They should NOT close a :BEGINCASE block, but SHOULD close a previous :CASE block.
                const firstToken = this.getFirstSignificantToken(stmt);
                const isCase = firstToken && CASE_KEYWORDS.includes(firstToken.text.toUpperCase().replace(':', ''));

                let shouldPop = true;
                if (isCase && stack.length > 1) {
                    const currentBlock = stack[stack.length - 1];
                    const starter = this.getBlockStarter(currentBlock);
                    if (starter) {
                        const starterToken = this.getFirstSignificantToken(starter);
                        if (starterToken && starterToken.text.toUpperCase().replace(':', '') === 'BEGINCASE') {
                            shouldPop = false;
                        }
                    }
                }

                // 1. Close current block (if valid and should pop)
                if (shouldPop && stack.length > 1) {
                    stack.pop();
                    currentNode = stack[stack.length - 1];
                }

                // 2. Add the middle statement (e.g. :ELSE) to the parent
                currentNode.children.push(stmt);

                // 3. Start a new block for the body following this middle statement
                const newBlock: Node = {
                    type: NodeType.Block,
                    tokens: [],
                    children: [],
                    parent: currentNode,
                    startLine: stmt.endLine + 1, // Approx
                    endLine: 0
                };
                currentNode.children.push(newBlock);
                currentNode = newBlock;
                stack.push(newBlock);

                // Skip the standard processing since we just handled it
                continue;
            }

            if (blockEnd) {
                // Standard pop
                if (stack.length > 1) {
                    stack.pop();
                    currentNode = stack[stack.length - 1];
                }

                // Special handling for ENDCASE: It blindly closes the BEGINCASE block as well
                const first = this.getFirstSignificantToken(stmt);
                if (first && first.text.toUpperCase().replace(':', '') === 'ENDCASE') {
                    if (stack.length > 1) {
                        const currentBlock = stack[stack.length - 1];
                        const starter = this.getBlockStarter(currentBlock);
                        if (starter) {
                            const starterToken = this.getFirstSignificantToken(starter);
                            if (starterToken && starterToken.text.toUpperCase().replace(':', '') === 'BEGINCASE') {
                                stack.pop();
                                currentNode = stack[stack.length - 1];
                            }
                        }
                    }
                }
            }

            // Add statement to current node
            currentNode.children.push(stmt);

            if (blockStart) {
                // :IF, :WHILE, :PROCEDURE
                // Start a new block
                const newBlock: Node = {
                    type: NodeType.Block,
                    tokens: [], // Block node itself doesn't have tokens, it collects children
                    children: [],
                    parent: currentNode,
                    startLine: stmt.startLine,
                    endLine: stmt.endLine
                };
                // The statement that STARTED the block (e.g. :IF) is already added to 'currentNode'.
                // The new block will collect SUBSEQUENT statements.

                currentNode.children.push(newBlock);
                currentNode = newBlock;
                stack.push(newBlock);
            }
        }

        return root;
    }

    // Helper to check if current token is starting a SQL function call
    private checkSqlContext(tokens: Token[], index: number) {
        // Look back: Identifier (Function Name) -> ( -> String (Current)
        // Check if Function Name is one of the SQL executing functions
        if (index < 2) { return; }

        const prev = tokens[index - 1];
        const func = tokens[index - 2];

        if (prev.text === '(' && func.type === TokenType.Identifier) {
            if (SQL_CONTEXT_FUNCTIONS.some(f => f.toLowerCase() === func.text.toLowerCase())) {
                // This string token is likely SQL
                const token = tokens[index]; // The string token
                if (token.type === TokenType.String) {
                    // Extract content (strip quotes)
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
                let j = i + 1;
                while (j < tokens.length && tokens[j].type === TokenType.Whitespace) { j++; }

                if (j < tokens.length) {
                    const next = tokens[j];

                    // Retrieve last significant token logic
                    let k = currentTokens.length - 2;
                    while (k >= 0 && currentTokens[k].type === TokenType.Whitespace) { k--; }

                    if (k >= 0) {
                        const last = currentTokens[k];
                        const lastIsContinuation = last.type === TokenType.Operator || last.text === ',' || last.text === '(' || last.text === '[' || last.text === '{';

                        const nextIsContinuation = next.type === TokenType.Operator || next.text === ',' || next.text === '.' || next.text === ')' || next.text === ']' || next.text === '}' || next.text === ';';
                        const isFunctionCall = next.text === '(' && (last.type === TokenType.Identifier || last.type === TokenType.Keyword);

                        if (!lastIsContinuation && !nextIsContinuation && !isFunctionCall) {
                            statements.push(this.createNode(currentTokens));
                            currentTokens = [];
                        }
                    }
                }
            }

            if (token.type === TokenType.Comment) {
                statements.push(this.createNode([token]));
                currentTokens = [];
            }
        }

        if (currentTokens.length > 0) {
            statements.push(this.createNode(currentTokens));
        }

        return statements;
    }

    private createNode(tokens: Token[]): Node {
        if (tokens.length === 0) { return { type: NodeType.Statement, tokens: [], children: [], startLine: 0, endLine: 0 }; }

        let type = NodeType.Statement;
        // If single token and it's a comment
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

    private isRegionStartText(text: string): boolean {
        return REGION_START_PATTERN.test(text);
    }

    private isRegionEndText(text: string): boolean {
        return REGION_END_PATTERN.test(text);
    }

    private isBlockStart(node: Node): boolean {
        const first = this.getFirstSignificantToken(node);
        if (!first) { return false; }
        if (first.type !== TokenType.Keyword) { return false; }

        const text = first.text.toUpperCase().replace(':', '');
        return BLOCK_START_KEYWORDS.includes(text);
    }

    private isBlockEnd(node: Node): boolean {
        const first = this.getFirstSignificantToken(node);
        if (!first || first.type !== TokenType.Keyword) { return false; }
        const text = first.text.toUpperCase().replace(':', '');
        return BLOCK_END_KEYWORDS.includes(text);
    }

    private isBlockMiddle(node: Node): boolean {
        const first = this.getFirstSignificantToken(node);
        if (!first || first.type !== TokenType.Keyword) { return false; }
        const text = first.text.toUpperCase().replace(':', '');
        return BLOCK_MIDDLE_KEYWORDS.includes(text) || CASE_KEYWORDS.includes(text);
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
