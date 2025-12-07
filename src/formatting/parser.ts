import { SqlLexer } from './sqlLexer';
import { Token, TokenType, Lexer } from './lexer';
import {
    BLOCK_START_KEYWORDS,
    BLOCK_END_KEYWORDS,
    BLOCK_MIDDLE_KEYWORDS,
    CASE_KEYWORDS
} from '../constants/language';

export enum NodeType {
    Program,
    Block,
    Statement,
    Comment,
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

        // We process tokens and group them into Statements
        // Then we structure statements into Blocks
        // Actually, let's do grouping into Statements first (logical lines)
        // Then build tree from Statements.

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
                // They should be sibling to the :IF and the previous body.

                // 1. Close current block (if valid)
                if (stack.length > 1) {
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
                // Pop until matching block?
                // SSL Guide: :ENDIF, :NEXT, :ENDWHILE, :ENDCASE, :ENDPROC
                // If we are in Program, ignore (unmatched end)
                if (stack.length > 1) {
                    stack.pop();
                    currentNode = stack[stack.length - 1];
                }
                // Add the END statement to the closing block?
                // Or does it belong to the parent?
                // Usually :ENDIF is part of the IfBlock or closes it.
                // Let's add it to the PARENT of the block we just closed?
                // No, flat list of statements inside the block?
                // indentation:
                // :IF
                //   stmt
                // :ENDIF
                // The :IF and :ENDIF are at same level.
                // So :IF starts the block, but is NOT inside it?
                // :IF is the specific header.

                // Better Tree:
                // IfStatementNode
                //   Header (:IF ...)
                //   Body (BlockNode) -> indented
                //   End (:ENDIF)

                // But that implies looking ahead.
                // Let's stick to indentation based on stack depth.
                // :IF -> Push Block
                // :ENDIF -> Pop Block

                // If blockEnd, we first pop (reduce indent), then add statement?
                // No, :ENDIF is aligned with :IF.
                // So :ENDIF should be added AFTER pop?
                // Yes.
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
        if (index < 2) return;

        const prev = tokens[index - 1];
        const func = tokens[index - 2];

        if (prev.text === '(' && func.type === TokenType.Identifier) {
            const sqlFunctions = ['SQLExecute', 'RunSQL', 'LSearch', 'LSelect', 'LSelect1', 'LSelectC', 'GetDataSet', 'GetNetDataSet'];
            if (sqlFunctions.some(f => f.toLowerCase() === func.text.toLowerCase())) {
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

            if (token.type === TokenType.EOF) break;

            currentTokens.push(token);

            // Statement terminator is ;
            // Also if comment, it stands alone?
            // SSL Guide: /* ... ;
            // If token is punctuation ';' -> End of statement.
            // But verify it's not inside something else? Lexer handles strings/comments.

            if (token.type === TokenType.Punctuation && token.text === ';') {
                statements.push(this.createNode(currentTokens));
                currentTokens = [];
            }
            // What if no semicolon? (e.g. end of file or forgotten)
            // Force flush on specific keywords?
            // SSL requires semicolons. If missing, line break might be separator?
            // Guide says "all_statements: require_semicolon".
            // But users make mistakes.
            // If we see a newline? Formatter usually treats newline as separator if lenient.
            // But let's stick to semicolon for now strictly.
            // Wait, Comments (TokenType.Comment) include the semicolon in text if block comment?
            // My Lexer `readBlockComment` includes `; `.
            // My Lexer `readLineComment` does NOT include `; ` (ends at newline).

            if (token.type === TokenType.Comment) {
                // Should comments be their own statements?
                // If we have `stmt; /* comment */`, lexer emits Punctuation `; ` then Comment.
                // The `; ` triggered flush above.
                // So Comment starts new statement.
                // If comment is block `/*...*/; `, it ends with `; `? 
                // Lexer `readBlockComment` consumes `; `.
                // So it is self-contained.
                statements.push(this.createNode([token]));
                currentTokens = [];
            }
        }

        if (currentTokens.length > 0) {
            statements.push(this.createNode(currentTokens));
        }

        // Post-process statements to separate inline comments?
        // No, Lexer emits Comment token.
        // If specific checking needed `isBlockStart`, `isBlockEnd` helpers.

        return statements;
    }

    private createNode(tokens: Token[]): Node {
        if (tokens.length === 0) return { type: NodeType.Statement, tokens: [], children: [], startLine: 0, endLine: 0 };
        return {
            type: NodeType.Statement,
            tokens: tokens,
            children: [],
            startLine: tokens[0].line,
            endLine: tokens[tokens.length - 1].line
        };
    }

    private isBlockStart(node: Node): boolean {
        // Check first token (skipping whitespace)
        const first = this.getFirstSignificantToken(node);
        if (!first) return false;
        if (first.type !== TokenType.Keyword) return false;

        // Check against known start keywords
        // :IF, :WHILE, :FOR, :CASE (sometimes start: :BEGINCASE vs :CASE as middle)
        // Guide: :BEGINCASE starts Case block.
        // :CASE is a label inside strict Case block?
        // Guide says:
        // case:
        //   begin: :BEGINCASE
        //   case: :CASE
        //   otherwise: :OTHERWISE
        //   end: :ENDCASE

        const text = first.text.toUpperCase().replace(':', '');
        return [...BLOCK_START_KEYWORDS, 'BEGINCASE', 'TRY', 'REGION'].includes(text) || text === 'PROCEDURE' || text === 'CLASS';
        // Need to check specific lists from constants
    }

    private isBlockEnd(node: Node): boolean {
        const first = this.getFirstSignificantToken(node);
        if (!first || first.type !== TokenType.Keyword) return false;
        const text = first.text.toUpperCase().replace(':', '');
        return [...BLOCK_END_KEYWORDS, 'ENDCASE', 'ENDTRY', 'ENDREGION', 'ENDPROC'].includes(text);
    }

    private isBlockMiddle(node: Node): boolean {
        const first = this.getFirstSignificantToken(node);
        if (!first || first.type !== TokenType.Keyword) return false;
        const text = first.text.toUpperCase().replace(':', '');
        return ['ELSE', 'ELSEIF', 'CATCH', 'FINALLY', 'CASE', 'OTHERWISE'].includes(text);
    }

    private getFirstSignificantToken(node: Node): Token | undefined {
        for (const t of node.tokens) {
            if (t.type !== TokenType.Whitespace && t.type !== TokenType.Comment) return t;
        }
        return undefined;
    }
}
