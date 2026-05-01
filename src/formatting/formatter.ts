import { Node, NodeType, Parser } from '../parsing/parser';
import { Lexer, Token, TokenType } from '../parsing/lexer';
import { SqlFormatter } from './sqlFormatter';
import { WhitespaceManager } from './whitespaceManager';
import { StatementPrinter } from './statementPrinter';

export interface FormattingOptions {
    tabSize: number;
    insertSpaces: boolean;
    [key: string]: boolean | number | string;
}

export class SSLFormatter {
    private options: FormattingOptions;
    private indentString: string;
    private output: string[] = [];
    private currentIndentLevel: number = 0;
    private sqlFormatter: SqlFormatter;
    private whitespaceManager: WhitespaceManager;
    private statementPrinter: StatementPrinter;
    private seenProcedure: boolean = false;

    constructor(options: FormattingOptions) {
        this.options = options;
        this.indentString = options.insertSpaces ? ' '.repeat(options.tabSize) : '\t';
        this.sqlFormatter = new SqlFormatter({
            wrapLength: (options['ssl.format.wrapLength'] as number) || 90,
            insertSpaces: options.insertSpaces,
            tabSize: options.tabSize,
            keywordCase: options['ssl.format.sql.keywordCase'] as 'upper' | 'lower' | 'title' | 'preserve',
            indentSpaces: options['ssl.format.sql.indentSpaces'] as number,
            style: options['ssl.format.sql.style'] as string
        });
        const maxConsecutiveBlankLines = (options['ssl.format.maxConsecutiveBlankLines'] as number) || 2;
        this.whitespaceManager = new WhitespaceManager(maxConsecutiveBlankLines);
        this.statementPrinter = new StatementPrinter(options, this.sqlFormatter);
    }

    public format(text: string, initialIndentLevel: number = 0): string {
        const lexer = new Lexer(text);
        const tokens = lexer.tokenize();
        const parser = new Parser(tokens);
        const root = parser.parse();

        // Reset state
        this.output = [];
        this.currentIndentLevel = initialIndentLevel;
        this.seenProcedure = false;

        // Start processing
        this.visitNode(root, undefined);

        // Join lines
        return this.output.join('\n').trim() + '\n';
    }

    private visitNode(node: Node, _prevSibling?: Node, nextSibling?: Node) {
        switch (node.type) {
            case NodeType.Program:
                this.visitProgram(node);
                break;
            case NodeType.Block:
                this.visitBlock(node);
                break;
            case NodeType.Statement:
            case NodeType.Comment:
            case NodeType.RegionStart:
            case NodeType.RegionEnd:
                this.visitStatement(node, nextSibling);
                break;
            default:
                // Handle unknowns or unexpected types gracefully if needed
                break;
        }
    }

    private visitProgram(node: Node) {
        const children = node.children.filter(child => !this.isWhitespaceOnlyNode(child));
        let prev: Node | undefined = undefined;
        for (let i = 0; i < children.length; i++) {
            const child = children[i];
            const next = i + 1 < children.length ? children[i + 1] : undefined;
            if (this.handleVerticalWhitespace(prev, child)) {
                prev = child;
                continue;
            }
            this.visitNode(child, prev, next);
            prev = child;
        }
    }

    private visitBlock(node: Node) {
        this.currentIndentLevel++;
        const children = node.children.filter(child => !this.isWhitespaceOnlyNode(child));
        let prev: Node | undefined = undefined;
        for (let i = 0; i < children.length; i++) {
            const child = children[i];
            const next = i + 1 < children.length ? children[i + 1] : undefined;
            if (this.handleVerticalWhitespace(prev, child)) {
                prev = child;
                continue;
            }
            this.visitNode(child, prev, next);
            prev = child;
        }
        this.currentIndentLevel--;
    }

    private visitStatement(node: Node, nextSibling?: Node) {
        let indentLevel = this.currentIndentLevel;

        // Temporary dedent for PARAMETERS which are syntactically children but visually top-level in procedure
        const firstToken = node.tokens.find(t => t.type !== TokenType.Whitespace && t.type !== TokenType.Comment);
        if (firstToken) {
            const text = firstToken.text.toUpperCase().replace(/^:/, ''); // Handle legacy colon
            if (text === 'PARAMETERS') {
                indentLevel = Math.max(0, indentLevel - 1);
            }
        }

        const keyword = this.getNodeKeyword(node);
        const blankLinesBetweenProcs = this.options['ssl.format.blankLinesBetweenProcs'] as number | undefined;
        if (keyword === 'PROCEDURE') {
            if (this.seenProcedure && (blankLinesBetweenProcs ?? 1) > 0) {
                const existingBlankLines = this.countTrailingBlankLines();
                const requiredBlankLines = blankLinesBetweenProcs ?? 1;
                const needed = Math.max(0, requiredBlankLines - existingBlankLines);
                for (let i = 0; i < needed; i++) {
                    this.output.push('');
                }
            }
            this.seenProcedure = true;
        }

        let line = this.statementPrinter.printStatement(node, indentLevel);
        if (line.trim() && this.shouldAppendSemicolon(node, nextSibling)) {
            line = line.trimEnd() + ';';
        }
        // Only add to output if there's actual content - prevents whitespace-only lines
        if (line.trim()) {
            const indentedLine = this.indentString.repeat(indentLevel) + line;
            this.output.push(indentedLine.trimEnd());
        }
    }

    /**
     * Handles vertical whitespace between nodes.
     * Returns true if the current node was merged into the previous line (e.g. inline comment)
     * and should be skipped for normal printing.
     */
    private handleVerticalWhitespace(prev: Node | undefined, current: Node): boolean {
        if (!prev) { return false; }

        const lines = this.whitespaceManager.getVerticalWhitespace(prev, current);
        if (lines === -1) {
            // Trailing inline comment - append to previous line
            const comment = this.statementPrinter.printStatement(current, this.currentIndentLevel);
            if (this.output.length > 0) {
                this.output[this.output.length - 1] += ' ' + comment;
            }
            return true; // Signal to skip standard visiting for this node
        } else {
            for (let i = 0; i < lines; i++) {
                this.output.push("");
            }
            return false;
        }
    }

    /** Check if a node contains only whitespace tokens and has no children */
    private isWhitespaceOnlyNode(node: Node): boolean {
        // Never skip nodes that have children (like Block nodes)
        if (node.children && node.children.length > 0) { return false; }
        // Never skip Block nodes
        if (node.type === NodeType.Block) { return false; }
        // Only skip Statement nodes that contain only whitespace tokens
        return node.tokens.length > 0 && node.tokens.every(t => t.type === TokenType.Whitespace);
    }

    private getNodeKeyword(node: Node): string | undefined {
        const firstToken = this.getFirstSignificantToken(node);
        if (!firstToken || firstToken.type !== TokenType.Keyword) {
            return undefined;
        }
        return this.normalizeKeyword(firstToken.text);
    }

    private countTrailingBlankLines(): number {
        let count = 0;
        for (let i = this.output.length - 1; i >= 0; i--) {
            if (this.output[i] !== '') {
                break;
            }
            count++;
        }
        return count;
    }

    private shouldAppendSemicolon(node: Node, nextSibling?: Node): boolean {
        if (this.options['ssl.format.semicolonEnforcement'] === false) {
            return false;
        }

        if (node.type === NodeType.Comment) {
            return false;
        }

        const lastToken = this.getLastSignificantToken(node);
        if (!lastToken) {
            return false;
        }
        if (lastToken.text === ';') {
            return false;
        }
        if (this.isOpenDelimiter(lastToken) || lastToken.type === TokenType.Operator || lastToken.text === ',') {
            return false;
        }
        if (lastToken.type === TokenType.Keyword) {
            const keyword = this.normalizeKeyword(lastToken.text);
            if (keyword === 'TO' || keyword === 'STEP') {
                return false;
            }
        }

        const nextToken = nextSibling ? this.getFirstSignificantToken(nextSibling) : undefined;
        if (nextToken && nextToken.type === TokenType.Keyword) {
            const keyword = this.normalizeKeyword(nextToken.text);
            const continuationKeywords = new Set(['ELSE', 'ELSEIF', 'CATCH', 'FINALLY', 'CASE', 'OTHERWISE', 'TO', 'STEP']);
            if (continuationKeywords.has(keyword)) {
                return false;
            }
            return this.isStatementContent(lastToken);
        }
        if (nextToken && nextToken.type === TokenType.Identifier) {
            return this.isStatementContent(lastToken);
        }
        if (!nextToken) {
            return this.isStatementContent(lastToken);
        }

        return false;
    }

    private getFirstSignificantToken(node: Node): Token | undefined {
        return node.tokens.find(t => t.type !== TokenType.Whitespace && t.type !== TokenType.Comment);
    }

    private getLastSignificantToken(node: Node): Token | undefined {
        for (let i = node.tokens.length - 1; i >= 0; i--) {
            const token = node.tokens[i];
            if (token.type !== TokenType.Whitespace && token.type !== TokenType.Comment) {
                return token;
            }
        }
        return undefined;
    }

    private normalizeKeyword(text: string): string {
        return text.toUpperCase().replace(/^:/, '');
    }

    private isStatementContent(token: Token): boolean {
        if (token.type === TokenType.Identifier || token.type === TokenType.Number || token.type === TokenType.String || token.type === TokenType.Keyword) {
            return true;
        }
        return this.isCloseDelimiter(token);
    }

    private isOpenDelimiter(token: Token): boolean {
        return token.text === '(' || token.text === '[' || token.text === '{';
    }

    private isCloseDelimiter(token: Token): boolean {
        return token.text === ')' || token.text === ']' || token.text === '}';
    }
}
