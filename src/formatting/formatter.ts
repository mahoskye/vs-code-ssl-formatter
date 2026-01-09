import { Node, NodeType, Parser } from '../parsing/parser';
import { Lexer, TokenType } from '../parsing/lexer';
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

    constructor(options: FormattingOptions) {
        this.options = options;
        this.indentString = options.insertSpaces ? ' '.repeat(options.tabSize) : '\t';
        this.sqlFormatter = new SqlFormatter({
            wrapLength: (options['ssl.format.wrapLength'] as number) || 90,
            insertSpaces: options.insertSpaces,
            tabSize: options.tabSize,
            keywordCase: options['ssl.format.sql.keywordCase'] as 'upper' | 'lower' | 'title' | 'preserve',
            indentSpaces: options['ssl.format.sql.indentSpaces'] as number
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

        // Start processing
        this.visitNode(root, undefined);

        // Join lines
        return this.output.join('\n').trim() + '\n';
    }

    private visitNode(node: Node, prevSibling?: Node) {
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
                this.visitStatement(node);
                break;
            default:
                // Handle unknowns or unexpected types gracefully if needed
                break;
        }
    }

    private visitProgram(node: Node) {
        let prev: Node | undefined = undefined;
        for (const child of node.children) {
            // Skip whitespace-only nodes - they interfere with spacing logic
            if (this.isWhitespaceOnlyNode(child)) { continue; }
            if (this.handleVerticalWhitespace(prev, child)) {
                prev = child;
                continue;
            }
            this.visitNode(child, prev);
            prev = child;
        }
    }

    private visitBlock(node: Node) {
        this.currentIndentLevel++;
        let prev: Node | undefined = undefined;
        for (const child of node.children) {
            // Skip whitespace-only nodes - they interfere with spacing logic
            if (this.isWhitespaceOnlyNode(child)) { continue; }
            if (this.handleVerticalWhitespace(prev, child)) {
                prev = child;
                continue;
            }
            this.visitNode(child, prev);
            prev = child;
        }
        this.currentIndentLevel--;
    }

    private visitStatement(node: Node) {
        let indentLevel = this.currentIndentLevel;

        // Temporary dedent for PARAMETERS which are syntactically children but visually top-level in procedure
        const firstToken = node.tokens.find(t => t.type !== TokenType.Whitespace && t.type !== TokenType.Comment);
        if (firstToken) {
            const text = firstToken.text.toUpperCase().replace(/^:/, ''); // Handle legacy colon
            if (text === 'PARAMETERS') {
                indentLevel = Math.max(0, indentLevel - 1);
            }
        }

        const line = this.statementPrinter.printStatement(node, indentLevel);
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
}
