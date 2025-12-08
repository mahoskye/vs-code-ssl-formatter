import { Node, NodeType, Parser } from './parser';
import { Lexer, TokenType } from './lexer';
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
        this.whitespaceManager = new WhitespaceManager();
        this.statementPrinter = new StatementPrinter(options, this.sqlFormatter);
    }

    public format(text: string, initialIndentLevel: number = 0): string {
        const lexer = new Lexer(text);
        const tokens = lexer.tokenize();
        const parser = new Parser(tokens);
        const root = parser.parse();

        // Recursively print nodes
        this.output = [];
        this.currentIndentLevel = initialIndentLevel;

        // Track previous node for vertical whitespace logic
        this.visit(root, undefined);

        // Join lines
        return this.output.join('\n').trim() + '\n';
    }

    private visit(node: Node, prevSibling?: Node) {
        if (node.type === NodeType.Program) {
            let prev: Node | undefined = undefined;
            for (const child of node.children) {
                if (prev) {
                    const lines = this.whitespaceManager.getVerticalWhitespace(prev, child);
                    if (lines === -1) {
                        // Trailing inline comment - append to previous line
                        const comment = this.statementPrinter.printStatement(child, this.currentIndentLevel);
                        if (this.output.length > 0) {
                            this.output[this.output.length - 1] += ' ' + comment;
                        }
                        prev = child;
                        continue;
                    }
                    for (let i = 0; i < lines; i++) { this.output.push(""); }
                }
                this.visit(child, prev);
                prev = child;
            }
        } else if (node.type === NodeType.Block) {
            this.currentIndentLevel++;
            let prev: Node | undefined = undefined;
            for (const child of node.children) {
                // Check for vertical whitespace before child
                if (prev) {
                    const lines = this.whitespaceManager.getVerticalWhitespace(prev, child);
                    if (lines === -1) {
                        // Trailing inline comment - append to previous line
                        const comment = this.statementPrinter.printStatement(child, this.currentIndentLevel);
                        if (this.output.length > 0) {
                            this.output[this.output.length - 1] += ' ' + comment;
                        }
                        prev = child;
                        continue;
                    }
                    for (let i = 0; i < lines; i++) { this.output.push(""); }
                }
                this.visit(child, prev);
                prev = child;
            }
            this.currentIndentLevel--;
        } else if (node.type === NodeType.Statement || node.type === NodeType.Comment || node.type === NodeType.RegionStart || node.type === NodeType.RegionEnd) {
            // Temporary dedent for PARAMETERS and ENDCASE which are often inside the block they define/close
            let indentLevel = this.currentIndentLevel;
            const firstToken = node.tokens.find(t => t.type !== TokenType.Whitespace && t.type !== TokenType.Comment);
            if (firstToken) {
                const text = firstToken.text.toUpperCase().replace(/^:/, ''); // Handle legacy colon
                // PARAMETERS is semantically part of the procedure definition but syntactically a child
                if (text === 'PARAMETERS') {
                    indentLevel = Math.max(0, indentLevel - 1);
                }
            }

            const line = this.statementPrinter.printStatement(node, indentLevel);
            this.output.push(this.indentString.repeat(indentLevel) + line);
        }
    }

    private getIndent(): string {
        return this.indentString.repeat(this.currentIndentLevel);
    }
}
