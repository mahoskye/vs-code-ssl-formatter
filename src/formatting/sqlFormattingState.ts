import { SqlToken, SqlTokenType } from './sqlLexer';
import { SQL } from "../constants/sql";

export interface ParenContext {
    isMultiline: boolean;
    indentOffset: number;
    closingIndent: number;
    wasInSelectColumns: boolean;
}

export class SqlFormattingState {
    public parenDepth: number = 0;
    public parenStack: ParenContext[] = [];

    // Context flags
    public afterUpdate: boolean = false;
    public afterSet: boolean = false;
    public inWhereClause: boolean = false;
    public inInsert: boolean = false;
    public inInsertColumnList: boolean = false;
    public inSelectColumns: boolean = false;

    // Line state
    public lineHasExtraIndent: boolean = false;

    // Formatting state
    public poppedClosingIndent: number = -1;

    public updateContext(token: SqlToken): void {
        const upperText = token.text.toUpperCase();

        if (token.type === SqlTokenType.Keyword) {
            if (upperText === SQL.KEYWORDS.UPDATE) { this.afterUpdate = true; }
            if (upperText === SQL.KEYWORDS.WHERE) { this.inWhereClause = true; this.afterSet = false; }
            if (upperText === SQL.KEYWORDS.SET) { this.afterUpdate = false; this.afterSet = true; }
            if (upperText === SQL.KEYWORDS.INSERT) { this.inInsert = true; this.inInsertColumnList = true; }
            if (upperText === SQL.KEYWORDS.SELECT) { this.inSelectColumns = true; this.inInsertColumnList = false; }
            if (upperText === SQL.KEYWORDS.VALUES) { this.inInsertColumnList = false; }
            if (upperText === SQL.KEYWORDS.FROM && this.inSelectColumns) { this.inSelectColumns = false; }
        }
    }

    public pushParen(indentOffset: number, closingIndent: number): void {
        this.parenDepth++;
        this.parenStack.push({
            isMultiline: false,
            indentOffset,
            closingIndent,
            wasInSelectColumns: this.inSelectColumns
        });
    }

    public popParen(): ParenContext | undefined {
        const ctx = this.parenStack.pop();
        if (ctx) {
            this.poppedClosingIndent = ctx.closingIndent;
            this.parenDepth--;
            // Restore context
            this.inSelectColumns = ctx.wasInSelectColumns;
        }
        return ctx;
    }

    public getCurrentStackOffset(): number {
        return this.parenStack.reduce((sum, ctx) => sum + ctx.indentOffset, 0);
    }
}
