/**
 * SSL Parser - Syntax analysis for SSL (STARLIMS Scripting Language)
 * Based on the SSL EBNF grammar specification
 */

import { Token, TokenType } from "./tokenizer";

export enum ASTNodeType {
    program = "program",
    procedure = "procedure",
    ifStatement = "ifStatement",
    whileStatement = "whileStatement",
    forStatement = "forStatement",
    beginCaseStatement = "beginCaseStatement",
    caseStatement = "caseStatement",
    tryStatement = "tryStatement",
    regionStatement = "regionStatement",
    classStatement = "classStatement",
    blockStatement = "blockStatement",
    expression = "expression",
    binaryExpression = "binaryExpression",
    unaryExpression = "unaryExpression",
    callExpression = "callExpression",
    memberExpression = "memberExpression",
    assignmentExpression = "assignmentExpression",
    identifier = "identifier",
    literal = "literal",
    comment = "comment",
    declaration = "declaration",
    parameter = "parameter",
    statement = "statement",
    unknown = "unknown",
}

export interface ASTNode {
    type: ASTNodeType;
    value?: string;
    children: ASTNode[];
    line?: number;
    column?: number;
    startOffset?: number;
    endOffset?: number;
    raw?: string;
}

export class SSLParser {
    private tokens: Token[];
    private current = 0;

    constructor(tokens: Token[]) {
        this.tokens = tokens.filter(
            (token) => token.type !== TokenType.whitespace && token.type !== TokenType.newline
        );
    }

    public parse(): ASTNode {
        const program: ASTNode = {
            type: ASTNodeType.program,
            children: [],
        };

        while (!this.isAtEnd()) {
            try {
                const statement = this.parseStatement();
                if (statement) {
                    program.children.push(statement);
                }
            } catch (error) {
                // Skip problematic tokens and continue
                this.advance();
            }
        }

        return program;
    }

    private parseStatement(): ASTNode | null {
        if (this.check(TokenType.procedure)) {
            return this.parseProcedure();
        }
        if (this.check(TokenType.if)) {
            return this.parseIfStatement();
        }
        if (this.check(TokenType.while)) {
            return this.parseWhileStatement();
        }
        if (this.check(TokenType.for)) {
            return this.parseForStatement();
        }
        if (this.check(TokenType.begincase)) {
            return this.parseBeginCaseStatement();
        }
        if (this.check(TokenType.try)) {
            return this.parseTryStatement();
        }
        if (this.check(TokenType.region)) {
            return this.parseRegionStatement();
        }
        if (this.check(TokenType.class)) {
            return this.parseClassStatement();
        }
        if (this.check(TokenType.declare)) {
            return this.parseDeclaration();
        }
        if (this.check(TokenType.singleLineComment) || this.check(TokenType.blockComment)) {
            return this.parseComment();
        }

        // Default: try to parse as expression statement
        return this.parseExpressionStatement();
    }

    private parseProcedure(): ASTNode {
        const node: ASTNode = {
            type: ASTNodeType.procedure,
            children: [],
            line: this.peek().position.line,
        };

        this.consume(TokenType.procedure, "Expected ':PROCEDURE'");

        // Parse procedure name if present
        if (this.check(TokenType.identifier)) {
            const nameNode: ASTNode = {
                type: ASTNodeType.identifier,
                value: this.advance().value,
                children: [],
            };
            node.children.push(nameNode);
        }

        // Parse parameters if present
        if (this.check(TokenType.parameters)) {
            node.children.push(this.parseParameters());
        }

        // Parse procedure body
        while (!this.check(TokenType.endproc) && !this.isAtEnd()) {
            const statement = this.parseStatement();
            if (statement) {
                node.children.push(statement);
            }
        }

        this.consume(TokenType.endproc, "Expected ':ENDPROC'");
        return node;
    }

    private parseIfStatement(): ASTNode {
        const node: ASTNode = {
            type: ASTNodeType.ifStatement,
            children: [],
            line: this.peek().position.line,
        };

        this.consume(TokenType.if, "Expected ':IF'");

        // Parse condition
        const condition = this.parseExpression();
        if (condition) {
            node.children.push(condition);
        }

        // Parse then block
        const thenBlock = this.parseBlock([TokenType.else, TokenType.endif]);
        node.children.push(thenBlock);

        // Parse else block if present
        if (this.check(TokenType.else)) {
            this.advance();
            const elseBlock = this.parseBlock([TokenType.endif]);
            node.children.push(elseBlock);
        }

        this.consume(TokenType.endif, "Expected ':ENDIF'");
        return node;
    }

    private parseWhileStatement(): ASTNode {
        const node: ASTNode = {
            type: ASTNodeType.whileStatement,
            children: [],
            line: this.peek().position.line,
        };

        this.consume(TokenType.while, "Expected ':WHILE'");

        // Parse condition
        const condition = this.parseExpression();
        if (condition) {
            node.children.push(condition);
        }

        // Parse body
        const body = this.parseBlock([TokenType.endwhile]);
        node.children.push(body);

        this.consume(TokenType.endwhile, "Expected ':ENDWHILE'");
        return node;
    }

    private parseForStatement(): ASTNode {
        const node: ASTNode = {
            type: ASTNodeType.forStatement,
            children: [],
            line: this.peek().position.line,
        };

        this.consume(TokenType.for, "Expected ':FOR'");

        // Parse for loop components (simplified)
        while (!this.check(TokenType.next) && !this.isAtEnd()) {
            const statement = this.parseStatement();
            if (statement) {
                node.children.push(statement);
            }
        }

        this.consume(TokenType.next, "Expected ':NEXT'");
        return node;
    }

    private parseBeginCaseStatement(): ASTNode {
        const node: ASTNode = {
            type: ASTNodeType.beginCaseStatement,
            children: [],
            line: this.peek().position.line,
        };

        this.consume(TokenType.begincase, "Expected ':BEGINCASE'");

        // Parse case blocks
        while (!this.check(TokenType.endcase) && !this.isAtEnd()) {
            if (this.check(TokenType.case) || this.check(TokenType.otherwise)) {
                const caseNode: ASTNode = {
                    type: ASTNodeType.caseStatement,
                    children: [],
                    line: this.peek().position.line,
                };
                this.advance(); // consume CASE or OTHERWISE

                // Parse case body
                const caseBody = this.parseBlock([
                    TokenType.case,
                    TokenType.otherwise,
                    TokenType.endcase,
                ]);
                caseNode.children.push(caseBody);
                node.children.push(caseNode);
            } else {
                const statement = this.parseStatement();
                if (statement) {
                    node.children.push(statement);
                }
            }
        }

        this.consume(TokenType.endcase, "Expected ':ENDCASE'");
        return node;
    }

    private parseTryStatement(): ASTNode {
        const node: ASTNode = {
            type: ASTNodeType.tryStatement,
            children: [],
            line: this.peek().position.line,
        };

        this.consume(TokenType.try, "Expected ':TRY'");

        // Parse try block
        const tryBlock = this.parseBlock([TokenType.catch, TokenType.finally, TokenType.endtry]);
        node.children.push(tryBlock);

        // Parse catch block if present
        if (this.check(TokenType.catch)) {
            this.advance();
            const catchBlock = this.parseBlock([TokenType.finally, TokenType.endtry]);
            node.children.push(catchBlock);
        }

        // Parse finally block if present
        if (this.check(TokenType.finally)) {
            this.advance();
            const finallyBlock = this.parseBlock([TokenType.endtry]);
            node.children.push(finallyBlock);
        }

        this.consume(TokenType.endtry, "Expected ':ENDTRY'");
        return node;
    }

    private parseRegionStatement(): ASTNode {
        const node: ASTNode = {
            type: ASTNodeType.regionStatement,
            children: [],
            line: this.peek().position.line,
        };

        this.consume(TokenType.region, "Expected ':REGION'");

        // Parse region body
        while (!this.check(TokenType.endregion) && !this.isAtEnd()) {
            const statement = this.parseStatement();
            if (statement) {
                node.children.push(statement);
            }
        }

        this.consume(TokenType.endregion, "Expected ':ENDREGION'");
        return node;
    }

    private parseClassStatement(): ASTNode {
        const node: ASTNode = {
            type: ASTNodeType.classStatement,
            children: [],
            line: this.peek().position.line,
        };

        this.consume(TokenType.class, "Expected ':CLASS'");

        // Parse class name if present
        if (this.check(TokenType.identifier)) {
            const nameNode: ASTNode = {
                type: ASTNodeType.identifier,
                value: this.advance().value,
                children: [],
            };
            node.children.push(nameNode);
        }

        // Parse class body (simplified - no specific end token in grammar)
        // This would need to be refined based on actual SSL class syntax
        return node;
    }

    private parseDeclaration(): ASTNode {
        const node: ASTNode = {
            type: ASTNodeType.declaration,
            children: [],
            line: this.peek().position.line,
        };

        this.consume(TokenType.declare, "Expected ':DECLARE'");

        // Parse variable name
        if (this.check(TokenType.identifier)) {
            const nameNode: ASTNode = {
                type: ASTNodeType.identifier,
                value: this.advance().value,
                children: [],
            };
            node.children.push(nameNode);
        }

        return node;
    }

    private parseParameters(): ASTNode {
        const node: ASTNode = {
            type: ASTNodeType.parameter,
            children: [],
            line: this.peek().position.line,
        };

        this.consume(TokenType.parameters, "Expected ':PARAMETERS'");

        // Parse parameter list (simplified)
        while (!this.isAtEnd() && this.check(TokenType.identifier)) {
            const paramNode: ASTNode = {
                type: ASTNodeType.identifier,
                value: this.advance().value,
                children: [],
            };
            node.children.push(paramNode);

            if (this.check(TokenType.comma)) {
                this.advance();
            }
        }

        return node;
    }

    private parseComment(): ASTNode {
        const token = this.advance();
        return {
            type: ASTNodeType.comment,
            value: token.value,
            children: [],
            line: token.position.line,
        };
    }

    private parseExpressionStatement(): ASTNode | null {
        const expr = this.parseExpression();
        return expr
            ? {
                  type: ASTNodeType.statement,
                  children: expr ? [expr] : [],
                  line: this.peek().position.line,
              }
            : null;
    }

    private parseExpression(): ASTNode | null {
        return this.parseAssignment();
    }

    private parseAssignment(): ASTNode | null {
        let expr = this.parseLogical();

        if (
            this.match(
                TokenType.assign,
                TokenType.plusAssign,
                TokenType.minusAssign,
                TokenType.multAssign,
                TokenType.divAssign,
                TokenType.powerAssign
            )
        ) {
            const operator = this.previous();
            const right = this.parseAssignment();

            if (expr && right) {
                return {
                    type: ASTNodeType.assignmentExpression,
                    value: operator.value,
                    children: [expr, right],
                    line: operator.position.line,
                };
            }
        }

        return expr;
    }

    private parseLogical(): ASTNode | null {
        let expr = this.parseComparison();

        while (this.match(TokenType.and, TokenType.or)) {
            const operator = this.previous();
            const right = this.parseComparison();

            if (expr && right) {
                expr = {
                    type: ASTNodeType.binaryExpression,
                    value: operator.value,
                    children: [expr, right],
                    line: operator.position.line,
                };
            }
        }

        return expr;
    }

    private parseComparison(): ASTNode | null {
        let expr = this.parseTerm();

        while (
            this.match(
                TokenType.equals,
                TokenType.notEquals,
                TokenType.lessThan,
                TokenType.greaterThan,
                TokenType.lessEqual,
                TokenType.greaterEqual
            )
        ) {
            const operator = this.previous();
            const right = this.parseTerm();

            if (expr && right) {
                expr = {
                    type: ASTNodeType.binaryExpression,
                    value: operator.value,
                    children: [expr, right],
                    line: operator.position.line,
                };
            }
        }

        return expr;
    }

    private parseTerm(): ASTNode | null {
        let expr = this.parseFactor();

        while (this.match(TokenType.plus, TokenType.minus)) {
            const operator = this.previous();
            const right = this.parseFactor();

            if (expr && right) {
                expr = {
                    type: ASTNodeType.binaryExpression,
                    value: operator.value,
                    children: [expr, right],
                    line: operator.position.line,
                };
            }
        }

        return expr;
    }

    private parseFactor(): ASTNode | null {
        let expr = this.parseUnary();

        while (
            this.match(TokenType.multiply, TokenType.divide, TokenType.modulo, TokenType.power)
        ) {
            const operator = this.previous();
            const right = this.parseUnary();

            if (expr && right) {
                expr = {
                    type: ASTNodeType.binaryExpression,
                    value: operator.value,
                    children: [expr, right],
                    line: operator.position.line,
                };
            }
        }

        return expr;
    }

    private parseUnary(): ASTNode | null {
        if (this.match(TokenType.not, TokenType.minus, TokenType.plus)) {
            const operator = this.previous();
            const right = this.parseUnary();

            if (right) {
                return {
                    type: ASTNodeType.unaryExpression,
                    value: operator.value,
                    children: [right],
                    line: operator.position.line,
                };
            }
        }

        return this.parsePrimary();
    }

    private parsePrimary(): ASTNode | null {
        if (
            this.match(
                TokenType.stringLiteral,
                TokenType.numberLiteral,
                TokenType.booleanLiteral,
                TokenType.nilLiteral
            )
        ) {
            const token = this.previous();
            return {
                type: ASTNodeType.literal,
                value: token.value,
                children: [],
                line: token.position.line,
            };
        }

        if (this.match(TokenType.identifier)) {
            const token = this.previous();
            return {
                type: ASTNodeType.identifier,
                value: token.value,
                children: [],
                line: token.position.line,
            };
        }

        if (this.match(TokenType.lparen)) {
            const expr = this.parseExpression();
            this.consume(TokenType.rparen, "Expected ')' after expression");
            return expr;
        }

        // If we can't parse anything, skip this token
        if (!this.isAtEnd()) {
            this.advance();
        }
        return null;
    }

    private parseBlock(endTokens: TokenType[]): ASTNode {
        const block: ASTNode = {
            type: ASTNodeType.blockStatement,
            children: [],
            line: this.peek().position.line,
        };

        while (!this.isAtEnd() && !endTokens.some((token) => this.check(token))) {
            const statement = this.parseStatement();
            if (statement) {
                block.children.push(statement);
            }
        }

        return block;
    }

    // Utility methods
    private match(...types: TokenType[]): boolean {
        for (const type of types) {
            if (this.check(type)) {
                this.advance();
                return true;
            }
        }
        return false;
    }

    private check(type: TokenType): boolean {
        if (this.isAtEnd()) {
            return false;
        }
        return this.peek().type === type;
    }

    private advance(): Token {
        if (!this.isAtEnd()) {
            this.current++;
        }
        return this.previous();
    }

    private isAtEnd(): boolean {
        return this.current >= this.tokens.length || this.peek().type === TokenType.eof;
    }

    private peek(): Token {
        if (this.current >= this.tokens.length) {
            return {
                type: TokenType.eof,
                value: "",
                position: { line: 0, column: 0, offset: 0 },
                length: 0,
            };
        }
        return this.tokens[this.current];
    }

    private previous(): Token {
        return this.tokens[this.current - 1];
    }

    private consume(type: TokenType, message: string): Token {
        if (this.check(type)) {
            return this.advance();
        }

        // For error recovery, just advance if we can't find the expected token
        if (!this.isAtEnd()) {
            this.advance();
        }

        return this.previous();
    }
}
