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
    memberExpression = "memberExpression", // Keep for obj:prop
    assignmentExpression = "assignmentExpression",
    identifier = "identifier",
    literal = "literal",
    comment = "comment",
    declaration = "declaration",
    parameter = "parameter",
    statement = "statement",
    codeBlockLiteral = "codeBlockLiteral",
    arrayAccess = "arrayAccess", // Added for myArray[index]
    arraySubscript = "arraySubscript", // Added for [index] or [index, index2]
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

        let lastPosition = -1;
        let stuckCount = 0;
        const MAX_STUCK_ATTEMPTS = 10;

        while (!this.isAtEnd()) {
            const currentPosition = this.current;

            // Check if we're stuck (not making progress)
            if (currentPosition === lastPosition) {
                stuckCount++;
                if (stuckCount >= MAX_STUCK_ATTEMPTS) {
                    console.warn(
                        `Parser stuck at position ${currentPosition}, breaking out of parse loop`
                    );
                    break;
                }
            } else {
                stuckCount = 0; // Reset stuck counter if we made progress
            }

            lastPosition = currentPosition;

            try {
                const statement = this.parseStatement();
                if (statement) {
                    program.children.push(statement);
                }

                // Ensure we always make progress - if parseStatement didn't advance, do it manually
                if (this.current === currentPosition && !this.isAtEnd()) {
                    this.advance();
                }
            } catch (error) {
                // Skip problematic tokens and continue - ensure we advance
                if (!this.isAtEnd()) {
                    this.advance();
                }
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
        if (this.check(TokenType.parameters)) {
            return this.parseParameters();
        }
        if (this.check(TokenType.singleLineComment) || this.check(TokenType.blockComment)) {
            // Skip comments - they should not appear in the AST
            this.advance();
            return null;
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

        // Parse inline parameters if present (immediately after procedure name)
        if (this.check(TokenType.parameters)) {
            node.children.push(this.parseParameters());
        }

        // Parse procedure body
        while (!this.check(TokenType.endproc) && !this.isAtEnd()) {
            // Check for parameters as a separate statement within the procedure
            if (this.check(TokenType.parameters)) {
                node.children.push(this.parseParameters());
            } else {
                const statement = this.parseStatement();
                if (statement) {
                    node.children.push(statement);
                }
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

        // Parse variable names (can be multiple, separated by commas)
        while (this.check(TokenType.identifier)) {
            const nameNode: ASTNode = {
                type: ASTNodeType.identifier,
                value: this.advance().value,
                children: [],
            };
            node.children.push(nameNode);

            // Check for comma and continue parsing more identifiers
            if (this.check(TokenType.comma)) {
                this.advance(); // consume comma
            } else {
                break; // no more identifiers
            }
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

        // Parse parameter list with comma separation
        while (this.check(TokenType.identifier)) {
            const paramNode: ASTNode = {
                type: ASTNodeType.identifier,
                value: this.advance().value,
                children: [],
            };
            node.children.push(paramNode);

            // Check for comma and continue parsing more parameters
            if (this.check(TokenType.comma)) {
                this.advance(); // consume comma
            } else {
                break; // no more parameters
            }
        }

        return node;
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
                TokenType.greaterEqual,
                TokenType.simpleEquals
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
        if (this.match(TokenType.not, TokenType.minus, TokenType.plus, TokenType.bang)) {
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
        const initialCurrent = this.current; // Save current position for backtracking

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

        // Attempt to parse DateLiteral with a lookahead
        if (this.check(TokenType.lbrace)) {
            // Lookahead to differentiate DateLiteral from ArrayLiteral or CodeBlockLiteral
            // A DateLiteral looks like: { Number, Number, Number ... }
            // An ArrayLiteral could be: { Expression, Expression ... } or {}
            // A CodeBlockLiteral looks like: {| IdentifierList | ExpressionList }

            const initialCurrent = this.current; // Save current position for backtracking if lookahead fails

            if (
                this.tokens[initialCurrent + 1]?.type === TokenType.numberLiteral &&
                this.tokens[initialCurrent + 2]?.type === TokenType.comma &&
                this.tokens[initialCurrent + 3]?.type === TokenType.numberLiteral &&
                this.tokens[initialCurrent + 4]?.type === TokenType.comma &&
                this.tokens[initialCurrent + 5]?.type === TokenType.numberLiteral
            ) {
                // Strong indication of a DateLiteral, proceed to parse
                const lbraceToken = this.advance(); // consume {
                const dateParts: ASTNode[] = [];
                let value = lbraceToken.value;

                // Parse Year, Month, Day
                for (let i = 0; i < 3; i++) {
                    if (this.check(TokenType.numberLiteral)) {
                        const numToken = this.advance();
                        dateParts.push({
                            type: ASTNodeType.literal,
                            value: numToken.value,
                            children: [],
                            line: numToken.position.line,
                        });
                        value += numToken.value;
                        if (i < 2) {
                            if (this.check(TokenType.comma)) {
                                value += this.advance().value; // consume ,
                            } else {
                                this.current = initialCurrent; // Backtrack
                                return null; // Not a valid date literal structure
                            }
                        }
                    } else {
                        this.current = initialCurrent; // Backtrack
                        return null; // Not a valid date literal structure
                    }
                }

                // Optionally parse Hour, Minute, Second
                if (this.check(TokenType.comma)) {
                    value += this.advance().value; // consume , before Hour
                    for (let i = 0; i < 3; i++) {
                        if (this.check(TokenType.numberLiteral)) {
                            const numToken = this.advance();
                            dateParts.push({
                                type: ASTNodeType.literal,
                                value: numToken.value,
                                children: [],
                                line: numToken.position.line,
                            });
                            value += numToken.value;
                            if (i < 2) {
                                if (this.check(TokenType.comma)) {
                                    value += this.advance().value; // consume ,
                                } else {
                                    this.current = initialCurrent; // Backtrack
                                    return null; // Incomplete time part
                                }
                            }
                        } else {
                            this.current = initialCurrent; // Backtrack
                            return null; // Incomplete time part
                        }
                    }
                }

                if (this.check(TokenType.rbrace)) {
                    value += this.advance().value; // consume }
                    return {
                        type: ASTNodeType.literal, // Consider ASTNodeType.dateLiteral if defined
                        value: value,
                        children: dateParts,
                        line: lbraceToken.position.line,
                    };
                } else {
                    this.current = initialCurrent; // Backtrack, missing closing brace
                    return null;
                }
            }
            // If lookahead doesn't match DateLiteral, current remains initialCurrent
            // Fall through to other parsing logic (e.g., array, code block, or parenthesized expression)
        }

        // Attempt to parse CodeBlockLiteral: {| IdentifierList | ExpressionList }
        if (this.check(TokenType.lBracePipe)) {
            // Corrected to check for lbracePipe
            const lbracePipeToken = this.advance(); // consume {|

            const params: ASTNode[] = [];
            let value = lbracePipeToken.value; // Start with {| token value

            // Parse IdentifierList (optional)
            if (this.check(TokenType.identifier)) {
                do {
                    const identToken = this.advance();
                    params.push({
                        type: ASTNodeType.identifier,
                        value: identToken.value,
                        children: [],
                        line: identToken.position.line,
                    });
                    value += identToken.value;
                    if (this.check(TokenType.comma)) {
                        value += this.advance().value; // consume ,
                    }
                } while (this.check(TokenType.identifier));
            }

            if (this.check(TokenType.pipe)) {
                value += this.advance().value; // consume |
            } else {
                // This is not a valid code block, backtrack or handle error
                this.current = initialCurrent; // Backtrack
                return null;
            }

            const expressions: ASTNode[] = [];
            // Parse ExpressionList
            do {
                const expr = this.parseExpression();
                if (expr) {
                    expressions.push(expr);
                    if (expr.raw) {
                        value += expr.raw;
                    } else if (expr.value) {
                        value += expr.value;
                    }
                }
                // Consume comma if present before the next expression
                if (this.check(TokenType.comma)) {
                    if (this.tokens[this.current + 1]?.type !== TokenType.rbrace) {
                        // Ensure comma is not before closing brace
                        value += this.advance().value;
                    } else {
                        // Comma before closing brace, likely an error or end of list
                        break;
                    }
                } else if (!this.check(TokenType.rbrace)) {
                    // No comma, and not a closing brace, means end of expression list or syntax error
                    break;
                }
            } while (!this.check(TokenType.rbrace) && !this.isAtEnd());

            if (this.check(TokenType.rbrace)) {
                value += this.advance().value; // consume }

                // Group expressions into a single node if there are multiple, or use the single expression directly
                const expressionNodeChildren = expressions;
                let expressionNode: ASTNode | null = null;

                if (expressions.length > 0) {
                    expressionNode = {
                        type: ASTNodeType.expression, // Or a more specific type like ASTNodeType.expressionList
                        children: expressionNodeChildren,
                        line:
                            expressions.length > 0
                                ? expressions[0].line
                                : lbracePipeToken.position.line,
                        // value: expressions.map(e => e.value || e.raw).join(\', \') // Optional: raw text of expressions
                    };
                }

                const finalChildren: ASTNode[] = [];
                if (params.length > 0) {
                    // Group params into a single node if there are multiple, or use the single param directly
                    if (params.length === 1) {
                        finalChildren.push(params[0]);
                    } else {
                        finalChildren.push({
                            type: ASTNodeType.parameter, // Or a more specific type like ASTNodeType.identifierList
                            children: params,
                            line:
                                params.length > 0 ? params[0].line : lbracePipeToken.position.line,
                        });
                    }
                }
                if (expressionNode) {
                    finalChildren.push(expressionNode);
                }

                return {
                    type: ASTNodeType.codeBlockLiteral,
                    value: value,
                    children: finalChildren,
                    line: lbracePipeToken.position.line,
                };
            } else {
                // Missing closing brace, backtrack or handle error
                this.current = initialCurrent; // Backtrack
                return null;
            }
        } // Parse ArrayLiteral: { ExpressionList } or {}
        if (this.match(TokenType.lbrace)) {
            const lbraceToken = this.previous();
            const elements: ASTNode[] = [];
            let value = lbraceToken.value;

            if (!this.check(TokenType.rbrace)) {
                do {
                    const element = this.parseExpression();
                    if (element) {
                        elements.push(element);
                        if (element.raw) {
                            value += element.raw;
                        } else if (element.value) {
                            value += element.value;
                        }
                    }

                    // Check for comma and consume it if present
                    if (this.check(TokenType.comma)) {
                        value += this.advance().value; // consume comma
                        // If next token is closing brace, break (trailing comma case)
                        if (this.check(TokenType.rbrace)) {
                            break;
                        }
                    } else {
                        // No comma, so we're done with the list
                        break;
                    }
                } while (!this.check(TokenType.rbrace) && !this.isAtEnd());
            }

            if (this.check(TokenType.rbrace)) {
                value += this.advance().value; // consume }
                return {
                    type: ASTNodeType.literal, // ArrayLiteral type
                    value: value,
                    children: elements,
                    line: lbraceToken.position.line,
                };
            } else {
                // Missing closing brace, create error or handle appropriately
                return {
                    type: ASTNodeType.literal,
                    value: value,
                    children: elements,
                    line: lbraceToken.position.line,
                };
            }
        } // Handle bitwise operations as function calls
        if (this.match(TokenType.bitwiseAnd, TokenType.bitwiseOr, TokenType.bitwiseNot)) {
            const token = this.previous();

            // Bitwise operations must be followed by '('
            if (this.check(TokenType.lparen)) {
                this.advance(); // consume '('

                const args: ASTNode[] = [];

                // Parse function arguments
                if (!this.check(TokenType.rparen)) {
                    do {
                        const arg = this.parseExpression();
                        if (arg) {
                            args.push(arg);
                        }
                    } while (this.match(TokenType.comma));
                }

                this.consume(TokenType.rparen, "Expected ')' after function arguments");
                return {
                    type: ASTNodeType.callExpression,
                    value: token.value,
                    children: args,
                    line: token.position.line,
                };
            }

            // If not followed by '(', treat as invalid
            return null;
        }

        // Handle CreateUDObject as function call
        if (this.match(TokenType.createUDObject)) {
            const token = this.previous();

            // CreateUDObject must be followed by '('
            if (this.check(TokenType.lparen)) {
                this.advance(); // consume '('

                const args: ASTNode[] = [];

                // Parse function arguments
                if (!this.check(TokenType.rparen)) {
                    do {
                        const arg = this.parseExpression();
                        if (arg) {
                            args.push(arg);
                        }
                    } while (this.match(TokenType.comma));
                }

                this.consume(TokenType.rparen, "Expected ')' after function arguments");
                return {
                    type: ASTNodeType.callExpression,
                    value: token.value,
                    children: args,
                    line: token.position.line,
                };
            }

            // If not followed by '(', treat as invalid
            return null;
        }

        // Handle ExecUDF as function call
        if (this.match(TokenType.execUDF)) {
            const token = this.previous();

            // ExecUDF must be followed by '('
            if (this.check(TokenType.lparen)) {
                this.advance(); // consume '('

                const args: ASTNode[] = [];

                // Parse function arguments
                if (!this.check(TokenType.rparen)) {
                    do {
                        const arg = this.parseExpression();
                        if (arg) {
                            args.push(arg);
                        }
                    } while (this.match(TokenType.comma));
                }

                this.consume(TokenType.rparen, "Expected ')' after function arguments");
                return {
                    type: ASTNodeType.callExpression,
                    value: token.value,
                    children: args,
                    line: token.position.line,
                };
            }

            // If not followed by '(', treat as invalid
            return null;
        }
        if (this.match(TokenType.identifier)) {
            const token = this.previous();

            // Check if this is a function call (identifier followed by '(')
            if (this.check(TokenType.lparen)) {
                this.advance(); // consume '('

                const args: ASTNode[] = [];

                // Parse function arguments
                if (!this.check(TokenType.rparen)) {
                    do {
                        const arg = this.parseExpression();
                        if (arg) {
                            args.push(arg);
                        }
                    } while (this.match(TokenType.comma));
                }

                this.consume(TokenType.rparen, "Expected ')' after function arguments");
                return {
                    type: ASTNodeType.callExpression,
                    value: token.value,
                    children: args,
                    line: token.position.line,
                };
            }

            // Check if this is array access (identifier followed by '[')
            if (this.check(TokenType.lbracket)) {
                let baseNode: ASTNode = {
                    type: ASTNodeType.identifier,
                    value: token.value,
                    children: [],
                    line: token.position.line,
                };

                // Parse array subscripts (supports chained access like arr[1][2])
                while (this.check(TokenType.lbracket)) {
                    this.advance(); // consume '['

                    const subscriptArgs: ASTNode[] = [];

                    // Parse array subscript expressions (supports comma-separated like arr[1,2])
                    if (!this.check(TokenType.rbracket)) {
                        do {
                            const arg = this.parseExpression();
                            if (arg) {
                                subscriptArgs.push(arg);
                            }
                        } while (this.match(TokenType.comma));
                    }

                    this.consume(TokenType.rbracket, "Expected ']' after array subscript");

                    // Create array access node
                    baseNode = {
                        type: ASTNodeType.arrayAccess,
                        value: token.value, // Keep original identifier name
                        children: [baseNode, ...subscriptArgs],
                        line: token.position.line,
                    };
                }
                return baseNode;
            }

            // Check if this is property access (identifier followed by ':')
            if (this.check(TokenType.colon)) {
                this.advance(); // consume ':'

                // Expect an identifier after the colon
                if (this.check(TokenType.identifier)) {
                    const propertyToken = this.advance();

                    return {
                        type: ASTNodeType.memberExpression,
                        value: `${token.value}:${propertyToken.value}`,
                        children: [
                            {
                                type: ASTNodeType.identifier,
                                value: token.value,
                                children: [],
                                line: token.position.line,
                            },
                            {
                                type: ASTNodeType.identifier,
                                value: propertyToken.value,
                                children: [],
                                line: propertyToken.position.line,
                            },
                        ],
                        line: token.position.line,
                    };
                } else {
                    // Invalid property access, backtrack
                    this.current--; // Put back the colon
                }
            }

            // Simple identifier
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
