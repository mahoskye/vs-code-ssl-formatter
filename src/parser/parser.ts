/**
 * Recursive Descent Parser for SSL Language
 * Implements the EBNF grammar specification
 */

import { Token } from "../tokenizer/token";
import { TokenType } from "../tokenizer/tokenType";
import {
    ASTNode,
    ASTNodeType,
    ProgramNode,
    StatementNode,
    ExpressionNode,
    ClassDefinitionNode,
    ClassDeclarationNode,
    InheritStatementNode,
    ParameterDeclarationNode,
    ParametersStatementNode,
    ProcedureStatementNode,
    IfStatementNode,
    WhileLoopNode,
    ForLoopNode,
    SwitchStatementNode,
    TryBlockNode,
    AssignmentNode,
    DirectFunctionCallNode,
    BinaryExpressionNode,
    UnaryExpressionNode,
    LiteralExpressionNode,
    VariableAccessNode,
    PropertyAccessNode,
    ArrayAccessNode,
    IdentifierListNode,
    ArgumentListNode,
    createBaseNode,
} from "./ast";

/**
 * Represents a syntax error found during parsing
 */
export interface ParseError {
    message: string;
    token: Token;
    line: number;
    column: number;
    severity: "error" | "warning";
}

/**
 * Result of parsing operation
 */
export interface ParseResult {
    ast: ProgramNode;
    errors: ParseError[];
    success: boolean;
}

/**
 * SSL Recursive Descent Parser
 */
export class Parser {
    private tokens: Token[];
    private current: number = 0;
    private errors: ParseError[] = [];
    private panicMode: boolean = false;
    private isInsideProcedure: boolean = false;

    constructor(tokens: Token[]) {
        this.tokens = tokens;
    }
    /**
     * Main entry point for parsing
     */
    public parse(): ParseResult {
        this.current = 0;
        this.errors = [];
        this.panicMode = false;
        this.isInsideProcedure = false;

        const ast = this.parseProgram();

        return {
            ast,
            errors: this.errors,
            success: this.errors.length === 0,
        };
    }

    /**
     * Parse the top-level program
     * Program ::= ClassDefinition | {Statement}
     */
    private parseProgram(): ProgramNode {
        const startToken = this.peek();
        const body: (ClassDefinitionNode | StatementNode)[] = [];
        while (!this.isAtEnd()) {
            if (this.panicMode) {
                this.synchronize();
                if (this.isAtEnd()) {
                    break;
                }
                this.panicMode = false;
            }

            try {
                // Check if we're starting a class definition
                if (this.check(TokenType.COLON) && this.checkNext(TokenType.CLASS)) {
                    body.push(this.parseClassDefinition());
                } else {
                    const stmt = this.parseStatement();
                    if (stmt) {
                        body.push(stmt);
                    }
                }
            } catch (error) {
                this.panicMode = true;
                // Continue to try to parse more
            }
        }

        const endToken = this.previous();
        return {
            kind: ASTNodeType.Program,
            startToken,
            endToken,
            body,
        };
    }

    /**
     * Parse a class definition
     * ClassDefinition ::= ClassDeclaration [InheritStatement] {ClassMember}
     */
    private parseClassDefinition(): ClassDefinitionNode {
        const startToken = this.peek();

        this.consume(TokenType.COLON, "Expected ':' before class declaration");
        this.consume(TokenType.CLASS, "Expected 'CLASS' keyword");
        const className = this.consume(TokenType.IDENTIFIER, "Expected class name");
        const declaration = {
            kind: ASTNodeType.ClassDeclaration,
            startToken: startToken,
            endToken: className,
            name: className,
        } as ClassDeclarationNode;

        let inherit = undefined;
        const members: any[] = [];

        // Optional inherit statement
        if (this.check(TokenType.COLON) && this.checkNext(TokenType.INHERIT)) {
            this.advance(); // consume ':'
            this.advance(); // consume 'INHERIT'
            const inheritClassName = this.consume(
                TokenType.IDENTIFIER,
                "Expected inherited class name"
            );
            inherit = {
                kind: ASTNodeType.InheritStatement,
                startToken: this.previous(),
                endToken: inheritClassName,
                className: inheritClassName,
            } as InheritStatementNode;
        }

        // Parse class members
        while (!this.isAtEnd() && this.check(TokenType.COLON)) {
            if (this.checkNext(TokenType.DECLARE)) {
                // Class field declaration
                this.advance(); // consume ':'
                this.advance(); // consume 'DECLARE'
                const identifiers = this.parseIdentifierList();
                members.push({
                    kind: ASTNodeType.ClassFieldDeclaration,
                    startToken: this.previous(),
                    endToken: this.previous(),
                    identifiers,
                });
            } else if (this.checkNext(TokenType.PROCEDURE)) {
                // Method declaration
                const procedure = this.parseProcedureStatement();
                members.push({
                    kind: ASTNodeType.MethodDeclaration,
                    startToken: procedure.startToken,
                    endToken: procedure.endToken,
                    procedure,
                });
            } else {
                break;
            }
        }

        const endToken = this.previous();
        return {
            kind: ASTNodeType.ClassDefinition,
            startToken,
            endToken,
            declaration,
            inherit,
            members,
        };
    }
    /**
     * Parse a statement
     */ private parseStatement(): StatementNode | null {
        try {
            // Skip whitespace tokens (including NEWLINE)
            this.skipWhitespace();

            // Check if we've reached the end after skipping whitespace
            if (this.isAtEnd()) {
                return null;
            }

            // Skip comment tokens - they don't produce statements
            if (
                this.check(TokenType.SINGLE_LINE_COMMENT) ||
                this.check(TokenType.BLOCK_COMMENT) ||
                this.check(TokenType.REGION_COMMENT) ||
                this.check(TokenType.ENDREGION_COMMENT)
            ) {
                this.advance(); // Skip the comment token
                return null; // Comments don't produce statements
            }

            // Check for colon-prefixed statements
            if (this.check(TokenType.COLON)) {
                return this.parseColonStatement();
            }

            // Assignment or function call
            if (this.check(TokenType.IDENTIFIER)) {
                return this.parseAssignmentOrFunctionCall();
            }

            // Skip unexpected tokens
            this.error("Unexpected token");
            this.advance();
            return null;
        } catch (error) {
            this.panicMode = true;
            return null;
        }
    }
    /**
     * Parse colon-prefixed statements
     */
    private parseColonStatement(): StatementNode {
        this.consume(TokenType.COLON, "Expected ':'");

        if (this.match(TokenType.PROCEDURE)) {
            return this.parseProcedureStatement();
        } else if (this.match(TokenType.IF)) {
            return this.parseIfStatement();
        } else if (this.match(TokenType.WHILE)) {
            return this.parseWhileStatement();
        } else if (this.match(TokenType.FOR)) {
            return this.parseForStatement();
        } else if (this.match(TokenType.BEGINCASE)) {
            return this.parseSwitchStatement();
        } else if (this.match(TokenType.TRY)) {
            return this.parseTryStatement();
        } else if (this.match(TokenType.DECLARE)) {
            return this.parseDeclareStatement();
        } else if (this.match(TokenType.PARAMETERS)) {
            return this.parseParametersStatement();
        } else if (this.match(TokenType.PUBLIC)) {
            return this.parsePublicStatement();
        } else if (this.match(TokenType.INCLUDE)) {
            return this.parseIncludeStatement();
        } else if (this.match(TokenType.RETURN)) {
            return this.parseReturnStatement();
        } else if (this.match(TokenType.LABEL)) {
            return this.parseLabelStatement();
        } else if (this.match(TokenType.REGION)) {
            return this.parseRegionStatement();
        } else if (this.match(TokenType.EXITWHILE)) {
            return this.parseExitWhileStatement();
        } else if (this.match(TokenType.EXITFOR)) {
            return this.parseExitForStatement();
        } else if (this.match(TokenType.LOOP)) {
            return this.parseLoopContinueStatement();
        }

        this.error("Unexpected statement after ':'");
        return this.createErrorNode();
    }

    /**
     * Parse procedure statement body (after : and PROCEDURE are consumed)
     */ private parseProcedureStatementBody(): ProcedureStatementNode {
        const startToken = this.previous();
        const name = this.consume(TokenType.IDENTIFIER, "Expected procedure name");
        let parameters = undefined;
        let defaultParameters = undefined;
        const body: StatementNode[] = [];

        // Set flag to track that we're inside a procedure
        const wasInsideProcedure = this.isInsideProcedure;
        this.isInsideProcedure = true;

        // Skip whitespace before checking for parameters
        this.skipWhitespace();

        // Optional parameter declaration
        if (this.check(TokenType.COLON) && this.checkNext(TokenType.PARAMETERS)) {
            this.advance(); // consume ':'
            this.advance(); // consume 'PARAMETERS'
            const paramList = this.parseIdentifierList();
            parameters = {
                kind: ASTNodeType.ParameterDeclaration,
                startToken: this.previous(),
                endToken: this.previous(),
                parameters: paramList,
            } as ParameterDeclarationNode;
        }

        // Skip whitespace before checking for default parameters
        this.skipWhitespace();

        // Optional default parameter declaration
        if (this.check(TokenType.COLON) && this.checkNext(TokenType.DEFAULT)) {
            this.advance(); // consume ':'
            this.advance(); // consume 'DEFAULT'
            // For now, just consume the default parameters
            // TODO: Implement proper default parameter parsing
            this.parseIdentifierList();
        } // Parse procedure body
        while (!this.isAtEnd()) {
            // Skip whitespace tokens before checking for end condition
            this.skipWhitespace();

            // Check for :ENDPROC
            if (this.check(TokenType.COLON) && this.checkNext(TokenType.ENDPROC)) {
                break;
            }

            const stmt = this.parseStatement();
            if (stmt) {
                body.push(stmt);
            }
        }

        // Consume :ENDPROC
        this.consume(TokenType.COLON, "Expected ':' before ENDPROC");
        this.consume(TokenType.ENDPROC, "Expected 'ENDPROC'");
        const endToken = this.previous();

        // Restore the previous procedure context
        this.isInsideProcedure = wasInsideProcedure;

        return {
            kind: ASTNodeType.ProcedureStatement,
            startToken,
            endToken,
            name,
            parameters,
            defaultParameters,
            body,
        };
    }
    /**
     * Parse procedure statement (starting from :PROCEDURE)
     */
    private parseProcedureStatement(): ProcedureStatementNode {
        // Check for nested procedures
        if (this.isInsideProcedure) {
            this.error("Nested procedures are not allowed in SSL");
            throw new Error("Nested procedures are not allowed in SSL");
        }

        // Note: COLON and PROCEDURE tokens are already consumed by parseColonStatement
        return this.parseProcedureStatementBody();
    }

    /**
     * Parse if statement
     */
    private parseIfStatement(): IfStatementNode {
        const startToken = this.previous();
        const condition = this.parseExpression();

        const thenBranch: StatementNode[] = [];
        let elseBranch: StatementNode[] | undefined = undefined; // Parse statements until ELSE or ENDIF
        while (!this.isAtEnd()) {
            // Skip whitespace tokens before checking for end condition
            this.skipWhitespace();

            // Check for :ELSE or :ENDIF
            if (
                this.check(TokenType.COLON) &&
                (this.checkNext(TokenType.ELSE) || this.checkNext(TokenType.ENDIF))
            ) {
                break;
            }

            const stmt = this.parseStatement();
            if (stmt) {
                thenBranch.push(stmt);
            }
        } // Optional ELSE branch
        if (this.check(TokenType.COLON) && this.checkNext(TokenType.ELSE)) {
            this.advance(); // consume ':'
            this.advance(); // consume 'ELSE'

            elseBranch = [];
            while (!this.isAtEnd()) {
                // Skip whitespace tokens before checking for end condition
                this.skipWhitespace();

                // Check for :ENDIF
                if (this.check(TokenType.COLON) && this.checkNext(TokenType.ENDIF)) {
                    break;
                }

                const stmt = this.parseStatement();
                if (stmt) {
                    elseBranch.push(stmt);
                }
            }
        }

        // Consume :ENDIF
        this.consume(TokenType.COLON, "Expected ':' before ENDIF");
        this.consume(TokenType.ENDIF, "Expected 'ENDIF'");
        const endToken = this.previous();

        return {
            kind: ASTNodeType.IfStatement,
            startToken,
            endToken,
            condition,
            thenBranch,
            elseBranch,
        };
    }
    /**
     * Parse while statement
     */
    private parseWhileStatement(): WhileLoopNode {
        const startToken = this.previous();
        const condition = this.parseExpression();

        const body: StatementNode[] = [];
        while (
            !this.isAtEnd() &&
            !(this.check(TokenType.COLON) && this.checkNext(TokenType.ENDWHILE))
        ) {
            this.skipWhitespace();
            if (this.check(TokenType.COLON) && this.checkNext(TokenType.ENDWHILE)) {
                break;
            }
            const stmt = this.parseStatement();
            if (stmt) {
                body.push(stmt);
            }
        }

        this.consume(TokenType.COLON, "Expected ':' before ENDWHILE");
        this.consume(TokenType.ENDWHILE, "Expected 'ENDWHILE'");
        const endToken = this.previous();

        return {
            kind: ASTNodeType.WhileLoop,
            startToken,
            endToken,
            condition,
            body,
        };
    }

    /**
     * Parse for statement
     */
    private parseForStatement(): ForLoopNode {
        const startToken = this.previous();
        const variable = this.consume(TokenType.IDENTIFIER, "Expected loop variable");
        this.consume(TokenType.ASSIGN, "Expected ':=' in for loop");
        const from = this.parseExpression();
        this.consume(TokenType.COLON, "Expected ':' before TO");
        this.consume(TokenType.TO, "Expected 'TO' in for loop");
        const to = this.parseExpression();
        const body: StatementNode[] = [];
        while (
            !this.isAtEnd() &&
            !(this.check(TokenType.COLON) && this.checkNext(TokenType.NEXT))
        ) {
            this.skipWhitespace();
            if (this.check(TokenType.COLON) && this.checkNext(TokenType.NEXT)) {
                break;
            }
            const stmt = this.parseStatement();
            if (stmt) {
                body.push(stmt);
            }
        }

        this.consume(TokenType.COLON, "Expected ':' before NEXT");
        this.consume(TokenType.NEXT, "Expected 'NEXT'");
        const endToken = this.previous();

        return {
            kind: ASTNodeType.ForLoop,
            startToken,
            endToken,
            variable,
            from,
            to,
            body,
        };
    }

    /**
     * Parse assignment or function call
     */ private parseAssignmentOrFunctionCall(): StatementNode {
        const startToken = this.peek();
        const expr = this.parseExpression();

        // Check if this is an assignment
        if (this.matchAssignmentOperator()) {
            const operator = this.previous();
            const right = this.parseExpression();

            // Ensure left side is assignable
            if (
                expr.kind !== ASTNodeType.VariableAccess &&
                expr.kind !== ASTNodeType.PropertyAccess
            ) {
                this.error("Invalid assignment target");
            }

            this.consume(TokenType.SEMICOLON, "Expected ';' after assignment");
            return {
                kind: ASTNodeType.Assignment,
                startToken,
                endToken: this.previous(),
                left: expr as any,
                operator,
                right,
            } as AssignmentNode;
        }

        // Check if this is a comparison expression using = at statement level
        // This should be rejected as = is only for comparisons, not assignments
        if (expr.kind === ASTNodeType.BinaryExpression) {
            const binaryExpr = expr as BinaryExpressionNode;
            if (binaryExpr.operator.type === TokenType.EQUAL) {
                this.error("Invalid assignment operator. Use ':=' for assignment, not '='");
            }
        }

        // Otherwise it's a function call expression statement
        this.consume(TokenType.SEMICOLON, "Expected ';' after expression");
        return expr;
    }

    /**
     * Parse expressions with proper precedence
     */
    private parseExpression(): ExpressionNode {
        return this.parseLogicalExpression();
    }

    /**
     * Parse logical expressions (.AND., .OR.)
     */
    private parseLogicalExpression(): ExpressionNode {
        let expr = this.parseComparisonExpression();
        while (this.match(TokenType.AND, TokenType.OR)) {
            const operator = this.previous();
            const right = this.parseComparisonExpression();
            expr = {
                kind: ASTNodeType.BinaryExpression,
                startToken: expr.startToken,
                endToken: right.endToken,
                left: expr,
                operator,
                right,
            } as BinaryExpressionNode;
        }

        return expr;
    }

    /**
     * Parse comparison expressions (==, !=, <, >, <=, >=, =)
     */
    private parseComparisonExpression(): ExpressionNode {
        let expr = this.parseArithmeticExpression();
        while (
            this.match(
                TokenType.STRICT_EQUAL,
                TokenType.NOT_EQUAL,
                TokenType.LESS_THAN,
                TokenType.GREATER_THAN,
                TokenType.LESS_EQUAL,
                TokenType.GREATER_EQUAL,
                TokenType.EQUAL
            )
        ) {
            const operator = this.previous();
            const right = this.parseArithmeticExpression();
            expr = {
                kind: ASTNodeType.BinaryExpression,
                startToken: expr.startToken,
                endToken: right.endToken,
                left: expr,
                operator,
                right,
            } as BinaryExpressionNode;
        }

        return expr;
    }

    /**
     * Parse arithmetic expressions (+, -)
     */
    private parseArithmeticExpression(): ExpressionNode {
        let expr = this.parseTerm();
        while (this.match(TokenType.PLUS, TokenType.MINUS)) {
            const operator = this.previous();
            const right = this.parseTerm();
            expr = {
                kind: ASTNodeType.BinaryExpression,
                startToken: expr.startToken,
                endToken: right.endToken,
                left: expr,
                operator,
                right,
            } as BinaryExpressionNode;
        }

        return expr;
    }

    /**
     * Parse terms (*, /, %)
     */
    private parseTerm(): ExpressionNode {
        let expr = this.parseFactor();
        while (this.match(TokenType.MULTIPLY, TokenType.DIVIDE, TokenType.MODULO)) {
            const operator = this.previous();
            const right = this.parseFactor();
            expr = {
                kind: ASTNodeType.BinaryExpression,
                startToken: expr.startToken,
                endToken: right.endToken,
                left: expr,
                operator,
                right,
            } as BinaryExpressionNode;
        }

        return expr;
    }

    /**
     * Parse factors (^)
     */
    private parseFactor(): ExpressionNode {
        let expr = this.parseUnary();
        while (this.match(TokenType.POWER)) {
            const operator = this.previous();
            const right = this.parseUnary();
            expr = {
                kind: ASTNodeType.BinaryExpression,
                startToken: expr.startToken,
                endToken: right.endToken,
                left: expr,
                operator,
                right,
            } as BinaryExpressionNode;
        }

        return expr;
    }

    /**
     * Parse unary expressions (!, .NOT., +, -)
     */
    private parseUnary(): ExpressionNode {
        if (this.match(TokenType.LOGICAL_NOT, TokenType.NOT, TokenType.PLUS, TokenType.MINUS)) {
            const operator = this.previous();
            const operand = this.parseUnary();
            return {
                kind: ASTNodeType.UnaryExpression,
                startToken: operator,
                endToken: operand.endToken,
                operator,
                operand,
            } as UnaryExpressionNode;
        }

        return this.parsePrimary();
    }

    /**
     * Parse primary expressions (literals, identifiers, function calls, etc.)
     */
    private parsePrimary(): ExpressionNode {
        // Literals
        if (this.match(TokenType.NUMBER, TokenType.STRING, TokenType.BOOLEAN, TokenType.NIL)) {
            const token = this.previous();
            return {
                kind: ASTNodeType.LiteralExpression,
                startToken: token,
                endToken: token,
                value: token.parsedValue || token.value,
                token,
            } as LiteralExpressionNode;
        } // Array literals
        if (this.match(TokenType.LBRACE)) {
            return this.parseArrayLiteral();
        }

        // Parenthesized expressions
        if (this.match(TokenType.LPAREN)) {
            const expr = this.parseExpression();
            this.consume(TokenType.RPAREN, "Expected ')' after expression");
            return expr;
        } // Identifiers (variables, function calls, property access)
        if (this.match(TokenType.IDENTIFIER)) {
            const name = this.previous();

            // Function call
            if (this.match(TokenType.LPAREN)) {
                const args = this.parseArgumentList();
                this.consume(TokenType.RPAREN, "Expected ')' after arguments");
                return {
                    kind: ASTNodeType.DirectFunctionCall,
                    startToken: name,
                    endToken: this.previous(),
                    name,
                    arguments: args,
                } as DirectFunctionCallNode;
            }

            // Property access (Object:Property)
            if (this.match(TokenType.COLON)) {
                const property = this.consume(
                    TokenType.IDENTIFIER,
                    "Expected property name after ':'"
                );
                return {
                    kind: ASTNodeType.PropertyAccess,
                    startToken: name,
                    endToken: property,
                    object: name,
                    property,
                } as PropertyAccessNode;
            }

            // Array access
            if (this.match(TokenType.LBRACKET)) {
                const indices: ExpressionNode[] = [];
                do {
                    indices.push(this.parseExpression());
                } while (this.match(TokenType.COMMA));

                this.consume(TokenType.RBRACKET, "Expected ']' after array index");

                return {
                    kind: ASTNodeType.ArrayAccess,
                    startToken: name,
                    endToken: this.previous(),
                    array: name,
                    indices,
                } as ArrayAccessNode;
            } // Simple variable access
            // Check for invalid identifiers that violate EBNF grammar
            if (name.value === "TRUE" || name.value === "FALSE") {
                this.error(
                    `Invalid boolean literal '${name.value}'. Use '.T.' or '.F.' instead according to SSL grammar.`
                );
                return this.createErrorNode();
            }

            return {
                kind: ASTNodeType.VariableAccess,
                startToken: name,
                endToken: name,
                name,
            } as VariableAccessNode;
        }

        this.error("Unexpected token in expression");
        return this.createErrorNode();
    }

    // Helper methods and utility functions continue...
    // [Additional parsing methods would be implemented here]

    /**
     * Helper method to create error recovery node
     */
    private createErrorNode(): ExpressionNode {
        const token = this.peek();
        return {
            kind: ASTNodeType.LiteralExpression,
            startToken: token,
            endToken: token,
            value: null,
            token,
        } as LiteralExpressionNode;
    }
    /**
     * Parse array literal
     */
    private parseArrayLiteral(): ExpressionNode {
        const startToken = this.previous(); // '{'
        const elements: ExpressionNode[] = [];

        if (!this.check(TokenType.RBRACE)) {
            do {
                elements.push(this.parseExpression());
            } while (this.match(TokenType.COMMA));
        }

        this.consume(TokenType.RBRACE, "Expected '}' after array elements");
        const endToken = this.previous();

        return {
            kind: ASTNodeType.ArrayLiteral,
            startToken,
            endToken,
            elements,
        } as any;
    }

    /**
     * Parse argument list
     */
    private parseArgumentList(): ArgumentListNode {
        const startToken = this.peek();
        const args: ExpressionNode[] = [];

        if (!this.check(TokenType.RPAREN)) {
            do {
                args.push(this.parseExpression());
            } while (this.match(TokenType.COMMA));
        }

        const endToken = this.previous();
        return {
            kind: ASTNodeType.ArgumentList,
            startToken,
            endToken,
            arguments: args,
        } as ArgumentListNode;
    }

    /**
     * Parse identifier list
     */
    private parseIdentifierList(): IdentifierListNode {
        const startToken = this.peek();
        const identifiers: Token[] = [];

        identifiers.push(this.consume(TokenType.IDENTIFIER, "Expected identifier"));

        while (this.match(TokenType.COMMA)) {
            identifiers.push(this.consume(TokenType.IDENTIFIER, "Expected identifier after ','"));
        }

        const endToken = this.previous();
        return {
            kind: ASTNodeType.IdentifierList,
            startToken,
            endToken,
            identifiers,
        };
    }

    // Stub implementations for other statement types
    private parseSwitchStatement(): StatementNode {
        // TODO: Implement switch statement parsing
        return this.createErrorNode();
    }

    private parseTryStatement(): StatementNode {
        // TODO: Implement try statement parsing
        return this.createErrorNode();
    }

    private parseDeclareStatement(): StatementNode {
        // TODO: Implement declare statement parsing
        return this.createErrorNode();
    }
    private parseParametersStatement(): ParametersStatementNode {
        // Parse according to EBNF: ParametersStatement ::= ":" "PARAMETERS" IdentifierList
        const startToken = this.previous(); // Should be "PARAMETERS" token
        const identifiers = this.parseIdentifierList();

        return {
            kind: ASTNodeType.ParametersStatement,
            startToken,
            endToken: this.previous(),
            identifiers,
        };
    }

    private parsePublicStatement(): StatementNode {
        // TODO: Implement public statement parsing
        return this.createErrorNode();
    }

    private parseIncludeStatement(): StatementNode {
        // TODO: Implement include statement parsing
        return this.createErrorNode();
    }

    private parseReturnStatement(): StatementNode {
        // TODO: Implement return statement parsing
        return this.createErrorNode();
    }

    private parseLabelStatement(): StatementNode {
        // TODO: Implement label statement parsing
        return this.createErrorNode();
    }

    private parseRegionStatement(): StatementNode {
        // TODO: Implement region statement parsing
        return this.createErrorNode();
    }

    private parseExitWhileStatement(): StatementNode {
        // TODO: Implement exit while statement parsing
        return this.createErrorNode();
    }

    private parseExitForStatement(): StatementNode {
        // TODO: Implement exit for statement parsing
        return this.createErrorNode();
    }

    private parseLoopContinueStatement(): StatementNode {
        // TODO: Implement loop continue statement parsing
        return this.createErrorNode();
    } // Utility methods
    private skipWhitespace(): void {
        while (!this.isAtEnd() && this.check(TokenType.NEWLINE)) {
            this.advance();
        }
    }

    private match(...types: TokenType[]): boolean {
        for (const type of types) {
            if (this.check(type)) {
                this.advance();
                return true;
            }
        }
        return false;
    }

    private matchAssignmentOperator(): boolean {
        return this.match(
            TokenType.ASSIGN,
            TokenType.PLUS_ASSIGN,
            TokenType.MINUS_ASSIGN,
            TokenType.MULT_ASSIGN,
            TokenType.DIV_ASSIGN,
            TokenType.POWER_ASSIGN
        );
    }
    private check(type: TokenType): boolean {
        if (this.isAtEnd()) {
            return false;
        }
        return this.peek().type === type;
    }

    private checkNext(type: TokenType): boolean {
        if (this.current + 1 >= this.tokens.length) {
            return false;
        }
        return this.tokens[this.current + 1].type === type;
    }

    private advance(): Token {
        if (!this.isAtEnd()) {
            this.current++;
        }
        return this.previous();
    }

    private isAtEnd(): boolean {
        return this.peek().type === TokenType.EOF;
    }

    private peek(): Token {
        return this.tokens[this.current];
    }

    private previous(): Token {
        return this.tokens[this.current - 1];
    }
    private consume(type: TokenType, message: string): Token {
        if (this.check(type)) {
            return this.advance();
        }

        this.error(message);
        throw new Error(message);
    }

    private error(message: string): void {
        const token = this.peek();
        this.errors.push({
            message,
            token,
            line: token.range.start.line,
            column: token.range.start.column,
            severity: "error",
        });
    }

    /**
     * Error recovery: advance to next synchronization point
     */
    private synchronize(): void {
        this.advance();
        while (!this.isAtEnd()) {
            if (this.previous().type === TokenType.SEMICOLON) {
                return;
            }

            switch (this.peek().type) {
                case TokenType.CLASS:
                case TokenType.PROCEDURE:
                case TokenType.IF:
                case TokenType.FOR:
                case TokenType.WHILE:
                case TokenType.RETURN:
                case TokenType.DECLARE:
                    return;
            }

            this.advance();
        }
    }
}
