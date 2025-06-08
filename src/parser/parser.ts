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

// Import specialized parsers
import {
    parseProcedureStatement as parseSpecializedProcedure,
    parseIfStatement as parseSpecializedIf,
    parseWhileStatement as parseSpecializedWhile,
    parseForStatement as parseSpecializedFor,
    parseSwitchStatement as parseSpecializedSwitch,
    parseTryStatement as parseSpecializedTry,
    parseDeclareStatement,
    parseParametersStatement,
    parsePublicStatement,
    parseIncludeStatement,
    parseReturnStatement,
    parseLabelStatement,
    parseRegionStatement,
    parseExitWhileStatement,
    parseExitForStatement,
    parseLoopContinueStatement,
    ProcedureParser,
    ControlFlowParser,
    CaseParser,
    TryStatementParser,
    StatementParser,
} from "./parsers";

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
export class Parser
    implements ProcedureParser, ControlFlowParser, CaseParser, TryStatementParser, StatementParser
{
    private tokens: Token[];
    private current: number = 0;
    private errors: ParseError[] = [];
    private panicMode: boolean = false;
    public isInsideProcedure: boolean = false;

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
                this.advance(); // consume ':'
                this.advance(); // consume 'PROCEDURE'
                const procedure = parseSpecializedProcedure(this);
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
     */ public parseStatement(): StatementNode | null {
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
     * Parse a statement that begins with a colon
     * This function is a dispatcher that calls the appropriate specialized parser
     */
    private parseColonStatement(): StatementNode | null {
        this.advance(); // consume ':'
        const keywordToken = this.peek();

        switch (keywordToken.type) {
            case TokenType.PROCEDURE:
                this.advance(); // consume 'PROCEDURE'
                return parseSpecializedProcedure(this);
            case TokenType.IF:
                this.advance(); // consume 'IF'
                return parseSpecializedIf(this);
            case TokenType.WHILE:
                this.advance(); // consume 'WHILE'
                return parseSpecializedWhile(this);
            case TokenType.FOR:
                this.advance(); // consume 'FOR'
                return parseSpecializedFor(this);
            case TokenType.BEGINCASE:
                this.advance(); // consume 'BEGINCASE'
                return parseSpecializedSwitch(this);
            case TokenType.TRY:
                this.advance(); // consume 'TRY'
                return parseSpecializedTry(this);
            case TokenType.DECLARE:
                this.advance(); // consume 'DECLARE'
                return parseDeclareStatement(this);
            case TokenType.PARAMETERS:
                this.advance(); // consume 'PARAMETERS'
                return parseParametersStatement(this);
            case TokenType.PUBLIC:
                this.advance(); // consume 'PUBLIC'
                return parsePublicStatement(this);
            case TokenType.INCLUDE:
                this.advance(); // consume 'INCLUDE'
                return parseIncludeStatement(this);
            case TokenType.RETURN:
                this.advance(); // consume 'RETURN'
                return parseReturnStatement(this);
            case TokenType.LABEL:
                this.advance(); // consume 'LABEL'
                return parseLabelStatement(this);
            case TokenType.REGION:
                 this.advance(); // consume 'REGION'
                return parseRegionStatement(this);
            case TokenType.EXITWHILE:
                this.advance(); // consume 'EXITWHILE'
                return parseExitWhileStatement(this);
            case TokenType.EXITFOR:
                this.advance(); // consume 'EXITFOR'
                return parseExitForStatement(this);
            case TokenType.LOOP:
                this.advance(); // consume 'LOOP'
                return parseLoopContinueStatement(this);

            // Keywords that do not start a new statement but are handled elsewhere
            case TokenType.ELSE:
            case TokenType.ENDIF:
            case TokenType.ENDWHILE:
            case TokenType.NEXT:
            case TokenType.CASE:
            case TokenType.OTHERWISE:
            case TokenType.ENDCASE:
            case TokenType.CATCH:
            case TokenType.FINALLY:
            case TokenType.ENDTRY:
            case TokenType.ENDPROC:
            case TokenType.ENDREGION:
                this.error(`Unexpected keyword '${keywordToken.value}' found.`);
                return null;

            default:
                this.error(`Unknown statement starting with ':${keywordToken.value}'`);
                return null;
        }
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
    public parseExpression(): ExpressionNode {
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
    public createErrorNode(): ExpressionNode {
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
    public parseIdentifierList(): IdentifierListNode {
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

    // Utility methods
    public skipWhitespace(): void {
        while (!this.isAtEnd() && this.check(TokenType.NEWLINE)) {
            this.advance();
        }
    }

    public match(...types: TokenType[]): boolean {
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
    public check(type: TokenType): boolean {
        if (this.isAtEnd()) {
            return false;
        }
        return this.peek().type === type;
    }

    public checkNext(type: TokenType): boolean {
        if (this.current + 1 >= this.tokens.length) {
            return false;
        }
        return this.tokens[this.current + 1].type === type;
    }

    public advance(): Token {
        if (!this.isAtEnd()) {
            this.current++;
        }
        return this.previous();
    }

    public isAtEnd(): boolean {
        return this.peek().type === TokenType.EOF;
    }

    public peek(): Token {
        return this.tokens[this.current];
    }

    public previous(): Token {
        return this.tokens[this.current - 1];
    }
    public consume(type: TokenType, message: string): Token {
        if (this.check(type)) {
            return this.advance();
        }

        this.error(message);
        throw new Error(message);
    }

    public error(message: string): void {
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
