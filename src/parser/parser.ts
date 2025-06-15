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
    AssignmentNode,
    BinaryExpressionNode,
    createBaseNode,
} from "./ast";

// Import specialized parsers
import {
    parseProcedureStatement,
    parseIfStatement,
    parseWhileStatement,
    parseForStatement,
    parseSwitchStatement,
    parseTryStatement,
    parseDeclareStatement,
    parseParametersStatement,
    parsePublicStatement,
    parseIncludeStatement,
    parseReturnStatement,
    parseLabelStatement,
    parseRegionStatement,
    parseInlineCodeBlock,
    parseExitWhileStatement,
    parseExitForStatement,
    parseLoopContinueStatement,
    parseExitCaseStatement,
    parseSqlExecute,
    parseLSearch,
    ProcedureParser,
    ControlFlowParser,
    CaseParser,
    TryStatementParser,
    StatementParser,
    SqlStatementParser,
} from "./parsers";

// Import new modular parsers
import {
    parseExpression,
    parseArithmeticExpression,
    ExpressionParser,
    ExpressionParserContext,
} from "./parsers/expressions";

import { parseClassDefinition, ClassParser, ClassParserContext } from "./parsers/classes";

import {
    parseDefaultStatement,
    parseErrorBlockStanza,
    parseInlineCodeStart,
    parseInlineCodeEnd,
} from "./parsers/statements";

import {
    ParseError,
    ParserUtilities,
    UtilityParserContext,
    skipWhitespace,
    match,
    check,
    checkNext,
    advance,
    isAtEnd,
    peek,
    previous,
    consume,
    error,
    createErrorNode,
    parseArgumentList,
    parseIdentifierList,
    matchAssignmentOperator,
    synchronize,
} from "./parsers/utilities";

// Re-export ParseError for external use
export { ParseError } from "./parsers/utilities";

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
    implements
        ProcedureParser,
        ControlFlowParser,
        CaseParser,
        TryStatementParser,
        StatementParser,
        SqlStatementParser,
        ExpressionParser,
        ClassParser,
        ParserUtilities,
        ExpressionParserContext,
        ClassParserContext,
        UtilityParserContext
{
    public tokens: Token[];
    public current: number = 0;
    public errors: ParseError[] = [];
    public panicMode: boolean = false;
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
     * Parse a statement
     */
    public parseStatement(): StatementNode | null {
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
                return parseProcedureStatement(this);
            case TokenType.IF:
                this.advance(); // consume 'IF'
                return parseIfStatement(this);
            case TokenType.WHILE:
                this.advance(); // consume 'WHILE'
                return parseWhileStatement(this);
            case TokenType.FOR:
                this.advance(); // consume 'FOR'
                return parseForStatement(this);
            case TokenType.BEGINCASE:
                this.advance(); // consume 'BEGINCASE'
                return parseSwitchStatement(this);
            case TokenType.TRY:
                this.advance(); // consume 'TRY'
                return parseTryStatement(this);
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
            case TokenType.EXITCASE:
                this.advance(); // consume 'EXITCASE'
                return parseExitCaseStatement(this);
            case TokenType.DEFAULT:
                this.advance(); // consume 'DEFAULT'
                return parseDefaultStatement(this);
            case TokenType.ERROR:
                this.advance(); // consume 'ERROR'
                return parseErrorBlockStanza(this);
            case TokenType.BEGININLINECODE:
                this.advance(); // consume 'BEGININLINECODE'
                return parseInlineCodeStart(this);
            case TokenType.ENDINLINECODE:
                this.advance(); // consume 'ENDINLINECODE'
                return parseInlineCodeEnd(this);

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
     */
    private parseAssignmentOrFunctionCall(): StatementNode {
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
        if (
            expr.kind === ASTNodeType.BinaryExpression ||
            expr.kind === ASTNodeType.ComparisonExpression
        ) {
            const binaryExpr = expr as BinaryExpressionNode;
            if (binaryExpr.operator.type === TokenType.EQUAL) {
                this.error("Invalid assignment operator. Use ':=' for assignment, not '='");
                return expr; // Return the expression as-is but with error recorded
            }
        }

        // Otherwise it's a function call expression statement
        this.consume(TokenType.SEMICOLON, "Expected ';' after expression");
        return expr;
    }

    // Delegate expression parsing to the expressions module
    public parseExpression(): ExpressionNode {
        return parseExpression(this);
    }

    public parseArithmeticExpression(): ExpressionNode {
        return parseArithmeticExpression(this);
    }

    // Delegate class parsing to the classes module
    public parseClassDefinition(): ClassDefinitionNode {
        return parseClassDefinition(this);
    }

    // Delegate utility methods to the utilities module
    public skipWhitespace(): void {
        return skipWhitespace(this);
    }

    public match(...types: TokenType[]): boolean {
        return match(this, ...types);
    }

    public check(type: TokenType): boolean {
        return check(this, type);
    }

    public checkNext(type: TokenType): boolean {
        return checkNext(this, type);
    }

    public advance(): Token {
        return advance(this);
    }

    public isAtEnd(): boolean {
        return isAtEnd(this);
    }

    public peek(): Token {
        return peek(this);
    }

    public previous(): Token {
        return previous(this);
    }

    public consume(type: TokenType, message: string): Token {
        return consume(this, type, message);
    }

    public error(message: string): void {
        return error(this, message);
    }

    public createErrorNode(): ExpressionNode {
        return createErrorNode(this);
    }

    public parseArgumentList() {
        return parseArgumentList(this);
    }

    public parseIdentifierList() {
        return parseIdentifierList(this);
    }

    private matchAssignmentOperator(): boolean {
        return matchAssignmentOperator(this);
    }

    public synchronize(): void {
        return synchronize(this);
    }
}
