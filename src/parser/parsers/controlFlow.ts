/**
 * Control flow parsing functions for SSL parser (IF, WHILE, FOR)
 */

import { Token } from "../../tokenizer/token";
import { TokenType } from "../../tokenizer/tokenType";
import {
    StatementNode,
    ExpressionNode,
    IfStatementNode,
    WhileLoopNode,
    ForLoopNode,
    ASTNodeType,
} from "../ast";

/**
 * Parser interface for control flow parsers
 */
export interface ControlFlowParser {
    match(...types: TokenType[]): boolean;
    advance(): Token;
    previous(): Token;
    peek(): Token;
    check(type: TokenType): boolean;
    checkNext(type: TokenType): boolean;
    consume(type: TokenType, message: string): Token;
    parseStatement(): StatementNode | null;
    parseExpression(): ExpressionNode;
    skipWhitespace(): void;
    error(message: string): void;
    isAtEnd(): boolean;
}

/**
 * Parse if statement
 */
export function parseIfStatement(parser: ControlFlowParser): IfStatementNode {
    const startToken = parser.previous();
    const condition = parser.parseExpression();

    const thenBranch: StatementNode[] = [];
    let elseBranch: StatementNode[] | undefined = undefined;

    // Parse statements until ELSE or ENDIF
    while (!parser.isAtEnd()) {
        // Skip whitespace tokens before checking for end condition
        parser.skipWhitespace();

        // Check for :ELSE or :ENDIF
        if (
            parser.check(TokenType.COLON) &&
            (parser.checkNext(TokenType.ELSE) || parser.checkNext(TokenType.ENDIF))
        ) {
            break;
        }

        const stmt = parser.parseStatement();
        if (stmt) {
            thenBranch.push(stmt);
        }
    }

    // Optional ELSE branch
    if (parser.check(TokenType.COLON) && parser.checkNext(TokenType.ELSE)) {
        parser.advance();
        parser.advance();
        elseBranch = [];
        while (!parser.isAtEnd()) {
            // Skip whitespace tokens before checking for end condition
            parser.skipWhitespace();

            // Check for :ENDIF
            if (parser.check(TokenType.COLON) && parser.checkNext(TokenType.ENDIF)) {
                break;
            }

            const stmt = parser.parseStatement();
            if (stmt) {
                elseBranch.push(stmt);
            }
        }
    }

    // Consume :ENDIF
    parser.consume(TokenType.COLON, "Expected ':' before ENDIF");
    parser.consume(TokenType.ENDIF, "Expected 'ENDIF'");
    const endToken = parser.previous();

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
export function parseWhileStatement(parser: ControlFlowParser): WhileLoopNode {
    const startToken = parser.previous();
    const condition = parser.parseExpression();

    const body: StatementNode[] = [];
    while (
        !parser.isAtEnd() &&
        !(parser.check(TokenType.COLON) && parser.checkNext(TokenType.ENDWHILE))
    ) {
        parser.skipWhitespace();
        if (parser.check(TokenType.COLON) && parser.checkNext(TokenType.ENDWHILE)) {
            break;
        }
        const stmt = parser.parseStatement();
        if (stmt) {
            body.push(stmt);
        }
    }

    parser.consume(TokenType.COLON, "Expected ':' before ENDWHILE");
    parser.consume(TokenType.ENDWHILE, "Expected 'ENDWHILE'");
    const endToken = parser.previous();

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
export function parseForStatement(parser: ControlFlowParser): ForLoopNode {
    const startToken = parser.previous();
    const variable = parser.consume(TokenType.IDENTIFIER, "Expected loop variable");
    parser.consume(TokenType.ASSIGN, "Expected ':=' in for loop");
    const from = parser.parseExpression();
    parser.consume(TokenType.COLON, "Expected ':' before TO");
    parser.consume(TokenType.TO, "Expected 'TO' in for loop");
    const to = parser.parseExpression();

    const body: StatementNode[] = [];
    while (
        !parser.isAtEnd() &&
        !(parser.check(TokenType.COLON) && parser.checkNext(TokenType.NEXT))
    ) {
        parser.skipWhitespace();
        if (parser.check(TokenType.COLON) && parser.checkNext(TokenType.NEXT)) {
            break;
        }
        const stmt = parser.parseStatement();
        if (stmt) {
            body.push(stmt);
        }
    }

    parser.consume(TokenType.COLON, "Expected ':' before NEXT");
    parser.consume(TokenType.NEXT, "Expected 'NEXT'");
    const endToken = parser.previous();

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
