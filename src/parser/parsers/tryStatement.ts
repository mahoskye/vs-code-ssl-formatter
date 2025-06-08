/**
 * Try-catch-finally statement parsing functions for SSL parser
 */

import { Token } from "../../tokenizer/token";
import { TokenType } from "../../tokenizer/tokenType";
import {
    StatementNode,
    TryBlockNode,
    CatchBlockNode,
    FinallyBlockNode,
    ASTNodeType,
    IdentifierListNode,
    ExpressionNode,
} from "../ast";

/**
 * Parser interface for try statement parsers
 */
export interface TryStatementParser {
    match(...types: TokenType[]): boolean;
    advance(): Token;
    previous(): Token;
    peek(): Token;
    check(type: TokenType): boolean;
    checkNext(type: TokenType): boolean;
    consume(type: TokenType, message: string): Token;
    parseStatement(): StatementNode | null;
    parseExpression(): ExpressionNode;
    parseIdentifierList(): IdentifierListNode;
    skipWhitespace(): void;
    error(message: string): void;
    isAtEnd(): boolean;
}

/**
 * Parse try statement
 */
export function parseTryStatement(parser: TryStatementParser): TryBlockNode {
    const startToken = parser.previous(); // TRY token
    const tryBody: StatementNode[] = [];
    let catchBlock: CatchBlockNode | undefined = undefined;
    let finallyBlock: FinallyBlockNode | undefined = undefined;

    // Parse try body
    while (!parser.isAtEnd()) {
        parser.skipWhitespace();

        // Check for :CATCH, :FINALLY, or :ENDTRY
        if (parser.check(TokenType.COLON)) {
            if (
                parser.checkNext(TokenType.CATCH) ||
                parser.checkNext(TokenType.FINALLY) ||
                parser.checkNext(TokenType.ENDTRY)
            ) {
                break;
            }
        }

        const stmt = parser.parseStatement();
        if (stmt) {
            tryBody.push(stmt);
        }
    }

    // Optional CATCH block
    if (parser.check(TokenType.COLON) && parser.checkNext(TokenType.CATCH)) {
        parser.advance(); // consume :
        parser.advance(); // consume CATCH

        const catchStartToken = parser.previous();

        // Optional exception variable declaration
        let exceptionVariable: Token | undefined = undefined;
        if (parser.check(TokenType.IDENTIFIER)) {
            exceptionVariable = parser.advance();
        }

        const catchBody: StatementNode[] = [];

        // Parse catch body
        while (!parser.isAtEnd()) {
            parser.skipWhitespace();

            // Check for :FINALLY or :ENDTRY
            if (parser.check(TokenType.COLON)) {
                if (parser.checkNext(TokenType.FINALLY) || parser.checkNext(TokenType.ENDTRY)) {
                    break;
                }
            }

            const stmt = parser.parseStatement();
            if (stmt) {
                catchBody.push(stmt);
            }
        }

        const catchEndToken = parser.previous();
        catchBlock = {
            kind: ASTNodeType.CatchBlock,
            startToken: catchStartToken,
            endToken: catchEndToken,
            statements: catchBody,
        } as CatchBlockNode;
    }

    // Optional FINALLY block
    if (parser.check(TokenType.COLON) && parser.checkNext(TokenType.FINALLY)) {
        parser.advance(); // consume :
        parser.advance(); // consume FINALLY

        const finallyStartToken = parser.previous();
        const finallyBody: StatementNode[] = [];

        // Parse finally body
        while (!parser.isAtEnd()) {
            parser.skipWhitespace();

            // Check for :ENDTRY
            if (parser.check(TokenType.COLON) && parser.checkNext(TokenType.ENDTRY)) {
                break;
            }

            const stmt = parser.parseStatement();
            if (stmt) {
                finallyBody.push(stmt);
            }
        }

        const finallyEndToken = parser.previous();
        finallyBlock = {
            kind: ASTNodeType.FinallyBlock,
            startToken: finallyStartToken,
            endToken: finallyEndToken,
            statements: finallyBody,
        } as FinallyBlockNode;
    }

    // Consume :ENDTRY
    parser.consume(TokenType.COLON, "Expected ':' before ENDTRY");
    parser.consume(TokenType.ENDTRY, "Expected 'ENDTRY'");
    const endToken = parser.previous();
    return {
        kind: ASTNodeType.TryBlock,
        startToken,
        endToken,
        tryStatements: tryBody,
        catchBlock,
        finallyBlock,
    } as TryBlockNode;
}
