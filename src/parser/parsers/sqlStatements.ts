/**
 * SQL statement parsing functions for SSL parser
 * Handles SqlExecute, LSearch, and SQL parameter parsing
 */

import { Token } from "../../tokenizer/token";
import { TokenType } from "../../tokenizer/tokenType";
import {
    ExpressionNode,
    ASTNodeType,
    SqlExecuteNode,
    LSearchNode,
    StringLiteralNode,
    ArrayLiteralNode,
} from "../ast";

/**
 * Parser interface for SQL statement parsers
 */
export interface SqlStatementParser {
    match(...types: TokenType[]): boolean;
    advance(): Token;
    previous(): Token;
    peek(): Token;
    check(type: TokenType): boolean;
    consume(type: TokenType, message: string): Token;
    parseExpression(): ExpressionNode;
    error(message: string): void;
    createErrorNode(): ExpressionNode;
}

/**
 * Parse SqlExecute function call
 * SqlExecute ::= "SqlExecute" "(" StringLiteral ["," ArrayLiteral] ")"
 */
export function parseSqlExecute(parser: SqlStatementParser): SqlExecuteNode {
    const startToken = parser.previous(); // SqlExecute token
    parser.consume(TokenType.LPAREN, "Expected '(' after SqlExecute");

    // Parse query string
    const queryToken = parser.consume(TokenType.STRING, "Expected SQL query string");
    const query: StringLiteralNode = {
        kind: ASTNodeType.StringLiteral,
        startToken: queryToken,
        endToken: queryToken,
        value: queryToken.parsedValue || queryToken.value,
        token: queryToken,
    };

    let parameters: ArrayLiteralNode | undefined = undefined;

    // Optional parameters array
    if (parser.match(TokenType.COMMA)) {
        const paramExpr = parser.parseExpression();
        if (paramExpr.kind === ASTNodeType.ArrayLiteral) {
            parameters = paramExpr as ArrayLiteralNode;
        } else {
            parser.error("Expected array literal for SqlExecute parameters");
        }
    }

    parser.consume(TokenType.RPAREN, "Expected ')' after SqlExecute arguments");
    const endToken = parser.previous();

    return {
        kind: ASTNodeType.SqlExecute,
        startToken,
        endToken,
        query,
        parameters,
    };
}

/**
 * Parse LSearch function call
 * LSearch ::= "LSearch" "(" StringLiteral ["," Expression] ["," Expression] ["," ArrayLiteral] ")"
 */
export function parseLSearch(parser: SqlStatementParser): LSearchNode {
    const startToken = parser.previous(); // LSearch token
    parser.consume(TokenType.LPAREN, "Expected '(' after LSearch");

    // Parse query string
    const queryToken = parser.consume(TokenType.STRING, "Expected SQL query string");
    const query: StringLiteralNode = {
        kind: ASTNodeType.StringLiteral,
        startToken: queryToken,
        endToken: queryToken,
        value: queryToken.parsedValue || queryToken.value,
        token: queryToken,
    };

    let parameter1: ExpressionNode | undefined = undefined;
    let parameter2: ExpressionNode | undefined = undefined;
    let parameters: ArrayLiteralNode | undefined = undefined;

    // Optional first parameter
    if (parser.match(TokenType.COMMA)) {
        parameter1 = parser.parseExpression();

        // Optional second parameter
        if (parser.match(TokenType.COMMA)) {
            parameter2 = parser.parseExpression();

            // Optional parameters array
            if (parser.match(TokenType.COMMA)) {
                const paramExpr = parser.parseExpression();
                if (paramExpr.kind === ASTNodeType.ArrayLiteral) {
                    parameters = paramExpr as ArrayLiteralNode;
                } else {
                    parser.error("Expected array literal for LSearch parameters");
                }
            }
        }
    }

    parser.consume(TokenType.RPAREN, "Expected ')' after LSearch arguments");
    const endToken = parser.previous();

    return {
        kind: ASTNodeType.LSearch,
        startToken,
        endToken,
        query,
        parameter1,
        parameter2,
        parameters,
    };
}
