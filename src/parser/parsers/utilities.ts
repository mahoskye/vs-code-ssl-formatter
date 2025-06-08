/**
 * Utility functions and helper methods for SSL parser
 * Handles token management, error handling, and common parsing utilities
 */

import { Token } from "../../tokenizer/token";
import { TokenType } from "../../tokenizer/tokenType";
import {
    ExpressionNode,
    ASTNodeType,
    LiteralExpressionNode,
    ArgumentListNode,
    IdentifierListNode,
} from "../ast";

/**
 * Interface for parser utility context
 */
export interface UtilityParserContext {
    tokens: Token[];
    current: number;
    errors: ParseError[];
    panicMode: boolean;
    parseExpression(): ExpressionNode;
}

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
 * Parser utility functions interface
 */
export interface ParserUtilities {
    skipWhitespace(): void;
    match(...types: TokenType[]): boolean;
    check(type: TokenType): boolean;
    checkNext(type: TokenType): boolean;
    advance(): Token;
    isAtEnd(): boolean;
    peek(): Token;
    previous(): Token;
    consume(type: TokenType, message: string): Token;
    error(message: string): void;
    createErrorNode(): ExpressionNode;
    parseArgumentList(): ArgumentListNode;
    parseIdentifierList(): IdentifierListNode;
    synchronize(): void;
}

/**
 * Skip whitespace tokens (including NEWLINE)
 */
export function skipWhitespace(context: UtilityParserContext): void {
    while (!isAtEnd(context) && check(context, TokenType.NEWLINE)) {
        advance(context);
    }
}

/**
 * Check if any of the given token types match the current token
 */
export function match(context: UtilityParserContext, ...types: TokenType[]): boolean {
    for (const type of types) {
        if (check(context, type)) {
            advance(context);
            return true;
        }
    }
    return false;
}

/**
 * Check if current token matches the given type
 */
export function check(context: UtilityParserContext, type: TokenType): boolean {
    if (isAtEnd(context)) {
        return false;
    }
    return peek(context).type === type;
}

/**
 * Check if next token matches the given type
 */
export function checkNext(context: UtilityParserContext, type: TokenType): boolean {
    if (context.current + 1 >= context.tokens.length) {
        return false;
    }
    return context.tokens[context.current + 1].type === type;
}

/**
 * Advance to next token
 */
export function advance(context: UtilityParserContext): Token {
    if (!isAtEnd(context)) {
        context.current++;
    }
    return previous(context);
}

/**
 * Check if we're at the end of tokens
 */
export function isAtEnd(context: UtilityParserContext): boolean {
    return peek(context).type === TokenType.EOF;
}

/**
 * Get current token
 */
export function peek(context: UtilityParserContext): Token {
    return context.tokens[context.current];
}

/**
 * Get previous token
 */
export function previous(context: UtilityParserContext): Token {
    return context.tokens[context.current - 1];
}

/**
 * Consume a token of the expected type or throw error
 */
export function consume(context: UtilityParserContext, type: TokenType, message: string): Token {
    if (check(context, type)) {
        return advance(context);
    }

    error(context, message);
    throw new Error(message);
}

/**
 * Record a parsing error
 */
export function error(context: UtilityParserContext, message: string): void {
    const token = peek(context);
    context.errors.push({
        message,
        token,
        line: token.range.start.line,
        column: token.range.start.column,
        severity: "error",
    });
}

/**
 * Create an error recovery node
 */
export function createErrorNode(context: UtilityParserContext): ExpressionNode {
    const token = peek(context);
    return {
        kind: ASTNodeType.LiteralExpression,
        startToken: token,
        endToken: token,
        value: null,
        token,
    } as LiteralExpressionNode;
}

/**
 * Parse argument list for function calls
 */
export function parseArgumentList(context: UtilityParserContext): ArgumentListNode {
    const startToken = peek(context);
    const args: ExpressionNode[] = [];

    if (!check(context, TokenType.RPAREN)) {
        do {
            // Handle empty arguments (consecutive commas)
            if (check(context, TokenType.COMMA) || check(context, TokenType.RPAREN)) {
                // Create a NIL literal for empty argument
                const nilToken = peek(context);
                args.push({
                    kind: ASTNodeType.LiteralExpression,
                    startToken: nilToken,
                    endToken: nilToken,
                    value: null,
                    token: {
                        ...nilToken,
                        type: TokenType.NIL,
                        value: "NIL",
                        parsedValue: null,
                    },
                } as any);
            } else if (check(context, TokenType.SQL_STRING)) {
                // Handle SQL_STRING tokens specially
                const token = advance(context);
                args.push({
                    kind: ASTNodeType.StringLiteral,
                    startToken: token,
                    endToken: token,
                    value: token.parsedValue || token.value,
                    token,
                } as any);
            } else {
                args.push(context.parseExpression());
            }
        } while (match(context, TokenType.COMMA));
    }

    const endToken = previous(context);
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
export function parseIdentifierList(context: UtilityParserContext): IdentifierListNode {
    const startToken = peek(context);
    const identifiers: Token[] = [];

    identifiers.push(consume(context, TokenType.IDENTIFIER, "Expected identifier"));

    while (match(context, TokenType.COMMA)) {
        identifiers.push(consume(context, TokenType.IDENTIFIER, "Expected identifier after ','"));
    }

    const endToken = previous(context);
    return {
        kind: ASTNodeType.IdentifierList,
        startToken,
        endToken,
        identifiers,
    };
}

/**
 * Check if current token is an assignment operator
 */
export function matchAssignmentOperator(context: UtilityParserContext): boolean {
    return match(
        context,
        TokenType.ASSIGN,
        TokenType.PLUS_ASSIGN,
        TokenType.MINUS_ASSIGN,
        TokenType.MULT_ASSIGN,
        TokenType.DIV_ASSIGN,
        TokenType.POWER_ASSIGN
    );
}

/**
 * Error recovery: advance to next synchronization point
 */
export function synchronize(context: UtilityParserContext): void {
    advance(context);
    while (!isAtEnd(context)) {
        if (previous(context).type === TokenType.SEMICOLON) {
            return;
        }

        switch (peek(context).type) {
            case TokenType.CLASS:
            case TokenType.PROCEDURE:
            case TokenType.IF:
            case TokenType.FOR:
            case TokenType.WHILE:
            case TokenType.RETURN:
            case TokenType.DECLARE:
                return;
        }

        advance(context);
    }
}
