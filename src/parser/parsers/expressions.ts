/**
 * Expression parsing module for SSL language
 * Handles all expression parsing with proper precedence
 */

import { Token } from "../../tokenizer/token";
import { TokenType } from "../../tokenizer/tokenType";
import {
    ExpressionNode,
    ASTNodeType,
    BinaryExpressionNode,
    UnaryExpressionNode,
    LiteralExpressionNode,
    VariableAccessNode,
    PropertyAccessNode,
    ArrayAccessNode,
    DirectFunctionCallNode,
    ArgumentListNode,
} from "../ast";

/**
 * Interface for expression parser dependencies
 */
export interface ExpressionParserContext {
    match(...types: TokenType[]): boolean;
    check(type: TokenType): boolean;
    advance(): Token;
    previous(): Token;
    peek(): Token;
    consume(type: TokenType, message: string): Token;
    error(message: string): void;
    createErrorNode(): ExpressionNode;
    parseArgumentList(): ArgumentListNode;
}

/**
 * Expression parser interface
 */
export interface ExpressionParser {
    parseExpression(): ExpressionNode;
    parseArithmeticExpression(): ExpressionNode;
}

/**
 * Parse expressions with proper precedence
 */
export function parseExpression(context: ExpressionParserContext): ExpressionNode {
    return parseLogicalExpression(context);
}

/**
 * Parse logical expressions (.AND., .OR.)
 */
function parseLogicalExpression(context: ExpressionParserContext): ExpressionNode {
    let expr = parseComparisonExpression(context);
    while (context.match(TokenType.AND, TokenType.OR)) {
        const operator = context.previous();
        const right = parseComparisonExpression(context);
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
function parseComparisonExpression(context: ExpressionParserContext): ExpressionNode {
    let expr = parseArithmeticExpression(context);
    while (
        context.match(
            TokenType.STRICT_EQUAL,
            TokenType.NOT_EQUAL,
            TokenType.LESS_THAN,
            TokenType.GREATER_THAN,
            TokenType.LESS_EQUAL,
            TokenType.GREATER_EQUAL,
            TokenType.EQUAL
        )
    ) {
        const operator = context.previous();
        const right = parseArithmeticExpression(context);
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
export function parseArithmeticExpression(context: ExpressionParserContext): ExpressionNode {
    let expr = parseTerm(context);
    while (context.match(TokenType.PLUS, TokenType.MINUS)) {
        const operator = context.previous();
        const right = parseTerm(context);
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
function parseTerm(context: ExpressionParserContext): ExpressionNode {
    let expr = parseFactor(context);
    while (context.match(TokenType.MULTIPLY, TokenType.DIVIDE, TokenType.MODULO)) {
        const operator = context.previous();
        const right = parseFactor(context);
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
function parseFactor(context: ExpressionParserContext): ExpressionNode {
    let expr = parseUnary(context);
    while (context.match(TokenType.POWER)) {
        const operator = context.previous();
        const right = parseUnary(context);
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
function parseUnary(context: ExpressionParserContext): ExpressionNode {
    if (context.match(TokenType.LOGICAL_NOT, TokenType.NOT, TokenType.PLUS, TokenType.MINUS)) {
        const operator = context.previous();
        const operand = parseUnary(context);
        return {
            kind: ASTNodeType.UnaryExpression,
            startToken: operator,
            endToken: operand.endToken,
            operator,
            operand,
        } as UnaryExpressionNode;
    }

    return parsePrimary(context);
}

/**
 * Parse primary expressions (literals, identifiers, function calls, etc.)
 */
function parsePrimary(context: ExpressionParserContext): ExpressionNode {
    // Literals
    if (context.match(TokenType.NUMBER, TokenType.STRING, TokenType.BOOLEAN, TokenType.NIL)) {
        const token = context.previous();
        return {
            kind: ASTNodeType.LiteralExpression,
            startToken: token,
            endToken: token,
            value: token.parsedValue || token.value,
            token,
        } as LiteralExpressionNode;
    }

    // Array literals
    if (context.match(TokenType.LBRACE)) {
        return parseArrayLiteral(context);
    }

    // Parenthesized expressions
    if (context.match(TokenType.LPAREN)) {
        const expr = parseExpression(context);
        context.consume(TokenType.RPAREN, "Expected ')' after expression");
        return expr;
    }

    // Identifiers (variables, function calls, property access)
    if (context.match(TokenType.IDENTIFIER)) {
        const name = context.previous();

        // Function call
        if (context.match(TokenType.LPAREN)) {
            const args = context.parseArgumentList();
            context.consume(TokenType.RPAREN, "Expected ')' after arguments");
            return {
                kind: ASTNodeType.DirectFunctionCall,
                startToken: name,
                endToken: context.previous(),
                name,
                arguments: args,
            } as DirectFunctionCallNode;
        }

        // Property access (Object:Property)
        if (context.match(TokenType.COLON)) {
            const property = context.consume(
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
        if (context.match(TokenType.LBRACKET)) {
            const indices: ExpressionNode[] = [];
            do {
                indices.push(parseExpression(context));
            } while (context.match(TokenType.COMMA));

            context.consume(TokenType.RBRACKET, "Expected ']' after array index");

            return {
                kind: ASTNodeType.ArrayAccess,
                startToken: name,
                endToken: context.previous(),
                array: name,
                indices,
            } as ArrayAccessNode;
        }

        // Simple variable access
        // Check for invalid identifiers that violate EBNF grammar
        if (name.value === "TRUE" || name.value === "FALSE") {
            context.error(
                `Invalid boolean literal '${name.value}'. Use '.T.' or '.F.' instead according to SSL grammar.`
            );
            return context.createErrorNode();
        }

        return {
            kind: ASTNodeType.VariableAccess,
            startToken: name,
            endToken: name,
            name,
        } as VariableAccessNode;
    }

    context.error("Unexpected token in expression");
    return context.createErrorNode();
}

/**
 * Parse array literal
 */
function parseArrayLiteral(context: ExpressionParserContext): ExpressionNode {
    const startToken = context.previous(); // '{'
    const elements: ExpressionNode[] = [];

    if (!context.check(TokenType.RBRACE)) {
        do {
            elements.push(parseExpression(context));
        } while (context.match(TokenType.COMMA));
    }

    context.consume(TokenType.RBRACE, "Expected '}' after array elements");
    const endToken = context.previous();

    return {
        kind: ASTNodeType.ArrayLiteral,
        startToken,
        endToken,
        elements,
    } as any;
}

/**
 * Simple expression parser for FOR loops to avoid property access issues
 */
export function parseSimpleExpression(context: ExpressionParserContext): ExpressionNode {
    return parseArithmeticExpression(context);
}
