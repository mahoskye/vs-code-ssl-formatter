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
    LogicalExpressionNode,
    ComparisonExpressionNode,
    ArithmeticExpressionNode,
    TermNode,
    FactorNode,
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
 * Parse expressions with proper precedence according to SSL EBNF grammar
 * Expression ::= LogicalExpression
 */
export function parseExpression(context: ExpressionParserContext): ExpressionNode {
    return parseLogicalExpression(context);
}

/**
 * Parse logical expressions (.AND., .OR.)
 * LogicalExpression ::= ComparisonExpression {LogicalOperator ComparisonExpression}
 * LogicalOperator ::= ".AND." | ".OR."
 */
function parseLogicalExpression(context: ExpressionParserContext): ExpressionNode {
    let expr = parseComparisonExpression(context);

    while (context.match(TokenType.AND, TokenType.OR)) {
        const operator = context.previous();
        const right = parseComparisonExpression(context);
        expr = {
            kind: ASTNodeType.LogicalExpression,
            startToken: expr.startToken,
            endToken: right.endToken,
            left: expr,
            operator,
            right,
        } as LogicalExpressionNode;
    }

    return expr;
}

/**
 * Parse comparison expressions (==, !=, <, >, <=, >=, =)
 * ComparisonExpression ::= ArithmeticExpression {ComparisonOperator ArithmeticExpression}
 * ComparisonOperator ::= "==" | "!=" | "<" | ">" | "<=" | ">=" | "="
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
            kind: ASTNodeType.ComparisonExpression,
            startToken: expr.startToken,
            endToken: right.endToken,
            left: expr,
            operator,
            right,
        } as ComparisonExpressionNode;
    }

    return expr;
}

/**
 * Parse arithmetic expressions (+, -)
 * ArithmeticExpression ::= Term {AdditiveOperator Term}
 * AdditiveOperator ::= "+" | "-"
 */
export function parseArithmeticExpression(context: ExpressionParserContext): ExpressionNode {
    let expr = parseTerm(context);

    while (context.match(TokenType.PLUS, TokenType.MINUS)) {
        const operator = context.previous();
        const right = parseTerm(context);
        expr = {
            kind: ASTNodeType.ArithmeticExpression,
            startToken: expr.startToken,
            endToken: right.endToken,
            left: expr,
            operator,
            right,
        } as ArithmeticExpressionNode;
    }

    return expr;
}

/**
 * Parse terms (*, /, %)
 * Term ::= Factor {MultiplicativeOperator Factor}
 * MultiplicativeOperator ::= "*" | "/" | "%"
 */
function parseTerm(context: ExpressionParserContext): ExpressionNode {
    let expr = parseFactor(context);

    while (context.match(TokenType.MULTIPLY, TokenType.DIVIDE, TokenType.MODULO)) {
        const operator = context.previous();
        const right = parseFactor(context);
        expr = {
            kind: ASTNodeType.Term,
            startToken: expr.startToken,
            endToken: right.endToken,
            left: expr,
            operator,
            right,
        } as TermNode;
    }

    return expr;
}

/**
 * Parse factors (^)
 * Factor ::= PowerOperand {"^" PowerOperand}
 */
function parseFactor(context: ExpressionParserContext): ExpressionNode {
    let expr = parsePowerOperand(context);

    while (context.match(TokenType.POWER)) {
        const operator = context.previous();
        const right = parsePowerOperand(context);
        expr = {
            kind: ASTNodeType.Factor,
            startToken: expr.startToken,
            endToken: right.endToken,
            left: expr,
            operator,
            right,
        } as FactorNode;
    }

    return expr;
}

/**
 * Parse power operands
 * PowerOperand ::= [UnaryOperator] Primary
 */
function parsePowerOperand(context: ExpressionParserContext): ExpressionNode {
    return parseUnary(context);
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
 * Primary ::= Literal | VariableAccess | PropertyAccess | ArrayAccess | FunctionCall |
 *            BitwiseOperation | "(" Expression ")" | IncrementExpression | MethodCall
 */
function parsePrimary(context: ExpressionParserContext): ExpressionNode {
    // Check for prefix increment/decrement
    if (context.match(TokenType.INCREMENT, TokenType.DECREMENT)) {
        const operator = context.previous();
        const operand = context.consume(
            TokenType.IDENTIFIER,
            "Expected identifier after prefix increment/decrement"
        );
        return {
            kind: ASTNodeType.IncrementExpression,
            startToken: operator,
            endToken: operand,
            operator,
            operand,
            prefix: true,
        } as any;
    }

    // Literals
    if (
        context.match(
            TokenType.NUMBER,
            TokenType.STRING,
            TokenType.SQL_STRING,
            TokenType.BOOLEAN,
            TokenType.NIL
        )
    ) {
        const token = context.previous();
        return {
            kind: ASTNodeType.LiteralExpression,
            startToken: token,
            endToken: token,
            value: token.parsedValue || token.value,
            token,
        } as LiteralExpressionNode;
    }

    // Array literals and special constructs starting with {
    if (context.match(TokenType.LBRACE)) {
        return parseArrayOrSpecialLiteral(context);
    }

    // Code block literals - {|params| expressions}
    if (context.match(TokenType.CODE_BLOCK_START)) {
        return parseCodeBlockLiteral(context);
    }

    // Date literals - handled by parseArrayOrSpecialLiteral when it detects date pattern
    if (context.match(TokenType.DATE)) {
        const token = context.previous();
        return {
            kind: ASTNodeType.DateLiteral,
            startToken: token,
            endToken: token,
            components: token.parsedValue || [],
        } as any;
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

        // Check for postfix increment/decrement
        if (context.match(TokenType.INCREMENT, TokenType.DECREMENT)) {
            const operator = context.previous();
            return {
                kind: ASTNodeType.IncrementExpression,
                startToken: name,
                endToken: operator,
                operator,
                operand: name,
                prefix: false,
            } as any;
        }

        // Function call
        if (context.match(TokenType.LPAREN)) {
            // Check for special SSL functions
            if (name.value === "SqlExecute") {
                // Handle SqlExecute specially - need to parse as SQL statement
                const args = context.parseArgumentList();
                context.consume(TokenType.RPAREN, "Expected ')' after arguments");

                if (args.arguments.length === 0) {
                    context.error("SqlExecute requires at least one argument (SQL query string)");
                    return context.createErrorNode();
                }

                return {
                    kind: ASTNodeType.SqlExecute,
                    startToken: name,
                    endToken: context.previous(),
                    query: args.arguments[0] as any, // First argument should be string literal
                    parameters: args.arguments.length > 1 ? (args.arguments[1] as any) : undefined, // Second argument should be array literal
                } as any;
            }

            if (name.value === "LSearch") {
                // Handle LSearch specially
                const args = context.parseArgumentList();
                context.consume(TokenType.RPAREN, "Expected ')' after arguments");
                return {
                    kind: ASTNodeType.LSearch,
                    startToken: name,
                    endToken: context.previous(),
                    query: args.arguments[0] as any,
                    parameter1: args.arguments[1],
                    parameter2: args.arguments[2],
                    parameters: args.arguments[3] as any,
                } as any;
            }

            if (name.value === "DoProc") {
                // Handle DoProc specially
                const args = context.parseArgumentList();
                context.consume(TokenType.RPAREN, "Expected ')' after arguments");
                return {
                    kind: ASTNodeType.DoProcCall,
                    startToken: name,
                    endToken: context.previous(),
                    procedureName: args.arguments[0] as any,
                    arguments: args.arguments[1] as any,
                } as any;
            }

            if (name.value === "ExecFunction") {
                // Handle ExecFunction specially
                const args = context.parseArgumentList();
                context.consume(TokenType.RPAREN, "Expected ')' after arguments");
                return {
                    kind: ASTNodeType.ExecFunctionCall,
                    startToken: name,
                    endToken: context.previous(),
                    functionName: args.arguments[0] as any,
                    arguments: args.arguments[1] as any,
                } as any;
            }

            if (name.value === "CreateUDObject") {
                // Handle object creation
                const args = context.parseArgumentList();
                context.consume(TokenType.RPAREN, "Expected ')' after arguments");
                return {
                    kind: ASTNodeType.ObjectCreation,
                    startToken: name,
                    endToken: context.previous(),
                    className: args.arguments[0] as any,
                } as any;
            }

            if (name.value === "Branch") {
                // Handle branch statement
                const args = context.parseArgumentList();
                context.consume(TokenType.RPAREN, "Expected ')' after arguments");
                return {
                    kind: ASTNodeType.BranchStatement,
                    startToken: name,
                    endToken: context.previous(),
                    target: args.arguments[0] as any,
                } as any;
            }

            if (name.value === "ExecUDF") {
                // Handle dynamic code execution
                const args = context.parseArgumentList();
                context.consume(TokenType.RPAREN, "Expected ')' after arguments");
                return {
                    kind: ASTNodeType.DynamicCodeExecution,
                    startToken: name,
                    endToken: context.previous(),
                    code: args.arguments[0] as any,
                    parameters: args.arguments[1] as any,
                } as any;
            }

            // Check for bitwise operations
            if (
                name.value === "_AND" ||
                name.value === "_OR" ||
                name.value === "_XOR" ||
                name.value === "_NOT"
            ) {
                const args = context.parseArgumentList();
                context.consume(TokenType.RPAREN, "Expected ')' after arguments");
                return {
                    kind: ASTNodeType.BitwiseOperation,
                    startToken: name,
                    endToken: context.previous(),
                    operation: name.value as any,
                    operands: args.arguments,
                } as any;
            }

            // Regular function call
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

        // Property access (Object:Property) or Method call (Object:Method())
        if (context.match(TokenType.COLON)) {
            const property = context.consume(
                TokenType.IDENTIFIER,
                "Expected property name after ':'"
            );

            // Check if this is a method call
            if (context.match(TokenType.LPAREN)) {
                const args = context.parseArgumentList();
                context.consume(TokenType.RPAREN, "Expected ')' after method arguments");
                return {
                    kind: ASTNodeType.MethodCall,
                    startToken: name,
                    endToken: context.previous(),
                    object: name,
                    method: property,
                    arguments: args,
                } as any;
            }

            // Property access
            return {
                kind: ASTNodeType.PropertyAccess,
                startToken: name,
                endToken: property,
                object: name,
                property,
            } as PropertyAccessNode;
        }

        // Array access - supports both arr[1,2] and arr[1][2] syntax
        let expr: ExpressionNode = {
            kind: ASTNodeType.VariableAccess,
            startToken: name,
            endToken: name,
            name,
        } as VariableAccessNode;

        // Handle chained array access: arr[1][2][3]
        while (context.match(TokenType.LBRACKET)) {
            const indices: ExpressionNode[] = [];
            do {
                indices.push(parseExpression(context));
            } while (context.match(TokenType.COMMA));

            context.consume(TokenType.RBRACKET, "Expected ']' after array index");

            expr = {
                kind: ASTNodeType.ArrayAccess,
                startToken: expr.startToken,
                endToken: context.previous(),
                array: expr,
                indices,
            } as ArrayAccessNode;
        }

        // If we processed any array access, return the result
        if (expr.kind === ASTNodeType.ArrayAccess) {
            return expr;
        }

        // Simple variable access
        // Validate against grammar rules
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
 * Parse array literal or special constructs starting with {
 * Handles arrays, date literals, and distinguishes from code blocks
 */
function parseArrayOrSpecialLiteral(context: ExpressionParserContext): ExpressionNode {
    const startToken = context.previous(); // '{'
    const elements: ExpressionNode[] = [];

    // Check if this might be a date literal by looking ahead
    // Date literals have the pattern: {number, number, number[, number, number, number]}
    let isDateLiteral = true;
    let elementCount = 0;

    if (!context.check(TokenType.RBRACE)) {
        do {
            const expr = parseExpression(context);
            elements.push(expr);
            elementCount++;

            // Check if this element is a number literal for date detection
            if (
                expr.kind !== ASTNodeType.LiteralExpression ||
                (expr as any).token?.type !== TokenType.NUMBER
            ) {
                isDateLiteral = false;
            }
        } while (context.match(TokenType.COMMA));
    }

    context.consume(TokenType.RBRACE, "Expected '}' after array elements");
    const endToken = context.previous();

    // Determine if this is a date literal based on EBNF grammar
    // DateLiteral ::= "{" NumberLiteral "," NumberLiteral "," NumberLiteral
    //                 ["," NumberLiteral "," NumberLiteral "," NumberLiteral] "}"
    if (isDateLiteral && (elementCount === 3 || elementCount === 6)) {
        return {
            kind: ASTNodeType.DateLiteral,
            startToken,
            endToken,
            components: elements,
        } as any;
    }

    // Regular array literal
    return {
        kind: ASTNodeType.ArrayLiteral,
        startToken,
        endToken,
        elements,
    } as any;
}

/**
 * Parse code block literal: {|params| expressions}
 * CodeBlockLiteral ::= "{|" [IdentifierList] "|" ExpressionList "}"
 */
function parseCodeBlockLiteral(context: ExpressionParserContext): ExpressionNode {
    const startToken = context.previous(); // '{|'
    let parameters: any = undefined;
    const body: ExpressionNode[] = [];

    // Parse optional parameter list - only if we don't immediately see a pipe
    // The issue was that we need to check for PIPE first, not try to parse identifiers
    if (!context.check(TokenType.PIPE)) {
        // Only try to parse parameters if the next token is an identifier
        if (context.check(TokenType.IDENTIFIER)) {
            const paramTokens: any[] = [];
            do {
                paramTokens.push(context.consume(TokenType.IDENTIFIER, "Expected parameter name"));
            } while (context.match(TokenType.COMMA));

            if (paramTokens.length > 0) {
                parameters = {
                    kind: ASTNodeType.IdentifierList,
                    startToken: paramTokens[0],
                    endToken: paramTokens[paramTokens.length - 1],
                    identifiers: paramTokens,
                };
            }
        }
    }

    context.consume(TokenType.PIPE, "Expected '|' after code block parameters");

    // Parse expression list - handle both single expressions and comma-separated lists
    if (!context.check(TokenType.RBRACE)) {
        do {
            body.push(parseExpression(context));
        } while (context.match(TokenType.COMMA));
    }

    context.consume(TokenType.RBRACE, "Expected '}' after code block body");
    const endToken = context.previous();

    return {
        kind: ASTNodeType.CodeBlockLiteral,
        startToken,
        endToken,
        parameters,
        body,
    } as any;
}

/**
 * Parse array literal (legacy function for backward compatibility)
 */
function parseArrayLiteral(context: ExpressionParserContext): ExpressionNode {
    return parseArrayOrSpecialLiteral(context);
}

/**
 * Simple expression parser for FOR loops to avoid property access issues
 */
export function parseSimpleExpression(context: ExpressionParserContext): ExpressionNode {
    return parseArithmeticExpression(context);
}
