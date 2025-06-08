/**
 * Procedure parsing functions for SSL parser
 */

import { Token } from "../../tokenizer/token";
import { TokenType } from "../../tokenizer/tokenType";
import {
    StatementNode,
    ProcedureStatementNode,
    ParameterDeclarationNode,
    ASTNodeType,
    IdentifierListNode,
    DefaultParameterDeclarationNode,
    DefaultParameterListNode,
    ExpressionNode,
} from "../ast";

/**
 * Parser interface for procedure parsers
 */
export interface ProcedureParser {
    match(...types: TokenType[]): boolean;
    advance(): Token;
    previous(): Token;
    peek(): Token;
    check(type: TokenType): boolean;
    checkNext(type: TokenType): boolean;
    consume(type: TokenType, message: string): Token;
    parseStatement(): StatementNode | null;
    parseIdentifierList(): IdentifierListNode;
    parseExpression(): ExpressionNode;
    skipWhitespace(): void;
    error(message: string): void;
    isAtEnd(): boolean;
    isInsideProcedure: boolean;
}

/**
 * Parse procedure statement (starting from :PROCEDURE)
 */
export function parseProcedureStatement(parser: ProcedureParser): ProcedureStatementNode {
    // Check for nested procedures
    if (parser.isInsideProcedure) {
        parser.error("Nested procedures are not allowed in SSL");
        throw new Error("Nested procedures are not allowed in SSL");
    }

    // Note: COLON and PROCEDURE tokens are already consumed by parseColonStatement
    return parseProcedureStatementBody(parser);
}

/**
 * Parse procedure statement body (after : and PROCEDURE are consumed)
 */
export function parseProcedureStatementBody(parser: ProcedureParser): ProcedureStatementNode {
    const startToken = parser.previous();
    const name = parser.consume(TokenType.IDENTIFIER, "Expected procedure name");
    parser.consume(TokenType.SEMICOLON, "Expected ';' after procedure name");
    let parameters = undefined;
    let defaultParameters = undefined;
    const body: StatementNode[] = [];

    // Set flag to track that we're inside a procedure
    const wasInsideProcedure = parser.isInsideProcedure;
    parser.isInsideProcedure = true;

    // Skip whitespace before checking for parameters
    parser.skipWhitespace();

    // Optional parameter declaration
    if (parser.check(TokenType.COLON) && parser.checkNext(TokenType.PARAMETERS)) {
        parser.advance(); // consume ':'
        parser.advance(); // consume 'PARAMETERS'
        const paramList = parser.parseIdentifierList();
        parser.consume(TokenType.SEMICOLON, "Expected ';' after PARAMETERS");
        parameters = {
            kind: ASTNodeType.ParameterDeclaration,
            startToken: parser.previous(),
            endToken: parser.previous(),
            parameters: paramList,
        } as ParameterDeclarationNode;
    }

    // Skip whitespace before checking for default parameters
    parser.skipWhitespace();

    // Optional default parameter declaration
    if (parser.check(TokenType.COLON) && parser.checkNext(TokenType.DEFAULT)) {
        const defaultStartToken = parser.advance(); // consume ':'
        parser.advance(); // consume 'DEFAULT'

        const defaultParamList = parseDefaultParameterList(parser);
        parser.consume(TokenType.SEMICOLON, "Expected ';' after DEFAULT");
        defaultParameters = {
            kind: ASTNodeType.DefaultParameterDeclaration,
            startToken: defaultStartToken,
            endToken: parser.previous(),
            defaults: defaultParamList,
        } as DefaultParameterDeclarationNode;
    }

    // Parse procedure body
    while (!parser.isAtEnd()) {
        // Skip whitespace tokens before checking for end condition
        parser.skipWhitespace();

        // Check for :ENDPROC
        if (parser.check(TokenType.COLON) && parser.checkNext(TokenType.ENDPROC)) {
            break;
        }

        const stmt = parser.parseStatement();
        if (stmt) {
            body.push(stmt);
        }
    }

    // Consume :ENDPROC
    parser.consume(TokenType.COLON, "Expected ':' before ENDPROC");
    parser.consume(TokenType.ENDPROC, "Expected 'ENDPROC'");
    parser.consume(TokenType.SEMICOLON, "Expected ';' after ENDPROC");
    const endToken = parser.previous();

    // Restore the previous procedure context
    parser.isInsideProcedure = wasInsideProcedure;

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
 * Parses a default parameter list
 * According to SSL grammar: DefaultParameterList ::= Identifier "," Expression
 * e.g., p1, "default1"
 */
function parseDefaultParameterList(parser: ProcedureParser): DefaultParameterListNode {
    const startToken = parser.peek();

    const identifier = parser.consume(
        TokenType.IDENTIFIER,
        "Expected identifier in default parameter list"
    );
    parser.consume(TokenType.COMMA, "Expected ',' after identifier in default parameter list");
    const defaultValue = parser.parseExpression();

    const endToken = parser.previous();

    return {
        kind: ASTNodeType.DefaultParameterList,
        startToken,
        endToken,
        pairs: [{ identifier, defaultValue }],
    };
}
