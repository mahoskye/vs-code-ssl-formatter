/**
 * Statement parsing functions for SSL parser
 */

import { Token } from "../../tokenizer/token";
import { TokenType } from "../../tokenizer/tokenType";
import {
    StatementNode,
    ExpressionNode,
    ASTNodeType,
    DeclareStatementNode,
    ParametersStatementNode,
    PublicStatementNode,
    IncludeStatementNode,
    ReturnStatementNode,
    LabelStatementNode,
    RegionBlockNode,
    ExitWhileStatementNode,
    ExitForStatementNode,
    LoopContinueNode,
    IdentifierListNode,
    StringLiteralNode,
} from "../ast";

/**
 * Parser interface for statement parsers
 */
export interface StatementParser {
    match(...types: TokenType[]): boolean;
    advance(): Token;
    previous(): Token;
    peek(): Token;
    check(type: TokenType): boolean;
    consume(type: TokenType, message: string): Token;
    parseExpression(): ExpressionNode;
    parseIdentifierList(): IdentifierListNode;
    error(message: string): void;
    isAtEnd(): boolean;
}

/**
 * Parse declare statement
 * :DECLARE IdentifierList
 */
export function parseDeclareStatement(parser: StatementParser): DeclareStatementNode {
    const startToken = parser.previous(); // DECLARE token
    const identifiers = parser.parseIdentifierList();
    const endToken = parser.previous();

    return {
        kind: ASTNodeType.DeclareStatement,
        startToken,
        endToken,
        identifiers,
    } as DeclareStatementNode;
}

/**
 * Parse parameters statement
 * :PARAMETERS IdentifierList
 */
export function parseParametersStatement(parser: StatementParser): ParametersStatementNode {
    const startToken = parser.previous(); // PARAMETERS token
    const identifiers = parser.parseIdentifierList();
    const endToken = parser.previous();

    return {
        kind: ASTNodeType.ParametersStatement,
        startToken,
        endToken,
        identifiers,
    } as ParametersStatementNode;
}

/**
 * Parse public statement
 * :PUBLIC IdentifierList
 */
export function parsePublicStatement(parser: StatementParser): PublicStatementNode {
    const startToken = parser.previous(); // PUBLIC token
    const identifiers = parser.parseIdentifierList();
    const endToken = parser.previous();

    return {
        kind: ASTNodeType.PublicStatement,
        startToken,
        endToken,
        identifiers,
    } as PublicStatementNode;
}

/**
 * Parse include statement
 * :INCLUDE StringLiteral
 */
export function parseIncludeStatement(parser: StatementParser): IncludeStatementNode {
    const startToken = parser.previous(); // INCLUDE token
    const pathToken = parser.consume(TokenType.STRING, "Expected string literal after INCLUDE");
    const path: StringLiteralNode = {
        kind: ASTNodeType.StringLiteral,
        startToken: pathToken,
        endToken: pathToken,
        value: pathToken.parsedValue || pathToken.value,
        token: pathToken,
    };

    return {
        kind: ASTNodeType.IncludeStatement,
        startToken,
        endToken: pathToken,
        path,
    } as IncludeStatementNode;
}

/**
 * Parse return statement
 * :RETURN [Expression]
 */
export function parseReturnStatement(parser: StatementParser): ReturnStatementNode {
    const startToken = parser.previous(); // RETURN token

    let value: ExpressionNode | undefined = undefined;
    let endToken = startToken;

    // Check if there's an expression to return
    if (!parser.check(TokenType.SEMICOLON) && !parser.isAtEnd()) {
        value = parser.parseExpression();
        endToken = parser.previous();
    }

    return {
        kind: ASTNodeType.ReturnStatement,
        startToken,
        endToken,
        value,
    } as ReturnStatementNode;
}

/**
 * Parse label statement
 * :LABEL Identifier
 */
export function parseLabelStatement(parser: StatementParser): LabelStatementNode {
    const startToken = parser.previous(); // LABEL token
    const name = parser.consume(TokenType.IDENTIFIER, "Expected label name");

    return {
        kind: ASTNodeType.LabelStatement,
        startToken,
        endToken: name,
        name,
    } as LabelStatementNode;
}

/**
 * Parse region statement
 * :REGION Identifier ; {Statement} :ENDREGION ;
 */
export function parseRegionStatement(parser: StatementParser): RegionBlockNode {
    const startToken = parser.previous(); // REGION token
    const name = parser.consume(TokenType.IDENTIFIER, "Expected region name");

    const body: StatementNode[] = [];

    // Parse statements until :ENDREGION
    while (
        !parser.isAtEnd() &&
        !(parser.check(TokenType.COLON) && parser.peek().type === TokenType.ENDREGION)
    ) {
        // Note: This would need to be implemented by the main parser
        // For now, we'll just skip to the end
        break;
    }

    // Consume :ENDREGION
    if (parser.check(TokenType.COLON)) {
        parser.advance(); // consume ':'
        parser.consume(TokenType.ENDREGION, "Expected 'ENDREGION'");
    }

    const endToken = parser.previous();

    return {
        kind: ASTNodeType.RegionBlock,
        startToken,
        endToken,
        name,
        body,
    } as RegionBlockNode;
}

/**
 * Parse exit while statement
 * :EXITWHILE
 */
export function parseExitWhileStatement(parser: StatementParser): ExitWhileStatementNode {
    const token = parser.previous(); // EXITWHILE token

    return {
        kind: ASTNodeType.ExitWhileStatement,
        startToken: token,
        endToken: token,
    } as ExitWhileStatementNode;
}

/**
 * Parse exit for statement
 * :EXITFOR
 */
export function parseExitForStatement(parser: StatementParser): ExitForStatementNode {
    const token = parser.previous(); // EXITFOR token

    return {
        kind: ASTNodeType.ExitForStatement,
        startToken: token,
        endToken: token,
    } as ExitForStatementNode;
}

/**
 * Parse loop continue statement
 * :LOOP
 */
export function parseLoopContinueStatement(parser: StatementParser): LoopContinueNode {
    const token = parser.previous(); // LOOP token

    return {
        kind: ASTNodeType.LoopContinue,
        startToken: token,
        endToken: token,
    } as LoopContinueNode;
}
