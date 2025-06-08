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
    checkNext(type: TokenType): boolean;
    consume(type: TokenType, message: string): Token;
    parseIdentifierList(): IdentifierListNode;
    parseExpression(): ExpressionNode;
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
    parser.consume(TokenType.SEMICOLON, "Expected ';' after DECLARE statement");
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
    parser.consume(TokenType.SEMICOLON, "Expected ';' after PARAMETERS statement");
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
    parser.consume(TokenType.SEMICOLON, "Expected ';' after PUBLIC statement");
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
    parser.consume(TokenType.SEMICOLON, "Expected ';' after INCLUDE statement");
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
        endToken: parser.previous(),
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

    parser.consume(TokenType.SEMICOLON, "Expected ';' after RETURN statement");
    endToken = parser.previous();

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
    parser.consume(TokenType.SEMICOLON, "Expected ';' after LABEL statement");

    return {
        kind: ASTNodeType.LabelStatement,
        startToken,
        endToken: parser.previous(),
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
    const startToken = parser.previous();
    parser.consume(TokenType.SEMICOLON, "Expected ';' after :EXITWHILE");
    return {
        kind: ASTNodeType.ExitWhileStatement,
        startToken: startToken,
        endToken: parser.previous(),
    } as ExitWhileStatementNode;
}

/**
 * Parse exit for statement
 * :EXITFOR
 */
export function parseExitForStatement(parser: StatementParser): ExitForStatementNode {
    const startToken = parser.previous();
    parser.consume(TokenType.SEMICOLON, "Expected ';' after :EXITFOR");
    return {
        kind: ASTNodeType.ExitForStatement,
        startToken: startToken,
        endToken: parser.previous(),
    } as ExitForStatementNode;
}

/**
 * Parse loop continue statement
 * :LOOP
 */
export function parseLoopContinueStatement(parser: StatementParser): LoopContinueNode {
    const startToken = parser.previous();
    parser.consume(TokenType.SEMICOLON, "Expected ';' after :LOOP");
    return {
        kind: ASTNodeType.LoopContinue,
        startToken: startToken,
        endToken: parser.previous(),
    } as LoopContinueNode;
}

export function parseExitCaseStatement(parser: StatementParser): StatementNode {
    const startToken = parser.previous();
    parser.consume(TokenType.SEMICOLON, "Expected ';' after :EXITCASE");
    return {
        kind: ASTNodeType.ExitCaseStatement,
        startToken: startToken,
        endToken: parser.previous(),
    };
}

/**
 * Parse default statement
 * :DEFAULT Identifier "," Expression
 */
export function parseDefaultStatement(parser: StatementParser): StatementNode {
    const startToken = parser.previous(); // DEFAULT token

    // Parse identifier
    const identifier = parser.consume(
        TokenType.IDENTIFIER,
        "Expected parameter name after :DEFAULT"
    );
    parser.consume(TokenType.COMMA, "Expected ',' after parameter name in :DEFAULT");

    // Parse default value expression
    const defaultValue = parser.parseExpression();
    parser.consume(TokenType.SEMICOLON, "Expected ';' after :DEFAULT statement");

    const endToken = parser.previous();

    return {
        kind: ASTNodeType.DefaultStatement,
        startToken,
        endToken,
        defaults: {
            kind: ASTNodeType.DefaultParameterList,
            startToken: identifier,
            endToken,
            pairs: [{ identifier, defaultValue }],
        },
    } as any;
}

/**
 * Parse error block stanza
 * :ERROR {Statement}
 */
export function parseErrorBlockStanza(parser: StatementParser): StatementNode {
    const startToken = parser.previous(); // ERROR token
    parser.consume(TokenType.SEMICOLON, "Expected ';' after :ERROR");

    const statements: StatementNode[] = [];

    // Parse statements until we hit another keyword or end of input
    // Note: This is a simplified implementation - in practice, error blocks
    // continue until the next major construct or end of procedure

    const endToken = parser.previous();

    return {
        kind: ASTNodeType.ErrorBlockStanza,
        startToken,
        endToken,
        statements,
    } as any;
}

/**
 * Parse inline code block start
 * :BEGININLINECODE [StringLiteral | Identifier]
 */
export function parseInlineCodeStart(parser: StatementParser): StatementNode {
    const startToken = parser.previous(); // BEGININLINECODE token

    let language: any = undefined;

    // Optional language specification
    if (parser.check(TokenType.STRING) || parser.check(TokenType.IDENTIFIER)) {
        const langToken = parser.advance();
        language =
            langToken.type === TokenType.STRING
                ? {
                      kind: ASTNodeType.StringLiteral,
                      startToken: langToken,
                      endToken: langToken,
                      value: langToken.parsedValue || langToken.value,
                      token: langToken,
                  }
                : langToken;
    }

    parser.consume(TokenType.SEMICOLON, "Expected ';' after :BEGININLINECODE");
    const endToken = parser.previous();

    return {
        kind: ASTNodeType.InlineCodeStart,
        startToken,
        endToken,
        language,
    } as any;
}

/**
 * Parse inline code block end
 * :ENDINLINECODE
 */
export function parseInlineCodeEnd(parser: StatementParser): StatementNode {
    const startToken = parser.previous(); // ENDINLINECODE token
    parser.consume(TokenType.SEMICOLON, "Expected ';' after :ENDINLINECODE");

    return {
        kind: ASTNodeType.InlineCodeEnd,
        startToken,
        endToken: parser.previous(),
    } as any;
}
