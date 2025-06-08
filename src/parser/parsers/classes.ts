/**
 * Class parsing module for SSL language
 * Handles class definitions, inheritance, and class members
 */

import { Token } from "../../tokenizer/token";
import { TokenType } from "../../tokenizer/tokenType";
import {
    ASTNodeType,
    ClassDefinitionNode,
    ClassDeclarationNode,
    InheritStatementNode,
    IdentifierListNode,
} from "../ast";
import { parseProcedureStatement } from "./procedure";

/**
 * Interface for class parser dependencies
 */
export interface ClassParserContext {
    check(type: TokenType): boolean;
    checkNext(type: TokenType): boolean;
    advance(): Token;
    previous(): Token;
    peek(): Token;
    consume(type: TokenType, message: string): Token;
    skipWhitespace(): void;
    parseIdentifierList(): IdentifierListNode;
    isAtEnd(): boolean;
}

/**
 * Class parser interface
 */
export interface ClassParser {
    parseClassDefinition(): ClassDefinitionNode;
}

/**
 * Parse a class definition
 * ClassDefinition ::= ClassDeclaration [InheritStatement] {ClassMember}
 */
export function parseClassDefinition(context: ClassParserContext): ClassDefinitionNode {
    const startToken = context.peek();

    context.consume(TokenType.COLON, "Expected ':' before class declaration");
    context.consume(TokenType.CLASS, "Expected 'CLASS' keyword");
    context.consume(TokenType.SEMICOLON, "Expected ';' after class declaration.");
    context.skipWhitespace(); // Skip any whitespace/newlines before class name
    const className = context.consume(TokenType.IDENTIFIER, "Expected class name");
    context.consume(TokenType.SEMICOLON, "Expected ';' after class name");

    const declaration = {
        kind: ASTNodeType.ClassDeclaration,
        startToken: startToken,
        endToken: className,
        name: className,
    } as ClassDeclarationNode;

    let inherit = undefined;
    const members: any[] = [];

    // Skip whitespace before checking for inheritance
    context.skipWhitespace();

    // Optional inherit statement
    if (context.check(TokenType.COLON) && context.checkNext(TokenType.INHERIT)) {
        context.advance(); // consume ':'
        context.advance(); // consume 'INHERIT'
        context.consume(TokenType.SEMICOLON, "Expected ';' after :INHERIT clause.");
        context.skipWhitespace(); // Skip any whitespace/newlines before inherited class name
        const inheritClassName = context.consume(
            TokenType.IDENTIFIER,
            "Expected inherited class name"
        );
        context.consume(TokenType.SEMICOLON, "Expected ';' after inherited class name");
        inherit = {
            kind: ASTNodeType.InheritStatement,
            startToken: context.previous(),
            endToken: inheritClassName,
            className: inheritClassName,
        } as InheritStatementNode;
    }

    // Parse class members
    while (!context.isAtEnd()) {
        context.skipWhitespace(); // Skip whitespace before checking for class members

        if (context.check(TokenType.COLON)) {
            if (context.checkNext(TokenType.DECLARE)) {
                // Class field declaration
                context.advance(); // consume ':'
                context.advance(); // consume 'DECLARE'
                const identifiers = context.parseIdentifierList();
                context.consume(TokenType.SEMICOLON, "Expected ';' after class field declaration");
                members.push({
                    kind: ASTNodeType.ClassFieldDeclaration,
                    startToken: context.previous(),
                    endToken: context.previous(),
                    identifiers,
                });
            } else if (context.checkNext(TokenType.PROCEDURE)) {
                // Method declaration
                context.advance(); // consume ':'
                context.advance(); // consume 'PROCEDURE'
                const procedure = parseProcedureStatement(context as any);
                members.push({
                    kind: ASTNodeType.MethodDeclaration,
                    startToken: procedure.startToken,
                    endToken: procedure.endToken,
                    procedure,
                });
            } else {
                break;
            }
        } else {
            break;
        }
    }

    const endToken = context.previous();
    return {
        kind: ASTNodeType.ClassDefinition,
        startToken,
        endToken,
        declaration,
        inherit,
        members,
    };
}
