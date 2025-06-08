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
 * Parse a dotted identifier (e.g., Research.clsVehicle)
 * Returns the concatenated identifier string
 */
function parseDottedIdentifier(context: ClassParserContext): string {
    let result = context.consume(TokenType.IDENTIFIER, "Expected identifier").value;

    // Check for dotted notation (namespace.classname)
    while (context.check(TokenType.DOT) && !context.isAtEnd()) {
        const dotToken = context.advance(); // consume dot
        if (context.check(TokenType.IDENTIFIER)) {
            const nextIdentifier = context.advance(); // consume next identifier
            result += dotToken.value + nextIdentifier.value;
        } else {
            // Put the dot back if it's not followed by an identifier
            // Note: This is a simplified approach - in a real parser we'd need proper backtracking
            break;
        }
    }

    return result;
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
    const className = context.consume(TokenType.IDENTIFIER, "Expected class name");
    context.consume(TokenType.SEMICOLON, "Expected ';' after class name");

    const declaration = {
        kind: ASTNodeType.ClassDeclaration,
        startToken: startToken,
        endToken: className,
        name: className,
    } as ClassDeclarationNode;

    let inherit = undefined;
    const members: any[] = []; // Skip whitespace before checking for inheritance
    context.skipWhitespace(); // Optional inherit statement
    if (context.check(TokenType.COLON) && context.checkNext(TokenType.INHERIT)) {
        const inheritStartToken = context.advance(); // consume ':'
        context.advance(); // consume 'INHERIT'
        const inheritClassName = parseDottedIdentifier(context);
        const semicolonToken = context.consume(
            TokenType.SEMICOLON,
            "Expected ';' after inherited class name"
        );

        // Create a token-like object for the inherited class name
        const inheritClassToken = {
            type: TokenType.IDENTIFIER,
            value: inheritClassName,
            range: {
                start: inheritStartToken.range.start,
                end: semicolonToken.range.start,
            },
        } as Token;

        inherit = {
            kind: ASTNodeType.InheritStatement,
            startToken: inheritStartToken,
            endToken: inheritClassToken,
            className: inheritClassToken,
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
