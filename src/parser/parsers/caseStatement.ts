/**
 * Case/Switch statement parsing functions for SSL parser
 */

import { Token } from "../../tokenizer/token";
import { TokenType } from "../../tokenizer/tokenType";
import {
    StatementNode,
    ExpressionNode,
    SwitchStatementNode,
    CaseBlockNode,
    OtherwiseBlockNode,
    ASTNodeType,
} from "../ast";

/**
 * Parser interface for case statement parsers
 */
export interface CaseParser {
    match(...types: TokenType[]): boolean;
    advance(): Token;
    previous(): Token;
    peek(): Token;
    check(type: TokenType): boolean;
    checkNext(type: TokenType): boolean;
    consume(type: TokenType, message: string): Token;
    parseStatement(): StatementNode | null;
    parseExpression(): ExpressionNode;
    skipWhitespace(): void;
    error(message: string): void;
    isAtEnd(): boolean;
    createErrorNode(): ExpressionNode;
}

/**
 * Parse switch/case statement (BEGINCASE)
 */
export function parseSwitchStatement(parser: CaseParser): SwitchStatementNode {
    const startToken = parser.previous(); // BEGINCASE token

    const cases: CaseBlockNode[] = [];
    let otherwiseBlock: OtherwiseBlockNode | undefined = undefined;

    // Parse case blocks
    while (!parser.isAtEnd()) {
        parser.skipWhitespace();

        // Check for :CASE
        if (parser.check(TokenType.COLON) && parser.checkNext(TokenType.CASE)) {
            parser.advance(); // consume :
            parser.advance(); // consume CASE

            const caseStartToken = parser.previous();
            const values: ExpressionNode[] = [];

            // Parse case values (can be multiple values separated by commas)
            values.push(parser.parseExpression());
            while (parser.match(TokenType.COMMA)) {
                values.push(parser.parseExpression());
            }

            // Parse case body
            const body: StatementNode[] = [];
            while (!parser.isAtEnd()) {
                parser.skipWhitespace();

                // Check for next :CASE, :OTHERWISE, or :ENDCASE
                if (parser.check(TokenType.COLON)) {
                    if (
                        parser.checkNext(TokenType.CASE) ||
                        parser.checkNext(TokenType.OTHERWISE) ||
                        parser.checkNext(TokenType.ENDCASE)
                    ) {
                        break;
                    }
                }

                const stmt = parser.parseStatement();
                if (stmt) {
                    body.push(stmt);
                }
            }
            const caseEndToken = parser.previous();
            cases.push({
                kind: ASTNodeType.CaseBlock,
                startToken: caseStartToken,
                endToken: caseEndToken,
                condition: values[0], // Use first value as condition for now
                statements: body,
            } as CaseBlockNode);
        }
        // Check for :OTHERWISE
        else if (parser.check(TokenType.COLON) && parser.checkNext(TokenType.OTHERWISE)) {
            parser.advance(); // consume :
            parser.advance(); // consume OTHERWISE

            const otherwiseStartToken = parser.previous();
            const body: StatementNode[] = [];

            // Parse otherwise body
            while (!parser.isAtEnd()) {
                parser.skipWhitespace();

                // Check for :ENDCASE
                if (parser.check(TokenType.COLON) && parser.checkNext(TokenType.ENDCASE)) {
                    break;
                }

                const stmt = parser.parseStatement();
                if (stmt) {
                    body.push(stmt);
                }
            }

            const otherwiseEndToken = parser.previous();
            otherwiseBlock = {
                kind: ASTNodeType.OtherwiseBlock,
                startToken: otherwiseStartToken,
                endToken: otherwiseEndToken,
                statements: body,
            } as OtherwiseBlockNode;
        }
        // Check for :ENDCASE
        else if (parser.check(TokenType.COLON) && parser.checkNext(TokenType.ENDCASE)) {
            break;
        } else {
            // Skip unexpected tokens or break if we can't make progress
            parser.advance();
        }
    }

    // Consume :ENDCASE
    parser.consume(TokenType.COLON, "Expected ':' before ENDCASE");
    parser.consume(TokenType.ENDCASE, "Expected 'ENDCASE'");
    const endToken = parser.previous();
    return {
        kind: ASTNodeType.SwitchStatement,
        startToken,
        endToken,
        cases,
        otherwiseBlock,
    } as SwitchStatementNode;
}
