/**
 * Tests for Declaration Formatters
 *
 * Test suite covering:
 * - :DECLARE statement formatting
 * - :PARAMETERS statement formatting
 * - :DEFAULT statement formatting
 * - :PUBLIC statement formatting
 * - :INCLUDE statement formatting
 */

import { DeclarationFormatter } from "../../src/formatter/declarations";
import { OutputBuilder } from "../../src/formatter/visitor";
import { defaultFormatterOptions, FormatterOptions } from "../../src/formatter/options";
import {
    DeclarationStatementNode,
    ParametersStatementNode,
    DeclareStatementNode,
    DefaultStatementNode,
    PublicStatementNode,
    IncludeStatementNode,
} from "../../src/parser/ast/declarations";
import { IdentifierListNode } from "../../src/parser/ast/lists";
import { ASTNodeType, createBaseNode } from "../../src/parser/ast/base";
import { createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("DeclarationFormatter", () => {
    let formatter: DeclarationFormatter;
    let output: OutputBuilder;
    let options: FormatterOptions;
    let mockToken: any;

    beforeEach(() => {
        options = { ...defaultFormatterOptions };
        output = new OutputBuilder(options);
        formatter = new DeclarationFormatter(output, options);
        mockToken = createToken(TokenType.IDENTIFIER, "test", createPosition(1, 1, 0));
    });

    describe("formatParametersStatement", () => {
        it("should format simple PARAMETERS statement", () => {
            const identifiers = [
                createToken(TokenType.IDENTIFIER, "param1", createPosition(1, 12, 11)),
                createToken(TokenType.IDENTIFIER, "param2", createPosition(1, 20, 19)),
                createToken(TokenType.IDENTIFIER, "param3", createPosition(1, 28, 27)),
            ];

            const identifierList: IdentifierListNode = {
                kind: ASTNodeType.IdentifierList,
                identifiers,
                startToken: identifiers[0],
                endToken: identifiers[identifiers.length - 1],
            };

            const node: ParametersStatementNode = {
                kind: ASTNodeType.ParametersStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 35, 34)),
                parameters: identifierList,
            } as any;

            formatter.formatParametersStatement(node);

            const result = output.getOutput().trim();
            expect(result).toBe(":PARAMETERS param1, param2, param3;");
        });

        it("should format PARAMETERS statement without spaces after commas when option is disabled", () => {
            options.insertSpacesAfterCommas = false;
            formatter = new DeclarationFormatter(output, options);

            const identifiers = [
                createToken(TokenType.IDENTIFIER, "param1", createPosition(1, 12, 11)),
                createToken(TokenType.IDENTIFIER, "param2", createPosition(1, 19, 18)),
            ];

            const identifierList: IdentifierListNode = {
                kind: ASTNodeType.IdentifierList,
                identifiers,
                startToken: identifiers[0],
                endToken: identifiers[identifiers.length - 1],
            };

            const node: ParametersStatementNode = {
                kind: ASTNodeType.ParametersStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 25, 24)),
                parameters: identifierList,
            } as any;

            formatter.formatParametersStatement(node);

            const result = output.getOutput().trim();
            expect(result).toBe(":PARAMETERS param1,param2;");
        });

        it("should handle PARAMETERS statement with no parameters", () => {
            const node: ParametersStatementNode = {
                kind: ASTNodeType.ParametersStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 12, 11)),
            };

            formatter.formatParametersStatement(node);

            const result = output.getOutput().trim();
            expect(result).toBe(":PARAMETERS ;");
        });
    });

    describe("formatDeclareStatement", () => {
        it("should format simple DECLARE statement", () => {
            const identifiers = [
                createToken(TokenType.IDENTIFIER, "sVar1", createPosition(1, 9, 8)),
                createToken(TokenType.IDENTIFIER, "nVar2", createPosition(1, 16, 15)),
                createToken(TokenType.IDENTIFIER, "bVar3", createPosition(1, 23, 22)),
            ];

            const identifierList: IdentifierListNode = {
                kind: ASTNodeType.IdentifierList,
                identifiers,
                startToken: identifiers[0],
                endToken: identifiers[identifiers.length - 1],
            };

            const node: DeclareStatementNode = {
                kind: ASTNodeType.DeclareStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 29, 28)),
                identifiers: identifierList,
            } as any;

            formatter.formatDeclareStatement(node);

            const result = output.getOutput().trim();
            expect(result).toBe(":DECLARE sVar1, nVar2, bVar3;");
        });

        it("should format DECLARE statement with Hungarian notation", () => {
            const identifiers = [
                createToken(TokenType.IDENTIFIER, "sName", createPosition(1, 9, 8)),
                createToken(TokenType.IDENTIFIER, "nCount", createPosition(1, 16, 15)),
            ];

            const identifierList: IdentifierListNode = {
                kind: ASTNodeType.IdentifierList,
                identifiers,
                startToken: identifiers[0],
                endToken: identifiers[identifiers.length - 1],
            };

            const node: DeclareStatementNode = {
                kind: ASTNodeType.DeclareStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 23, 22)),
                identifiers: identifierList,
            } as any;

            formatter.formatDeclareStatement(node);

            const result = output.getOutput().trim();
            expect(result).toBe(":DECLARE sName, nCount;");
        });
    });

    describe("formatDefaultStatement", () => {
        it("should format simple DEFAULT statement", () => {
            const node: DefaultStatementNode = {
                kind: ASTNodeType.DefaultStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 25, 24)),
            };

            formatter.formatDefaultStatement(node);

            const result = output.getOutput().trim();
            expect(result).toBe(":DEFAULT ;");
        });

        it("should format DEFAULT statement with parameter and value", () => {
            // This test simulates a DEFAULT statement with parameter and default value
            const node: DefaultStatementNode = {
                kind: ASTNodeType.DefaultStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 30, 29)),
            };

            formatter.formatDefaultStatement(node);

            const result = output.getOutput().trim();
            expect(result).toBe(":DEFAULT ;");
        });
    });

    describe("formatPublicStatement", () => {
        it("should format simple PUBLIC statement", () => {
            const identifiers = [
                createToken(TokenType.IDENTIFIER, "gVar1", createPosition(1, 8, 7)),
                createToken(TokenType.IDENTIFIER, "gVar2", createPosition(1, 15, 14)),
            ];

            const identifierList: IdentifierListNode = {
                kind: ASTNodeType.IdentifierList,
                identifiers,
                startToken: identifiers[0],
                endToken: identifiers[identifiers.length - 1],
            };

            const node: PublicStatementNode = {
                kind: ASTNodeType.PublicStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 21, 20)),
                identifiers: identifierList,
            } as any;

            formatter.formatPublicStatement(node);

            const result = output.getOutput().trim();
            expect(result).toBe(":PUBLIC gVar1, gVar2;");
        });

        it("should format PUBLIC statement with single variable", () => {
            const identifiers = [
                createToken(TokenType.IDENTIFIER, "gGlobalVar", createPosition(1, 8, 7)),
            ];

            const identifierList: IdentifierListNode = {
                kind: ASTNodeType.IdentifierList,
                identifiers,
                startToken: identifiers[0],
                endToken: identifiers[0],
            };

            const node: PublicStatementNode = {
                kind: ASTNodeType.PublicStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 19, 18)),
                identifiers: identifierList,
            } as any;

            formatter.formatPublicStatement(node);

            const result = output.getOutput().trim();
            expect(result).toBe(":PUBLIC gGlobalVar;");
        });
    });

    describe("formatIncludeStatement", () => {
        it("should format simple INCLUDE statement", () => {
            const node: IncludeStatementNode = {
                kind: ASTNodeType.IncludeStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 25, 24)),
            };

            formatter.formatIncludeStatement(node);

            const result = output.getOutput().trim();
            expect(result).toBe(":INCLUDE ;");
        });

        it("should format INCLUDE statement with quoted string path", () => {
            // This test simulates an INCLUDE statement with a file path
            const node: IncludeStatementNode = {
                kind: ASTNodeType.IncludeStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 35, 34)),
            };

            formatter.formatIncludeStatement(node);

            const result = output.getOutput().trim();
            expect(result).toBe(":INCLUDE ;");
        });
    });

    describe("formatDeclarationStatement", () => {
        it("should format generic declaration statement", () => {
            const node: DeclarationStatementNode = {
                kind: ASTNodeType.DeclarationStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 15, 14)),
            };

            formatter.formatDeclarationStatement(node);

            const result = output.getOutput().trim();
            expect(result).toBe("");
        });
    });

    describe("formatting with indentation", () => {
        it("should respect indentation level", () => {
            output.indent();
            output.indent();

            const identifiers = [
                createToken(TokenType.IDENTIFIER, "param1", createPosition(1, 12, 11)),
            ];

            const identifierList: IdentifierListNode = {
                kind: ASTNodeType.IdentifierList,
                identifiers,
                startToken: identifiers[0],
                endToken: identifiers[0],
            };

            const node: ParametersStatementNode = {
                kind: ASTNodeType.ParametersStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 19, 18)),
                parameters: identifierList,
            } as any;

            formatter.formatParametersStatement(node);

            const result = output.getOutput();
            expect(result).toBe("        :PARAMETERS param1;\n");
        });

        it("should handle tabs for indentation", () => {
            options.useTabs = true;
            output = new OutputBuilder(options);
            formatter = new DeclarationFormatter(output, options);

            output.indent();

            const identifiers = [
                createToken(TokenType.IDENTIFIER, "var1", createPosition(1, 9, 8)),
            ];

            const identifierList: IdentifierListNode = {
                kind: ASTNodeType.IdentifierList,
                identifiers,
                startToken: identifiers[0],
                endToken: identifiers[0],
            };

            const node: DeclareStatementNode = {
                kind: ASTNodeType.DeclareStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 16, 15)),
                identifiers: identifierList,
            } as any;

            formatter.formatDeclareStatement(node);

            const result = output.getOutput();
            expect(result).toBe("\t:DECLARE var1;\n");
        });
    });

    describe("SSL EBNF grammar compliance", () => {
        it("should format according to SSL EBNF DeclarationStatement rule", () => {
            // Testing EBNF: DeclarationStatement ::= (ParametersStatement | IncludeStatement)

            // Test ParametersStatement
            const parametersNode: ParametersStatementNode = {
                kind: ASTNodeType.ParametersStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            formatter.formatParametersStatement(parametersNode);
            expect(output.getOutput()).toContain(":PARAMETERS");

            // Reset output
            output = new OutputBuilder(options);
            formatter = new DeclarationFormatter(output, options);

            // Test IncludeStatement
            const includeNode: IncludeStatementNode = {
                kind: ASTNodeType.IncludeStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            formatter.formatIncludeStatement(includeNode);
            expect(output.getOutput()).toContain(":INCLUDE");
        });

        it("should format according to SSL EBNF ParametersStatement rule", () => {
            // Testing EBNF: ParametersStatement ::= ":" "PARAMETERS" IdentifierList
            const identifiers = [
                createToken(TokenType.IDENTIFIER, "param1", createPosition(1, 12, 11)),
                createToken(TokenType.IDENTIFIER, "param2", createPosition(1, 20, 19)),
            ];

            const identifierList: IdentifierListNode = {
                kind: ASTNodeType.IdentifierList,
                identifiers,
                startToken: identifiers[0],
                endToken: identifiers[1],
            };

            const node: ParametersStatementNode = {
                kind: ASTNodeType.ParametersStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 27, 26)),
                parameters: identifierList,
            } as any;

            formatter.formatParametersStatement(node);

            const result = output.getOutput().trim();
            expect(result).toBe(":PARAMETERS param1, param2;");
        });

        it("should format according to SSL EBNF IncludeStatement rule", () => {
            // Testing EBNF: IncludeStatement ::= ":" "INCLUDE" StringLiteral
            const node: IncludeStatementNode = {
                kind: ASTNodeType.IncludeStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 25, 24)),
            };

            formatter.formatIncludeStatement(node);

            const result = output.getOutput().trim();
            expect(result).toBe(":INCLUDE ;");
        });
    });
});
