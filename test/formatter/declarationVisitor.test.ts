/**
 * Tests for SSL Declaration Formatter Visitor
 *
 * Integration tests for declaration statement formatting
 * through the visitor pattern
 */

import { SSLDeclarationFormatterVisitor } from "../../src/formatter/declarationVisitor";
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

describe("SSLDeclarationFormatterVisitor", () => {
    let visitor: SSLDeclarationFormatterVisitor;
    let options: FormatterOptions;
    let mockToken: any;

    beforeEach(() => {
        options = { ...defaultFormatterOptions };
        visitor = new SSLDeclarationFormatterVisitor(options);
        mockToken = createToken(TokenType.IDENTIFIER, "test", createPosition(1, 1, 0));
    });

    describe("Declaration Statement Integration", () => {
        it("should format PARAMETERS statement through visitor pattern", () => {
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

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe(":PARAMETERS param1, param2;");
        });

        it("should format DECLARE statement through visitor pattern", () => {
            const identifiers = [
                createToken(TokenType.IDENTIFIER, "sVar1", createPosition(1, 9, 8)),
                createToken(TokenType.IDENTIFIER, "nVar2", createPosition(1, 16, 15)),
            ];

            const identifierList: IdentifierListNode = {
                kind: ASTNodeType.IdentifierList,
                identifiers,
                startToken: identifiers[0],
                endToken: identifiers[1],
            };

            const node: DeclareStatementNode = {
                kind: ASTNodeType.DeclareStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 23, 22)),
                identifiers: identifierList,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe(":DECLARE sVar1, nVar2;");
        });

        it("should format PUBLIC statement through visitor pattern", () => {
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

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe(":PUBLIC gGlobalVar;");
        });

        it("should format INCLUDE statement through visitor pattern", () => {
            const node: IncludeStatementNode = {
                kind: ASTNodeType.IncludeStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 25, 24)),
            };

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe(":INCLUDE ;");
        });

        it("should format DEFAULT statement through visitor pattern", () => {
            const node: DefaultStatementNode = {
                kind: ASTNodeType.DefaultStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 25, 24)),
            };

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe(":DEFAULT ;");
        });
    });

    describe("Formatting Options Integration", () => {
        it("should respect insertSpacesAfterCommas option", () => {
            options.insertSpacesAfterCommas = false;
            visitor = new SSLDeclarationFormatterVisitor(options);

            const identifiers = [
                createToken(TokenType.IDENTIFIER, "param1", createPosition(1, 12, 11)),
                createToken(TokenType.IDENTIFIER, "param2", createPosition(1, 19, 18)),
                createToken(TokenType.IDENTIFIER, "param3", createPosition(1, 26, 25)),
            ];

            const identifierList: IdentifierListNode = {
                kind: ASTNodeType.IdentifierList,
                identifiers,
                startToken: identifiers[0],
                endToken: identifiers[2],
            };

            const node: DeclareStatementNode = {
                kind: ASTNodeType.DeclareStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 33, 32)),
                identifiers: identifierList,
            } as any;

            visitor.visit(node);

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe(":DECLARE param1,param2,param3;");
        });

        it("should respect indentation settings", () => {
            options.indentSize = 2;
            visitor = new SSLDeclarationFormatterVisitor(options);

            // Manually add indentation for testing
            (visitor as any).output.indent();

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

            visitor.visit(node);

            const output = visitor.getFormattedOutput();
            expect(output).toBe("  :DECLARE var1;\n");
        });

        it("should respect tab indentation", () => {
            options.useTabs = true;
            visitor = new SSLDeclarationFormatterVisitor(options);

            // Manually add indentation for testing
            (visitor as any).output.indent();

            const identifiers = [
                createToken(TokenType.IDENTIFIER, "var1", createPosition(1, 9, 8)),
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

            visitor.visit(node);

            const output = visitor.getFormattedOutput();
            expect(output).toBe("\t:PARAMETERS var1;\n");
        });
    });

    describe("SSL EBNF Grammar Compliance", () => {
        it("should handle all DeclarationStatement types from EBNF", () => {
            // Test all declaration statement types according to EBNF:
            // DeclarationStatement ::= (ParametersStatement | IncludeStatement)
            // DeclareStatement ::= ":" "DECLARE" IdentifierList
            // DefaultStatement ::= ":" "DEFAULT" DefaultParameterList
            // PublicStatement ::= ":" "PUBLIC" IdentifierList

            const declarationTypes = [
                ASTNodeType.ParametersStatement,
                ASTNodeType.DeclareStatement,
                ASTNodeType.DefaultStatement,
                ASTNodeType.PublicStatement,
                ASTNodeType.IncludeStatement,
            ];

            declarationTypes.forEach((nodeType) => {
                const node = createBaseNode(nodeType, mockToken, mockToken);
                const result = visitor.visit(node);

                expect(result.shouldContinue).toBe(true);
                expect(result.error).toBeUndefined();
            });
        });

        it("should format identifier lists according to EBNF", () => {
            // Test EBNF: IdentifierList ::= Identifier {"," Identifier}
            const testCases = [
                // Single identifier
                ["var1"],
                // Multiple identifiers
                ["var1", "var2", "var3"],
                // Hungarian notation
                ["sName", "nCount", "bIsValid"],
            ];

            testCases.forEach((identifierNames) => {
                const identifiers = identifierNames.map((name, index) =>
                    createToken(
                        TokenType.IDENTIFIER,
                        name,
                        createPosition(1, 9 + index * 7, 8 + index * 7)
                    )
                );

                const identifierList: IdentifierListNode = {
                    kind: ASTNodeType.IdentifierList,
                    identifiers,
                    startToken: identifiers[0],
                    endToken: identifiers[identifiers.length - 1],
                };

                const node: DeclareStatementNode = {
                    kind: ASTNodeType.DeclareStatement,
                    startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                    endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 50, 49)),
                    identifiers: identifierList,
                } as any;

                visitor = new SSLDeclarationFormatterVisitor(options); // Reset visitor
                visitor.visit(node);

                const output = visitor.getFormattedOutput().trim();
                const expected = `:DECLARE ${identifierNames.join(", ")};`;
                expect(output).toBe(expected);
            });
        });
    });

    describe("Error Handling", () => {
        it("should handle nodes with missing properties gracefully", () => {
            const node: ParametersStatementNode = {
                kind: ASTNodeType.ParametersStatement,
                startToken: mockToken,
                endToken: mockToken,
                // parameters property is missing
            };

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();
        });

        it("should handle empty identifier lists", () => {
            const identifierList: IdentifierListNode = {
                kind: ASTNodeType.IdentifierList,
                identifiers: [],
                startToken: mockToken,
                endToken: mockToken,
            };

            const node: DeclareStatementNode = {
                kind: ASTNodeType.DeclareStatement,
                startToken: mockToken,
                endToken: mockToken,
                identifiers: identifierList,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe(":DECLARE ;");
        });
    });
});
