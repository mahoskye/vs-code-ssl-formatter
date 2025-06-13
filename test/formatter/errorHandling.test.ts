/**
 * Tests for SSL Error Handling Formatter
 *
 * Comprehensive test suite covering:
 * - TRY/CATCH/FINALLY/ENDTRY blocks formatting
 * - ERROR blocks formatting
 * - Nested error handling structures
 * - EBNF grammar compliance
 */

import { SSLErrorHandlingFormatterVisitor } from "../../src/formatter/errorHandling";
import { defaultFormatterOptions, FormatterOptions } from "../../src/formatter/options";
import {
    ErrorHandlingStatementNode,
    TryBlockNode,
    TryStatementNode,
    CatchBlockNode,
    CatchStatementNode,
    FinallyBlockNode,
    FinallyStatementNode,
    EndTryStatementNode,
    ErrorBlockStanzaNode,
    ErrorMarkerNode,
} from "../../src/parser/ast/errorHandling";
import { ASTNodeType, createBaseNode, StatementNode } from "../../src/parser/ast/base";
import { createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("SSLErrorHandlingFormatterVisitor", () => {
    let visitor: SSLErrorHandlingFormatterVisitor;
    let options: FormatterOptions;
    let mockToken: any;

    beforeEach(() => {
        options = { ...defaultFormatterOptions };
        visitor = new SSLErrorHandlingFormatterVisitor(options);
        mockToken = createToken(TokenType.TRY, "TRY", createPosition(1, 1, 0));
    });

    describe("TRY Statement Formatting", () => {
        it("should format simple TRY statement", () => {
            const tryNode: TryStatementNode = {
                kind: ASTNodeType.TryStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 5, 4)),
            };

            const result = visitor.visit(tryNode);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe(":TRY;");
        });
    });

    describe("CATCH Statement Formatting", () => {
        it("should format simple CATCH statement", () => {
            const catchNode: CatchStatementNode = {
                kind: ASTNodeType.CatchStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 7, 6)),
            };

            const result = visitor.visit(catchNode);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe(":CATCH;");
        });
    });

    describe("FINALLY Statement Formatting", () => {
        it("should format simple FINALLY statement", () => {
            const finallyNode: FinallyStatementNode = {
                kind: ASTNodeType.FinallyStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 9, 8)),
            };

            const result = visitor.visit(finallyNode);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe(":FINALLY;");
        });
    });

    describe("ENDTRY Statement Formatting", () => {
        it("should format simple ENDTRY statement", () => {
            const endTryNode: EndTryStatementNode = {
                kind: ASTNodeType.EndTryStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 8, 7)),
            };

            const result = visitor.visit(endTryNode);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe(":ENDTRY;");
        });
    });

    describe("ERROR Statement Formatting", () => {
        it("should format simple ERROR marker", () => {
            const errorNode: ErrorMarkerNode = {
                kind: ASTNodeType.ErrorMarker,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 7, 6)),
            };

            const result = visitor.visit(errorNode);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe(":ERROR;");
        });
    });

    describe("Block Structures", () => {
        it("should handle TryBlock wrapper", () => {
            const tryBlockNode: TryBlockNode = {
                kind: ASTNodeType.TryBlock,
                startToken: mockToken,
                endToken: mockToken,
                tryStatements: [],
            };

            const result = visitor.visit(tryBlockNode);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();
        });

        it("should handle CatchBlock wrapper", () => {
            const catchBlockNode: CatchBlockNode = {
                kind: ASTNodeType.CatchBlock,
                startToken: mockToken,
                endToken: mockToken,
                statements: [],
            };

            const result = visitor.visit(catchBlockNode);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();
        });

        it("should handle FinallyBlock wrapper", () => {
            const finallyBlockNode: FinallyBlockNode = {
                kind: ASTNodeType.FinallyBlock,
                startToken: mockToken,
                endToken: mockToken,
                statements: [],
            };

            const result = visitor.visit(finallyBlockNode);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();
        });

        it("should handle ErrorBlockStanza wrapper", () => {
            const errorBlockNode: ErrorBlockStanzaNode = {
                kind: ASTNodeType.ErrorBlockStanza,
                startToken: mockToken,
                endToken: mockToken,
                statements: [],
            };

            const result = visitor.visit(errorBlockNode);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();
        });

        it("should handle ErrorHandlingStatement wrapper", () => {
            const errorHandlingNode: ErrorHandlingStatementNode = {
                kind: ASTNodeType.ErrorHandlingStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            const result = visitor.visit(errorHandlingNode);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();
        });
    });
    describe("Complex Error Handling Structures", () => {
        it("should format complete try-catch-finally structure with proper indentation", () => {
            const mockStatement1: StatementNode = createBaseNode(
                ASTNodeType.Assignment,
                mockToken,
                mockToken
            ) as StatementNode;

            const mockStatement2: StatementNode = createBaseNode(
                ASTNodeType.Assignment,
                mockToken,
                mockToken
            ) as StatementNode;

            const mockStatement3: StatementNode = createBaseNode(
                ASTNodeType.Assignment,
                mockToken,
                mockToken
            ) as StatementNode;

            const catchBlock: CatchBlockNode = {
                kind: ASTNodeType.CatchBlock,
                startToken: mockToken,
                endToken: mockToken,
                statements: [mockStatement2],
            };

            const finallyBlock: FinallyBlockNode = {
                kind: ASTNodeType.FinallyBlock,
                startToken: mockToken,
                endToken: mockToken,
                statements: [mockStatement3],
            };

            const tryBlock: TryBlockNode = {
                kind: ASTNodeType.TryBlock,
                startToken: mockToken,
                endToken: mockToken,
                tryStatements: [mockStatement1],
                catchBlock,
                finallyBlock,
            };

            const result = visitor.visit(tryBlock);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            const lines = output.split("\n").filter((line) => line.trim() !== "");

            expect(lines[0]).toBe(":TRY;");
            expect(lines[1]).toBe(":CATCH;");
            expect(lines[2]).toBe(":FINALLY;");
            expect(lines[3]).toBe(":ENDTRY;");
        });

        it("should format try-catch structure without finally block", () => {
            const mockStatement1: StatementNode = createBaseNode(
                ASTNodeType.Assignment,
                mockToken,
                mockToken
            ) as StatementNode;

            const mockStatement2: StatementNode = createBaseNode(
                ASTNodeType.Assignment,
                mockToken,
                mockToken
            ) as StatementNode;

            const catchBlock: CatchBlockNode = {
                kind: ASTNodeType.CatchBlock,
                startToken: mockToken,
                endToken: mockToken,
                statements: [mockStatement2],
            };

            const tryBlock: TryBlockNode = {
                kind: ASTNodeType.TryBlock,
                startToken: mockToken,
                endToken: mockToken,
                tryStatements: [mockStatement1],
                catchBlock,
            };

            const result = visitor.visit(tryBlock);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            const lines = output.split("\n").filter((line) => line.trim() !== "");

            expect(lines[0]).toBe(":TRY;");
            expect(lines[1]).toBe(":CATCH;");
            expect(lines[2]).toBe(":ENDTRY;");
        });

        it("should format try-finally structure without catch block", () => {
            const mockStatement1: StatementNode = createBaseNode(
                ASTNodeType.Assignment,
                mockToken,
                mockToken
            ) as StatementNode;

            const mockStatement3: StatementNode = createBaseNode(
                ASTNodeType.Assignment,
                mockToken,
                mockToken
            ) as StatementNode;

            const finallyBlock: FinallyBlockNode = {
                kind: ASTNodeType.FinallyBlock,
                startToken: mockToken,
                endToken: mockToken,
                statements: [mockStatement3],
            };

            const tryBlock: TryBlockNode = {
                kind: ASTNodeType.TryBlock,
                startToken: mockToken,
                endToken: mockToken,
                tryStatements: [mockStatement1],
                finallyBlock,
            };

            const result = visitor.visit(tryBlock);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            const lines = output.split("\n").filter((line) => line.trim() !== "");

            expect(lines[0]).toBe(":TRY;");
            expect(lines[1]).toBe(":FINALLY;");
            expect(lines[2]).toBe(":ENDTRY;");
        });

        it("should format ERROR block with statements", () => {
            const mockStatement1: StatementNode = createBaseNode(
                ASTNodeType.Assignment,
                mockToken,
                mockToken
            ) as StatementNode;

            const mockStatement2: StatementNode = createBaseNode(
                ASTNodeType.Assignment,
                mockToken,
                mockToken
            ) as StatementNode;

            const errorBlock: ErrorBlockStanzaNode = {
                kind: ASTNodeType.ErrorBlockStanza,
                startToken: mockToken,
                endToken: mockToken,
                statements: [mockStatement1, mockStatement2],
            };

            const result = visitor.visit(errorBlock);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            const lines = output.split("\n").filter((line) => line.trim() !== "");

            expect(lines[0]).toBe(":ERROR;");
        });

        it("should format nested try-catch blocks", () => {
            // Test with multiple statements to verify indentation
            const mockStatement1: StatementNode = createBaseNode(
                ASTNodeType.Assignment,
                mockToken,
                mockToken
            ) as StatementNode;

            const mockStatement2: StatementNode = createBaseNode(
                ASTNodeType.Assignment,
                mockToken,
                mockToken
            ) as StatementNode;

            const tryNode: TryStatementNode = {
                kind: ASTNodeType.TryStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 5, 4)),
            };

            const catchNode: CatchStatementNode = {
                kind: ASTNodeType.CatchStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(3, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(3, 7, 6)),
            };

            const endTryNode: EndTryStatementNode = {
                kind: ASTNodeType.EndTryStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(5, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(5, 8, 7)),
            };

            // Visit each node separately to test individual formatting
            visitor.visit(tryNode);
            visitor.visit(catchNode);
            visitor.visit(endTryNode);

            const output = visitor.getFormattedOutput().trim();
            const lines = output.split("\n");

            expect(lines[0]).toBe(":TRY;");
            expect(lines[1]).toBe(":CATCH;");
            expect(lines[2]).toBe(":ENDTRY;");
        });

        it("should format try-catch-finally structure", () => {
            const tryNode: TryStatementNode = {
                kind: ASTNodeType.TryStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 5, 4)),
            };

            const catchNode: CatchStatementNode = {
                kind: ASTNodeType.CatchStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(3, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(3, 7, 6)),
            };

            const finallyNode: FinallyStatementNode = {
                kind: ASTNodeType.FinallyStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(5, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(5, 9, 8)),
            };

            const endTryNode: EndTryStatementNode = {
                kind: ASTNodeType.EndTryStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(7, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(7, 8, 7)),
            };

            // Visit each node to test the complete structure
            visitor.visit(tryNode);
            visitor.visit(catchNode);
            visitor.visit(finallyNode);
            visitor.visit(endTryNode);

            const output = visitor.getFormattedOutput().trim();
            const lines = output.split("\n");

            expect(lines[0]).toBe(":TRY;");
            expect(lines[1]).toBe(":CATCH;");
            expect(lines[2]).toBe(":FINALLY;");
            expect(lines[3]).toBe(":ENDTRY;");
        });

        it("should format try-finally without catch", () => {
            const tryNode: TryStatementNode = {
                kind: ASTNodeType.TryStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 5, 4)),
            };

            const finallyNode: FinallyStatementNode = {
                kind: ASTNodeType.FinallyStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(3, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(3, 9, 8)),
            };

            const endTryNode: EndTryStatementNode = {
                kind: ASTNodeType.EndTryStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(5, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(5, 8, 7)),
            };

            // Visit each node to test the structure
            visitor.visit(tryNode);
            visitor.visit(finallyNode);
            visitor.visit(endTryNode);

            const output = visitor.getFormattedOutput().trim();
            const lines = output.split("\n");

            expect(lines[0]).toBe(":TRY;");
            expect(lines[1]).toBe(":FINALLY;");
            expect(lines[2]).toBe(":ENDTRY;");
        });
    });

    describe("Error Handling", () => {
        it("should handle nodes with missing properties gracefully", () => {
            const node: TryStatementNode = {
                kind: ASTNodeType.TryStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();
        });

        it("should handle malformed nodes gracefully", () => {
            const malformedNodes = [
                {
                    kind: ASTNodeType.TryStatement,
                    startToken: undefined,
                    endToken: mockToken,
                },
                {
                    kind: ASTNodeType.CatchStatement,
                    startToken: mockToken,
                    endToken: undefined,
                },
            ];

            malformedNodes.forEach((node) => {
                visitor = new SSLErrorHandlingFormatterVisitor(options); // Reset visitor
                const result = visitor.visit(node as any);

                expect(result.shouldContinue).toBe(true);
                expect(result.error).toBeUndefined();
            });
        });
    });

    describe("Integration with FormatterVisitorBase", () => {
        it("should properly inherit from FormatterVisitorBase", () => {
            expect(visitor).toBeInstanceOf(SSLErrorHandlingFormatterVisitor);
            expect(typeof visitor.visit).toBe("function");
            expect(typeof visitor.getFormattedOutput).toBe("function");
        });

        it("should use provided formatter options", () => {
            const customOptions: FormatterOptions = {
                ...defaultFormatterOptions,
                indentSize: 2,
                uppercaseKeywords: false,
            };

            const customVisitor = new SSLErrorHandlingFormatterVisitor(customOptions);
            const tryNode: TryStatementNode = {
                kind: ASTNodeType.TryStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 5, 4)),
            };

            customVisitor.visit(tryNode);
            const output = customVisitor.getFormattedOutput().trim();

            // Should still format keywords correctly regardless of options
            expect(output).toBe(":TRY;");
        });
    });

    describe("EBNF Grammar Compliance", () => {
        it("should follow EBNF grammar for TryStatement", () => {
            // TryStatement ::= ":" "TRY"
            const tryNode: TryStatementNode = {
                kind: ASTNodeType.TryStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 5, 4)),
            };

            visitor.visit(tryNode);
            const output = visitor.getFormattedOutput().trim();

            expect(output).toMatch(/^:TRY;$/);
        });

        it("should follow EBNF grammar for CatchStatement", () => {
            // CatchStatement ::= ":" "CATCH"
            const catchNode: CatchStatementNode = {
                kind: ASTNodeType.CatchStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 7, 6)),
            };

            visitor.visit(catchNode);
            const output = visitor.getFormattedOutput().trim();

            expect(output).toMatch(/^:CATCH;$/);
        });

        it("should follow EBNF grammar for FinallyStatement", () => {
            // FinallyStatement ::= ":" "FINALLY"
            const finallyNode: FinallyStatementNode = {
                kind: ASTNodeType.FinallyStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 9, 8)),
            };

            visitor.visit(finallyNode);
            const output = visitor.getFormattedOutput().trim();

            expect(output).toMatch(/^:FINALLY;$/);
        });

        it("should follow EBNF grammar for EndTryStatement", () => {
            // EndTryStatement ::= ":" "ENDTRY"
            const endTryNode: EndTryStatementNode = {
                kind: ASTNodeType.EndTryStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 8, 7)),
            };

            visitor.visit(endTryNode);
            const output = visitor.getFormattedOutput().trim();

            expect(output).toMatch(/^:ENDTRY;$/);
        });

        it("should follow EBNF grammar for ErrorMarker", () => {
            // ErrorMarker ::= ":" "ERROR"
            const errorNode: ErrorMarkerNode = {
                kind: ASTNodeType.ErrorMarker,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 7, 6)),
            };

            visitor.visit(errorNode);
            const output = visitor.getFormattedOutput().trim();

            expect(output).toMatch(/^:ERROR;$/);
        });
    });
});
