/**
 * SSL Main Formatter Tests
 *
 * Test suite for the main SSL formatter orchestration
 */

import { SSLFormatter, formatSSL } from "../../src/formatter/index";
import { FormatterOptions, defaultFormatterOptions } from "../../src/formatter/options";
import { ASTNodeType, createBaseNode, ProgramNode } from "../../src/parser/ast";
import {
    TryBlockNode,
    CatchBlockNode,
    FinallyBlockNode,
    EndTryStatementNode,
} from "../../src/parser/ast/errorHandling";
import { createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("SSLFormatter", () => {
    let formatter: SSLFormatter;
    let options: FormatterOptions;
    let mockToken: any;

    beforeEach(() => {
        options = { ...defaultFormatterOptions };
        formatter = new SSLFormatter(options);
        mockToken = createToken(TokenType.IDENTIFIER, "test", createPosition(1, 1, 0));
    });

    describe("constructor", () => {
        it("should initialize with default options", () => {
            const defaultFormatter = new SSLFormatter();
            expect(defaultFormatter).toBeDefined();
        });

        it("should initialize with custom options", () => {
            const customOptions = {
                ...defaultFormatterOptions,
                indentSize: 2,
                useTabs: true,
            };
            const customFormatter = new SSLFormatter(customOptions);
            expect(customFormatter).toBeDefined();
        });
    });

    describe("format method", () => {
        it("should format a simple program node", () => {
            const tryBlock: TryBlockNode = {
                kind: ASTNodeType.TryBlock,
                startToken: mockToken,
                endToken: mockToken,
                tryStatements: [],
            };

            const program: ProgramNode = {
                kind: ASTNodeType.Program,
                startToken: mockToken,
                endToken: mockToken,
                body: [tryBlock],
            };

            const result = formatter.format(program);
            expect(typeof result).toBe("string");
        });

        it("should handle empty program", () => {
            const program: ProgramNode = {
                kind: ASTNodeType.Program,
                startToken: mockToken,
                endToken: mockToken,
                body: [],
            };

            const result = formatter.format(program);
            expect(result).toBe("");
        });

        it("should format program with multiple statements", () => {
            const tryBlock1: TryBlockNode = {
                kind: ASTNodeType.TryBlock,
                startToken: mockToken,
                endToken: mockToken,
                tryStatements: [],
            };

            const catchBlock: CatchBlockNode = {
                kind: ASTNodeType.CatchBlock,
                startToken: mockToken,
                endToken: mockToken,
                statements: [],
            };

            const program: ProgramNode = {
                kind: ASTNodeType.Program,
                startToken: mockToken,
                endToken: mockToken,
                body: [tryBlock1, catchBlock],
            };

            const result = formatter.format(program);
            expect(typeof result).toBe("string");
            expect(result.length).toBeGreaterThan(0);
        });
    });

    describe("visitor routing", () => {
        it("should route error handling nodes to error handling visitor", () => {
            const tryBlock: TryBlockNode = {
                kind: ASTNodeType.TryBlock,
                startToken: mockToken,
                endToken: mockToken,
                tryStatements: [],
            };

            const result = formatter.format(tryBlock);
            expect(typeof result).toBe("string");
        });

        it("should handle unknown node types gracefully", () => {
            const unknownNode = {
                kind: "UnknownType" as any,
                startToken: mockToken,
                endToken: mockToken,
                token: mockToken,
            };

            const result = formatter.format(unknownNode);
            expect(typeof result).toBe("string");
        });
    });

    describe("convenience function", () => {
        it("should format using convenience function", () => {
            const tryBlock: TryBlockNode = {
                kind: ASTNodeType.TryBlock,
                startToken: mockToken,
                endToken: mockToken,
                tryStatements: [],
            };

            const result = formatSSL(tryBlock);
            expect(typeof result).toBe("string");
        });

        it("should format with custom options using convenience function", () => {
            const customOptions = {
                ...defaultFormatterOptions,
                indentSize: 2,
            };

            const tryBlock: TryBlockNode = {
                kind: ASTNodeType.TryBlock,
                startToken: mockToken,
                endToken: mockToken,
                tryStatements: [],
            };

            const result = formatSSL(tryBlock, customOptions);
            expect(typeof result).toBe("string");
        });
    });

    describe("node type classification", () => {
        it("should correctly identify error handling nodes", () => {
            const errorNodes = [
                { kind: ASTNodeType.TryBlock },
                { kind: ASTNodeType.CatchBlock },
                { kind: ASTNodeType.FinallyBlock },
                { kind: ASTNodeType.EndTryStatement },
            ];

            errorNodes.forEach((node) => {
                const testNode = {
                    ...node,
                    startToken: mockToken,
                    endToken: mockToken,
                };

                // This should route to error handling visitor without errors
                expect(() => formatter.format(testNode)).not.toThrow();
            });
        });
        it("should correctly identify control flow nodes", () => {
            const controlFlowTypes = [
                ASTNodeType.IfStatement,
                ASTNodeType.ElseStatement,
                ASTNodeType.EndIfStatement,
                ASTNodeType.WhileStatement,
                ASTNodeType.ForStatement,
            ];

            controlFlowTypes.forEach((kind) => {
                const testNode = {
                    kind,
                    startToken: mockToken,
                    endToken: mockToken,
                };

                // This should route to control flow visitor without errors
                expect(() => formatter.format(testNode)).not.toThrow();
            });
        });
        it("should correctly identify comment nodes", () => {
            const commentTypes = [
                ASTNodeType.CommentStatement,
                ASTNodeType.BlockComment,
                ASTNodeType.SingleLineComment,
                ASTNodeType.RegionComment,
            ];

            commentTypes.forEach((kind) => {
                const testNode = {
                    kind,
                    startToken: mockToken,
                    endToken: mockToken,
                };

                // This should route to comment visitor without errors
                expect(() => formatter.format(testNode)).not.toThrow();
            });
        });
    });

    describe("output management", () => {
        it("should provide access to formatted output", () => {
            const tryBlock: TryBlockNode = {
                kind: ASTNodeType.TryBlock,
                startToken: mockToken,
                endToken: mockToken,
                tryStatements: [],
            };

            formatter.format(tryBlock);
            const output = formatter.getFormattedOutput();
            expect(typeof output).toBe("string");
        });
    });

    describe("integration", () => {
        it("should work with error handling formatter", () => {
            const tryBlock: TryBlockNode = {
                kind: ASTNodeType.TryBlock,
                startToken: mockToken,
                endToken: mockToken,
                tryStatements: [],
            };

            const catchBlock: CatchBlockNode = {
                kind: ASTNodeType.CatchBlock,
                startToken: mockToken,
                endToken: mockToken,
                statements: [],
            };

            const finallyBlock: FinallyBlockNode = {
                kind: ASTNodeType.FinallyBlock,
                startToken: mockToken,
                endToken: mockToken,
                statements: [],
            };

            const endTry: EndTryStatementNode = {
                kind: ASTNodeType.EndTryStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            // Format each part individually
            const tryResult = formatter.format(tryBlock);
            const catchResult = formatter.format(catchBlock);
            const finallyResult = formatter.format(finallyBlock);
            const endTryResult = formatter.format(endTry);

            // All should produce string output
            expect(typeof tryResult).toBe("string");
            expect(typeof catchResult).toBe("string");
            expect(typeof finallyResult).toBe("string");
            expect(typeof endTryResult).toBe("string");
        });

        it("should handle complex nested program structure", () => {
            const innerTryBlock: TryBlockNode = {
                kind: ASTNodeType.TryBlock,
                startToken: mockToken,
                endToken: mockToken,
                tryStatements: [],
            };

            const program: ProgramNode = {
                kind: ASTNodeType.Program,
                startToken: mockToken,
                endToken: mockToken,
                body: [innerTryBlock],
            };

            const result = formatter.format(program);
            expect(typeof result).toBe("string");
        });
    });
});
