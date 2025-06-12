/**
 * SSL Comment Formatter Tests
 *
 * Comprehensive test suite for SSL comment formatting functionality:
 * - Block comments (multi-line /* comment ;)
 * - Single-line comments (/* comment ;)
 * - Region comments (/* region name ;)
 * - End region comments (/* endregion ;)
 * - Comment alignment and wrapping
 * - Comment preservation and formatting options
 */

import {
    CommentStatementNode,
    BlockCommentNode,
    SingleLineCommentNode,
    RegionCommentNode,
    EndRegionCommentNode,
} from "../../src/parser/ast/comments";
import { SSLCommentFormatterVisitor } from "../../src/formatter/comments";
import { FormatterOptions, defaultFormatterOptions } from "../../src/formatter/options";
import { ASTNodeType } from "../../src/parser/ast/base";
import { Token, createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("SSLCommentFormatterVisitor", () => {
    let visitor: SSLCommentFormatterVisitor;
    let options: FormatterOptions;
    let mockToken: Token;

    beforeEach(() => {
        options = { ...defaultFormatterOptions };
        visitor = new SSLCommentFormatterVisitor(options);
        mockToken = createToken(TokenType.BLOCK_COMMENT, "/* test ;", createPosition(1, 1, 0));
    });

    describe("Comment Statement Formatting", () => {
        it("should format comment statements (wrapper)", () => {
            const token = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* This is a test comment ;",
                createPosition(1, 1, 0)
            );

            const node: CommentStatementNode = {
                kind: ASTNodeType.CommentStatement,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();

            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("/* This is a test comment ;");
        });

        it("should handle comment statements without token", () => {
            const node: CommentStatementNode = {
                kind: ASTNodeType.CommentStatement,
                startToken: mockToken,
                endToken: mockToken,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();
        });
    });

    describe("Block Comment Formatting", () => {
        it("should format single-line block comments", () => {
            const token = createToken(
                TokenType.BLOCK_COMMENT,
                "/* This is a single line block comment ;",
                createPosition(1, 1, 0)
            );

            const node: BlockCommentNode = {
                kind: ASTNodeType.BlockComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("/* This is a single line block comment ;");
        });

        it("should format multi-line block comments with proper indentation", () => {
            const token = createToken(
                TokenType.BLOCK_COMMENT,
                "/* This is a\nmulti-line\nblock comment ;",
                createPosition(1, 1, 0)
            );

            const node: BlockCommentNode = {
                kind: ASTNodeType.BlockComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput();
            const lines = output.split("\n");

            expect(lines[0]).toBe("/* This is a");
            expect(lines[1]).toBe("multi-line");
            expect(lines[2]).toBe("block comment ;");
        });

        it("should preserve formatting when formatMultiLineComments is disabled", () => {
            const customOptions = { ...defaultFormatterOptions, formatMultiLineComments: false };
            visitor = new SSLCommentFormatterVisitor(customOptions);

            const token = createToken(
                TokenType.BLOCK_COMMENT,
                "/* Preserve\n    this   formatting ;",
                createPosition(1, 1, 0)
            );

            const node: BlockCommentNode = {
                kind: ASTNodeType.BlockComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            visitor.visit(node);
            const output = visitor.getFormattedOutput();

            expect(output).toBe("/* Preserve\n    this   formatting ;\n");
        });

        it("should handle empty block comments", () => {
            const token = createToken(TokenType.BLOCK_COMMENT, "/* ;", createPosition(1, 1, 0));

            const node: BlockCommentNode = {
                kind: ASTNodeType.BlockComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            visitor.visit(node);
            const output = visitor.getFormattedOutput().trim();

            expect(output).toBe("/* ;");
        });

        it("should format block comments with embedded newlines properly", () => {
            const token = createToken(
                TokenType.BLOCK_COMMENT,
                "/* Start\n   Middle line\n   Another line\n   End ;",
                createPosition(1, 1, 0)
            );

            const node: BlockCommentNode = {
                kind: ASTNodeType.BlockComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            visitor.visit(node);
            const output = visitor.getFormattedOutput();
            const lines = output.split("\n");

            expect(lines[0]).toBe("/* Start");
            expect(lines[1]).toBe("Middle line");
            expect(lines[2]).toBe("Another line");
            expect(lines[3]).toBe("End ;");
        });
    });

    describe("Single-Line Comment Formatting", () => {
        it("should format basic single-line comments", () => {
            const token = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* This is a single-line comment ;",
                createPosition(1, 1, 0)
            );

            const node: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("/* This is a single-line comment ;");
        });

        it("should wrap long single-line comments when enabled", () => {
            const customOptions = {
                ...defaultFormatterOptions,
                wrapLongComments: true,
                maxLineLength: 40,
            };
            visitor = new SSLCommentFormatterVisitor(customOptions);

            const token = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* This is a very long single-line comment that should be wrapped ;",
                createPosition(1, 1, 0)
            );

            const node: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            visitor.visit(node);
            const output = visitor.getFormattedOutput();
            const lines = output.split("\n").filter((line) => line.length > 0);

            expect(lines.length).toBeGreaterThan(1);
            expect(lines[0]).toContain("/* This");
            expect(lines[lines.length - 1]).toContain(";");
        });

        it("should not wrap comments when wrapLongComments is disabled", () => {
            const customOptions = {
                ...defaultFormatterOptions,
                wrapLongComments: false,
                maxLineLength: 40,
            };
            visitor = new SSLCommentFormatterVisitor(customOptions);

            const longComment =
                "/* This is a very long single-line comment that exceeds the max line length ;";
            const token = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                longComment,
                createPosition(1, 1, 0)
            );

            const node: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            visitor.visit(node);
            const output = visitor.getFormattedOutput().trim();

            expect(output).toBe(longComment);
        });

        it("should handle empty single-line comments", () => {
            const token = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* ;",
                createPosition(1, 1, 0)
            );

            const node: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            visitor.visit(node);
            const output = visitor.getFormattedOutput().trim();

            expect(output).toBe("/* ;");
        });
    });

    describe("Region Comment Formatting", () => {
        it("should format region comments when preserveRegionMarkers is enabled", () => {
            const customOptions = { ...defaultFormatterOptions, preserveRegionMarkers: true };
            visitor = new SSLCommentFormatterVisitor(customOptions);

            const token = createToken(
                TokenType.REGION_COMMENT,
                "/* region Main Logic ;",
                createPosition(1, 1, 0)
            );

            const node: RegionCommentNode = {
                kind: ASTNodeType.RegionComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("/* region Main Logic ;");
        });

        it("should treat region comments as regular comments when preserveRegionMarkers is disabled", () => {
            const customOptions = { ...defaultFormatterOptions, preserveRegionMarkers: false };
            visitor = new SSLCommentFormatterVisitor(customOptions);

            const token = createToken(
                TokenType.REGION_COMMENT,
                "/* region Main Logic ;",
                createPosition(1, 1, 0)
            );

            const node: RegionCommentNode = {
                kind: ASTNodeType.RegionComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            visitor.visit(node);
            const output = visitor.getFormattedOutput().trim();

            expect(output).toBe("/* region Main Logic ;");
        });

        it("should format region comments with complex names", () => {
            const token = createToken(
                TokenType.REGION_COMMENT,
                "/* region Data Validation and Processing Logic ;",
                createPosition(1, 1, 0)
            );

            const node: RegionCommentNode = {
                kind: ASTNodeType.RegionComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            visitor.visit(node);
            const output = visitor.getFormattedOutput().trim();

            expect(output).toBe("/* region Data Validation and Processing Logic ;");
        });

        it("should handle empty region comments", () => {
            const token = createToken(
                TokenType.REGION_COMMENT,
                "/* region ;",
                createPosition(1, 1, 0)
            );

            const node: RegionCommentNode = {
                kind: ASTNodeType.RegionComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            visitor.visit(node);
            const output = visitor.getFormattedOutput().trim();

            expect(output).toBe("/* region ;");
        });
    });

    describe("End Region Comment Formatting", () => {
        it("should format endregion comments when preserveRegionMarkers is enabled", () => {
            const customOptions = { ...defaultFormatterOptions, preserveRegionMarkers: true };
            visitor = new SSLCommentFormatterVisitor(customOptions);

            const token = createToken(
                TokenType.ENDREGION_COMMENT,
                "/* endregion Main Logic ;",
                createPosition(1, 1, 0)
            );

            const node: EndRegionCommentNode = {
                kind: ASTNodeType.EndRegionComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            const output = visitor.getFormattedOutput().trim();
            expect(output).toBe("/* endregion Main Logic ;");
        });

        it("should treat endregion comments as regular comments when preserveRegionMarkers is disabled", () => {
            const customOptions = { ...defaultFormatterOptions, preserveRegionMarkers: false };
            visitor = new SSLCommentFormatterVisitor(customOptions);

            const token = createToken(
                TokenType.ENDREGION_COMMENT,
                "/* endregion Main Logic ;",
                createPosition(1, 1, 0)
            );

            const node: EndRegionCommentNode = {
                kind: ASTNodeType.EndRegionComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            visitor.visit(node);
            const output = visitor.getFormattedOutput().trim();

            expect(output).toBe("/* endregion Main Logic ;");
        });

        it("should handle simple endregion comments", () => {
            const token = createToken(
                TokenType.ENDREGION_COMMENT,
                "/* endregion ;",
                createPosition(1, 1, 0)
            );

            const node: EndRegionCommentNode = {
                kind: ASTNodeType.EndRegionComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            visitor.visit(node);
            const output = visitor.getFormattedOutput().trim();

            expect(output).toBe("/* endregion ;");
        });
    });

    describe("Comment Wrapping", () => {
        it("should wrap long comments across multiple lines", () => {
            const customOptions = {
                ...defaultFormatterOptions,
                wrapLongComments: true,
                maxLineLength: 30,
            };
            visitor = new SSLCommentFormatterVisitor(customOptions);

            const token = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* This is a very long comment that needs to be wrapped across multiple lines ;",
                createPosition(1, 1, 0)
            );

            const node: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            visitor.visit(node);
            const output = visitor.getFormattedOutput();
            const lines = output.split("\n").filter((line) => line.length > 0);

            expect(lines.length).toBeGreaterThan(1);
            lines.forEach((line) => {
                expect(line).toContain("/*");
                expect(line).toContain(";");
            });
        });

        it("should handle comments with single words longer than max length", () => {
            const customOptions = {
                ...defaultFormatterOptions,
                wrapLongComments: true,
                maxLineLength: 15,
            };
            visitor = new SSLCommentFormatterVisitor(customOptions);

            const token = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* VeryLongWordThatExceedsMaxLength ;",
                createPosition(1, 1, 0)
            );

            const node: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            visitor.visit(node);
            const output = visitor.getFormattedOutput().trim();

            // Should still format properly even with long words
            expect(output).toContain("/*");
            expect(output).toContain(";");
        });

        it("should handle empty comment content gracefully", () => {
            const customOptions = {
                ...defaultFormatterOptions,
                wrapLongComments: true,
                maxLineLength: 20,
            };
            visitor = new SSLCommentFormatterVisitor(customOptions);

            const token = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/*    ;",
                createPosition(1, 1, 0)
            );

            const node: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            visitor.visit(node);
            const output = visitor.getFormattedOutput().trim();

            expect(output).toBe("/*    ;");
        });
    });

    describe("Comment Alignment", () => {
        it("should align end-of-line comments when enabled", () => {
            const customOptions = {
                ...defaultFormatterOptions,
                alignEndOfLineComments: true,
                commentAlignmentColumn: 40,
            };
            visitor = new SSLCommentFormatterVisitor(customOptions);

            // Simulate writing some code first
            visitor["output"].write("someCode := value;");

            const token = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* End of line comment ;",
                createPosition(1, 20, 19)
            );

            const node: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            visitor.visit(node);
            const output = visitor.getFormattedOutput();

            expect(output.length).toBeGreaterThan("someCode := value;".length);
            expect(output).toContain("/* End of line comment ;");
        });

        it("should not align region comments even when alignment is enabled", () => {
            const customOptions = {
                ...defaultFormatterOptions,
                alignEndOfLineComments: true,
                commentAlignmentColumn: 40,
            };
            visitor = new SSLCommentFormatterVisitor(customOptions);

            // Simulate writing some code first
            visitor["output"].write("someCode := value;");

            const token = createToken(
                TokenType.REGION_COMMENT,
                "/* region Test ;",
                createPosition(1, 20, 19)
            );

            const node: RegionCommentNode = {
                kind: ASTNodeType.RegionComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            visitor.visit(node);
            const output = visitor.getFormattedOutput();

            // Region comments should not be aligned
            expect(output).toContain("/* region Test ;");
        });
    });

    describe("SSL EBNF Grammar Compliance", () => {
        it("should handle all comment types defined in SSL EBNF", () => {
            // Test all comment types according to EBNF:
            // BlockComment ::= "/*" {Character} ";"
            // SingleLineComment ::= "/*" {Character} ";"
            // RegionComment ::= "/*" "region" {Character} ";"
            // EndRegionComment ::= "/*" "endregion" {Character} ";"

            const commentTestCases = [
                {
                    type: TokenType.BLOCK_COMMENT,
                    nodeType: ASTNodeType.BlockComment,
                    value: "/* This is a block comment ;",
                    description: "BlockComment EBNF compliance",
                },
                {
                    type: TokenType.SINGLE_LINE_COMMENT,
                    nodeType: ASTNodeType.SingleLineComment,
                    value: "/* This is a single line comment ;",
                    description: "SingleLineComment EBNF compliance",
                },
                {
                    type: TokenType.REGION_COMMENT,
                    nodeType: ASTNodeType.RegionComment,
                    value: "/* region Test Section ;",
                    description: "RegionComment EBNF compliance",
                },
                {
                    type: TokenType.ENDREGION_COMMENT,
                    nodeType: ASTNodeType.EndRegionComment,
                    value: "/* endregion Test Section ;",
                    description: "EndRegionComment EBNF compliance",
                },
            ];

            commentTestCases.forEach((testCase) => {
                visitor = new SSLCommentFormatterVisitor(options);

                const token = createToken(testCase.type, testCase.value, createPosition(1, 1, 0));
                const node = {
                    kind: testCase.nodeType,
                    startToken: token,
                    endToken: token,
                    token: token,
                } as any;

                const result = visitor.visit(node);

                expect(result.shouldContinue).toBe(false);
                expect(result.error).toBeUndefined();

                const output = visitor.getFormattedOutput().trim();
                expect(output).toBe(testCase.value);
            });
        });

        it("should preserve SSL comment syntax (/* ... ;)", () => {
            const sslCommentFormats = [
                "/* Simple comment ;",
                "/* Comment with special chars: :IF, :=, .AND. ;",
                "/* Comment with numbers 123 and symbols @#$ ;",
                "/* Comment with\nmultiple\nlines ;",
                "/* region Complex Region Name With Spaces ;",
                "/* endregion Complex Region Name With Spaces ;",
            ];

            sslCommentFormats.forEach((commentValue) => {
                visitor = new SSLCommentFormatterVisitor(options);

                const tokenType = commentValue.includes("region")
                    ? commentValue.includes("endregion")
                        ? TokenType.ENDREGION_COMMENT
                        : TokenType.REGION_COMMENT
                    : commentValue.includes("\n")
                    ? TokenType.BLOCK_COMMENT
                    : TokenType.SINGLE_LINE_COMMENT;

                const nodeType = commentValue.includes("region")
                    ? commentValue.includes("endregion")
                        ? ASTNodeType.EndRegionComment
                        : ASTNodeType.RegionComment
                    : commentValue.includes("\n")
                    ? ASTNodeType.BlockComment
                    : ASTNodeType.SingleLineComment;

                const token = createToken(tokenType, commentValue, createPosition(1, 1, 0));
                const node = {
                    kind: nodeType,
                    startToken: token,
                    endToken: token,
                    token: token,
                } as any;

                const result = visitor.visit(node);

                expect(result.shouldContinue).toBe(false);
                expect(result.error).toBeUndefined();

                const output = visitor.getFormattedOutput();
                expect(output).toContain("/*");
                expect(output).toContain(";");
            });
        });
    });

    describe("Formatting Options Integration", () => {
        it("should respect all comment-related formatting options", () => {
            const customOptions: FormatterOptions = {
                ...defaultFormatterOptions,
                preserveRegionMarkers: false,
                formatMultiLineComments: false,
                wrapLongComments: false,
                alignEndOfLineComments: false,
                commentAlignmentColumn: 80,
            };

            visitor = new SSLCommentFormatterVisitor(customOptions);

            const token = createToken(
                TokenType.REGION_COMMENT,
                "/* region This is a very long region name that would normally be wrapped ;",
                createPosition(1, 1, 0)
            );

            const node: RegionCommentNode = {
                kind: ASTNodeType.RegionComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            visitor.visit(node);
            const output = visitor.getFormattedOutput().trim();

            // Should preserve original formatting with all options disabled
            expect(output).toBe(
                "/* region This is a very long region name that would normally be wrapped ;"
            );
        });

        it("should apply indentation settings from options", () => {
            const customOptions = {
                ...defaultFormatterOptions,
                indentSize: 2,
                useTabs: false,
            };
            visitor = new SSLCommentFormatterVisitor(customOptions);

            // Add some indentation
            visitor["output"].indent();

            const token = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* Indented comment ;",
                createPosition(1, 1, 0)
            );

            const node: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            visitor.visit(node);
            const output = visitor.getFormattedOutput();

            expect(output).toMatch(/^\s+\/\* Indented comment ;/);
        });

        it("should handle maximum line length correctly", () => {
            const customOptions = {
                ...defaultFormatterOptions,
                maxLineLength: 50,
                wrapLongComments: true,
            };
            visitor = new SSLCommentFormatterVisitor(customOptions);

            const longComment =
                "/* This is a very long comment that should exceed the maximum line length specified in the options ;";
            const token = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                longComment,
                createPosition(1, 1, 0)
            );

            const node: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            visitor.visit(node);
            const output = visitor.getFormattedOutput();
            const lines = output.split("\n").filter((line) => line.length > 0);

            // Should be wrapped into multiple lines
            expect(lines.length).toBeGreaterThan(1);
            lines.forEach((line) => {
                expect(line.length).toBeLessThanOrEqual(customOptions.maxLineLength + 10); // Allow some tolerance
            });
        });
    });

    describe("Error Handling", () => {
        it("should handle nodes without tokens gracefully", () => {
            const node: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: mockToken,
                endToken: mockToken,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();
        });

        it("should handle tokens without values gracefully", () => {
            const token = createToken(TokenType.SINGLE_LINE_COMMENT, "", createPosition(1, 1, 0));

            const node: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            const result = visitor.visit(node);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();
        });

        it("should handle malformed comment tokens gracefully", () => {
            const malformedTokens = [
                "/* Missing semicolon",
                "Missing comment start ;",
                "/* ;", // Empty comment
                "/*;", // No space
            ];

            malformedTokens.forEach((tokenValue) => {
                visitor = new SSLCommentFormatterVisitor(options);

                const token = createToken(
                    TokenType.SINGLE_LINE_COMMENT,
                    tokenValue,
                    createPosition(1, 1, 0)
                );
                const node: SingleLineCommentNode = {
                    kind: ASTNodeType.SingleLineComment,
                    startToken: token,
                    endToken: token,
                    token: token,
                } as any;

                const result = visitor.visit(node);

                expect(result.shouldContinue).toBe(false);
                expect(result.error).toBeUndefined();
            });
        });
    });

    describe("Integration with FormatterVisitorBase", () => {
        it("should properly inherit from FormatterVisitorBase", () => {
            expect(visitor).toBeInstanceOf(SSLCommentFormatterVisitor);
            expect(typeof visitor.visit).toBe("function");
            expect(typeof visitor.getFormattedOutput).toBe("function");
        });

        it("should maintain proper visitor result structure", () => {
            const token = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* Test comment ;",
                createPosition(1, 1, 0)
            );

            const node: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            const result = visitor.visit(node);

            expect(result).toHaveProperty("shouldContinue");
            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBeUndefined();
        });

        it("should use the output builder correctly", () => {
            const token = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* Output builder test ;",
                createPosition(1, 1, 0)
            );

            const node: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: token,
                endToken: token,
                token: token,
            } as any;

            visitor.visit(node);

            const output = visitor.getFormattedOutput();
            expect(typeof output).toBe("string");
            expect(output.length).toBeGreaterThan(0);
            expect(output).toContain("/* Output builder test ;");
        });
    });
});
