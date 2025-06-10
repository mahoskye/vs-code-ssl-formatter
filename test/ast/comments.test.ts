import {
    CommentStatementNode,
    BlockCommentNode,
    SingleLineCommentNode,
    RegionCommentNode,
    EndRegionCommentNode,
} from "../../src/parser/ast/comments";
import { ASTNodeType } from "../../src/parser/ast/base";
import { Token, createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("AST Comments", () => {
    let mockToken: Token;

    beforeEach(() => {
        mockToken = createToken(
            TokenType.BLOCK_COMMENT,
            "/* test comment ;",
            createPosition(1, 1, 0)
        );
    });

    describe("CommentStatementNode", () => {
        it("should have correct structure", () => {
            const commentStatement: CommentStatementNode = {
                kind: ASTNodeType.CommentStatement,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(commentStatement.kind).toBe(ASTNodeType.CommentStatement);
            expect(commentStatement.startToken).toBe(mockToken);
            expect(commentStatement.endToken).toBe(mockToken);
        });
    });

    describe("BlockCommentNode", () => {
        it("should have correct structure", () => {
            const blockComment: BlockCommentNode = {
                kind: ASTNodeType.BlockComment,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(blockComment.kind).toBe(ASTNodeType.BlockComment);
            expect(blockComment.startToken).toBe(mockToken);
            expect(blockComment.endToken).toBe(mockToken);
        });
    });

    describe("SingleLineCommentNode", () => {
        it("should have correct structure", () => {
            const singleLineComment: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(singleLineComment.kind).toBe(ASTNodeType.SingleLineComment);
            expect(singleLineComment.startToken).toBe(mockToken);
            expect(singleLineComment.endToken).toBe(mockToken);
        });
    });

    describe("RegionCommentNode", () => {
        it("should have correct structure", () => {
            const regionComment: RegionCommentNode = {
                kind: ASTNodeType.RegionComment,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(regionComment.kind).toBe(ASTNodeType.RegionComment);
            expect(regionComment.startToken).toBe(mockToken);
            expect(regionComment.endToken).toBe(mockToken);
        });
    });

    describe("EndRegionCommentNode", () => {
        it("should have correct structure", () => {
            const endRegionComment: EndRegionCommentNode = {
                kind: ASTNodeType.EndRegionComment,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(endRegionComment.kind).toBe(ASTNodeType.EndRegionComment);
            expect(endRegionComment.startToken).toBe(mockToken);
            expect(endRegionComment.endToken).toBe(mockToken);
        });
    });

    describe("Comment type consistency", () => {
        it("should have consistent naming for comment types", () => {
            const commentTypes = [
                ASTNodeType.CommentStatement,
                ASTNodeType.BlockComment,
                ASTNodeType.SingleLineComment,
                ASTNodeType.RegionComment,
                ASTNodeType.EndRegionComment,
            ];

            commentTypes.forEach((type) => {
                expect(type).toContain("Comment");
            });
        });

        it("should have unique identifiers for each comment type", () => {
            const commentTypes = [
                ASTNodeType.CommentStatement,
                ASTNodeType.BlockComment,
                ASTNodeType.SingleLineComment,
                ASTNodeType.RegionComment,
                ASTNodeType.EndRegionComment,
            ];

            const uniqueTypes = new Set(commentTypes);
            expect(commentTypes.length).toBe(uniqueTypes.size);
        });
    });

    describe("Comment functionality", () => {
        it("should support different comment styles", () => {
            const commentVariants = [
                {
                    token: createToken(
                        TokenType.SINGLE_LINE_COMMENT,
                        "/* single line ;",
                        createPosition(1, 1, 0)
                    ),
                    type: ASTNodeType.SingleLineComment,
                },
                {
                    token: createToken(
                        TokenType.BLOCK_COMMENT,
                        "/* block ;",
                        createPosition(1, 1, 0)
                    ),
                    type: ASTNodeType.BlockComment,
                },
                {
                    token: createToken(
                        TokenType.REGION_COMMENT,
                        "/* region test ;",
                        createPosition(1, 1, 0)
                    ),
                    type: ASTNodeType.RegionComment,
                },
                {
                    token: createToken(
                        TokenType.ENDREGION_COMMENT,
                        "/* endregion ;",
                        createPosition(1, 1, 0)
                    ),
                    type: ASTNodeType.EndRegionComment,
                },
            ];

            commentVariants.forEach((variant) => {
                const commentNode = {
                    kind: variant.type,
                    startToken: variant.token,
                    endToken: variant.token,
                };

                expect(commentNode.kind).toBe(variant.type);
                expect(commentNode.startToken.value).toBe(variant.token.value);
            });
        });

        it("should handle empty comments", () => {
            const emptyComment = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* ;",
                createPosition(1, 1, 0)
            );
            const commentNode: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: emptyComment,
                endToken: emptyComment,
            };

            expect(commentNode.startToken.value).toBe("/* ;");
            expect(commentNode.startToken.value.length).toBe(4);
        });

        it("should handle multiline content in block comments", () => {
            const multilineComment = createToken(
                TokenType.BLOCK_COMMENT,
                "/* line 1\n   line 2\n   line 3 ;",
                createPosition(1, 1, 0)
            );

            const commentNode: BlockCommentNode = {
                kind: ASTNodeType.BlockComment,
                startToken: multilineComment,
                endToken: multilineComment,
            };

            expect(commentNode.startToken.value).toContain("\n");
            expect(commentNode.startToken.value).toContain("line 1");
            expect(commentNode.startToken.value).toContain("line 3");
        });
    });
});
