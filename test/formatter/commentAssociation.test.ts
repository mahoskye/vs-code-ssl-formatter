/**
 * Comment Association Tests
 *
 * Comprehensive test suite for comment association functionality:
 * - Leading comment association
 * - Trailing comment association
 * - Following comment association
 * - Standalone comment handling
 * - Region comment special handling
 * - Comment positioning and preservation
 */

import {
    CommentAssociator,
    CommentAssociation,
    CommentPosition,
    defaultCommentAssociationOptions,
    CommentAssociationOptions,
} from "../../src/formatter/commentAssociation";
import {
    CommentStatementNode,
    BlockCommentNode,
    SingleLineCommentNode,
    RegionCommentNode,
    EndRegionCommentNode,
} from "../../src/parser/ast/comments";
import { ASTNode, ASTNodeType } from "../../src/parser/ast/base";
import { ProcedureStatementNode } from "../../src/parser/ast/procedures";
import { IfStatementNode } from "../../src/parser/ast/controlFlow";
import { DeclareStatementNode } from "../../src/parser/ast/declarations";
import { Token, createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("CommentAssociator", () => {
    let associator: CommentAssociator;

    beforeEach(() => {
        associator = new CommentAssociator();
    });

    describe("Basic Comment Association", () => {
        it("should associate leading comments with following nodes", () => {
            // Create a comment on line 1
            const commentToken = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* This is a leading comment ;",
                createPosition(1, 1, 0)
            );

            const comment: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: commentToken,
                endToken: commentToken,
            };

            // Create a procedure statement on line 3
            const procToken = createToken(TokenType.COLON, ":", createPosition(3, 1, 20));

            const procedure: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: procToken,
                endToken: procToken,
            } as any;

            const associations = associator.associateComments([comment], [procedure]);

            expect(associations).toHaveLength(1);
            expect(associations[0].comment).toBe(comment);
            expect(associations[0].associatedNode).toBe(procedure);
            expect(associations[0].position).toBe(CommentPosition.Leading);
            expect(associations[0].preserve).toBe(true);
        });

        it("should associate trailing comments with preceding nodes on same line", () => {
            // Create a declaration on line 1
            const declToken = createToken(
                TokenType.IDENTIFIER,
                "variable",
                createPosition(1, 1, 0)
            );

            const declaration: DeclareStatementNode = {
                kind: ASTNodeType.DeclareStatement,
                startToken: declToken,
                endToken: declToken,
            } as any;

            // Create a comment on the same line, after the declaration
            const commentToken = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* Variable declaration ;",
                createPosition(1, 15, 14)
            );

            const comment: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: commentToken,
                endToken: commentToken,
            };

            const associations = associator.associateComments([comment], [declaration]);

            expect(associations).toHaveLength(1);
            expect(associations[0].comment).toBe(comment);
            expect(associations[0].associatedNode).toBe(declaration);
            expect(associations[0].position).toBe(CommentPosition.Trailing);
            expect(associations[0].preserve).toBe(true);
        });

        it("should associate following comments with preceding nodes", () => {
            // Create a procedure statement on line 1
            const procToken = createToken(TokenType.COLON, ":", createPosition(1, 1, 0));

            const procedure: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: procToken,
                endToken: procToken,
            } as any;

            // Create a comment on line 2 (following the procedure)
            const commentToken = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* End of procedure ;",
                createPosition(2, 1, 20)
            );

            const comment: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: commentToken,
                endToken: commentToken,
            };

            const associations = associator.associateComments([comment], [procedure]);

            expect(associations).toHaveLength(1);
            expect(associations[0].comment).toBe(comment);
            expect(associations[0].associatedNode).toBe(procedure);
            expect(associations[0].position).toBe(CommentPosition.Following);
            expect(associations[0].preserve).toBe(true);
        });

        it("should mark distant comments as standalone", () => {
            // Create a procedure statement on line 1
            const procToken = createToken(TokenType.COLON, ":", createPosition(1, 1, 0));

            const procedure: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: procToken,
                endToken: procToken,
            } as any;

            // Create a comment on line 10 (too far from the procedure)
            const commentToken = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* Standalone comment ;",
                createPosition(10, 1, 100)
            );

            const comment: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: commentToken,
                endToken: commentToken,
            };

            const associations = associator.associateComments([comment], [procedure]);

            expect(associations).toHaveLength(1);
            expect(associations[0].comment).toBe(comment);
            expect(associations[0].associatedNode).toBeNull();
            expect(associations[0].position).toBe(CommentPosition.Standalone);
            expect(associations[0].preserve).toBe(true);
        });
    });

    describe("Region Comment Handling", () => {
        it("should associate region comments with following block structures", () => {
            // Create a region comment on line 1
            const regionToken = createToken(
                TokenType.REGION_COMMENT,
                "/* region Data Processing ;",
                createPosition(1, 1, 0)
            );

            const regionComment: RegionCommentNode = {
                kind: ASTNodeType.RegionComment,
                startToken: regionToken,
                endToken: regionToken,
            };

            // Create a procedure statement on line 2
            const procToken = createToken(TokenType.COLON, ":", createPosition(2, 1, 10));

            const procedure: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: procToken,
                endToken: procToken,
            } as any;

            const associations = associator.associateComments([regionComment], [procedure]);

            expect(associations).toHaveLength(1);
            expect(associations[0].comment).toBe(regionComment);
            expect(associations[0].associatedNode).toBe(procedure);
            expect(associations[0].position).toBe(CommentPosition.Leading);
            expect(associations[0].preserve).toBe(true);
        });

        it("should handle endregion comments as standalone when no matching structure", () => {
            // Create an endregion comment
            const endRegionToken = createToken(
                TokenType.ENDREGION_COMMENT,
                "/* endregion Data Processing ;",
                createPosition(5, 1, 50)
            );

            const endRegionComment: EndRegionCommentNode = {
                kind: ASTNodeType.EndRegionComment,
                startToken: endRegionToken,
                endToken: endRegionToken,
            };

            const associations = associator.associateComments([endRegionComment], []);

            expect(associations).toHaveLength(1);
            expect(associations[0].comment).toBe(endRegionComment);
            expect(associations[0].associatedNode).toBeNull();
            expect(associations[0].position).toBe(CommentPosition.Standalone);
            expect(associations[0].preserve).toBe(true);
        });
    });

    describe("Multiple Comments Association", () => {
        it("should associate multiple leading comments with same node", () => {
            // Create multiple comments before a procedure
            const comment1Token = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* First comment ;",
                createPosition(1, 1, 0)
            );

            const comment1: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: comment1Token,
                endToken: comment1Token,
            };

            const comment2Token = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* Second comment ;",
                createPosition(2, 1, 10)
            );

            const comment2: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: comment2Token,
                endToken: comment2Token,
            };

            // Create a procedure statement on line 3 (within maxLeadingDistance of 2)
            const procToken = createToken(TokenType.COLON, ":", createPosition(3, 1, 30));

            const procedure: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: procToken,
                endToken: procToken,
            } as any;
            const associations = associator.associateComments([comment1, comment2], [procedure]);

            expect(associations).toHaveLength(2);

            // Both comments should be associated with the procedure as leading
            expect(associations[0].associatedNode).toBe(procedure);
            expect(associations[0].position).toBe(CommentPosition.Leading);
            expect(associations[1].associatedNode).toBe(procedure);
            expect(associations[1].position).toBe(CommentPosition.Leading);

            // Check that getLeadingComments returns both in correct order
            const leadingComments = associator.getLeadingComments(procedure);
            expect(leadingComments).toHaveLength(2);
            expect(leadingComments[0].comment).toBe(comment1); // First in line order
            expect(leadingComments[1].comment).toBe(comment2);
        });

        it("should choose closest node for comment association", () => {
            // Create two nodes
            const node1Token = createToken(TokenType.IDENTIFIER, "node1", createPosition(1, 1, 0));

            const node1: DeclareStatementNode = {
                kind: ASTNodeType.DeclareStatement,
                startToken: node1Token,
                endToken: node1Token,
            } as any;

            const node2Token = createToken(
                TokenType.IDENTIFIER,
                "node2",
                createPosition(10, 1, 100)
            );

            const node2: DeclareStatementNode = {
                kind: ASTNodeType.DeclareStatement,
                startToken: node2Token,
                endToken: node2Token,
            } as any;

            // Create a comment closer to node1
            const commentToken = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* Comment closer to node1 ;",
                createPosition(2, 1, 10)
            );

            const comment: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: commentToken,
                endToken: commentToken,
            };

            const associations = associator.associateComments([comment], [node1, node2]);

            expect(associations).toHaveLength(1);
            expect(associations[0].associatedNode).toBe(node1);
            expect(associations[0].position).toBe(CommentPosition.Following);
        });
    });

    describe("Comment Association Options", () => {
        it("should respect maxLeadingDistance option", () => {
            const customOptions: CommentAssociationOptions = {
                ...defaultCommentAssociationOptions,
                maxLeadingDistance: 1, // Only associate if comment is 1 line before
            };
            associator = new CommentAssociator(customOptions);

            // Create a comment 2 lines before a node (exceeds maxLeadingDistance)
            const commentToken = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* Distant comment ;",
                createPosition(1, 1, 0)
            );

            const comment: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: commentToken,
                endToken: commentToken,
            };

            const nodeToken = createToken(
                TokenType.IDENTIFIER,
                "node",
                createPosition(3, 1, 20) // 2 lines after comment
            );

            const node: DeclareStatementNode = {
                kind: ASTNodeType.DeclareStatement,
                startToken: nodeToken,
                endToken: nodeToken,
            } as any;

            const associations = associator.associateComments([comment], [node]);

            expect(associations).toHaveLength(1);
            expect(associations[0].associatedNode).toBeNull();
            expect(associations[0].position).toBe(CommentPosition.Standalone);
        });

        it("should respect maxFollowingDistance option", () => {
            const customOptions: CommentAssociationOptions = {
                ...defaultCommentAssociationOptions,
                maxFollowingDistance: 0, // Don't associate following comments
            };
            associator = new CommentAssociator(customOptions);

            const nodeToken = createToken(TokenType.IDENTIFIER, "node", createPosition(1, 1, 0));

            const node: DeclareStatementNode = {
                kind: ASTNodeType.DeclareStatement,
                startToken: nodeToken,
                endToken: nodeToken,
            } as any;

            // Create a comment 1 line after the node
            const commentToken = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* Following comment ;",
                createPosition(2, 1, 10)
            );

            const comment: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: commentToken,
                endToken: commentToken,
            };

            const associations = associator.associateComments([comment], [node]);

            expect(associations).toHaveLength(1);
            expect(associations[0].associatedNode).toBeNull();
            expect(associations[0].position).toBe(CommentPosition.Standalone);
        });

        it("should respect preserveStandaloneComments option", () => {
            const customOptions: CommentAssociationOptions = {
                ...defaultCommentAssociationOptions,
                preserveStandaloneComments: false,
            };
            associator = new CommentAssociator(customOptions);

            // Create a standalone comment (no nearby nodes)
            const commentToken = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* Standalone comment ;",
                createPosition(5, 1, 50)
            );

            const comment: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: commentToken,
                endToken: commentToken,
            };

            const associations = associator.associateComments([comment], []);

            expect(associations).toHaveLength(1);
            expect(associations[0].position).toBe(CommentPosition.Standalone);
            expect(associations[0].preserve).toBe(false); // Should not preserve
        });
    });

    describe("Block Comment Types", () => {
        it("should handle block comments properly", () => {
            const blockCommentToken = createToken(
                TokenType.BLOCK_COMMENT,
                "/* This is a\nmulti-line\nblock comment ;",
                createPosition(1, 1, 0)
            );

            const blockComment: BlockCommentNode = {
                kind: ASTNodeType.BlockComment,
                startToken: blockCommentToken,
                endToken: blockCommentToken,
            };

            const nodeToken = createToken(TokenType.COLON, ":", createPosition(3, 1, 30));

            const node: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: nodeToken,
                endToken: nodeToken,
            } as any;

            const associations = associator.associateComments([blockComment], [node]);

            expect(associations).toHaveLength(1);
            expect(associations[0].comment).toBe(blockComment);
            expect(associations[0].associatedNode).toBe(node);
            expect(associations[0].position).toBe(CommentPosition.Leading);
        });

        it("should handle comment statements (wrappers)", () => {
            // Create a comment statement node that wraps another comment
            const innerCommentToken = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* Inner comment ;",
                createPosition(1, 1, 0)
            );

            const commentStatement: CommentStatementNode = {
                kind: ASTNodeType.CommentStatement,
                startToken: innerCommentToken,
                endToken: innerCommentToken,
            };

            const nodeToken = createToken(TokenType.IDENTIFIER, "node", createPosition(2, 1, 10));

            const node: DeclareStatementNode = {
                kind: ASTNodeType.DeclareStatement,
                startToken: nodeToken,
                endToken: nodeToken,
            } as any;

            const associations = associator.associateComments([commentStatement], [node]);

            expect(associations).toHaveLength(1);
            expect(associations[0].comment).toBe(commentStatement);
            expect(associations[0].position).toBe(CommentPosition.Leading);
        });
    });

    describe("Utility Methods", () => {
        it("should retrieve specific associations", () => {
            const commentToken = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* Test comment ;",
                createPosition(1, 1, 0)
            );

            const comment: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: commentToken,
                endToken: commentToken,
            };

            const nodeToken = createToken(TokenType.IDENTIFIER, "node", createPosition(2, 1, 10));

            const node: DeclareStatementNode = {
                kind: ASTNodeType.DeclareStatement,
                startToken: nodeToken,
                endToken: nodeToken,
            } as any;

            associator.associateComments([comment], [node]);

            const association = associator.getAssociation(comment);
            expect(association).toBeDefined();
            expect(association!.comment).toBe(comment);
            expect(association!.associatedNode).toBe(node);
        });

        it("should get standalone comments", () => {
            const standaloneToken = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* Standalone ;",
                createPosition(10, 1, 100)
            );

            const standaloneComment: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: standaloneToken,
                endToken: standaloneToken,
            };

            const leadingToken = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* Leading ;",
                createPosition(1, 1, 0)
            );

            const leadingComment: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: leadingToken,
                endToken: leadingToken,
            };

            const nodeToken = createToken(TokenType.IDENTIFIER, "node", createPosition(2, 1, 10));

            const node: DeclareStatementNode = {
                kind: ASTNodeType.DeclareStatement,
                startToken: nodeToken,
                endToken: nodeToken,
            } as any;

            associator.associateComments([standaloneComment, leadingComment], [node]);

            const standaloneComments = associator.getStandaloneComments();
            expect(standaloneComments).toHaveLength(1);
            expect(standaloneComments[0].comment).toBe(standaloneComment);
        });

        it("should get trailing comments", () => {
            const nodeToken = createToken(TokenType.IDENTIFIER, "node", createPosition(1, 1, 0));

            const node: DeclareStatementNode = {
                kind: ASTNodeType.DeclareStatement,
                startToken: nodeToken,
                endToken: nodeToken,
            } as any;

            const trailingToken = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* Trailing ;",
                createPosition(1, 10, 9)
            );

            const trailingComment: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: trailingToken,
                endToken: trailingToken,
            };

            const followingToken = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* Following ;",
                createPosition(2, 1, 10)
            );

            const followingComment: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: followingToken,
                endToken: followingToken,
            };

            associator.associateComments([trailingComment, followingComment], [node]);

            const trailingComments = associator.getTrailingComments(node);
            expect(trailingComments).toHaveLength(2);

            // Should return in line order
            expect(trailingComments[0].comment).toBe(trailingComment);
            expect(trailingComments[1].comment).toBe(followingComment);
        });

        it("should clear associations", () => {
            const commentToken = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* Test ;",
                createPosition(1, 1, 0)
            );

            const comment: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: commentToken,
                endToken: commentToken,
            };

            associator.associateComments([comment], []);
            expect(associator.size()).toBe(1);

            associator.clear();
            expect(associator.size()).toBe(0);
            expect(associator.getAssociation(comment)).toBeUndefined();
        });
    });

    describe("Edge Cases", () => {
        it("should handle empty arrays", () => {
            const associations = associator.associateComments([], []);
            expect(associations).toHaveLength(0);
        });

        it("should handle comments without tokens", () => {
            const comment: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: null as any,
                endToken: null as any,
            };

            const associations = associator.associateComments([comment], []);
            expect(associations).toHaveLength(1);
            expect(associations[0].lineNumber).toBe(0);
            expect(associations[0].columnPosition).toBe(0);
        });

        it("should handle nodes without tokens", () => {
            const commentToken = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* Test ;",
                createPosition(1, 1, 0)
            );

            const comment: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: commentToken,
                endToken: commentToken,
            };

            const node: DeclareStatementNode = {
                kind: ASTNodeType.DeclareStatement,
                startToken: null as any,
                endToken: null as any,
            } as any;

            const associations = associator.associateComments([comment], [node]);
            expect(associations).toHaveLength(1);
            expect(associations[0].position).toBe(CommentPosition.Standalone);
        });

        it("should handle comments and nodes on same line with same column", () => {
            const position = createPosition(1, 1, 0);

            const commentToken = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* Comment ;",
                position
            );

            const comment: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: commentToken,
                endToken: commentToken,
            };

            const nodeToken = createToken(TokenType.IDENTIFIER, "node", position);

            const node: DeclareStatementNode = {
                kind: ASTNodeType.DeclareStatement,
                startToken: nodeToken,
                endToken: nodeToken,
            } as any;

            const associations = associator.associateComments([comment], [node]);
            expect(associations).toHaveLength(1);
            // Should still be standalone since comment doesn't come after node
            expect(associations[0].position).toBe(CommentPosition.Standalone);
        });
    });

    describe("EBNF Grammar Compliance", () => {
        it("should handle all SSL comment types from EBNF grammar", () => {
            // Test all comment types defined in the EBNF grammar
            const commentTypes = [
                {
                    type: TokenType.BLOCK_COMMENT,
                    kind: ASTNodeType.BlockComment,
                    value: "/* Block comment with\nmultiple lines ;",
                },
                {
                    type: TokenType.SINGLE_LINE_COMMENT,
                    kind: ASTNodeType.SingleLineComment,
                    value: "/* Single line comment ;",
                },
                {
                    type: TokenType.REGION_COMMENT,
                    kind: ASTNodeType.RegionComment,
                    value: "/* region Test Region ;",
                },
                {
                    type: TokenType.ENDREGION_COMMENT,
                    kind: ASTNodeType.EndRegionComment,
                    value: "/* endregion Test Region ;",
                },
            ];

            const comments: (
                | BlockCommentNode
                | SingleLineCommentNode
                | RegionCommentNode
                | EndRegionCommentNode
            )[] = [];

            commentTypes.forEach((commentType, index) => {
                const token = createToken(
                    commentType.type,
                    commentType.value,
                    createPosition(index + 1, 1, index * 10)
                );

                const comment = {
                    kind: commentType.kind,
                    startToken: token,
                    endToken: token,
                } as any;

                comments.push(comment);
            });

            const associations = associator.associateComments(comments, []);

            expect(associations).toHaveLength(4);
            associations.forEach((association) => {
                expect(association.preserve).toBe(true);
                expect(association.position).toBe(CommentPosition.Standalone);
            });
        });

        it("should preserve comment positioning according to EBNF rules", () => {
            // According to EBNF, comments can appear in various contexts
            // This test ensures positioning is preserved correctly

            const leadingComment = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* Leading comment before procedure ;",
                createPosition(1, 1, 0)
            );

            const procedureToken = createToken(TokenType.COLON, ":", createPosition(2, 1, 10));

            const trailingComment = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* Trailing comment after assignment ;",
                createPosition(2, 20, 29)
            );

            const leadingCommentNode: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: leadingComment,
                endToken: leadingComment,
            };

            const procedureNode: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: procedureToken,
                endToken: procedureToken,
            } as any;

            const trailingCommentNode: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: trailingComment,
                endToken: trailingComment,
            };

            const associations = associator.associateComments(
                [leadingCommentNode, trailingCommentNode],
                [procedureNode]
            );

            expect(associations).toHaveLength(2);

            const leadingAssoc = associations.find((a) => a.comment === leadingCommentNode);
            const trailingAssoc = associations.find((a) => a.comment === trailingCommentNode);

            expect(leadingAssoc!.position).toBe(CommentPosition.Leading);
            expect(trailingAssoc!.position).toBe(CommentPosition.Trailing);
        });
    });
});
