/**
 * Comment Association Integration Tests
 *
 * Tests that verify comment association works correctly with the main SSL formatter:
 * - Comments are properly associated with AST nodes
 * - Leading and trailing comments are preserved
 * - Comment positioning is maintained during formatting
 * - Integration with existing formatter functionality
 */

import { SSLFormatter } from "../../src/formatter/index";
import { FormatterOptions, defaultFormatterOptions } from "../../src/formatter/options";
import { ASTNode, ASTNodeType } from "../../src/parser/ast/base";
import { ProgramNode } from "../../src/parser/ast/program";
import { ProcedureStatementNode } from "../../src/parser/ast/procedures";
import { DeclareStatementNode } from "../../src/parser/ast/declarations";
import {
    SingleLineCommentNode,
    BlockCommentNode,
    RegionCommentNode,
} from "../../src/parser/ast/comments";
import { Token, createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("Comment Association Integration Tests", () => {
    let formatter: SSLFormatter;
    let options: FormatterOptions;

    beforeEach(() => {
        options = { ...defaultFormatterOptions };
        formatter = new SSLFormatter(options);
    });

    describe("Comment Association with Main Formatter", () => {
        it("should format code with leading comments properly", () => {
            // Create a leading comment
            const commentToken = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* This procedure handles data processing ;",
                createPosition(1, 1, 0)
            );

            const comment: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: commentToken,
                endToken: commentToken,
            };

            // Create a procedure that the comment should lead
            const procToken = createToken(TokenType.COLON, ":", createPosition(3, 1, 50));

            const procedure: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: procToken,
                endToken: procToken,
                name: createToken(TokenType.IDENTIFIER, "ProcessData", createPosition(3, 12, 61)),
                body: [],
            } as any;

            // Create a program node with both comment and procedure
            const program: ProgramNode = {
                kind: ASTNodeType.Program,
                startToken: commentToken,
                endToken: procToken,
                body: [comment, procedure],
            };

            const result = formatter.format(program);

            // The result should contain both the comment and the procedure
            expect(result).toContain("/* This procedure handles data processing ;");
            expect(result).toContain("ProcessData");

            // The comment should appear before the procedure
            const commentIndex = result.indexOf("/* This procedure handles data processing ;");
            const procedureIndex = result.indexOf("ProcessData");
            expect(commentIndex).toBeLessThan(procedureIndex);
        });

        it("should format code with trailing comments properly", () => {
            // Create a declaration
            const declToken = createToken(
                TokenType.IDENTIFIER,
                "sUserName",
                createPosition(1, 1, 0)
            );

            const declaration: DeclareStatementNode = {
                kind: ASTNodeType.DeclareStatement,
                startToken: declToken,
                endToken: declToken,
                identifiers: {
                    kind: ASTNodeType.IdentifierList,
                    identifiers: [declToken],
                    startToken: declToken,
                    endToken: declToken,
                },
            } as any;

            // Create a trailing comment on the same line
            const commentToken = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* User name variable ;",
                createPosition(1, 20, 19)
            );

            const comment: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: commentToken,
                endToken: commentToken,
            };

            const program: ProgramNode = {
                kind: ASTNodeType.Program,
                startToken: declToken,
                endToken: commentToken,
                body: [declaration, comment],
            };

            const result = formatter.format(program);

            expect(result).toContain("sUserName");
            expect(result).toContain("/* User name variable ;");
        });

        it("should handle region comments with block structures", () => {
            // Create a region comment
            const regionToken = createToken(
                TokenType.REGION_COMMENT,
                "/* region Data Validation ;",
                createPosition(1, 1, 0)
            );

            const regionComment: RegionCommentNode = {
                kind: ASTNodeType.RegionComment,
                startToken: regionToken,
                endToken: regionToken,
            };

            // Create a procedure within the region
            const procToken = createToken(TokenType.COLON, ":", createPosition(3, 1, 30));

            const procedure: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: procToken,
                endToken: procToken,
                name: createToken(TokenType.IDENTIFIER, "ValidateInput", createPosition(3, 12, 41)),
                body: [],
            } as any;

            const program: ProgramNode = {
                kind: ASTNodeType.Program,
                startToken: regionToken,
                endToken: procToken,
                body: [regionComment, procedure],
            };

            const result = formatter.format(program);

            expect(result).toContain("/* region Data Validation ;");
            expect(result).toContain("ValidateInput");

            // Region comment should appear before the procedure
            const regionIndex = result.indexOf("/* region Data Validation ;");
            const procedureIndex = result.indexOf("ValidateInput");
            expect(regionIndex).toBeLessThan(procedureIndex);
        });

        it("should handle standalone comments", () => {
            // Create a standalone comment (not close to any code)
            const standaloneToken = createToken(
                TokenType.BLOCK_COMMENT,
                "/* TODO: Implement error handling\n   for this module ;",
                createPosition(1, 1, 0)
            );

            const standaloneComment: BlockCommentNode = {
                kind: ASTNodeType.BlockComment,
                startToken: standaloneToken,
                endToken: standaloneToken,
            };

            // Create a procedure far away from the comment
            const procToken = createToken(TokenType.COLON, ":", createPosition(10, 1, 100));

            const procedure: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: procToken,
                endToken: procToken,
                name: createToken(
                    TokenType.IDENTIFIER,
                    "SomeFunction",
                    createPosition(10, 12, 111)
                ),
                body: [],
            } as any;

            const program: ProgramNode = {
                kind: ASTNodeType.Program,
                startToken: standaloneToken,
                endToken: procToken,
                body: [standaloneComment, procedure],
            };

            const result = formatter.format(program);

            expect(result).toContain("/* TODO: Implement error handling");
            expect(result).toContain("SomeFunction");
        });

        it("should handle multiple comments with different associations", () => {
            // Create multiple comments and procedures
            const leadingCommentToken = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* Function to process user data ;",
                createPosition(1, 1, 0)
            );

            const leadingComment: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: leadingCommentToken,
                endToken: leadingCommentToken,
            };

            const proc1Token = createToken(TokenType.COLON, ":", createPosition(3, 1, 40));

            const procedure1: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: proc1Token,
                endToken: proc1Token,
                name: createToken(TokenType.IDENTIFIER, "ProcessUser", createPosition(3, 12, 51)),
                body: [],
            } as any;

            const trailingCommentToken = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* End of user processing ;",
                createPosition(4, 1, 60)
            );

            const trailingComment: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: trailingCommentToken,
                endToken: trailingCommentToken,
            };

            const standaloneCommentToken = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* Standalone helper comment ;",
                createPosition(10, 1, 150)
            );

            const standaloneComment: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: standaloneCommentToken,
                endToken: standaloneCommentToken,
            };

            const proc2Token = createToken(TokenType.COLON, ":", createPosition(15, 1, 200));

            const procedure2: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: proc2Token,
                endToken: proc2Token,
                name: createToken(
                    TokenType.IDENTIFIER,
                    "HelperFunction",
                    createPosition(15, 12, 211)
                ),
                body: [],
            } as any;

            const program: ProgramNode = {
                kind: ASTNodeType.Program,
                startToken: leadingCommentToken,
                endToken: proc2Token,
                body: [leadingComment, procedure1, trailingComment, standaloneComment, procedure2],
            };

            const result = formatter.format(program);

            // All comments and procedures should be present
            expect(result).toContain("/* Function to process user data ;");
            expect(result).toContain("ProcessUser");
            expect(result).toContain("/* End of user processing ;");
            expect(result).toContain("/* Standalone helper comment ;");
            expect(result).toContain("HelperFunction");

            // Verify proper ordering
            const parts = [
                "/* Function to process user data ;",
                "ProcessUser",
                "/* End of user processing ;",
                "/* Standalone helper comment ;",
                "HelperFunction",
            ];

            let lastIndex = -1;
            for (const part of parts) {
                const currentIndex = result.indexOf(part);
                expect(currentIndex).toBeGreaterThan(lastIndex);
                lastIndex = currentIndex;
            }
        });

        it("should preserve comment formatting within associations", () => {
            // Create a block comment with specific formatting
            const blockCommentToken = createToken(
                TokenType.BLOCK_COMMENT,
                "/* Important function:\n   - Validates input\n   - Processes data\n   - Returns result ;",
                createPosition(1, 1, 0)
            );

            const blockComment: BlockCommentNode = {
                kind: ASTNodeType.BlockComment,
                startToken: blockCommentToken,
                endToken: blockCommentToken,
            };

            const procToken = createToken(TokenType.COLON, ":", createPosition(6, 1, 100));

            const procedure: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: procToken,
                endToken: procToken,
                name: createToken(
                    TokenType.IDENTIFIER,
                    "ImportantFunction",
                    createPosition(6, 12, 111)
                ),
                body: [],
            } as any;

            const program: ProgramNode = {
                kind: ASTNodeType.Program,
                startToken: blockCommentToken,
                endToken: procToken,
                body: [blockComment, procedure],
            };

            const result = formatter.format(program);

            // Block comment formatting should be preserved
            expect(result).toContain("/* Important function:");
            expect(result).toContain("- Validates input");
            expect(result).toContain("- Processes data");
            expect(result).toContain("- Returns result ;");
            expect(result).toContain("ImportantFunction");
        });
    });

    describe("SSL EBNF Grammar Compliance", () => {
        it("should handle all SSL comment types according to EBNF grammar", () => {
            // Test all comment types defined in the EBNF grammar
            const comments = [
                {
                    token: createToken(
                        TokenType.SINGLE_LINE_COMMENT,
                        "/* Single line comment according to EBNF ;",
                        createPosition(1, 1, 0)
                    ),
                    kind: ASTNodeType.SingleLineComment,
                },
                {
                    token: createToken(
                        TokenType.BLOCK_COMMENT,
                        "/* Block comment\nspanning multiple\nlines according to EBNF ;",
                        createPosition(3, 1, 50)
                    ),
                    kind: ASTNodeType.BlockComment,
                },
                {
                    token: createToken(
                        TokenType.REGION_COMMENT,
                        "/* region EBNF Compliance Test ;",
                        createPosition(7, 1, 150)
                    ),
                    kind: ASTNodeType.RegionComment,
                },
            ];

            const commentNodes = comments.map(({ token, kind }) => ({
                kind,
                startToken: token,
                endToken: token,
            })) as (SingleLineCommentNode | BlockCommentNode | RegionCommentNode)[];

            const procToken = createToken(TokenType.COLON, ":", createPosition(10, 1, 200));

            const procedure: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: procToken,
                endToken: procToken,
                name: createToken(
                    TokenType.IDENTIFIER,
                    "EBNFTestFunction",
                    createPosition(10, 12, 211)
                ),
                body: [],
            } as any;

            const program: ProgramNode = {
                kind: ASTNodeType.Program,
                startToken: comments[0].token,
                endToken: procToken,
                body: [...commentNodes, procedure],
            };

            const result = formatter.format(program);

            // All EBNF-compliant comment types should be preserved
            expect(result).toContain("/* Single line comment according to EBNF ;");
            expect(result).toContain("/* Block comment");
            expect(result).toContain("spanning multiple");
            expect(result).toContain("lines according to EBNF ;");
            expect(result).toContain("/* region EBNF Compliance Test ;");
            expect(result).toContain("EBNFTestFunction");
        });

        it("should maintain SSL syntax requirements for comments", () => {
            // SSL comments must start with /* and end with ;
            const sslCommentToken = createToken(
                TokenType.SINGLE_LINE_COMMENT,
                "/* This is a proper SSL comment with semicolon terminator ;",
                createPosition(1, 1, 0)
            );

            const sslComment: SingleLineCommentNode = {
                kind: ASTNodeType.SingleLineComment,
                startToken: sslCommentToken,
                endToken: sslCommentToken,
            };

            const program: ProgramNode = {
                kind: ASTNodeType.Program,
                startToken: sslCommentToken,
                endToken: sslCommentToken,
                body: [sslComment],
            };

            const result = formatter.format(program);

            // SSL syntax should be preserved
            expect(result).toMatch(/^\/\*.*;\s*$/); // Starts with /* and ends with ;
            expect(result).toContain("/* This is a proper SSL comment with semicolon terminator ;");
        });
    });
});
