import {
    LabelStatementNode,
    RegionBlockNode,
    RegionStartNode,
    RegionEndNode,
    InlineCodeBlockNode,
    InlineCodeStartNode,
    InlineCodeEndNode,
    DynamicCodeExecutionNode,
    BranchStatementNode,
} from "../../src/parser/ast/special";
import { ASTNodeType, createBaseNode } from "../../src/parser/ast/base";
import { createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("AST Special", () => {
    let mockToken: any;

    beforeEach(() => {
        mockToken = createToken(TokenType.IDENTIFIER, "label", createPosition(1, 1, 0));
    });

    describe("LabelStatementNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.LabelStatement,
                mockToken,
                mockToken
            ) as LabelStatementNode;
            expect(node.kind).toBe(ASTNodeType.LabelStatement);
        });
    });

    describe("RegionBlockNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.RegionBlock,
                mockToken,
                mockToken
            ) as RegionBlockNode;
            expect(node.kind).toBe(ASTNodeType.RegionBlock);
        });
    });

    describe("RegionStartNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.RegionStart,
                mockToken,
                mockToken
            ) as RegionStartNode;
            expect(node.kind).toBe(ASTNodeType.RegionStart);
        });
    });

    describe("RegionEndNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.RegionEnd,
                mockToken,
                mockToken
            ) as RegionEndNode;
            expect(node.kind).toBe(ASTNodeType.RegionEnd);
        });
    });

    describe("InlineCodeBlockNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.InlineCodeBlock,
                mockToken,
                mockToken
            ) as InlineCodeBlockNode;
            expect(node.kind).toBe(ASTNodeType.InlineCodeBlock);
        });
    });

    describe("InlineCodeStartNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.InlineCodeStart,
                mockToken,
                mockToken
            ) as InlineCodeStartNode;
            expect(node.kind).toBe(ASTNodeType.InlineCodeStart);
        });
    });

    describe("InlineCodeEndNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.InlineCodeEnd,
                mockToken,
                mockToken
            ) as InlineCodeEndNode;
            expect(node.kind).toBe(ASTNodeType.InlineCodeEnd);
        });
    });

    describe("DynamicCodeExecutionNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.DynamicCodeExecution,
                mockToken,
                mockToken
            ) as DynamicCodeExecutionNode;
            expect(node.kind).toBe(ASTNodeType.DynamicCodeExecution);
        });
    });

    describe("BranchStatementNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.BranchStatement,
                mockToken,
                mockToken
            ) as BranchStatementNode;
            expect(node.kind).toBe(ASTNodeType.BranchStatement);
        });
    });

    describe("Special structures validation", () => {
        it("should have all required special structure node types", () => {
            const specialTypes = [
                ASTNodeType.LabelStatement,
                ASTNodeType.RegionBlock,
                ASTNodeType.RegionStart,
                ASTNodeType.RegionEnd,
                ASTNodeType.InlineCodeBlock,
                ASTNodeType.InlineCodeStart,
                ASTNodeType.InlineCodeEnd,
                ASTNodeType.DynamicCodeExecution,
                ASTNodeType.BranchStatement,
            ];

            specialTypes.forEach((type) => {
                expect(type).toBeDefined();
                const node = createBaseNode(type, mockToken, mockToken);
                expect(node.kind).toBe(type);
            });
        });

        it("should have consistent naming for region types", () => {
            const regionTypes = [
                ASTNodeType.RegionBlock,
                ASTNodeType.RegionStart,
                ASTNodeType.RegionEnd,
            ];

            regionTypes.forEach((type) => {
                expect(type).toContain("Region");
            });
        });

        it("should have consistent naming for inline code types", () => {
            const inlineTypes = [
                ASTNodeType.InlineCodeBlock,
                ASTNodeType.InlineCodeStart,
                ASTNodeType.InlineCodeEnd,
            ];

            inlineTypes.forEach((type) => {
                expect(type).toContain("InlineCode");
            });
        });

        it("should support special SSL features", () => {
            // Test dynamic code execution (ExecUDF)
            const dynamicNode = createBaseNode(
                ASTNodeType.DynamicCodeExecution,
                mockToken,
                mockToken
            );
            expect(dynamicNode.kind).toBe(ASTNodeType.DynamicCodeExecution);

            // Test label statements
            const labelNode = createBaseNode(ASTNodeType.LabelStatement, mockToken, mockToken);
            expect(labelNode.kind).toBe(ASTNodeType.LabelStatement);

            // Test branch statements
            const branchNode = createBaseNode(ASTNodeType.BranchStatement, mockToken, mockToken);
            expect(branchNode.kind).toBe(ASTNodeType.BranchStatement);
        });

        it("should have proper enum values", () => {
            expect(ASTNodeType.LabelStatement).toBe("LabelStatement");
            expect(ASTNodeType.RegionBlock).toBe("RegionBlock");
            expect(ASTNodeType.DynamicCodeExecution).toBe("DynamicCodeExecution");
            expect(ASTNodeType.BranchStatement).toBe("BranchStatement");
        });
    });
});
