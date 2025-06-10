import {
    SwitchStatementNode,
    BeginCaseStatementNode,
    CaseBlockNode,
    CaseStatementNode,
    OtherwiseBlockNode,
    OtherwiseStatementNode,
    EndCaseStatementNode,
    ExitCaseStatementNode,
} from "../../src/parser/ast/switchCase";
import { ASTNodeType, createBaseNode } from "../../src/parser/ast/base";
import { createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("AST Switch Case", () => {
    let mockToken: any;

    beforeEach(() => {
        mockToken = createToken(TokenType.BEGINCASE, "begincase", createPosition(1, 1, 0));
    });

    describe("SwitchStatementNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.SwitchStatement,
                mockToken,
                mockToken
            ) as SwitchStatementNode;
            expect(node.kind).toBe(ASTNodeType.SwitchStatement);
        });
    });

    describe("BeginCaseStatementNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.BeginCaseStatement,
                mockToken,
                mockToken
            ) as BeginCaseStatementNode;
            expect(node.kind).toBe(ASTNodeType.BeginCaseStatement);
        });
    });

    describe("CaseBlockNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.CaseBlock,
                mockToken,
                mockToken
            ) as CaseBlockNode;
            expect(node.kind).toBe(ASTNodeType.CaseBlock);
        });
    });

    describe("CaseStatementNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.CaseStatement,
                mockToken,
                mockToken
            ) as CaseStatementNode;
            expect(node.kind).toBe(ASTNodeType.CaseStatement);
        });
    });

    describe("OtherwiseBlockNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.OtherwiseBlock,
                mockToken,
                mockToken
            ) as OtherwiseBlockNode;
            expect(node.kind).toBe(ASTNodeType.OtherwiseBlock);
        });
    });

    describe("OtherwiseStatementNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.OtherwiseStatement,
                mockToken,
                mockToken
            ) as OtherwiseStatementNode;
            expect(node.kind).toBe(ASTNodeType.OtherwiseStatement);
        });
    });

    describe("EndCaseStatementNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.EndCaseStatement,
                mockToken,
                mockToken
            ) as EndCaseStatementNode;
            expect(node.kind).toBe(ASTNodeType.EndCaseStatement);
        });
    });

    describe("ExitCaseStatementNode", () => {
        it("should have correct kind", () => {
            const node = createBaseNode(
                ASTNodeType.ExitCaseStatement,
                mockToken,
                mockToken
            ) as ExitCaseStatementNode;
            expect(node.kind).toBe(ASTNodeType.ExitCaseStatement);
        });
    });

    describe("Switch case structure validation", () => {
        it("should have all required switch case node types", () => {
            const switchTypes = [
                ASTNodeType.SwitchStatement,
                ASTNodeType.BeginCaseStatement,
                ASTNodeType.CaseBlock,
                ASTNodeType.CaseStatement,
                ASTNodeType.OtherwiseBlock,
                ASTNodeType.OtherwiseStatement,
                ASTNodeType.EndCaseStatement,
                ASTNodeType.ExitCaseStatement,
            ];

            switchTypes.forEach((type) => {
                expect(type).toBeDefined();
                const node = createBaseNode(type, mockToken, mockToken);
                expect(node.kind).toBe(type);
            });
        });

        it("should have consistent naming for case types", () => {
            const caseTypes = [
                ASTNodeType.BeginCaseStatement,
                ASTNodeType.CaseBlock,
                ASTNodeType.CaseStatement,
                ASTNodeType.EndCaseStatement,
                ASTNodeType.ExitCaseStatement,
            ];

            caseTypes.forEach((type) => {
                expect(type).toContain("Case");
            });
        });

        it("should have consistent naming for otherwise types", () => {
            const otherwiseTypes = [ASTNodeType.OtherwiseBlock, ASTNodeType.OtherwiseStatement];

            otherwiseTypes.forEach((type) => {
                expect(type).toContain("Otherwise");
            });
        });

        it("should support complete switch case structure", () => {
            // Test that we can create a complete switch case structure
            const switchNode = createBaseNode(ASTNodeType.SwitchStatement, mockToken, mockToken);
            const beginNode = createBaseNode(ASTNodeType.BeginCaseStatement, mockToken, mockToken);
            const caseNode = createBaseNode(ASTNodeType.CaseStatement, mockToken, mockToken);
            const otherwiseNode = createBaseNode(
                ASTNodeType.OtherwiseStatement,
                mockToken,
                mockToken
            );
            const endNode = createBaseNode(ASTNodeType.EndCaseStatement, mockToken, mockToken);

            expect(switchNode.kind).toBe(ASTNodeType.SwitchStatement);
            expect(beginNode.kind).toBe(ASTNodeType.BeginCaseStatement);
            expect(caseNode.kind).toBe(ASTNodeType.CaseStatement);
            expect(otherwiseNode.kind).toBe(ASTNodeType.OtherwiseStatement);
            expect(endNode.kind).toBe(ASTNodeType.EndCaseStatement);
        });

        it("should have proper enum values", () => {
            expect(ASTNodeType.SwitchStatement).toBe("SwitchStatement");
            expect(ASTNodeType.BeginCaseStatement).toBe("BeginCaseStatement");
            expect(ASTNodeType.CaseStatement).toBe("CaseStatement");
            expect(ASTNodeType.OtherwiseStatement).toBe("OtherwiseStatement");
            expect(ASTNodeType.EndCaseStatement).toBe("EndCaseStatement");
            expect(ASTNodeType.ExitCaseStatement).toBe("ExitCaseStatement");
        });

        it("should distinguish between blocks and statements", () => {
            const blockTypes = [ASTNodeType.CaseBlock, ASTNodeType.OtherwiseBlock];
            const statementTypes = [
                ASTNodeType.BeginCaseStatement,
                ASTNodeType.CaseStatement,
                ASTNodeType.OtherwiseStatement,
                ASTNodeType.EndCaseStatement,
                ASTNodeType.ExitCaseStatement,
            ];

            blockTypes.forEach((type) => {
                expect(type).toContain("Block");
            });

            statementTypes.forEach((type) => {
                expect(type).toContain("Statement");
            });
        });
    });
});
