import {
    FunctionCallNode,
    DirectFunctionCallNode,
    DoProcCallNode,
    ExecFunctionCallNode,
} from "../../src/parser/ast/functions";
import { ASTNodeType } from "../../src/parser/ast/base";
import { Token, createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("AST Functions", () => {
    let mockToken: Token;
    let mockExpressionNode: any;

    beforeEach(() => {
        mockToken = createToken(TokenType.IDENTIFIER, "testFunction", createPosition(1, 1, 0));

        mockExpressionNode = {
            kind: ASTNodeType.Primary,
            startToken: mockToken,
            endToken: mockToken,
        };
    });

    describe("FunctionCallNode", () => {
        it("should have correct structure", () => {
            const functionCall: FunctionCallNode = {
                kind: ASTNodeType.FunctionCall,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(functionCall.kind).toBe(ASTNodeType.FunctionCall);
            expect(functionCall.startToken).toBe(mockToken);
            expect(functionCall.endToken).toBe(mockToken);
        });
    });

    describe("DirectFunctionCallNode", () => {
        it("should have correct structure", () => {
            const directFunctionCall: DirectFunctionCallNode = {
                kind: ASTNodeType.DirectFunctionCall,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(directFunctionCall.kind).toBe(ASTNodeType.DirectFunctionCall);
            expect(directFunctionCall.startToken).toBe(mockToken);
            expect(directFunctionCall.endToken).toBe(mockToken);
        });
    });

    describe("DoProcCallNode", () => {
        it("should have correct structure", () => {
            const doProcCall: DoProcCallNode = {
                kind: ASTNodeType.DoProcCall,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(doProcCall.kind).toBe(ASTNodeType.DoProcCall);
            expect(doProcCall.startToken).toBe(mockToken);
            expect(doProcCall.endToken).toBe(mockToken);
        });
    });

    describe("ExecFunctionCallNode", () => {
        it("should have correct structure", () => {
            const execFunctionCall: ExecFunctionCallNode = {
                kind: ASTNodeType.ExecFunctionCall,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(execFunctionCall.kind).toBe(ASTNodeType.ExecFunctionCall);
            expect(execFunctionCall.startToken).toBe(mockToken);
            expect(execFunctionCall.endToken).toBe(mockToken);
        });
    });

    describe("Function call type consistency", () => {
        it("should have consistent naming for function call types", () => {
            const functionCallTypes = [
                ASTNodeType.FunctionCall,
                ASTNodeType.DirectFunctionCall,
                ASTNodeType.ExecFunctionCall,
            ];

            functionCallTypes.forEach((type) => {
                expect(type).toContain("FunctionCall");
            });
        });

        it("should have procedure call types", () => {
            expect(ASTNodeType.DoProcCall).toContain("ProcCall");
            expect(ASTNodeType.DoProcCall).toContain("Do");
        });

        it("should have execution-related types", () => {
            expect(ASTNodeType.ExecFunctionCall).toContain("Exec");
            expect(ASTNodeType.DirectFunctionCall).toContain("Direct");
        });
    });

    describe("Function call hierarchy", () => {
        it("should represent different call mechanisms", () => {
            // Test that all necessary function call concepts are represented
            expect(ASTNodeType.FunctionCall).toBeDefined();
            expect(ASTNodeType.DirectFunctionCall).toBeDefined();
            expect(ASTNodeType.DoProcCall).toBeDefined();
            expect(ASTNodeType.ExecFunctionCall).toBeDefined();
        });

        it("should have unique identifiers for each call type", () => {
            const callTypes = [
                ASTNodeType.FunctionCall,
                ASTNodeType.DirectFunctionCall,
                ASTNodeType.DoProcCall,
                ASTNodeType.ExecFunctionCall,
            ];

            const uniqueTypes = new Set(callTypes);
            expect(callTypes.length).toBe(uniqueTypes.size);
        });
    });

    describe("Function call validation", () => {
        it("should support different function call patterns", () => {
            const functionCallVariants = [
                {
                    kind: ASTNodeType.FunctionCall,
                    name: "regular function call",
                },
                {
                    kind: ASTNodeType.DirectFunctionCall,
                    name: "direct function call",
                },
                {
                    kind: ASTNodeType.DoProcCall,
                    name: "procedure call",
                },
                {
                    kind: ASTNodeType.ExecFunctionCall,
                    name: "exec function call",
                },
            ];

            functionCallVariants.forEach((variant) => {
                const callNode = {
                    kind: variant.kind,
                    startToken: mockToken,
                    endToken: mockToken,
                };

                expect(callNode.kind).toBe(variant.kind);
                expect(callNode.startToken).toBe(mockToken);
                expect(callNode.endToken).toBe(mockToken);
            });
        });
    });
});
