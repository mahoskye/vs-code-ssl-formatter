import {
    ProcedureStatementNode,
    ProcedureStartNode,
    ProcedureEndNode,
    ParameterDeclarationNode,
    DefaultParameterDeclarationNode,
    DefaultParameterListNode,
    ParameterListNode,
} from "../../src/parser/ast/procedures";
import { ASTNodeType } from "../../src/parser/ast/base";
import { Token, createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("AST Procedures", () => {
    let mockToken: Token;
    let mockStatementNode: any;
    let mockParameterListNode: ParameterListNode;
    let mockDefaultParameterListNode: DefaultParameterListNode;

    beforeEach(() => {
        mockToken = createToken(TokenType.IDENTIFIER, "TestProcedure", createPosition(1, 1, 0));

        mockStatementNode = {
            kind: ASTNodeType.Assignment,
            startToken: mockToken,
            endToken: mockToken,
        };

        mockParameterListNode = {
            kind: ASTNodeType.ParameterList,
            startToken: mockToken,
            endToken: mockToken,
            identifiers: [mockToken],
        };

        mockDefaultParameterListNode = {
            kind: ASTNodeType.DefaultParameterList,
            startToken: mockToken,
            endToken: mockToken,
            pairs: [{ identifier: mockToken, defaultValue: "defaultValue" }],
        };
    });

    describe("ProcedureStatementNode", () => {
        it("should have correct structure", () => {
            const procedureStatement: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: mockToken,
                endToken: mockToken,
                name: mockToken,
                body: [mockStatementNode],
            };

            expect(procedureStatement.kind).toBe(ASTNodeType.ProcedureStatement);
            expect(procedureStatement.body).toHaveLength(1);
            expect(procedureStatement.body[0]).toBe(mockStatementNode);
            expect(procedureStatement.startToken).toBe(mockToken);
            expect(procedureStatement.endToken).toBe(mockToken);
        });

        it("should support empty procedure body", () => {
            const procedureStatement: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: mockToken,
                endToken: mockToken,
                name: mockToken,
                body: [],
            };

            expect(procedureStatement.body).toHaveLength(0);
            expect(Array.isArray(procedureStatement.body)).toBe(true);
        });

        it("should support multiple statements in body", () => {
            const procedureStatement: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: mockToken,
                endToken: mockToken,
                name: mockToken,
                body: [mockStatementNode, mockStatementNode, mockStatementNode],
            };

            expect(procedureStatement.body).toHaveLength(3);
            procedureStatement.body.forEach((stmt) => {
                expect(stmt).toBe(mockStatementNode);
            });
        });
    });

    describe("ProcedureStartNode", () => {
        it("should have correct structure", () => {
            const procedureStart: ProcedureStartNode = {
                kind: ASTNodeType.ProcedureStart,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(procedureStart.kind).toBe(ASTNodeType.ProcedureStart);
            expect(procedureStart.startToken).toBe(mockToken);
            expect(procedureStart.endToken).toBe(mockToken);
        });
    });

    describe("ProcedureEndNode", () => {
        it("should have correct structure", () => {
            const procedureEnd: ProcedureEndNode = {
                kind: ASTNodeType.ProcedureEnd,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(procedureEnd.kind).toBe(ASTNodeType.ProcedureEnd);
            expect(procedureEnd.startToken).toBe(mockToken);
            expect(procedureEnd.endToken).toBe(mockToken);
        });
    });

    describe("ParameterDeclarationNode", () => {
        it("should have correct structure", () => {
            const parameterDeclaration: ParameterDeclarationNode = {
                kind: ASTNodeType.ParameterDeclaration,
                startToken: mockToken,
                endToken: mockToken,
                parameters: mockParameterListNode,
            };

            expect(parameterDeclaration.kind).toBe(ASTNodeType.ParameterDeclaration);
            expect(parameterDeclaration.parameters).toBe(mockParameterListNode);
            expect(parameterDeclaration.startToken).toBe(mockToken);
            expect(parameterDeclaration.endToken).toBe(mockToken);
        });
    });

    describe("DefaultParameterDeclarationNode", () => {
        it("should have correct structure", () => {
            const defaultParameterDeclaration: DefaultParameterDeclarationNode = {
                kind: ASTNodeType.DefaultParameterDeclaration,
                startToken: mockToken,
                endToken: mockToken,
                parameters: mockDefaultParameterListNode,
            };

            expect(defaultParameterDeclaration.kind).toBe(ASTNodeType.DefaultParameterDeclaration);
            expect(defaultParameterDeclaration.parameters).toBe(mockDefaultParameterListNode);
            expect(defaultParameterDeclaration.startToken).toBe(mockToken);
            expect(defaultParameterDeclaration.endToken).toBe(mockToken);
        });
    });

    describe("DefaultParameterListNode", () => {
        it("should have correct structure", () => {
            const defaultParameterList: DefaultParameterListNode = {
                kind: ASTNodeType.DefaultParameterList,
                startToken: mockToken,
                endToken: mockToken,
                pairs: [{ identifier: mockToken, defaultValue: "test" }],
            };

            expect(defaultParameterList.kind).toBe(ASTNodeType.DefaultParameterList);
            expect(defaultParameterList.pairs).toHaveLength(1);
            expect(defaultParameterList.pairs[0].identifier).toBe(mockToken);
            expect(defaultParameterList.startToken).toBe(mockToken);
            expect(defaultParameterList.endToken).toBe(mockToken);
        });

        it("should support empty parameter list", () => {
            const defaultParameterList: DefaultParameterListNode = {
                kind: ASTNodeType.DefaultParameterList,
                startToken: mockToken,
                endToken: mockToken,
                pairs: [],
            };

            expect(defaultParameterList.pairs).toHaveLength(0);
            expect(Array.isArray(defaultParameterList.pairs)).toBe(true);
        });

        it("should support multiple parameters", () => {
            const defaultParameterList: DefaultParameterListNode = {
                kind: ASTNodeType.DefaultParameterList,
                startToken: mockToken,
                endToken: mockToken,
                pairs: [
                    { identifier: mockToken, defaultValue: "test1" },
                    { identifier: mockToken, defaultValue: "test2" },
                    { identifier: mockToken, defaultValue: "test3" }
                ],
            };

            expect(defaultParameterList.pairs).toHaveLength(3);
            defaultParameterList.pairs.forEach((pair) => {
                expect(pair.identifier).toBe(mockToken);
            });
        });
    });

    describe("ParameterListNode", () => {
        it("should have correct structure", () => {
            const parameterList: ParameterListNode = {
                kind: ASTNodeType.ParameterList,
                startToken: mockToken,
                endToken: mockToken,
                identifiers: [mockToken],
            };

            expect(parameterList.kind).toBe(ASTNodeType.ParameterList);
            expect(parameterList.identifiers).toHaveLength(1);
            expect(parameterList.identifiers[0]).toBe(mockToken);
            expect(parameterList.startToken).toBe(mockToken);
            expect(parameterList.endToken).toBe(mockToken);
        });

        it("should support empty parameter list", () => {
            const parameterList: ParameterListNode = {
                kind: ASTNodeType.ParameterList,
                startToken: mockToken,
                endToken: mockToken,
                identifiers: [],
            };

            expect(parameterList.identifiers).toHaveLength(0);
            expect(Array.isArray(parameterList.identifiers)).toBe(true);
        });

        it("should support multiple parameters", () => {
            const parameterList: ParameterListNode = {
                kind: ASTNodeType.ParameterList,
                startToken: mockToken,
                endToken: mockToken,
                identifiers: [mockToken, mockToken, mockToken],
            };

            expect(parameterList.identifiers).toHaveLength(3);
            parameterList.identifiers.forEach((param) => {
                expect(param).toBe(mockToken);
            });
        });
    });

    describe("Procedure node type consistency", () => {
        it("should have consistent naming for procedure-related types", () => {
            const procedureTypes = [
                ASTNodeType.ProcedureStatement,
                ASTNodeType.ProcedureStart,
                ASTNodeType.ProcedureEnd,
            ];

            procedureTypes.forEach((type) => {
                expect(type).toContain("Procedure");
            });
        });

        it("should have parameter-related types", () => {
            const parameterTypes = [
                ASTNodeType.ParameterDeclaration,
                ASTNodeType.DefaultParameterDeclaration,
                ASTNodeType.ParameterList,
                ASTNodeType.DefaultParameterList,
            ];

            parameterTypes.forEach((type) => {
                expect(type).toContain("Parameter");
            });
        });

        it("should have declaration types", () => {
            const declarationTypes = [
                ASTNodeType.ParameterDeclaration,
                ASTNodeType.DefaultParameterDeclaration,
            ];

            declarationTypes.forEach((type) => {
                expect(type).toContain("Declaration");
            });
        });

        it("should have list types", () => {
            const listTypes = [ASTNodeType.ParameterList, ASTNodeType.DefaultParameterList];

            listTypes.forEach((type) => {
                expect(type).toContain("List");
            });
        });
    });

    describe("Procedure structure validation", () => {
        it("should represent complete procedure structure", () => {
            // Test that all necessary procedure concepts are represented
            expect(ASTNodeType.ProcedureStatement).toBeDefined();
            expect(ASTNodeType.ProcedureStart).toBeDefined();
            expect(ASTNodeType.ProcedureEnd).toBeDefined();
            expect(ASTNodeType.ParameterDeclaration).toBeDefined();
            expect(ASTNodeType.ParameterList).toBeDefined();
        });

        it("should support procedure with parameters", () => {
            const paramList: ParameterListNode = {
                kind: ASTNodeType.ParameterList,
                startToken: mockToken,
                endToken: mockToken,
                identifiers: [mockToken, mockToken],
            };

            expect(paramList.identifiers).toHaveLength(2);
            expect(paramList.identifiers[0]).toBe(mockToken);
            expect(paramList.identifiers[1]).toBe(mockToken);
        });
    });
});
