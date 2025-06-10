import {
    ClassDefinitionNode,
    ClassDeclarationNode,
    InheritStatementNode,
    ClassFieldDeclarationNode,
    MethodDeclarationNode,
} from "../../src/parser/ast";
import { ASTNodeType } from "../../src/parser/ast/base";
import { Token, createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("AST Classes", () => {
    let mockToken: Token;
    let mockClassFieldNode: ClassFieldDeclarationNode;
    let mockMethodNode: MethodDeclarationNode;

    beforeEach(() => {
        mockToken = createToken(TokenType.IDENTIFIER, "TestClass", createPosition(1, 1, 0));

        mockClassFieldNode = {
            kind: ASTNodeType.ClassFieldDeclaration,
            startToken: mockToken,
            endToken: mockToken,
        };

        mockMethodNode = {
            kind: ASTNodeType.MethodDeclaration,
            startToken: mockToken,
            endToken: mockToken,
        };
    });

    describe("ClassDefinitionNode", () => {
        it("should have correct structure", () => {
            const classDeclaration: ClassDeclarationNode = {
                kind: ASTNodeType.ClassDeclaration,
                startToken: mockToken,
                endToken: mockToken,
                name: mockToken,
            };

            const classDefinition: ClassDefinitionNode = {
                kind: ASTNodeType.ClassDefinition,
                startToken: mockToken,
                endToken: mockToken,
                declaration: classDeclaration,
                members: [mockClassFieldNode],
            };

            expect(classDefinition.kind).toBe(ASTNodeType.ClassDefinition);
            expect(classDefinition.declaration).toBe(classDeclaration);
            expect(classDefinition.members).toHaveLength(1);
            expect(classDefinition.members[0]).toBe(mockClassFieldNode);
            expect(classDefinition.startToken).toBe(mockToken);
            expect(classDefinition.endToken).toBe(mockToken);
        });

        it("should support empty class members", () => {
            const classDeclaration: ClassDeclarationNode = {
                kind: ASTNodeType.ClassDeclaration,
                startToken: mockToken,
                endToken: mockToken,
                name: mockToken,
            };

            const classDefinition: ClassDefinitionNode = {
                kind: ASTNodeType.ClassDefinition,
                startToken: mockToken,
                endToken: mockToken,
                declaration: classDeclaration,
                members: [],
            };

            expect(classDefinition.members).toHaveLength(0);
            expect(Array.isArray(classDefinition.members)).toBe(true);
        });

        it("should support multiple class members", () => {
            const classDeclaration: ClassDeclarationNode = {
                kind: ASTNodeType.ClassDeclaration,
                startToken: mockToken,
                endToken: mockToken,
                name: mockToken,
            };

            const classDefinition: ClassDefinitionNode = {
                kind: ASTNodeType.ClassDefinition,
                startToken: mockToken,
                endToken: mockToken,
                declaration: classDeclaration,
                members: [mockClassFieldNode, mockMethodNode, mockClassFieldNode],
            };

            expect(classDefinition.members).toHaveLength(3);
            expect(classDefinition.members[0]).toBe(mockClassFieldNode);
            expect(classDefinition.members[1]).toBe(mockMethodNode);
            expect(classDefinition.members[2]).toBe(mockClassFieldNode);
        });

        it("should support inheritance", () => {
            const classDeclaration: ClassDeclarationNode = {
                kind: ASTNodeType.ClassDeclaration,
                startToken: mockToken,
                endToken: mockToken,
                name: mockToken,
            };

            const inheritStatement: InheritStatementNode = {
                kind: ASTNodeType.InheritStatement,
                startToken: mockToken,
                endToken: mockToken,
                className: mockToken,
            };

            const classDefinition: ClassDefinitionNode = {
                kind: ASTNodeType.ClassDefinition,
                startToken: mockToken,
                endToken: mockToken,
                declaration: classDeclaration,
                inherit: inheritStatement,
                members: [],
            };

            expect(classDefinition.inherit).toBeDefined();
            expect(classDefinition.inherit).toBe(inheritStatement);
        });
    });

    describe("ClassDeclarationNode", () => {
        it("should have correct structure", () => {
            const classDeclaration: ClassDeclarationNode = {
                kind: ASTNodeType.ClassDeclaration,
                startToken: mockToken,
                endToken: mockToken,
                name: mockToken,
            };

            expect(classDeclaration.kind).toBe(ASTNodeType.ClassDeclaration);
            expect(classDeclaration.name).toBe(mockToken);
            expect(classDeclaration.startToken).toBe(mockToken);
            expect(classDeclaration.endToken).toBe(mockToken);
        });
    });

    describe("InheritStatementNode", () => {
        it("should have correct structure", () => {
            const inheritStatement: InheritStatementNode = {
                kind: ASTNodeType.InheritStatement,
                startToken: mockToken,
                endToken: mockToken,
                className: mockToken,
            };

            expect(inheritStatement.kind).toBe(ASTNodeType.InheritStatement);
            expect(inheritStatement.className).toBe(mockToken);
            expect(inheritStatement.startToken).toBe(mockToken);
            expect(inheritStatement.endToken).toBe(mockToken);
        });
    });

    describe("ClassFieldDeclarationNode", () => {
        it("should have correct structure", () => {
            const fieldDeclaration: ClassFieldDeclarationNode = {
                kind: ASTNodeType.ClassFieldDeclaration,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(fieldDeclaration.kind).toBe(ASTNodeType.ClassFieldDeclaration);
            expect(fieldDeclaration.startToken).toBe(mockToken);
            expect(fieldDeclaration.endToken).toBe(mockToken);
        });
    });

    describe("MethodDeclarationNode", () => {
        it("should have correct structure", () => {
            const methodDeclaration: MethodDeclarationNode = {
                kind: ASTNodeType.MethodDeclaration,
                startToken: mockToken,
                endToken: mockToken,
            };

            expect(methodDeclaration.kind).toBe(ASTNodeType.MethodDeclaration);
            expect(methodDeclaration.startToken).toBe(mockToken);
            expect(methodDeclaration.endToken).toBe(mockToken);
        });
    });

    describe("Class node type consistency", () => {
        it("should have consistent naming for class-related types", () => {
            const classTypes = [
                ASTNodeType.ClassDefinition,
                ASTNodeType.ClassDeclaration,
                ASTNodeType.ClassFieldDeclaration,
            ];

            classTypes.forEach((type) => {
                expect(type).toContain("Class");
            });
        });

        it("should have declaration-related types", () => {
            const declarationTypes = [
                ASTNodeType.ClassDeclaration,
                ASTNodeType.ClassFieldDeclaration,
                ASTNodeType.MethodDeclaration,
            ];

            declarationTypes.forEach((type) => {
                expect(type).toContain("Declaration");
            });
        });

        it("should have inherit statement type", () => {
            expect(ASTNodeType.InheritStatement).toContain("Statement");
            expect(ASTNodeType.InheritStatement).toContain("Inherit");
        });
    });

    describe("Class hierarchy validation", () => {
        it("should properly represent object-oriented concepts", () => {
            // Test that all necessary OOP concepts are represented
            expect(ASTNodeType.ClassDefinition).toBeDefined();
            expect(ASTNodeType.ClassDeclaration).toBeDefined();
            expect(ASTNodeType.InheritStatement).toBeDefined();
            expect(ASTNodeType.ClassFieldDeclaration).toBeDefined();
            expect(ASTNodeType.MethodDeclaration).toBeDefined();
        });

        it("should support class composition", () => {
            const classDeclaration: ClassDeclarationNode = {
                kind: ASTNodeType.ClassDeclaration,
                startToken: mockToken,
                endToken: mockToken,
                name: mockToken,
            };

            // A class should be able to contain multiple members
            const classWithMethods: ClassDefinitionNode = {
                kind: ASTNodeType.ClassDefinition,
                startToken: mockToken,
                endToken: mockToken,
                declaration: classDeclaration,
                members: [
                    {
                        kind: ASTNodeType.ClassFieldDeclaration,
                        startToken: mockToken,
                        endToken: mockToken,
                    },
                    {
                        kind: ASTNodeType.MethodDeclaration,
                        startToken: mockToken,
                        endToken: mockToken,
                    },
                ],
            };

            expect(classWithMethods.members).toHaveLength(2);
            expect(classWithMethods.members[0].kind).toBe(ASTNodeType.ClassFieldDeclaration);
            expect(classWithMethods.members[1].kind).toBe(ASTNodeType.MethodDeclaration);
        });
    });
});
