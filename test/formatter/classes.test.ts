/**
 * Tests for Class Formatter
 *
 * Test suite covering:
 * - :CLASS declaration formatting
 * - :INHERIT statement formatting
 * - Class field declaration formatting
 * - Method declaration formatting
 * - Complete class definition formatting
 * - EBNF grammar compliance
 */

import { ClassFormatter } from "../../src/formatter/classes";
import { OutputBuilder } from "../../src/formatter/outputBuilder";
import { defaultFormatterOptions, FormatterOptions } from "../../src/formatter/options";
import {
    ClassDefinitionNode,
    ClassDeclarationNode,
    InheritStatementNode,
    ClassFieldDeclarationNode,
    MethodDeclarationNode,
} from "../../src/parser/ast/classes";
import { IdentifierListNode } from "../../src/parser/ast/lists";
import { ProcedureStatementNode } from "../../src/parser/ast/procedures";
import { ASTNodeType, createBaseNode } from "../../src/parser/ast/base";
import { createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("ClassFormatter", () => {
    let formatter: ClassFormatter;
    let output: OutputBuilder;
    let options: FormatterOptions;

    beforeEach(() => {
        options = { ...defaultFormatterOptions };
        output = new OutputBuilder(options);
        formatter = new ClassFormatter(output, options);
    });

    describe("formatClassDeclaration", () => {
        it("should format simple class declaration", () => {
            const classNameToken = createToken(
                TokenType.IDENTIFIER,
                "MyClass",
                createPosition(1, 8, 7)
            );

            const node: ClassDeclarationNode = {
                kind: ASTNodeType.ClassDeclaration,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 16, 15)),
                name: classNameToken,
            };

            formatter.formatClassDeclaration(node);

            const result = output.getOutput().trim();
            expect(result).toBe(":CLASS MyClass;");
        });

        it("should format class declaration with Hungarian notation", () => {
            const classNameToken = createToken(
                TokenType.IDENTIFIER,
                "clsCar",
                createPosition(1, 8, 7)
            );

            const node: ClassDeclarationNode = {
                kind: ASTNodeType.ClassDeclaration,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 15, 14)),
                name: classNameToken,
            };

            formatter.formatClassDeclaration(node);

            const result = output.getOutput().trim();
            expect(result).toBe(":CLASS clsCar;");
        });

        it("should handle missing class name gracefully", () => {
            const node: ClassDeclarationNode = {
                kind: ASTNodeType.ClassDeclaration,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 8, 7)),
                name: undefined as any,
            };

            formatter.formatClassDeclaration(node);

            const result = output.getOutput().trim();
            expect(result).toBe(":CLASS ;");
        });
    });

    describe("formatInheritStatement", () => {
        it("should format simple inheritance", () => {
            const parentClassToken = createToken(
                TokenType.IDENTIFIER,
                "BaseClass",
                createPosition(1, 10, 9)
            );

            const node: InheritStatementNode = {
                kind: ASTNodeType.InheritStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 20, 19)),
                className: parentClassToken,
            };

            formatter.formatInheritStatement(node);

            const result = output.getOutput().trim();
            expect(result).toBe(":INHERIT BaseClass;");
        });

        it("should format qualified inheritance (Category.ClassName)", () => {
            const qualifiedClassToken = createToken(
                TokenType.IDENTIFIER,
                "Research.clsVehicle",
                createPosition(1, 10, 9)
            );

            const node: InheritStatementNode = {
                kind: ASTNodeType.InheritStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 30, 29)),
                className: qualifiedClassToken,
            };

            formatter.formatInheritStatement(node);

            const result = output.getOutput().trim();
            expect(result).toBe(":INHERIT Research.clsVehicle;");
        });

        it("should handle missing class name gracefully", () => {
            const node: InheritStatementNode = {
                kind: ASTNodeType.InheritStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 9, 8)),
                className: undefined as any,
            };

            formatter.formatInheritStatement(node);

            const result = output.getOutput().trim();
            expect(result).toBe(":INHERIT ;");
        });
    });

    describe("formatClassFieldDeclaration", () => {
        it("should format single field declaration", () => {
            const identifiers = [
                createToken(TokenType.IDENTIFIER, "MSRPPrice", createPosition(1, 9, 8)),
            ];

            const identifierList: IdentifierListNode = {
                kind: ASTNodeType.IdentifierList,
                identifiers,
                startToken: identifiers[0],
                endToken: identifiers[identifiers.length - 1],
            };

            const node: ClassFieldDeclarationNode = {
                kind: ASTNodeType.ClassFieldDeclaration,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 19, 18)),
                identifiers: identifierList,
            } as any;

            formatter.formatClassFieldDeclaration(node);

            const result = output.getOutput().trim();
            expect(result).toBe(":DECLARE MSRPPrice;");
        });

        it("should format multiple field declarations", () => {
            const identifiers = [
                createToken(TokenType.IDENTIFIER, "MSRPPrice", createPosition(1, 9, 8)),
                createToken(TokenType.IDENTIFIER, "Wheels", createPosition(1, 20, 19)),
                createToken(TokenType.IDENTIFIER, "Color", createPosition(1, 28, 27)),
            ];

            const identifierList: IdentifierListNode = {
                kind: ASTNodeType.IdentifierList,
                identifiers,
                startToken: identifiers[0],
                endToken: identifiers[identifiers.length - 1],
            };

            const node: ClassFieldDeclarationNode = {
                kind: ASTNodeType.ClassFieldDeclaration,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 34, 33)),
                identifiers: identifierList,
            } as any;

            formatter.formatClassFieldDeclaration(node);

            const result = output.getOutput().trim();
            expect(result).toBe(":DECLARE MSRPPrice, Wheels, Color;");
        });

        it("should format fields with Hungarian notation", () => {
            const identifiers = [
                createToken(TokenType.IDENTIFIER, "sName", createPosition(1, 9, 8)),
                createToken(TokenType.IDENTIFIER, "nCount", createPosition(1, 16, 15)),
                createToken(TokenType.IDENTIFIER, "bIsValid", createPosition(1, 24, 23)),
            ];

            const identifierList: IdentifierListNode = {
                kind: ASTNodeType.IdentifierList,
                identifiers,
                startToken: identifiers[0],
                endToken: identifiers[identifiers.length - 1],
            };

            const node: ClassFieldDeclarationNode = {
                kind: ASTNodeType.ClassFieldDeclaration,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 33, 32)),
                identifiers: identifierList,
            } as any;

            formatter.formatClassFieldDeclaration(node);

            const result = output.getOutput().trim();
            expect(result).toBe(":DECLARE sName, nCount, bIsValid;");
        });

        it("should handle empty field list gracefully", () => {
            const identifierList: IdentifierListNode = {
                kind: ASTNodeType.IdentifierList,
                identifiers: [],
                startToken: createToken(TokenType.IDENTIFIER, "", createPosition(1, 9, 8)),
                endToken: createToken(TokenType.IDENTIFIER, "", createPosition(1, 9, 8)),
            };

            const node: ClassFieldDeclarationNode = {
                kind: ASTNodeType.ClassFieldDeclaration,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 9, 8)),
                identifiers: identifierList,
            } as any;

            formatter.formatClassFieldDeclaration(node);

            const result = output.getOutput().trim();
            expect(result).toBe(":DECLARE ;");
        });
    });

    describe("formatMethodDeclaration", () => {
        it("should format simple method declaration", () => {
            const methodNameToken = createToken(
                TokenType.IDENTIFIER,
                "GetPrice",
                createPosition(1, 11, 10)
            );

            const procedureNode: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.ENDPROC, ":ENDPROC", createPosition(3, 1, 0)),
                name: methodNameToken,
                body: [],
            } as any;

            const node: MethodDeclarationNode = {
                kind: ASTNodeType.MethodDeclaration,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.ENDPROC, ":ENDPROC", createPosition(3, 9, 8)),
                procedure: procedureNode,
            } as any;

            formatter.formatMethodDeclaration(node);

            const result = output.getOutput().trim();
            const lines = result.split("\n");
            expect(lines[0]).toBe(":PROCEDURE GetPrice;");
            expect(lines[1]).toBe(":ENDPROC;");
        });

        it("should format method with body statements", () => {
            const methodNameToken = createToken(
                TokenType.IDENTIFIER,
                "Constructor",
                createPosition(1, 11, 10)
            );

            const mockStatement = {
                kind: ASTNodeType.Assignment,
                startToken: createToken(TokenType.IDENTIFIER, "Me:Wheels", createPosition(2, 1, 0)),
                endToken: createToken(TokenType.NUMBER, "4", createPosition(2, 13, 12)),
            } as any;

            const procedureNode: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.ENDPROC, ":ENDPROC", createPosition(3, 1, 0)),
                name: methodNameToken,
                body: [mockStatement],
            } as any;

            const node: MethodDeclarationNode = {
                kind: ASTNodeType.MethodDeclaration,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.ENDPROC, ":ENDPROC", createPosition(3, 9, 8)),
                procedure: procedureNode,
            } as any;

            formatter.formatMethodDeclaration(node);

            const result = output.getOutput().trim();
            const lines = result.split("\n").filter((line) => line.trim() !== "");
            expect(lines[0]).toBe(":PROCEDURE Constructor;");
            expect(lines[1]).toContain("Me:Wheels"); // Body statement should be indented
            expect(lines[2]).toBe(":ENDPROC;");
        });

        it("should handle method without procedure gracefully", () => {
            const node: MethodDeclarationNode = {
                kind: ASTNodeType.MethodDeclaration,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.ENDPROC, ":ENDPROC", createPosition(3, 9, 8)),
                procedure: undefined as any,
            } as any;

            formatter.formatMethodDeclaration(node);

            const result = output.getOutput().trim();
            expect(result.length).toBeGreaterThan(0);
        });
    });

    describe("formatClassDefinition", () => {
        it("should format complete class definition without inheritance", () => {
            const classNameToken = createToken(
                TokenType.IDENTIFIER,
                "MyClass",
                createPosition(1, 8, 7)
            );

            const classDeclaration: ClassDeclarationNode = {
                kind: ASTNodeType.ClassDeclaration,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 16, 15)),
                name: classNameToken,
            };

            const fieldIdentifiers = [
                createToken(TokenType.IDENTIFIER, "field1", createPosition(3, 9, 8)),
                createToken(TokenType.IDENTIFIER, "field2", createPosition(3, 17, 16)),
            ];

            const identifierList: IdentifierListNode = {
                kind: ASTNodeType.IdentifierList,
                identifiers: fieldIdentifiers,
                startToken: fieldIdentifiers[0],
                endToken: fieldIdentifiers[fieldIdentifiers.length - 1],
            };

            const fieldDeclaration: ClassFieldDeclarationNode = {
                kind: ASTNodeType.ClassFieldDeclaration,
                startToken: createToken(TokenType.COLON, ":", createPosition(3, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(3, 24, 23)),
                identifiers: identifierList,
            } as any;

            const node: ClassDefinitionNode = {
                kind: ASTNodeType.ClassDefinition,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(3, 24, 23)),
                declaration: classDeclaration,
                members: [fieldDeclaration],
            };

            formatter.formatClassDefinition(node);

            const result = output.getOutput().trim();
            const lines = result.split("\n").filter((line) => line.trim() !== "");
            expect(lines[0]).toBe(":CLASS MyClass;");
            expect(lines[1]).toBe(":DECLARE field1, field2;");
        });

        it("should format complete class definition with inheritance", () => {
            const classNameToken = createToken(
                TokenType.IDENTIFIER,
                "clsCar",
                createPosition(1, 8, 7)
            );

            const classDeclaration: ClassDeclarationNode = {
                kind: ASTNodeType.ClassDeclaration,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 15, 14)),
                name: classNameToken,
            };

            const parentClassToken = createToken(
                TokenType.IDENTIFIER,
                "Research.clsVehicle",
                createPosition(2, 10, 9)
            );

            const inheritStatement: InheritStatementNode = {
                kind: ASTNodeType.InheritStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(2, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(2, 30, 29)),
                className: parentClassToken,
            };

            const fieldIdentifiers = [
                createToken(TokenType.IDENTIFIER, "MSRPPrice", createPosition(4, 9, 8)),
                createToken(TokenType.IDENTIFIER, "Wheels", createPosition(4, 20, 19)),
            ];

            const identifierList: IdentifierListNode = {
                kind: ASTNodeType.IdentifierList,
                identifiers: fieldIdentifiers,
                startToken: fieldIdentifiers[0],
                endToken: fieldIdentifiers[fieldIdentifiers.length - 1],
            };

            const fieldDeclaration: ClassFieldDeclarationNode = {
                kind: ASTNodeType.ClassFieldDeclaration,
                startToken: createToken(TokenType.COLON, ":", createPosition(4, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(4, 27, 26)),
                identifiers: identifierList,
            } as any;

            const node: ClassDefinitionNode = {
                kind: ASTNodeType.ClassDefinition,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(4, 27, 26)),
                declaration: classDeclaration,
                inherit: inheritStatement,
                members: [fieldDeclaration],
            };

            formatter.formatClassDefinition(node);

            const result = output.getOutput().trim();
            const lines = result.split("\n").filter((line) => line.trim() !== "");
            expect(lines[0]).toBe(":CLASS clsCar;");
            expect(lines[1]).toBe(":INHERIT Research.clsVehicle;");
            expect(lines[2]).toBe(":DECLARE MSRPPrice, Wheels;");
        });

        it("should format class with multiple members and methods", () => {
            const classNameToken = createToken(
                TokenType.IDENTIFIER,
                "clsCar",
                createPosition(1, 8, 7)
            );

            const classDeclaration: ClassDeclarationNode = {
                kind: ASTNodeType.ClassDeclaration,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 15, 14)),
                name: classNameToken,
            };

            // Field declaration
            const fieldIdentifiers = [
                createToken(TokenType.IDENTIFIER, "MSRPPrice", createPosition(2, 9, 8)),
            ];

            const identifierList: IdentifierListNode = {
                kind: ASTNodeType.IdentifierList,
                identifiers: fieldIdentifiers,
                startToken: fieldIdentifiers[0],
                endToken: fieldIdentifiers[fieldIdentifiers.length - 1],
            };

            const fieldDeclaration: ClassFieldDeclarationNode = {
                kind: ASTNodeType.ClassFieldDeclaration,
                startToken: createToken(TokenType.COLON, ":", createPosition(2, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(2, 19, 18)),
                identifiers: identifierList,
            } as any;

            // Method declaration
            const methodNameToken = createToken(
                TokenType.IDENTIFIER,
                "GetPrice",
                createPosition(4, 11, 10)
            );

            const procedureNode: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(4, 1, 0)),
                endToken: createToken(TokenType.ENDPROC, ":ENDPROC", createPosition(6, 1, 0)),
                name: methodNameToken,
                body: [],
            } as any;

            const methodDeclaration: MethodDeclarationNode = {
                kind: ASTNodeType.MethodDeclaration,
                startToken: createToken(TokenType.COLON, ":", createPosition(4, 1, 0)),
                endToken: createToken(TokenType.ENDPROC, ":ENDPROC", createPosition(6, 9, 8)),
                procedure: procedureNode,
            } as any;

            const node: ClassDefinitionNode = {
                kind: ASTNodeType.ClassDefinition,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.ENDPROC, ":ENDPROC", createPosition(6, 9, 8)),
                declaration: classDeclaration,
                members: [fieldDeclaration, methodDeclaration],
            };

            formatter.formatClassDefinition(node);

            const result = output.getOutput().trim();
            const lines = result.split("\n").filter((line) => line.trim() !== "");
            expect(lines[0]).toBe(":CLASS clsCar;");
            expect(lines[1]).toBe(":DECLARE MSRPPrice;");
            expect(lines[2]).toBe(":PROCEDURE GetPrice;");
            expect(lines[3]).toBe(":ENDPROC;");
        });

        it("should handle empty class definition", () => {
            const classNameToken = createToken(
                TokenType.IDENTIFIER,
                "EmptyClass",
                createPosition(1, 8, 7)
            );

            const classDeclaration: ClassDeclarationNode = {
                kind: ASTNodeType.ClassDeclaration,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 19, 18)),
                name: classNameToken,
            };

            const node: ClassDefinitionNode = {
                kind: ASTNodeType.ClassDefinition,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 19, 18)),
                declaration: classDeclaration,
                members: [],
            };

            formatter.formatClassDefinition(node);

            const result = output.getOutput().trim();
            expect(result).toBe(":CLASS EmptyClass;");
        });
    });

    describe("formatting with indentation", () => {
        it("should respect indentation level", () => {
            output.indent();

            const classNameToken = createToken(
                TokenType.IDENTIFIER,
                "IndentedClass",
                createPosition(1, 8, 7)
            );

            const node: ClassDeclarationNode = {
                kind: ASTNodeType.ClassDeclaration,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 22, 21)),
                name: classNameToken,
            };
            formatter.formatClassDeclaration(node);

            const result = output.getOutput().trimEnd(); // Only trim trailing whitespace, preserve indentation
            expect(result).toBe("    :CLASS IndentedClass;");
        });

        it("should maintain proper indentation for method bodies", () => {
            const methodNameToken = createToken(
                TokenType.IDENTIFIER,
                "TestMethod",
                createPosition(1, 11, 10)
            );

            const mockStatement = {
                kind: ASTNodeType.Assignment,
                startToken: createToken(TokenType.IDENTIFIER, "result", createPosition(2, 1, 0)),
                endToken: createToken(TokenType.NUMBER, "42", createPosition(2, 10, 9)),
            } as any;

            const procedureNode: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.ENDPROC, ":ENDPROC", createPosition(3, 1, 0)),
                name: methodNameToken,
                body: [mockStatement],
            } as any;

            const node: MethodDeclarationNode = {
                kind: ASTNodeType.MethodDeclaration,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.ENDPROC, ":ENDPROC", createPosition(3, 9, 8)),
                procedure: procedureNode,
            } as any;

            formatter.formatMethodDeclaration(node);

            const result = output.getOutput().trim();
            const lines = result.split("\n");

            // Method declaration should not be indented
            expect(lines[0]).toBe(":PROCEDURE TestMethod;");

            // Method body should be indented
            expect(lines[1]).toMatch(/^\s+result/);

            // ENDPROC should not be indented
            expect(lines[2]).toBe(":ENDPROC;");
        });
    });

    describe("EBNF grammar compliance", () => {
        it("should format according to ClassDefinition grammar rule", () => {
            // ClassDefinition ::= ClassDeclaration [InheritStatement] {ClassMember}
            const result = setupCompleteClassExample();

            const lines = result.split("\n").filter((line) => line.trim() !== "");

            // Should start with ClassDeclaration
            expect(lines[0]).toMatch(/^:CLASS \w+;$/);

            // Should have InheritStatement (optional)
            expect(lines[1]).toMatch(/^:INHERIT [\w.]+;$/);

            // Should have ClassMembers (field declarations and methods)
            expect(lines[2]).toMatch(/^:DECLARE [\w, ]+;$/);
            expect(lines[3]).toMatch(/^:PROCEDURE \w+;$/);
        });

        it("should format according to ClassDeclaration grammar rule", () => {
            // ClassDeclaration ::= ":" "CLASS" Identifier
            const classNameToken = createToken(
                TokenType.IDENTIFIER,
                "ValidClassName",
                createPosition(1, 8, 7)
            );

            const node: ClassDeclarationNode = {
                kind: ASTNodeType.ClassDeclaration,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 23, 22)),
                name: classNameToken,
            };

            formatter.formatClassDeclaration(node);

            const result = output.getOutput().trim();
            expect(result).toMatch(/^:CLASS \w+;$/);
        });

        it("should format according to InheritStatement grammar rule", () => {
            // InheritStatement ::= ":" "INHERIT" Identifier
            const parentClassToken = createToken(
                TokenType.IDENTIFIER,
                "Category.ParentClass",
                createPosition(1, 10, 9)
            );

            const node: InheritStatementNode = {
                kind: ASTNodeType.InheritStatement,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 31, 30)),
                className: parentClassToken,
            };

            formatter.formatInheritStatement(node);

            const result = output.getOutput().trim();
            expect(result).toMatch(/^:INHERIT [\w.]+;$/);
        });

        it("should format according to ClassFieldDeclaration grammar rule", () => {
            // ClassFieldDeclaration ::= ":" "DECLARE" IdentifierList
            const identifiers = [
                createToken(TokenType.IDENTIFIER, "field1", createPosition(1, 9, 8)),
                createToken(TokenType.IDENTIFIER, "field2", createPosition(1, 17, 16)),
                createToken(TokenType.IDENTIFIER, "field3", createPosition(1, 25, 24)),
            ];

            const identifierList: IdentifierListNode = {
                kind: ASTNodeType.IdentifierList,
                identifiers,
                startToken: identifiers[0],
                endToken: identifiers[identifiers.length - 1],
            };

            const node: ClassFieldDeclarationNode = {
                kind: ASTNodeType.ClassFieldDeclaration,
                startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
                endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 32, 31)),
                identifiers: identifierList,
            } as any;

            formatter.formatClassFieldDeclaration(node);

            const result = output.getOutput().trim();
            expect(result).toMatch(/^:DECLARE [\w, ]+;$/);
        });
    });

    function setupCompleteClassExample(): string {
        const classNameToken = createToken(TokenType.IDENTIFIER, "clsCar", createPosition(1, 8, 7));

        const classDeclaration: ClassDeclarationNode = {
            kind: ASTNodeType.ClassDeclaration,
            startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
            endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 15, 14)),
            name: classNameToken,
        };

        const parentClassToken = createToken(
            TokenType.IDENTIFIER,
            "Research.clsVehicle",
            createPosition(2, 10, 9)
        );

        const inheritStatement: InheritStatementNode = {
            kind: ASTNodeType.InheritStatement,
            startToken: createToken(TokenType.COLON, ":", createPosition(2, 1, 0)),
            endToken: createToken(TokenType.SEMICOLON, ";", createPosition(2, 30, 29)),
            className: parentClassToken,
        };

        const fieldIdentifiers = [
            createToken(TokenType.IDENTIFIER, "MSRPPrice", createPosition(4, 9, 8)),
            createToken(TokenType.IDENTIFIER, "Wheels", createPosition(4, 20, 19)),
        ];

        const identifierList: IdentifierListNode = {
            kind: ASTNodeType.IdentifierList,
            identifiers: fieldIdentifiers,
            startToken: fieldIdentifiers[0],
            endToken: fieldIdentifiers[fieldIdentifiers.length - 1],
        };

        const fieldDeclaration: ClassFieldDeclarationNode = {
            kind: ASTNodeType.ClassFieldDeclaration,
            startToken: createToken(TokenType.COLON, ":", createPosition(4, 1, 0)),
            endToken: createToken(TokenType.SEMICOLON, ";", createPosition(4, 27, 26)),
            identifiers: identifierList,
        } as any;

        const methodNameToken = createToken(
            TokenType.IDENTIFIER,
            "GetPrice",
            createPosition(6, 11, 10)
        );

        const procedureNode: ProcedureStatementNode = {
            kind: ASTNodeType.ProcedureStatement,
            startToken: createToken(TokenType.COLON, ":", createPosition(6, 1, 0)),
            endToken: createToken(TokenType.ENDPROC, ":ENDPROC", createPosition(8, 1, 0)),
            name: methodNameToken,
            body: [],
        } as any;

        const methodDeclaration: MethodDeclarationNode = {
            kind: ASTNodeType.MethodDeclaration,
            startToken: createToken(TokenType.COLON, ":", createPosition(6, 1, 0)),
            endToken: createToken(TokenType.ENDPROC, ":ENDPROC", createPosition(8, 9, 8)),
            procedure: procedureNode,
        } as any;

        const node: ClassDefinitionNode = {
            kind: ASTNodeType.ClassDefinition,
            startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
            endToken: createToken(TokenType.ENDPROC, ":ENDPROC", createPosition(8, 9, 8)),
            declaration: classDeclaration,
            inherit: inheritStatement,
            members: [fieldDeclaration, methodDeclaration],
        };

        formatter.formatClassDefinition(node);

        return output.getOutput().trim();
    }
});
