/**
 * Demo: Complete SSL Class Formatter Integration
 *
 * This demo showcases the complete integration of the class formatter with the main SSL formatter.
 * It demonstrates formatting of all class-related constructs according to SSL EBNF grammar.
 */

import { SSLFormatter } from "../../src/formatter/index";
import { defaultFormatterOptions } from "../../src/formatter/options";
import {
    ClassDefinitionNode,
    ClassDeclarationNode,
    InheritStatementNode,
    ClassFieldDeclarationNode,
    MethodDeclarationNode,
} from "../../src/parser/ast/classes";
import { IdentifierListNode } from "../../src/parser/ast/lists";
import { ProcedureStatementNode } from "../../src/parser/ast/procedures";
import { ProgramNode } from "../../src/parser/ast";
import { ASTNodeType } from "../../src/parser/ast/base";
import { createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("SSL Class Formatter Integration Demo", () => {
    let formatter: SSLFormatter;

    beforeAll(() => {
        formatter = new SSLFormatter(defaultFormatterOptions);
    });

    it("should demonstrate complete SSL class formatting capabilities", () => {
        // Create a comprehensive class definition that showcases all SSL class features
        const completeClass = createComprehensiveClassExample();

        const program: ProgramNode = {
            kind: ASTNodeType.Program,
            startToken: completeClass.startToken,
            endToken: completeClass.endToken,
            body: [completeClass],
        };
        const formattedCode = formatter.format(program);

        // Verify the formatted output matches EBNF grammar expectations
        const lines = formattedCode.split("\n").filter((line) => line.trim() !== "");

        // 1. ClassDeclaration ::= ":" "CLASS" Identifier
        expect(lines[0]).toMatch(/^:CLASS clsAdvancedExample;$/);

        // 2. InheritStatement ::= ":" "INHERIT" Identifier (supports qualified names)
        expect(lines[1]).toMatch(/^:INHERIT Research\.clsBaseVehicle;$/);

        // 3. ClassFieldDeclaration ::= ":" "DECLARE" IdentifierList
        expect(lines[2]).toMatch(
            /^:DECLARE sManufacturer, nEngineSize, bIsElectric, dManufactureDate;$/
        );

        // 4. MethodDeclaration ::= ProcedureStatement (procedures within class context)
        expect(lines[3]).toMatch(/^:PROCEDURE CalculateValue;$/);
        expect(lines[4]).toMatch(/^:ENDPROC;$/);

        // 5. Verify proper indentation and spacing
        expect(formattedCode).toContain("\n\n"); // Blank lines between major sections

        // 6. Verify Hungarian notation support (SSL convention)
        expect(formattedCode).toContain("sManufacturer"); // String
        expect(formattedCode).toContain("nEngineSize"); // Number
        expect(formattedCode).toContain("bIsElectric"); // Boolean
        expect(formattedCode).toContain("dManufactureDate"); // Date
    });

    function createComprehensiveClassExample(): ClassDefinitionNode {
        // Class Declaration
        const classNameToken = createToken(
            TokenType.IDENTIFIER,
            "clsAdvancedExample",
            createPosition(1, 8, 7)
        );

        const classDeclaration: ClassDeclarationNode = {
            kind: ASTNodeType.ClassDeclaration,
            startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
            endToken: createToken(TokenType.SEMICOLON, ";", createPosition(1, 24, 23)),
            name: classNameToken,
        };

        // Inheritance Statement (with qualified class name)
        const parentClassToken = createToken(
            TokenType.IDENTIFIER,
            "Research.clsBaseVehicle",
            createPosition(3, 10, 9)
        );

        const inheritStatement: InheritStatementNode = {
            kind: ASTNodeType.InheritStatement,
            startToken: createToken(TokenType.COLON, ":", createPosition(3, 1, 0)),
            endToken: createToken(TokenType.SEMICOLON, ";", createPosition(3, 34, 33)),
            className: parentClassToken,
        };

        // Class Field Declaration (multiple fields with Hungarian notation)
        const fieldIdentifiers = [
            createToken(TokenType.IDENTIFIER, "sManufacturer", createPosition(5, 9, 8)),
            createToken(TokenType.IDENTIFIER, "nEngineSize", createPosition(5, 24, 23)),
            createToken(TokenType.IDENTIFIER, "bIsElectric", createPosition(5, 37, 36)),
            createToken(TokenType.IDENTIFIER, "dManufactureDate", createPosition(5, 50, 49)),
        ];

        const identifierList: IdentifierListNode = {
            kind: ASTNodeType.IdentifierList,
            identifiers: fieldIdentifiers,
            startToken: fieldIdentifiers[0],
            endToken: fieldIdentifiers[fieldIdentifiers.length - 1],
        };

        const fieldDeclaration: ClassFieldDeclarationNode = {
            kind: ASTNodeType.ClassFieldDeclaration,
            startToken: createToken(TokenType.COLON, ":", createPosition(5, 1, 0)),
            endToken: createToken(TokenType.SEMICOLON, ";", createPosition(5, 67, 66)),
            identifiers: identifierList,
        } as any;

        // Method Declaration
        const methodNameToken = createToken(
            TokenType.IDENTIFIER,
            "CalculateValue",
            createPosition(7, 11, 10)
        );

        const procedureNode: ProcedureStatementNode = {
            kind: ASTNodeType.ProcedureStatement,
            startToken: createToken(TokenType.COLON, ":", createPosition(7, 1, 0)),
            endToken: createToken(TokenType.ENDPROC, ":ENDPROC", createPosition(9, 9, 8)),
            name: methodNameToken,
            body: [], // Method body would contain statements
        } as any;

        const methodDeclaration: MethodDeclarationNode = {
            kind: ASTNodeType.MethodDeclaration,
            startToken: createToken(TokenType.COLON, ":", createPosition(7, 1, 0)),
            endToken: createToken(TokenType.ENDPROC, ":ENDPROC", createPosition(9, 9, 8)),
            procedure: procedureNode,
        } as any;

        // Complete Class Definition
        return {
            kind: ASTNodeType.ClassDefinition,
            startToken: createToken(TokenType.COLON, ":", createPosition(1, 1, 0)),
            endToken: createToken(TokenType.ENDPROC, ":ENDPROC", createPosition(9, 9, 8)),
            declaration: classDeclaration,
            inherit: inheritStatement,
            members: [fieldDeclaration, methodDeclaration],
        };
    }
});
