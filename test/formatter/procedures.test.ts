/**
 * Simple Tests for Procedure Formatters
 */

import { ProcedureFormatter } from "../../src/formatter/procedures";
import { OutputBuilder } from "../../src/formatter/visitor";
import { defaultFormatterOptions, FormatterOptions } from "../../src/formatter/options";
import {
    ProcedureStatementNode,
    ParameterDeclarationNode,
    ParameterListNode,
    DefaultParameterDeclarationNode,
    DefaultParameterListNode,
} from "../../src/parser/ast/procedures";
import { ASTNodeType, StatementNode } from "../../src/parser/ast/base";
import { createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("ProcedureFormatter", () => {
    let formatter: ProcedureFormatter;
    let output: OutputBuilder;
    let options: FormatterOptions;
    let mockToken: any;

    beforeEach(() => {
        options = { ...defaultFormatterOptions };
        output = new OutputBuilder(options);
        formatter = new ProcedureFormatter(output, options);
        mockToken = createToken(TokenType.IDENTIFIER, "test", createPosition(1, 1, 0));
    });

    describe("formatProcedureStatement", () => {
        it("should format simple procedure without parameters", () => {
            const procName = createToken(
                TokenType.IDENTIFIER,
                "TestProc",
                createPosition(1, 12, 11)
            );

            const procedureNode: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: mockToken,
                endToken: mockToken,
                name: procName,
                body: [],
            };

            formatter.formatProcedureStatement(procedureNode);
            const result = output.getOutput();

            expect(result).toContain(":PROCEDURE TestProc;");
            expect(result).toContain(":ENDPROC;");
        });

        it("should format procedure with parameters", () => {
            const procName = createToken(
                TokenType.IDENTIFIER,
                "TestProc",
                createPosition(1, 12, 11)
            );
            const param1 = createToken(TokenType.IDENTIFIER, "param1", createPosition(2, 12, 23));
            const param2 = createToken(TokenType.IDENTIFIER, "param2", createPosition(2, 20, 31));

            const parameterList: ParameterListNode = {
                kind: ASTNodeType.ParameterList,
                startToken: mockToken,
                endToken: mockToken,
                identifiers: [param1, param2],
            };

            const parameterDeclaration: ParameterDeclarationNode = {
                kind: ASTNodeType.ParameterDeclaration,
                startToken: mockToken,
                endToken: mockToken,
                parameters: parameterList,
            };

            const procedureNode: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: mockToken,
                endToken: mockToken,
                name: procName,
                parameters: parameterDeclaration,
                body: [],
            };

            formatter.formatProcedureStatement(procedureNode);
            const result = output.getOutput();

            expect(result).toContain(":PROCEDURE TestProc;");
            expect(result).toContain(":PARAMETERS param1, param2;");
            expect(result).toContain(":ENDPROC;");
        });

        it("should format procedure with default parameters", () => {
            const procName = createToken(
                TokenType.IDENTIFIER,
                "TestProc",
                createPosition(1, 12, 11)
            );
            const param1 = createToken(TokenType.IDENTIFIER, "param1", createPosition(3, 9, 40));

            const defaultParameterList: DefaultParameterListNode = {
                kind: ASTNodeType.DefaultParameterList,
                startToken: mockToken,
                endToken: mockToken,
                pairs: [
                    {
                        identifier: param1,
                        defaultValue: null, // Simplified for test
                    },
                ],
            };

            const defaultParameterDeclaration: DefaultParameterDeclarationNode = {
                kind: ASTNodeType.DefaultParameterDeclaration,
                startToken: mockToken,
                endToken: mockToken,
                parameters: defaultParameterList,
            };

            const procedureNode: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: mockToken,
                endToken: mockToken,
                name: procName,
                defaultParameters: defaultParameterDeclaration,
                body: [],
            };

            formatter.formatProcedureStatement(procedureNode);
            const result = output.getOutput();

            expect(result).toContain(":PROCEDURE TestProc;");
            expect(result).toContain(':DEFAULT param1, "defaultValue";');
            expect(result).toContain(":ENDPROC;");
        });

        it("should format procedure with body statements", () => {
            const procName = createToken(
                TokenType.IDENTIFIER,
                "TestProc",
                createPosition(1, 12, 11)
            );

            // Create mock body statements using a valid AST node type
            const mockStatement: StatementNode = {
                kind: ASTNodeType.Assignment, // Use a valid statement type
                startToken: mockToken,
                endToken: mockToken,
            };

            const procedureNode: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: mockToken,
                endToken: mockToken,
                name: procName,
                body: [mockStatement, mockStatement],
            };

            formatter.formatProcedureStatement(procedureNode);
            const result = output.getOutput();

            expect(result).toContain(":PROCEDURE TestProc;");
            expect(result).toContain("/* statement */");
            expect(result).toContain(":ENDPROC;");

            // Check that body statements are indented
            const lines = result.split("\n");
            const statementLines = lines.filter((line) => line.includes("/* statement */"));
            expect(statementLines.length).toBe(2);
            statementLines.forEach((line) => {
                expect(line).toMatch(/^\s+\/\* statement \*\/$/);
            });
        });
    });

    describe("formatParameterDeclaration", () => {
        it("should format parameter declaration with single parameter", () => {
            const param1 = createToken(
                TokenType.IDENTIFIER,
                "singleParam",
                createPosition(1, 12, 11)
            );

            const parameterList: ParameterListNode = {
                kind: ASTNodeType.ParameterList,
                startToken: mockToken,
                endToken: mockToken,
                identifiers: [param1],
            };

            const parameterDeclaration: ParameterDeclarationNode = {
                kind: ASTNodeType.ParameterDeclaration,
                startToken: mockToken,
                endToken: mockToken,
                parameters: parameterList,
            };

            formatter.formatParameterDeclaration(parameterDeclaration);
            const result = output.getOutput();

            expect(result.trim()).toBe(":PARAMETERS singleParam;");
        });
    });

    describe("formatDefaultParameterDeclaration", () => {
        it("should format default parameter declaration", () => {
            const param1 = createToken(TokenType.IDENTIFIER, "param1", createPosition(1, 9, 8));

            const defaultParameterList: DefaultParameterListNode = {
                kind: ASTNodeType.DefaultParameterList,
                startToken: mockToken,
                endToken: mockToken,
                pairs: [
                    {
                        identifier: param1,
                        defaultValue: null, // Simplified for test
                    },
                ],
            };

            const defaultParameterDeclaration: DefaultParameterDeclarationNode = {
                kind: ASTNodeType.DefaultParameterDeclaration,
                startToken: mockToken,
                endToken: mockToken,
                parameters: defaultParameterList,
            };

            formatter.formatDefaultParameterDeclaration(defaultParameterDeclaration);
            const result = output.getOutput();

            expect(result.trim()).toBe(':DEFAULT param1, "defaultValue";');
        });

        it("should handle empty default parameter list", () => {
            const defaultParameterList: DefaultParameterListNode = {
                kind: ASTNodeType.DefaultParameterList,
                startToken: mockToken,
                endToken: mockToken,
                pairs: [],
            };

            const defaultParameterDeclaration: DefaultParameterDeclarationNode = {
                kind: ASTNodeType.DefaultParameterDeclaration,
                startToken: mockToken,
                endToken: mockToken,
                parameters: defaultParameterList,
            };

            formatter.formatDefaultParameterDeclaration(defaultParameterDeclaration);
            const result = output.getOutput();

            expect(result.trim()).toBe(":DEFAULT ;");
        });
    });

    describe("comprehensive procedure formatting", () => {
        it("should format procedure with both parameters and defaults", () => {
            const procName = createToken(
                TokenType.IDENTIFIER,
                "ComplexProc",
                createPosition(1, 12, 11)
            );
            const param1 = createToken(TokenType.IDENTIFIER, "param1", createPosition(2, 12, 23));
            const param2 = createToken(TokenType.IDENTIFIER, "param2", createPosition(2, 20, 31));
            const defaultParam = createToken(
                TokenType.IDENTIFIER,
                "param1",
                createPosition(3, 9, 40)
            );

            const parameterList: ParameterListNode = {
                kind: ASTNodeType.ParameterList,
                startToken: mockToken,
                endToken: mockToken,
                identifiers: [param1, param2],
            };

            const parameterDeclaration: ParameterDeclarationNode = {
                kind: ASTNodeType.ParameterDeclaration,
                startToken: mockToken,
                endToken: mockToken,
                parameters: parameterList,
            };

            const defaultParameterList: DefaultParameterListNode = {
                kind: ASTNodeType.DefaultParameterList,
                startToken: mockToken,
                endToken: mockToken,
                pairs: [
                    {
                        identifier: defaultParam,
                        defaultValue: null, // Simplified for test
                    },
                ],
            };

            const defaultParameterDeclaration: DefaultParameterDeclarationNode = {
                kind: ASTNodeType.DefaultParameterDeclaration,
                startToken: mockToken,
                endToken: mockToken,
                parameters: defaultParameterList,
            };

            const procedureNode: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: mockToken,
                endToken: mockToken,
                name: procName,
                parameters: parameterDeclaration,
                defaultParameters: defaultParameterDeclaration,
                body: [],
            };

            formatter.formatProcedureStatement(procedureNode);
            const result = output.getOutput();

            expect(result).toContain(":PROCEDURE ComplexProc;");
            expect(result).toContain(":PARAMETERS param1, param2;");
            expect(result).toContain(':DEFAULT param1, "defaultValue";');
            expect(result).toContain(":ENDPROC;");
        });

        it("should include blank line between declarations and body", () => {
            const procName = createToken(
                TokenType.IDENTIFIER,
                "TestProc",
                createPosition(1, 12, 11)
            );
            const param1 = createToken(TokenType.IDENTIFIER, "param1", createPosition(2, 12, 23));

            const parameterList: ParameterListNode = {
                kind: ASTNodeType.ParameterList,
                startToken: mockToken,
                endToken: mockToken,
                identifiers: [param1],
            };

            const parameterDeclaration: ParameterDeclarationNode = {
                kind: ASTNodeType.ParameterDeclaration,
                startToken: mockToken,
                endToken: mockToken,
                parameters: parameterList,
            };

            const mockStatement: StatementNode = {
                kind: ASTNodeType.Assignment,
                startToken: mockToken,
                endToken: mockToken,
            };

            const procedureNode: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: mockToken,
                endToken: mockToken,
                name: procName,
                parameters: parameterDeclaration,
                body: [mockStatement],
            };

            formatter.formatProcedureStatement(procedureNode);
            const result = output.getOutput();

            const lines = result.split("\n");

            // Find the indexes of key lines
            const parametersIndex = lines.findIndex((line) => line.includes(":PARAMETERS"));
            const statementIndex = lines.findIndex((line) => line.includes("/* statement */"));

            // There should be a blank line between parameters and body
            expect(parametersIndex).toBeLessThan(statementIndex);
            expect(lines[parametersIndex + 1]).toBe(""); // Blank line
        });
    });

    describe("indentation handling", () => {
        it("should properly indent procedure body", () => {
            const procName = createToken(
                TokenType.IDENTIFIER,
                "IndentTest",
                createPosition(1, 12, 11)
            );

            const mockStatement: StatementNode = {
                kind: ASTNodeType.Assignment,
                startToken: mockToken,
                endToken: mockToken,
            };

            const procedureNode: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: mockToken,
                endToken: mockToken,
                name: procName,
                body: [mockStatement],
            };

            formatter.formatProcedureStatement(procedureNode);
            const result = output.getOutput();

            const lines = result.split("\n");
            const procedureLine = lines.find((line) => line.includes(":PROCEDURE"));
            const statementLine = lines.find((line) => line.includes("/* statement */"));
            const endprocLine = lines.find((line) => line.includes(":ENDPROC"));

            // Procedure and endproc should not be indented (assuming they start at column 0)
            expect(procedureLine).toMatch(/^:PROCEDURE/);
            expect(endprocLine).toMatch(/^:ENDPROC/);

            // Statement should be indented
            expect(statementLine).toMatch(/^\s+\/\* statement \*\/$/);
        });

        it("should use correct indentation size from options", () => {
            options.indentSize = 2;
            options.useTabs = false;
            output = new OutputBuilder(options);
            formatter = new ProcedureFormatter(output, options);

            const procName = createToken(
                TokenType.IDENTIFIER,
                "IndentTest",
                createPosition(1, 12, 11)
            );

            const mockStatement: StatementNode = {
                kind: ASTNodeType.Assignment,
                startToken: mockToken,
                endToken: mockToken,
            };

            const procedureNode: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: mockToken,
                endToken: mockToken,
                name: procName,
                body: [mockStatement],
            };

            formatter.formatProcedureStatement(procedureNode);
            const result = output.getOutput();

            const lines = result.split("\n");
            const statementLine = lines.find((line) => line.includes("/* statement */"));

            // Should be indented with 2 spaces
            expect(statementLine).toMatch(/^  \/\* statement \*\/$/);
        });

        it("should use tabs when configured", () => {
            options.useTabs = true;
            output = new OutputBuilder(options);
            formatter = new ProcedureFormatter(output, options);

            const procName = createToken(
                TokenType.IDENTIFIER,
                "TabTest",
                createPosition(1, 12, 11)
            );

            const mockStatement: StatementNode = {
                kind: ASTNodeType.Assignment,
                startToken: mockToken,
                endToken: mockToken,
            };

            const procedureNode: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: mockToken,
                endToken: mockToken,
                name: procName,
                body: [mockStatement],
            };

            formatter.formatProcedureStatement(procedureNode);
            const result = output.getOutput();

            const lines = result.split("\n");
            const statementLine = lines.find((line) => line.includes("/* statement */"));

            // Should be indented with tab
            expect(statementLine).toMatch(/^\t\/\* statement \*\/$/);
        });
    });

    describe("edge cases", () => {
        it("should handle empty parameter list", () => {
            const parameterList: ParameterListNode = {
                kind: ASTNodeType.ParameterList,
                startToken: mockToken,
                endToken: mockToken,
                identifiers: [],
            };

            const parameterDeclaration: ParameterDeclarationNode = {
                kind: ASTNodeType.ParameterDeclaration,
                startToken: mockToken,
                endToken: mockToken,
                parameters: parameterList,
            };

            formatter.formatParameterDeclaration(parameterDeclaration);
            const result = output.getOutput();

            expect(result.trim()).toBe(":PARAMETERS ;");
        });

        it("should not include blank line when there are no body statements", () => {
            const procName = createToken(
                TokenType.IDENTIFIER,
                "EmptyProc",
                createPosition(1, 12, 11)
            );
            const param1 = createToken(TokenType.IDENTIFIER, "param1", createPosition(2, 12, 23));

            const parameterList: ParameterListNode = {
                kind: ASTNodeType.ParameterList,
                startToken: mockToken,
                endToken: mockToken,
                identifiers: [param1],
            };

            const parameterDeclaration: ParameterDeclarationNode = {
                kind: ASTNodeType.ParameterDeclaration,
                startToken: mockToken,
                endToken: mockToken,
                parameters: parameterList,
            };

            const procedureNode: ProcedureStatementNode = {
                kind: ASTNodeType.ProcedureStatement,
                startToken: mockToken,
                endToken: mockToken,
                name: procName,
                parameters: parameterDeclaration,
                body: [], // Empty body
            };

            formatter.formatProcedureStatement(procedureNode);
            const result = output.getOutput();

            const lines = result.split("\n").filter((line) => line.trim() !== "");

            // Should only have procedure declaration, parameters, and endproc
            expect(lines.length).toBe(3);
            expect(lines[0]).toContain(":PROCEDURE EmptyProc;");
            expect(lines[1]).toContain(":PARAMETERS param1;");
            expect(lines[2]).toContain(":ENDPROC;");
        });
    });
});
