/**
 * Statement parsing tests
 */

import { tokenize } from "../../src/tokenizer";
import { parse } from "../../src/parser";
import { ASTNodeType } from "../../src/parser/ast";

describe("Parser - Statement Parsing", () => {
    describe("Declaration statements", () => {
        it("should parse DECLARE statement", () => {
            const input = ":DECLARE var1, var2, var3;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body).toHaveLength(1);
            expect(result.ast.body[0].kind).toBe(ASTNodeType.DeclareStatement);
        });

        it("should parse PARAMETERS statement", () => {
            const input = ":PARAMETERS param1, param2;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body[0].kind).toBe(ASTNodeType.ParametersStatement);
        });

        it("should parse PUBLIC statement", () => {
            const input = ":PUBLIC var1, var2;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body[0].kind).toBe(ASTNodeType.PublicStatement);
        });

        it("should parse INCLUDE statement", () => {
            const input = ':INCLUDE "myfile.ssl";';
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body[0].kind).toBe(ASTNodeType.IncludeStatement);
        });
    });

    describe("Assignment statements", () => {
        it("should parse simple assignment", () => {
            const input = "x := 42;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body[0].kind).toBe(ASTNodeType.Assignment);
        });

        it("should parse compound assignment operators", () => {
            const operators = [":=", "+=", "-=", "*=", "/=", "^="];

            for (const op of operators) {
                const input = `x ${op} 5;`;
                const tokens = tokenize(input);
                const result = parse(tokens);

                expect(result.success).toBe(true);
                expect(result.errors).toHaveLength(0);
                expect(result.ast.body[0].kind).toBe(ASTNodeType.Assignment);
            }
        });

        it("should reject = as assignment operator", () => {
            const input = "x = 42;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(false);
            expect(result.errors.length).toBeGreaterThan(0);
        });
    });

    describe("Return statements", () => {
        it("should parse return with value", () => {
            const input = ":RETURN 42;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body[0].kind).toBe(ASTNodeType.ReturnStatement);
        });

        it("should parse return without value", () => {
            const input = ":RETURN;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body[0].kind).toBe(ASTNodeType.ReturnStatement);
        });
    });

    describe("Label statements", () => {
        it("should parse label statement", () => {
            const input = ":LABEL MyLabel;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body[0].kind).toBe(ASTNodeType.LabelStatement);
        });
    });

    describe("Loop control statements", () => {
        it("should parse EXITWHILE statement", () => {
            const input = ":EXITWHILE;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body[0].kind).toBe(ASTNodeType.ExitWhileStatement);
        });

        it("should parse EXITFOR statement", () => {
            const input = ":EXITFOR;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body[0].kind).toBe(ASTNodeType.ExitForStatement);
        });

        it("should parse LOOP statement", () => {
            const input = ":LOOP;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body[0].kind).toBe(ASTNodeType.LoopContinue);
        });

        it("should parse EXITCASE statement", () => {
            const input = ":EXITCASE;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body[0].kind).toBe(ASTNodeType.ExitCaseStatement);
        });
    });
});
