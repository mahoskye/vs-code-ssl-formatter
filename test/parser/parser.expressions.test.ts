/**
 * Expression parsing tests
 */

import { tokenize } from "../../src/tokenizer";
import { parse } from "../../src/parser";
import { ASTNodeType } from "../../src/parser/ast";

describe("Parser - Expression Parsing", () => {
    describe("Arithmetic expressions", () => {
        it("should parse simple addition", () => {
            const input = "result := a + b;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body).toHaveLength(1);

            const assignment = result.ast.body[0];
            expect(assignment.kind).toBe(ASTNodeType.Assignment);
        });

        it("should parse multiplication with correct precedence", () => {
            const input = "result := a + b * c;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse power operations", () => {
            const input = "result := a ^ b;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse parenthesized expressions", () => {
            const input = "result := (a + b) * c;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
    });

    describe("Logical expressions", () => {
        it("should parse .AND. operator", () => {
            const input = "result := a .AND. b;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse .OR. operator", () => {
            const input = "result := a .OR. b;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse .NOT. operator", () => {
            const input = "result := .NOT. a;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse complex logical expressions", () => {
            const input = "result := a .AND. b .OR. c;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
    });

    describe("Comparison expressions", () => {
        it("should parse equality comparison", () => {
            const input = "result := a = b;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse strict equality", () => {
            const input = "result := a == b;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse inequality operators", () => {
            const input = "result := a < b .AND. c > d .AND. e <= f .AND. g >= h;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
    });

    describe("Literals", () => {
        it("should parse number literals", () => {
            const input = "result := 42;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse string literals", () => {
            const input = 'result := "hello world";';
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse boolean literals", () => {
            const input = "result := .T.;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse NIL literal", () => {
            const input = "result := NIL;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse array literals", () => {
            const input = "result := {1, 2, 3};";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
    });

    describe("Property and array access", () => {
        it("should parse property access", () => {
            const input = "result := obj:property;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse array access", () => {
            const input = "result := arr[1];";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse multi-dimensional array access", () => {
            const input = "result := arr[1, 2];";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
    });

    describe("Function calls", () => {
        it("should parse function calls with no arguments", () => {
            const input = "result := MyFunction();";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse function calls with arguments", () => {
            const input = "result := MyFunction(a, b, c);";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
    });
});
