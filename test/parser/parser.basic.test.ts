/**
 * Basic parser functionality tests
 */

import { tokenize } from "../../src/tokenizer";
import { parse } from "../../src/parser";

describe("Parser - Basic Functionality", () => {
    describe("Program parsing", () => {
        it("should parse an empty program", () => {
            const tokens = tokenize("");
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body).toHaveLength(0);
        });

        it("should parse a simple assignment", () => {
            const input = "x := 42;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body).toHaveLength(1);
        });
        it("should parse a simple procedure", () => {
            const input = `:PROCEDURE TestProc
x := 42;
:ENDPROC`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body).toHaveLength(1);
        });
    });

    describe("Expression parsing", () => {
        it("should parse arithmetic expressions", () => {
            const input = "result := a + b * c;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse logical expressions", () => {
            const input = "result := a .AND. b .OR. c;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse function calls", () => {
            const input = "result := MyFunction(x, y, z);";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
    });

    describe("Control flow parsing", () => {
        it("should parse if statements", () => {
            const input = `:IF x > 0
    y := x;
:ENDIF`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse while loops", () => {
            const input = `:WHILE x > 0
    x := x - 1;
:ENDWHILE`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse for loops", () => {
            const input = `:FOR i := 1 :TO 10
    sum := sum + i;
:NEXT`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
    });

    describe("Error handling", () => {
        it("should report syntax errors", () => {
            const input = "x := ; // Missing expression";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(false);
            expect(result.errors.length).toBeGreaterThan(0);
        });

        it("should continue parsing after errors", () => {
            const input = `x := ;
y := 42;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(false);
            expect(result.errors.length).toBeGreaterThan(0);
            // Should still have parsed some statements
            expect(result.ast.body.length).toBeGreaterThan(0);
        });
    });
});
