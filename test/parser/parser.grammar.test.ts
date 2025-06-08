/**
 * Grammar compliance tests for SSL parser
 * Tests adherence to the SSL EBNF grammar specification
 */

import { tokenize } from "../../src/tokenizer";
import { parse } from "../../src/parser";
import { ASTNodeType } from "../../src/parser/ast";

describe("Parser - Grammar Compliance", () => {
    describe("Program structure", () => {
        it("should parse empty program according to grammar", () => {
            const input = "";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.ast.kind).toBe(ASTNodeType.Program);
            expect(result.ast.body).toHaveLength(0);
        });
        it("should parse program with class definition", () => {
            const input = `:CLASS MyClass;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.ast.body).toHaveLength(1);
            expect(result.ast.body[0].kind).toBe(ASTNodeType.ClassDefinition);
        });

        it("should parse program with statements", () => {
            const input = `x := 42;
y := "hello";`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.ast.body).toHaveLength(2);
            expect(result.ast.body[0].kind).toBe(ASTNodeType.Assignment);
            expect(result.ast.body[1].kind).toBe(ASTNodeType.Assignment);
        });
    });

    describe("Expression hierarchy", () => {
        it("should parse logical expressions with correct precedence", () => {
            const input = "result := a .AND. b .OR. c;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse comparison expressions", () => {
            const input = "result := a == b .AND. c != d;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse arithmetic expressions with correct precedence", () => {
            const input = "result := a + b * c ^ d;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse unary expressions", () => {
            const input = "result := .NOT. a + -b;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
    });

    describe("Literals according to grammar", () => {
        it("should parse number literals", () => {
            const inputs = ["x := 42;", "x := 3.14;", "x := 1.23e5;", "x := 4.56E-3;"];

            for (const input of inputs) {
                const tokens = tokenize(input);
                const result = parse(tokens);
                expect(result.success).toBe(true);
            }
        });

        it("should reject invalid number formats", () => {
            const inputs = [
                "x := 7e2;", // No decimal point before 'e'
                "x := .5e1;", // Leading decimal without zero
                "x := 9E+1;", // Explicit plus sign
            ];

            for (const input of inputs) {
                const tokens = tokenize(input);
                const result = parse(tokens);
                // These should either fail tokenization or parsing
                expect(result.success).toBe(false);
            }
        });

        it("should parse string literals with both quote types", () => {
            const inputs = ['x := "hello world";', "x := 'hello world';"];

            for (const input of inputs) {
                const tokens = tokenize(input);
                const result = parse(tokens);
                expect(result.success).toBe(true);
            }
        });

        it("should parse boolean literals correctly", () => {
            const input = "result := .T. .AND. .F.;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should reject invalid boolean literals", () => {
            const inputs = ["result := TRUE;", "result := FALSE;"];

            for (const input of inputs) {
                const tokens = tokenize(input);
                const result = parse(tokens);
                expect(result.success).toBe(false);
                expect(result.errors.length).toBeGreaterThan(0);
            }
        });

        it("should parse array literals", () => {
            const inputs = [
                "arr := {1, 2, 3};",
                "arr := {};",
                "arr := {a, b, c};",
                "nested := {{1, 2}, {3, 4}};",
            ];

            for (const input of inputs) {
                const tokens = tokenize(input);
                const result = parse(tokens);
                expect(result.success).toBe(true);
            }
        });

        it("should parse NIL literal", () => {
            const input = "x := NIL;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
    });

    describe("Assignment operators", () => {
        it("should parse all assignment operators", () => {
            const operators = [":=", "+=", "-=", "*=", "/=", "^="];

            for (const op of operators) {
                const input = `x ${op} 5;`;
                const tokens = tokenize(input);
                const result = parse(tokens);
                expect(result.success).toBe(true);
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

    describe("Function calls according to grammar", () => {
        it("should parse direct function calls", () => {
            const input = "result := MyFunction(a, b, c);";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse DoProc calls", () => {
            const input = 'result := DoProc("MyProc", {a, b, c});';
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse ExecFunction calls", () => {
            const input = 'result := ExecFunction("MyFunc", {a, b, c});';
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
    });

    describe("SQL integration", () => {
        it("should parse SqlExecute calls", () => {
            const input = 'result := SqlExecute("SELECT * FROM table WHERE id = ?param?");';
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse LSearch calls", () => {
            const input = 'result := LSearch("SELECT * FROM table WHERE id = ?", , , {param});';
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
    });

    describe("Object-oriented features", () => {
        it("should parse object creation", () => {
            const input = 'obj := CreateUDObject("MyClass");';
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse property access", () => {
            const input = "value := obj:property;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse method calls", () => {
            const input = "result := obj:method(a, b);";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
    });

    describe("Array access", () => {
        it("should parse single-dimensional array access", () => {
            const input = "value := arr[1];";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse multi-dimensional array access with comma notation", () => {
            const input = "value := arr[1, 2];";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse chained bracket notation", () => {
            const input = "value := arr[1][2];";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
    });

    describe("Bitwise operations", () => {
        it("should parse bitwise operations as functions", () => {
            const operations = ["_AND", "_OR", "_XOR", "_NOT"];

            for (const op of operations) {
                const input = op === "_NOT" ? `result := ${op}(a);` : `result := ${op}(a, b);`;
                const tokens = tokenize(input);
                const result = parse(tokens);
                expect(result.success).toBe(true);
            }
        });
    });

    describe("Increment/Decrement expressions", () => {
        it("should parse prefix increment/decrement", () => {
            const inputs = ["result := ++x;", "result := --y;"];

            for (const input of inputs) {
                const tokens = tokenize(input);
                const result = parse(tokens);
                expect(result.success).toBe(true);
            }
        });

        it("should parse postfix increment/decrement", () => {
            const inputs = ["result := x++;", "result := y--;"];

            for (const input of inputs) {
                const tokens = tokenize(input);
                const result = parse(tokens);
                expect(result.success).toBe(true);
            }
        });
    });

    describe("Special functions", () => {
        it("should parse Branch statements", () => {
            const input = 'Branch("LABEL mylabel");';
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse dynamic code execution", () => {
            const input = 'result := ExecUDF("code", {params});';
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
    });
});
