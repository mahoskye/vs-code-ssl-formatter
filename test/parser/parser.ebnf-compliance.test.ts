/**
 * EBNF Grammar Compliance Tests for SSL Parser
 * Tests specific adherence to the SSL EBNF grammar specification
 * Covers constructs that may not be tested elsewhere
 */

import { tokenize } from "../../src/tokenizer";
import { parse } from "../../src/parser";
import { ASTNodeType } from "../../src/parser/ast";

describe("Parser - EBNF Grammar Compliance", () => {
    describe("Date Literals", () => {
        it("should parse date literals with 3 components (year, month, day)", () => {
            const input = "date := {2024, 12, 25};";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);

            const assignment = result.ast.body[0] as any;
            expect(assignment.kind).toBe(ASTNodeType.Assignment);
            expect(assignment.right.kind).toBe(ASTNodeType.DateLiteral);
            expect(assignment.right.components).toHaveLength(3);
        });

        it("should parse date literals with 6 components (year, month, day, hour, minute, second)", () => {
            const input = "datetime := {2024, 12, 25, 14, 30, 45};";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);

            const assignment = result.ast.body[0] as any;
            expect(assignment.right.kind).toBe(ASTNodeType.DateLiteral);
            expect(assignment.right.components).toHaveLength(6);
        });

        it("should parse regular arrays when not matching date pattern", () => {
            const input = "arr := {1, 2, 3, 4};"; // 4 elements, not date
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            const assignment = result.ast.body[0] as any;
            expect(assignment.right.kind).toBe(ASTNodeType.ArrayLiteral);
        });
    });

    describe("Code Block Literals", () => {
        it("should parse code block literals with parameters", () => {
            const input = "block := {|x, y| x + y};";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);

            const assignment = result.ast.body[0] as any;
            expect(assignment.right.kind).toBe(ASTNodeType.CodeBlockLiteral);
            expect(assignment.right.parameters).toBeDefined();
            expect(assignment.right.parameters.identifiers).toHaveLength(2);
            expect(assignment.right.body).toHaveLength(1);
        });

        it("should parse code block literals without parameters", () => {
            const input = "block := {||42};";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            const assignment = result.ast.body[0] as any;
            expect(assignment.right.kind).toBe(ASTNodeType.CodeBlockLiteral);
            expect(assignment.right.body).toHaveLength(1);
        });

        it("should parse complex code block expressions", () => {
            const input = "filter := {|item| item > 10 .AND. item < 100};";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
    });

    describe("Default Parameter Declarations", () => {
        it("should parse :DEFAULT statements", () => {
            const input = ':DEFAULT param1, "default_value";';
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);

            const statement = result.ast.body[0] as any;
            expect(statement.kind).toBe(ASTNodeType.DefaultStatement);
            expect(statement.defaults.pairs).toHaveLength(1);
            expect(statement.defaults.pairs[0].identifier.value).toBe("param1");
        });
    });

    describe("Error Block Stanzas", () => {
        it("should parse :ERROR blocks", () => {
            const input = ":ERROR;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);

            const statement = result.ast.body[0] as any;
            expect(statement.kind).toBe(ASTNodeType.ErrorBlockStanza);
        });
    });

    describe("Inline Code Blocks", () => {
        it("should parse :BEGININLINECODE without language", () => {
            const input = ":BEGININLINECODE;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);

            const statement = result.ast.body[0] as any;
            expect(statement.kind).toBe(ASTNodeType.InlineCodeStart);
            expect(statement.language).toBeUndefined();
        });

        it("should parse :BEGININLINECODE with string language", () => {
            const input = ':BEGININLINECODE "SQL";';
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            const statement = result.ast.body[0] as any;
            expect(statement.kind).toBe(ASTNodeType.InlineCodeStart);
            expect(statement.language).toBeDefined();
            expect(statement.language.kind).toBe(ASTNodeType.StringLiteral);
        });

        it("should parse :BEGININLINECODE with identifier language", () => {
            const input = ":BEGININLINECODE JavaScript;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            const statement = result.ast.body[0] as any;
            expect(statement.language.value).toBe("JavaScript");
        });

        it("should parse :ENDINLINECODE", () => {
            const input = ":ENDINLINECODE;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            const statement = result.ast.body[0] as any;
            expect(statement.kind).toBe(ASTNodeType.InlineCodeEnd);
        });
    });

    describe("Scientific Notation Compliance", () => {
        it("should accept valid scientific notation formats", () => {
            const validFormats = ["x := 1.23e5;", "x := 4.56E-3;", "x := 0.5e1;", "x := 7.0e2;"];

            for (const input of validFormats) {
                const tokens = tokenize(input);
                const result = parse(tokens);
                expect(result.success).toBe(true);
            }
        });

        it("should reject invalid scientific notation formats", () => {
            const invalidFormats = [
                "x := 7e2;", // No decimal point before 'e'
                "x := .5e1;", // Leading decimal without zero
                "x := 9E+1;", // Explicit plus sign
            ];

            for (const input of invalidFormats) {
                const tokens = tokenize(input);
                const result = parse(tokens);
                expect(result.success).toBe(false);
            }
        });
    });

    describe("Assignment Operator Compliance", () => {
        it("should accept all valid assignment operators", () => {
            const operators = [":=", "+=", "-=", "*=", "/=", "^="];

            for (const op of operators) {
                const input = `x ${op} 5;`;
                const tokens = tokenize(input);
                const result = parse(tokens);
                expect(result.success).toBe(true);
            }
        });

        it("should reject = as assignment operator at statement level", () => {
            const input = "x = 42;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(false);
            expect(result.errors.length).toBeGreaterThan(0);
            expect(result.errors[0].message).toContain("Invalid assignment operator");
        });
    });

    describe("Boolean Literal Compliance", () => {
        it("should accept .T. and .F. boolean literals", () => {
            const input = "result := .T. .AND. .F.;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should reject TRUE and FALSE as boolean literals", () => {
            const inputs = ["result := TRUE;", "result := FALSE;"];

            for (const input of inputs) {
                const tokens = tokenize(input);
                const result = parse(tokens);
                expect(result.success).toBe(false);
                expect(result.errors.length).toBeGreaterThan(0);
                expect(result.errors[0].message).toContain("Invalid boolean literal");
            }
        });
    });

    describe("String Delimiter Compliance", () => {
        it("should accept both double and single quoted strings", () => {
            const inputs = ['x := "double quoted";', "x := 'single quoted';"];

            for (const input of inputs) {
                const tokens = tokenize(input);
                const result = parse(tokens);
                expect(result.success).toBe(true);
            }
        });
    });

    describe("Bitwise Operations as Functions", () => {
        it("should parse bitwise operations as function calls", () => {
            const operations = [
                "result := _AND(a, b);",
                "result := _OR(x, y);",
                "result := _XOR(p, q);",
                "result := _NOT(value);",
            ];

            for (const input of operations) {
                const tokens = tokenize(input);
                const result = parse(tokens);
                expect(result.success).toBe(true);

                const assignment = result.ast.body[0] as any;
                expect(assignment.right.kind).toBe(ASTNodeType.BitwiseOperation);
            }
        });
    });

    describe("Array Access Syntax Compliance", () => {
        it("should parse comma notation for multi-dimensional arrays", () => {
            const input = "value := matrix[1, 2, 3];";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            const assignment = result.ast.body[0] as any;
            expect(assignment.right.kind).toBe(ASTNodeType.ArrayAccess);
            expect(assignment.right.indices).toHaveLength(3);
        });

        it("should parse chained bracket notation", () => {
            const input = "value := matrix[1][2][3];";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            // Should create nested ArrayAccess nodes
            const assignment = result.ast.body[0] as any;
            expect(assignment.right.kind).toBe(ASTNodeType.ArrayAccess);
        });
    });

    describe("Property Access Syntax", () => {
        it("should use colon for property access", () => {
            const input = "value := object:property;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            const assignment = result.ast.body[0] as any;
            expect(assignment.right.kind).toBe(ASTNodeType.PropertyAccess);
            expect(assignment.right.object.value).toBe("object");
            expect(assignment.right.property.value).toBe("property");
        });
    });

    describe("Increment/Decrement Expression Compliance", () => {
        it("should parse prefix increment/decrement", () => {
            const inputs = ["result := ++counter;", "result := --index;"];

            for (const input of inputs) {
                const tokens = tokenize(input);
                const result = parse(tokens);
                expect(result.success).toBe(true);

                const assignment = result.ast.body[0] as any;
                expect(assignment.right.kind).toBe(ASTNodeType.IncrementExpression);
                expect(assignment.right.prefix).toBe(true);
            }
        });

        it("should parse postfix increment/decrement", () => {
            const inputs = ["result := counter++;", "result := index--;"];

            for (const input of inputs) {
                const tokens = tokenize(input);
                const result = parse(tokens);
                expect(result.success).toBe(true);

                const assignment = result.ast.body[0] as any;
                expect(assignment.right.kind).toBe(ASTNodeType.IncrementExpression);
                expect(assignment.right.prefix).toBe(false);
            }
        });
    });

    describe("NIL Literal Compliance", () => {
        it("should parse NIL literal", () => {
            const input = "value := NIL;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            const assignment = result.ast.body[0] as any;
            expect(assignment.right.kind).toBe(ASTNodeType.LiteralExpression);
            expect(assignment.right.token.type).toBe("NIL");
        });
    });
});
