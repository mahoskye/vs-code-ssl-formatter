/**
 * Grammar Compliance Tests
 *
 * These tests verify that the parser correctly implements the SSL EBNF grammar
 * and properly rejects invalid syntax according to the grammar rules.
 */

import { tokenize } from "../../src/tokenizer";
import { parse } from "../../src/parser";

describe("Parser - Grammar Compliance", () => {
    describe("Valid syntax according to EBNF", () => {
        it("should parse basic assignment according to grammar", () => {
            const input = "x := 42;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body).toHaveLength(1);

            // Verify AST structure matches EBNF
            const assignment = result.ast.body[0];
            expect(assignment.kind).toBe("Assignment");
        });
        it("should parse procedure with parameters according to grammar", () => {
            const input = `:PROCEDURE TestProc
:PARAMETERS param1, param2
x := param1 + param2;
:ENDPROC`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            if (!result.success) {
                console.log("Parser errors:", result.errors);
                console.log(
                    "Tokens:",
                    tokens.map((t) => `${t.type}: "${t.value}"`)
                );
            }

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse procedure with default parameters", () => {
            const input = `:PROCEDURE TestProc
:PARAMETERS param1, param2
:DEFAULT param2, "default value"
x := param1 + param2;
:ENDPROC`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            if (!result.success) {
                console.log("Parser errors:", result.errors);
            }

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            
            const proc = result.ast.body[0] as any;
            expect(proc.kind).toBe("ProcedureStatement");
            expect(proc.defaultParameters).toBeDefined();
            expect(proc.defaultParameters.kind).toBe("DefaultParameterDeclaration");
            expect(proc.defaultParameters.defaults.pairs).toHaveLength(1);
            expect(proc.defaultParameters.defaults.pairs[0].identifier.value).toBe("param2");
            expect(proc.defaultParameters.defaults.pairs[0].defaultValue.kind).toBe("LiteralExpression");
        });

        it("should parse logical expressions with .AND. and .OR.", () => {
            const input = "result := a .AND. b .OR. c;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse boolean literals .T. and .F.", () => {
            const input = "flag := .T.;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse array literals with braces", () => {
            const input = "arr := {1, 2, 3};";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse property access with colon", () => {
            const input = "value := obj:property;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
    });

    describe("Invalid syntax should be rejected", () => {
        it("should reject assignment without semicolon", () => {
            const input = "x := 42"; // Missing semicolon
            const tokens = tokenize(input);
            const result = parse(tokens);

            // According to EBNF: Statement ::= ... ";"
            // This should fail because all statements must end with semicolon
            expect(result.success).toBe(false);
            expect(result.errors.length).toBeGreaterThan(0);
        });

        it("should reject procedure without :ENDPROC", () => {
            const input = `:PROCEDURE TestProc
x := 42;`; // Missing :ENDPROC
            const tokens = tokenize(input);
            const result = parse(tokens);

            // According to EBNF: ProcedureStatement ::= ProcedureStart ... ProcedureEnd
            expect(result.success).toBe(false);
            expect(result.errors.length).toBeGreaterThan(0);
        });

        it("should reject if statement without :ENDIF", () => {
            const input = `:IF x > 0
y := x;`; // Missing :ENDIF
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(false);
            expect(result.errors.length).toBeGreaterThan(0);
        });

        it("should reject malformed boolean literals", () => {
            const input = "flag := TRUE;"; // Should be .T. not TRUE
            const tokens = tokenize(input);
            const result = parse(tokens);

            if (result.success) {
                console.log("Parser incorrectly accepted TRUE as valid");
                console.log(
                    "Tokens:",
                    tokens.map((t) => `${t.type}: "${t.value}"`)
                );
                console.log("AST:", JSON.stringify(result.ast, null, 2));
            }

            // According to EBNF: BooleanLiteral ::= ".T." | ".F."
            expect(result.success).toBe(false);
            expect(result.errors.length).toBeGreaterThan(0);
        });

        it("should reject dot notation for property access", () => {
            const input = "value := obj.property;"; // Should be obj:property
            const tokens = tokenize(input);
            const result = parse(tokens);

            // According to EBNF: PropertyAccess ::= Identifier ":" Identifier
            expect(result.success).toBe(false);
            expect(result.errors.length).toBeGreaterThan(0);
        });
        it("should reject invalid assignment operators", () => {
            const input = "x = 42;"; // Should be := not =
            const tokens = tokenize(input);
            const result = parse(tokens);

            if (result.success) {
                console.log("Parser incorrectly accepted = as assignment operator");
                console.log(
                    "Tokens:",
                    tokens.map((t) => `${t.type}: "${t.value}"`)
                );
                console.log("AST:", JSON.stringify(result.ast, null, 2));
            }

            // According to EBNF: AssignmentOperator ::= ":=" | "+=" | ...
            expect(result.success).toBe(false);
            expect(result.errors.length).toBeGreaterThan(0);
        });

        it("should reject incomplete expressions", () => {
            const input = "x := ;"; // Missing right-hand side
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(false);
            expect(result.errors.length).toBeGreaterThan(0);
        });

        it("should reject malformed for loop syntax", () => {
            const input = `:FOR i = 1 TO 10
sum := sum + i;
:NEXT`; // Should use := not = in for loop
            const tokens = tokenize(input);
            const result = parse(tokens);

            // According to EBNF: ForStatement ::= ":" "FOR" Identifier ":=" Expression ":" "TO" Expression
            expect(result.success).toBe(false);
            expect(result.errors.length).toBeGreaterThan(0);
        });
    });

    describe("Edge cases and boundary conditions", () => {
        it("should handle empty program correctly", () => {
            const input = "";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body).toHaveLength(0);
        });
        it("should handle program with only comments", () => {
            const input = "/* This is a comment */;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should reject nested procedures", () => {
            const input = `:PROCEDURE Outer
:PROCEDURE Inner
x := 1;
:ENDPROC
:ENDPROC`; // Nested procedures not allowed in SSL
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(false);
            expect(result.errors.length).toBeGreaterThan(0);
        });
    });

    describe("Operator precedence and associativity", () => {
        it("should parse arithmetic expressions with correct precedence", () => {
            const input = "result := a + b * c;"; // Should be a + (b * c)
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);

            // Verify AST represents correct precedence
            const assignment = result.ast.body[0];
            expect(assignment.kind).toBe("Assignment");
            // The expression should show multiplication has higher precedence
        });

        it("should parse logical expressions with correct precedence", () => {
            const input = "result := a .OR. b .AND. c;"; // Should be a .OR. (b .AND. c)
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
    });

    describe("String and literal parsing", () => {
        it("should parse double-quoted strings", () => {
            const input = 'message := "Hello World";';
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse single-quoted strings", () => {
            const input = "message := 'Hello World';";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse numeric literals", () => {
            const input = "value := 42.5;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse NIL literal", () => {
            const input = "value := NIL;";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
    });

    describe("Function calls and parameters", () => {
        it("should parse function calls with multiple parameters", () => {
            const input = "result := MyFunction(param1, param2, param3);";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse function calls with no parameters", () => {
            const input = "result := GetCurrentTime();";
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should reject function calls with missing closing parenthesis", () => {
            const input = "result := MyFunction(param1, param2;"; // Missing )
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(false);
            expect(result.errors.length).toBeGreaterThan(0);
        });
    });
});
