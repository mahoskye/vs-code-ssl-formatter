/**
 * Class and procedure parsing tests
 */

import { tokenize } from "../../src/tokenizer";
import { parse } from "../../src/parser";
import { ASTNodeType } from "../../src/parser/ast";

describe("Parser - Classes and Procedures", () => {
    describe("Procedure definitions", () => {
        it("should parse simple procedure", () => {
            const input = `:PROCEDURE TestProc;
x := 42;
:ENDPROC;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body).toHaveLength(1);
            expect(result.ast.body[0].kind).toBe(ASTNodeType.ProcedureStatement);
        });

        it("should parse procedure with parameters", () => {
            const input = `:PROCEDURE TestProc;
:PARAMETERS param1, param2;
result := param1 + param2;
:ENDPROC;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse procedure with default parameters", () => {
            const input = `:PROCEDURE TestProc;
:PARAMETERS param1, param2;
:DEFAULT param2, "default_value";
result := param1 + param2;
:ENDPROC;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should reject nested procedures", () => {
            const input = `:PROCEDURE OuterProc;
:PROCEDURE InnerProc;
x := 1;
:ENDPROC;
:ENDPROC;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(false);
            expect(result.errors.length).toBeGreaterThan(0);
        });
    });

    describe("Class definitions", () => {
        it("should parse simple class", () => {
            const input = `:CLASS clsCar;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body).toHaveLength(1);
            expect(result.ast.body[0].kind).toBe(ASTNodeType.ClassDefinition);
        });
        it("should parse class with inheritance", () => {
            const input = `:CLASS clsCar;
:INHERIT Research.clsVehicle;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
        it("should parse class with fields", () => {
            const input = `:CLASS clsCar;
:DECLARE MSRPPrice, field2, field3;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
        it("should parse class with methods", () => {
            const input = `:CLASS clsCar;
:PROCEDURE Constructor;
Me:Wheels := 4;
:ENDPROC;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
        it("should parse complete class definition", () => {
            const input = `:CLASS clsCar;
:INHERIT Research.clsVehicle;
:DECLARE MSRPPrice, Wheels;
:PROCEDURE Constructor;
Me:Wheels := 4;
:ENDPROC;
:PROCEDURE GetPrice;
:RETURN Me:MSRPPrice + 500; /* Car owner fee ;
:ENDPROC;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
    });
});
