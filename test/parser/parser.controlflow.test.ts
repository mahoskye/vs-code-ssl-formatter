/**
 * Control flow parsing tests
 */

import { tokenize } from "../../src/tokenizer";
import { parse } from "../../src/parser";
import { ASTNodeType } from "../../src/parser/ast";

describe("Parser - Control Flow", () => {
    describe("IF statements", () => {
        it("should parse simple IF statement", () => {
            const input = `:IF x > 0;
    y := x;
:ENDIF;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body).toHaveLength(1);
            expect(result.ast.body[0].kind).toBe(ASTNodeType.IfStatement);
        });

        it("should parse IF-ELSE statement", () => {
            const input = `:IF x > 0;
    y := x;
:ELSE;
    y := 0;
:ENDIF;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body[0].kind).toBe(ASTNodeType.IfStatement);
        });

        it("should parse nested IF statements", () => {
            const input = `:IF x > 0;
    :IF y > 0;
        z := x + y;
    :ENDIF;
:ENDIF;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
    });

    describe("WHILE loops", () => {
        it("should parse simple WHILE loop", () => {
            const input = `:WHILE x > 0;
    x := x - 1;
:ENDWHILE;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body[0].kind).toBe(ASTNodeType.WhileLoop);
        });

        it("should parse WHILE loop with EXITWHILE", () => {
            const input = `:WHILE .T.;
    :IF x <= 0;
        :EXITWHILE;
    :ENDIF;
    x := x - 1;
:ENDWHILE;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse WHILE loop with LOOP continue", () => {
            const input = `:WHILE x > 0;
    :IF x = 5;
        :LOOP;
    :ENDIF;
    x := x - 1;
:ENDWHILE;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
    });

    describe("FOR loops", () => {
        it("should parse simple FOR loop", () => {
            const input = `:FOR i := 1 :TO 10;
    sum := sum + i;
:NEXT;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body[0].kind).toBe(ASTNodeType.ForLoop);
        });

        it("should parse FOR loop with variable expressions", () => {
            const input = `:FOR i := start :TO end;
    ProcessItem(i);
:NEXT;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse FOR loop with EXITFOR", () => {
            const input = `:FOR i := 1 :TO 100;
    :IF i > 50;
        :EXITFOR;
    :ENDIF;
    sum := sum + i;
:NEXT;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
    });

    describe("SWITCH/CASE statements", () => {
        it("should parse simple BEGINCASE statement", () => {
            const input = `:BEGINCASE;
:CASE 1;
    result := "one";
:CASE 2;
    result := "two";
:OTHERWISE;
    result := "other";
:ENDCASE;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body[0].kind).toBe(ASTNodeType.SwitchStatement);
        });

        it("should parse CASE with multiple values", () => {
            const input = `:BEGINCASE;
:CASE 1, 2, 3;
    result := "small";
:CASE 4, 5, 6;
    result := "medium";
:OTHERWISE;
    result := "large";
:ENDCASE;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse CASE with EXITCASE", () => {
            const input = `:BEGINCASE;
:CASE 1;
    result := "one";
    :EXITCASE;
:CASE 2;
    result := "two";
:ENDCASE;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
    });

    describe("TRY-CATCH statements", () => {
        it("should parse simple TRY-CATCH", () => {
            const input = `:TRY;
    result := RiskyOperation();
:CATCH;
    result := "error";
:ENDTRY;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
            expect(result.ast.body[0].kind).toBe(ASTNodeType.TryBlock);
        });

        it("should parse TRY-CATCH-FINALLY", () => {
            const input = `:TRY;
    result := RiskyOperation();
:CATCH;
    result := "error";
:FINALLY;
    Cleanup();
:ENDTRY;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("should parse TRY-FINALLY without CATCH", () => {
            const input = `:TRY;
    result := RiskyOperation();
:FINALLY;
    Cleanup();
:ENDTRY;`;
            const tokens = tokenize(input);
            const result = parse(tokens);

            expect(result.success).toBe(true);
            expect(result.errors).toHaveLength(0);
        });
    });
});
