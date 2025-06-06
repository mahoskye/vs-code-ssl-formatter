// Tests for the token types in the tokenizer module
import { TokenType } from "../../src/tokenizer";

describe("TokenType", () => {
    describe("Token type enumeration", () => {
        it("should have all basic literal types", () => {
            expect(TokenType.NUMBER).toBeDefined();
            expect(TokenType.STRING).toBeDefined();
            expect(TokenType.BOOLEAN).toBeDefined();
            expect(TokenType.NIL).toBeDefined();
            expect(TokenType.DATE).toBeDefined();
        });

        it("should have all control flow keywords", () => {
            expect(TokenType.IF).toBeDefined();
            expect(TokenType.ELSE).toBeDefined();
            expect(TokenType.ENDIF).toBeDefined();
            expect(TokenType.WHILE).toBeDefined();
            expect(TokenType.ENDWHILE).toBeDefined();
            expect(TokenType.FOR).toBeDefined();
            expect(TokenType.NEXT).toBeDefined();
        });

        it("should have all assignment operators", () => {
            expect(TokenType.ASSIGN).toBeDefined();
            expect(TokenType.PLUS_ASSIGN).toBeDefined();
            expect(TokenType.MINUS_ASSIGN).toBeDefined();
            expect(TokenType.MULT_ASSIGN).toBeDefined();
            expect(TokenType.DIV_ASSIGN).toBeDefined();
            expect(TokenType.POWER_ASSIGN).toBeDefined();
        });

        it("should have all arithmetic operators", () => {
            expect(TokenType.PLUS).toBeDefined();
            expect(TokenType.MINUS).toBeDefined();
            expect(TokenType.MULTIPLY).toBeDefined();
            expect(TokenType.DIVIDE).toBeDefined();
            expect(TokenType.MODULO).toBeDefined();
            expect(TokenType.POWER).toBeDefined();
        });

        it("should have all comparison operators", () => {
            expect(TokenType.EQUAL).toBeDefined();
            expect(TokenType.STRICT_EQUAL).toBeDefined();
            expect(TokenType.NOT_EQUAL).toBeDefined();
            expect(TokenType.LESS_THAN).toBeDefined();
            expect(TokenType.GREATER_THAN).toBeDefined();
            expect(TokenType.LESS_EQUAL).toBeDefined();
            expect(TokenType.GREATER_EQUAL).toBeDefined();
        });

        it("should have all logical operators", () => {
            expect(TokenType.AND).toBeDefined();
            expect(TokenType.OR).toBeDefined();
            expect(TokenType.NOT).toBeDefined();
            expect(TokenType.LOGICAL_NOT).toBeDefined();
        });

        it("should have all punctuation tokens", () => {
            expect(TokenType.SEMICOLON).toBeDefined();
            expect(TokenType.COMMA).toBeDefined();
            expect(TokenType.COLON).toBeDefined();
            expect(TokenType.DOT).toBeDefined();
            expect(TokenType.LPAREN).toBeDefined();
            expect(TokenType.RPAREN).toBeDefined();
            expect(TokenType.LBRACE).toBeDefined();
            expect(TokenType.RBRACE).toBeDefined();
            expect(TokenType.LBRACKET).toBeDefined();
            expect(TokenType.RBRACKET).toBeDefined();
        });

        it("should have SSL-specific tokens", () => {
            expect(TokenType.PROCEDURE).toBeDefined();
            expect(TokenType.ENDPROC).toBeDefined();
            expect(TokenType.CLASS).toBeDefined();
            expect(TokenType.INHERIT).toBeDefined();
            expect(TokenType.SQL_PARAM_NAMED).toBeDefined();
            expect(TokenType.SQL_PARAM_UNNAMED).toBeDefined();
        });

        it("should have comment types", () => {
            expect(TokenType.BLOCK_COMMENT).toBeDefined();
            expect(TokenType.SINGLE_LINE_COMMENT).toBeDefined();
            expect(TokenType.REGION_COMMENT).toBeDefined();
            expect(TokenType.ENDREGION_COMMENT).toBeDefined();
        });

        it("should have special tokens", () => {
            expect(TokenType.IDENTIFIER).toBeDefined();
            expect(TokenType.NEWLINE).toBeDefined();
            expect(TokenType.WHITESPACE).toBeDefined();
            expect(TokenType.EOF).toBeDefined();
            expect(TokenType.UNKNOWN).toBeDefined();
        });
    });

    describe("Token type values", () => {
        it("should have string values that match the key names", () => {
            expect(TokenType.IDENTIFIER).toBe("IDENTIFIER");
            expect(TokenType.NUMBER).toBe("NUMBER");
            expect(TokenType.STRING).toBe("STRING");
            expect(TokenType.IF).toBe("IF");
            expect(TokenType.ASSIGN).toBe("ASSIGN");
        });
    });
});
