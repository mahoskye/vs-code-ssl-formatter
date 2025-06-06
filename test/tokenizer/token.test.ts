// Tests for the Token class in the tokenizer module

// filepath: c:\Users\mahos\OneDrive\Documents\Projects\vs-code-ssl-formatter\test\tokenizer\token.test.ts
// Tests for the Token class in the tokenizer module

import {
    createToken,
    createPosition,
    isKeywordType,
    isOperatorType,
    isLiteralType,
    isPunctuationType,
} from "../../src/tokenizer";
import { TokenType } from "../../src/tokenizer";

describe("Token", () => {
    describe("Token creation", () => {
        it("should create a token with correct properties", () => {
            const position = createPosition(0, 0, 0);
            const token = createToken(TokenType.IDENTIFIER, "testVar", position);

            expect(token.type).toBe(TokenType.IDENTIFIER);
            expect(token.value).toBe("testVar");
            expect(token.range.start).toEqual(position);
            expect(token.isKeyword).toBe(false);
            expect(token.isOperator).toBe(false);
            expect(token.isLiteral).toBe(false);
            expect(token.isPunctuation).toBe(false);
        });

        it("should set isKeyword flag for keyword tokens", () => {
            const position = createPosition(0, 0, 0);
            const token = createToken(TokenType.IF, "IF", position);

            expect(token.isKeyword).toBe(true);
        });

        it("should set isOperator flag for operator tokens", () => {
            const position = createPosition(0, 0, 0);
            const token = createToken(TokenType.ASSIGN, ":=", position);

            expect(token.isOperator).toBe(true);
        });

        it("should set isLiteral flag for literal tokens", () => {
            const position = createPosition(0, 0, 0);
            const token = createToken(TokenType.STRING, '"test"', position);

            expect(token.isLiteral).toBe(true);
        });

        it("should set isPunctuation flag for punctuation tokens", () => {
            const position = createPosition(0, 0, 0);
            const token = createToken(TokenType.SEMICOLON, ";", position);

            expect(token.isPunctuation).toBe(true);
        });
    });

    describe("Type guards", () => {
        it("should correctly identify keyword types", () => {
            expect(isKeywordType(TokenType.IF)).toBe(true);
            expect(isKeywordType(TokenType.WHILE)).toBe(true);
            expect(isKeywordType(TokenType.PROCEDURE)).toBe(true);
            expect(isKeywordType(TokenType.IDENTIFIER)).toBe(false);
        });

        it("should correctly identify operator types", () => {
            expect(isOperatorType(TokenType.ASSIGN)).toBe(true);
            expect(isOperatorType(TokenType.PLUS)).toBe(true);
            expect(isOperatorType(TokenType.AND)).toBe(true);
            expect(isOperatorType(TokenType.IDENTIFIER)).toBe(false);
        });

        it("should correctly identify literal types", () => {
            expect(isLiteralType(TokenType.NUMBER)).toBe(true);
            expect(isLiteralType(TokenType.STRING)).toBe(true);
            expect(isLiteralType(TokenType.BOOLEAN)).toBe(true);
            expect(isLiteralType(TokenType.IDENTIFIER)).toBe(false);
        });

        it("should correctly identify punctuation types", () => {
            expect(isPunctuationType(TokenType.SEMICOLON)).toBe(true);
            expect(isPunctuationType(TokenType.LPAREN)).toBe(true);
            expect(isPunctuationType(TokenType.RBRACE)).toBe(true);
            expect(isPunctuationType(TokenType.IDENTIFIER)).toBe(false);
        });
    });

    describe("Position handling", () => {
        it("should create positions correctly", () => {
            const position = createPosition(5, 10, 100);

            expect(position.line).toBe(5);
            expect(position.column).toBe(10);
            expect(position.offset).toBe(100);
        });

        it("should calculate end position automatically", () => {
            const start = createPosition(0, 0, 0);
            const token = createToken(TokenType.IDENTIFIER, "testVar", start);

            expect(token.range.end.column).toBe(7); // 'testVar' has length 7
            expect(token.range.end.offset).toBe(7);
        });
    });
});
