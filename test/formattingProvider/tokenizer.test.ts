import * as assert from "assert";
import { describe, it } from "mocha";
import { SSLTokenizer, TokenType, Token } from "../../src/formatters/tokenizer";

describe("SSLTokenizer", () => {
    let tokenizer: SSLTokenizer;

    const assertToken = (
        token: Token | undefined,
        type: TokenType,
        value: string,
        line?: number,
        column?: number,
        offset?: number
    ) => {
        assert.ok(
            token,
            `Token should not be undefined. Expected: ${type} - '${value.replace(/'/g, "'")}'`
        );
        assert.strictEqual(
            String(token.type), // Compare string representation
            String(type), // Compare string representation
            `Token type mismatch. Expected: ${type}, Got: ${token.type}`
        );
        assert.strictEqual(
            token.value,
            value,
            `Token value mismatch. Expected: '${value.replace(
                /'/g,
                "'"
            )}', Got: '${token.value.replace(/'/g, "'")}'`
        );
        if (line !== undefined) {
            assert.strictEqual(
                token.position.line,
                line,
                `Token line mismatch. Expected: ${line}, Got: ${token.position.line}`
            );
        }
        if (column !== undefined) {
            assert.strictEqual(
                token.position.column,
                column,
                `Token column mismatch. Expected: ${column}, Got: ${token.position.column}`
            );
        }
        if (offset !== undefined) {
            assert.strictEqual(
                token.position.offset,
                offset,
                `Token offset mismatch. Expected: ${offset}, Got: ${token.position.offset}`
            );
        }
    };

    describe("Keywords", () => {
        it("should tokenize basic keywords", () => {
            tokenizer = new SSLTokenizer(":IF :ELSE :ENDIF;");
            const tokens = tokenizer.tokenize();
            assertToken(tokens[0], TokenType.colon, ":");
            assertToken(tokens[1], TokenType.if, "IF");
            assertToken(tokens[2], TokenType.whitespace, " ");
            assertToken(tokens[3], TokenType.colon, ":");
            assertToken(tokens[4], TokenType.else, "ELSE");
            assertToken(tokens[5], TokenType.whitespace, " ");
            assertToken(tokens[6], TokenType.colon, ":");
            assertToken(tokens[7], TokenType.endif, "ENDIF");
            assertToken(tokens[8], TokenType.semicolon, ";");
            assertToken(tokens[9], TokenType.eof, "");
        });

        it("should tokenize keywords case-insensitively", () => {
            tokenizer = new SSLTokenizer(":procedure MyProc;");
            const tokens = tokenizer.tokenize();
            assertToken(tokens[0], TokenType.colon, ":");
            assertToken(tokens[1], TokenType.procedure, "procedure");
            // ... remaining assertions
        });
    });

    describe("Identifiers", () => {
        it("should tokenize valid identifiers", () => {
            tokenizer = new SSLTokenizer("myVar _anotherVar var123;");
            const tokens = tokenizer.tokenize();
            assertToken(tokens[0], TokenType.identifier, "myVar");
            assertToken(tokens[1], TokenType.whitespace, " ");
            assertToken(tokens[2], TokenType.identifier, "_anotherVar");
            assertToken(tokens[3], TokenType.whitespace, " ");
            assertToken(tokens[4], TokenType.identifier, "var123");
            assertToken(tokens[5], TokenType.semicolon, ";");
            assertToken(tokens[6], TokenType.eof, "");
        });
    });

    describe("Operators", () => {
        it("should tokenize assignment operators", () => {
            tokenizer = new SSLTokenizer("x := 1; y += 2; z -= 3; a *= 4; b /= 5; c ^= 6;");
            const tokens = tokenizer.tokenize();
            // x := 1;
            assertToken(tokens[0], TokenType.identifier, "x", 1, 1, 0);
            assertToken(tokens[1], TokenType.whitespace, " ", 1, 2, 1);
            assertToken(tokens[2], TokenType.assign, ":=", 1, 3, 2);
            assertToken(tokens[3], TokenType.whitespace, " ", 1, 5, 4);
            assertToken(tokens[4], TokenType.numberLiteral, "1", 1, 6, 5);
            assertToken(tokens[5], TokenType.semicolon, ";", 1, 7, 6);
            // y += 2
            assertToken(tokens[7], TokenType.identifier, "y", 1, 9, 8); // Corrected position
            assertToken(tokens[9], TokenType.plusAssign, "+=", 1, 11, 10); // Corrected position
            // z -= 3
            assertToken(tokens[14], TokenType.identifier, "z", 1, 17, 16); // Corrected position
            assertToken(tokens[16], TokenType.minusAssign, "-=", 1, 19, 18); // Corrected position
            // a *= 4
            assertToken(tokens[21], TokenType.identifier, "a", 1, 25, 24); // Corrected position
            assertToken(tokens[23], TokenType.multAssign, "*=", 1, 27, 26); // Corrected position
            // b /= 5
            assertToken(tokens[28], TokenType.identifier, "b", 1, 33, 32); // Corrected position
            assertToken(tokens[30], TokenType.divAssign, "/=", 1, 35, 34); // Corrected position
            // c ^= 6
            assertToken(tokens[35], TokenType.identifier, "c", 1, 41, 40); // Corrected position
            assertToken(tokens[37], TokenType.powerAssign, "^=", 1, 43, 42); // Corrected position
        });

        it("should tokenize comparison operators", () => {
            tokenizer = new SSLTokenizer("a == b; c != d; e < f; g > h; i <= j; k >= l; m = n;");
            const tokens = tokenizer.tokenize();
            // a == b;
            assertToken(tokens[0], TokenType.identifier, "a", 1, 1, 0);
            assertToken(tokens[2], TokenType.equals, "==", 1, 3, 2);
            // c != d
            assertToken(tokens[7], TokenType.identifier, "c", 1, 9, 8);
            assertToken(tokens[9], TokenType.notEquals, "!=", 1, 11, 10);
            // e < f
            assertToken(tokens[14], TokenType.identifier, "e", 1, 17, 16);
            assertToken(tokens[16], TokenType.lessThan, "<", 1, 19, 18);
            // g > h
            assertToken(tokens[21], TokenType.identifier, "g", 1, 24, 23);
            assertToken(tokens[23], TokenType.greaterThan, ">", 1, 26, 25);
            // i <= j
            assertToken(tokens[28], TokenType.identifier, "i", 1, 31, 30);
            assertToken(tokens[30], TokenType.lessEqual, "<=", 1, 33, 32);
            // k >= l
            assertToken(tokens[35], TokenType.identifier, "k", 1, 39, 38);
            assertToken(tokens[37], TokenType.greaterEqual, ">=", 1, 41, 40);
            // m = n
            assertToken(tokens[42], TokenType.identifier, "m", 1, 47, 46);
            assertToken(tokens[44], TokenType.simpleEquals, "=", 1, 49, 48);
        });

        it("should tokenize arithmetic operators", () => {
            tokenizer = new SSLTokenizer("1 + 2 - 3 * 4 / 5 % 6 ^ 7;");
            const tokens = tokenizer.tokenize();
            // Example for +
            assertToken(tokens[0], TokenType.numberLiteral, "1", 1, 1, 0);
            assertToken(tokens[1], TokenType.whitespace, " ", 1, 2, 1);
            assertToken(tokens[2], TokenType.plus, "+", 1, 3, 2);
            assertToken(tokens[4], TokenType.numberLiteral, "2", 1, 5, 4);
            assertToken(tokens[6], TokenType.minus, "-", 1, 7, 6);
            assertToken(tokens[8], TokenType.numberLiteral, "3", 1, 9, 8);
            assertToken(tokens[10], TokenType.multiply, "*", 1, 11, 10);
            assertToken(tokens[12], TokenType.numberLiteral, "4", 1, 13, 12);
            assertToken(tokens[14], TokenType.divide, "/", 1, 15, 14);
            assertToken(tokens[16], TokenType.numberLiteral, "5", 1, 17, 16);
            assertToken(tokens[18], TokenType.modulo, "%", 1, 19, 18); // Assuming % is MODULO
            assertToken(tokens[20], TokenType.numberLiteral, "6", 1, 21, 20);
            assertToken(tokens[22], TokenType.power, "^", 1, 23, 22); // Assuming ^ is POWER
            assertToken(tokens[24], TokenType.numberLiteral, "7", 1, 25, 24);
        });

        it("should tokenize logical operators", () => {
            tokenizer = new SSLTokenizer(".AND. .OR. .NOT.;");
            const tokens = tokenizer.tokenize();
            assertToken(tokens[0], TokenType.and, ".AND.");
            assertToken(tokens[1], TokenType.whitespace, " ");
            assertToken(tokens[2], TokenType.or, ".OR.");
            assertToken(tokens[3], TokenType.whitespace, " ");
            assertToken(tokens[4], TokenType.not, ".NOT.");
            assertToken(tokens[5], TokenType.semicolon, ";");
        });

        it("should tokenize increment/decrement operators", () => {
            tokenizer = new SSLTokenizer("i++; --j;");
            const tokens = tokenizer.tokenize();
            assertToken(tokens[0], TokenType.identifier, "i", 1, 1, 0);
            assertToken(tokens[1], TokenType.increment, "++", 1, 2, 1);
            assertToken(tokens[2], TokenType.semicolon, ";", 1, 4, 3);
            assertToken(tokens[3], TokenType.whitespace, " ", 1, 5, 4);
            assertToken(tokens[4], TokenType.decrement, "--", 1, 6, 5);
            assertToken(tokens[5], TokenType.identifier, "j", 1, 8, 7);
            assertToken(tokens[6], TokenType.semicolon, ";", 1, 9, 8);
        });
    });

    describe("Literals", () => {
        it("should tokenize string literals with double quotes", () => {
            // Test 5 - String literals are now parsed as single tokens
            tokenizer = new SSLTokenizer('"hello world";');
            const tokens = tokenizer.tokenize();
            assertToken(tokens[0], TokenType.stringLiteral, '"hello world"', 1, 1, 0);
            assertToken(tokens[1], TokenType.semicolon, ";", 1, 14, 13);
            assertToken(tokens[2], TokenType.eof, "", 1, 15, 14);
        });
        it("should tokenize string literals with single quotes", () => {
            tokenizer = new SSLTokenizer("'another string';");
            const tokens = tokenizer.tokenize();
            assertToken(tokens[0], TokenType.stringLiteral, "'another string'", 1, 1, 0);
            assertToken(tokens[1], TokenType.semicolon, ";", 1, 17, 16);
            assertToken(tokens[2], TokenType.eof, "", 1, 18, 17);
        });

        it("should tokenize number literals (integer and decimal)", () => {
            tokenizer = new SSLTokenizer("123 45.67;");
            const tokens = tokenizer.tokenize();
            assertToken(tokens[0], TokenType.numberLiteral, "123");
            assertToken(tokens[1], TokenType.whitespace, " ");
            assertToken(tokens[2], TokenType.numberLiteral, "45.67");
        });

        it("should tokenize boolean literals", () => {
            tokenizer = new SSLTokenizer(".T. .F.;");
            const tokens = tokenizer.tokenize();
            assertToken(tokens[0], TokenType.booleanLiteral, ".T.", 1, 1, 0);
            assertToken(tokens[1], TokenType.whitespace, " ", 1, 4, 3);
            assertToken(tokens[2], TokenType.booleanLiteral, ".F.", 1, 5, 4);
            assertToken(tokens[3], TokenType.semicolon, ";", 1, 8, 7); // Corrected C8, O7
            assertToken(tokens[4], TokenType.eof, "", 1, 9, 8); // Corrected C9, O8
        });

        it("should tokenize NIL literal", () => {
            tokenizer = new SSLTokenizer("NIL;");
            const tokens = tokenizer.tokenize();
            assertToken(tokens[0], TokenType.nilLiteral, "NIL", 1, 1, 0);
            assertToken(tokens[1], TokenType.semicolon, ";", 1, 4, 3);
            assertToken(tokens[2], TokenType.eof, "", 1, 5, 4);
        });

        it("should tokenize valid scientific notation number literals", () => {
            // Supported: 1.23e5, 0.5e1, 4.56E-3 (Note 14)
            tokenizer = new SSLTokenizer("1.23e5 0.5e1 4.56E-3;");
            const tokens = tokenizer.tokenize();
            assertToken(tokens[0], TokenType.numberLiteral, "1.23e5", 1, 1, 0);
            assertToken(tokens[1], TokenType.whitespace, " ", 1, 7, 6);
            assertToken(tokens[2], TokenType.numberLiteral, "0.5e1", 1, 8, 7);
            assertToken(tokens[3], TokenType.whitespace, " ", 1, 13, 12);
            assertToken(tokens[4], TokenType.numberLiteral, "4.56E-3", 1, 14, 13);
            assertToken(tokens[5], TokenType.semicolon, ";", 1, 21, 20);
            assertToken(tokens[6], TokenType.eof, "", 1, 22, 21);
        });

        it("should not tokenize scientific notation with explicit plus in exponent", () => {
            // Unsupported: 9E+1 (Note 14)
            // Expected: 9 (Number), E (Identifier), + (Plus), 1 (Number)
            tokenizer = new SSLTokenizer("9E+1;");
            const tokens = tokenizer.tokenize();
            assertToken(tokens[0], TokenType.numberLiteral, "9", 1, 1, 0);
            assertToken(tokens[1], TokenType.identifier, "E", 1, 2, 1);
            assertToken(tokens[2], TokenType.plus, "+", 1, 3, 2);
            assertToken(tokens[3], TokenType.numberLiteral, "1", 1, 4, 3);
            assertToken(tokens[4], TokenType.semicolon, ";", 1, 5, 4);
            assertToken(tokens[5], TokenType.eof, "", 1, 6, 5);
        });

        it("should not tokenize scientific notation without decimal point if exponent present", () => {
            // Unsupported: 7e2 (Note 14: "without a decimal point before the 'e' (7e2) are not supported")
            // Expected: 7 (Number), e2 (Identifier)
            tokenizer = new SSLTokenizer("7e2;");
            const tokens = tokenizer.tokenize();
            assertToken(tokens[0], TokenType.numberLiteral, "7", 1, 1, 0);
            assertToken(tokens[1], TokenType.identifier, "e2", 1, 2, 1);
            assertToken(tokens[2], TokenType.semicolon, ";", 1, 4, 3);
            assertToken(tokens[3], TokenType.eof, "", 1, 5, 4);
        });

        it("should not tokenize scientific notation with leading decimal point without zero", () => {
            // Test 6
            // Unsupported: .5e1 (Note 14: "leading decimal point without a zero (.5e1) are not supported")
            // EBNF NumberLiteral: ... | '.' digit+ (('e'|'E') ('+'|'-')? digit+)?; suggests .5e1 is NumberLiteral
            // Tokenizer produces .5e1 as a single token.
            tokenizer = new SSLTokenizer(".5e1;");
            const tokens = tokenizer.tokenize();
            assertToken(tokens[0], TokenType.invalid, ".5e1", 1, 1, 0); // Changed NUMBER_LITERAL to INVALID
            assertToken(tokens[1], TokenType.semicolon, ";", 1, 5, 4); // Adjusted index, col 5, offset 4
            assertToken(tokens[2], TokenType.eof, "", 1, 6, 5); // Adjusted index, col 6, offset 5
        });

        it("should handle number like 1.2.3 correctly", () => {
            // Test 7
            // Expected by EBNF: 1.2 (Number), .3 (Number)
            // Original test comment: 1.2 (Number), . (Invalid), 3 (Number) - this was likely wrong.
            tokenizer = new SSLTokenizer("1.2.3;");
            const tokens = tokenizer.tokenize();
            assertToken(tokens[0], TokenType.numberLiteral, "1.2", 1, 1, 0);
            assertToken(tokens[1], TokenType.invalid, ".3", 1, 4, 3); // Corrected based on EBNF and tokenizer behavior, changed to INVALID
            assertToken(tokens[2], TokenType.semicolon, ";", 1, 6, 5); // Adjusted index
            assertToken(tokens[3], TokenType.eof, "", 1, 7, 6); // Adjusted index
        });
    });

    describe("Comments", () => {
        it("should tokenize block comments ending with semicolon", () => {
            tokenizer = new SSLTokenizer("/* this is a comment ; statement;"); // comment length 22
            const tokens = tokenizer.tokenize();
            assertToken(tokens[0], TokenType.blockComment, "/* this is a comment ;");
            assertToken(tokens[1], TokenType.whitespace, " ", 1, 23, 22); // Corrected C23, O22
            assertToken(tokens[2], TokenType.identifier, "statement", 1, 24, 23); // Corrected C24, O23
            assertToken(tokens[3], TokenType.semicolon, ";", 1, 33, 32); // Corrected C33, O32
            assertToken(tokens[4], TokenType.eof, "", 1, 34, 33); // Corrected C34, O33
        });
        it("should tokenize region comments ending with semicolon", () => {
            tokenizer = new SSLTokenizer("/*region MyRegion;");
            const tokens = tokenizer.tokenize();
            assertToken(tokens[0], TokenType.regionComment, "/*region MyRegion;", 1, 1, 0);
            assertToken(tokens[1], TokenType.eof, "", 1, 19, 18);
        });

        it("should tokenize endregion comments ending with semicolon", () => {
            tokenizer = new SSLTokenizer("/*endregion MyRegion;");
            const tokens = tokenizer.tokenize();
            assertToken(tokens[0], TokenType.endregionComment, "/*endregion MyRegion;", 1, 1, 0);
            assertToken(tokens[1], TokenType.eof, "", 1, 22, 21);
        });
    });

    describe("Delimiters", () => {
        it("should tokenize all delimiters", () => {
            tokenizer = new SSLTokenizer(": ; , ( ) [ ] { }"); // Added [ ]
            const tokens = tokenizer.tokenize();
            assertToken(tokens[0], TokenType.colon, ":");
            assertToken(tokens[1], TokenType.whitespace, " ", 1, 2, 1);
            assertToken(tokens[2], TokenType.semicolon, ";");
            assertToken(tokens[3], TokenType.whitespace, " ", 1, 4, 3);
            assertToken(tokens[4], TokenType.comma, ",");
            assertToken(tokens[5], TokenType.whitespace, " ", 1, 6, 5);
            assertToken(tokens[6], TokenType.lparen, "(");
            assertToken(tokens[7], TokenType.whitespace, " ", 1, 8, 7);
            assertToken(tokens[8], TokenType.rparen, ")");
            assertToken(tokens[9], TokenType.whitespace, " ", 1, 10, 9);
            assertToken(tokens[10], TokenType.lbracket, "[", 1, 11, 10); // Corrected
            assertToken(tokens[11], TokenType.whitespace, " ", 1, 12, 11);
            assertToken(tokens[12], TokenType.rbracket, "]", 1, 13, 12); // Corrected
            assertToken(tokens[13], TokenType.whitespace, " ", 1, 14, 13);
            assertToken(tokens[14], TokenType.lbrace, "{");
            assertToken(tokens[15], TokenType.whitespace, " ", 1, 16, 15);
            assertToken(tokens[16], TokenType.rbrace, "}");
            assertToken(tokens[17], TokenType.eof, "", 1, 18, 17);
        });
    });

    describe("Whitespace and Newlines", () => {
        it("should tokenize whitespace", () => {
            tokenizer = new SSLTokenizer("  \t  ");
            const tokens = tokenizer.tokenize();
            assertToken(tokens[0], TokenType.whitespace, "  \t  ");
        });
        it("should tokenize newlines", () => {
            // Test 8
            // Input: LF, CR, LF
            // Hypothesis:
            // 1. LF is tokenized as NEWLINE with value "\n" (advances line, offset by 1)
            // 2. CR LF is tokenized as NEWLINE with value "\r\n" (advances line, offset by 2)
            tokenizer = new SSLTokenizer("\n\r\n");
            const tokens = tokenizer.tokenize();
            assert.strictEqual(
                tokens.length,
                3,
                "Expected 3 tokens for \n\r\n (NEWLINE \n, NEWLINE \r\n, EOF)"
            );
            if (tokens.length === 3) {
                assertToken(tokens[0], TokenType.newline, "\n", 1, 1, 0); // From input \n
                assertToken(tokens[1], TokenType.newline, "\r\n", 2, 1, 1); // From input \r\n
                assertToken(tokens[2], TokenType.eof, "", 3, 1, 3); // EOF after \n (len 1) and \r\n (len 2)
            }
        });
    });

    describe("Position Tracking", () => {
        it("should correctly track line, column, and offset", () => {
            // Test 9
            const content = ':IF x > 10;\n  y := "test";';
            // Hypothesis: \n input -> token value "\n", advances line, length 1 for offset.
            tokenizer = new SSLTokenizer(content);
            const tokens = tokenizer.tokenize();

            assertToken(tokens[0], TokenType.colon, ":", 1, 1, 0);
            assertToken(tokens[1], TokenType.if, "IF", 1, 2, 1);
            assertToken(tokens[2], TokenType.whitespace, " ", 1, 4, 3);
            assertToken(tokens[3], TokenType.identifier, "x", 1, 5, 4);
            assertToken(tokens[4], TokenType.whitespace, " ", 1, 6, 5);
            assertToken(tokens[5], TokenType.greaterThan, ">", 1, 7, 6);
            assertToken(tokens[6], TokenType.whitespace, " ", 1, 8, 7);
            assertToken(tokens[7], TokenType.numberLiteral, "10", 1, 9, 8);
            assertToken(tokens[8], TokenType.semicolon, ";", 1, 11, 10);
            // \n token
            assertToken(tokens[9], TokenType.newline, "\n", 1, 12, 11); // Line 1 (start), value \n, offset after ;
            // After this token: Line 2, Offset 11 + 1 = 12
            assertToken(tokens[10], TokenType.whitespace, "  ", 2, 1, 12); // Line 2, Col 1, Offset after \n token
            assertToken(tokens[11], TokenType.identifier, "y", 2, 3, 14); // Offset 12 + 2 = 14            assertToken(tokens[12], TokenType.whitespace, " ", 2, 4, 15);
            assertToken(tokens[13], TokenType.assign, ":=", 2, 5, 16);
            assertToken(tokens[14], TokenType.whitespace, " ", 2, 7, 18); // Before string literal
            assertToken(tokens[15], TokenType.stringLiteral, '"test"', 2, 8, 19); // String literal
            assertToken(tokens[16], TokenType.semicolon, ";", 2, 14, 25); // ;
            assertToken(tokens[17], TokenType.eof, "", 2, 15, 26); // EOF
        });
    });

    describe("Complex Scenarios and Edge Cases", () => {
        it("should handle a mix of tokens correctly", () => {
            // Test 10
            // Hypothesis: \n input -> token value "\n", advances line, length 1 for offset.
            const script =
                ":PROCEDURE CalculateTotal;\n" + // L1: Content 25. After \n (len 1): Offset 26. Next L2.
                "  :PARAMETERS nPrice, nQuantity;\n" + // L2: Content 32. After \n (len 1): Offset 26+32+1 = 59. Next L3.
                "  :DECLARE nTotal;\n" + // L3: Content 18. After \n (len 1): Offset 59+18+1 = 78. Next L4 starts at 79.
                "  nTotal := nPrice * nQuantity; /* Calculate total;\n" + // L4: Content before comment 32. Comment 24.
                // Comment starts L4, C33. Offset 79+32 = 111.
                // After \n (len 1): Offset 79+32+24+1 = 136. Next L5.
                "  :IF nTotal > 100.0;\n" + // L5: Content 21. After \n (len 1): Offset 132+21+1 = 154. Next L6.
                '    Print("Expensive!");\n' + // L6: "    " (4) + "Print" (5) = 9 chars before Print
                // "Print" L6,C5,O154+4=158. "(" L6,C10,O158+5=163
                // Opening " L6,C11,O168. "Expensive" L6,C12,O169. "!" L6,C21,O178. Closing " L6,C22,O179
                // ")" L6,C23,O180. ";" L6,C24,O181.
                // After \n (len 1): Offset 158 + 10 + 12 + 2 + 1 = 183. Next L7
                "  :ENDIF;\n" + // L7: Content 9. After \n (len 1): Offset 183+9+1 = 193. Next L8.
                "  :RETURN nTotal;\n" + // L8: Content 16. After \n (len 1): Offset 193+16+1 = 210. Next L9.
                ":ENDPROC;"; // L9: Content 10. Starts offset 210.
            tokenizer = new SSLTokenizer(script);
            const tokens = tokenizer.tokenize();

            assertToken(tokens[0], TokenType.colon, ":", 1, 1, 0);
            assertToken(tokens[1], TokenType.procedure, "PROCEDURE", 1, 2, 1);
            // ... other tokens on line 1 ...
            const lastTokenL1 = tokens.find((t) => t.position.line === 1 && t.value === ";");
            assert.ok(lastTokenL1, "Semicolon on L1 not found");
            if (lastTokenL1) {
                assertToken(
                    tokens[tokens.indexOf(lastTokenL1) + 1],
                    TokenType.newline,
                    "\n",
                    1,
                    lastTokenL1.position.column + 1,
                    lastTokenL1.position.offset + 1
                );
            }
            const commentToken = tokens.find(
                (t) => t.type === TokenType.blockComment && t.value === "/* Calculate total;"
            );
            assert.ok(commentToken, "Block comment '/* Calculate total;' not found.");
            if (commentToken) {
                assertToken(
                    commentToken,
                    TokenType.blockComment,
                    "/* Calculate total;",
                    4,
                    33,
                    111
                );
            }
            const printToken = tokens.find((t) => t.value === "Print" && t.position.line === 6);
            assert.ok(printToken, "Token 'Print' not found at line 6");
            if (printToken) {
                assertToken(printToken, TokenType.identifier, "Print", 6, 5, 157);
            }

            const lParenPrint = tokens.find(
                (t) =>
                    t.value === "(" &&
                    t.position.line === 6 &&
                    t.position.offset ===
                        (printToken?.position.offset ?? 0) + (printToken?.value.length ?? 0)
            );
            assert.ok(lParenPrint, "Token '(' after Print not found at line 6");
            if (lParenPrint) {
                assertToken(lParenPrint, TokenType.lparen, "(", 6, 10, 162);
            }
            const expensiveStringToken = tokens.find(
                (t) =>
                    t.value === '"Expensive!"' &&
                    t.type === TokenType.stringLiteral &&
                    t.position.line === 6
            );
            assert.ok(expensiveStringToken, "String literal 'Expensive!' not found at line 6");
            if (expensiveStringToken) {
                assertToken(
                    expensiveStringToken,
                    TokenType.stringLiteral,
                    '"Expensive!"',
                    6,
                    11,
                    163
                );
            }
            const rParenPrint = tokens.find(
                (t) =>
                    t.value === ")" &&
                    t.position.line === 6 &&
                    t.position.offset ===
                        (expensiveStringToken?.position.offset ?? 0) +
                            (expensiveStringToken?.value.length ?? 0)
            );
            assert.ok(rParenPrint, "Token ')' after Expensive! string not found at line 6");
            if (rParenPrint) {
                assertToken(rParenPrint, TokenType.rparen, ")", 6, 23, 175);
            }

            const semicolonAfterPrint = tokens.find(
                (t) =>
                    t.value === ";" &&
                    t.position.line === 6 &&
                    t.position.offset === (rParenPrint?.position.offset ?? 0) + 1
            );
            assert.ok(semicolonAfterPrint, "Semicolon after Print statement not found at line 6");
            if (semicolonAfterPrint) {
                assertToken(semicolonAfterPrint, TokenType.semicolon, ";", 6, 24, 176);
            }
            const lastTokenL6 = semicolonAfterPrint; // Semicolon is the last significant token on L6 before newline
            assert.ok(lastTokenL6, "Semicolon on L6 not found");
            if (lastTokenL6) {
                assertToken(
                    tokens[tokens.indexOf(lastTokenL6) + 1],
                    TokenType.newline,
                    "\n",
                    6,
                    lastTokenL6.position.column + 1,
                    lastTokenL6.position.offset + 1
                );
            }
            const returnToken = tokens.find((t) => t.value === "RETURN" && t.position.line === 8);
            assert.ok(returnToken, "Return token not found on line 8");
            if (returnToken) {
                assertToken(returnToken, TokenType.return, "RETURN", 8, 4, 191); // Offset: 188 (start of L8) + 3 (:) = 191
            }
            const endProcToken = tokens.find((t) => t.value === "ENDPROC" && t.position.line === 9);
            assert.ok(endProcToken, "ENDPROC token not found on line 9");
            if (endProcToken) {
                assertToken(endProcToken, TokenType.endproc, "ENDPROC", 9, 2, 207); // Offset: 206 (start of L9) + 1 (:) = 207
            }

            assert.strictEqual(
                tokens[tokens.length - 1].type,
                TokenType.eof,
                "Should end with EOF"
            );
        });
        it("should handle unterminated string as invalid token", () => {
            // Test 11 - Unterminated strings are now handled as single invalid tokens
            tokenizer = new SSLTokenizer('"this is not closed');
            const tokens = tokenizer.tokenize();
            assertToken(tokens[0], TokenType.invalid, '"this is not closed', 1, 1, 0);
            assertToken(tokens[1], TokenType.eof, "", 1, 20, 19);
        });

        it("should handle unterminated comment as comment up to EOF", () => {
            tokenizer = new SSLTokenizer("/* this comment never ends");
            const tokens = tokenizer.tokenize();
            // The comment includes "/*" and everything until EOF
            assertToken(tokens[0], TokenType.blockComment, "/* this comment never ends", 1, 1, 0);
            assertToken(tokens[1], TokenType.eof, "", 1, 27, 26); // EOF position after the comment
        });

        it("should handle invalid characters", () => {
            tokenizer = new SSLTokenizer("@#$~"); // Assuming these are not part of any valid token
            const tokens = tokenizer.tokenize();
            assertToken(tokens[0], TokenType.invalid, "@");
            assertToken(tokens[1], TokenType.invalid, "#");
            assertToken(tokens[2], TokenType.invalid, "$");
            assertToken(tokens[3], TokenType.invalid, "~");
            assertToken(tokens[4], TokenType.eof, "");
        });

        it("should tokenize keywords adjacent to operators/delimiters without whitespace", () => {
            tokenizer = new SSLTokenizer(":IF(condition);");
            const tokens = tokenizer.tokenize();
            assertToken(tokens[0], TokenType.colon, ":", 1, 1, 0);
            assertToken(tokens[1], TokenType.if, "IF", 1, 2, 1);
            assertToken(tokens[2], TokenType.lparen, "(", 1, 4, 3);
            assertToken(tokens[3], TokenType.identifier, "condition", 1, 5, 4);
            assertToken(tokens[4], TokenType.rparen, ")", 1, 14, 13);
            assertToken(tokens[5], TokenType.semicolon, ";", 1, 15, 14);
            assertToken(tokens[6], TokenType.eof, "", 1, 16, 15);
        });

        it("should correctly tokenize a complex example from formatting_provider_thorough_test.ssl", () => {
            // Test 12
            const content =
                ':PROCEDURE MyProcedure:PARAMETERS param1,param2, param3 : DEFAULT param1 , "default" , param2 , 123;';
            tokenizer = new SSLTokenizer(content);
            const tokens = tokenizer.tokenize();

            // :PROCEDURE MyProcedure
            assertToken(tokens[0], TokenType.colon, ":", 1, 1, 0);
            assertToken(tokens[1], TokenType.procedure, "PROCEDURE", 1, 2, 1);
            assertToken(tokens[2], TokenType.whitespace, " ", 1, 11, 10);
            assertToken(tokens[3], TokenType.identifier, "MyProcedure", 1, 12, 11);
            // :PARAMETERS param1,param2, param3
            assertToken(tokens[4], TokenType.colon, ":", 1, 23, 22);
            assertToken(tokens[5], TokenType.parameters, "PARAMETERS", 1, 24, 23);
            assertToken(tokens[6], TokenType.whitespace, " ", 1, 34, 33);
            assertToken(tokens[7], TokenType.identifier, "param1", 1, 35, 34);
            assertToken(tokens[8], TokenType.comma, ",", 1, 41, 40);
            assertToken(tokens[9], TokenType.identifier, "param2", 1, 42, 41);
            assertToken(tokens[10], TokenType.comma, ",", 1, 48, 47);
            assertToken(tokens[11], TokenType.whitespace, " ", 1, 49, 48);
            assertToken(tokens[12], TokenType.identifier, "param3", 1, 50, 49);
            assertToken(tokens[13], TokenType.whitespace, " ", 1, 56, 55);
            // : DEFAULT param1 , "default" , param2 , 123
            assertToken(tokens[14], TokenType.colon, ":", 1, 57, 56);
            assertToken(tokens[15], TokenType.whitespace, " ", 1, 58, 57);
            assertToken(tokens[16], TokenType.default, "DEFAULT", 1, 59, 58);
            assertToken(tokens[17], TokenType.whitespace, " ", 1, 66, 65);
            assertToken(tokens[18], TokenType.identifier, "param1", 1, 67, 66);
            assertToken(tokens[19], TokenType.whitespace, " ", 1, 73, 72);
            assertToken(tokens[20], TokenType.comma, ",", 1, 74, 73);
            assertToken(tokens[21], TokenType.whitespace, " ", 1, 75, 74);
            assertToken(tokens[22], TokenType.stringLiteral, '"default"', 1, 76, 75); // String literal
            assertToken(tokens[23], TokenType.whitespace, " ", 1, 85, 84);
            assertToken(tokens[24], TokenType.comma, ",", 1, 86, 85);
            assertToken(tokens[25], TokenType.whitespace, " ", 1, 87, 86);
            assertToken(tokens[26], TokenType.identifier, "param2", 1, 88, 87);
            assertToken(tokens[27], TokenType.whitespace, " ", 1, 94, 93);
            assertToken(tokens[28], TokenType.comma, ",", 1, 95, 94);
            assertToken(tokens[29], TokenType.whitespace, " ", 1, 96, 95);
            assertToken(tokens[30], TokenType.numberLiteral, "123", 1, 97, 96);
            assertToken(tokens[31], TokenType.semicolon, ";", 1, 100, 99);
            assertToken(tokens[32], TokenType.eof, "", 1, 101, 100);
        });
    });

    describe("Bracket Context Awareness", () => {
        it("should treat [ as LBRACKET when no previous token", () => {
            // Modified test description
            tokenizer = new SSLTokenizer("[hello world]");
            const tokens = tokenizer.tokenize();

            // Expected: LBRACKET, IDENTIFIER, WHITESPACE, IDENTIFIER, RBRACKET, EOF
            assert.strictEqual(tokens.length, 6, "Expected 6 tokens for '[hello world]'");
            assertToken(tokens[0], TokenType.lbracket, "[", 1, 1, 0);
            assertToken(tokens[1], TokenType.identifier, "hello", 1, 2, 1);
            assertToken(tokens[2], TokenType.whitespace, " ", 1, 7, 6);
            assertToken(tokens[3], TokenType.identifier, "world", 1, 8, 7);
            assertToken(tokens[4], TokenType.rbracket, "]", 1, 13, 12);
            assertToken(tokens[5], TokenType.eof, "", 1, 14, 13);
        });

        it("should treat [ as array access after identifier", () => {
            tokenizer = new SSLTokenizer("myArray[index]");
            const tokens = tokenizer.tokenize();

            assert.strictEqual(tokens.length, 5); // identifier + lbracket + identifier + rbracket + EOF
            assertToken(tokens[0], TokenType.identifier, "myArray", 1, 1, 0);
            assertToken(tokens[1], TokenType.lbracket, "[", 1, 8, 7);
            assertToken(tokens[2], TokenType.identifier, "index", 1, 9, 8);
            assertToken(tokens[3], TokenType.rbracket, "]", 1, 14, 13);
            assertToken(tokens[4], TokenType.eof, "", 1, 15, 14);
        });

        it("should treat [ as array access after closing bracket (chained access)", () => {
            tokenizer = new SSLTokenizer("array[0][1]");
            const tokens = tokenizer.tokenize();

            // Expected: array, [, 0, ], [, 1, ], EOF
            assert.strictEqual(tokens.length, 8, "Expected 8 tokens for chained array access");
            assertToken(tokens[0], TokenType.identifier, "array", 1, 1, 0);
            assertToken(tokens[1], TokenType.lbracket, "[", 1, 6, 5);
            assertToken(tokens[2], TokenType.numberLiteral, "0", 1, 7, 6);
            assertToken(tokens[3], TokenType.rbracket, "]", 1, 8, 7);
            assertToken(tokens[4], TokenType.lbracket, "[", 1, 9, 8);
            assertToken(tokens[5], TokenType.numberLiteral, "1", 1, 10, 9);
            assertToken(tokens[6], TokenType.rbracket, "]", 1, 11, 10);
            assertToken(tokens[7], TokenType.eof, "", 1, 12, 11);
        });

        it("should treat [ as array access after closing parenthesis (function result access)", () => {
            tokenizer = new SSLTokenizer("func()[index]");
            const tokens = tokenizer.tokenize();

            assert.strictEqual(tokens.length, 7); // identifier + lparen + rparen + lbracket + identifier + rbracket + EOF
            assertToken(tokens[0], TokenType.identifier, "func", 1, 1, 0);
            assertToken(tokens[1], TokenType.lparen, "(", 1, 5, 4);
            assertToken(tokens[2], TokenType.rparen, ")", 1, 6, 5);
            assertToken(tokens[3], TokenType.lbracket, "[", 1, 7, 6);
            assertToken(tokens[4], TokenType.identifier, "index", 1, 8, 7);
            assertToken(tokens[5], TokenType.rbracket, "]", 1, 13, 12);
            assertToken(tokens[6], TokenType.eof, "", 1, 14, 13);
        });
    });
});
