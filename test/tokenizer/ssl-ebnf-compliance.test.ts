import { tokenize, TokenType } from "../../src/tokenizer";

describe("SSL EBNF Grammar Compliance", () => {
    describe("Code Block Constructs", () => {
        it("should tokenize code block delimiters", () => {
            const input = "{| expression |}";
            const tokens = tokenize(input);

            // Find specific tokens
            const codeBlockStart = tokens.find((t) => t.type === TokenType.CODE_BLOCK_START);
            const codeBlockEnd = tokens.find((t) => t.type === TokenType.CODE_BLOCK_END);

            expect(codeBlockStart).toBeDefined();
            expect(codeBlockStart?.value).toBe("{|");
            expect(codeBlockEnd).toBeDefined();
            expect(codeBlockEnd?.value).toBe("|}");
        });
    });

    describe("SQL Parameter Tokens", () => {
        it("should recognize named SQL parameters", () => {
            const input = "?paramName?";
            const tokens = tokenize(input);

            const namedParam = tokens.find((t) => t.type === TokenType.SQL_PARAM_NAMED);
            expect(namedParam).toBeDefined();
            expect(namedParam?.value).toBe("?paramName?");
        });

        it("should recognize unnamed SQL parameters", () => {
            const input = "WHERE id = ?";
            const tokens = tokenize(input);

            const unnamedParam = tokens.find((t) => t.type === TokenType.SQL_PARAM_UNNAMED);
            expect(unnamedParam).toBeDefined();
            expect(unnamedParam?.value).toBe("?");
        });
    });

    describe("Bitwise Function Identifiers", () => {
        it("should tokenize bitwise function names", () => {
            const input = "_AND _OR _XOR _NOT";
            const tokens = tokenize(input);

            const identifiers = tokens.filter((t) => t.type === TokenType.IDENTIFIER);
            expect(identifiers).toHaveLength(4);
            expect(identifiers[0].value).toBe("_AND");
            expect(identifiers[1].value).toBe("_OR");
            expect(identifiers[2].value).toBe("_XOR");
            expect(identifiers[3].value).toBe("_NOT");
        });
    });
    describe("Scientific Notation Support", () => {
        it("should handle valid scientific notation per EBNF spec", () => {
            const input = "1.5e10 2.3E-5 0.5e1";
            const tokens = tokenize(input);

            const numbers = tokens.filter((t) => t.type === TokenType.NUMBER);
            expect(numbers).toHaveLength(3);
            expect(numbers[0].value).toBe("1.5e10");
            expect(numbers[1].value).toBe("2.3E-5");
            expect(numbers[2].value).toBe("0.5e1");
        });

        it("should reject invalid scientific notation formats per EBNF spec", () => {
            // Per EBNF note 14: explicit plus signs, missing decimal points, and leading decimals are not supported
            const invalidFormats = [
                "9E+1", // explicit plus sign
                "7e2", // missing decimal point before 'e'
                ".5e1", // leading decimal point without zero
            ];

            invalidFormats.forEach((format) => {
                const tokens = tokenize(format);
                const numbers = tokens.filter((t) => t.type === TokenType.NUMBER);

                // Should NOT be tokenized as a single scientific notation number
                // Instead should be multiple tokens or cause tokenization issues
                expect(numbers.length === 0 || numbers[0].value !== format).toBe(true);
            });
        });
    });

    describe("Boolean and Logical Operator Support", () => {
        it("should tokenize SSL boolean literals", () => {
            const input = ".T. .F.";
            const tokens = tokenize(input);

            const booleans = tokens.filter((t) => t.type === TokenType.BOOLEAN);
            expect(booleans).toHaveLength(2);
            expect(booleans[0].value).toBe(".T.");
            expect(booleans[0].parsedValue).toBe(true);
            expect(booleans[1].value).toBe(".F.");
            expect(booleans[1].parsedValue).toBe(false);
        });

        it("should tokenize logical operators with dots", () => {
            const input = ".AND. .OR. .NOT.";
            const tokens = tokenize(input);

            const logicalTokens = tokens.filter(
                (t) =>
                    t.type === TokenType.AND || t.type === TokenType.OR || t.type === TokenType.NOT
            );

            expect(logicalTokens).toHaveLength(3);
            expect(logicalTokens[0].value).toBe(".AND.");
            expect(logicalTokens[1].value).toBe(".OR.");
            expect(logicalTokens[2].value).toBe(".NOT.");
        });
    });

    describe("SSL Comment Syntax", () => {
        it("should handle SSL single-line comments", () => {
            const input = "/* This is a comment ;";
            const tokens = tokenize(input);

            expect(tokens[0].type).toBe(TokenType.SINGLE_LINE_COMMENT);
            expect(tokens[0].value).toBe("/* This is a comment ;");
        });

        it("should handle SSL multi-line block comments", () => {
            const input = "/* This is a\nmulti-line comment ;";
            const tokens = tokenize(input);

            expect(tokens[0].type).toBe(TokenType.BLOCK_COMMENT);
            expect(tokens[0].value).toBe("/* This is a\nmulti-line comment ;");
        });

        it("should handle region comments", () => {
            const input = "/* region: TestRegion ;";
            const tokens = tokenize(input);

            expect(tokens[0].type).toBe(TokenType.REGION_COMMENT);
            expect(tokens[0].value).toBe("/* region: TestRegion ;");
        });
    });

    describe("Date Literal Components", () => {
        it("should tokenize date components as separate tokens", () => {
            const input = "{2024, 12, 25}";
            const tokens = tokenize(input);

            // According to EBNF, date literals are tokenized as individual components
            expect(tokens[0].type).toBe(TokenType.LBRACE);
            expect(tokens[1].type).toBe(TokenType.NUMBER);
            expect(tokens[1].value).toBe("2024");
            expect(tokens[2].type).toBe(TokenType.COMMA);
            expect(tokens[3].type).toBe(TokenType.NUMBER);
            expect(tokens[3].value).toBe("12");
            expect(tokens[4].type).toBe(TokenType.COMMA);
            expect(tokens[5].type).toBe(TokenType.NUMBER);
            expect(tokens[5].value).toBe("25");
            expect(tokens[6].type).toBe(TokenType.RBRACE);
        });
    });

    describe("Complex SSL Expression", () => {
        it("should handle procedure with code blocks correctly", () => {
            const input = ":procedure test; {| x | x + 1 |} :endproc;";
            const tokens = tokenize(input);

            // Check key token types are present
            const procedureToken = tokens.find((t) => t.type === TokenType.PROCEDURE);
            const codeBlockStart = tokens.find((t) => t.type === TokenType.CODE_BLOCK_START);
            const codeBlockEnd = tokens.find((t) => t.type === TokenType.CODE_BLOCK_END);
            const endprocToken = tokens.find((t) => t.type === TokenType.ENDPROC);

            expect(procedureToken).toBeDefined();
            expect(codeBlockStart).toBeDefined();
            expect(codeBlockEnd).toBeDefined();
            expect(endprocToken).toBeDefined();
        });
    });
});
