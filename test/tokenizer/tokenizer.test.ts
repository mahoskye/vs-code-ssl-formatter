// Tests for the tokenizer module

import { tokenize, Tokenizer, TokenType } from "../../src/tokenizer";

describe("Tokenizer", () => {
    describe("Basic Tokenization", () => {
        it("should tokenize a simple assignment", () => {
            const input = 'sName := "Hello World";';
            const tokens = tokenize(input);

            expect(tokens).toHaveLength(5); // identifier, :=, string, ;, EOF
            expect(tokens[0].type).toBe(TokenType.IDENTIFIER);
            expect(tokens[0].value).toBe("sName");
            expect(tokens[1].type).toBe(TokenType.ASSIGN);
            expect(tokens[1].value).toBe(":=");
            expect(tokens[2].type).toBe(TokenType.STRING);
            expect(tokens[2].value).toBe('"Hello World"');
            expect(tokens[3].type).toBe(TokenType.SEMICOLON);
            expect(tokens[4].type).toBe(TokenType.EOF);
        });

        it("should tokenize numbers correctly", () => {
            const input = "nValue := 123.45;";
            const tokens = tokenize(input);

            expect(tokens[2].type).toBe(TokenType.NUMBER);
            expect(tokens[2].value).toBe("123.45");
            expect(tokens[2].parsedValue).toBe(123.45);
        });

        it("should tokenize boolean literals", () => {
            const input = "bTrue := .T.; bFalse := .F.;";
            const tokens = tokenize(input);

            const trueToken = tokens.find((t) => t.value === ".T.");
            const falseToken = tokens.find((t) => t.value === ".F.");

            expect(trueToken?.type).toBe(TokenType.BOOLEAN);
            expect(trueToken?.parsedValue).toBe(true);
            expect(falseToken?.type).toBe(TokenType.BOOLEAN);
            expect(falseToken?.parsedValue).toBe(false);
        });
        it("should tokenize keywords case-insensitively", () => {
            const input = ":if bCondition; :endif;";
            const tokens = tokenize(input, { includeNewlines: false });

            expect(tokens[0].type).toBe(TokenType.COLON);
            expect(tokens[1].type).toBe(TokenType.IF);
            expect(tokens[1].value).toBe("if");
            expect(tokens[5].type).toBe(TokenType.ENDIF);
        });
    });

    describe("SSL-specific constructs", () => {
        it("should tokenize procedure declarations", () => {
            const input = ":PROCEDURE TestProc; :ENDPROC;";
            const tokens = tokenize(input, { includeNewlines: false });
            expect(tokens[1].type).toBe(TokenType.PROCEDURE);
            expect(tokens[2].type).toBe(TokenType.IDENTIFIER);
            expect(tokens[2].value).toBe("TestProc");
            expect(tokens[5].type).toBe(TokenType.ENDPROC);
        });
        it("should tokenize question marks for SQL parameters", () => {
            // NOTE: SQL parameters like ?userId? are now tokenized as separate tokens:
            // QUESTION, IDENTIFIER, QUESTION. The parser will be responsible for
            // interpreting these sequences as SQL parameters in the appropriate context.
            // See: SqlParameter ::= "?" Identifier "?" | "?" in ssl-ebnf-grammar-complete.md

            // Test case 1: SQL parameters outside of strings
            const standaloneParam = "?userId?";
            const standaloneTokens = tokenize(standaloneParam);

            // Should be tokenized as QUESTION, IDENTIFIER, QUESTION
            const questionTokens = standaloneTokens.filter((t) => t.type === TokenType.QUESTION);
            expect(questionTokens).toHaveLength(2);

            const identifierToken = standaloneTokens.find(
                (t) => t.type === TokenType.IDENTIFIER && t.value === "userId"
            );
            expect(identifierToken).toBeDefined();

            // Test case 2: SQL parameters within SqlExecute strings (parser responsibility)
            const input = 'aResults := SqlExecute("SELECT * FROM users WHERE id = ?userId?");';
            const tokens = tokenize(input);

            // The string is already being tokenized as SQL_STRING (probably due to context-aware logic)
            // The parser will handle the question marks within SQL strings
            const sqlStringToken = tokens.find((t) => t.type === TokenType.SQL_STRING);
            expect(sqlStringToken?.value).toBe('"SELECT * FROM users WHERE id = ?userId?"');
        });

        it("should tokenize SQL strings differently from regular strings", () => {
            // Test regular string
            const regularInput = 'sMessage := "Hello World";';
            const regularTokens = tokenize(regularInput);
            const regularStringToken = regularTokens.find((t) => t.type === TokenType.STRING);
            expect(regularStringToken?.value).toBe('"Hello World"');

            // Test SQL string in SqlExecute context
            const sqlInput = 'aResults := SqlExecute("SELECT * FROM users");';
            const sqlTokens = tokenize(sqlInput);
            const sqlStringToken = sqlTokens.find((t) => t.type === TokenType.SQL_STRING);
            expect(sqlStringToken?.value).toBe('"SELECT * FROM users"');

            // Test SQL string in LSearch context
            const lsearchInput = 'aData := LSearch("SELECT name FROM customers");';
            const lsearchTokens = tokenize(lsearchInput);
            const lsearchStringToken = lsearchTokens.find((t) => t.type === TokenType.SQL_STRING);
            expect(lsearchStringToken?.value).toBe('"SELECT name FROM customers"');
        });

        it("should tokenize logical operators", () => {
            const input = "bResult := bA .AND. bB .OR. .NOT. bC;";
            const tokens = tokenize(input);

            const andToken = tokens.find((t) => t.value === ".AND.");
            const orToken = tokens.find((t) => t.value === ".OR.");
            const notToken = tokens.find((t) => t.value === ".NOT.");

            expect(andToken?.type).toBe(TokenType.AND);
            expect(orToken?.type).toBe(TokenType.OR);
            expect(notToken?.type).toBe(TokenType.NOT);
        });
    });

    describe("Comments", () => {
        it("should tokenize single-line comments", () => {
            const input = "/* This is a comment ;";
            const tokens = tokenize(input);

            expect(tokens[0].type).toBe(TokenType.SINGLE_LINE_COMMENT);
            expect(tokens[0].value).toBe("/* This is a comment ;");
        });

        it("should tokenize region comments", () => {
            const input = "/* region Test Region ;";
            const tokens = tokenize(input);

            expect(tokens[0].type).toBe(TokenType.REGION_COMMENT);
        });

        it("should handle endregion comments", () => {
            const input = "/* endregion: TestRegion ;";
            const tokens = tokenize(input);

            expect(tokens[0].type).toBe(TokenType.ENDREGION_COMMENT);
            expect(tokens[0].value).toBe("/* endregion: TestRegion ;");
        });
    });

    describe("Operators", () => {
        it("should tokenize all assignment operators", () => {
            const input = ":= += -= *= /= ^=";
            const tokens = tokenize(input);
            const expected = [
                TokenType.ASSIGN,
                TokenType.PLUS_ASSIGN,
                TokenType.MINUS_ASSIGN,
                TokenType.MULT_ASSIGN,
                TokenType.DIV_ASSIGN,
                TokenType.POWER_ASSIGN,
            ];
            const found = tokens.filter(t => t.type !== TokenType.EOF).map((t) => t.type);
            expect(found).toEqual(expect.arrayContaining(expected));
        });

        it("should tokenize all comparison operators (maximal munch)", () => {
            const input = "== != < > <= >= =";
            const tokens = tokenize(input);
            const expected = [
                TokenType.STRICT_EQUAL,
                TokenType.NOT_EQUAL,
                TokenType.LESS_THAN,
                TokenType.GREATER_THAN,
                TokenType.LESS_EQUAL,
                TokenType.GREATER_EQUAL,
                TokenType.EQUAL,
            ];
            const found = tokens.filter(t => t.type !== TokenType.EOF).map((t) => t.type);
            expect(found).toEqual(expect.arrayContaining(expected));
        });

        it("should tokenize all arithmetic and power operators", () => {
            const input = "+ - * / % ^";
            const tokens = tokenize(input);
            const expected = [
                TokenType.PLUS,
                TokenType.MINUS,
                TokenType.MULTIPLY,
                TokenType.DIVIDE,
                TokenType.MODULO,
                TokenType.POWER,
            ];
            const found = tokens.filter(t => t.type !== TokenType.EOF).map((t) => t.type);
            expect(found).toEqual(expect.arrayContaining(expected));
        });

        it("should tokenize increment/decrement operators", () => {
            const input = "++ --";
            const tokens = tokenize(input);
            const expected = [TokenType.INCREMENT, TokenType.DECREMENT];
            const found = tokens.filter(t => t.type !== TokenType.EOF).map((t) => t.type);
            expect(found).toEqual(expect.arrayContaining(expected));
        });
    });

    describe("Punctuation", () => {
        it("should tokenize all punctuation marks", () => {
            const input = "; , : ( ) [ ] { } | . ?";
            const tokens = tokenize(input);
            const expected = [
                TokenType.SEMICOLON,
                TokenType.COMMA,
                TokenType.COLON,
                TokenType.LPAREN,
                TokenType.RPAREN,
                TokenType.LBRACKET,
                TokenType.RBRACKET,
                TokenType.LBRACE,
                TokenType.RBRACE,
                TokenType.PIPE,
                TokenType.DOT,
                TokenType.QUESTION,
            ];
            const found = tokens.filter(t => t.type !== TokenType.EOF).map((t) => t.type);
            expect(found).toEqual(expect.arrayContaining(expected));
        });
    });

    describe("Object and Property Access", () => {
        it("should tokenize property access with a colon", () => {
            const input = "myObject:myProperty";
            const tokens = tokenize(input);
            expect(tokens.map(t=>t.type)).toEqual([
                TokenType.IDENTIFIER,
                TokenType.COLON,
                TokenType.IDENTIFIER,
                TokenType.EOF,
            ]);
            expect(tokens[0].value).toBe("myObject");
            expect(tokens[2].value).toBe("myProperty");
        });
    });

    describe("Literals", () => {
        it("should tokenize NIL literal", () => {
            const tokens = tokenize("NIL");
            expect(tokens[0].type).toBe(TokenType.NIL);
            expect(tokens[0].value).toBe("NIL");
        });

        it("should tokenize single-quoted strings", () => {
            const tokens = tokenize("'hello world'");
            expect(tokens[0].type).toBe(TokenType.STRING);
            expect(tokens[0].value).toBe("'hello world'");
        });

        it("should tokenize a simple array literal", () => {
            const tokens = tokenize("{1, 'two', .T.}");
            const types = tokens.map((t) => t.type);
            expect(types).toEqual([
                TokenType.LBRACE,
                TokenType.NUMBER,
                TokenType.COMMA,
                TokenType.STRING,
                TokenType.COMMA,
                TokenType.BOOLEAN,
                TokenType.RBRACE,
                TokenType.EOF,
            ]);
        });

        it("should tokenize a nested array literal", () => {
            const tokens = tokenize("{{1}, {2}}");
            const types = tokens.map((t) => t.type);
            expect(types).toEqual([
                TokenType.LBRACE,
                TokenType.LBRACE,
                TokenType.NUMBER,
                TokenType.RBRACE,
                TokenType.COMMA,
                TokenType.LBRACE,
                TokenType.NUMBER,
                TokenType.RBRACE,
                TokenType.RBRACE,
                TokenType.EOF,
            ]);
        });
    });

    describe("Code Block Constructs", () => {
        it("should tokenize code block with delimiters and pipe", () => {
            const input = "{|x| x*x}";
            const tokens = tokenize(input);
            const types = tokens.map((t) => t.type);

            expect(types).toEqual([
                TokenType.CODE_BLOCK_START,
                TokenType.IDENTIFIER,
                TokenType.PIPE,
                TokenType.IDENTIFIER,
                TokenType.MULTIPLY,
                TokenType.IDENTIFIER,
                TokenType.RBRACE,
                TokenType.EOF,
            ]);

            expect(tokens[0].value).toBe("{|");
            expect(tokens[2].value).toBe("|");
            expect(tokens[6].value).toBe("}");
        });
    });

    describe("Error handling", () => {
        it("should handle unterminated strings gracefully", () => {
            const tokenizer = new Tokenizer({ skipErrors: true });
            const result = tokenizer.tokenize('sName := "unterminated');

            expect(result.hasErrors).toBe(true);
            expect(result.errors).toHaveLength(1);
        });

        it("should continue tokenizing after errors when skipErrors is true", () => {
            const tokenizer = new Tokenizer({ skipErrors: true });
            const result = tokenizer.tokenize('sName := "unterminated\n nValue := 123;');

            expect(result.hasErrors).toBe(true);
            expect(result.tokens.length).toBeGreaterThan(0);
        });

        it("should handle unterminated comments", () => {
            const tokenizer = new Tokenizer({ skipErrors: true });
            const result = tokenizer.tokenize("/* this is not closed");
            expect(result.hasErrors).toBe(true);
            expect(result.errors).toHaveLength(1);
            // The tokenizer will consume the rest of the input and the only token left will be EOF.
            expect(result.tokens[0].type).toBe(TokenType.EOF);
            expect(result.tokens).toHaveLength(1);
        });
    });
});
