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
        it("should tokenize SQL parameters", () => {
            // NOTE: This test currently documents a limitation in our tokenizer.
            // According to the SSL EBNF grammar, SQL parameters like ?userId? within SQL strings
            // should be tokenized as separate SQL_PARAM_NAMED tokens. However, implementing this
            // requires complex context-aware string parsing which is deferred for now.
            // See: SqlParameter ::= "?" Identifier "?" | "?" in ssl-ebnf-grammar-complete.md

            // Test case 1: SQL parameters outside of strings (currently supported)
            const standaloneParam = "?userId?";
            const standaloneTokens = tokenize(standaloneParam);
            const namedParamToken = standaloneTokens.find(
                (t) => t.type === TokenType.SQL_PARAM_NAMED
            );
            expect(namedParamToken?.value).toBe("?userId?");

            // Test case 2: SQL parameters within SqlExecute strings (current limitation)
            // TODO: Implement context-aware SQL parameter extraction within string literals
            const input = 'aResults := SqlExecute("SELECT * FROM users WHERE id = ?userId?");';
            const tokens = tokenize(input);

            // The SQL string is now correctly tokenized as a SQL_STRING token due to SqlExecute context
            const sqlStringToken = tokens.find((t) => t.type === TokenType.SQL_STRING);
            expect(sqlStringToken?.value).toBe('"SELECT * FROM users WHERE id = ?userId?"'); // Future enhancement: Extract ?userId? as a separate SQL_PARAM_NAMED token
            // This would require implementing a specialized SQL string parser
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
    });
});
