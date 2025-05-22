/**
 * SSL Parser and Diagnostic Tests
 * Validates the SSL parser against the EBNF grammar
 */

import * as assert from "assert";
import * as path from "path";
import * as fs from "fs";
import { SSLParser } from "../src/parser/sslParser";
import { SSLTokenizer } from "../src/parser/tokenizer";

suite("SSL Parser Test Suite", () => {
    test("Basic Tokenization", () => {
        const code = `:PROCEDURE MyProc;
:PARAMETERS param1, param2;
:DECLARE result;
result := param1 + param2;
:RETURN result;
:ENDPROC;`;

        const tokenizer = new SSLTokenizer(code);
        const tokens = tokenizer.tokenize().filter((t) => t.type !== 5); // Filter out whitespace

        // Check number of tokens
        assert.strictEqual(tokens.length > 0, true);

        // Check specific tokens
        assert.strictEqual(tokens[0].value, ":PROCEDURE");
        assert.strictEqual(tokens[1].value, "MyProc");
        assert.strictEqual(tokens[2].value, ";");
    });

    test("Comment Tokenization", () => {
        const code = `/* This is a comment; 
/* This is another comment;
:PROCEDURE Test;
/* Comment inside procedure;
:ENDPROC;`;

        const tokenizer = new SSLTokenizer(code);
        const tokens = tokenizer.tokenize().filter((t) => t.type !== 5); // Filter out whitespace

        // Check that comments are correctly identified
        assert.strictEqual(tokens[0].value.startsWith("/*"), true);
        assert.strictEqual(tokens[0].value.endsWith(";"), true);
        assert.strictEqual(tokens[1].value.startsWith("/*"), true);
    });

    test("Basic Parsing", () => {
        const code = `:PROCEDURE MyProc;
:PARAMETERS param1, param2;
:DECLARE result;
result := param1 + param2;
:RETURN result;
:ENDPROC;`;

        const parser = new SSLParser();
        const { ast, errors } = parser.parse(code);

        // Check that parsing completed without errors
        assert.strictEqual(errors.length, 0);

        // Check AST structure
        assert.strictEqual(ast.type, "Program");
        assert.strictEqual(ast.body.length > 0, true);
        assert.strictEqual(ast.body[0].type, "ProcedureStatement");
    });

    test("Error Detection", () => {
        const code = `:PROCEDURE MyProc;
:PARAMETERS param1, param2
:DECLARE result;
result := param1 + param2;
:RETURN result;
:ENDPROC;`;

        const parser = new SSLParser();
        const { ast, errors } = parser.parse(code);

        // Check that the missing semicolon error was detected
        assert.strictEqual(errors.length > 0, true);
    });

    test("Complex Control Flow", () => {
        const code = `:PROCEDURE ComplexFlow;
:PARAMETERS value;
:DECLARE result;

:IF value > 10;
    result := "Large";
:ELSE;
    :IF value > 5;
        result := "Medium";
    :ELSE;
        result := "Small";
    :ENDIF;
:ENDIF;

:BEGINCASE;
:CASE value = 1;
    result := "One";
:CASE value = 2;
    result := "Two";
:OTHERWISE;
    result := "Other";
:ENDCASE;

:WHILE value > 0;
    value -= 1;
:ENDWHILE;

:FOR i := 1 :TO 10;
    result := result + i;
:NEXT;

:RETURN result;
:ENDPROC;`;

        const parser = new SSLParser();
        const { ast, errors } = parser.parse(code);

        // Check that complex control flow was parsed correctly
        assert.strictEqual(errors.length, 0);
    });

    test("Class Definition", () => {
        const code = `:CLASS MyClass;
:INHERIT ParentClass;
:DECLARE field1, field2;

:PROCEDURE Method1;
:PARAMETERS param;
:RETURN param + 1;
:ENDPROC;
`;

        const parser = new SSLParser();
        const { ast, errors } = parser.parse(code);

        // Check that class definition was parsed correctly
        assert.strictEqual(errors.length, 0);
        assert.strictEqual(ast.isClassDefinition, true);
    });

    test("Error Handling", () => {
        const code = `:PROCEDURE ErrorTest;
:TRY;
    DoSomething();
:CATCH;
    /* Handle error;
:FINALLY;
    /* Clean up;
:ENDTRY;
:ENDPROC;`;

        const parser = new SSLParser();
        const { ast, errors } = parser.parse(code);

        // Check that error handling was parsed correctly
        assert.strictEqual(errors.length, 0);
    });

    test("Array Access", () => {
        const code = `:PROCEDURE ArrayTest;
:DECLARE arr, result;
arr := {1, 2, 3, 4, 5};
result := arr[1];  /* 1-based indexing */
result := arr[1, 2];  /* Multi-dimensional */
result := arr[1][2];  /* Chained access */
:ENDPROC;`;

        const parser = new SSLParser();
        const { ast, errors } = parser.parse(code);

        // Check that array access was parsed correctly
        assert.strictEqual(errors.length, 0);
    });

    test("Property Access", () => {
        const code = `:PROCEDURE PropertyTest;
:DECLARE obj, result;
obj := CreateUDObject();
result := obj:property;
result := obj:method();
:ENDPROC;`;

        const parser = new SSLParser();
        const { ast, errors } = parser.parse(code);

        // Check that property access was parsed correctly
        assert.strictEqual(errors.length, 0);
    });

    test("SQL Integration", () => {
        const code = `:PROCEDURE SQLTest;
:DECLARE results;
results := SqlExecute("SELECT * FROM table WHERE field = ?param?", {value});
results := LSearch("SELECT * FROM table WHERE field = ?", value);
:ENDPROC;`;

        const parser = new SSLParser();
        const { ast, errors } = parser.parse(code);

        // Check that SQL integration was parsed correctly
        assert.strictEqual(errors.length, 0);
    });

    test("Validate Against EBNF Grammar - Comments", () => {
        // Test special comment syntax (starts with /* ends with ;)
        const code = `/* This is a regular comment;
/* region Comment Section;
:PROCEDURE Test;
/* This is a comment inside a procedure;
:ENDPROC;
/* endregion;`;

        const parser = new SSLParser();
        const { ast, errors } = parser.parse(code);

        // Check that comment syntax is correctly handled
        assert.strictEqual(errors.length, 0);
    });

    test("Validate Against EBNF Grammar - String Literals", () => {
        // Test various string literal formats
        const code = `:PROCEDURE StringTest;
:DECLARE s1, s2, s3;
s1 := "Double quoted string";
s2 := 'Single quoted string';
s3 := [Bracket enclosed string];
:ENDPROC;`;

        const parser = new SSLParser();
        const { ast, errors } = parser.parse(code);

        // Check that string literal formats are correctly handled
        assert.strictEqual(errors.length, 0);
    });

    test("Validate Against EBNF Grammar - Boolean Literals", () => {
        // Test boolean literal formats
        const code = `:PROCEDURE BooleanTest;
:DECLARE b1, b2;
b1 := .T.;
b2 := .F.;
:ENDPROC;`;

        const parser = new SSLParser();
        const { ast, errors } = parser.parse(code);

        // Check that boolean literals are correctly handled
        assert.strictEqual(errors.length, 0);
    });

    test("Validate Against EBNF Grammar - Number Literals", () => {
        // Test number literal formats including scientific notation
        const code = `:PROCEDURE NumberTest;
:DECLARE n1, n2, n3, n4;
n1 := 123;
n2 := 123.456;
n3 := 1.23e2;
n4 := 4.56E-3;
:ENDPROC;`;

        const parser = new SSLParser();
        const { ast, errors } = parser.parse(code);

        // Check that number literals are correctly handled
        assert.strictEqual(errors.length, 0);
    });

    test("Validate Against EBNF Grammar - Logical Operators", () => {
        // Test logical operators
        const code = `:PROCEDURE LogicalTest;
:DECLARE result;
result := a .AND. b;
result := a .OR. b;
result := .NOT. a;
:ENDPROC;`;

        const parser = new SSLParser();
        const { ast, errors } = parser.parse(code);

        // Check that logical operators are correctly handled
        assert.strictEqual(errors.length, 0);
    });

    test("Validate Against EBNF Grammar - Assignment Operators", () => {
        // Test assignment operators
        const code = `:PROCEDURE AssignmentTest;
:DECLARE a;
a := 1;
a += 1;
a -= 1;
a *= 2;
a /= 2;
a ^= 2;
:ENDPROC;`;

        const parser = new SSLParser();
        const { ast, errors } = parser.parse(code);

        // Check that assignment operators are correctly handled
        assert.strictEqual(errors.length, 0);
    });

    test("Real-world Code Sample", () => {
        // Test with more complex real-world code
        const code = `:PROCEDURE ProcessSample;
:PARAMETERS data, options;
:DECLARE result, i, found, temp;

/* Initialize variables;
found := .F.;
result := "";
temp := CreateUDObject();

/* Process the data;
:IF Len(data) > 0;
    :WHILE (i += 1) <= Len(data);
        :TRY;
            /* Retrieve the item;
            temp:item := data[i];
            
            /* Apply processing;
            :IF temp:item:value > options:threshold;
                result := result + temp:item:name;
                found := .T.;
                :EXITWHILE;
            :ENDIF;
        :CATCH;
            /* Log error and continue;
            result := result + "Error";
        :ENDTRY;
    :ENDWHILE;
:ELSE;
    result := "No data";
:ENDIF;

:RETURN result;
:ENDPROC;`;

        const parser = new SSLParser();
        const { ast, errors } = parser.parse(code);

        // Check that real-world code is correctly parsed
        assert.strictEqual(errors.length, 0);
    });
});
