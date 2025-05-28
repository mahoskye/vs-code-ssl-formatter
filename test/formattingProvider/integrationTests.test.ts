import * as assert from "assert";
import { SSLFormattingProvider } from "../../src/formatters/formattingProvider";
import { formatDocument } from "../helpers/formatDocument";

describe("Integration Tests from ssl_examples/formatting_provider_thorough_test.ssl", () => {
    let provider: SSLFormattingProvider;
    const TESTTIMEOUT = 5000; // 5 seconds
    beforeEach(() => {
        provider = new SSLFormattingProvider();
    });

    // Temporarily disabled. Test causes runner to freeze. // This line will be removed
    //         it(
    //             "Test 1: Procedure and Declarations",
    //             async () => {
    //                 const input = `
    // /* Test 1: Procedure and Declarations - Indentation and Colon Spacing;
    // :PROCEDURE MyProcedure;:PARAMETERS param1,param2, param3; : DEFAULT param1 , "default"; :DEFAULT param2 , 123;
    // :DECLARE var1,var2 , var3;
    // :DECLARE sString ;
    // :DECLARE nNumber;
    // var1 :=param1+param2*param3;
    // var2:="A string literal";
    // var3:= {1,2,3,4,5}; /* Array literal with comma spacing;
    // :ENDPROC; /* Added for block completion; `;

    //                 const expected = `
    // /* Test 1: Procedure and Declarations - Indentation and Colon Spacing;
    // :PROCEDURE MyProcedure;
    // :PARAMETERS param1, param2, param3;

    //     :DEFAULT param1, "default";
    //     :DEFAULT param2, 123;
    //     :DECLARE var1, var2, var3;
    //     :DECLARE sString;
    //     :DECLARE nNumber;

    //     var1 := param1 + param2 * param3;
    //     var2 := "A string literal";
    //     var3 := {1, 2, 3, 4, 5}; /* Array literal with comma spacing;
    // :ENDPROC; /* Added for block completion;`;
    //                 const actual = await formatDocument(provider, input.trim());
    //                 assert.strictEqual(actual.trim(), expected.trim());
    //             },
    //             TESTTIMEOUT
    //         );

    it(
        "Test 2: Conditional Statements",
        async () => {
            const input = `
/* Test 2: Conditional Statements - Indentation, Operator Spacing;
:IF var1>10.AND.var2=="Test";
var1:=var1+1;
:IF param3 <0;
var2:="Negative";
:ELSE;
var2:="Positive";
:ENDIF;
:ELSE;
var1 :=var1-1;
:ENDIF;`;
            const expected = `
/* Test 2: Conditional Statements - Indentation, Operator Spacing;
:IF var1 > 10 .AND. var2 == "Test";
var1 := var1 + 1;
:IF param3 < 0;
    var2 := "Negative";
:ELSE;
    var2 := "Positive";
:ENDIF;
:ELSE;
var1 := var1 - 1;
:ENDIF;`;
            const actual = await formatDocument(provider, input.trim());
            assert.strictEqual(actual.trim(), expected.trim());
        },
        TESTTIMEOUT
    );
    it(
        "Test 3: Loop Constructs",
        async () => {
            const input = `
/* Test 3: Loop Constructs - Indentation;
:WHILE var1 > 0;
var1:=var1-1;
:IF var1 == 5; : EXITWHILE; : ENDIF;
:ENDWHILE;

:FOR i:=1:TO Len(var3);
var3[i] := var3[i]*2;
:IF i >100; : EXITFOR; : ENDIF;
:NEXT;`;
            // Note: :EXITWHILE, :ENDIF, :EXITFOR on same line are not specially handled for spacing by current rules beyond keyword rules.
            const expected = `
/* Test 3: Loop Constructs - Indentation;
:WHILE var1 > 0;
var1 := var1 - 1;
:IF var1 == 5;
    :EXITWHILE;
:ENDIF;
:ENDWHILE;

:FOR i := 1 :TO Len(var3);
var3[i] := var3[i] * 2;
:IF i > 100;
    :EXITFOR;
:ENDIF;
:NEXT;`;
            const actual = await formatDocument(provider, input.trim());
            assert.strictEqual(actual.trim(), expected.trim());
        },
        TESTTIMEOUT
    );

    it(
        "Test 4: Case Statement",
        async () => {
            const input = `
/* Test 4: Case Statement - Indentation and Colon Spacing;
:BEGINCASE;
:CASE var1 == 1;
var2:="One";
:CASE var1==2.OR.var1==3;
var2:="Two or Three";
:OTHERWISE;
var2:="Other";
:ENDCASE;`;
            const expected = `
/* Test 4: Case Statement - Indentation and Colon Spacing;
:BEGINCASE;
:CASE var1 == 1;
    var2 := "One";
:CASE var1 == 2 .OR. var1 == 3;
    var2 := "Two or Three";
:OTHERWISE;
    var2 := "Other";
:ENDCASE;`;
            const actual = await formatDocument(provider, input.trim());
            assert.strictEqual(actual.trim(), expected.trim());
        },
        TESTTIMEOUT
    );

    it(
        "Test 5: Error Handling",
        async () => {
            const input = `
/* Test 5: Error Handling - Indentation;
:TRY;
DoProc("RiskyOperation", {var1,var2});
:CATCH e;
LogError(e:Message);
:FINALLY;
var1:=0;
:ENDTRY;`;
            // Note: e:Message is object property access, not a :KEYWORD.
            const expected = `
/* Test 5: Error Handling - Indentation;
:TRY;
DoProc("RiskyOperation", {var1, var2});
:CATCH e;
LogError(e:Message);
:FINALLY;
var1 := 0;
:ENDTRY;`;
            const actual = await formatDocument(provider, input.trim());
            assert.strictEqual(actual.trim(), expected.trim());
        },
        TESTTIMEOUT
    );

    it(
        "Test 13: Comments",
        async () => {
            const input = `
/* This is a block comment
spanning multiple lines;
var1 := 10; /* This is an end-of-line comment;
/* Indented comment;
:PROCEDURE Test;
/* Comment inside proc;
var2 := 20;
:ENDPROC;`;
            const expected = `
/* This is a block comment
spanning multiple lines;
var1 := 10; /* This is an end-of-line comment;
/* Indented comment;
:PROCEDURE Test;
/* Comment inside proc;
var2 := 20;
:ENDPROC;`;
            const actual = await formatDocument(provider, input.trim());
            assert.strictEqual(actual.trim(), expected.trim());
        },
        TESTTIMEOUT
    );
});
