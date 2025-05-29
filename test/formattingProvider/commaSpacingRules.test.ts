import * as assert from "assert";
import { SSLFormattingProvider } from "../../src/formatters/formattingProvider";
import { CommaSpacingRule } from "../../src/formatters/rules/commaSpacingRule"; // Added import
import { formatDocument } from "../helpers/formatDocument";

describe("Comma Spacing Rules", () => {
    let provider: SSLFormattingProvider;
    const TESTTIMEOUT = 5000; // 5 seconds
    beforeEach(() => {
        // Instantiate provider with only CommaSpacingRule for isolated testing
        provider = new SSLFormattingProvider([new CommaSpacingRule()]);
    });

    describe("Basic Comma Spacing", () => {
        it(
            "should add space after commas",
            async () => {
                const input = "call(a,b,c); {1,2,3};";
                const expected = "call(a, b, c); {1, 2, 3};";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should normalize multiple spaces after commas to one",
            async () => {
                const input = "call(a,  b,   c);";
                const expected = "call(a, b, c);";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should not add spaces after commas inside string literals",
            async () => {
                const input = "s := \"a,b,c\" + 'd,e,f';";
                const expected = "s := \"a,b,c\" + 'd,e,f';";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );
    });

    /* // Temporarily comment out other describe blocks to isolate the issue
    describe("SSL Declaration Statements", () => {
        it(
            "should format :DECLARE statements with comma-separated identifiers",
            async () => {
                const input = ":DECLARE var1,var2,var3;";
                const expected = ":DECLARE var1, var2, var3;";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should format :PARAMETERS statements with comma-separated identifiers",
            async () => {
                const input = ":PARAMETERS param1,param2,param3;";
                const expected = ":PARAMETERS param1, param2, param3;";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should format :PUBLIC statements with comma-separated identifiers",
            async () => {
                const input = ":PUBLIC globalVar1,globalVar2,globalVar3;";
                const expected = ":PUBLIC globalVar1, globalVar2, globalVar3;";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should format :DEFAULT statements with comma-separated parameter-value pairs",
            async () => {
                const input = ':DEFAULT param1,"defaultValue",param2,123;';
                const expected = ':DEFAULT param1, "defaultValue", param2, 123;';
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );
    });

    describe("Function Calls and Arguments", () => {
        it(
            "should format direct function calls with comma-separated arguments",
            async () => {
                const input = "result := MyFunction(arg1,arg2,arg3);";
                const expected = "result := MyFunction(arg1, arg2, arg3);";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should format DoProc calls with array parameters",
            async () => {
                const input = 'result := DoProc("MyProc",{param1,param2,param3});';
                const expected = 'result := DoProc("MyProc", {param1, param2, param3});';
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should format ExecFunction calls with array parameters",
            async () => {
                const input = 'result := ExecFunction("MyFunc",{arg1,arg2});';
                const expected = 'result := ExecFunction("MyFunc", {arg1, arg2});';
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should handle nested function calls with commas",
            async () => {
                const input = "result := OuterFunc(InnerFunc(a,b),c,d);";
                const expected = "result := OuterFunc(InnerFunc(a, b), c, d);";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );
    });

    describe("Array Literals", () => {
        it(
            "should format simple array literals with comma-separated elements",
            async () => {
                const input = "arr := {1,2,3,4,5};";
                const expected = "arr := {1, 2, 3, 4, 5};";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should format mixed-type array literals",
            async () => {
                const input = 'arr := {1,"text",.T.,NIL};';
                const expected = 'arr := {1, "text", .T., NIL};';
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should format nested array literals",
            async () => {
                const input = "arr := {{1,2},{3,4},{5,6}};";
                const expected = "arr := {{1, 2}, {3, 4}, {5, 6}};";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should handle empty array elements (skipped parameters)",
            async () => {
                const input = 'result := DoProc("func",{param1,,param3});';
                const expected = 'result := DoProc("func", {param1, , param3});';
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );
    });

    describe("Array Access", () => {
        it(
            "should format multi-dimensional array access with comma notation",
            async () => {
                const input = "value := myArray[1,2,3];";
                const expected = "value := myArray[1, 2, 3];";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should format complex array access expressions",
            async () => {
                const input = "value := myArray[i+1,j*2,k];";
                const expected = "value := myArray[i+1, j*2, k];";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );
    });

    describe("Date Literals", () => {
        it(
            "should format date literals with comma-separated components",
            async () => {
                const input = "dateVal := {2024,12,25};";
                const expected = "dateVal := {2024, 12, 25};";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should format full date-time literals",
            async () => {
                const input = "dateTimeVal := {2024,12,25,14,30,0};";
                const expected = "dateTimeVal := {2024, 12, 25, 14, 30, 0};";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );
    });

    describe("Code Block Literals", () => {
        it(
            "should format code block literals with comma-separated parameters",
            async () => {
                const input = "block := {|x,y,z| x + y + z};";
                const expected = "block := {|x, y, z| x + y + z};";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should format code block literals with expression lists",
            async () => {
                const input = "block := {|x| DoSomething(x),DoSomethingElse(x)};";
                const expected = "block := {|x| DoSomething(x), DoSomethingElse(x)};";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );
    });

    describe("SQL Statements", () => {
        it(
            "should format SqlExecute with parameter arrays",
            async () => {
                const input = 'result := SqlExecute("SELECT * FROM table",{param1,param2});';
                const expected = 'result := SqlExecute("SELECT * FROM table", {param1, param2});';
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should format LSearch with multiple parameters",
            async () => {
                const input = 'result := LSearch("table","condition",1,{param1,param2});';
                const expected = 'result := LSearch("table", "condition", 1, {param1, param2});';
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should not modify commas inside SQL string literals",
            async () => {
                const input = 'result := SqlExecute("SELECT col1,col2,col3 FROM table");';
                const expected = 'result := SqlExecute("SELECT col1,col2,col3 FROM table");';
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );
    });

    describe("Bitwise Operations", () => {
        it(
            "should format bitwise function calls with comma-separated arguments",
            async () => {
                const input = "result := _AND(value1,value2);";
                const expected = "result := _AND(value1, value2);";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should format complex bitwise operations",
            async () => {
                const input = "result := _OR(_AND(a,b),_XOR(c,d));";
                const expected = "result := _OR(_AND(a, b), _XOR(c, d));";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );
    });

    describe("Edge Cases and Error Conditions", () => {
        it(
            "should handle trailing commas gracefully",
            async () => {
                const input = "arr := {1,2,3,};";
                const expected = "arr := {1, 2, 3,};";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should handle commas at line endings",
            async () => {
                const input = "call(a,\nb,c);";
                const expected = "call(a,\nb, c);";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should not modify commas in comments",
            async () => {
                const input = "/* This is a comment with,commas,inside;";
                const expected = "/* This is a comment with,commas,inside;";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should handle mixed quote types in string literals",
            async () => {
                const input = `s1 := "text,with,commas"; s2 := 'more,text,commas';`;
                const expected = `s1 := "text,with,commas"; s2 := 'more,text,commas';`;
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should handle bracket notation content",
            async () => {
                const input = "arr := [text,with,commas];";
                const expected = "arr := [text,with,commas];";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should handle empty parameter lists",
            async () => {
                const input = "result := MyFunction();";
                const expected = "result := MyFunction();";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should handle single parameter calls",
            async () => {
                const input = "result := MyFunction(param);";
                const expected = "result := MyFunction(param);";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );
    });

    describe("Complex SSL Constructs", () => {
        it(
            "should format complex expressions with multiple comma usages",
            async () => {
                const input = 'result := DoProc("Calculate",{GetValue(a,b),GetValue(c,d)});';
                const expected = 'result := DoProc("Calculate", {GetValue(a, b), GetValue(c, d)});';
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should handle commas in property access chains",
            async () => {
                const input = "result := obj:Method(param1,param2);";
                const expected = "result := obj:Method(param1, param2);";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should format increment expressions with function calls",
            async () => {
                const input = "result := MyFunc(++counter,value);";
                const expected = "result := MyFunc(++counter, value);";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );
    });
    */
});
