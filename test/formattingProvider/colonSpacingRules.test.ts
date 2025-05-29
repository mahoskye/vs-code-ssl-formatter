import { SSLFormattingProvider } from "../../src/formatters/formattingProvider";
import { ColonSpacingRule } from "../../src/formatters/rules/colonSpacingRule"; // Corrected path
import { formatDocument } from "../helpers/formatDocument";
import * as assert from "assert";

describe("Colon Spacing Rules", () => {
    let provider: SSLFormattingProvider;
    const TESTTIMEOUT = 5000; // 5 seconds

    beforeEach(() => {
        // Instantiate provider with only ColonSpacingRule for isolated testing
        provider = new SSLFormattingProvider([new ColonSpacingRule()]);
    });

    describe("Basic Colon Keyword Formatting", () => {
        it(
            "should add space after colon-keywords and uppercase them",
            async () => {
                const input = ":proceduremyProc;:parametersp1;";
                const expected = ":PROCEDURE myProc; :PARAMETERS p1;";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);

                const input2 = ":if.t.";
                const expected2 = ":IF .t.";
                const actual2 = await formatDocument(provider, input2);
                assert.strictEqual(actual2, expected2);
            },
            TESTTIMEOUT
        );

        it(
            "should handle keywords already spaced and uppercased",
            async () => {
                const input = ":PROCEDURE ExistingProc; :PARAMETERS P1;";
                const expected = ":PROCEDURE ExistingProc; :PARAMETERS P1;";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should handle mixed case keywords",
            async () => {
                const input = ":Procedure myProc; :Parameters p1, p2;";
                const expected = ":PROCEDURE myProc; :PARAMETERS p1, p2;";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );
    });

    describe("Common SSL Colon Keywords", () => {
        it(
            "should format procedure-related keywords",
            async () => {
                const tests = [
                    { input: ":proceduremyFunction;", expected: ":PROCEDURE myFunction;" },
                    { input: ":endproc;", expected: ":ENDPROC;" },
                    { input: ":parametersparam1,param2;", expected: ":PARAMETERS param1,param2;" },
                    { input: ':defaultparam1,"value";', expected: ':DEFAULT param1,"value";' },
                ];

                for (const test of tests) {
                    const actual = await formatDocument(provider, test.input);
                    assert.strictEqual(actual, test.expected, `Failed for input: ${test.input}`);
                }
            },
            TESTTIMEOUT
        );

        it(
            "should format control flow keywords",
            async () => {
                const tests = [
                    { input: ":ifcondition;", expected: ":IF condition;" },
                    { input: ":else;", expected: ":ELSE;" },
                    { input: ":endif;", expected: ":ENDIF;" },
                    { input: ":whilei<10;", expected: ":WHILE i<10;" },
                    { input: ":endwhile;", expected: ":ENDWHILE;" },
                    { input: ":fori:=1:to10;", expected: ":FOR i:=1 :TO 10;" },
                    { input: ":next;", expected: ":NEXT;" },
                ];

                for (const test of tests) {
                    const actual = await formatDocument(provider, test.input);
                    assert.strictEqual(actual, test.expected, `Failed for input: ${test.input}`);
                }
            },
            TESTTIMEOUT
        );

        it(
            "should format case statement keywords",
            async () => {
                const tests = [
                    { input: ":begincase;", expected: ":BEGINCASE;" },
                    { input: ":casenValue;", expected: ":CASE nValue;" },
                    { input: ":otherwise;", expected: ":OTHERWISE;" },
                    { input: ":endcase;", expected: ":ENDCASE;" },
                    { input: ":exitcase;", expected: ":EXITCASE;" },
                ];

                for (const test of tests) {
                    const actual = await formatDocument(provider, test.input);
                    assert.strictEqual(actual, test.expected, `Failed for input: ${test.input}`);
                }
            },
            TESTTIMEOUT
        );

        it(
            "should format error handling keywords",
            async () => {
                const tests = [
                    { input: ":try;", expected: ":TRY;" },
                    { input: ":catch;", expected: ":CATCH;" },
                    { input: ":finally;", expected: ":FINALLY;" },
                    { input: ":endtry;", expected: ":ENDTRY;" },
                    { input: ":error;", expected: ":ERROR;" },
                ];

                for (const test of tests) {
                    const actual = await formatDocument(provider, test.input);
                    assert.strictEqual(actual, test.expected, `Failed for input: ${test.input}`);
                }
            },
            TESTTIMEOUT
        );

        it(
            "should format declaration keywords",
            async () => {
                const tests = [
                    { input: ":declarevariable;", expected: ":DECLARE variable;" },
                    { input: ":publicglobalVar;", expected: ":PUBLIC globalVar;" },
                    { input: ':include"script.ssl";', expected: ':INCLUDE "script.ssl";' },
                ];

                for (const test of tests) {
                    const actual = await formatDocument(provider, test.input);
                    assert.strictEqual(actual, test.expected, `Failed for input: ${test.input}`);
                }
            },
            TESTTIMEOUT
        );
    });

    describe("Semicolon Handling", () => {
        it(
            "should not add space before semicolons",
            async () => {
                const input = ":procedure;";
                const expected = ":PROCEDURE;";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should handle keywords with content and semicolons",
            async () => {
                const input = ":proceduremyProc;";
                const expected = ":PROCEDURE myProc;";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );
    });

    describe("Multiple Colon Keywords on Same Line", () => {
        it(
            "should format multiple keywords on same line",
            async () => {
                const input = ":proceduremyProc;:parametersp1,p2;";
                const expected = ":PROCEDURE myProc; :PARAMETERS p1,p2;";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should handle mixed formatted and unformatted keywords",
            async () => {
                const input = ":PROCEDURE myProc;:parametersp1;";
                const expected = ":PROCEDURE myProc; :PARAMETERS p1;";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );
    });

    describe("Whitespace Preservation", () => {
        it(
            "should preserve leading indentation",
            async () => {
                const input = "    :proceduremyProc;";
                const expected = "    :PROCEDURE myProc;";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should preserve tabs and mixed indentation",
            async () => {
                const input = "\t  :ifcondition;";
                const expected = "\t  :IF condition;";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );
    });

    describe("Non-Colon Keywords", () => {
        it(
            "should not modify lines without colon keywords",
            async () => {
                const input = 'sVariable := "value";';
                const expected = 'sVariable := "value";';
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should not modify unknown colon keywords",
            async () => {
                const input = ":unknownkeyword;";
                const expected = ":unknownkeyword;";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should not modify empty or whitespace-only lines",
            async () => {
                const input = "   ";
                const expected = "   ";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );
    });

    describe("Complex Expressions with Colon Keywords", () => {
        it(
            "should handle keywords with complex conditions",
            async () => {
                const input = ':if(nValue>10).AND.(sName="test");';
                const expected = ':IF (nValue>10).AND.(sName="test");';
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should handle keywords with function calls",
            async () => {
                const input = ":whileLen(sString)>0;";
                const expected = ":WHILE Len(sString)>0;";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should handle keywords with boolean literals",
            async () => {
                const input = ":if.T.;";
                const expected = ":IF .T.;";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );
    });

    describe("Edge Cases", () => {
        it(
            "should handle colon at start with no keyword",
            async () => {
                const input = ":";
                const expected = ":";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should handle malformed colon syntax",
            async () => {
                const input = ": procedure;"; // Space after colon
                const expected = ": procedure;"; // Should remain unchanged
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should handle string literals containing colons",
            async () => {
                const input = ':include"path:to:script";';
                const expected = ':INCLUDE "path:to:script";';
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );
    });
});
