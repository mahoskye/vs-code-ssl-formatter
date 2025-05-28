import * as assert from "assert";
import { SSLFormattingProvider } from "../../src/formatters/formattingProvider";
import { formatDocument } from "../helpers/formatDocument";

describe("Indentation Rules", () => {
    let provider: SSLFormattingProvider;
    const TESTTIMEOUT = 5000; // 5 seconds

    beforeEach(() => {
        provider = new SSLFormattingProvider();
    });

    it(
        "should indent :PROCEDURE blocks",
        async () => {
            const input = `
:PROCEDURE TestProc;
var1 := 1;
:ENDPROC;`;
            const expected = `
:PROCEDURE TestProc;
    var1 := 1;
:ENDPROC;`;
            const actual = await formatDocument(provider, input.trim(), {
                tabSize: 4,
                insertSpaces: true,
            });
            assert.strictEqual(actual.trim(), expected.trim());
        },
        TESTTIMEOUT
    );

    it("should indent :IF/:ELSE/:ENDIF blocks", async () => {
        const input = `
:IF .T.;
var1 := 1;
:ELSE;
var2 := 2;
:ENDIF;`;
        const expected = `
:IF .T.;
    var1 := 1;
:ELSE;
    var2 := 2;
:ENDIF;`;
        const actual = await formatDocument(provider, input.trim());
        assert.strictEqual(actual.trim(), expected.trim());
    });

    it(
        "should indent nested blocks correctly",
        async () => {
            const input = `
:PROCEDURE Outer;
:IF .T.;
var1 := 1;
:WHILE var1 < 10;
var1 := var1 + 1;
:ENDWHILE;
:ENDIF;
:ENDPROC;`;
            const expected = `
:PROCEDURE Outer;
    :IF .T.;
        var1 := 1;
        :WHILE var1 < 10;
            var1 := var1 + 1;
        :ENDWHILE;
    :ENDIF;
:ENDPROC;`;
            const actual = await formatDocument(provider, input.trim());
            assert.strictEqual(actual.trim(), expected.trim());
        },
        TESTTIMEOUT
    );

    it(
        "should not indent empty lines",
        async () => {
            const input = `
:PROCEDURE Test;

var1 := 1;

:ENDPROC;`;
            const expected = `
:PROCEDURE Test;

    var1 := 1;

:ENDPROC;`;
            const actual = await formatDocument(provider, input.trim());
            assert.strictEqual(actual.trim(), expected.trim());
        },
        TESTTIMEOUT
    );

    it(
        "should indent comments normally within procedures",
        async () => {
            const input = `
/* Top level comment;
:PROCEDURE Test;
/* Indented comment in procedure;
var1 := 1;
:ENDPROC;`;
            const expected = `
/* Top level comment;
:PROCEDURE Test;
    /* Indented comment in procedure;
    var1 := 1;
:ENDPROC;`;
            const actual = await formatDocument(provider, input.trim());
            assert.strictEqual(actual.trim(), expected.trim());
        },
        TESTTIMEOUT
    );

    it(
        "should handle :TRY/:CATCH/:FINALLY/:ENDTRY blocks",
        async () => {
            const input = `
:PROCEDURE TestErrorHandling;
:TRY;
var1 := SomeRiskyOperation();
:CATCH;
var1 := NIL;
:FINALLY;
CleanupResources();
:ENDTRY;
:ENDPROC;`;
            const expected = `
:PROCEDURE TestErrorHandling;
    :TRY;
        var1 := SomeRiskyOperation();
    :CATCH;
        var1 := NIL;
    :FINALLY;
        CleanupResources();
    :ENDTRY;
:ENDPROC;`;
            const actual = await formatDocument(provider, input.trim());
            assert.strictEqual(actual.trim(), expected.trim());
        },
        TESTTIMEOUT
    );

    it(
        "should handle :BEGINCASE/:CASE/:OTHERWISE/:ENDCASE blocks",
        async () => {
            const input = `
:PROCEDURE TestCaseStatement;
:BEGINCASE;
:CASE nValue = 1;
result := "One";
:CASE nValue = 2;
result := "Two";
:OTHERWISE;
result := "Unknown";
:ENDCASE;
:ENDPROC;`;
            const expected = `
:PROCEDURE TestCaseStatement;
    :BEGINCASE;
        :CASE nValue = 1;
            result := "One";
        :CASE nValue = 2;
            result := "Two";
        :OTHERWISE;
            result := "Unknown";
    :ENDCASE;
:ENDPROC;`;
            const actual = await formatDocument(provider, input.trim());
            assert.strictEqual(actual.trim(), expected.trim());
        },
        TESTTIMEOUT
    );

    it(
        "should handle :FOR/:TO/:NEXT loops",
        async () => {
            const input = `
:PROCEDURE TestForLoop;
:FOR i := 1 :TO 10;
sum := sum + i;
:NEXT;
:ENDPROC;`;
            const expected = `
:PROCEDURE TestForLoop;
    :FOR i := 1 :TO 10;
        sum := sum + i;
    :NEXT;
:ENDPROC;`;
            const actual = await formatDocument(provider, input.trim());
            assert.strictEqual(actual.trim(), expected.trim());
        },
        TESTTIMEOUT
    );

    it(
        "should handle :REGION/:ENDREGION blocks",
        async () => {
            const input = `
:PROCEDURE TestRegions;
:REGION Setup;
var1 := 1;
var2 := 2;
:ENDREGION;
:REGION Processing;
result := var1 + var2;
:ENDREGION;
:ENDPROC;`;
            const expected = `
:PROCEDURE TestRegions;
    :REGION Setup;
        var1 := 1;
        var2 := 2;
    :ENDREGION;
    :REGION Processing;
        result := var1 + var2;
    :ENDREGION;
:ENDPROC;`;
            const actual = await formatDocument(provider, input.trim());
            assert.strictEqual(actual.trim(), expected.trim());
        },
        TESTTIMEOUT
    );

    it(
        "should handle :CLASS definitions and inheritance",
        async () => {
            const input = `
:CLASS MyClass;
:INHERIT BaseClass;
:DECLARE sProperty1, nProperty2;
:PROCEDURE Initialize;
sProperty1 := "default";
nProperty2 := 0;
:ENDPROC;
:PROCEDURE DoSomething;
:RETURN sProperty1;
:ENDPROC;`;
            const expected = `
:CLASS MyClass;
:INHERIT BaseClass;
    :DECLARE sProperty1, nProperty2;
    :PROCEDURE Initialize;
        sProperty1 := "default";
        nProperty2 := 0;
    :ENDPROC;
    :PROCEDURE DoSomething;
        :RETURN sProperty1;
    :ENDPROC;`;
            const actual = await formatDocument(provider, input.trim());
            assert.strictEqual(actual.trim(), expected.trim());
        },
        TESTTIMEOUT
    );

    it(
        "should handle complex nested structures",
        async () => {
            const input = `
:PROCEDURE ComplexExample;
:DECLARE arItems, i;
arItems := {1, 2, 3, 4, 5};
:FOR i := 1 :TO Len(arItems);
:IF arItems[i] % 2 = 0;
:TRY;
result := ProcessEvenNumber(arItems[i]);
:CATCH;
result := 0;
:ENDTRY;
:ELSE;
result := arItems[i];
:ENDIF;
:NEXT;
:ENDPROC;`;
            const expected = `
:PROCEDURE ComplexExample;
    :DECLARE arItems, i;
    arItems := {1, 2, 3, 4, 5};
    :FOR i := 1 :TO Len(arItems);
        :IF arItems[i] % 2 = 0;
            :TRY;
                result := ProcessEvenNumber(arItems[i]);
            :CATCH;
                result := 0;
            :ENDTRY;
        :ELSE;
            result := arItems[i];
        :ENDIF;
    :NEXT;
:ENDPROC;`;
            const actual = await formatDocument(provider, input.trim());
            assert.strictEqual(actual.trim(), expected.trim());
        },
        TESTTIMEOUT
    );

    it(
        "should handle :PARAMETERS and :DECLARE statements",
        async () => {
            const input = `
:PROCEDURE TestDeclarations;
:PARAMETERS param1, param2;
:DECLARE sLocal, nLocal;
sLocal := param1;
nLocal := param2;
:ENDPROC;`;
            const expected = `
:PROCEDURE TestDeclarations;
    :PARAMETERS param1, param2;
    :DECLARE sLocal, nLocal;
    sLocal := param1;
    nLocal := param2;
:ENDPROC;`;
            const actual = await formatDocument(provider, input.trim());
            assert.strictEqual(actual.trim(), expected.trim());
        },
        TESTTIMEOUT
    );
});
