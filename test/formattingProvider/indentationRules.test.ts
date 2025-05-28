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
            // Pass formatting options to formatDocument
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
        "should trim and not indent line-starting comments",
        async () => {
            const input = `
/* Top level comment;
:PROCEDURE Test;
/* Indented comment in source;
var1 := 1;
:ENDPROC`;
            // According to IndentationRule: comments starting a line are trimmed and not indented by the rule.
            const expected = `
/* Top level comment;
:PROCEDURE Test;
    /* Indented comment in source;
    var1 := 1;
:ENDPROC`;
            const actual = await formatDocument(provider, input.trim());
            assert.strictEqual(actual.trim(), expected.trim());
        },
        TESTTIMEOUT
    );
});
