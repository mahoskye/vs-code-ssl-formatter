import * as assert from "assert";
import { SSLFormattingProvider } from "../../src/formatters/formattingProvider";
import { formatDocument } from "../helpers/formatDocument";

describe("Formatting Options", () => {
    let provider: SSLFormattingProvider;
    const TESTTIMEOUT = 5000; // 5 seconds
    beforeEach(() => {
        provider = new SSLFormattingProvider();
    });

    it(
        "should use specified tabSize and insertSpaces=true",
        async () => {
            const input = `
:PROCEDURE Test;
:IF .T.;
var1:=1;
:ENDIF;
:ENDPROC;`;
            const expected = `
:PROCEDURE Test;
:IF .T.;
    var1 := 1;
:ENDIF;
:ENDPROC;`;
            const actual = await formatDocument(provider, input.trim(), {
                tabSize: 2,
                insertSpaces: true,
            });
            assert.strictEqual(actual.trim(), expected.trim());
        },
        TESTTIMEOUT
    );

    it(
        "should use specified tabSize and insertSpaces=false (tabs)",
        async () => {
            const input = `
:PROCEDURE Test;
:IF .T.;
var1:=1;
:ENDIF;
:ENDPROC;`;
            const expected = `
:PROCEDURE Test;
:IF .T.;
    var1 := 1;
:ENDIF;
:ENDPROC;`;
            const actual = await formatDocument(provider, input.trim(), {
                tabSize: 1,
                insertSpaces: false,
            });
            assert.strictEqual(actual.trim(), expected.trim());
        },
        TESTTIMEOUT
    );
});
