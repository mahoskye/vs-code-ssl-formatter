import * as assert from "assert";
import { SSLFormattingProvider } from "../../src/formatters/formattingProvider";
import { formatDocument } from "../helpers/formatDocument";

describe("Colon Spacing Rules", () => {
    let provider: SSLFormattingProvider;
    const TESTTIMEOUT = 5000; // 5 seconds
    beforeEach(() => {
        provider = new SSLFormattingProvider();
    });

    it(
        "should add space after colon-keywords and uppercase them",
        async () => {
            const input = ":proceduremyProc;:parametersp1;";
            // Rule only processes first :keyword on line, and uppercases it.
            // Assumes MyProcedure is an identifier part of :PROCEDURE if no space.
            // If :procedure myProc, keyword is "procedure", rest is "myProc..."
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
});
