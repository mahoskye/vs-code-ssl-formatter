import * as assert from "assert";
import { SSLFormattingProvider } from "../../src/formatters/formattingProvider";
import { formatDocument } from "../helpers/formatDocument";

describe("Comma Spacing Rules", () => {
    let provider: SSLFormattingProvider;
    const TESTTIMEOUT = 5000; // 5 seconds
    beforeEach(() => {
        provider = new SSLFormattingProvider();
    });

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
        "should not add spaces after commas inside string literals (heuristic)",
        async () => {
            const input = "s := \"a,b,c\" + 'd,e,f';";
            const expected = "s := \"a,b,c\" + 'd,e,f';";
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );
});
