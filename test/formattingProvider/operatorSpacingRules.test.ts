import * as assert from "assert";
import { SSLFormattingProvider } from "../../src/formatters/formattingProvider";
import { formatDocument } from "../helpers/formatDocument";

describe("Operator Spacing Rules", () => {
    let provider: SSLFormattingProvider;
    const TESTTIMEOUT = 5000; // 5 seconds
    beforeEach(() => {
        provider = new SSLFormattingProvider();
    });

    it(
        "should add spaces around assignment operators",
        async () => {
            const input = "a:=1; b+=2; c-=3; d*=4; e/=5; f^=6;";
            const expected = "a := 1; b += 2; c -= 3; d *= 4; e /= 5; f ^= 6;";
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should add spaces around comparison operators",
        async () => {
            const input = "a==b; c!=d; e<f; g>h; i<=j; k>=l; m=n;";
            // Note: simple '=' is tokenized as SIMPLE_EQUALS, rule adds spaces
            const expected = "a == b; c != d; e < f; g > h; i <= j; k >= l; m = n;";
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should add spaces around arithmetic operators",
        async () => {
            const input = "a=1+2-3*4/5^6%7";
            const expected = "a = 1 + 2 - 3 * 4 / 5 ^ 6 % 7";
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should add spaces around logical operators .AND. and .OR.",
        async () => {
            const input = "a .AND. b .OR. c";
            const expected = "a .AND. b .OR. c"; // Assumes input already has spaces, rule ensures one space
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);

            const input2 = "a.AND.b.OR.c";
            const expected2 = "a .AND. b .OR. c";
            const actual2 = await formatDocument(provider, input2);
            assert.strictEqual(actual2, expected2);
        },
        TESTTIMEOUT
    );

    it(
        "should not add spaces around operators inside string literals (heuristic)",
        async () => {
            const input = "s := \"a:=b\" + 'c+d' + [e*f];";
            const expected = "s := \"a:=b\" + 'c+d' + [e*f];"; // Heuristic should prevent spacing inside
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );
});
