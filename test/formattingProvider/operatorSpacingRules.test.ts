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
            const expected = "a .AND. b .OR. c";
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
        "should handle .NOT. logical operator",
        async () => {
            const input = "result := .NOT.condition";
            const expected = "result := .NOT. condition";
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should not add spaces around increment/decrement operators",
        async () => {
            const input = "i++; j--; ++k; --l;";
            const expected = "i++; j--; ++k; --l;";
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should handle unary operators without spaces",
        async () => {
            const input = "a := -5; b := +10; c := !flag;";
            const expected = "a := -5; b := +10; c := !flag;";
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should handle unary operators in expressions",
        async () => {
            const input = "result := x + -y * +z;";
            const expected = "result := x + -y * +z;";
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should handle mixed assignment and arithmetic operators",
        async () => {
            const input = "total+=price*quantity-discount;";
            const expected = "total += price * quantity - discount;";
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should handle complex expressions with parentheses",
        async () => {
            const input = "result:=(a+b)*(c-d)/(e^f);";
            const expected = "result := (a + b) * (c - d) / (e ^ f);";
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should not add spaces around operators inside string literals",
        async () => {
            const input = 's := "a:=b" + \'c+d\' + "e*f";';
            const expected = 's := "a:=b" + \'c+d\' + "e*f";';
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should not add spaces around operators inside comments",
        async () => {
            const input = "a := 1; /* comment with a:=b+c */";
            const expected = "a := 1; /* comment with a:=b+c */";
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should handle property access colons without spaces",
        async () => {
            const input = "value := object:property;";
            const expected = "value := object:property;";
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should handle method calls with property access",
        async () => {
            const input = "result := object:method(param1,param2);";
            const expected = "result := object:method(param1, param2);";
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should handle array access with operators",
        async () => {
            const input = "arr[i+1]:=value*2;";
            const expected = "arr[i + 1] := value * 2;";
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should handle power operator precedence",
        async () => {
            const input = "result:=a+b^c*d;";
            const expected = "result := a + b ^ c * d;";
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should handle modulo operator",
        async () => {
            const input = "remainder:=dividend%divisor;";
            const expected = "remainder := dividend % divisor;";
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should handle boolean literals with logical operators",
        async () => {
            const input = "flag := .T. .AND. condition .OR. .F.;";
            const expected = "flag := .T. .AND. condition .OR. .F.;";
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should handle function calls with operator expressions",
        async () => {
            const input = 'result := DoProc("Calculate", {a+b, c*d});';
            const expected = 'result := DoProc("Calculate", {a + b, c * d});';
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should handle nested array literals with operators",
        async () => {
            const input = "matrix := {{a+b, c-d}, {e*f, g/h}};";
            const expected = "matrix := {{a + b, c - d}, {e * f, g / h}};";
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should handle conditional expressions in SSL style",
        async () => {
            const input = "result := IIF(a>b, a+1, b-1);";
            const expected = "result := IIF(a > b, a + 1, b - 1);";
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should handle SQL parameter expressions",
        async () => {
            const input = 'sql := "SELECT * FROM table WHERE id = ?param? AND value > ?value?";';
            const expected = 'sql := "SELECT * FROM table WHERE id = ?param? AND value > ?value?";';
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should handle compound operators in loops",
        async () => {
            const input = ":FOR i := 1 :TO count; total += values[i]; :NEXT;";
            const expected = ":FOR i := 1 :TO count; total += values[i]; :NEXT;";
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should preserve existing correct spacing",
        async () => {
            const input = "a := b + c * d - e / f;";
            const expected = "a := b + c * d - e / f;";
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should handle multiple operators on same line",
        async () => {
            const input = "x:=1;y+=2;z*=3;";
            const expected = "x := 1; y += 2; z *= 3;";
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should handle operators in date literals",
        async () => {
            const input = "date := {year+1, month-1, day};";
            const expected = "date := {year + 1, month - 1, day};";
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should handle operators with scientific notation",
        async () => {
            const input = "result := 1.23e5 + 4.56e-3 * factor;";
            const expected = "result := 1.23e5 + 4.56e-3 * factor;";
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should handle code block expressions with operators",
        async () => {
            const input = "block := {|x| x*2 + 1};";
            const expected = "block := {|x| x * 2 + 1};";
            const actual = await formatDocument(provider, input);
            assert.strictEqual(actual, expected);
        },
        TESTTIMEOUT
    );

    describe("Edge cases and error handling", () => {
        it(
            "should handle malformed expressions gracefully",
            async () => {
                const input = "a := +; b := *=; c := ==;";
                // Should not crash, fallback to string processing
                const actual = await formatDocument(provider, input);
                assert.ok(typeof actual === "string");
            },
            TESTTIMEOUT
        );

        it(
            "should handle empty lines",
            async () => {
                const input = "";
                const expected = "";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should handle lines with only operators",
            async () => {
                const input = "+= -= *=";
                const expected = "+= -= *=";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );
    });

    describe("SSL-specific operator patterns", () => {
        it(
            "should handle bitwise function calls (not operators)",
            async () => {
                const input = "result := _AND(a, b) + _OR(c, d);";
                const expected = "result := _AND(a, b) + _OR(c, d);";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should handle object creation with operators",
            async () => {
                const input = "obj := CreateUDObject() + value;";
                const expected = "obj := CreateUDObject() + value;";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );

        it(
            "should handle NIL comparisons",
            async () => {
                const input = "if (value != NIL .AND. value > 0);";
                const expected = "if (value != NIL .AND. value > 0);";
                const actual = await formatDocument(provider, input);
                assert.strictEqual(actual, expected);
            },
            TESTTIMEOUT
        );
    });
});
