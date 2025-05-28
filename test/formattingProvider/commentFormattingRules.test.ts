import * as assert from "assert";
import { SSLFormattingProvider } from "../../src/formatters/formattingProvider";
import { formatDocument } from "../helpers/formatDocument";

describe("Comment Formatting Rules", () => {
    let provider: SSLFormattingProvider;
    const TESTTIMEOUT = 5000; // 5 seconds
    beforeEach(() => {
        provider = new SSLFormattingProvider();
    });

    it(
        "should format SSL comments with proper /* syntax",
        async () => {
            const input = "/ * This is a comment;";
            const expected = "/* This is a comment;";
            const result = await formatDocument(provider, input);
            assert.strictEqual(result, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should handle multiple spaces before asterisk",
        async () => {
            const input = "/   * Comment with multiple spaces;";
            const expected = "/* Comment with multiple spaces;";
            const result = await formatDocument(provider, input);
            assert.strictEqual(result, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should preserve comment content without modification",
        async () => {
            const input = "/* This comment has    extra    spaces inside;";
            const expected = "/* This comment has    extra    spaces inside;";
            const result = await formatDocument(provider, input);
            assert.strictEqual(result, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should format comments in SSL code context",
        async () => {
            const input = `:PROCEDURE TestProc;
/ * This is a procedure comment;
nVariable = 1;
:ENDPROC;`;
            const expected = `:PROCEDURE TestProc
/* This is a procedure comment;
nVariable = 1;
:ENDPROC;`;
            const result = await formatDocument(provider, input);
            assert.strictEqual(result, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should handle multiple comment lines",
        async () => {
            const input = `/ * First comment;
/ * Second comment;
:PROCEDURE Test;
:ENDPROC;`;
            const expected = `/* First comment;
/* Second comment;
:PROCEDURE Test;
:ENDPROC;`;
            const result = await formatDocument(provider, input);
            assert.strictEqual(result, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should not modify properly formatted comments",
        async () => {
            const input = "/* Already properly formatted comment;";
            const expected = "/* Already properly formatted comment;";
            const result = await formatDocument(provider, input);
            assert.strictEqual(result, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should handle comments with various spacing patterns",
        async () => {
            const input = `/  *Comment with two spaces;
/\t*Comment with tab;
/    *Comment with four spaces;`;
            const expected = `/*Comment with two spaces;
/*Comment with tab;
/*Comment with four spaces;`;
            const result = await formatDocument(provider, input);
            assert.strictEqual(result, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should handle comments in class definitions",
        async () => {
            const input = `:CLASS TestClass;
/ * Class comment;
:DECLARE nField;
:PROCEDURE Method;
/ * Method comment;
:ENDPROC;`;
            const expected = `:CLASS TestClass;
/* Class comment;
:DECLARE nField;
:PROCEDURE Method;
    /* Method comment;
:ENDPROC;`;
            const result = await formatDocument(provider, input);
            assert.strictEqual(result, expected);
        },
        TESTTIMEOUT
    );
});
