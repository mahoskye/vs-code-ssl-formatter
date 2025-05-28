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
            const expected = `:PROCEDURE TestProc;
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
            const expected = `/* Comment with two spaces;
/* Comment with tab;
/* Comment with four spaces;`;
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

    it(
        "should format region comments properly",
        async () => {
            const input = `/ * region Test Section;
nVariable := 1;
/ * endregion;`;
            const expected = `/* region Test Section;
nVariable := 1;
/* endregion;`;
            const result = await formatDocument(provider, input);
            assert.strictEqual(result, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should ensure comments end with semicolon",
        async () => {
            const input = "/* Comment without semicolon";
            const expected = "/* Comment without semicolon;";
            const result = await formatDocument(provider, input);
            assert.strictEqual(result, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should handle single space after /* for readability",
        async () => {
            const input = "/*Comment without space after asterisk;";
            const expected = "/* Comment without space after asterisk;";
            const result = await formatDocument(provider, input);
            assert.strictEqual(result, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should normalize multiple spaces after /*",
        async () => {
            const input = "/*    Multiple spaces after asterisk;";
            const expected = "/* Multiple spaces after asterisk;";
            const result = await formatDocument(provider, input);
            assert.strictEqual(result, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should handle empty comments",
        async () => {
            const input = "/*;";
            const expected = "/*;";
            const result = await formatDocument(provider, input);
            assert.strictEqual(result, expected);
        },
        TESTTIMEOUT
    );

    it(
        "should format region comments with proper casing",
        async () => {
            const input = `/* REGION test section;
/* ENDREGION;`;
            const expected = `/* region test section;
/* endregion;`;
            const result = await formatDocument(provider, input);
            assert.strictEqual(result, expected);
        },
        TESTTIMEOUT
    );
});
