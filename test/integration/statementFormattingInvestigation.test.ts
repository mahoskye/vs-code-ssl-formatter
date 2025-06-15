import { tokenize } from "../../src/tokenizer";
import { parse } from "../../src/parser";
import { SSLFormatter } from "../../src/formatter";

describe("General Statement/Expression Formatting Investigation", () => {
    it("should investigate simple expression formatting", () => {
        const testCases = [
            { name: "Simple number assignment", code: "value := 1;" },
            { name: "Simple string assignment", code: 'text := "hello";' },
            { name: "Simple variable assignment", code: "result := value;" },
            { name: "Simple assignment with string", code: 'result := "one";' },
        ];

        for (const testCase of testCases) {
            const tokens = tokenize(testCase.code);
            const parseResult = parse(tokens);

            if (parseResult.success) {
                const formatter = new SSLFormatter();
                const formatted = formatter.format(parseResult.ast); // Check if anything was actually formatted
                if (formatted.trim() === "") {
                    throw new Error(`No output generated for: ${testCase.name}`);
                } else {
                    // Basic assertion that formatting succeeded
                    expect(formatted).toBeDefined();
                }
            } else {
                throw new Error(`Parse failed for: ${testCase.name}`);
            }
        }
    });
});
