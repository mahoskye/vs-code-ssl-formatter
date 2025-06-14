import { tokenize } from "../../src/tokenizer";
import { parse } from "../../src/parser";
import { SSLFormatter } from "../../src/formatter";

describe("General Statement/Expression Formatting Investigation", () => {
    it("should investigate simple expression formatting", () => {
        const testCases = [
            { name: "Simple number", code: "1" },
            { name: "Simple string", code: '"hello"' },
            { name: "Simple variable", code: "result" },
            { name: "Simple assignment", code: 'result := "one";' },
        ];

        for (const testCase of testCases) {
            console.log(`\n=== Testing: ${testCase.name} ===`);
            console.log(`Input: ${testCase.code}`);

            const tokens = tokenize(testCase.code);
            const parseResult = parse(tokens);

            if (parseResult.success) {
                console.log(`AST kind: ${parseResult.ast.kind}`);

                if (parseResult.ast.body && parseResult.ast.body.length > 0) {
                    console.log(`First statement kind: ${parseResult.ast.body[0].kind}`);
                }

                const formatter = new SSLFormatter();
                const formatted = formatter.format(parseResult.ast);
                console.log(`Formatted: "${formatted.trim()}"`);

                // Check if anything was actually formatted
                if (formatted.trim() === "") {
                    console.log("❌ ISSUE: No output generated");
                } else if (formatted.trim() === testCase.code.trim()) {
                    console.log("✅ OK: Output matches input");
                } else {
                    console.log("⚠️  CHANGED: Output differs from input");
                }
            } else {
                console.log("❌ Parse failed");
            }
        }
    });
});
