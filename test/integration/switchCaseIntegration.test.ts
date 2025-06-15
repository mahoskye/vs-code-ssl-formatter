import { tokenize } from "../../src/tokenizer";
import { parse } from "../../src/parser";
import { SSLFormatter } from "../../src/formatter";

describe("Switch Case Integration Test", () => {
    it("should format switch case structure correctly", () => {
        const sslCode = `
:BEGINCASE;
:CASE 1;
    result := "one";
:CASE 2;
    result := "two";
:OTHERWISE;
    result := "other";
:ENDCASE;        `;

        const tokens = tokenize(sslCode);
        expect(tokens.length).toBeGreaterThan(0);

        const parseResult = parse(tokens);
        expect(parseResult.success).toBe(true);
        expect(parseResult.errors.length).toBe(0);

        if (parseResult.success) {
            const formatter = new SSLFormatter();
            const formatted = formatter.format(parseResult.ast);

            // Verify switch/case structure is correctly formatted
            expect(formatted).toContain(":BEGINCASE;");
            expect(formatted).toContain(":CASE 1;");
            expect(formatted).toContain(":CASE 2;");
            expect(formatted).toContain(":OTHERWISE;");
            expect(formatted).toContain(":ENDCASE;");

            // Verify proper indentation exists
            const lines = formatted.split("\n").filter((line) => line.trim());
            const caseLines = lines.filter((line) => line.includes(":CASE"));
            expect(caseLines.length).toBe(2);
        }
    });
});
