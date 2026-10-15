import { describe, it } from "mocha";
import { expect } from "chai";
import { replaceRetiredValue, RETIRED_VALUES } from "../src/utils/settingsMigration";

describe("Retired setting values", () => {
    it("maps the retired SQL style compact to canonicalCompact", () => {
        expect(replaceRetiredValue("format.sql.style", "compact")).to.equal("canonicalCompact");
    });

    it("leaves current values unchanged", () => {
        for (const style of ["standard", "canonicalCompact", "expanded"]) {
            expect(replaceRetiredValue("format.sql.style", style)).to.equal(style);
        }
    });

    it("only maps the value for its own key", () => {
        expect(replaceRetiredValue("format.sql.keywordCase", "compact")).to.equal("compact");
    });

    it("never maps a value to one that is itself retired", () => {
        for (const { key, replacement } of RETIRED_VALUES) {
            expect(replaceRetiredValue(key, replacement)).to.equal(replacement);
        }
    });
});
