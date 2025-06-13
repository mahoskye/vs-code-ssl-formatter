/**
 * Tests for formatter options
 * Ensures configuration interface works correctly and validates SSL-specific formatting options
 */

import {
    FormatterOptions,
    defaultFormatterOptions,
    mergeFormatterOptions,
    validateFormatterOptions,
    createFormatterOptionsFromConfig,
} from "../../src/formatter/options";

describe("FormatterOptions", () => {
    describe("defaultFormatterOptions", () => {
        it("should have correct default values for basic formatting", () => {
            expect(defaultFormatterOptions.indentSize).toBe(4);
            expect(defaultFormatterOptions.useTabs).toBe(false);
            expect(defaultFormatterOptions.maxLineLength).toBe(90); // Per EBNF implementation notes
            expect(defaultFormatterOptions.insertFinalNewline).toBe(true);
            expect(defaultFormatterOptions.trimTrailingWhitespace).toBe(true);
        });

        it("should have correct default values for spacing options", () => {
            expect(defaultFormatterOptions.insertSpacesAroundOperators).toBe(true);
            expect(defaultFormatterOptions.insertSpacesAfterCommas).toBe(true);
            expect(defaultFormatterOptions.insertSpacesAroundAssignmentOperators).toBe(true);
            expect(defaultFormatterOptions.insertSpacesAroundComparisonOperators).toBe(true);
            expect(defaultFormatterOptions.insertSpacesAroundLogicalOperators).toBe(true);
            expect(defaultFormatterOptions.insertSpacesAroundPropertyAccess).toBe(false); // SSL uses object:property
        });

        it("should have correct default values for line breaking", () => {
            expect(defaultFormatterOptions.preserveBlankLines).toBe(true);
            expect(defaultFormatterOptions.maxPreserveBlankLines).toBe(2);
            expect(defaultFormatterOptions.breakLongParameterLists).toBe(true);
            expect(defaultFormatterOptions.breakLongArrayLiterals).toBe(true);
            expect(defaultFormatterOptions.breakLongSqlStatements).toBe(true);
            expect(defaultFormatterOptions.parameterListBreakThreshold).toBe(4);
        });

        it("should have correct default values for SSL-specific options", () => {
            expect(defaultFormatterOptions.formatEmbeddedSql).toBe(true);
            expect(defaultFormatterOptions.alignSqlClauses).toBe(true);
            expect(defaultFormatterOptions.uppercaseKeywords).toBe(true);
            expect(defaultFormatterOptions.enforceHungarianNotation).toBe(false);
            expect(defaultFormatterOptions.alignEndOfLineComments).toBe(true);
            expect(defaultFormatterOptions.commentAlignmentColumn).toBe(60);
        });

        it("should have correct default values for control flow formatting", () => {
            expect(defaultFormatterOptions.blankLinesBeforeControlFlow).toBe(false);
            expect(defaultFormatterOptions.blankLinesAfterControlFlow).toBe(false);
            expect(defaultFormatterOptions.indentCaseStatements).toBe(true);
            expect(defaultFormatterOptions.alignProcedureParameters).toBe(true);
        });

        it("should have correct default values for comment formatting", () => {
            expect(defaultFormatterOptions.preserveRegionMarkers).toBe(true);
            expect(defaultFormatterOptions.formatMultiLineComments).toBe(true);
            expect(defaultFormatterOptions.wrapLongComments).toBe(true);
        });

        it("should have correct default values for array and object formatting", () => {
            expect(defaultFormatterOptions.alignArrayElements).toBe(true);
            expect(defaultFormatterOptions.insertTrailingCommas).toBe(false); // SSL doesn't support trailing commas
            expect(defaultFormatterOptions.breakObjectCreationCalls).toBe(true);
        });
    });

    describe("mergeFormatterOptions", () => {
        it("should return default options when no user options provided", () => {
            const result = mergeFormatterOptions();
            expect(result).toEqual(defaultFormatterOptions);
        });

        it("should merge user options with defaults", () => {
            const userOptions: Partial<FormatterOptions> = {
                indentSize: 2,
                useTabs: true,
                maxLineLength: 80,
                uppercaseKeywords: false,
            };

            const result = mergeFormatterOptions(userOptions);

            expect(result.indentSize).toBe(2);
            expect(result.useTabs).toBe(true);
            expect(result.maxLineLength).toBe(80);
            expect(result.uppercaseKeywords).toBe(false);
            // Should preserve defaults for non-specified options
            expect(result.insertFinalNewline).toBe(true);
            expect(result.preserveBlankLines).toBe(true);
        });

        it("should handle partial SSL-specific options", () => {
            const userOptions: Partial<FormatterOptions> = {
                formatEmbeddedSql: false,
                alignSqlClauses: false,
                commentAlignmentColumn: 80,
            };

            const result = mergeFormatterOptions(userOptions);

            expect(result.formatEmbeddedSql).toBe(false);
            expect(result.alignSqlClauses).toBe(false);
            expect(result.commentAlignmentColumn).toBe(80);
            // Should preserve other SSL defaults
            expect(result.uppercaseKeywords).toBe(true);
            expect(result.insertSpacesAroundPropertyAccess).toBe(false);
        });

        it("should handle empty object", () => {
            const result = mergeFormatterOptions({});
            expect(result).toEqual(defaultFormatterOptions);
        });
    });

    describe("validateFormatterOptions", () => {
        it("should return no warnings for valid default options", () => {
            const warnings = validateFormatterOptions(defaultFormatterOptions);
            expect(warnings).toEqual([]);
        });

        it("should warn about invalid indentSize", () => {
            const options = { ...defaultFormatterOptions, indentSize: 0 };
            const warnings = validateFormatterOptions(options);
            expect(warnings).toContain("indentSize should be between 1 and 8");

            const options2 = { ...defaultFormatterOptions, indentSize: 10 };
            const warnings2 = validateFormatterOptions(options2);
            expect(warnings2).toContain("indentSize should be between 1 and 8");
        });

        it("should warn about invalid maxLineLength", () => {
            const options = { ...defaultFormatterOptions, maxLineLength: 30 };
            const warnings = validateFormatterOptions(options);
            expect(warnings).toContain("maxLineLength should be between 40 and 200");

            const options2 = { ...defaultFormatterOptions, maxLineLength: 250 };
            const warnings2 = validateFormatterOptions(options2);
            expect(warnings2).toContain("maxLineLength should be between 40 and 200");
        });

        it("should warn about invalid maxPreserveBlankLines", () => {
            const options = { ...defaultFormatterOptions, maxPreserveBlankLines: -1 };
            const warnings = validateFormatterOptions(options);
            expect(warnings).toContain("maxPreserveBlankLines should be between 0 and 10");

            const options2 = { ...defaultFormatterOptions, maxPreserveBlankLines: 15 };
            const warnings2 = validateFormatterOptions(options2);
            expect(warnings2).toContain("maxPreserveBlankLines should be between 0 and 10");
        });

        it("should warn about invalid parameterListBreakThreshold", () => {
            const options = { ...defaultFormatterOptions, parameterListBreakThreshold: 0 };
            const warnings = validateFormatterOptions(options);
            expect(warnings).toContain("parameterListBreakThreshold should be between 1 and 20");

            const options2 = { ...defaultFormatterOptions, parameterListBreakThreshold: 25 };
            const warnings2 = validateFormatterOptions(options2);
            expect(warnings2).toContain("parameterListBreakThreshold should be between 1 and 20");
        });

        it("should warn about invalid commentAlignmentColumn", () => {
            const options = { ...defaultFormatterOptions, commentAlignmentColumn: 10 };
            const warnings = validateFormatterOptions(options);
            expect(warnings).toContain("commentAlignmentColumn should be between 20 and 120");

            const options2 = { ...defaultFormatterOptions, commentAlignmentColumn: 150 };
            const warnings2 = validateFormatterOptions(options2);
            expect(warnings2).toContain("commentAlignmentColumn should be between 20 and 120");
        });
        it("should warn about SSL-specific issues", () => {
            const options = { ...defaultFormatterOptions, insertSpacesAroundPropertyAccess: true };
            const warnings = validateFormatterOptions(options);
            expect(warnings).toContain(
                "insertSpacesAroundPropertyAccess=true is not recommended for SSL (object:property syntax should not have spaces)"
            );
        });
        it("should return multiple warnings for multiple issues", () => {
            const options: FormatterOptions = {
                ...defaultFormatterOptions,
                indentSize: 0,
                maxLineLength: 300,
                insertSpacesAroundPropertyAccess: true,
            };
            const warnings = validateFormatterOptions(options);
            expect(warnings.length).toBe(4);
            expect(warnings).toContain("indentSize should be between 1 and 8");
            expect(warnings).toContain("maxLineLength should be between 40 and 200");
            expect(warnings).toContain(
                "insertSpacesAroundPropertyAccess=true is not recommended for SSL (object:property syntax should not have spaces)"
            );
            expect(warnings).toContain(
                "SSL procedures often have deep nesting. Consider maxLineLength≤120 to prevent excessive line wrapping."
            );
        });
    });

    describe("createFormatterOptionsFromConfig", () => {
        it("should handle empty config", () => {
            const result = createFormatterOptionsFromConfig({});
            expect(result).toEqual(defaultFormatterOptions);
        });

        it("should map VS Code indentSize setting", () => {
            const config = { indentSize: 2 };
            const result = createFormatterOptionsFromConfig(config);
            expect(result.indentSize).toBe(2);
            expect(result.useTabs).toBe(false); // Should remain default
        });

        it("should map VS Code insertSpaces setting", () => {
            const config = { insertSpaces: false };
            const result = createFormatterOptionsFromConfig(config);
            expect(result.useTabs).toBe(true);
        });

        it("should map VS Code tabSize when using tabs", () => {
            const config = { insertSpaces: false, tabSize: 8 };
            const result = createFormatterOptionsFromConfig(config);
            expect(result.useTabs).toBe(true);
            expect(result.indentSize).toBe(8);
        });

        it("should map SSL-specific config", () => {
            const config = {
                ssl: {
                    uppercaseKeywords: false,
                    formatEmbeddedSql: false,
                    maxLineLength: 100,
                    commentAlignmentColumn: 80,
                },
            };
            const result = createFormatterOptionsFromConfig(config);
            expect(result.uppercaseKeywords).toBe(false);
            expect(result.formatEmbeddedSql).toBe(false);
            expect(result.maxLineLength).toBe(100);
            expect(result.commentAlignmentColumn).toBe(80);
        });

        it("should handle mixed VS Code and SSL config", () => {
            const config = {
                indentSize: 3,
                insertSpaces: true,
                ssl: {
                    uppercaseKeywords: false,
                    alignEndOfLineComments: false,
                },
            };
            const result = createFormatterOptionsFromConfig(config);
            expect(result.indentSize).toBe(3);
            expect(result.useTabs).toBe(false);
            expect(result.uppercaseKeywords).toBe(false);
            expect(result.alignEndOfLineComments).toBe(false);
            // Should preserve other defaults
            expect(result.insertFinalNewline).toBe(true);
        });

        it("should ignore invalid SSL config keys", () => {
            const config = {
                ssl: {
                    invalidKey: "invalidValue",
                    uppercaseKeywords: false,
                },
            };
            const result = createFormatterOptionsFromConfig(config);
            expect(result.uppercaseKeywords).toBe(false);
            expect((result as any).invalidKey).toBeUndefined();
        });
    });

    describe("SSL EBNF Grammar Compliance", () => {
        it("should support formatting options for all SSL constructs", () => {
            // Test that we have formatting options for key SSL language features
            expect(defaultFormatterOptions.uppercaseKeywords).toBeDefined(); // Keywords
            expect(defaultFormatterOptions.insertSpacesAroundPropertyAccess).toBeDefined(); // object:property
            expect(defaultFormatterOptions.formatEmbeddedSql).toBeDefined(); // SQL integration
            expect(defaultFormatterOptions.alignSqlClauses).toBeDefined(); // SQL formatting
            expect(defaultFormatterOptions.preserveRegionMarkers).toBeDefined(); // Region blocks
            expect(defaultFormatterOptions.indentCaseStatements).toBeDefined(); // Switch/case
            expect(defaultFormatterOptions.alignProcedureParameters).toBeDefined(); // Procedures
            expect(defaultFormatterOptions.alignEndOfLineComments).toBeDefined(); // Comments
        });

        it("should have SSL-appropriate defaults based on grammar", () => {
            // SSL uses colon for property access without spaces
            expect(defaultFormatterOptions.insertSpacesAroundPropertyAccess).toBe(false);

            // SSL keywords are traditionally uppercase
            expect(defaultFormatterOptions.uppercaseKeywords).toBe(true);

            // SSL doesn't support trailing commas
            expect(defaultFormatterOptions.insertTrailingCommas).toBe(false);

            // Line length per EBNF implementation notes
            expect(defaultFormatterOptions.maxLineLength).toBe(90);

            // SQL integration is important in SSL
            expect(defaultFormatterOptions.formatEmbeddedSql).toBe(true);
            expect(defaultFormatterOptions.alignSqlClauses).toBe(true);
        });

        it("should validate SSL-specific formatting rules", () => {
            // Test that validation catches SSL-specific issues
            const badOptions = {
                ...defaultFormatterOptions,
                insertSpacesAroundPropertyAccess: true, // Not recommended for SSL
            };
            const warnings = validateFormatterOptions(badOptions);
            expect(warnings.length).toBeGreaterThan(0);
            expect(warnings.some((w) => w.includes("insertSpacesAroundPropertyAccess"))).toBe(true);
        });
    });

    describe("Edge Cases", () => {
        it("should handle null and undefined values gracefully", () => {
            const config = {
                indentSize: null,
                ssl: {
                    uppercaseKeywords: undefined,
                    maxLineLength: null,
                },
            };
            const result = createFormatterOptionsFromConfig(config);
            // Should fall back to defaults for null/undefined values
            expect(result.indentSize).toBe(defaultFormatterOptions.indentSize);
            expect(result.uppercaseKeywords).toBe(defaultFormatterOptions.uppercaseKeywords);
            expect(result.maxLineLength).toBe(defaultFormatterOptions.maxLineLength);
        });
        it("should handle extreme but valid values", () => {
            const extremeOptions: FormatterOptions = {
                ...defaultFormatterOptions,
                indentSize: 1,
                maxLineLength: 40,
                maxPreserveBlankLines: 0,
                parameterListBreakThreshold: 1,
                commentAlignmentColumn: 20,
            };
            const warnings = validateFormatterOptions(extremeOptions);
            // Extreme values (while valid) trigger SSL-specific best practice warnings
            expect(warnings.length).toBe(2);
            expect(warnings).toContain(
                "Very short line lengths with SQL formatting may cause excessive line breaks in complex queries."
            );
            expect(warnings).toContain(
                "Comment wrapping with very short lines may fragment important documentation."
            );
        });
        it("should handle maximum valid values", () => {
            const maxOptions: FormatterOptions = {
                ...defaultFormatterOptions,
                indentSize: 8,
                maxLineLength: 200,
                maxPreserveBlankLines: 10,
                parameterListBreakThreshold: 20,
                commentAlignmentColumn: 120,
            };
            const warnings = validateFormatterOptions(maxOptions);
            // Maximum values (while valid) trigger SSL-specific best practice warnings
            expect(warnings.length).toBe(2);
            expect(warnings).toContain(
                "SSL procedures often have deep nesting. Consider maxLineLength≤120 to prevent excessive line wrapping."
            );
            expect(warnings).toContain(
                "parameterListBreakThreshold>10 may prevent line breaking. SSL procedures often have many parameters."
            );
        });
    });
});
