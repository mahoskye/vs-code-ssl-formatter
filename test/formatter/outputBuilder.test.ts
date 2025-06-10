/**
 * Tests for OutputBuilder
 *
 * Comprehensive test suite covering:
 * - Basic writing functionality
 * - Indentation management
 * - Blank line management
 * - SSL-specific formatting features
 * - Line wrapping and length management
 * - State management and snapshots
 */

import { OutputBuilder, OutputBuilderSnapshot } from "../../src/formatter/outputBuilder";
import { FormatterOptions, defaultFormatterOptions } from "../../src/formatter/options";

describe("OutputBuilder", () => {
    let builder: OutputBuilder;
    let options: FormatterOptions;

    beforeEach(() => {
        options = { ...defaultFormatterOptions };
        builder = new OutputBuilder(options);
    });

    describe("Basic Writing", () => {
        it("should write text without indentation", () => {
            builder.write("test");
            expect(builder.getOutput()).toBe("test\n");
        });

        it("should write multiple text segments on same line", () => {
            builder.write("hello");
            builder.write(" ");
            builder.write("world");
            expect(builder.getOutput()).toBe("hello world\n");
        });
        it("should write line with automatic line ending", () => {
            builder.writeLine("test line");
            expect(builder.getOutput()).toBe("test line\n");
        });

        it("should write line with indentation", () => {
            builder.indent();
            builder.writeLineIndented("indented line");
            expect(builder.getOutput()).toBe("    indented line\n");
        });

        it("should handle empty writeLine", () => {
            builder.writeLine();
            expect(builder.getOutput()).toBe("\n");
        });
    });

    describe("Indentation Management", () => {
        it("should start with zero indentation", () => {
            expect(builder.getIndentLevel()).toBe(0);
        });

        it("should increase indentation level", () => {
            builder.indent();
            expect(builder.getIndentLevel()).toBe(1);
            builder.indent();
            expect(builder.getIndentLevel()).toBe(2);
        });

        it("should decrease indentation level", () => {
            builder.indent();
            builder.indent();
            builder.dedent();
            expect(builder.getIndentLevel()).toBe(1);
        });

        it("should not allow negative indentation", () => {
            builder.dedent();
            expect(builder.getIndentLevel()).toBe(0);
        });

        it("should set specific indentation level", () => {
            builder.setIndentLevel(3);
            expect(builder.getIndentLevel()).toBe(3);
        });

        it("should not allow negative indentation when setting level", () => {
            builder.setIndentLevel(-1);
            expect(builder.getIndentLevel()).toBe(0);
        });

        it("should apply correct indentation with spaces", () => {
            builder.indent();
            builder.writeLineIndented("test");
            expect(builder.getOutput()).toBe("    test\n");
        });

        it("should apply correct indentation with tabs", () => {
            const tabOptions = { ...options, useTabs: true };
            const tabBuilder = new OutputBuilder(tabOptions);
            tabBuilder.indent();
            tabBuilder.writeLineIndented("test");
            expect(tabBuilder.getOutput()).toBe("\ttest\n");
        });

        it("should handle custom indent size", () => {
            const customOptions = { ...options, indentSize: 2 };
            const customBuilder = new OutputBuilder(customOptions);
            customBuilder.indent();
            customBuilder.writeLineIndented("test");
            expect(customBuilder.getOutput()).toBe("  test\n");
        });

        it("should apply indentation automatically with writeIndented", () => {
            builder.indent();
            builder.writeIndented("indented");
            builder.write(" text");
            expect(builder.getOutput()).toBe("    indented text\n");
        });
    });

    describe("Blank Line Management", () => {
        it("should add single blank line", () => {
            builder.writeLine("line1");
            builder.writeBlankLine();
            builder.writeLine("line2");
            expect(builder.getOutput()).toBe("line1\n\nline2\n");
        });
        it("should prevent consecutive blank lines", () => {
            builder.writeLine("line1");
            builder.writeBlankLine();
            builder.writeBlankLine();
            builder.writeLine("line2");
            expect(builder.getOutput()).toBe("line1\n\nline2\n");
        });
        it("should add multiple blank lines up to limit", () => {
            builder.writeLine("line1");
            builder.writeBlankLines(3);
            builder.writeLine("line2");
            expect(builder.getOutput()).toBe("line1\n\n\nline2\n");
        });
        it("should limit excessive blank lines in output", () => {
            // Test the blank line limiting through normal API
            builder.writeLine("line1");
            builder.writeBlankLines(5); // Request 5 blank lines
            builder.writeLine("line2");
            const result = builder.getOutput();
            // Should limit to 2 blank lines maximum
            expect(result).toBe("line1\n\n\nline2\n");
        });
        it("should force blank line even after existing blank line", () => {
            builder.writeLine("line1");
            builder.writeBlankLine();
            builder.forceBlankLine();
            builder.writeLine("line2");
            const lines = builder.getOutput().split("\n");
            expect(lines).toEqual(["line1", "", "", "line2", ""]);
        });
    });

    describe("SSL-Specific Formatting", () => {
        it("should write keywords in uppercase", () => {
            builder.writeKeyword("procedure");
            expect(builder.getOutput()).toBe("PROCEDURE\n");
        });
        it("should write keyword line in uppercase", () => {
            builder.writeKeywordLine("endproc");
            expect(builder.getOutput()).toBe("ENDPROC\n");
        });
        it("should write operators with proper spacing", () => {
            builder.writeOperator(":=", true, true);
            expect(builder.getOutput()).toBe(" :=\n");
        });

        it("should write operators without spacing when specified", () => {
            builder.writeOperator(":", false, false);
            expect(builder.getOutput()).toBe(":\n");
        });

        it("should write comma with space when enabled", () => {
            builder.write("param1");
            builder.writeComma();
            builder.write("param2");
            expect(builder.getOutput()).toBe("param1, param2\n");
        });

        it("should write comma without space when disabled", () => {
            const noSpaceOptions = { ...options, insertSpacesAfterCommas: false };
            const noSpaceBuilder = new OutputBuilder(noSpaceOptions);
            noSpaceBuilder.write("param1");
            noSpaceBuilder.writeComma();
            noSpaceBuilder.write("param2");
            expect(noSpaceBuilder.getOutput()).toBe("param1,param2\n");
        });

        it("should write semicolon (SSL statement terminator)", () => {
            builder.write("statement");
            builder.writeSemicolon();
            expect(builder.getOutput()).toBe("statement;\n");
        });

        it("should write various brackets and braces", () => {
            builder.writeOpenParen();
            builder.write("content");
            builder.writeCloseParen();
            builder.writeOpenBracket();
            builder.write("index");
            builder.writeCloseBracket();
            builder.writeOpenBrace();
            builder.write("array");
            builder.writeCloseBrace();
            expect(builder.getOutput()).toBe("(content)[index]{array}\n");
        });
    });

    describe("Conditional Writing", () => {
        it("should write space conditionally", () => {
            builder.writeSpaceIf(true);
            builder.write("text");
            builder.writeSpaceIf(false);
            expect(builder.getOutput()).toBe(" text\n");
        });

        it("should ensure space when not present", () => {
            builder.write("text");
            builder.ensureSpace();
            builder.write("more");
            expect(builder.getOutput()).toBe("text more\n");
        });

        it("should not add space when already present", () => {
            builder.write("text ");
            builder.ensureSpace();
            builder.write("more");
            expect(builder.getOutput()).toBe("text more\n");
        });
    });

    describe("Line State Queries", () => {
        it("should detect empty current line", () => {
            expect(builder.isCurrentLineEmpty()).toBe(true);
            builder.write("text");
            expect(builder.isCurrentLineEmpty()).toBe(false);
        });

        it("should detect start of line", () => {
            expect(builder.isAtStartOfLine()).toBe(true);
            builder.write("text");
            expect(builder.isAtStartOfLine()).toBe(false);
        });

        it("should detect start of line with indentation", () => {
            builder.indent();
            builder.writeIndented("");
            expect(builder.isAtStartOfLine()).toBe(true);
        });

        it("should get current line content", () => {
            builder.write("current");
            expect(builder.getCurrentLine()).toBe("current");
        });

        it("should get current line length", () => {
            builder.write("test");
            expect(builder.getCurrentLineLength()).toBe(4);
        });

        it("should detect if adding text would exceed max length", () => {
            const shortOptions = { ...options, maxLineLength: 10 };
            const shortBuilder = new OutputBuilder(shortOptions);
            shortBuilder.write("12345");
            expect(shortBuilder.wouldExceedMaxLength("67890")).toBe(false);
            expect(shortBuilder.wouldExceedMaxLength("678901")).toBe(true);
        });

        it("should detect if current line is too long", () => {
            const shortOptions = { ...options, maxLineLength: 5 };
            const shortBuilder = new OutputBuilder(shortOptions);
            shortBuilder.write("123");
            expect(shortBuilder.isCurrentLineTooLong()).toBe(false);
            shortBuilder.write("456");
            expect(shortBuilder.isCurrentLineTooLong()).toBe(true);
        });
    });

    describe("Line Management", () => {
        it("should clear current line", () => {
            builder.write("test");
            builder.clearCurrentLine();
            expect(builder.getCurrentLine()).toBe("");
        });

        it("should end line without adding text", () => {
            builder.write("test");
            builder.endLine();
            expect(builder.getOutput()).toBe("test\n");
        });

        it("should count lines correctly", () => {
            expect(builder.getLineCount()).toBe(0);
            builder.writeLine("line1");
            expect(builder.getLineCount()).toBe(1);
            builder.writeLine("line2");
            expect(builder.getLineCount()).toBe(2);
        });

        it("should count current line in total", () => {
            builder.write("current");
            expect(builder.getLineCount()).toBe(1);
        });

        it("should detect empty output", () => {
            expect(builder.isEmpty()).toBe(true);
            builder.write("text");
            expect(builder.isEmpty()).toBe(false);
        });
    });

    describe("Output Options", () => {
        it("should trim trailing whitespace when enabled", () => {
            const trimOptions = { ...options, trimTrailingWhitespace: true };
            const trimBuilder = new OutputBuilder(trimOptions);
            trimBuilder.writeLine("text   ");
            expect(trimBuilder.getOutput()).toBe("text\n");
        });
        it("should preserve trailing whitespace when disabled", () => {
            const noTrimOptions = { ...options, trimTrailingWhitespace: false };
            const noTrimBuilder = new OutputBuilder(noTrimOptions);
            noTrimBuilder.writeLine("text   ");
            expect(noTrimBuilder.getOutput()).toBe("text   \n");
        });

        it("should add final newline when enabled", () => {
            const finalNewlineOptions = { ...options, insertFinalNewline: true };
            const finalNewlineBuilder = new OutputBuilder(finalNewlineOptions);
            finalNewlineBuilder.write("text");
            expect(finalNewlineBuilder.getOutput()).toBe("text\n");
        });

        it("should not add final newline when disabled", () => {
            const noFinalNewlineOptions = { ...options, insertFinalNewline: false };
            const noFinalNewlineBuilder = new OutputBuilder(noFinalNewlineOptions);
            noFinalNewlineBuilder.write("text");
            expect(noFinalNewlineBuilder.getOutput()).toBe("text");
        });

        it("should not add final newline to empty content", () => {
            expect(builder.getOutput()).toBe("");
        });
    });

    describe("State Management", () => {
        it("should reset to initial state", () => {
            builder.writeLine("test");
            builder.indent();
            builder.writeBlankLine();
            builder.reset();

            expect(builder.isEmpty()).toBe(true);
            expect(builder.getIndentLevel()).toBe(0);
            expect(builder.getCurrentLine()).toBe("");
            expect(builder.getLineCount()).toBe(0);
        });

        it("should create and restore snapshots", () => {
            builder.writeLine("line1");
            builder.indent();
            builder.write("current");

            const snapshot = builder.createSnapshot();

            builder.writeLine("more");
            builder.indent();

            builder.restoreSnapshot(snapshot);

            expect(builder.getCurrentLine()).toBe("current");
            expect(builder.getIndentLevel()).toBe(1);
            expect(builder.getLineCount()).toBe(2); // includes current line
        });

        it("should create independent snapshot copies", () => {
            builder.writeLine("line1");
            const snapshot = builder.createSnapshot();

            // Modify original
            builder.writeLine("line2"); // Snapshot should be unchanged
            expect(snapshot.lines).toEqual(["line1"]);
            expect(snapshot.currentLine).toBe("");
        });
    });

    describe("Complex Formatting Scenarios", () => {
        it("should format SSL procedure declaration", () => {
            builder.writeKeywordLine(":PROCEDURE TestProc");
            builder.indent();
            builder.writeKeywordLine(":PARAMETERS sParam1, nParam2");
            builder.writeBlankLine();
            builder.writeLineIndented("sResult := sParam1;");
            builder.dedent();
            builder.writeKeywordLine(":ENDPROC");
            const expected = [
                ":PROCEDURE TESTPROC",
                "    :PARAMETERS SPARAM1, NPARAM2",
                "",
                "    sResult := sParam1;",
                ":ENDPROC",
                "",
            ].join("\n");

            expect(builder.getOutput()).toBe(expected);
        });

        it("should format SSL conditional statement", () => {
            builder.writeKeyword(":IF");
            builder.ensureSpace();
            builder.write("nValue > 0");
            builder.endLine();
            builder.indent();
            builder.writeLineIndented("DoSomething();");
            builder.dedent();
            builder.writeKeywordLine(":ELSE");
            builder.indent();
            builder.writeLineIndented("DoSomethingElse();");
            builder.dedent();
            builder.writeKeywordLine(":ENDIF");
            const expected = [
                ":IF nValue > 0",
                "    DoSomething();",
                ":ELSE",
                "    DoSomethingElse();",
                ":ENDIF",
                "",
            ].join("\n");

            expect(builder.getOutput()).toBe(expected);
        });
        it("should format SSL array literal with proper spacing", () => {
            builder.write("aValues");
            builder.writeOperator(":=");
            builder.writeOpenBrace();
            builder.write("1");
            builder.writeComma();
            builder.write("2");
            builder.writeComma();
            builder.write("3");
            builder.writeCloseBrace();
            builder.writeSemicolon();

            expect(builder.getOutput()).toBe("aValues := {1, 2, 3};\n");
        });

        it("should handle skipped parameters in SSL style", () => {
            builder.write("DoProc(");
            builder.write("param1");
            builder.write(",");
            builder.write(","); // Skipped parameter - no space after first comma per SSL style
            builder.write("param3");
            builder.write(")");
            builder.writeSemicolon();

            expect(builder.getOutput()).toBe("DoProc(param1,,param3);\n");
        });
    });

    describe("Edge Cases", () => {
        it("should handle multiple consecutive operations", () => {
            builder.writeKeyword("if");
            builder.writeSpaceIf(true);
            builder.writeOpenParen();
            builder.write("condition");
            builder.writeCloseParen();
            builder.endLine();

            expect(builder.getOutput()).toBe("IF (condition)\n");
        });

        it("should handle empty lines and whitespace correctly", () => {
            builder.writeLine("   "); // Line with only spaces
            builder.writeBlankLine();
            builder.writeLine("text");
            const lines = builder.getOutput().split("\n");
            expect(lines[0].trim()).toBe(""); // Should be trimmed if trimTrailingWhitespace is true
            expect(lines[1]).toBe(""); // Blank line
            expect(lines[2]).toBe("text");
        });

        it("should handle very long lines", () => {
            const longText = "a".repeat(100);
            builder.write(longText);
            expect(builder.isCurrentLineTooLong()).toBe(true);
            expect(builder.wouldExceedMaxLength("more")).toBe(true);
        });
    });
});
