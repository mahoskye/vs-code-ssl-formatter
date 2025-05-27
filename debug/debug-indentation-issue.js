/**
 * Debug script to trace the indentation issue in SSL formatting provider
 */

console.log('=== Debug Indentation Issue in SSL Formatting Provider ===');

// Core classes
class FormattingContext {
    constructor(indentLevel, blockType, options, lineNumber) {
        this.indentLevel = indentLevel;
        this.blockType = blockType;
        this.options = options;
        this.lineNumber = lineNumber;
        this.previousLine = null;
        this.nextLine = null;
    }
}

class IndentationRule {
    constructor() {
        this.name = "Indentation";
        this.description = "Ensures consistent indentation for SSL blocks";
    }

    apply(line, context) {
        console.log(`IndentationRule - Input: "${line}", indentLevel: ${context.indentLevel}`);

        // Don't indent empty lines or comments at start of line
        if (line.trim() === "" || line.trim().startsWith("/*")) {
            return line.trim();
        }

        const indentString = context.options.insertSpaces
            ? " ".repeat(context.options.tabSize * context.indentLevel)
            : "\t".repeat(context.indentLevel);

        const result = indentString + line.trim();
        console.log(`IndentationRule - Result: "${result}"`);
        return result;
    }
}

// SSL Language Helper Functions
class SSLLanguageHelper {
    static isBlockStart(line) {
        const blockStarts = [
            ":procedure", ":if", ":while", ":for", ":begincase",
            ":try", ":error", ":region", ":class", ":begininlinecode"
        ];
        return blockStarts.some(keyword => line.toLowerCase().startsWith(keyword));
    }

    static isBlockEnd(line) {
        const blockEnds = [
            ":endproc", ":endif", ":endwhile", ":next", ":endcase",
            ":endtry", ":endregion", ":endinlinecode"
        ];
        return blockEnds.some(keyword => line.toLowerCase().startsWith(keyword));
    }

    static isIntermediateBlock(line) {
        const intermediates = [":else", ":case", ":otherwise", ":catch", ":finally"];
        return intermediates.some(keyword => line.toLowerCase().startsWith(keyword));
    }

    static getBlockType(line) {
        const lowercaseLine = line.toLowerCase();
        if (lowercaseLine.startsWith(":procedure")) { return "PROCEDURE"; }
        if (lowercaseLine.startsWith(":if")) { return "IF"; }
        // Add more types as needed
        return "UNKNOWN";
    }
}

// Test Formatting Provider
class TestFormattingProvider {
    constructor() {
        this.indentationRule = new IndentationRule();
        this.options = { tabSize: 4, insertSpaces: true };
    }

    formatCode(input) {
        const lines = input.split('\n');
        const formattedLines = [];
        let currentIndentLevel = 0;

        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            const trimmedLine = line.trim();
            const lineNumber = i + 1;

            console.log(`\n[PROCESSING] Line ${lineNumber}: "${line}" (currentIndentLevel: ${currentIndentLevel})`);

            // Handle empty lines
            if (trimmedLine === "") {
                formattedLines.push("");
                console.log(`Line ${lineNumber} is empty. Skipping.`);
                continue;
            }

            // Calculate indent level for current line
            let lineIndentLevel = currentIndentLevel;

            // Block end keywords should be at one level less
            if (SSLLanguageHelper.isBlockEnd(trimmedLine)) {
                lineIndentLevel = Math.max(0, currentIndentLevel - 1);
            }

            console.log(`Line ${lineNumber}: "${trimmedLine}" -> calculated lineIndentLevel: ${lineIndentLevel}`);

            // Apply indentation
            const context = new FormattingContext(
                lineIndentLevel,
                SSLLanguageHelper.getBlockType(trimmedLine),
                this.options,
                lineNumber
            );

            const formattedLine = this.indentationRule.apply(trimmedLine, context);
            console.log(`Line ${lineNumber} after indentation: "${formattedLine}"`);

            formattedLines.push(formattedLine);

            // Update indentation level for next line
            if (SSLLanguageHelper.isBlockStart(trimmedLine)) {
                currentIndentLevel += 1;
            } else if (SSLLanguageHelper.isBlockEnd(trimmedLine)) {
                currentIndentLevel = Math.max(0, currentIndentLevel - 1);
            }

            console.log(`Line ${lineNumber} processed. Next currentIndentLevel: ${currentIndentLevel}`);
        }

        return formattedLines.join('\n');
    }
}

// Test Runner
class TestRunner {
    static runIndentationTest() {
        const testInput = `:PROCEDURE TestProc;
var1 := 1;
:ENDPROC;`;

        const expectedOutput = `:PROCEDURE TestProc;
    var1 := 1;
:ENDPROC;`;

        console.log('\n=== Test Input ===');
        console.log(JSON.stringify(testInput));

        console.log('\n=== Expected Output ===');
        console.log(JSON.stringify(expectedOutput));

        console.log('\n=== Running Test ===');
        const provider = new TestFormattingProvider();
        const actualOutput = provider.formatCode(testInput);

        console.log('\n=== Actual Output ===');
        console.log(JSON.stringify(actualOutput));

        console.log('\n=== Result Analysis ===');
        const testPassed = actualOutput === expectedOutput;
        console.log(`Test passed: ${testPassed}`);

        if (!testPassed) {
            this.analyzeFailure(actualOutput, expectedOutput);
        }

        return testPassed;
    }

    static analyzeFailure(actual, expected) {
        console.log('\n=== Detailed Comparison ===');
        const actualLines = actual.split('\n');
        const expectedLines = expected.split('\n');

        for (let i = 0; i < Math.max(actualLines.length, expectedLines.length); i++) {
            const actualLine = actualLines[i] || '';
            const expectedLine = expectedLines[i] || '';
            const match = actualLine === expectedLine ? '✓' : '✗';

            console.log(`${match} Line ${i + 1}:`);
            console.log(`  Actual  : "${actualLine}"`);
            console.log(`  Expected: "${expectedLine}"`);

            if (actualLine !== expectedLine) {
                console.log(`  Actual leading spaces  : ${actualLine.length - actualLine.trimStart().length}`);
                console.log(`  Expected leading spaces: ${expectedLine.length - expectedLine.trimStart().length}`);
            }
        }
    }
}

// Run the test
TestRunner.runIndentationTest();
