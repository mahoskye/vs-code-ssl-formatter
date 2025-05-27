/**
 * Debug script for SSL indentation issue
 */

// Simple test case with a procedure block
const testInput = `:PROCEDURE TestProc;
var1 := 1;
:ENDPROC;`;

console.log('Test input:');
console.log(JSON.stringify(testInput));

const expected = `:PROCEDURE TestProc;
    var1 := 1;
:ENDPROC;`;

console.log('Expected output:');
console.log(JSON.stringify(expected));

// Indentation rule implementation (from formattingProvider.ts)
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

        // Use the context's indentLevel directly (it's already calculated correctly)
        const indentLevel = context.indentLevel;

        const indentString = context.options.insertSpaces
            ? " ".repeat(context.options.tabSize * indentLevel)
            : "\t".repeat(indentLevel);

        const result = indentString + line.trim();
        console.log(`IndentationRule - Result: "${result}"`);
        return result;
    }
}

// Helper functions
function isBlockStart(line) {
    const blockStarts = [
        ":procedure",
        ":if",
        ":while",
        ":for",
        ":begincase",
        ":try",
        ":error",
        ":region",
        ":class",
        ":begininlinecode",
    ];
    return blockStarts.some((keyword) => line.toLowerCase().startsWith(keyword));
}

function isBlockEnd(line) {
    const blockEnds = [
        ":endproc",
        ":endif",
        ":endwhile",
        ":next",
        ":endcase",
        ":endtry",
        ":endregion",
        ":endinlinecode",
    ];
    return blockEnds.some((keyword) => line.toLowerCase().startsWith(keyword));
}

function formatText(text) {
    const lines = text.split('\n');
    const formattedLines = [];
    let currentIndentLevel = 0;

    // Create rule instances
    const indentationRule = new IndentationRule();
    const colonSpacingRule = new ColonSpacingRule();

    // Process each line
    for (let i = 0; i < lines.length; i++) {
        const line = lines[i];
        const trimmedLine = line.trim();
        const lineNumber = i + 1;

        console.log(`\n[PROCESSING] Line ${lineNumber}: "${line}" (currentIndentLevel: ${currentIndentLevel})`);

        // Skip empty lines
        if (trimmedLine === "") {
            formattedLines.push("");
            console.log(`Line ${lineNumber} is empty. Skipping.`);
            continue;
        }

        // Calculate indent level for this line
        let lineIndentLevel = currentIndentLevel;

        // Block end keywords should be at one level less
        if (isBlockEnd(trimmedLine)) {
            lineIndentLevel = Math.max(0, currentIndentLevel - 1);
        }

        console.log(`Line ${lineNumber}: "${trimmedLine}" -> calculated lineIndentLevel: ${lineIndentLevel}`);

        // Create context
        const context = {
            indentLevel: lineIndentLevel,
            blockType: null,
            options: { tabSize: 4, insertSpaces: true }
        };

        // Apply rules in the same order as in the formatter
        let formattedLine = trimmedLine;

        // First apply indentation
        formattedLine = indentationRule.apply(formattedLine, context);
        console.log(`After IndentationRule: "${formattedLine}"`);

        // Then apply colon spacing
        formattedLine = colonSpacingRule.apply(formattedLine, context);
        console.log(`After ColonSpacingRule: "${formattedLine}"`);

        formattedLines.push(formattedLine);

        // Update indentation level for next line
        if (isBlockStart(trimmedLine)) {
            currentIndentLevel += 1;
            console.log(`Block start detected, increasing indent level to ${currentIndentLevel}`);
        } else if (isBlockEnd(trimmedLine)) {
            currentIndentLevel = Math.max(0, currentIndentLevel - 1);
            console.log(`Block end detected, decreasing indent level to ${currentIndentLevel}`);
        }
    }

    return formattedLines.join('\n');
}

// Run the test
console.log('\n=== Testing Indentation ===');
const result = formatText(testInput);

console.log('\n=== Result ===');
console.log(result);

console.log('\n=== Comparison ===');
console.log('Expected:', JSON.stringify(expected));
console.log('Actual  :', JSON.stringify(result));
console.log('Match   :', expected === result);

// Detailed comparison
if (expected !== result) {
    console.log('\n=== Line-by-line comparison ===');
    const expectedLines = expected.split('\n');
    const resultLines = result.split('\n');

    for (let i = 0; i < Math.max(expectedLines.length, resultLines.length); i++) {
        const expectedLine = expectedLines[i] || '';
        const resultLine = resultLines[i] || '';
        const match = expectedLine === resultLine ? '✓' : '✗';

        console.log(`${match} Line ${i + 1}:`);
        console.log(`  Expected: "${expectedLine}"`);
        console.log(`  Actual  : "${resultLine}"`);

        if (expectedLine !== resultLine) {
            console.log(`  Expected leading spaces: ${expectedLine.length - expectedLine.trimStart().length}`);
            console.log(`  Actual leading spaces  : ${resultLine.length - resultLine.trimStart().length}`);
        }
    }
}
