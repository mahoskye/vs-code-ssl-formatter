/**
 * Focused test to debug individual formatting rules
 */

// Create a simple test environment that doesn't depend on vscode
const assert = require('assert');

// Simplified mock environment  
function createMockFormattingContext() {
    return {
        indentLevel: 0,
        blockType: null,
        previousLine: null,
        nextLine: null,
        lineNumber: 1,
        options: {
            tabSize: 4,
            insertSpaces: true,
            maxLineLength: 90,
            indentStyle: "space"
        },
        ast: null
    };
}

// Manual implementation of OperatorSpacingRule for testing
class TestOperatorSpacingRule {
    name = "Test Operator Spacing";
    description = "Test version of operator spacing rule";

    apply(line, context) {
        console.log(`Input to OperatorSpacingRule: "${line}"`);

        let result = line;

        // Test assignment operator :=
        result = this.addSpacingAroundOperator(result, ":=");
        console.log(`After := operator: "${result}"`);

        return result;
    }

    addSpacingAroundOperator(text, operator) {
        console.log(`  addSpacingAroundOperator input: "${text}", operator: "${operator}"`);

        // Don't modify operators inside strings or comments
        if (this.isInsideStringOrComment(text, operator)) {
            console.log(`  Skipping - inside string or comment`);
            return text;
        }        // Escape special regex characters but keep the operator as a single unit
        const escapedOperator = operator.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
        console.log(`  Escaped operator: "${escapedOperator}"`);

        const regex = new RegExp(`\\s*${escapedOperator}\\s*`, "g");
        console.log(`  Regex: ${regex}`);

        // Replace with spaced operator
        let result = text.replace(regex, ` ${operator} `);
        console.log(`  After regex replace: "${result}"`);        // Clean up extra spaces
        result = result.replace(/\s{2,}/g, " ");
        console.log(`  After space cleanup: "${result}"`);

        return result;
    }

    isInsideStringOrComment(text, operator) {
        // Simplified version - just check for quotes
        const beforeOperator = text.substring(0, text.indexOf(operator));
        const singleQuotes = (beforeOperator.match(/'/g) || []).length;
        const doubleQuotes = (beforeOperator.match(/"/g) || []).length;

        const insideString = singleQuotes % 2 === 1 || doubleQuotes % 2 === 1;
        console.log(`  isInsideStringOrComment: ${insideString} (single: ${singleQuotes}, double: ${doubleQuotes})`);

        return insideString;
    }
}

// Test cases
const testCases = [
    { input: "var1:=1", expected: "var1 := 1" },
    { input: "a==b", expected: "a == b" },
    { input: "x+y", expected: "x + y" }
];

console.log("=== Testing Operator Spacing Rule ===\\n");

const rule = new TestOperatorSpacingRule();
const context = createMockFormattingContext();

testCases.forEach((testCase, index) => {
    console.log(`Test ${index + 1}: "${testCase.input}" -> expected: "${testCase.expected}"`);

    try {
        const result = rule.apply(testCase.input, context);
        console.log(`Result: "${result}"`);
        console.log(`Match: ${result === testCase.expected ? "✓" : "✗"}`);

        if (result !== testCase.expected) {
            console.log(`Expected: "${testCase.expected}"`);
            console.log(`Got:      "${result}"`);
        }
    } catch (error) {
        console.error(`Error in test ${index + 1}:`, error);
    }

    console.log("");
});
