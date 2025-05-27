/**
 * Simple test to debug operator spacing regex issue
 */

// Test the regex pattern directly
function testOperatorRegex(text, operator) {
    console.log(`\nTesting: "${text}" with operator "${operator}"`);

    // Escape special regex characters but keep the operator as a single unit
    const escapedOperator = operator.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    console.log(`Escaped operator: "${escapedOperator}"`);

    const regex = new RegExp(`\\s*${escapedOperator}\\s*`, "g");
    console.log(`Regex: ${regex}`);

    // Test if regex matches
    const matches = text.match(regex);
    console.log(`Matches found: ${matches ? matches.length : 0}`, matches);

    // Test replacement
    let result = text.replace(regex, ` ${operator} `);
    console.log(`After replacement: "${result}"`);

    // Clean up extra spaces
    result = result.replace(/\s{2,}/g, " ");
    console.log(`After cleanup: "${result}"`);

    return result;
}

// Test cases
const testCases = [
    { text: "var1:=1", operator: ":=" },
    { text: "a==b", operator: "==" },
    { text: "x+y", operator: "+" },
    { text: "a := b", operator: ":=" }, // Already spaced
    { text: "var1   :=   1", operator: ":=" }, // Multiple spaces
];

testCases.forEach(({ text, operator }) => {
    testOperatorRegex(text, operator);
});
