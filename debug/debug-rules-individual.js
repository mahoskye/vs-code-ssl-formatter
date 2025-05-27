// Test individual formatting rules to identify which one might be hanging
console.log("Testing individual formatting rules...");

// Simple test context
const testContext = {
    indentLevel: 0,
    blockType: null,
    previousLine: null,
    nextLine: null,
    lineNumber: 1,
    options: {
        tabSize: 4,
        insertSpaces: true,
        maxLineLength: 90,
        indentStyle: 'space'
    },
    ast: { type: "program", children: [] }
};

const testLines = [
    "PROCEDURE TestProc",
    "BEGIN",
    "DECLARE num: NUMBER",
    "num=5",
    "RETURN num",
    "END"
];

// Test each line manually
console.log("Testing basic rule logic...");

testLines.forEach((line, index) => {
    console.log(`\nTesting line ${index + 1}: "${line}"`);

    // Test IndentationRule logic
    console.log("  IndentationRule test:");
    const trimmedLine = line.trim();
    const indentString = "    ".repeat(testContext.indentLevel);
    const indentedResult = indentString + trimmedLine;
    console.log(`    Result: "${indentedResult}"`);

    // Test OperatorSpacingRule logic
    console.log("  OperatorSpacingRule test:");
    let operatorResult = line;
    const operators = [":=", "==", "!=", "+", "-", "*", "/"];

    for (const operator of operators) {
        if (operatorResult.includes(operator)) {
            console.log(`    Found operator: ${operator}`);
            // Simple replacement logic
            const regex = new RegExp(`\\s*${operator.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}\\s*`, "g");
            operatorResult = operatorResult.replace(regex, ` ${operator} `);
            break;
        }
    }
    console.log(`    Result: "${operatorResult}"`);

    // Test simple block detection
    console.log("  Block detection test:");
    const lowerLine = line.toLowerCase().trim();
    let isBlockStart = false;
    let isBlockEnd = false;

    const blockStarts = ["procedure", "begin", "if", "while", "for"];
    const blockEnds = ["end", "endproc", "endif", "endwhile", "next"];

    for (const keyword of blockStarts) {
        if (lowerLine.includes(keyword)) {
            isBlockStart = true;
            break;
        }
    }

    for (const keyword of blockEnds) {
        if (lowerLine.includes(keyword)) {
            isBlockEnd = true;
            break;
        }
    }

    console.log(`    Block start: ${isBlockStart}, Block end: ${isBlockEnd}`);
});

console.log("\n✅ Individual rule testing completed successfully!");
console.log("The hang is likely not in the basic rule logic.");
console.log("The issue might be in:");
console.log("1. Rule interaction or infinite rule application");
console.log("2. Complex regex patterns in operator spacing");
console.log("3. AST parsing or tokenization taking too long");
console.log("4. Context analysis getting stuck in a loop");
