const { SSLTokenizer } = require('../out/src/formatters/tokenizer');

function tokenize(text) {
    const tokenizer = new SSLTokenizer(text);
    return tokenizer.tokenize();
}

console.log("=== Debugging String Literal Parsing ===");

const testCases = [
    '"hello"',
    '"Expensive!"',
    "'single quotes'",
    '"hello world"',
    '"test with ! and @ symbols"'
];

testCases.forEach((testCase, index) => {
    console.log(`\nTest ${index + 1}: ${testCase}`);
    const tokens = tokenize(testCase);
    console.log("Tokens:");
    tokens.forEach((token, idx) => {
        console.log(`  ${idx}: ${token.type} - "${token.value}"`);
    });
});
