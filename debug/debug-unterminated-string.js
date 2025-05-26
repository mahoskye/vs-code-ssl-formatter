const { SSLTokenizer } = require('../out/src/formatters/tokenizer');

function tokenize(text) {
    const tokenizer = new SSLTokenizer(text);
    return tokenizer.tokenize();
}

console.log("=== Testing Unterminated String ===");

const tokens = tokenize('"this is not closed');
console.log("Tokens:");
tokens.forEach((token, idx) => {
    console.log(`  ${idx}: ${token.type} - "${token.value}"`);
});
