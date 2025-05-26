// Debug script to test tokenizer behavior with special characters
const { SSLTokenizer, TokenType } = require('../out/src/formatters/tokenizer');

console.log('Testing tokenizer with special characters: @#$~');

const tokenizer = new SSLTokenizer("@#$~");
const tokens = tokenizer.tokenize();

console.log('Tokens found:');
tokens.forEach((token, index) => {
    console.log(`${index}: ${token.type} - '${token.value}' at line ${token.position.line}, col ${token.position.column}, offset ${token.position.offset}`);
});

console.log('\nExpected behavior according to EBNF grammar:');
console.log('@ should be INVALID (not in grammar)');
console.log('# should be INVALID (not in grammar)');
console.log('$ should be INVALID (not in grammar)');
console.log('~ should be INVALID (not in grammar)');

console.log('\nChecking individual characters:');
['@', '#', '$', '~'].forEach(char => {
    const charTokenizer = new SSLTokenizer(char);
    const charTokens = charTokenizer.tokenize();
    console.log(`${char}: ${charTokens[0].type} - '${charTokens[0].value}'`);
});
