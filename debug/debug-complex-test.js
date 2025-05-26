// Debug script to analyze the complex tokenizer test case
const { SSLTokenizer, TokenType } = require('../out/src/formatters/tokenizer');

const content = ':PROCEDURE MyProcedure:PARAMETERS param1,param2, param3 : DEFAULT param1 , "default" , param2 , 123;';
console.log('Testing complex content:', content);
console.log('Length:', content.length);

const tokenizer = new SSLTokenizer(content);
const tokens = tokenizer.tokenize();

console.log('\nTokens found:');
tokens.forEach((token, index) => {
    console.log(`${index}: ${token.type} - '${token.value}' at line ${token.position.line}, col ${token.position.column}, offset ${token.position.offset}`);
});

console.log('\nFocusing on the "default" string area:');
const relevantTokens = tokens.filter(token =>
    token.position.offset >= 70 && token.position.offset <= 90
);
relevantTokens.forEach((token, index) => {
    console.log(`  ${token.type} - '${token.value}' at col ${token.position.column}, offset ${token.position.offset}`);
});

console.log('\nExpected vs Actual for problematic tokens:');
console.log('Token 22 (offset 75): Expected INVALID, Actual:', tokens[22]?.type, '- Value:', tokens[22]?.value);
console.log('Token 23 (offset 76): Expected DEFAULT, Actual:', tokens[23]?.type, '- Value:', tokens[23]?.value);
console.log('Token 24 (offset 83): Expected INVALID, Actual:', tokens[24]?.type, '- Value:', tokens[24]?.value);
