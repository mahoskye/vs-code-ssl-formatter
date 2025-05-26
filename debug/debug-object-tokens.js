const { SSLTokenizer } = require('../out/src/formatters/tokenizer');

const code = `obj := CreateUDObject("ClassName");`;

console.log('=== DEBUGGING OBJECT CREATION TOKENS ===');
console.log('Input code:', code);
console.log('\n=== TOKENS ===');

try {
    const tokenizer = new SSLTokenizer(code);
    const tokens = tokenizer.tokenize();
    
    console.log('Total tokens:', tokens.length);
    tokens.forEach((token, index) => {
        console.log(`Token ${index}: Type=${token.type}, Value="${token.value}"`);
    });
    
} catch (error) {
    console.error('Tokenization failed:', error);
}
