const { SSLTokenizer } = require('../out/src/formatters/tokenizer');

const code = `
                result1 := _AND(a, b);
                result2 := _OR(x, y);
                result3 := _NOT(flag);
            `;

console.log('=== DEBUGGING TOKENIZATION ===');
console.log('Input code:');
console.log(code);
console.log('\n=== TOKENS ===');

try {
    const tokenizer = new SSLTokenizer(code);
    const tokens = tokenizer.tokenize();
    
    console.log('Total tokens:', tokens.length);
    tokens.forEach((token, index) => {
        console.log(`Token ${index}: Type=${token.type}, Value="${token.value}", Line=${token.line}, Column=${token.column}`);
    });
    
} catch (error) {
    console.error('Tokenization failed:', error);
    console.error('Stack:', error.stack);
}
