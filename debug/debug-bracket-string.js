const { SSLTokenizer } = require('../out/src/formatters/tokenizer');

function tokenize(text) {
    const tokenizer = new SSLTokenizer(text);
    return tokenizer.tokenize();
}

console.log('=== DEBUGGING BRACKET STRING LITERALS ===');

const testCases = [
    '[hello world]',
    '[This is a bracket string]',
    '[String with spaces and punctuation!]',
    'identifier[arrayAccess]',
    'func()[index]',
    'array[0][1]'
];

testCases.forEach((testCase, index) => {
    console.log(`\nTest ${index + 1}: ${testCase}`);
    const tokens = tokenize(testCase);
    console.log('Tokens:');
    tokens.forEach((token, idx) => {
        console.log(`  ${idx}: ${token.type} - "${token.value}"`);
    });
});

console.log('\n=== Context-aware bracket handling ===');
console.log('Testing how brackets are interpreted based on context...');

const contextTests = [
    { code: '[standalone string]', expected: 'STRING_LITERAL' },
    { code: 'myArray[index]', expected: 'LBRACKET' },
    { code: 'func()[result]', expected: 'LBRACKET' },
    { code: 'arr[0][1]', expected: 'LBRACKET' }
];

contextTests.forEach((test, index) => {
    console.log(`\nContext Test ${index + 1}: ${test.code}`);
    console.log(`Expected first bracket: ${test.expected}`);
    const tokens = tokenize(test.code);
    const bracketToken = tokens.find(t => t.type === 'LBRACKET' || t.type === 'STRING_LITERAL');
    if (bracketToken) {
        console.log(`Actual: ${bracketToken.type} - "${bracketToken.value}"`);
        console.log(`Match: ${bracketToken.type === test.expected ? '✅' : '❌'}`);
    } else {
        console.log('No bracket token found');
    }
});