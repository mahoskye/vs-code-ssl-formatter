const { SSLParser, ASTNodeType } = require('../out/src/formatters/parser');
const { SSLTokenizer } = require('../out/src/formatters/tokenizer');

function tokenize(text) {
    const tokenizer = new SSLTokenizer(text);
    return tokenizer.tokenize();
}

function parse(text) {
    const tokens = tokenize(text);
    const parser = new SSLParser(tokens);
    return parser.parse();
}

const code = `
                /* This is a block comment;
                x := 1;
            `;

console.log('=== DEBUGGING BLOCK COMMENTS TEST ===');
console.log('Input code:');
console.log(JSON.stringify(code));
console.log('\n=== TOKENIZATION ===');

try {
    const tokens = tokenize(code);
    console.log('Tokens:');
    tokens.forEach((token, index) => {
        console.log(`${index}: ${token.type} = "${token.value}" (${token.startPos}-${token.endPos})`);
    });
} catch (error) {
    console.error('Tokenization error:', error.message);
}

console.log('\n=== PARSING RESULT ===');
try {
    const ast = parse(code);
    console.log('AST children count:', ast.children.length);
    console.log('Expected: 1, Actual:', ast.children.length);

    console.log('\n=== CHILDREN SUMMARY ===');
    ast.children.forEach((child, index) => {
        console.log(`Child ${index}: Type=${child.type}, Value="${child.value}", Children=${child.children?.length || 0}`);
        if (child.children) {
            child.children.forEach((grandchild, gIndex) => {
                console.log(`  Child ${index}.${gIndex}: Type=${grandchild.type}, Value="${grandchild.value}", Children=${grandchild.children?.length || 0}`);
            });
        }
    });

    console.log('\n=== FULL AST ===');
    console.log(JSON.stringify(ast, null, 2));
} catch (error) {
    console.error('Parsing failed:', error);
    console.error('Stack:', error.stack);
}
