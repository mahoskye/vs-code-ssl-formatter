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

console.log('=== Debug Object Creation Test ===');

const code = `
                obj := CreateUDObject("ClassName");
                expandoObj := CreateUDObject();
            `;

console.log('Code to parse:');
console.log(code);

console.log('\n=== Tokenization ===');
try {
    const tokens = tokenize(code);
    console.log('Tokens:');
    tokens.forEach((token, index) => {
        if (token.type !== 'whitespace' && token.type !== 'newline') {
            console.log(`${index}: ${token.type} = "${token.value}" (${token.startPos}-${token.endPos})`);
        }
    });
} catch (error) {
    console.error('Tokenization error:', error.message);
}

console.log('\n=== Parsing ===');
try {
    const ast = parse(code);
    console.log('AST structure:');
    console.log(`Root children count: ${ast.children.length}`);

    ast.children.forEach((child, index) => {
        console.log(`Child ${index}:`, {
            type: child.type,
            value: child.value,
            childrenCount: child.children ? child.children.length : 0
        });

        if (child.children) {
            child.children.forEach((grandchild, gindex) => {
                console.log(`  Grandchild ${gindex}:`, {
                    type: grandchild.type,
                    value: grandchild.value,
                    childrenCount: grandchild.children ? grandchild.children.length : 0
                });
            });
        }
    });

    console.log('\nFull AST:');
    console.log(JSON.stringify(ast, null, 2));
} catch (error) {
    console.error('Parsing error:', error.message);
    console.error(error.stack);
}
