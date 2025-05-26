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
                ExecUDF("MyFunction", paramArray);
            `;

console.log('=== DEBUGGING DYNAMIC CODE EXECUTION TEST ===');
console.log('Input code:');
console.log(code);
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
                if (grandchild.children) {
                    grandchild.children.forEach((greatgrandchild, ggIndex) => {
                        console.log(`    Child ${index}.${gIndex}.${ggIndex}: Type=${greatgrandchild.type}, Value="${greatgrandchild.value}"`);
                    });
                }
            });
        }
    });

    console.log('\n=== TOKENIZATION ===');
    const tokens = tokenize(code);
    console.log('Tokens (non-whitespace):');
    tokens.forEach((token, index) => {
        if (token.type !== 'whitespace' && token.type !== 'newline') {
            console.log(`${index}: ${token.type} = "${token.value}"`);
        }
    });

} catch (error) {
    console.error('Parsing failed:', error);
    console.error('Stack:', error.stack);
}
