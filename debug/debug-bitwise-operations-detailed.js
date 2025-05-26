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
                result1 := _AND(a, b);
                result2 := _OR(x, y);
                result3 := _NOT(flag);
            `;

console.log('=== DEBUGGING BITWISE OPERATIONS TEST ===');
console.log('Input code:');
console.log(code);
console.log('\n=== PARSING RESULT ===');

try {
    const ast = parse(code);
    console.log('AST children count:', ast.children.length);
    console.log('Expected: 3, Actual:', ast.children.length);

    console.log('\n=== AST STRUCTURE ===');
    console.log(JSON.stringify(ast, null, 2));

    console.log('\n=== CHILDREN SUMMARY ===');
    ast.children.forEach((child, index) => {
        console.log(`Child ${index}: Type=${child.type}, Value="${child.value}", Children=${child.children?.length || 0}`);
        if (child.children) {
            child.children.forEach((grandchild, gIndex) => {
                console.log(`  Child ${index}.${gIndex}: Type=${grandchild.type}, Value="${grandchild.value}", Children=${grandchild.children?.length || 0}`);
            });
        }
    });

} catch (error) {
    console.error('Parsing failed:', error);
    console.error('Stack:', error.stack);
}
