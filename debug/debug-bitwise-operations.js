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

function printAST(node, indent = 0) {
    console.log(`${" ".repeat(indent)}Type: ${node.type}, Value: ${node.value || ""}, Line: ${node.line || "?"}`);
    if (node.children) {
        node.children.forEach((child) => printAST(child, indent + 2));
    }
}

console.log("=== Debugging Bitwise Operations Test ===");

const code = `
    result1 := _AND(a, b);
    result2 := _OR(x, y);
    result3 := _NOT(flag);
`;

console.log("Code to parse:");
console.log(code);

const ast = parse(code);
console.log(`\nAST children count: ${ast.children.length}`);
console.log("Expected: 3");
console.log("Actual:", ast.children.length);

console.log("\nFull AST structure:");
printAST(ast);

console.log("\n=== Analyzing each statement ===");
ast.children.forEach((child, index) => {
    console.log(`\nStatement ${index + 1}:`);
    printAST(child, 2);
});

console.log("\n=== Token analysis ===");
const tokens = tokenize(code);
console.log("Tokens:");
tokens.forEach((token, index) => {
    console.log(`${index}: ${token.type} - "${token.value}"`);
});
