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

console.log("=== Debugging Unary Expressions Test ===");

const code = `
    notFlag := .NOT. flag;
    negative := -value;
    positive := +value;
    bangFlag := !condition;
`;

console.log("Code to parse:");
console.log(code);

const ast = parse(code);
console.log(`\nAST children count: ${ast.children.length}`);
console.log("Expected: 4");
console.log("Actual:", ast.children.length);

console.log("\nFull AST structure:");
printAST(ast);

console.log("\n=== Analyzing each statement ===");
ast.children.forEach((child, index) => {
    console.log(`\nStatement ${index + 1}:`);
    printAST(child, 2);
});

console.log("\n=== Checking first unary expression (.NOT.) ===");
if (ast.children.length > 0) {
    const firstStmt = ast.children[0];
    console.log("First statement structure:");
    printAST(firstStmt, 0);
    
    // Navigate to the unary expression: children[0].children[1]
    if (firstStmt.children && firstStmt.children[0] && firstStmt.children[0].children) {
        const notExpr = firstStmt.children[0].children[1];
        console.log("\nExpected unary expression (.NOT.):");
        if (notExpr) {
            console.log(`Type: ${notExpr.type}`);
            console.log(`Value: ${notExpr.value}`);
            console.log("Expected type: unaryExpression");
            console.log("Expected value: .NOT.");
        } else {
            console.log("notExpr is undefined");
        }
    }
}

console.log("\n=== Token analysis ===");
const tokens = tokenize(code);
console.log("Tokens:");
tokens.forEach((token, index) => {
    console.log(`${index}: ${token.type} - "${token.value}"`);
});
