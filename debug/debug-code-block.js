const { SSLParser: sslParser } = require("../out/src/formatters/parser"); // Renamed to SslParser
const { SSLTokenizer: sslTokenizer } = require('../out/src/formatters/tokenizer'); // Renamed to SslTokenizer

// Test case for CodeBlockLiteral
const code = `
    val1 := {|x| x*x};
    val2 := {| | 1+1};
    val3 := {|| 2*2};
`;

console.log("Tokenizing the code...");
const tokenizerInstance = new sslTokenizer(code); // Use renamed SslTokenizer
const tokens = tokenizerInstance.tokenize();
// Filter out whitespace and newline tokens for clarity in debugging the parser
const filteredTokens = tokens.filter(token => token.type !== 'WHITESPACE' && token.type !== 'NEWLINE');
console.log("Filtered Tokens:", JSON.stringify(filteredTokens, null, 2));


console.log("\nParsing the tokens...");
try {
    const parserInstance = new sslParser(filteredTokens); // Use renamed SslParser
    const ast = parserInstance.parse();
    console.log("AST:", JSON.stringify(ast, null, 2));

    if (ast && ast.children && ast.children.length > 0) {
        ast.children.forEach((statement, index) => {
            console.log(`\nDetails for statement ${index + 1}:`);
            console.log("  Statement Type:", statement.type); // Should be LogicStatement
            if (statement.children && statement.children[0]) {
                console.log("    First Child Type:", statement.children[0].type); // Should be Assignment
                if (statement.children[0].children && statement.children[0].children[1]) {
                    const expressionNode = statement.children[0].children[1];
                    console.log("      Expression Node Type:", expressionNode.type); // Should be Expression
                    if (expressionNode.children && expressionNode.children[0]) {
                        const primaryNode = expressionNode.children[0];
                        console.log("        Primary Node Type:", primaryNode.type); // Should be Primary
                        if (primaryNode.children && primaryNode.children[0]) {
                            console.log("          Literal Node Type:", primaryNode.children[0].type); // Should be CodeBlockLiteral
                            console.log("          Literal Node Value:", primaryNode.children[0].value);
                        } else {
                            console.log("          Literal Node not found or not structured as expected.");
                        }
                    } else {
                        console.log("        Primary Node not found or not structured as expected.");
                    }
                } else {
                    console.log("      Expression Node not found or not structured as expected.");
                }
            } else {
                console.log("    First Child not found or not structured as expected.");
            }
        });
    } else {
        console.log("AST is empty or not structured as expected.");
    }

} catch (error) {
    console.error("Error during parsing:", error);
}
