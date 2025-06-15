const { Tokenizer } = require('./out/src/tokenizer');
const { Parser } = require('./out/src/parser');

// Test code block literal
const content = `dynamicCode := {|param1, param2|
    param1 + param2
};`;

console.log('Testing content:');
console.log(content);
console.log('');

console.log('Lines:');
content.split('\n').forEach((line, i) => console.log(`${i}: ${line}`));
console.log('');

const tokenizer = new Tokenizer();
const tokenResult = tokenizer.tokenize(content);

console.log('Tokenization result:');
console.log('hasErrors:', tokenResult.hasErrors);
console.log('tokens count:', tokenResult.tokens.length);

if (tokenResult.tokens.length > 0) {
    console.log('\nFirst 10 tokens:');
    tokenResult.tokens.slice(0, 10).forEach((token, i) => {
        console.log(`${i}: ${token.type} = "${token.value}" at line ${token.range.start.line}`);
    });

    const parser = new Parser(tokenResult.tokens);
    const parseResult = parser.parse();

    console.log('\nParse result:');
    console.log('success:', parseResult.success);

    if (parseResult.success && parseResult.ast) {
        console.log('AST root kind:', parseResult.ast.kind);
        console.log('AST body length:', parseResult.ast.body ? parseResult.ast.body.length : 'no body');

        if (parseResult.ast.body && parseResult.ast.body.length > 0) {
            const firstStatement = parseResult.ast.body[0];
            console.log('First statement kind:', firstStatement.kind);

            // If it's an assignment, check the right side
            if (firstStatement.kind === 'Assignment' && firstStatement.right) {
                console.log('Assignment right side kind:', firstStatement.right.kind);
                console.log('Assignment right side start line:', firstStatement.right.startToken?.range?.start?.line);
                console.log('Assignment right side end line:', firstStatement.right.endToken?.range?.end?.line);
            }
        }
    } else {
        console.log('Parse errors:', parseResult.errors);
    }
}
