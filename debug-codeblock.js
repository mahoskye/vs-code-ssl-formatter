const { Tokenizer } = require("./out/src/tokenizer");

const testCode = `dynamicCode := {|
    :DECLARE result;
    result := param1 + param2;
    :RETURN result;
|};`;

console.log("Testing code block literal:");
console.log(testCode);
console.log("\nTokens:");

const tokenizer = new Tokenizer();
const result = tokenizer.tokenize(testCode);

if (result.hasErrors) {
    console.log("Tokenization errors:", result.errors);
}

result.tokens.forEach((token, index) => {
    console.log(`${index}: ${token.type} - "${token.value}" - Line ${token.range.start.line}, Col ${token.range.start.character}`);
});
