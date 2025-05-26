const { SSLParser, ASTNodeType } = require('../out/src/formatters/parser');
const { SSLTokenizer } = require('../out/src/formatters/tokenizer');

// Mock the SSLParser to add debug logging
class DebugSSLParser extends SSLParser {
    constructor(tokens) {
        super(tokens);
        this.debugDepth = 0;
    }

    log(message) {
        console.log('  '.repeat(this.debugDepth) + message);
    }

    parseStatement() {
        this.debugDepth++;
        this.log(`parseStatement() - current token: ${this.peek()?.type} "${this.peek()?.value}"`);
        const result = super.parseStatement();
        this.log(`parseStatement() result: ${result?.type || 'null'}`);
        this.debugDepth--;
        return result;
    }

    parseExpressionStatement() {
        this.debugDepth++;
        this.log(`parseExpressionStatement() - current token: ${this.peek()?.type} "${this.peek()?.value}"`);
        const result = super.parseExpressionStatement();
        this.log(`parseExpressionStatement() result: ${result?.type || 'null'}`);
        this.debugDepth--;
        return result;
    }

    parseExpression() {
        this.debugDepth++;
        this.log(`parseExpression() - current token: ${this.peek()?.type} "${this.peek()?.value}"`);
        const result = super.parseExpression();
        this.log(`parseExpression() result: ${result?.type || 'null'} "${result?.value || ''}"`);
        this.debugDepth--;
        return result;
    }

    parseAssignment() {
        this.debugDepth++;
        this.log(`parseAssignment() - current token: ${this.peek()?.type} "${this.peek()?.value}"`);
        const result = super.parseAssignment();
        this.log(`parseAssignment() result: ${result?.type || 'null'} "${result?.value || ''}"`);
        this.debugDepth--;
        return result;
    }

    parsePrimary() {
        this.debugDepth++;
        this.log(`parsePrimary() - current token: ${this.peek()?.type} "${this.peek()?.value}"`);
        const result = super.parsePrimary();
        this.log(`parsePrimary() result: ${result?.type || 'null'} "${result?.value || ''}"`);
        this.debugDepth--;
        return result;
    }

    // Access protected methods
    peek() {
        return this.tokens[this.current];
    }

    isAtEnd() {
        return this.current >= this.tokens.length || this.peek()?.type === 'EOF';
    }
}

function tokenize(text) {
    const tokenizer = new SSLTokenizer(text);
    return tokenizer.tokenize();
}

function parse(text) {
    const tokens = tokenize(text);
    console.log('Filtered tokens for parser:');
    const filteredTokens = tokens.filter(
        (token) => token.type !== 'WHITESPACE' && token.type !== 'NEWLINE'
    );
    filteredTokens.forEach((token, index) => {
        console.log(`  ${index}: ${token.type} "${token.value}"`);
    });

    const parser = new DebugSSLParser(filteredTokens);
    return parser.parse();
}

const code = `result1 := _AND(a, b);`;

console.log('=== DEBUGGING SINGLE ASSIGNMENT ===');
console.log('Input code:', code);
console.log('\n=== DETAILED PARSING TRACE ===');

try {
    const ast = parse(code);
    console.log('\n=== FINAL RESULT ===');
    console.log('AST children count:', ast.children.length);
    console.log('Expected: 1, Actual:', ast.children.length);

} catch (error) {
    console.error('Parsing failed:', error);
    console.error('Stack:', error.stack);
}
