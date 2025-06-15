// Mock VSCode module for testing
const mockVSCode = {
    FoldingRangeKind: {
        Comment: 'comment',
        Region: 'region',
        Imports: 'imports'
    },
    FoldingRange: class {
        constructor(start, end, kind) {
            this.start = start;
            this.end = end;
            this.kind = kind;
        }
    }
};

// Replace vscode module
const Module = require('module');
const originalRequire = Module.prototype.require;
Module.prototype.require = function (id) {
    if (id === 'vscode') {
        return mockVSCode;
    }
    return originalRequire.apply(this, arguments);
};

// Now load our modules
const { Tokenizer } = require('./out/src/tokenizer');
const { Parser } = require('./out/src/parser');
const { SSLFoldingRangeProvider } = require('./out/src/providers/foldingRangeProvider');

// Mock TextDocument
class MockTextDocument {
    constructor(content) {
        this.content = content;
        this.lines = content.split('\n');
    }

    getText() {
        return this.content;
    }

    lineAt(line) {
        return {
            text: this.lines[line] || '',
            lineNumber: line
        };
    }

    get lineCount() {
        return this.lines.length;
    }
}

// Test content (consecutive single-line comments)
const content = `/* First comment line ;
/* Second comment line ;
/* Third comment line ;
/* Fourth comment line ;

someCode();`;

console.log('Testing content:');
console.log(content);
console.log('');

console.log('Lines:');
content.split('\n').forEach((line, i) => console.log(`${i}: "${line}"`));
console.log('');

// Test tokenization
const tokenizer = new Tokenizer();
const tokenResult = tokenizer.tokenize(content);

console.log('Tokenization result:');
console.log('hasErrors:', tokenResult.hasErrors);
console.log('tokens count:', tokenResult.tokens.length);

if (tokenResult.tokens.length > 0) {
    console.log('\nTokens (focusing on positions):');
    tokenResult.tokens.forEach((token, i) => {
        const displayValue = token.value.replace(/\n/g, '\\n');
        console.log(`${i}: ${token.type} = "${displayValue}" at line ${token.range.start.line}-${token.range.end.line}`);
    });

    // Test parsing
    const parser = new Parser(tokenResult.tokens);
    const parseResult = parser.parse();

    console.log('\nParse result:');
    console.log('success:', parseResult.success);

    if (parseResult.success && parseResult.ast) {
        console.log('AST root kind:', parseResult.ast.kind);
        console.log('AST body length:', parseResult.ast.body ? parseResult.ast.body.length : 'no body');

        if (parseResult.ast.body && parseResult.ast.body.length > 0) {
            const firstStatement = parseResult.ast.body[0];
            console.log('\nFirst statement details:');
            console.log('  kind:', firstStatement.kind);
            console.log('  start line:', firstStatement.startToken?.range?.start?.line);
            console.log('  end line:', firstStatement.endToken?.range?.end?.line);

            // If it's an assignment, check the right side
            if (firstStatement.kind === 'Assignment' && firstStatement.right) {
                console.log('\nAssignment right side (code block literal):');
                console.log('  kind:', firstStatement.right.kind);
                console.log('  start line:', firstStatement.right.startToken?.range?.start?.line);
                console.log('  end line:', firstStatement.right.endToken?.range?.end?.line);
                console.log('  start token:', firstStatement.right.startToken?.type + ' "' + firstStatement.right.startToken?.value + '"');
                console.log('  end token:', firstStatement.right.endToken?.type + ' "' + firstStatement.right.endToken?.value + '"');
                console.log('  multi-line?', firstStatement.right.endToken?.range?.end?.line > firstStatement.right.startToken?.range?.start?.line);
            }
        }

        // Test folding provider
        console.log('\n=== Testing Folding Provider ===');
        const document = new MockTextDocument(content);
        const provider = new SSLFoldingRangeProvider();

        try {
            const ranges = provider.provideFoldingRanges(document, {}, {});

            console.log('Folding ranges count:', ranges?.length || 0);
            if (ranges && ranges.length > 0) {
                ranges.forEach((range, i) => {
                    console.log(`Range ${i}: lines ${range.start}-${range.end}, kind: ${range.kind || 'default'}`);
                });
            } else {
                console.log('No folding ranges returned!');
            }
        } catch (error) {
            console.log('Error in folding provider:', error.message);
        }
    } else {
        console.log('Parse failed:', parseResult.errors);
    }
}
