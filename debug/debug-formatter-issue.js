/**
 * Focused debug script to isolate the formatting issue
 */

console.log('=== Debug SSL Formatting Issue ===\n');

// Check if the compiled provider exists
const path = require('path');
const fs = require('fs');

const formattingProviderPath = path.join(__dirname, '../out/src/formatters/formattingProvider.js');

if (!fs.existsSync(formattingProviderPath)) {
    console.error('ERROR: Compiled formatting provider not found. Run npm run compile first.');
    process.exit(1);
}

console.log('✓ Found compiled formatting provider');

// Mock VS Code environment
const mockVscode = {
    Range: class {
        constructor(startLine, startChar, endLine, endChar) {
            this.start = { line: startLine, character: startChar };
            this.end = { line: endLine, character: endChar };
        }
    },
    TextEdit: {
        replace: (range, newText) => ({
            range: range,
            newText: newText
        })
    },
    window: {
        showErrorMessage: (msg) => console.error('VS Code Error:', msg)
    },
    CancellationTokenSource: class {
        constructor() {
            this.token = { isCancellationRequested: false };
        }
    },
    EndOfLine: { LF: 1 },
    Uri: {
        file: (path) => ({ path })
    }
};

// Mock the vscode module in Node's module cache before requiring
const Module = require('module');
const originalRequire = Module.prototype.require;

Module.prototype.require = function (id) {
    if (id === 'vscode') {
        return mockVscode;
    }
    return originalRequire.apply(this, arguments);
};

// Set up global mock as well
global.vscode = mockVscode;

// Now require the provider
const { SSLFormattingProvider } = require(formattingProviderPath);

console.log('✓ Successfully loaded SSLFormattingProvider');

async function testFormatter() {
    const provider = new SSLFormattingProvider();

    // Test input
    const testInput = `:PROCEDURE TestProc;
var1 := 1;
:ENDPROC;`;

    const expectedOutput = `:PROCEDURE TestProc;
    var1 := 1;
:ENDPROC;`;

    console.log('\n=== Test Input ===');
    console.log('Raw:', JSON.stringify(testInput));
    console.log('Display:');
    console.log(testInput);

    console.log('\n=== Expected Output ===');
    console.log('Raw:', JSON.stringify(expectedOutput));
    console.log('Display:');
    console.log(expectedOutput);

    // Create mock document
    const lines = testInput.split('\n');
    const mockDocument = {
        getText: () => testInput,
        lineCount: lines.length,
        lineAt: (lineNumber) => {
            const lineText = lines[lineNumber] || '';
            return {
                text: lineText,
                range: new mockVscode.Range(lineNumber, 0, lineNumber, lineText.length)
            };
        }
    };

    // Mock formatting options
    const mockOptions = {
        tabSize: 4,
        insertSpaces: true
    };

    // Mock cancellation token
    const mockToken = {
        isCancellationRequested: false
    };

    console.log('\n=== Calling Provider ===');

    try {
        const edits = await provider.provideDocumentFormattingEdits(
            mockDocument,
            mockOptions,
            mockToken
        );

        console.log('Edits returned:', edits ? edits.length : 'null/undefined');

        if (edits && edits.length > 0) {
            const edit = edits[0];
            console.log('Edit structure:', {
                hasRange: !!edit.range,
                hasNewText: !!edit.newText,
                newTextType: typeof edit.newText
            });

            console.log('\n=== Actual Output ===');
            console.log('Raw:', JSON.stringify(edit.newText));
            console.log('Display:');
            console.log(edit.newText);

            console.log('\n=== Comparison ===');
            const matches = edit.newText === expectedOutput;
            console.log('Matches expected:', matches);

            if (!matches) {
                console.log('\n--- Line by Line Comparison ---');
                const actualLines = edit.newText.split('\n');
                const expectedLines = expectedOutput.split('\n');

                for (let i = 0; i < Math.max(actualLines.length, expectedLines.length); i++) {
                    const actualLine = actualLines[i] || '';
                    const expectedLine = expectedLines[i] || '';
                    const match = actualLine === expectedLine ? '✓' : '✗';

                    console.log(`${match} Line ${i + 1}:`);
                    console.log(`  Actual  : "${actualLine}"`);
                    console.log(`  Expected: "${expectedLine}"`);

                    if (actualLine !== expectedLine) {
                        console.log(`  Actual spaces  : ${actualLine.length - actualLine.trimStart().length}`);
                        console.log(`  Expected spaces: ${expectedLine.length - expectedLine.trimStart().length}`);
                    }
                }
            }
        } else {
            console.log('✗ No edits returned');
        }

    } catch (error) {
        console.error('✗ Error occurred:', error);
        console.error('Stack:', error.stack);
    }
}

testFormatter();
