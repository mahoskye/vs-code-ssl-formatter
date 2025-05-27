/**
 * Debug script to diagnose formatting provider issue
 * The error: TypeError: Cannot read properties of undefined (reading 'replace')
 * on line 70: return [vscode.TextEdit.replace(range, formattedText)];
 */

const fs = require('fs');
const path = require('path');

// Mock VS Code modules
const mockVSCode = {
    TextEdit: {
        replace: (range, text) => {
            console.log('TextEdit.replace called with:');
            console.log('  range:', range);
            console.log('  text type:', typeof text);
            console.log('  text value:', text);
            console.log('  text is undefined:', text === undefined);
            console.log('  text is null:', text === null);
            return { range, newText: text };
        }
    },
    Range: class {
        constructor(startLine, startChar, endLine, endChar) {
            this.start = { line: startLine, character: startChar };
            this.end = { line: endLine, character: endChar };
        }
    },
    window: {
        showErrorMessage: (msg) => console.error('VS Code Error:', msg)
    }
};

// Mock the vscode module
require.cache[require.resolve('vscode')] = {
    exports: mockVSCode
};

// Now load our modules
const { SSLFormattingProvider } = require('../src/formatters/formattingProvider.ts');

async function debugFormattingProvider() {
    console.log('=== SSL Formatting Provider Debug ===\n');

    const provider = new SSLFormattingProvider();
    
    // Simple test input that should work
    const testCases = [
        {
            name: 'Simple assignment',
            input: ':DECLARE sTest;\nsTest := "value";'
        },
        {
            name: 'Empty string',
            input: ''
        },
        {
            name: 'Single line',
            input: ':DECLARE sTest;'
        },
        {
            name: 'Multiple lines',
            input: ':DECLARE sTest;\nsTest := "value";\n:IF sTest == "value";\n    /* do something */;\n:ENDIF;'
        }
    ];

    for (const testCase of testCases) {
        console.log(`\n--- Testing: ${testCase.name} ---`);
        console.log('Input:', JSON.stringify(testCase.input));

        // Mock document
        const mockDocument = {
            getText: () => testCase.input,
            lineCount: testCase.input.split('\n').length,
            lineAt: (index) => ({
                text: testCase.input.split('\n')[index] || ''
            })
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

        try {
            console.log('Calling provideDocumentFormattingEdits...');
            const result = await provider.provideDocumentFormattingEdits(
                mockDocument,
                mockOptions,
                mockToken
            );
            
            console.log('Result type:', typeof result);
            console.log('Result is array:', Array.isArray(result));
            console.log('Result length:', result ? result.length : 'N/A');
            console.log('Result:', result);
            
        } catch (error) {
            console.error('Error occurred:', error);
            console.error('Error stack:', error.stack);
        }
    }

    // Test the formatText method directly if accessible
    console.log('\n--- Testing formatText directly (if accessible) ---');
    
    if (provider.formatText) {
        try {
            const directResult = await provider.formatText(':DECLARE sTest;', {
                tabSize: 4,
                insertSpaces: true,
                maxLineLength: 90,
                indentStyle: 'space'
            }, { isCancellationRequested: false });
            
            console.log('Direct formatText result type:', typeof directResult);
            console.log('Direct formatText result:', directResult);
        } catch (error) {
            console.error('Direct formatText error:', error);
        }
    } else {
        console.log('formatText method not accessible (private)');
    }
}

// Handle TypeScript compilation issues
try {
    debugFormattingProvider().catch(console.error);
} catch (error) {
    console.error('Failed to load modules:', error);
    console.log('\nNote: This script requires TypeScript compilation.');
    console.log('Try running: npm run compile');
}
