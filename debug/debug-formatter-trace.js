/**
 * Enhanced debug script to trace SSL Formatting Provider rule failures
 */

const path = require('path');

// Mock VS Code environment
const originalConsole = console;
global.console = {
    log: (...args) => originalConsole.log('[DEBUG]', ...args),
    error: (...args) => originalConsole.error('[ERROR]', ...args),
    warn: (...args) => originalConsole.warn('[WARN]', ...args)
};

// Mock vscode module for Node.js testing
const mockVscode = {
    Range: class {
        constructor(start, startChar, end, endChar) {
            this.start = { line: start, character: startChar };
            this.end = { line: end, character: endChar };
        }
    },
    TextEdit: {
        replace: (range, text) => ({ range, text })
    },
    window: {
        showErrorMessage: (msg) => console.error('VS Code Error:', msg)
    }
};

global.vscode = mockVscode;

try {
    // Load the compiled formatter
    const formatterPath = path.join(__dirname, '..', 'out', 'src', 'formatters', 'formattingProvider.js');
    const { SSLFormattingProvider } = require(formatterPath);
    
    console.log('Successfully loaded SSLFormattingProvider');
    
    // Create provider instance
    const provider = new SSLFormattingProvider();
    console.log('Provider created successfully');
    
    // Test simple input step by step
    const testInput = "var1:=1";
    console.log(`\\nTesting input: "${testInput}"`);
    
    // Create mock document
    const mockDocument = {
        getText: () => testInput,
        lineCount: 1,
        lineAt: (index) => ({ text: testInput, lineNumber: index })
    };
    
    // Create mock formatting options
    const mockOptions = {
        tabSize: 4,
        insertSpaces: true
    };
    
    // Create mock cancellation token
    const mockToken = {
        isCancellationRequested: false
    };
    
    console.log('\\nCalling provideDocumentFormattingEdits...');
    
    // Call the formatter and trace the result
    provider.provideDocumentFormattingEdits(mockDocument, mockOptions, mockToken)
        .then(result => {
            console.log('\\n=== FORMATTING RESULT ===');
            console.log('Result type:', typeof result);
            console.log('Result length:', result ? result.length : 'undefined');
            
            if (result && result.length > 0) {
                console.log('First edit:', result[0]);
                console.log('Formatted text:', JSON.stringify(result[0].text));
            } else {
                console.log('No edits returned');
            }
        })
        .catch(error => {
            console.error('\\n=== FORMATTING ERROR ===');
            console.error('Error type:', error.constructor.name);
            console.error('Error message:', error.message);
            console.error('Error stack:', error.stack);
        });
        
} catch (error) {
    console.error('\\n=== SETUP ERROR ===');
    console.error('Failed to load or setup formatter:', error.message);
    console.error('Stack:', error.stack);
    
    // Check if compilation is needed
    const fs = require('fs');
    const outPath = path.join(__dirname, '..', 'out');
    if (!fs.existsSync(outPath)) {
        console.log('\\n=== COMPILATION NEEDED ===');
        console.log('The "out" directory does not exist. Please run: npm run compile');
    }
}
