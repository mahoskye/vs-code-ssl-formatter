/**
 * Debug script to trace the SSL Formatting Provider step by step
 */

const fs = require('fs');
const path = require('path');

// Mock VS Code environment
const originalConsole = console;
global.console = {
    log: (...args) => originalConsole.log('[DEBUG]', ...args),
    error: (...args) => originalConsole.error('[ERROR]', ...args),
    warn: (...args) => originalConsole.warn('[WARN]', ...args)
};

// Mock vscode module
global.vscode = {
    TextEdit: {
        replace: (range, text) => ({ range, newText: text })
    },
    Range: class Range {
        constructor(startLine, startChar, endLine, endChar) {
            this.start = { line: startLine, character: startChar };
            this.end = { line: endLine, character: endChar };
        }
    },
    window: {
        showErrorMessage: (msg) => console.error('VS Code Error:', msg)
    }
};

try {
    // Load the compiled modules
    const formattingProviderPath = path.join(__dirname, '..', 'out', 'src', 'formatters', 'formattingProvider.js');

    if (!fs.existsSync(formattingProviderPath)) {
        console.error('ERROR: Compiled formatting provider not found. Run npm run compile first.');
        process.exit(1);
    }

    const { SSLFormattingProvider } = require(formattingProviderPath);

    console.log('✓ Successfully loaded SSLFormattingProvider');

    // Create provider instance
    const provider = new SSLFormattingProvider();
    console.log('✓ Created provider instance');
    console.log('Rules:', provider.rules?.map(r => r.name) || 'No rules found');

    // Test simple case that's failing
    const testText = "var1:=1";
    console.log(`\n=== Testing: "${testText}" ===`);

    // Mock document
    const mockDocument = {
        getText: () => testText,
        lineCount: 1,
        lineAt: (index) => ({ text: testText })
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

    // Try to format
    console.log('Calling provideDocumentFormattingEdits...');

    provider.provideDocumentFormattingEdits(mockDocument, mockOptions, mockToken)
        .then(edits => {
            console.log('✓ Formatting completed successfully');
            console.log('Edits:', edits);
            if (edits && edits.length > 0) {
                console.log('Formatted text:', JSON.stringify(edits[0].newText));
            }
        })
        .catch(error => {
            console.error('✗ Formatting failed:', error);
            console.error('Stack trace:', error.stack);

            // Try to trace the issue step by step
            console.log('\n=== Manual Step-by-Step Debug ===');

            try {
                // Test individual rules manually
                const lines = testText.split(/\r?\n/);
                console.log('Lines:', lines);

                for (let i = 0; i < lines.length; i++) {
                    const line = lines[i];
                    const trimmedLine = line.trim();
                    console.log(`\nLine ${i}: "${line}"`);
                    console.log(`Trimmed: "${trimmedLine}"`);

                    // Create context
                    const context = {
                        indentLevel: 0,
                        blockType: null,
                        previousLine: null,
                        nextLine: null,
                        lineNumber: i + 1,
                        options: {
                            tabSize: 4,
                            insertSpaces: true,
                            maxLineLength: 90,
                            indentStyle: "space"
                        }
                    };

                    console.log('Context:', context);

                    // Test each rule individually
                    let formattedLine = trimmedLine;
                    console.log(`Starting with: "${formattedLine}"`);

                    if (provider.rules) {
                        for (const rule of provider.rules) {
                            console.log(`\nApplying rule: ${rule.name}`);
                            console.log(`Input to rule: "${formattedLine}" (type: ${typeof formattedLine})`);

                            try {
                                const result = rule.apply(formattedLine, context);
                                console.log(`Result from rule: "${result}" (type: ${typeof result})`);

                                if (result === undefined || result === null) {
                                    console.error(`✗ Rule ${rule.name} returned ${result}!`);
                                    break;
                                }

                                formattedLine = result;
                            } catch (ruleError) {
                                console.error(`✗ Rule ${rule.name} threw error:`, ruleError);
                                break;
                            }
                        }
                    }

                    console.log(`Final formatted line: "${formattedLine}"`);
                }

            } catch (manualError) {
                console.error('Manual debug failed:', manualError);
            }
        });

} catch (loadError) {
    console.error('Failed to load modules:', loadError);
    console.log('\nTrying to compile first...');

    const { exec } = require('child_process');
    exec('npm run compile', { cwd: path.join(__dirname, '..') }, (error, stdout, stderr) => {
        if (error) {
            console.error('Compilation failed:', error);
            return;
        }
        console.log('Compilation output:', stdout);
        if (stderr) console.error('Compilation warnings:', stderr);

        console.log('Please run this debug script again after compilation completes.');
    });
}
