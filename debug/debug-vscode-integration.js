// Debug script to test the exact VS Code integration logic

const path = require('path');

// Use the existing comprehensive VS Code mock
const mockVscode = require('../out/test/__mocks__/vscode');

// Mock the module system BEFORE any other requires
const Module = require('module');
const originalRequire = Module.prototype.require;
const originalResolve = require.resolve;

// Override require.resolve to handle vscode module
require.resolve = function (id) {
    if (id === 'vscode') {
        return 'vscode'; // Return a fake path
    }
    return originalResolve.apply(this, arguments);
};

Module.prototype.require = function (id) {
    if (id === 'vscode') {
        console.log('VS Code module requested - returning mock');
        return mockVscode.vscode || mockVscode;
    }
    return originalRequire.apply(this, arguments);
};

// Set up the mock in require cache
const vscodeMock = mockVscode.vscode || mockVscode;
require.cache['vscode'] = { exports: vscodeMock };

// Set global mock
global.vscode = vscodeMock;

// Now import the formatting provider
const { SSLFormattingProvider } = require('../out/src/formatters/formattingProvider');

// Create a mock cancellation token
const mockCancellationToken = vscodeMock.CancellationTokenSource ?
    new vscodeMock.CancellationTokenSource().token :
    { isCancellationRequested: false };

// Create a mock TextDocument using a simpler approach
function createMockDocument(text) {
    return {
        getText: () => text,
        lineAt: (line) => ({
            text: text.split('\n')[line] || '',
            range: { start: { line, character: 0 }, end: { line, character: text.split('\n')[line]?.length || 0 } }
        }),
        lineCount: text.split('\n').length,
        uri: { fsPath: 'test.ssl' },
        fileName: 'test.ssl',
        languageId: 'ssl',
        version: 1,
        isDirty: false,
        isClosed: false,
        save: () => Promise.resolve(true),
        eol: 1, // LF
        positionAt: (offset) => ({ line: 0, character: offset }),
        offsetAt: (position) => position.character,
        validateRange: (range) => range,
        validatePosition: (position) => position
    };
}

// Create formatting options exactly like the test
const mockOptions = {
    tabSize: 4,
    insertSpaces: true,
    maxLineLength: 80,
    indentStyle: "space"
};

async function testFormattingIntegration() {
    console.log("=".repeat(60));
    console.log("Initializing SSL Formatting Provider");
    console.log("=".repeat(60));

    try {
        const provider = new SSLFormattingProvider();
        console.log("✓ Provider initialized successfully");

        // Log provider properties for debugging
        console.log("Provider properties:", Object.getOwnPropertyNames(provider));
        console.log("Provider prototype:", Object.getOwnPropertyNames(Object.getPrototypeOf(provider)));

        const testCases = [
            { input: "a:=1", expected: "a := 1", description: "Assignment operator spacing" },
            { input: "x==y", expected: "x == y", description: "Equality operator spacing" },
            { input: "a+b", expected: "a + b", description: "Addition operator spacing" },
            { input: ":PROCEDURE TestProc;\nvar1 := 1;\n:ENDPROC;", expected: ":PROCEDURE TestProc;\n    var1 := 1;\n:ENDPROC;", description: "Procedure indentation" }
        ];

        for (const testCase of testCases) {
            console.log("\n" + "=".repeat(60));
            console.log(`Testing: ${testCase.description}`);
            console.log("=".repeat(60));

            try {
                const mockDocument = createMockDocument(testCase.input);
                console.log("Input:", JSON.stringify(testCase.input));
                console.log("Document created:", !!mockDocument);

                const edits = await provider.provideDocumentFormattingEdits(
                    mockDocument,
                    mockOptions,
                    mockCancellationToken
                );

                console.log("Edits received:", edits?.length || 0);

                if (edits && edits.length > 0) {
                    const actualOutput = edits[0].newText;
                    console.log("Actual output:", JSON.stringify(actualOutput));
                    console.log("Expected output:", JSON.stringify(testCase.expected));

                    const success = actualOutput === testCase.expected;
                    console.log(success ? "✓ SUCCESS" : "✗ FAILED");

                    if (!success) {
                        console.log("Difference analysis:");
                        const actualLines = actualOutput.split('\n');
                        const expectedLines = testCase.expected.split('\n');

                        for (let i = 0; i < Math.max(actualLines.length, expectedLines.length); i++) {
                            const actual = actualLines[i] || '';
                            const expected = expectedLines[i] || '';
                            if (actual !== expected) {
                                console.log(`  Line ${i + 1}:`);
                                console.log(`    Actual  : "${actual}"`);
                                console.log(`    Expected: "${expected}"`);
                            }
                        }
                    }
                } else {
                    console.log("✗ No edits returned!");
                    console.log("Document text:", mockDocument.getText());
                    console.log("Options:", mockOptions);
                    console.log("Token:", mockCancellationToken);
                }

            } catch (error) {
                console.error("✗ Error during formatting:", error.message);
                console.error("Stack:", error.stack);
            }
        }

    } catch (error) {
        console.error("✗ Failed to initialize provider:", error.message);
        console.error("Stack:", error.stack);
    }
}

// Additional debug function to test the formatting rules directly
async function testFormattingRulesDirectly() {
    console.log("\n" + "=".repeat(60));
    console.log("Testing Formatting Rules Directly");
    console.log("=".repeat(60));

    try {
        // Test if we can access the formatting provider's internal methods
        const provider = new SSLFormattingProvider();

        // Try to format a simple line
        const testInput = "a:=1";
        console.log("Direct formatting test input:", testInput);

        // If the provider has a format method, test it
        if (typeof provider.formatText === 'function') {
            const result = provider.formatText(testInput, mockOptions);
            console.log("Direct format result:", result);
        } else {
            console.log("No direct format method available");
        }

        // Test individual components if available
        console.log("Provider methods available:", Object.getOwnPropertyNames(provider));

    } catch (error) {
        console.error("Error in direct formatting test:", error);
    }
}

// Additional debug function to verify the mock setup
function verifyMockSetup() {
    console.log("\n" + "=".repeat(60));
    console.log("Verifying Mock Setup");
    console.log("=".repeat(60));

    console.log("VS Code mock available:", !!vscodeMock);
    console.log("Mock properties:", Object.keys(vscodeMock));
    console.log("Global vscode:", !!global.vscode);

    try {
        const testRequire = require('vscode');
        console.log("Required vscode module:", !!testRequire);
        console.log("Required module properties:", Object.keys(testRequire));
    } catch (error) {
        console.log("Error requiring vscode:", error.message);
    }
}

// Run verification first, then tests
console.log("Starting debug script...");
verifyMockSetup();

testFormattingIntegration()
    .then(() => testFormattingRulesDirectly())
    .catch(error => {
        console.error("Unhandled error:", error);
        console.error("Stack:", error.stack);
    });