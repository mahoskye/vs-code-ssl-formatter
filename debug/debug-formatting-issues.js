// Debug script to diagnose formatting provider issues
const fs = require('fs');
const path = require('path');

// Load the modules directly
const tokenizerPath = path.join(__dirname, '../src/formatters/tokenizer.ts');
const parserPath = path.join(__dirname, '../src/formatters/parser.ts');
const formatterPath = path.join(__dirname, '../src/formatters/formattingProvider.ts');

console.log('=== SSL Formatting Debug Script ===\n');

// Test simple assignment formatting
const testCode = 'a:=1;';
console.log('Input code:', JSON.stringify(testCode));

// We need to compile TypeScript first or use ts-node
console.log('\nChecking if TypeScript files exist...');
console.log('Tokenizer exists:', fs.existsSync(tokenizerPath));
console.log('Parser exists:', fs.existsSync(parserPath));
console.log('Formatter exists:', fs.existsSync(formatterPath));

// Check if compiled JS files exist
const compiledTokenizerPath = path.join(__dirname, '../out/formatters/tokenizer.js');
const compiledParserPath = path.join(__dirname, '../out/formatters/parser.js');
const compiledFormatterPath = path.join(__dirname, '../out/formatters/formattingProvider.js');

console.log('\nChecking compiled JS files...');
console.log('Compiled tokenizer exists:', fs.existsSync(compiledTokenizerPath));
console.log('Compiled parser exists:', fs.existsSync(compiledParserPath));
console.log('Compiled formatter exists:', fs.existsSync(compiledFormatterPath));

if (fs.existsSync(compiledFormatterPath)) {
    try {
        console.log('\n=== Testing with compiled files ===');
        const { SSLFormattingProvider } = require(compiledFormatterPath);
        
        const provider = new SSLFormattingProvider();
        console.log('Provider created successfully');
        
        // Create a mock document
        const mockDocument = {
            getText: () => testCode,
            lineAt: (line) => ({ text: testCode }),
            lineCount: 1,
            uri: { fsPath: 'test.ssl' }
        };
        
        const options = {
            tabSize: 4,
            insertSpaces: true
        };
        
        console.log('\nCalling provideDocumentFormattingEdits...');
        const edits = provider.provideDocumentFormattingEdits(mockDocument, options);
        console.log('Edits returned:', JSON.stringify(edits, null, 2));
        
        if (edits && edits.length > 0) {
            console.log('\nFirst edit text:', JSON.stringify(edits[0].newText));
        } else {
            console.log('\nNo edits returned - this is the problem!');
        }
        
    } catch (error) {
        console.error('Error testing compiled formatter:', error);
    }
} else {
    console.log('\nCompiled files not found. Need to compile TypeScript first.');
    
    // Try to read the source files to understand the structure
    try {
        const formatterSource = fs.readFileSync(formatterPath, 'utf8');
        console.log('\nFormatter source file length:', formatterSource.length);
        
        // Look for key methods
        const hasProvideMethod = formatterSource.includes('provideDocumentFormattingEdits');
        const hasFormatMethod = formatterSource.includes('formatDocument');
        console.log('Has provideDocumentFormattingEdits method:', hasProvideMethod);
        console.log('Has formatDocument method:', hasFormatMethod);
        
        // Check for import statements
        const imports = formatterSource.match(/import.*from.*/g) || [];
        console.log('Import statements:', imports);
        
    } catch (error) {
        console.error('Error reading formatter source:', error);
    }
}

console.log('\n=== Debug complete ===');
