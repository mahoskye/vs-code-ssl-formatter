/**
 * Debug script to analyze the SSL Formatting Provider issues
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

// Load the source files by requiring the compiled JavaScript
try {
    const formattingProviderPath = path.join(__dirname, '..', 'out', 'src', 'formatters', 'formattingProvider.js');
    const tokenizerPath = path.join(__dirname, '..', 'out', 'src', 'formatters', 'tokenizer.js');
    const parserPath = path.join(__dirname, '..', 'out', 'src', 'formatters', 'parser.js');

    console.log('Loading compiled modules...');
    console.log('- formattingProvider:', formattingProviderPath);
    console.log('- tokenizer:', tokenizerPath);
    console.log('- parser:', parserPath);

    // Check if compiled files exist
    if (!fs.existsSync(formattingProviderPath)) {
        console.error('ERROR: Compiled formatting provider not found. Run npm run compile first.');
        process.exit(1);
    }

    // Simple test cases
    const testCases = [
        {
            name: "Simple assignment",
            input: "var1:=1",
            expected: "var1 := 1"
        },
        {
            name: "Simple procedure",
            input: ":procedure test\nvar1:=1\n:endproc",
            expected: ":PROCEDURE test\n    var1 := 1\n:ENDPROC"
        },
        {
            name: "Keywords",
            input: ":procedure test :parameters p1",
            expected: ":PROCEDURE test :PARAMETERS p1"
        }
    ];

    // Test each case with step-by-step analysis
    testCases.forEach((testCase, index) => {
        console.log(`\n=== Test Case ${index + 1}: ${testCase.name} ===`);
        console.log('Input:', JSON.stringify(testCase.input));
        console.log('Expected:', JSON.stringify(testCase.expected));

        // Analyze the input step by step
        const lines = testCase.input.split(/\r?\n/);
        console.log('Lines:', lines);

        // Check for keyword patterns
        lines.forEach((line, lineIndex) => {
            console.log(`Line ${lineIndex}: "${line}"`);
            console.log(`  Trimmed: "${line.trim()}"`);
            console.log(`  Starts with colon: ${line.trim().startsWith(':')})`);

            if (line.trim().startsWith(':')) {
                const keywordMatch = line.trim().match(/^:(\w+)\s*/);
                if (keywordMatch) {
                    console.log(`  Keyword match: "${keywordMatch[1]}"`);
                    console.log(`  Rest: "${line.trim().substring(keywordMatch[0].length)}"`);
                }
            }

            // Check for operators
            const operators = [':=', '==', '!=', '+', '-', '*', '/', '<', '>', '<=', '>='];
            operators.forEach(op => {
                if (line.includes(op)) {
                    console.log(`  Contains operator "${op}"`);
                }
            });
        });
    });

    // Try to analyze the actual formatting logic manually
    console.log('\n=== Manual Rule Analysis ===');

    const testLine = "var1:=1";
    console.log(`Testing line: "${testLine}"`);

    // Simulate OperatorSpacingRule logic
    console.log('\n--- Operator Spacing Rule Simulation ---');
    let result = testLine;
    const operator = ":=";

    console.log(`Original: "${result}"`);

    // Escape special regex characters
    const escapedOperator = operator.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    console.log(`Escaped operator: "${escapedOperator}"`);

    const regex = new RegExp(`\\s*${escapedOperator}\\s*`, "g");
    console.log(`Regex: ${regex}`);

    result = result.replace(regex, ` ${operator} `);
    console.log(`After replacement: "${result}"`);

    result = result.replace(/\s+/g, " ");
    console.log(`After space normalization: "${result}"`);

    // Simulate ColonSpacingRule logic
    console.log('\n--- Colon Spacing Rule Simulation ---');
    const testKeyword = ":procedure test";
    console.log(`Testing keyword: "${testKeyword}"`);

    const keywordMatch = testKeyword.match(/^:(\w+)\s*/);
    if (keywordMatch) {
        console.log(`Keyword match:`, keywordMatch);
        const keyword = keywordMatch[1];
        const rest = testKeyword.substring(keywordMatch[0].length);
        console.log(`Keyword: "${keyword}"`);
        console.log(`Rest: "${rest}"`);
        const formatted = `:${keyword.toUpperCase()}${rest ? " " + rest : ""}`;
        console.log(`Formatted: "${formatted}"`);
    }

} catch (error) {
    console.error('Error in debug script:', error);
    console.log('\nTrying to compile first...');

    // Try to run npm compile
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
