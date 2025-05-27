const fs = require('fs');

// Read the compiled JavaScript file to check for syntax issues
const formattingProviderPath = 'out/src/formatters/formattingProvider.js';

try {
    const content = fs.readFileSync(formattingProviderPath, 'utf8');

    // Look for the problematic area around the context object
    const lines = content.split('\n');
    const contextAreaStart = lines.findIndex(line => line.includes('ast: ast'));

    if (contextAreaStart !== -1) {
        console.log('Context area found at line:', contextAreaStart + 1);
        console.log('Lines around context area:');
        for (let i = Math.max(0, contextAreaStart - 3); i < Math.min(lines.length, contextAreaStart + 8); i++) {
            console.log(`${i + 1}: ${lines[i]}`);
        }
    } else {
        console.log('Context area not found');
    }

    // Try to eval the file to see if there are syntax errors
    try {
        new Function(content);
        console.log('✓ No syntax errors detected in compiled JS');
    } catch (syntaxError) {
        console.log('✗ Syntax error detected:', syntaxError.message);
    }

} catch (error) {
    console.error('Error reading file:', error.message);
}
