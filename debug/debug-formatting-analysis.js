/**
 * Simplified debug script to examine the formatting provider issue
 * Focus on understanding what's happening in the formatText method
 */

const fs = require('fs');
const path = require('path');

// Read the compiled JavaScript
const formattingProviderPath = path.join(__dirname, '..', 'out', 'src', 'formatters', 'formattingProvider.js');

console.log('=== SSL Formatting Provider Debug ===\n');

// Check if the compiled file exists
if (!fs.existsSync(formattingProviderPath)) {
    console.log('Compiled formattingProvider.js not found at:', formattingProviderPath);
    console.log('Try running: npm run compile');
    process.exit(1);
}

// Read and examine the compiled code
const compiledCode = fs.readFileSync(formattingProviderPath, 'utf8');

console.log('Compiled file found. Analyzing potential issues...\n');

// Look for the problematic line 70
const lines = compiledCode.split('\n');
console.log('=== Line 70 and surrounding context ===');
for (let i = 65; i < 75 && i < lines.length; i++) {
    const lineNum = i + 1;
    const prefix = lineNum === 70 ? '>>> ' : '    ';
    console.log(`${prefix}${lineNum}: ${lines[i]}`);
}

// Look for the formatText method
console.log('\n=== formatText method signature ===');
const formatTextMatch = compiledCode.match(/formatText\s*\([\s\S]*?\)\s*{/);
if (formatTextMatch) {
    console.log('Found formatText method signature:', formatTextMatch[0]);
} else {
    console.log('formatText method not found in compiled code');
}

// Look for return statements in formatText
console.log('\n=== Return statements in formatText ===');
const returnMatches = compiledCode.match(/return [^;]+;/g);
if (returnMatches) {
    returnMatches.forEach((match, index) => {
        console.log(`Return ${index + 1}: ${match}`);
    });
} else {
    console.log('No return statements found');
}

// Look for the join operation
console.log('\n=== Join operations ===');
const joinMatches = compiledCode.match(/[^.]*\.join\([^)]*\)/g);
if (joinMatches) {
    joinMatches.forEach((match, index) => {
        console.log(`Join ${index + 1}: ${match}`);
    });
} else {
    console.log('No join operations found');
}

// Look for async/await issues
console.log('\n=== Async/await patterns ===');
const asyncMatches = compiledCode.match(/async [^{]+{|await [^;]+;/g);
if (asyncMatches) {
    asyncMatches.forEach((match, index) => {
        console.log(`Async ${index + 1}: ${match}`);
    });
} else {
    console.log('No async/await patterns found');
}

// Check for formatting rules usage
console.log('\n=== Formatting rules application ===');
const ruleMatches = compiledCode.match(/rule\.apply\([^)]+\)/g);
if (ruleMatches) {
    ruleMatches.forEach((match, index) => {
        console.log(`Rule application ${index + 1}: ${match}`);
    });
} else {
    console.log('No rule applications found');
}

// Look for error handling in formatText
console.log('\n=== Error handling in formatText ===');
const tryBlocks = compiledCode.match(/try\s*{[\s\S]*?catch\s*\([^)]*\)\s*{[\s\S]*?}/g);
if (tryBlocks) {
    tryBlocks.forEach((block, index) => {
        const firstLine = block.split('\n')[0];
        console.log(`Try block ${index + 1}: ${firstLine}...`);
    });
} else {
    console.log('No try-catch blocks found');
}

console.log('\n=== Analysis Complete ===');
console.log('The issue appears to be that formatText is returning undefined.');
console.log('This could be due to:');
console.log('1. Missing return statement');
console.log('2. Async function not being awaited properly');
console.log('3. An exception being thrown and caught, returning undefined');
console.log('4. Formatting rules returning undefined/null values');
