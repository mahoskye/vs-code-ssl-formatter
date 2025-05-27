/**
 * Debug script to trace the formatText method step by step
 * This will help us identify where undefined is being returned
 */

const fs = require('fs');
const path = require('path');

// Read the compiled JavaScript to trace the execution
const compiledPath = path.join(__dirname, '..', 'out', 'src', 'formatters', 'formattingProvider.js');

if (!fs.existsSync(compiledPath)) {
    console.log('Compiled file not found. Running compile...');
    process.exit(1);
}

console.log('=== formatText Debug Analysis ===\n');

const compiledCode = fs.readFileSync(compiledPath, 'utf8');

// Look for the formatText method and its return statements
console.log('=== formatText method analysis ===');

// Find the start of formatText method
const formatTextStart = compiledCode.indexOf('formatText(');
if (formatTextStart === -1) {
    console.log('formatText method not found!');
    process.exit(1);
}

// Extract the method (roughly)
const methodStartBrace = compiledCode.indexOf('{', formatTextStart);
let braceCount = 1;
let methodEnd = methodStartBrace + 1;

while (braceCount > 0 && methodEnd < compiledCode.length) {
    if (compiledCode[methodEnd] === '{') braceCount++;
    if (compiledCode[methodEnd] === '}') braceCount--;
    methodEnd++;
}

const formatTextMethod = compiledCode.substring(formatTextStart, methodEnd);

console.log('Method signature:');
const firstLine = formatTextMethod.split('\n')[0];
console.log(firstLine);

console.log('\n=== Return statements in formatText ===');
const returnMatches = formatTextMethod.match(/return [^;]+;/g);
if (returnMatches) {
    returnMatches.forEach((match, index) => {
        console.log(`Return ${index + 1}: ${match}`);
    });
} else {
    console.log('No return statements found in formatText method!');
}

console.log('\n=== Checking for missing return statement ===');
const lastLines = formatTextMethod.split('\n').slice(-10);
console.log('Last 10 lines of formatText method:');
lastLines.forEach((line, index) => {
    console.log(`${lastLines.length - 10 + index + 1}: ${line}`);
});

console.log('\n=== Checking for undefined assignments ===');
const undefinedMatches = formatTextMethod.match(/= undefined|return undefined/g);
if (undefinedMatches) {
    undefinedMatches.forEach((match, index) => {
        console.log(`Undefined assignment ${index + 1}: ${match}`);
    });
} else {
    console.log('No explicit undefined assignments found');
}

console.log('\n=== Checking for potential async issues ===');
const awaitMatches = formatTextMethod.match(/await [^;]+/g);
if (awaitMatches) {
    console.log('Found await statements:');
    awaitMatches.forEach((match, index) => {
        console.log(`Await ${index + 1}: ${match}`);
    });
} else {
    console.log('No await statements found');
}

console.log('\n=== Checking variable assignments and formattedLines ===');
const formattedLinesMatches = formatTextMethod.match(/formattedLines[^;]*;/g);
if (formattedLinesMatches) {
    console.log('formattedLines operations:');
    formattedLinesMatches.forEach((match, index) => {
        console.log(`Operation ${index + 1}: ${match}`);
    });
}

const resultMatches = formatTextMethod.match(/const result = [^;]+;|result = [^;]+;/g);
if (resultMatches) {
    console.log('\nResult assignments:');
    resultMatches.forEach((match, index) => {
        console.log(`Assignment ${index + 1}: ${match}`);
    });
}

// Check if the method has proper error handling that might return undefined
console.log('\n=== Error handling analysis ===');
const tryBlocks = formatTextMethod.match(/try\s*{[\s\S]*?catch\s*\([^)]*\)\s*{[\s\S]*?}/g);
if (tryBlocks) {
    tryBlocks.forEach((block, index) => {
        console.log(`Try block ${index + 1}:`);
        const catchPart = block.match(/catch\s*\([^)]*\)\s*{([\s\S]*?)}/);
        if (catchPart) {
            console.log(`  Catch block: ${catchPart[1].trim()}`);
        }
    });
} else {
    console.log('No try-catch blocks found in formatText');
}

console.log('\n=== Conclusion ===');
console.log('If formatText is returning undefined, it could be due to:');
console.log('1. Missing return statement');
console.log('2. A path through the code that doesn\'t return anything');
console.log('3. An exception being caught and not re-thrown');
console.log('4. An async operation not being awaited properly');
