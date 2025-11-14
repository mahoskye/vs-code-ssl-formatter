/**
 * Test script to validate that mismatched block keywords are detected
 * This test checks if ENDIF, ELSE, EXITWHILE, ENDWHILE, LOOP, etc.
 * throw errors when used without their matching opening keywords
 */

const fs = require('fs');
const path = require('path');

// Read the test file
const testFilePath = path.join(__dirname, 'test-mismatched-blocks.ssl');
const content = fs.readFileSync(testFilePath, 'utf-8');
const lines = content.split('\n');

console.log('========================================');
console.log('BLOCK KEYWORD MISMATCH DETECTION TEST');
console.log('========================================\n');

// Simple block matching algorithm
const blockStack = [];
const errors = [];

const BLOCK_PAIRS = {
	'IF': 'ENDIF',
	'WHILE': 'ENDWHILE',
	'FOR': 'NEXT',
	'BEGINCASE': 'ENDCASE',
	'TRY': 'ENDTRY',
	'PROCEDURE': 'ENDPROC',
	'REGION': 'ENDREGION',
	'BEGININLINECODE': 'ENDINLINECODE'
};

const BLOCK_MIDDLE = {
	'ELSE': 'IF',
	'CATCH': 'TRY',
	'FINALLY': 'TRY',
	'CASE': 'BEGINCASE',
	'OTHERWISE': 'BEGINCASE',
	'TO': 'FOR',
	'STEP': 'FOR'
};

// Keywords that need to appear in specific context (not necessarily in blockStack)
const CONTEXT_KEYWORDS = {
	'DEFAULT': 'PARAMETERS'  // DEFAULT must follow PARAMETERS
};

const LOOP_CONTROL = {
	'EXITWHILE': 'WHILE',
	'LOOP': 'WHILE',
	'EXITFOR': 'FOR',
	'EXITCASE': 'BEGINCASE'
};

// Track if we've seen PARAMETERS in current procedure
let hasParameters = false;
let inProcedure = false;

lines.forEach((line, index) => {
	const trimmed = line.trim();
	const lineNum = index + 1;

	// Skip comments and empty lines
	if (!trimmed || trimmed.startsWith('/*') || trimmed.startsWith('*')) {
		return;
	}

	// Extract keyword
	const keywordMatch = trimmed.match(/^:([A-Z]+)\b/i);
	if (!keywordMatch) {
		return;
	}

	const keyword = keywordMatch[1].toUpperCase();

	// Track PROCEDURE scope for PARAMETERS tracking
	if (keyword === 'PROCEDURE') {
		inProcedure = true;
		hasParameters = false;
	}
	if (keyword === 'ENDPROC') {
		inProcedure = false;
		hasParameters = false;
	}
	if (keyword === 'PARAMETERS') {
		hasParameters = true;
	}

	// Check if it's a block start keyword
	if (BLOCK_PAIRS[keyword]) {
		blockStack.push({ keyword, line: lineNum });
		return;
	}

	// Check if it's a block end keyword
	const expectedStart = Object.keys(BLOCK_PAIRS).find(k => BLOCK_PAIRS[k] === keyword);
	if (expectedStart) {
		const lastBlock = blockStack[blockStack.length - 1];
		if (!lastBlock) {
			errors.push({
				line: lineNum,
				keyword,
				error: `${keyword} without matching ${expectedStart}`,
				text: trimmed
			});
		} else if (lastBlock.keyword !== expectedStart) {
			errors.push({
				line: lineNum,
				keyword,
				error: `${keyword} mismatched - expected ${BLOCK_PAIRS[lastBlock.keyword]}, found ${keyword}`,
				text: trimmed
			});
		} else {
			blockStack.pop(); // Valid match
		}
		return;
	}

	// Check if it's a block middle keyword
	if (BLOCK_MIDDLE[keyword]) {
		const expectedStart = BLOCK_MIDDLE[keyword];
		const hasMatchingBlock = blockStack.some(block => block.keyword === expectedStart);
		if (!hasMatchingBlock) {
			errors.push({
				line: lineNum,
				keyword,
				error: `${keyword} without matching ${expectedStart}`,
				text: trimmed
			});
		}
		return;
	}

	// Check if it's a loop control keyword
	if (LOOP_CONTROL[keyword]) {
		const expectedLoop = LOOP_CONTROL[keyword];
		const hasMatchingLoop = blockStack.some(block => block.keyword === expectedLoop);
		if (!hasMatchingLoop) {
			errors.push({
				line: lineNum,
				keyword,
				error: `${keyword} without matching ${expectedLoop}`,
				text: trimmed
			});
		}
		return;
	}

	// Check context keywords (like DEFAULT requiring PARAMETERS)
	if (CONTEXT_KEYWORDS[keyword]) {
		const requiredContext = CONTEXT_KEYWORDS[keyword];
		if (keyword === 'DEFAULT' && !hasParameters) {
			errors.push({
				line: lineNum,
				keyword,
				error: `${keyword} without ${requiredContext}`,
				text: trimmed
			});
		}
		return;
	}
});

// Report results
console.log(`Total lines: ${lines.length}`);
console.log(`Errors found: ${errors.length}\n`);

if (errors.length > 0) {
	console.log('DETECTED ERRORS:');
	console.log('================\n');
	errors.forEach((err, idx) => {
		console.log(`${idx + 1}. Line ${err.line}: ${err.error}`);
		console.log(`   Code: ${err.text}`);
		console.log('');
	});
} else {
	console.log('❌ NO ERRORS DETECTED!');
	console.log('This means the diagnostic system is NOT catching mismatched block keywords.\n');
}

// Summary
console.log('========================================');
console.log('EXPECTED ERRORS (from test file):');
console.log('========================================');
console.log('1. ENDIF without IF (line ~7)');
console.log('2. ELSE without IF (line ~14)');
console.log('3. EXITWHILE without WHILE (line ~22)');
console.log('4. ENDWHILE without WHILE (line ~29)');
console.log('5. LOOP without WHILE (line ~37)');
console.log('6. ENDCASE without BEGINCASE (line ~45)');
console.log('7. ENDPROC without PROCEDURE (line ~51)');
console.log('8. NEXT without FOR (line ~57)');
console.log('9. TO without FOR (line ~64)');
console.log('10. STEP without FOR (line ~71)');
console.log('11. DEFAULT without PARAMETERS (line ~77)');
console.log('12. ENDREGION without REGION (line ~85)');
console.log('13. ENDINLINECODE without BEGININLINECODE (line ~92)');
console.log('14. CATCH without TRY (line ~99)');
console.log('15. FINALLY without TRY (line ~107)');
console.log('16. ENDTRY without TRY (line ~115)');
console.log('17. CASE without BEGINCASE (line ~122)');
console.log('18. OTHERWISE without BEGINCASE (line ~130)');
console.log('19. EXITCASE without BEGINCASE (line ~138)');
console.log('20. EXITFOR without FOR (line ~145)');
console.log('');

if (errors.length >= 20) {
	console.log('✅ TEST PASSED: All mismatched blocks were detected!');
} else {
	console.log(`⚠️  TEST INCOMPLETE: Only ${errors.length}/20 errors detected`);
	console.log('   This is a basic validation algorithm - not the VSCode extension.');
	console.log('   The diagnostic provider needs to implement similar logic.');
}
