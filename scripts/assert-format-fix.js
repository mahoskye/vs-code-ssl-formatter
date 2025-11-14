const fs = require('fs');
const path = require('path');

// Load compiled formatter utils from out/src
const utilsPath = path.resolve(__dirname, '..', 'out', 'src', 'utils', 'formatters.js');
if (!fs.existsSync(utilsPath)) {
    console.error('Compiled utils not found at', utilsPath);
    process.exit(2);
}

const utils = require(utilsPath);
const normalize = utils.normalizeOperatorSpacing || (utils.default && utils.default.normalizeOperatorSpacing);
if (!normalize) {
    console.error('normalizeOperatorSpacing function not found in compiled utils');
    process.exit(2);
}

const fixturePath = path.resolve(__dirname, '..', 'tests', 'fixtures', 'comprehensive-formatter-test.ssl');
const input = fs.readFileSync(fixturePath, 'utf8');
const output = normalize(input);

console.log('Running operator-split assertions on formatted output...');

// Patterns that indicate an operator has been split (e.g. ': =', '+ =', '- =', '* =', '/ =', '^ =', '% =')
const badPatterns = [
    /:\s+=/, /\+\s+=/, /-\s+=/, /\*\s+=/, /\/\s+=/, /\^\s+=/, /%\s+=/, /\=\s+\=/
];

const found = badPatterns.flatMap((p) => {
    const m = output.match(p);
    return m ? [p.toString()] : [];
});

if (found.length > 0) {
    console.error('Found split-operator patterns after formatting:', found);
    process.exit(1);
}

console.log('No split-operator patterns found. OK.');
process.exit(0);
