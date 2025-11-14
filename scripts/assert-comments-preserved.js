const fs = require('fs');
const path = require('path');

const utilsPath = path.resolve(__dirname, '..', 'out', 'src', 'utils', 'formatters.js');
if (!fs.existsSync(utilsPath)) {
    console.error('Compiled utils not found at', utilsPath, '\nRun: npm run compile');
    process.exit(2);
}

const utils = require(utilsPath);
const normalizeOp = utils.normalizeOperatorSpacing || (utils.default && utils.default.normalizeOperatorSpacing);
const normalizeIndent = utils.normalizeIndentation || (utils.default && utils.default.normalizeIndentation);
if (!normalizeOp || !normalizeIndent) {
    console.error('Required formatting functions not found in compiled utils');
    process.exit(2);
}

const fixtureDir = path.resolve(__dirname, '..', 'tests', 'fixtures', 'comments');
const badPath = path.join(fixtureDir, 'comments-bad.ssl');
const expectedPath = path.join(fixtureDir, 'comments-expected.ssl');

const bad = fs.readFileSync(badPath, 'utf8');
// Run the formatting pipeline (partial): operator spacing then indentation
const afterOp = normalizeOp(bad);
const afterIndent = normalizeIndent(afterOp, 'tab', 1, 4);

// DEBUG: show first comment block transformations
try {
    const firstIn = bad.split('\n').slice(0, 10).join('\n');
    const firstOp = afterOp.split('\n').slice(0, 10).join('\n');
    const firstInd = afterIndent.split('\n').slice(0, 10).join('\n');
    console.log('--- DEBUG: first 10 lines ORIGINAL ---');
    console.log(firstIn);
    console.log('--- DEBUG: first 10 lines AFTER OP ---');
    console.log(firstOp);
    console.log('--- DEBUG: first 10 lines AFTER INDENT ---');
    console.log(firstInd);
} catch (e) {
    // ignore
}

// Extract comment blocks from text
function extractCommentBlocks(text) {
    const lines = text.split('\n');
    const blocks = [];
    let i = 0;
    while (i < lines.length) {
        const line = lines[i];
        const trimmed = line.trim();
        if (trimmed.startsWith('/*')) {
            // start of block comment
            const block = [];
            block.push(line);
            if (trimmed.endsWith(';')) {
                // single-line block
                blocks.push(block);
                i++;
                continue;
            }
            i++;
            while (i < lines.length) {
                block.push(lines[i]);
                if (lines[i].trim().endsWith(';')) {
                    i++;
                    break;
                }
                i++;
            }
            blocks.push(block);
            continue;
        } else if (trimmed.startsWith('*')) {
            // single line star-comment
            blocks.push([line]);
            i++;
            continue;
        }
        i++;
    }
    return blocks;
}

function normalizeLeadingWhitespace(lines) {
    return lines.map(l => l.replace(/^\s+/, ''));
}

const inputBlocks = extractCommentBlocks(bad);
const outputBlocks = extractCommentBlocks(afterIndent);

if (inputBlocks.length !== outputBlocks.length) {
    console.error('Number of comment blocks changed after formatting:', inputBlocks.length, '->', outputBlocks.length);
    process.exit(1);
}

for (let idx = 0; idx < inputBlocks.length; idx++) {
    const inBlock = normalizeLeadingWhitespace(inputBlocks[idx]);
    const outBlock = normalizeLeadingWhitespace(outputBlocks[idx]);
    if (inBlock.length !== outBlock.length) {
        console.error(`Comment block ${idx} line count changed: ${inBlock.length} -> ${outBlock.length}`);
        console.error('Input block:', inputBlocks[idx].join('\n'));
        console.error('Output block:', outputBlocks[idx].join('\n'));
        process.exit(1);
    }

    for (let j = 0; j < inBlock.length; j++) {
        if (inBlock[j] !== outBlock[j]) {
            console.error(`Comment content changed in block ${idx} line ${j}`);
            console.error('IN :', JSON.stringify(inBlock[j]));
            console.error('OUT:', JSON.stringify(outBlock[j]));
            process.exit(1);
        }
    }
}

console.log('All comment blocks preserved (modulo leading indentation). OK.');
process.exit(0);
