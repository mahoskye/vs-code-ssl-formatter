const { SSLFoldingRangeProvider } = require('./out/src/providers/foldingRangeProvider');

const content = `dynamicCode := {|param1, param2|
    param1 + param2
};`;

console.log('Content:');
console.log(content);
console.log('\nLines:');
content.split('\n').forEach((line, i) => console.log(`${i}: ${line}`));

const document = {
    getText: () => content,
    lineCount: content.split('\n').length,
    lineAt: (line) => ({
        text: content.split('\n')[line] || '',
        lineNumber: line,
        range: { start: { line: line, character: 0 }, end: { line: line, character: (content.split('\n')[line] || '').length } },
        rangeIncludingLineBreak: { start: { line: line, character: 0 }, end: { line: line + 1, character: 0 } },
        firstNonWhitespaceCharacterIndex: 0,
        isEmptyOrWhitespace: (content.split('\n')[line] || '').trim().length === 0
    }),
    positionAt: (offset) => ({ line: 0, character: offset }),
    offsetAt: (position) => position.character,
    save: async () => true,
    isClosed: false,
    isDirty: false,
    isUntitled: false,
    languageId: 'ssl',
    version: 1,
    fileName: 'test.ssl',
    uri: { scheme: 'file', path: 'test.ssl' }
};

const provider = new SSLFoldingRangeProvider();
const ranges = provider.provideFoldingRanges(document, {}, {});

console.log('\nFolding ranges:');
if (ranges && ranges.length > 0) {
    ranges.forEach((range, i) => {
        console.log(`${i}: start=${range.start}, end=${range.end}, kind=${range.kind}`);
    });
} else {
    console.log('No folding ranges returned');
}
