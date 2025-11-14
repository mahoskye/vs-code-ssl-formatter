const fs = require('fs');
const path = require('path');

function replaceOutsideStrings(line, pattern, replacement) {
    const segments = [];
    let current = '';
    let inString = false;
    let stringChar = null;

    for (let i = 0; i < line.length; i++) {
        const char = line[i];

        if (!inString && (char === '"' || char === "'")) {
            if (current) {
                segments.push({ text: current, inString: false });
                current = '';
            }
            inString = true;
            stringChar = char;
            current = char;
        } else if (inString && char === stringChar) {
            current += char;
            segments.push({ text: current, inString: true });
            current = '';
            inString = false;
            stringChar = null;
        } else {
            current += char;
        }
    }

    if (current) segments.push({ text: current, inString });

    return segments.map(seg => seg.inString ? seg.text : seg.text.replace(pattern, replacement)).join('');
}

function normalizeOperatorSpacing(text) {
    const lines = text.split('\n');
    const out = lines.map(line => {
        const trimmed = line.trim();
        // skip comment heuristics similar to provider
        if (!trimmed) return line;
        if (trimmed.startsWith('/*') && !trimmed.endsWith(';')) return line;
        if (trimmed.startsWith('/*') || trimmed.startsWith('*')) return line;

        let result = line;

        // Space around assignment operators
        result = replaceOutsideStrings(result, /\s*:=\s*/g, ' := ');
        result = replaceOutsideStrings(result, /\s*\+=\s*/g, ' += ');
        result = replaceOutsideStrings(result, /\s*-=\s*/g, ' -= ');
        result = replaceOutsideStrings(result, /\s*\*=\s*/g, ' *= ');
        result = replaceOutsideStrings(result, /\s*\/=\s*/g, ' /= ');
        result = replaceOutsideStrings(result, /\s*\^=\s*/g, ' ^= ');
        result = replaceOutsideStrings(result, /\s*%=\s*/g, ' %= ');

        // Space around arithmetic operators - multiple passes
        let prev = '';
        while (prev !== result) {
            prev = result;
            result = replaceOutsideStrings(result, /([a-zA-Z0-9_\)])(\+|\*|\/|\^|%)([a-zA-Z0-9_\(])/g, '$1 $2 $3');
            result = replaceOutsideStrings(result, /([a-zA-Z0-9_\)])(-)([a-zA-Z0-9_\(])/g, '$1 $2 $3');
        }

        // Comparison operators
        result = replaceOutsideStrings(result, /\s*=\s*=+\s*/g, ' == ');
        result = replaceOutsideStrings(result, /\s*!=\s*/g, ' != ');
        result = replaceOutsideStrings(result, /\s*<>\s*/g, ' <> ');
        result = replaceOutsideStrings(result, /\s*<=\s*/g, ' <= ');
        result = replaceOutsideStrings(result, /\s*>=\s*/g, ' >= ');
        result = replaceOutsideStrings(result, /\s*#\s*/g, ' # ');
        // Single = operator last; exclude ':' on left to avoid splitting ':='
        result = replaceOutsideStrings(result, /([^=<>!:])\s*=\s*([^=])/g, '$1 = $2');
        result = replaceOutsideStrings(result, /([^<>=])\s*<\s*([^>=])/g, '$1 < $2');
        result = replaceOutsideStrings(result, /([^<>=])\s*>\s*([^=])/g, '$1 > $2');

        // Logical operators
        result = replaceOutsideStrings(result, /\s*\.AND\.\s*/gi, ' .AND. ');
        result = replaceOutsideStrings(result, /\s*\.OR\.\s*/gi, ' .OR. ');
        result = replaceOutsideStrings(result, /\s*\.NOT\.\s*/gi, ' .NOT. ');

        // Unary !
        result = replaceOutsideStrings(result, /([a-zA-Z0-9_\)])\s*!/g, '$1 !');
        result = replaceOutsideStrings(result, /!\s*([a-zA-Z0-9_\(])/g, '! $1');

        // Space after commas
        result = replaceOutsideStrings(result, /,(\S)/g, ', $1');

        // No space before semicolons
        result = replaceOutsideStrings(result, /\s+;/g, ';');

        return result;
    });

    return out.join('\n');
}

// Allow passing a file path as argument (e.g. node runFormatterTest.js tests/fixtures/comprehensive-formatter-test.ssl.orig)
const fixtureArg = process.argv[2];
const fixturePath = fixtureArg
    ? path.resolve(process.cwd(), fixtureArg)
    : path.resolve(__dirname, '..', 'tests', 'fixtures', 'comprehensive-formatter-test.ssl');
const input = fs.readFileSync(fixturePath, 'utf8');
const result = normalizeOperatorSpacing(input);

console.log('----- Formatted Output Start -----');
console.log(result);
console.log('----- Formatted Output End -----');
