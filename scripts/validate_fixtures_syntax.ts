
import * as fs from 'fs';
import * as path from 'path';
import * as glob from 'glob';

// Fix for ESM/Node environment
const fixturesDir = path.join(process.cwd(), 'tests/fixtures');

function validateFile(filePath: string): string[] {
    const content = fs.readFileSync(filePath, 'utf8');
    const errors: string[] = [];
    const lines = content.split('\n');

    let inComment = false;
    let commentStartLine = 0;

    for (let i = 0; i < lines.length; i++) {
        const line = lines[i];

        // Scan line for comment markers
        let cursor = 0;
        while (cursor < line.length) {
            if (!inComment) {
                // Check for start of string
                if (line[cursor] === '"' || line[cursor] === "'") {
                    const quote = line[cursor];
                    cursor++;
                    while (cursor < line.length) {
                        if (line[cursor] === quote) {
                            // Check for escaped quote? simple check
                            break;
                        }
                        cursor++;
                    }
                }
                // Check for comment start
                else if (line.substr(cursor, 2) === '/*') {
                    inComment = true;
                    commentStartLine = i + 1;
                    cursor += 1;
                }
                // Check for invalid comment start
                else if (line.substr(cursor, 2) === '//') {
                    errors.push(`Line ${i + 1}: Found invalid '//' comment`);
                    cursor += 1; // skip to avoid infinite loop if logic flaw
                }
                // Check for invalid comment end outside comment
                else if (line.substr(cursor, 2) === '*/') {
                    // Might be valid inside string, but we skipped strings.
                    // If we are here, we are not in string, not in comment.
                    // BUT SSL syntax implies EBNF, does it forbid */? 
                    // It's not a terminator. It's just chars. 
                    // But usually users mistake it for terminator.
                    errors.push(`Line ${i + 1}: Found '*/' outside of comment (suspected invalid terminator usage)`);
                    cursor += 1;
                }
            } else {
                // In comment
                if (line[cursor] === ';') {
                    inComment = false;
                } else if (line.substr(cursor, 2) === '*/') {
                    errors.push(`Line ${i + 1}: Found '*/' inside comment (suspected invalid terminator usage)`);
                }
            }
            cursor++;
        }
    }

    if (inComment) {
        errors.push(`Line ${commentStartLine}: Comment started but never ended (missing ';')`);
    }

    return errors;
}

const files = glob.sync('**/*.{ssl,srvscr}', { cwd: fixturesDir, absolute: true });
let hasErrors = false;

files.forEach(file => {
    const errors = validateFile(file);
    if (errors.length > 0) {
        console.log(`\nFile: ${path.relative(process.cwd(), file)}`);
        errors.forEach(e => console.log(`  ${e}`));
        hasErrors = true;
    }
});

if (!hasErrors) {
    console.log("All fixtures valid according to check.");
}
