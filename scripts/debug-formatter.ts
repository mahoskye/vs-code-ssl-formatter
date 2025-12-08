
import * as fs from 'fs';
import * as path from 'path';
import { SSLFormatter } from '../src/formatting/formatter';

const fixturesDir = path.join(process.cwd(), 'tests/fixtures/style-guide');

const filesToUpdate = [
    '20-arrays-literal-syntax'
];

const options = {
    tabSize: 4,
    insertSpaces: false, // Use tabs for consistent failure matching (tests seem to expect tabs usually)
    'ssl.format.wrapLength': 90,
    'ssl.format.sql.enabled': true, // Enable SQL formatting
    'ssl.format.sql.keywordCase': 'upper',
    'ssl.format.sql.indentSpaces': 4
};

const formatter = new SSLFormatter(options as any);

filesToUpdate.forEach(baseName => {
    const badFile = path.join(fixturesDir, `${baseName}-bad.ssl`);
    if (fs.existsSync(badFile)) {
        const input = fs.readFileSync(badFile, 'utf8');
        const formatted = formatter.format(input);

        console.log(`\n--- START ${baseName} ---`);
        process.stdout.write(formatted);
        console.log(`\n--- END ${baseName} ---`);

        // Optionally write to expected file directly?
        // const expectedFile = path.join(fixturesDir, `${baseName}-expected.ssl`);
        // fs.writeFileSync(expectedFile, formatted);
        // console.log(`Updated ${expectedFile}`);
    } else {
        console.log(`File not found: ${badFile}`);
    }
});
