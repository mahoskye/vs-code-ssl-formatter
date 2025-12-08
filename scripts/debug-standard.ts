
import * as fs from 'fs';
import * as path from 'path';
import { SSLFormatter } from '../src/formatting/formatter';

const fixturesDir = path.join(process.cwd(), 'tests/fixtures/style-guide');

const filesToUpdate = [
    '03-formatting-spacing-after-comma',
    '22-function-calls-multiline-arguments'
];

const options = {
    tabSize: 4,
    insertSpaces: false,
    'ssl.format.wrapLength': 90,
    'ssl.format.sql.enabled': false, // DISABLED
    'ssl.format.sql.keywordCase': 'upper',
    'ssl.format.sql.indentSpaces': 4
};

const formatter = new SSLFormatter(options as any);

filesToUpdate.forEach(baseName => {
    const badFile = path.join(fixturesDir, `${baseName}-bad.ssl`);
    if (fs.existsSync(badFile)) {
        const input = fs.readFileSync(badFile, 'utf8');
        const formatted = formatter.format(input);

        const escaped = formatted.replace(/\t/g, '[TAB]').replace(/ /g, '.');
        console.log(`\n--- START ${baseName} ---`);
        console.log(escaped);
        console.log(`\n--- END ${baseName} ---`);
    } else {
        console.log(`File not found: ${badFile}`);
    }
});
