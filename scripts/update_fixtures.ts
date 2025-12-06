
import * as fs from 'fs';
import * as path from 'path';
import { SSLFormattingProvider } from '../src/sslFormattingProvider';
import { createDocument, applyEdits, createFormattingOptions } from '../tests/helpers/mockVSCode';

const fixtureDir = path.join(__dirname, '../tests/fixtures/style-guide');

if (!fs.existsSync(fixtureDir)) {
    console.error('Fixture directory not found');
    process.exit(1);
}

const fixtures = fs.readdirSync(fixtureDir)
    .filter(f => f.endsWith('-bad.ssl'))
    .map(f => f.replace('-bad.ssl', ''));

const formatter = new SSLFormattingProvider();
const options = createFormattingOptions();

fixtures.forEach(fixtureName => {
    const badPath = path.join(fixtureDir, `${fixtureName}-bad.ssl`);
    const expectedPath = path.join(fixtureDir, `${fixtureName}-expected.ssl`);

    console.log(`Updating fixture: ${fixtureName}`);
    const input = fs.readFileSync(badPath, 'utf-8');
    const doc = createDocument(input);

    // Provide edits
    const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
    const formatted = applyEdits(input, edits as any[]);

    fs.writeFileSync(expectedPath, formatted);
});

console.log('All fixtures updated.');
