
import * as fs from 'fs';
import * as path from 'path';
import * as vscode from 'vscode';
import { SSLFormattingProvider } from '../src/sslFormattingProvider';
import { createSSLConfig, MockUri, MockTextDocument, MockFormattingOptions } from '../tests/helpers/mockVSCode';

// Setup mock environment
const mockConfig = createSSLConfig({
    // Enable SQL formatting as requested
    'ssl.format.sql.enabled': true,
    'ssl.format.sql.style': 'canonicalCompact',
    'ssl.format.sql.keywordCase': 'upper',
    // Ensure wrapping is enabled
    'ssl.format.wrapLength': 90
});

// Mock workspace configuration
const mockVscode = require('vscode');
if (!mockVscode.workspace) {
    mockVscode.workspace = {};
}
mockVscode.workspace.getConfiguration = (section: string) => mockConfig;

async function run() {
    const provider = new SSLFormattingProvider();

    const args = process.argv.slice(2);
    if (args.length < 1) {
        console.error('Usage: ts-node scripts/generate_output.ts <input_file>');
        process.exit(1);
    }

    const inputPath = path.resolve(process.cwd(), args[0]);
    let outputPath = inputPath.replace(/\.srvscr$|\.ssl$|\.ds$/, '.expected$&');
    // If extension wasn't matched, just append .expected
    if (outputPath === inputPath) {
        outputPath = path.join(path.dirname(inputPath), path.basename(inputPath) + '.expected');
    }

    console.log(`Reading from ${inputPath}`);
    const content = fs.readFileSync(inputPath, 'utf8');

    // Create mock document
    const doc = new MockTextDocument(
        MockUri.file(inputPath),
        'ssl',
        content
    );

    const options: MockFormattingOptions = {
        tabSize: 4,
        insertSpaces: false
    };

    console.log('Formatting...');
    const edits = provider.provideDocumentFormattingEdits(doc as any, options as any, {} as any);

    if (edits && edits.length > 0) {
        console.log(`Writing formatted content to ${outputPath}`);
        fs.writeFileSync(outputPath, edits[0].newText);
        console.log('Done.');
    } else {
        console.log('No edits generated.');
    }
}

run().catch(err => console.error(err));
