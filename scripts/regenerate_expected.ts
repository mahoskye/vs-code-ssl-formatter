/**
 * Regenerate the expected output file using the formatter
 * Run with: npx ts-node --require tests/setup.ts scripts/regenerate_expected.ts
 */

import * as fs from 'fs';
import * as path from 'path';
import * as vscode from 'vscode';
import { SSLFormattingProvider } from '../src/sslFormattingProvider';
import { createSSLConfig, MockTextDocument, MockUri, MockFormattingOptions } from '../tests/helpers/mockVSCode';

// Configure for SQL formatting
const mockConfig = createSSLConfig({
    'ssl.format.sql.enabled': true,
    'ssl.format.sql.style': 'canonicalCompact',
    'ssl.format.sql.keywordCase': 'upper',
    'ssl.format.wrapLength': 90
});

// Set the mock configuration
const mockVscode = require('vscode');
mockVscode.workspace.configuration = mockConfig;

async function run() {
    const provider = new SSLFormattingProvider();

    const inputPath = path.resolve(process.cwd(), 'DETERMINERUNLIST_Rungroup.srvscr');
    const outputPath = path.resolve(process.cwd(), 'DETERMINERUNLIST_Rungroup.expected.srvscr');

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
