import * as fs from 'fs';
import * as path from 'path';
// import { SSLFormattingProvider } from '../src/sslFormattingProvider'; // Moved to dynamic require
import { createDocument, createFormattingOptions, createSSLConfig, applyEdits } from '../tests/helpers/mockVSCode';
import { Module } from 'module';

// Mock VSCode module (similar to tests/setup.ts but minimal for formatting)
const mockVscode = {
    workspace: {
        configuration: createSSLConfig(),
        getConfiguration: () => createSSLConfig(),
        asRelativePath: (p: string) => p
    },
    window: { showErrorMessage: () => { } },
    Range: require('../tests/helpers/mockVSCode').MockRange,
    Position: require('../tests/helpers/mockVSCode').MockPosition,
    TextEdit: require('../tests/helpers/mockVSCode').MockTextEdit,
    Uri: require('../tests/helpers/mockVSCode').MockUri,
    EndOfLine: { LF: 1, CRLF: 2 }
};

// Override require('vscode')
const originalRequire = Module.prototype.require;
(Module.prototype.require as any) = function (id: string) {
    if (id === 'vscode') return mockVscode;
    return originalRequire.apply(this, arguments as any);
};

async function updateSnapshots() {
    const fixturesDir = path.join(__dirname, '../tests/fixtures/style-guide');
    const commentsDir = path.join(__dirname, '../tests/fixtures/comments'); // Add comments dir

    // Get files from both dirs
    const styleFiles = fs.readdirSync(fixturesDir).filter(f => f.endsWith('.ssl') && !f.endsWith('.expected.ssl')).map(f => ({ dir: fixturesDir, name: f }));
    const commentFiles = fs.existsSync(commentsDir) ? fs.readdirSync(commentsDir).filter(f => f.endsWith('.ssl') && !f.endsWith('.expected.ssl')).map(f => ({ dir: commentsDir, name: f })) : [];

    const files = [...styleFiles, ...commentFiles];

    const { SSLFormattingProvider } = require('../src/sslFormattingProvider');

    // We need to toggle config based on fixture type, similar to formatter.test.ts
    const config = mockVscode.workspace.configuration;
    const options = createFormattingOptions(); // This can stay outside the loop as it doesn't change per file based on config

    console.log(`Found ${files.length} fixtures to update.`);

    for (const fileObj of files) {
        const { dir, name: file } = fileObj;

        // Reset config defaults
        config.update('ssl.format.sql.enabled', false);
        config.update('ssl.format.sql.indentSpaces', 4);

        if (file.includes('sql-formatting')) {
            config.update('ssl.format.sql.enabled', true);
        }

        const inputPath = path.join(dir, file);
        const expectedPath = path.join(dir, file.replace('-bad.ssl', '-expected.ssl'));

        const input = fs.readFileSync(inputPath, 'utf8').replace(/\r\n/g, '\n');
        const doc = createDocument(input);

        // Re-instantiate formatter to pick up config? 
        // Provider reads config in provideDocumentFormattingEdits usually via workspace.getConfiguration
        // But let's instantiate per file to be safe if it caches.
        const formatter = new SSLFormattingProvider();

        // Pass null for token
        const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
        const formatted = applyEdits(input, edits as any[]);

        fs.writeFileSync(expectedPath, formatted, 'utf8');
        console.log(`Updated ${path.basename(expectedPath)} (SQL: ${file.includes('sql-formatting')})`);
    }
}

updateSnapshots().catch(console.error);
