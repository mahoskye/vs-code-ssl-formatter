import * as assert from 'assert';
import * as vscode from 'vscode';
import { SSLCodeLensProvider } from '../src/sslCodeLensProvider';

describe('CodeLens Provider Tests', () => {
    it('Detects DoProc references strings even with SSL comments', async () => {
        const text = `
/* This is an SSL comment;
:PROCEDURE CheckCalculations;
:ENDPROC;

/* Another SSL comment;
:PROCEDURE Main;
    DoProc("CheckCalculations");
:ENDPROC;
`;
        const document = await vscode.workspace.openTextDocument({ content: text, language: 'ssl' });
        const provider = new SSLCodeLensProvider();
        const codeLenses = provider.provideCodeLenses(document, new vscode.CancellationTokenSource().token);

        assert.strictEqual(codeLenses.length, 2, 'Should find 2 procedures');

        // Find CheckCalculations lens
        const lens = codeLenses.find(l => l.command?.title.includes('reference'));
        // If undefined, it means 0 references were found (impl might omit "0 references" or title format differs)
        // Let's check what we got
        const checkCalcLens = codeLenses.find(l => l.range.start.line === 2);

        if (checkCalcLens) {
            assert.strictEqual(checkCalcLens.command?.title, '1 reference', 'Should count DoProc string as reference');
        } else {
            assert.fail('Could not find CodeLens for CheckCalculations');
        }
    });
});
