import * as assert from 'assert';
import * as vscode from 'vscode';
import { SSLCodeLensProvider } from '../src/sslCodeLensProvider';

describe('CodeLens Provider Tests', () => {
    it('Detects DoProc references strings even with SSL comments', async () => {
        const text = `
/* This is an SSL comment;
:PROCEDURE TallyWidgets;
:ENDPROC;

/* Another SSL comment;
:PROCEDURE Main;
    DoProc("TallyWidgets");
:ENDPROC;
`;
        const document = await vscode.workspace.openTextDocument({ content: text, language: 'ssl' });
        const provider = new SSLCodeLensProvider();
        const codeLenses = provider.provideCodeLenses(document, new vscode.CancellationTokenSource().token);

        assert.strictEqual(codeLenses.length, 2, 'Should find 2 procedures');

        // Find TallyWidgets lens at line 2
        const tallyLens = codeLenses.find(l => l.range.start.line === 2);

        if (tallyLens) {
            assert.strictEqual(tallyLens.command?.title, '1 reference', 'Should count DoProc string as reference');
        } else {
            assert.fail('Could not find CodeLens for TallyWidgets');
        }
    });
});
