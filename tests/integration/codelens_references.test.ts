
import * as assert from 'assert';
import * as vscode from 'vscode';
import { SSLCodeLensProvider } from '../../src/sslCodeLensProvider';

suite('SSL CodeLens References', () => {
    test('Should count CodeLens references including DoProc strings', async () => {
        const content = `
:PROCEDURE MyProc;
:ENDPROC;

:PROCEDURE Other;
    DoProc("MyProc");
    ExecFunction('MyProc');
:ENDPROC;
`;
        const document = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        const provider = new SSLCodeLensProvider();
        const lenses = provider.provideCodeLenses(document, {} as any);

        // Should have 1 CodeLens for MyProc (line 1)
        // References: DoProc (line 5), ExecFunction (line 6) -> Total 2

        const myProcLens = lenses.find(l => l.range.start.line === 1);
        if (!myProcLens) {
            assert.fail('CodeLens for MyProc should exist');
        }

        // Check title
        // Should be "2 references"
        console.log('CodeLens Title:', myProcLens.command?.title);
        assert.strictEqual(myProcLens.command?.title, '2 references');
    });
});
