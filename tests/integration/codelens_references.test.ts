
import * as assert from 'assert';
import * as vscode from 'vscode';
import { SSLReferenceProvider } from '../../src/sslReferenceProvider';
import { SSLCodeLensProvider } from '../../src/sslCodeLensProvider';

suite('SSL Reference Provider Reproduction', () => {
    test('Should find DoProc and ExecFunction references for Procedure', async () => {
        const content = `
:PROCEDURE MyProc;
:ENDPROC;

:PROCEDURE Other;
    DoProc("MyProc");
    ExecFunction('MyProc');
    MyProc();
    :DECLARE x;
    x := MyProc;
:ENDPROC;
`;
        const document = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        const provider = new SSLReferenceProvider();

        // Position at :PROCEDURE MyProc (Line 1, char 12)
        // :PROCEDURE MyProc;
        // 01234567890123
        const position = new vscode.Position(1, 12);

        // Token validation
        const wordRange = document.getWordRangeAtPosition(position);
        const word = document.getText(wordRange);
        assert.strictEqual(word, 'MyProc');


        const token: vscode.CancellationToken = {
            isCancellationRequested: false,
            onCancellationRequested: (listener: any) => ({ dispose: () => { } })
        } as any;

        const results = provider.provideReferences(document, position, { includeDeclaration: true }, token);

        // Expected line numbers based on content:
        // Line 1: :PROCEDURE MyProc; (definition)
        // Line 5: DoProc("MyProc");
        // Line 6: ExecFunction('MyProc');
        // Line 7: MyProc(); - should NOT be found
        // Line 9: x := MyProc; - should NOT be found

        const lineNumbers = results.map(l => l.range.start.line).sort((a, b) => a - b);

        console.log('Found references at lines:', lineNumbers);

        assert.ok(lineNumbers.includes(1), 'Should find definition');
        assert.ok(lineNumbers.includes(5), 'Should find DoProc');
        assert.ok(lineNumbers.includes(6), 'Should find ExecFunction');
        assert.ok(!lineNumbers.includes(7), 'Should NOT find direct call MyProc() when searching for procedure');
        assert.ok(!lineNumbers.includes(9), 'Should NOT find variable usage x := MyProc when searching for procedure');
    });

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
