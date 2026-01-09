
import * as assert from 'assert';
import * as vscode from 'vscode';
import { SSLReferenceProvider } from '../../src/sslReferenceProvider';
import { SSLCodeLensProvider } from '../../src/sslCodeLensProvider';

suite('SSL Reference Provider Reproduction', () => {
    test('Should find DoProc and ExecFunction references for Procedure', async () => {
        const content = `
:PROCEDURE MyProc
EndProc

:PROCEDURE Other
    DoProc("MyProc")
    ExecFunction('MyProc')
    MyProc()
    var x = MyProc
EndProc
`;
        const document = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        const provider = new SSLReferenceProvider();

        // Position at :PROCEDURE MyProc (Line 1, char 12)
        // :PROCEDURE MyProc
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

        // Expected:
        // 1. Definition at line 1
        // 2. DoProc at line 5
        // 3. ExecFunction at line 6
        // Should NOT find line 7 (MyProc()) or line 8 (x = MyProc) if strictly looking for procedure references.

        const lineNumbers = results.map(l => l.range.start.line).sort((a, b) => a - b);

        // If the bug exists (finding `FunctionName()` style), we might see line 7.
        // If it works as intended for SSL procedures, we should see [1, 5, 6].

        console.log('Found references at lines:', lineNumbers);

        assert.ok(lineNumbers.includes(1), 'Should find definition');
        assert.ok(lineNumbers.includes(5), 'Should find DoProc');
        assert.ok(lineNumbers.includes(6), 'Should find ExecFunction');
        assert.ok(!lineNumbers.includes(7), 'Should NOT find direct call MyProc() when searching for procedure');
        assert.ok(!lineNumbers.includes(8), 'Should NOT find variable usage x = MyProc when searching for procedure');
    });

    test('Should count CodeLens references including DoProc strings', async () => {
        const content = `
:PROCEDURE MyProc
EndProc

:PROCEDURE Other
    DoProc("MyProc")
    ExecFunction('MyProc')
EndProc
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
