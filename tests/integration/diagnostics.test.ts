
import * as assert from 'assert';
import * as vscode from 'vscode';

suite('Diagnostics Integration Test', () => {

    test('Reports errors for unclosed blocks', async () => {
        const content = `
:PROCEDURE TestBlocks;
    :IF .T.;
    :DECLARE x;
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        await vscode.window.showTextDocument(doc);

        // Wait for diagnostics to settle
        await new Promise(resolve => setTimeout(resolve, 2000));

        const diagnostics = vscode.languages.getDiagnostics(doc.uri);
        assert.ok(diagnostics.length > 0, 'Should report diagnostics for unclosed block');
        assert.strictEqual(diagnostics[0].severity, vscode.DiagnosticSeverity.Error);
    });

    test('Does NOT report errors for valid blocks', async () => {
        const content = `
:PROCEDURE TestValidBlocks;
    :IF .T.;
        :DECLARE x;
    :ENDIF;
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        await vscode.window.showTextDocument(doc);

        // Wait for diagnostics
        await new Promise(resolve => setTimeout(resolve, 2000));

        const diagnostics = vscode.languages.getDiagnostics(doc.uri);
        assert.strictEqual(diagnostics.length, 0, 'Should not report diagnostics for valid blocks');
    });
});
