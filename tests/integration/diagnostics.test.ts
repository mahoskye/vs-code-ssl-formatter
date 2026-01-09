
import * as assert from 'assert';
import * as vscode from 'vscode';

suite('Diagnostics Integration Test', () => {

    test('Reports errors for invalid RunSQL usage', async () => {
        const content = `
:PROCEDURE TestRunSQL;
    /* Missing parameters argument;
    RunSQL("SELECT * FROM table WHERE id = ?");
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        await vscode.window.showTextDocument(doc);

        // Wait for diagnostics to settle
        await new Promise(resolve => setTimeout(resolve, 2000));

        const diagnostics = vscode.languages.getDiagnostics(doc.uri);
        const runSqlError = diagnostics.find(d => d.message.includes('RunSQL requires parameters argument'));

        assert.ok(runSqlError, 'Should report error for RunSQL with placeholders but no params');
        assert.strictEqual(runSqlError?.severity, vscode.DiagnosticSeverity.Error);
    });

    test('Does NOT report errors for ALen and AEval (Regression)', async () => {
        const content = `
:PROCEDURE TestBuiltins;
    :DECLARE arr;
    arr := {1, 2, 3};
    :DECLARE len;
    len := ALen(arr);
    AEval(arr, "SomeFunc");
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        await vscode.window.showTextDocument(doc);

        // Wait for diagnostics
        await new Promise(resolve => setTimeout(resolve, 2000));

        const diagnostics = vscode.languages.getDiagnostics(doc.uri);
        const undefinedVars = diagnostics.filter(d => d.message.includes('Undefined variable'));

        // Check specifically for 'len' vs 'ALen' just in case
        const falsePositives = undefinedVars.filter(d =>
            d.range.start.line > 0 && // ignore header
            (doc.getText(d.range) === 'ALen' || doc.getText(d.range) === 'AEval')
        );

        assert.strictEqual(falsePositives.length, 0, 'Should not report undefined variable for ALen or AEval');
    });
});
