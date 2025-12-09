
import * as assert from 'assert';
import * as vscode from 'vscode';
import { describe, it } from 'mocha';
import { SSLCodeActionProvider } from '../src/sslCodeActionProvider';
import { SSLDiagnosticProvider } from '../src/sslDiagnosticProvider'; // Need to trigger diagnostic generation logic if possible, or manually create diagnostic

describe('Code Action: Convert to DoProc', () => {

    it('Should offer code action for invalid direct procedure call', async function () {
        const content = `
:PROCEDURE TestContext;
    MyProc(1, "abc");
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content });

        // Simulate Diagnostic
        // In a real integration test, the diagnostic provider would run.
        // Here we can manually construct the diagnostic that the provider expects.
        // The provider expects a diagnostic with code 'ssl-invalid-direct-call' on the function name.

        const range = new vscode.Range(2, 4, 2, 10); // "MyProc"
        const diagnostic = new vscode.Diagnostic(range, "Invalid direct call", vscode.DiagnosticSeverity.Error);
        diagnostic.code = 'ssl-invalid-direct-call';

        const context: vscode.CodeActionContext = {
            diagnostics: [diagnostic],
            only: undefined,
            triggerKind: vscode.CodeActionTriggerKind.Invoke
        };

        const provider = new SSLCodeActionProvider();
        const actions = provider.provideCodeActions(doc, range, context, {} as vscode.CancellationToken);

        const fix = actions.find(a => a.title.startsWith('Convert to DoProc'));
        assert.ok(fix, 'Should find convert action');

        // Apply edit
        if (fix.edit) {
            await vscode.workspace.applyEdit(fix.edit);
        }

        const newContent = doc.getText();
        assert.ok(newContent.includes('DoProc("MyProc", {1, "abc"})'), 'Should replace with DoProc call');
    });

    it('Should handle calls without arguments', async function () {
        const content = `
:PROCEDURE TestContext;
    MyProc();
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content, fileName: '/test2.ssl' } as any);

        // "MyProc" at line 2 cols 4-10
        const range = new vscode.Range(2, 4, 2, 10);
        const diagnostic = new vscode.Diagnostic(range, "Invalid direct call", vscode.DiagnosticSeverity.Error);
        diagnostic.code = 'ssl-invalid-direct-call';

        const context: vscode.CodeActionContext = {
            diagnostics: [diagnostic],
            only: undefined,
            triggerKind: vscode.CodeActionTriggerKind.Invoke
        };

        const provider = new SSLCodeActionProvider();
        const actions = provider.provideCodeActions(doc, range, context, {} as vscode.CancellationToken);

        const fix = actions.find(a => a.title.startsWith('Convert to DoProc'));
        assert.ok(fix, 'Should find convert action');

        if (fix.edit) {
            await vscode.workspace.applyEdit(fix.edit);
        }

        const newContent = doc.getText();
        assert.ok(newContent.includes('DoProc("MyProc")'), 'Should replace with DoProc("MyProc")');
    });
});
