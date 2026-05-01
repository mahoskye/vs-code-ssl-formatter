
import * as assert from 'assert';
import * as vscode from 'vscode';
import { describe, it } from 'mocha';
import { SSLCodeActionProvider } from '../src/sslCodeActionProvider';

describe('Code Action: Convert to DoProc', () => {

    it('Should offer code action for invalid direct procedure call', async function () {
        const content = `
:PROCEDURE TestContext;
    MyProc(1, "abc");
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content, fileName: `/test-${Math.random()}.ssl` } as any);

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

describe('Code Actions: LSP-emitted slug codes', () => {

    function diagnostic(range: vscode.Range, message: string, code: string): vscode.Diagnostic {
        const d = new vscode.Diagnostic(range, message, vscode.DiagnosticSeverity.Warning);
        d.code = code;
        return d;
    }

    function context(diag: vscode.Diagnostic): vscode.CodeActionContext {
        return {
            diagnostics: [diag],
            only: undefined,
            triggerKind: vscode.CodeActionTriggerKind.Invoke
        };
    }

    it('udobject_array_in_clause: extracts UDObject property to a local var', async function () {
        const content = `:PROCEDURE Test;
SqlExecute("SELECT * FROM t WHERE id IN (?o:Items?)");
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content, fileName: `/test-${Math.random()}.ssl` } as any);

        // Range covers '?o:Items?' on line 1.
        const lineText = doc.lineAt(1).text;
        const startCol = lineText.indexOf('?o:Items?');
        const endCol = startCol + '?o:Items?'.length;
        const range = new vscode.Range(1, startCol, 1, endCol);
        const diag = diagnostic(range, "UDObject array property '?o:Items?' in IN clause", 'udobject_array_in_clause');

        const provider = new SSLCodeActionProvider();
        const actions = provider.provideCodeActions(doc, range, context(diag), {} as vscode.CancellationToken);

        const fix = actions.find(a => a.title.startsWith('Extract'));
        assert.ok(fix, 'expected an extract-to-local code action');

        await vscode.workspace.applyEdit(fix.edit!);
        const newContent = doc.getText();
        assert.ok(newContent.includes(':DECLARE aItems;'), 'should declare the new local');
        assert.ok(newContent.includes('aItems := o:Items;'), 'should assign the property to it');
        assert.ok(newContent.includes('?aItems?'), 'should rewrite the placeholder');
        assert.ok(!newContent.includes('?o:Items?'), 'old placeholder should be gone');
    });

    it('keyword_uppercase: uppercases the keyword token', async function () {
        const content = `:procedure Test;
:endproc;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content, fileName: `/test-${Math.random()}.ssl` } as any);
        const range = new vscode.Range(0, 1, 0, 10); // "procedure"
        const diag = diagnostic(range, 'Keywords must be uppercase', 'keyword_uppercase');

        const provider = new SSLCodeActionProvider();
        const actions = provider.provideCodeActions(doc, range, context(diag), {} as vscode.CancellationToken);

        const fix = actions.find(a => a.title.startsWith('Uppercase keyword'));
        assert.ok(fix, 'expected an uppercase-keyword code action');
        await vscode.workspace.applyEdit(fix.edit!);
        assert.ok(doc.getText().includes(':PROCEDURE'));
    });

    it('not_preferred_operator: <> is replaced with !=', async function () {
        const content = `:PROCEDURE T;
:IF nA <> nB;
:ENDIF;
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content, fileName: `/test-${Math.random()}.ssl` } as any);
        const lineText = doc.lineAt(1).text;
        const start = lineText.indexOf('<>');
        const range = new vscode.Range(1, start, 1, start + 2);
        const diag = diagnostic(range, "Use '!=' instead of '<>'", 'not_preferred_operator');

        const provider = new SSLCodeActionProvider();
        const actions = provider.provideCodeActions(doc, range, context(diag), {} as vscode.CancellationToken);

        const fix = actions.find(a => a.title.includes("'<>'") && a.title.includes("'!='"));
        assert.ok(fix, 'expected a replace-operator code action');
        await vscode.workspace.applyEdit(fix.edit!);
        const text = doc.getText();
        assert.ok(text.includes('nA != nB'));
        assert.ok(!text.includes('<>'));
    });

    it('prefer_exitcase: inserts :EXITCASE; before next :CASE', async function () {
        const content = `:PROCEDURE T;
:BEGINCASE;
:CASE x = 1;
nY := 1;
:CASE x = 2;
nY := 2;
:ENDCASE;
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content, fileName: `/test-${Math.random()}.ssl` } as any);
        // Diagnostic on the first :CASE (line 2)
        const range = new vscode.Range(2, 0, 2, 5);
        const diag = diagnostic(range, ":CASE block should end with ':EXITCASE;'", 'prefer_exitcase');

        const provider = new SSLCodeActionProvider();
        const actions = provider.provideCodeActions(doc, range, context(diag), {} as vscode.CancellationToken);

        const fix = actions.find(a => a.title.startsWith("Insert ':EXITCASE;'"));
        assert.ok(fix, 'expected an insert-EXITCASE code action');
        await vscode.workspace.applyEdit(fix.edit!);
        const text = doc.getText();
        // :EXITCASE; must appear between the first body and the second :CASE.
        const idxFirstBody = text.indexOf('nY := 1');
        const idxExitCase = text.indexOf(':EXITCASE;');
        const idxSecondCase = text.indexOf(':CASE x = 2');
        assert.ok(idxFirstBody >= 0 && idxExitCase > idxFirstBody && idxSecondCase > idxExitCase,
            ':EXITCASE; should be between body and next :CASE');
    });
});
