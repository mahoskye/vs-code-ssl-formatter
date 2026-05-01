
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

describe('Code Actions: v1.7.0 additions', () => {

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

    async function openDoc(content: string): Promise<vscode.TextDocument> {
        return vscode.workspace.openTextDocument({
            language: 'ssl',
            content,
            fileName: `/test-${Math.random()}.ssl`
        } as any);
    }

    it('class_instantiation_curly: rewrites Foo(args) to Foo{args}', async function () {
        const doc = await openDoc(`x := Email("a@b");\n`);
        const lineText = doc.lineAt(0).text;
        const start = lineText.indexOf('Email');
        const end = lineText.indexOf(')') + 1;
        const range = new vscode.Range(0, start, 0, end);
        const diag = diagnostic(range, "use curly", 'class_instantiation_curly');

        const provider = new SSLCodeActionProvider();
        const actions = provider.provideCodeActions(doc, range, context(diag), {} as vscode.CancellationToken);
        const fix = actions.find(a => a.title.startsWith('Use curly'));
        assert.ok(fix, 'expected curly-brace fix');
        await vscode.workspace.applyEdit(fix.edit!);
        assert.ok(doc.getText().includes('Email{"a@b"}'));
    });

    it('dot_property_access: replaces dot with colon', async function () {
        const doc = await openDoc(`x := obj.prop;\n`);
        const lineText = doc.lineAt(0).text;
        const start = lineText.indexOf('obj.prop');
        const range = new vscode.Range(0, start, 0, start + 'obj.prop'.length);
        const diag = diagnostic(range, "use colon", 'dot_property_access');

        const provider = new SSLCodeActionProvider();
        const actions = provider.provideCodeActions(doc, range, context(diag), {} as vscode.CancellationToken);
        const fix = actions.find(a => a.title.includes("'.'"));
        assert.ok(fix, 'expected dot-to-colon fix');
        await vscode.workspace.applyEdit(fix.edit!);
        assert.ok(doc.getText().includes('obj:prop'));
        assert.ok(!doc.getText().includes('obj.prop'));
    });

    it('step_spacing: inserts a single space before :STEP', async function () {
        const doc = await openDoc(`:FOR i := 1 :TO 10:STEP 2;\n:NEXT;\n`);
        const lineText = doc.lineAt(0).text;
        const start = lineText.indexOf(':STEP');
        const range = new vscode.Range(0, start, 0, start + ':STEP'.length);
        const diag = diagnostic(range, "step needs space", 'step_spacing');

        const provider = new SSLCodeActionProvider();
        const actions = provider.provideCodeActions(doc, range, context(diag), {} as vscode.CancellationToken);
        const fix = actions.find(a => a.title.includes("space before"));
        assert.ok(fix, 'expected step-spacing fix');
        await vscode.workspace.applyEdit(fix.edit!);
        assert.ok(doc.getText().includes(' :STEP'));
    });

    it('comment_termination: appends semicolon when missing', async function () {
        const doc = await openDoc(`/* note text\n:PROCEDURE T;\n:ENDPROC;\n`);
        // Span the comment text on line 0.
        const range = new vscode.Range(0, 0, 0, doc.lineAt(0).text.length);
        const diag = diagnostic(range, "missing terminator", 'comment_termination');

        const provider = new SSLCodeActionProvider();
        const actions = provider.provideCodeActions(doc, range, context(diag), {} as vscode.CancellationToken);
        const fix = actions.find(a => a.title.includes("Terminate"));
        assert.ok(fix, 'expected terminator fix');
        await vscode.workspace.applyEdit(fix.edit!);
        assert.ok(doc.getText().startsWith('/* note text;'));
    });

    it('redeclare_is_noop: deletes the duplicate :DECLARE line', async function () {
        const doc = await openDoc(`:DECLARE x;\n:DECLARE x;\nx := 1;\n`);
        const range = new vscode.Range(1, 0, 1, doc.lineAt(1).text.length);
        const diag = diagnostic(range, "redundant", 'redeclare_is_noop');

        const provider = new SSLCodeActionProvider();
        const actions = provider.provideCodeActions(doc, range, context(diag), {} as vscode.CancellationToken);
        const fix = actions.find(a => a.title.includes('Remove'));
        assert.ok(fix, 'expected remove fix');
        await vscode.workspace.applyEdit(fix.edit!);
        const text = doc.getText();
        // Only one :DECLARE x; remains.
        const declareCount = (text.match(/:DECLARE x;/g) || []).length;
        assert.strictEqual(declareCount, 1);
    });

    it('equals_vs_strict_equals: replaces single = with ==', async function () {
        const doc = await openDoc(`:IF s = "x";\n:ENDIF;\n`);
        const lineText = doc.lineAt(0).text;
        const eqIdx = lineText.indexOf('=', lineText.indexOf('s'));
        const range = new vscode.Range(0, eqIdx, 0, eqIdx + 1);
        const diag = diagnostic(range, "use strict equals", 'equals_vs_strict_equals');

        const provider = new SSLCodeActionProvider();
        const actions = provider.provideCodeActions(doc, range, context(diag), {} as vscode.CancellationToken);
        const fix = actions.find(a => a.title.includes("'=='"));
        assert.ok(fix, 'expected strict-equals fix');
        await vscode.workspace.applyEdit(fix.edit!);
        assert.ok(doc.getText().includes('s == "x"'));
    });

    it('parameters_first: moves :PARAMETERS up after :PROCEDURE', async function () {
        const doc = await openDoc(`:PROCEDURE T;\nnLocal := 1;\n:PARAMETERS sFoo;\n:ENDPROC;\n`);
        const range = new vscode.Range(2, 0, 2, doc.lineAt(2).text.length);
        const diag = diagnostic(range, "must be first", 'parameters_first');

        const provider = new SSLCodeActionProvider();
        const actions = provider.provideCodeActions(doc, range, context(diag), {} as vscode.CancellationToken);
        const fix = actions.find(a => a.title.includes("PARAMETERS"));
        assert.ok(fix, 'expected parameters-first fix');
        await vscode.workspace.applyEdit(fix.edit!);
        const lines = doc.getText().split('\n');
        const procIdx = lines.findIndex(l => /^:PROCEDURE/.test(l));
        const paramsIdx = lines.findIndex(l => /^:PARAMETERS/.test(l));
        assert.strictEqual(paramsIdx, procIdx + 1, ':PARAMETERS should be on the line after :PROCEDURE');
    });

    it('default_after_parameters: moves :DEFAULT up after :PARAMETERS', async function () {
        const doc = await openDoc(`:PROCEDURE T;\n:PARAMETERS sFoo;\nnLocal := 1;\n:DEFAULT sFoo := "hello";\n:ENDPROC;\n`);
        const range = new vscode.Range(3, 0, 3, doc.lineAt(3).text.length);
        const diag = diagnostic(range, "after :PARAMETERS", 'default_after_parameters');

        const provider = new SSLCodeActionProvider();
        const actions = provider.provideCodeActions(doc, range, context(diag), {} as vscode.CancellationToken);
        const fix = actions.find(a => a.title.includes("DEFAULT"));
        assert.ok(fix, 'expected default-after-parameters fix');
        await vscode.workspace.applyEdit(fix.edit!);
        const lines = doc.getText().split('\n');
        const paramsIdx = lines.findIndex(l => /^:PARAMETERS/.test(l));
        const defaultIdx = lines.findIndex(l => /^:DEFAULT/.test(l));
        assert.strictEqual(defaultIdx, paramsIdx + 1, ':DEFAULT should be directly after :PARAMETERS');
    });

    it('nested_iif: rewrites assignment-form IIF into :IF/:ELSE block', async function () {
        const doc = await openDoc(`:PROCEDURE T;\n\tx := IIF(c, a, b);\n:ENDPROC;\n`);
        const lineText = doc.lineAt(1).text;
        const iifStart = lineText.indexOf('IIF(');
        const iifEnd = lineText.lastIndexOf(')') + 1;
        const range = new vscode.Range(1, iifStart, 1, iifEnd);
        const diag = diagnostic(range, "nested IIF", 'nested_iif');

        const provider = new SSLCodeActionProvider();
        const actions = provider.provideCodeActions(doc, range, context(diag), {} as vscode.CancellationToken);
        const fix = actions.find(a => a.title.includes('IIF'));
        assert.ok(fix, 'expected nested-iif refactor');
        assert.ok(fix.kind?.contains(vscode.CodeActionKind.RefactorRewrite),
            'should be a refactor.rewrite kind');
        await vscode.workspace.applyEdit(fix.edit!);
        const text = doc.getText();
        assert.ok(text.includes(':IF c;'));
        assert.ok(text.includes('x := a;'));
        assert.ok(text.includes(':ELSE;'));
        assert.ok(text.includes('x := b;'));
        assert.ok(text.includes(':ENDIF;'));
        assert.ok(!text.includes('IIF('));
    });
});
