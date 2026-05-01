import * as assert from 'assert';
import * as vscode from 'vscode';

/**
 * End-to-end exercise of the LSP -> middleware -> code-action chain.
 * Opens a file with a known violation, waits for the LSP to publish a
 * coded diagnostic, asks VS Code for code actions at the diagnostic
 * range, applies the first one, and asserts the resulting document.
 *
 * Picks `udobject_array_in_clause` as the canary because:
 *   - it's a single-emit diagnostic with a stable slug,
 *   - the fix produces a unique signature in the output (`:DECLARE` + a new
 *     placeholder name) we can assert without hand-tracing the LSP's range,
 *   - it's been wired since the first round of code actions.
 */
suite('Code Actions Integration Test', () => {

    test('udobject_array_in_clause: lightbulb fix extracts UDObject prop to local', async function () {
        // Activation, LSP startup, and first diagnostic round can take a few
        // seconds on slower CI runners.
        this.timeout(20000);

        const content = `:PROCEDURE TestExtract;
SqlExecute("SELECT * FROM t WHERE id IN (?o:Items?)");
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        await vscode.window.showTextDocument(doc);

        // Wait for the LSP to publish at least one diagnostic with the
        // expected code. Poll up to 10 s; each tick waits for either the
        // initial publish or any subsequent change event.
        const targetCode = 'udobject_array_in_clause';
        const deadline = Date.now() + 10000;
        let target: vscode.Diagnostic | undefined;
        while (Date.now() < deadline) {
            const diags = vscode.languages.getDiagnostics(doc.uri);
            target = diags.find(d => stringifyCode(d.code) === targetCode);
            if (target) {
                break;
            }
            await new Promise(r => setTimeout(r, 200));
        }
        assert.ok(target, `LSP should emit a '${targetCode}' diagnostic within 10s`);

        const actions = await vscode.commands.executeCommand<vscode.CodeAction[]>(
            'vscode.executeCodeActionProvider',
            doc.uri,
            target!.range
        );
        assert.ok(actions && actions.length > 0,
            `Expected at least one code action at the diagnostic range`);

        const fix = actions.find(a => a.title.startsWith('Extract '));
        assert.ok(fix, 'expected the extract-to-local fix to be offered');
        assert.ok(fix.edit, 'fix should carry a WorkspaceEdit');

        await vscode.workspace.applyEdit(fix.edit!);
        const after = doc.getText();
        assert.ok(after.includes(':DECLARE aItems;'),
            `After fix, document should declare the new local. Got:\n${after}`);
        assert.ok(after.includes('aItems := o:Items;'),
            `After fix, document should assign the property. Got:\n${after}`);
        assert.ok(after.includes('?aItems?'),
            `After fix, the SQL placeholder should reference the local. Got:\n${after}`);
        assert.ok(!after.includes('?o:Items?'),
            `Original placeholder should be gone. Got:\n${after}`);
    });
});

function stringifyCode(code: vscode.Diagnostic['code']): string | undefined {
    if (typeof code === 'string') {
        return code;
    }
    if (typeof code === 'number') {
        return String(code);
    }
    if (code && typeof code === 'object' && 'value' in code) {
        const v = (code as { value: string | number }).value;
        return typeof v === 'string' ? v : String(v);
    }
    return undefined;
}
