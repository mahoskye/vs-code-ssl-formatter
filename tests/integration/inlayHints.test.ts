
import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';

suite('Inlay Hints Integration Test', () => {
    vscode.window.showInformationMessage('Start Inlay Hints tests.');

    test('Inlay hints update when moving cursor', async () => {
        // Create a new document with content
        const content = `
:PROCEDURE Test1;
Len("test");
:ENDPROC;

:PROCEDURE Test2;
Len("test2");
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        const editor = await vscode.window.showTextDocument(doc);

        // Wait for extension to activate
        const ext = vscode.extensions.getExtension('mahoskye.vs-code-ssl-formatter');
        if (ext && !ext.isActive) {
            await ext.activate();
        }

        // Configure settings to show on active line only
        const config = vscode.workspace.getConfiguration('ssl');
        await config.update('intellisense.inlayHints.enabled', true, vscode.ConfigurationTarget.Global);
        await config.update('intellisense.inlayHints.parameterNames', true, vscode.ConfigurationTarget.Global);
        await config.update('intellisense.inlayHints.showOnActiveLineOnly', true, vscode.ConfigurationTarget.Global);

        // Move cursor to line 2 (Test1)
        editor.selection = new vscode.Selection(2, 0, 2, 0);

        // Wait for potential debounce
        await new Promise(resolve => setTimeout(resolve, 100));

        // Get inlay hints
        const hints1 = await vscode.commands.executeCommand<vscode.InlayHint[]>(
            'vscode.executeInlayHintProvider',
            doc.uri,
            new vscode.Range(0, 0, doc.lineCount, 0)
        );

        // Should have hint on line 2
        const hintsOnLine2 = hints1.filter(h => h.position.line === 2);
        const hintsOnLine6 = hints1.filter(h => h.position.line === 6);

        assert.ok(hintsOnLine2.length > 0, 'Should have hints on active line 2');
        assert.strictEqual(hintsOnLine6.length, 0, 'Should NOT have hints on inactive line 6');

        // Move cursor to line 6 (Test2)
        editor.selection = new vscode.Selection(6, 0, 6, 0);

        // Wait for debounce and refresh
        await new Promise(resolve => setTimeout(resolve, 100));

        // Get inlay hints again
        const hints2 = await vscode.commands.executeCommand<vscode.InlayHint[]>(
            'vscode.executeInlayHintProvider',
            doc.uri,
            new vscode.Range(0, 0, doc.lineCount, 0)
        );

        const hintsOnLine2_after = hints2.filter(h => h.position.line === 2);
        const hintsOnLine6_after = hints2.filter(h => h.position.line === 6);

        // Assert updates
        assert.strictEqual(hintsOnLine2_after.length, 0, 'Should NOT have hints on inactive line 2');
        assert.ok(hintsOnLine6_after.length > 0, 'Should have hints on active line 6');
    });
});
