
import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';

suite('Inlay Hints Integration Test', () => {
    vscode.window.showInformationMessage('Start Inlay Hints tests.');

    test('Inlay hints appear for all lines', async () => {
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
        await vscode.window.showTextDocument(doc);

        // Wait for extension to activate
        const ext = vscode.extensions.getExtension('mahoskye.vs-code-ssl-formatter');
        if (ext && !ext.isActive) {
            await ext.activate();
        }

        // Configure settings to show hints
        const config = vscode.workspace.getConfiguration('ssl');
        await config.update('intellisense.inlayHints.enabled', true, vscode.ConfigurationTarget.Global);
        await config.update('intellisense.inlayHints.parameterNames', true, vscode.ConfigurationTarget.Global);
        // Ensure showOnActiveLineOnly is OFF (or ignored if removed)
        // await config.update('intellisense.inlayHints.showOnActiveLineOnly', false, vscode.ConfigurationTarget.Global);

        // Wait for ready
        await new Promise(resolve => setTimeout(resolve, 500));

        // Get inlay hints
        const hints = await vscode.commands.executeCommand<vscode.InlayHint[]>(
            'vscode.executeInlayHintProvider',
            doc.uri,
            new vscode.Range(0, 0, doc.lineCount, 0)
        );

        // Should have hints on both lines
        const hintsOnLine2 = hints.filter(h => h.position.line === 2);
        const hintsOnLine6 = hints.filter(h => h.position.line === 6);

        assert.ok(hintsOnLine2.length > 0, 'Should have hints on line 2');
        assert.ok(hintsOnLine6.length > 0, 'Should have hints on line 6');
    });
});
