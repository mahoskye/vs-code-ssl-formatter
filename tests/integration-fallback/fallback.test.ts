import * as assert from 'assert';
import * as vscode from 'vscode';

suite('Fallback Integration Test', () => {
    suiteSetup(async () => {
        const extension = vscode.extensions.getExtension('mahoskye.vs-code-ssl-formatter');
        await extension?.activate();
    });

    test('Native providers respond when LSP is disabled', async () => {
        const config = vscode.workspace.getConfiguration('ssl');
        const lspEnabled = config.get<boolean>('languageServer.enabled', true);
        assert.strictEqual(lspEnabled, false, 'Language server should be disabled for fallback tests');

        const content = `
:PROCEDURE FallbackHover;
Len("test");
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        await vscode.window.showTextDocument(doc);

        const hoverPosition = new vscode.Position(2, 1);
        const hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
            'vscode.executeHoverProvider',
            doc.uri,
            hoverPosition
        );

        assert.ok(hovers && hovers.length > 0, 'Hover should work in fallback mode');

        const completionPosition = new vscode.Position(1, 0);
        const completions = await vscode.commands.executeCommand<vscode.CompletionList>(
            'vscode.executeCompletionItemProvider',
            doc.uri,
            completionPosition,
            ':'
        );

        assert.ok(completions && completions.items.length > 0, 'Completion should work in fallback mode');
    });

    test('Rename works when LSP is disabled', async () => {
        const content = `
:PROCEDURE FallbackRename;
    :DECLARE sName;
    sName := "test";
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        await vscode.window.showTextDocument(doc);

        const position = new vscode.Position(2, 16);
        const edit = await vscode.commands.executeCommand<vscode.WorkspaceEdit>(
            'vscode.executeDocumentRenameProvider',
            doc.uri,
            position,
            'sNewName'
        );

        assert.ok(edit, 'Rename should work in fallback mode');
    });

    test('Inlay hints work when LSP is disabled', async () => {
        const content = `
:PROCEDURE FallbackInlay;
    Len("test");
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        await vscode.window.showTextDocument(doc);

        const range = new vscode.Range(0, 0, 4, 0);
        const hints = await vscode.commands.executeCommand<vscode.InlayHint[]>(
            'vscode.executeInlayHintProvider',
            doc.uri,
            range
        );

        assert.ok(hints !== undefined, 'Inlay hints should work in fallback mode');
    });

    test('Formatting works when LSP is disabled', async () => {
        const content = `
:PROCEDURE FallbackFormat;
:IF .T.;
:DECLARE x;
:ENDIF;
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        await vscode.window.showTextDocument(doc);

        await vscode.commands.executeCommand('editor.action.formatDocument');
        const text = doc.getText();

        assert.ok(text.includes('\t:IF') || text.includes('    :IF'), 'Formatting should indent in fallback mode');
    });
});
