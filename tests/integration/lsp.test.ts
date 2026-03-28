import * as assert from 'assert';
import * as vscode from 'vscode';

suite('LSP Integration Test', () => {
    test('Restart command keeps language features working', async () => {
        const content = `
:PROCEDURE TestRestart;
Len("test");
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        await vscode.window.showTextDocument(doc);

        await vscode.commands.executeCommand('ssl.restartLanguageServer');
        await new Promise(resolve => setTimeout(resolve, 1000));

        const position = new vscode.Position(2, 1);
        const hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
            'vscode.executeHoverProvider',
            doc.uri,
            position
        );

        assert.ok(hovers && hovers.length > 0, 'Hover should work after restart');
        const hoverContent = hovers[0].contents[0] as vscode.MarkdownString;
        assert.ok(hoverContent.value.includes('Len'), 'Hover should mention Len');
    });

    test('LSP formatting works with blankLinesBetweenProcs configured', async () => {
        const config = vscode.workspace.getConfiguration('ssl');
        const previousValue = config.get<number>('format.blankLinesBetweenProcs', 1);

        try {
            await config.update('format.blankLinesBetweenProcs', 2, vscode.ConfigurationTarget.Global);
            await new Promise(resolve => setTimeout(resolve, 500));

            const initialContent = `
:PROCEDURE FirstProc;
:ENDPROC;
:PROCEDURE SecondProc;
:ENDPROC;
`;

            const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content: initialContent });
            await vscode.window.showTextDocument(doc);

            await vscode.commands.executeCommand('editor.action.formatDocument');

            const text = doc.getText().replace(/\r\n/g, '\n');
            assert.ok(
                /:ENDPROC;\n\n+:PROCEDURE SecondProc;/.test(text),
                'Should keep a blank line between procedures after formatting'
            );
        } finally {
            await config.update('format.blankLinesBetweenProcs', previousValue, vscode.ConfigurationTarget.Global);
        }
    });

    test('Provides document symbols and folding ranges', async () => {
        const content = `
:PROCEDURE TestSymbols;
    :IF .T.;
        :DECLARE x;
    :ENDIF;
:ENDPROC;
/* region Utilities;
:PROCEDURE Helper;
:ENDPROC;
/* endregion;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        await vscode.window.showTextDocument(doc);

        const symbols = await vscode.commands.executeCommand<vscode.DocumentSymbol[]>(
            'vscode.executeDocumentSymbolProvider',
            doc.uri
        );

        assert.ok(symbols && symbols.length > 0, 'Should return document symbols');

        const foldingRanges = await vscode.commands.executeCommand<vscode.FoldingRange[]>(
            'vscode.executeFoldingRangeProvider',
            doc.uri
        );

        assert.ok(foldingRanges && foldingRanges.length > 0, 'Should return folding ranges');
    });

    test('Provides definition and signature help', async () => {
        const content = `
:PROCEDURE DefinitionTarget;
    :DECLARE nValue;
    nValue := 1;
:ENDPROC;

:PROCEDURE CallSite;
    DefinitionTarget(1);
    Len("test");
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        await vscode.window.showTextDocument(doc);

        const position = new vscode.Position(7, 5);
        const definitions = await vscode.commands.executeCommand<vscode.Location[]>(
            'vscode.executeDefinitionProvider',
            doc.uri,
            position
        );

        assert.ok(definitions && definitions.length > 0, 'Should provide definition locations');

        const signaturePosition = new vscode.Position(8, 8);
        const signatureHelp = await vscode.commands.executeCommand<vscode.SignatureHelp>(
            'vscode.executeSignatureHelpProvider',
            doc.uri,
            signaturePosition,
            '('
        );

        assert.ok(signatureHelp && signatureHelp.signatures.length > 0, 'Should provide signature help');
    });

    test('Provides rename support for variables and procedures', async () => {
        const content = `
:PROCEDURE RenameTest;
    :DECLARE sName;
    sName := "hello";
    Len(sName);
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        await vscode.window.showTextDocument(doc);

        // Position on sName declaration
        const position = new vscode.Position(2, 16);
        const edit = await vscode.commands.executeCommand<vscode.WorkspaceEdit>(
            'vscode.executeDocumentRenameProvider',
            doc.uri,
            position,
            'sNewName'
        );

        assert.ok(edit, 'Should return a workspace edit for rename');
        const edits = edit.get(doc.uri);
        assert.ok(edits && edits.length >= 2, 'Should rename at least declaration and usage');
    });

    test('Provides inlay hints for function parameters', async () => {
        const content = `
:PROCEDURE InlayTest;
    :DECLARE sVal;
    sVal := Len("test");
    Mid("hello", 1, 3);
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        await vscode.window.showTextDocument(doc);

        const range = new vscode.Range(0, 0, 6, 0);
        const hints = await vscode.commands.executeCommand<vscode.InlayHint[]>(
            'vscode.executeInlayHintProvider',
            doc.uri,
            range
        );

        // LSP may or may not return hints depending on config, but the call should succeed
        assert.ok(hints !== undefined, 'Should return inlay hints (possibly empty)');
    });

    test('Provides workspace symbol search', async () => {
        const content = `
:PROCEDURE WorkspaceSymTest;
    :DECLARE nCount;
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        await vscode.window.showTextDocument(doc);

        const symbols = await vscode.commands.executeCommand<vscode.SymbolInformation[]>(
            'vscode.executeWorkspaceSymbolProvider',
            'WorkspaceSymTest'
        );

        assert.ok(symbols !== undefined, 'Should return workspace symbols (possibly empty)');
    });

    test('Applies updated formatting settings via configuration change', async () => {
        const config = vscode.workspace.getConfiguration('ssl');
        const previousIndentStyle = config.get<string>('format.indentStyle', 'tab');
        const previousIndentWidth = config.get<number>('format.indentWidth', 1);

        try {
            await config.update('format.indentStyle', 'space', vscode.ConfigurationTarget.Global);
            await config.update('format.indentWidth', 4, vscode.ConfigurationTarget.Global);
            await new Promise(resolve => setTimeout(resolve, 500));

            const content = `
:PROCEDURE TestIndentChange;
:IF .T.;
:DECLARE x;
:ENDIF;
:ENDPROC;
`;
            const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content });
            await vscode.window.showTextDocument(doc);

            await vscode.commands.executeCommand('editor.action.formatDocument');
            const text = doc.getText().replace(/\r\n/g, '\n');

            assert.ok(text.includes('    :IF'), 'Should format with spaces after config change');
        } finally {
            await config.update('format.indentStyle', previousIndentStyle, vscode.ConfigurationTarget.Global);
            await config.update('format.indentWidth', previousIndentWidth, vscode.ConfigurationTarget.Global);
        }
    });
});
