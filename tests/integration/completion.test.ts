
import * as assert from 'assert';
import * as vscode from 'vscode';

suite('Completion Integration Test', () => {

    test('Provides completion for keywords', async () => {
        const content = `
:PROCEDURE TestCompletion;
PR
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        await vscode.window.showTextDocument(doc);

        // Position at end of 'PR'
        const position = new vscode.Position(2, 2);

        const list = await vscode.commands.executeCommand<vscode.CompletionList>(
            'vscode.executeCompletionItemProvider',
            doc.uri,
            position
        );

        assert.ok(list && list.items.length > 0, 'Should provide completions');
        const item = list.items.find(i => i.label === 'PROCEDURE'); // Or :PROCEDURE depending on how provider returns it
        // Keywords usually returned as names.
        // Actually, if we are inside PROCEDURE, maybe relevant keywords?
        // But 'PR' matches PROCEDURE.
        // Also PRINT?

        // Let's look for known keywords
        assert.ok(list.items.some(i => i.label === 'PROCEDURE' || i.label === ':PROCEDURE'), 'Should suggest PROCEDURE');
    });

    test('Provides completion for built-in functions', async () => {
        const content = `
:PROCEDURE TestCompletionFunc;
Str
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        await vscode.window.showTextDocument(doc);

        const position = new vscode.Position(2, 3); // 'Str'

        const list = await vscode.commands.executeCommand<vscode.CompletionList>(
            'vscode.executeCompletionItemProvider',
            doc.uri,
            position
        );

        assert.ok(list.items.some(i => i.label === 'StrTran'), 'Should suggest StrTran');
        assert.ok(list.items.some(i => i.label === 'Str'), 'Should suggest Str');
    });

    test('Provides completion for local procedures', async () => {
        const content = `
:PROCEDURE MyLocalProc;
:ENDPROC;

:PROCEDURE TestCompletionProc;
MyLo
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        await vscode.window.showTextDocument(doc);

        const position = new vscode.Position(5, 4); // 'MyLo'

        const list = await vscode.commands.executeCommand<vscode.CompletionList>(
            'vscode.executeCompletionItemProvider',
            doc.uri,
            position
        );

        assert.ok(list.items.some(i => i.label === 'MyLocalProc'), 'Should suggest MyLocalProc');
    });

    test('Provides completion for DoProc', async () => {
        const content = `
:PROCEDURE TargetProc;
:ENDPROC;

:PROCEDURE TestDoProc;
DoProc("Target
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        await vscode.window.showTextDocument(doc);

        const position = new vscode.Position(5, 14); // 'Target' inside quotes

        const list = await vscode.commands.executeCommand<vscode.CompletionList>(
            'vscode.executeCompletionItemProvider',
            doc.uri,
            position
        );

        assert.ok(list.items.some(i => i.label === 'TargetProc'), 'Should suggest TargetProc inside DoProc');
    });
});
