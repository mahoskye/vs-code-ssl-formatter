
import * as assert from 'assert';
import * as vscode from 'vscode';

suite('Hover Integration Test', () => {

    test('Provides hover for built-in functions', async () => {
        const content = `
:PROCEDURE TestHover;
variable str;
str = "test";
Len(str);
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        await vscode.window.showTextDocument(doc);

        // Hover over 'Len'
        // Line 4, Col 0-3
        // Position(4, 1)
        const position = new vscode.Position(4, 1);

        // Wait for extension activation? It should be active.

        // Execute Hover Provider
        const hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
            'vscode.executeHoverProvider',
            doc.uri,
            position
        );

        // Assert we got a hover
        assert.ok(hovers && hovers.length > 0, 'Should provide hover for Len');

        // Assert content
        const hoverContent = hovers[0].contents[0] as vscode.MarkdownString;
        assert.ok(hoverContent.value.includes('Len'), 'Hover should mention Len');
        assert.ok(hoverContent.value.includes('string'), 'Hover should mention return type');
    });

    test('Provides docstrings for built-in functions', async () => {
        const content = `
:PROCEDURE TestHoverDocs;
RunSQL("...");
:ENDPROC;
`;
        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        await vscode.window.showTextDocument(doc);

        const position = new vscode.Position(2, 2); // 'RunSQL'
        const hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
            'vscode.executeHoverProvider',
            doc.uri,
            position
        );

        assert.ok(hovers && hovers.length > 0, 'Should provide hover for RunSQL');
        const hoverContent = hovers[0].contents[0] as vscode.MarkdownString;
        assert.ok(hoverContent.value.includes('Executes a SQL command'), 'Hover should contain function description');
    });
});
