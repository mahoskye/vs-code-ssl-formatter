
import * as assert from 'assert';
import * as vscode from 'vscode';

suite('Formatting Integration Test', () => {

        test('Formats SSL code with correct indentation', async () => {
                // Explicitly format with Tabs
                const config = vscode.workspace.getConfiguration('ssl');
                const editorConfig = vscode.workspace.getConfiguration('editor');
                await config.update('format.indentStyle', 'tab', vscode.ConfigurationTarget.Global);
                await editorConfig.update('insertSpaces', false, vscode.ConfigurationTarget.Global);
                await editorConfig.update('tabSize', 4, vscode.ConfigurationTarget.Global);

                const initialContent = `
:PROCEDURE TestFormat;
:IF .T.;
:DECLARE x;
x := 1;
:ENDIF;
:ENDPROC;
`;
                // Expect tabs. 
                // Logic:
                // :PROCEDURE (0)
                // 	:IF (1)
                // 		:DECLARE x (2)
                // 		x := 1 (2)
                // 	:ENDIF (1)
                // :ENDPROC (0)
                const expectedContent = `
:PROCEDURE TestFormat;
	:IF .T.;
		:DECLARE x;

		x := 1;
	:ENDIF;
:ENDPROC;
`;
                const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content: initialContent });
                await vscode.window.showTextDocument(doc);

                // Execute Format Document
                await vscode.commands.executeCommand('editor.action.formatDocument');

                const text = doc.getText();

                // Check if we got tabs
                assert.ok(text.includes('\t:IF'), 'Should indent :IF with tab');
                assert.ok(text.includes('\t\t:DECLARE'), 'Should indent body with 2 tabs');

                // Normalize newlines for cross-platform strict check
                assert.strictEqual(text.trim().replace(/\r\n/g, '\n'), expectedContent.trim().replace(/\r\n/g, '\n'));
        });

        test('Formats SQL strings inside RunSQL', async () => {
                // Enforce SQL formatting enabled
                const config = vscode.workspace.getConfiguration('ssl');
                await config.update('format.sql.enabled', true, vscode.ConfigurationTarget.Global);

                const initialContent = `
:PROCEDURE TestSqlFormat;
RunSQL("SELECT * FROM table WHERE id = 1");
:ENDPROC;
`;
                // Expect standard formatting (canonical compact is default)
                // SELECT * FROM table WHERE id = 1  -->  SELECT * FROM table WHERE id = 1 (might stay one line if short)
                // Let's try a strict expected output. The formatter usually upper-cases keywords.
                // It might not wrap if it's short.

                // Let's rely on checking if it CHANGED properly, e.g. uppercasing if lowercase was used?
                // But the input is already upper.
                // Let's use lower case input.

                const inputLower = `
:PROCEDURE TestSqlFormat;
RunSQL("select * from table where id = 1");
:ENDPROC;
`;

                const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content: inputLower });
                await vscode.window.showTextDocument(doc);

                await vscode.commands.executeCommand('editor.action.formatDocument');
                const text = doc.getText();

                // Should uppercase keywords: SELECT * FROM TABLE WHERE ID = 1
                // Note: Table/Column names might preserve case depending on settings, but keywords verify formatter ran.
                assert.ok(text.includes('SELECT'), 'SQL keywords should be uppercased');
                assert.ok(text.includes('FROM'), 'SQL keywords should be uppercased');
        });
});
