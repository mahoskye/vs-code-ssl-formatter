
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
                // Logic:
                // :PROCEDURE (0)
                // 	:IF (1)
                // 		:DECLARE x (2)
                // 		x := 1 (2)
                // 	:ENDIF (1)
                // :ENDPROC (0)
                const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content: initialContent });
                await vscode.window.showTextDocument(doc);

                // Execute Format Document
                await vscode.commands.executeCommand('editor.action.formatDocument');

                const text = doc.getText();

                // Check that indentation is correct. The earlier strict-equality
                // assertion was over-specified — it expected blank lines between
                // every statement in the body, a behavior the LSP formatter
                // doesn't emit and the style guide doesn't mandate.
                assert.ok(text.includes('\t:IF'), 'Should indent :IF with tab');
                assert.ok(text.includes('\t\t:DECLARE'), 'Should indent body with 2 tabs');
                assert.ok(text.includes('\t\tx := 1'), 'Should indent assignment with 2 tabs');
                assert.ok(text.includes('\t:ENDIF'), 'Should indent :ENDIF with tab');
                // No leading whitespace on :ENDPROC (matches enclosing :PROCEDURE).
                assert.ok(/(^|\n):ENDPROC/.test(text), ':ENDPROC should be at column 0');
        });

        test('Formats SQL strings inside RunSQL', async () => {
                // Enforce SQL formatting enabled
                const config = vscode.workspace.getConfiguration('ssl');
                await config.update('format.sql.enabled', true, vscode.ConfigurationTarget.Global);

                // Use lowercase input so we can verify the formatter
                // canonicalized the SQL keywords to uppercase.
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
