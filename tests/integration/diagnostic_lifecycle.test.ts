
import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';



suite('Diagnostic Lifecycle Integration Test', () => {

    test('Diagnostics persist for other files when one is closed', async () => {
        // Extension is activated automatically when opening SSL files

        // Document A content (has error - undefined SQL parameter)
        const contentA = `
:PROCEDURE TestA;
    SQLExecute("SELECT * FROM table WHERE id = ?sUndefinedVar?");
:ENDPROC;
`;
        // Document B content (has error - undeclared variable usage)
        const contentB = `
:PROCEDURE TestB;
    :DECLARE sResult;
    sResult := sUndeclaredVar + 1;
:ENDPROC;
`;

        // Open Doc A
        const docA = await vscode.workspace.openTextDocument({ language: 'ssl', content: contentA });
        await vscode.window.showTextDocument(docA);

        // Wait for A diagnostics
        await new Promise(resolve => setTimeout(resolve, 1000));
        const diagnosticsA_Initial = vscode.languages.getDiagnostics(docA.uri);
        assert.ok(diagnosticsA_Initial.length > 0, 'Doc A should have diagnostics initially');

        // Open Doc B
        const docB = await vscode.workspace.openTextDocument({ language: 'ssl', content: contentB });
        await vscode.window.showTextDocument(docB);

        // Wait for B diagnostics
        await new Promise(resolve => setTimeout(resolve, 1000));
        const diagnosticsB_Initial = vscode.languages.getDiagnostics(docB.uri);
        assert.ok(diagnosticsB_Initial.length > 0, 'Doc B should have diagnostics initially');

        // Verify A still has diagnostics
        const diagnosticsA_AfterOpenB = vscode.languages.getDiagnostics(docA.uri);
        assert.strictEqual(diagnosticsA_AfterOpenB.length, diagnosticsA_Initial.length, 'Doc A diagnostics should persist after opening Doc B');

        // Close Doc A
        // Note: 'vscode.commands.executeCommand("workbench.action.closeActiveEditor")' closes active.
        // We need to be careful about which is active. 
        // showTextDocument(docB) made B active.

        // Let's close B first since it is active
        await vscode.commands.executeCommand('workbench.action.closeActiveEditor');

        // Wait for clean up
        await new Promise(resolve => setTimeout(resolve, 1000));

        // B should be gone
        const diagnosticsB_AfterClose = vscode.languages.getDiagnostics(docB.uri);
        assert.strictEqual(diagnosticsB_AfterClose.length, 0, 'Doc B diagnostics should be removed after close');

        // A should STILL be there
        const diagnosticsA_AfterCloseB = vscode.languages.getDiagnostics(docA.uri);
        assert.ok(diagnosticsA_AfterCloseB.length > 0, 'Doc A diagnostics should persist after closing Doc B');

        // Cleanup A (close it too)
        await vscode.window.showTextDocument(docA);
        await vscode.commands.executeCommand('workbench.action.closeActiveEditor');
    });
});
