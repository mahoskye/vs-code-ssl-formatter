
import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';
import { describe, it } from 'mocha';

import { activate } from '../../src/extension';

describe('Diagnostic Lifecycle Integration Test', () => {

    it('Diagnostics persist for other files when one is closed', async () => {
        // Activate extension to register listeners
        const context: any = { subscriptions: [], extensionUri: vscode.Uri.file('/') };
        await activate(context);

        // Document A content (has error)
        const contentA = `
:PROCEDURE TestA;
    // Missing semicolon error
    variable x
:ENDPROC;
`;
        // Document B content (has error)
        const contentB = `
:PROCEDURE TestB;
    // Undefined variable error
    y := 1;
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
