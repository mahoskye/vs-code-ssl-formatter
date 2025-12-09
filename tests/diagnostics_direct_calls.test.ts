
import * as assert from 'assert';
import * as vscode from 'vscode';
import { activate } from '../src/extension';
import { describe, it } from 'mocha';
import { SSLHoverProvider } from '../src/sslHoverProvider';
import { SSLDiagnosticProvider } from '../src/sslDiagnosticProvider';
import { WorkspaceProcedureIndex } from '../src/utils/procedureIndex';

describe('Diagnostic Reproduction: Invalid Direct Calls', () => {

    it('Should flag direct calls to user procedures', async function () {
        this.timeout(5000);
        const content = `
:PROCEDURE MyTest;
    variable result;
    // Valid calls
    result := AAdd({1}, 2);
    ExecFunction("MyOtherProc");
    DoProc({ "MyOtherProc" });

    // Invalid call (User Procedure)
    MyOtherProc(1);
:ENDPROC;

:PROCEDURE MyOtherProc;
:ENDPROC;
`;

        // Setup Index and Provider
        const index = new WorkspaceProcedureIndex();
        await index.initialize();
        const provider = new SSLHoverProvider(undefined, index);

        const doc = await vscode.workspace.openTextDocument({ language: 'ssl', content });
        // Wait for index to pick up the changes via listeners
        await new Promise(resolve => setTimeout(resolve, 500));

        const diagnostics = vscode.languages.getDiagnostics(doc.uri);
        // Note: Diagnostics come from the extension's DiagnosticProvider which listens to events too, 
        // OR we might need to rely on the fact that activate() was called?
        // Actually, for getDiagnostics to work, we rely on the extension's provider.
        // But for Hover, we use our local provider instance.

        await new Promise(resolve => setTimeout(resolve, 1000)); // buffer for diagnostic generation

        const diagProvider = new SSLDiagnosticProvider();
        diagProvider.updateDiagnostics(doc);

        const diags = vscode.languages.getDiagnostics(doc.uri);
        const invCall = diags.find(d => d.message.includes('Procedures must be called using DoProc or ExecFunction'));
        assert.ok(invCall, 'Should report error for direct procedure call MyOtherProc(1)');

        // Check Hover
        const hoverResult = provider.provideHover(doc, new vscode.Position(9, 6), {} as any);
        assert.ok(hoverResult, 'Should provide hover');
        const contents = hoverResult.contents as vscode.MarkdownString[]; // Hover can be MarkedString | ... but usually array or MDString
        // VSCode API Mock might vary, but in source we return 'new vscode.Hover(md, range)'
        // The mock Hover implementation stores contents.

        const mdValue = (contents as any).value || (contents as any).content || (contents as any)[0]?.value || ""; // Handle mock structure
        assert.ok(mdValue.includes('must be called using `DoProc`'), 'Hover should warn about usage');
    });
});
