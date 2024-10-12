import * as assert from "assert";
import * as vscode from "vscode";
import { SSLFoldingProvider } from "../src/sslFoldingProvider";

suite("SSL Extension Test Suite", () => {
    vscode.window.showInformationMessage("Start all tests.");

    test("SSLFoldingProvider Initialization", () => {
        assert.doesNotThrow(() => new SSLFoldingProvider());
    });

    test("SSLFoldingProvider provides folding ranges", async () => {
        const provider = new SSLFoldingProvider();
        const document = await vscode.workspace.openTextDocument({
            content: `
/*region Test Region
Some code here
/*endregion;
:PROCEDURE TestProc
    Some procedure code
:ENDPROC
            `,
            language: "ssl",
        });

        const foldingRanges = provider.provideFoldingRanges(document, {}, new vscode.CancellationTokenSource().token);

        assert.strictEqual(foldingRanges.length, 2, "Should provide 2 folding ranges");
        assert.strictEqual(foldingRanges[0].kind, vscode.FoldingRangeKind.Region, "First range should be a Region");
        assert.strictEqual(foldingRanges[1].kind, undefined, "Second range should not have a specific kind");
    });
});
