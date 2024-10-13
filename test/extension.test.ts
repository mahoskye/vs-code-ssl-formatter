import * as assert from "assert";
import * as vscode from "vscode";
import { SSLFoldingProvider } from "../src/sslFoldingProvider";
import { SSLCompletionProvider } from "../src/sslCompletionProvider";

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

    test("SSLCompletionProvider Initialization", () => {
        assert.doesNotThrow(() => new SSLCompletionProvider());
    });

    test("SSLCompletionProvider provides completion items", async () => {
        const provider = new SSLCompletionProvider();
        const document = await vscode.workspace.openTextDocument({
            content: "example",
            language: "ssl",
        });

        const position = new vscode.Position(0, 7); // At the end of "example"
        const completionItems = await provider.provideCompletionItems(
            document,
            position,
            new vscode.CancellationTokenSource().token,
            { triggerKind: vscode.CompletionTriggerKind.Invoke, triggerCharacter: undefined }
        );

        assert.ok(Array.isArray(completionItems), "Should return an array of completion items");
        assert.ok(completionItems.length > 0, "Should provide at least one completion item");

        const exampleFunction = completionItems.find((item) => item.label === "exampleFunction()");
        assert.ok(exampleFunction, "Should include the example function");
        assert.strictEqual(
            exampleFunction.kind,
            vscode.CompletionItemKind.Function,
            "Example function should be of kind Function"
        );
    });

    test("Reload command updates completion items", async () => {
        const provider = new SSLCompletionProvider();
        const initialItems = await provider.provideCompletionItems(
            await vscode.workspace.openTextDocument({ content: "", language: "ssl" }),
            new vscode.Position(0, 0),
            new vscode.CancellationTokenSource().token,
            { triggerKind: vscode.CompletionTriggerKind.Invoke, triggerCharacter: undefined }
        );

        // Ensure completions is an object with arrays as values (grouped by categories)
        const completions = vscode.workspace.getConfiguration("sslFormatter").get<any>("completions", {});
        if (typeof completions !== "object" || completions === null) {
            throw new Error("Completions configuration is not an object.");
        }

        // Simulate adding a new completion item under a category
        const updatedCompletions = {
            ...completions,
            "Miscellaneous Functions": [
                ...(completions["Miscellaneous Functions"] || []),
                {
                    label: "newTestFunction()",
                    kind: "function",
                    detail: "A new test function",
                    documentation: "This function was added for testing purposes.",
                },
            ],
        };

        await vscode.workspace
            .getConfiguration("sslFormatter")
            .update("completions", updatedCompletions, vscode.ConfigurationTarget.Global);

        // Trigger the reload command
        await vscode.commands.executeCommand("sslFormatter.reloadCompletions");

        const updatedItems = await provider.provideCompletionItems(
            await vscode.workspace.openTextDocument({ content: "", language: "ssl" }),
            new vscode.Position(0, 0),
            new vscode.CancellationTokenSource().token,
            { triggerKind: vscode.CompletionTriggerKind.Invoke, triggerCharacter: undefined }
        );

        assert.ok(Array.isArray(initialItems), "Initial items should be an array");
        assert.ok(Array.isArray(updatedItems), "Updated items should be an array");
        assert.strictEqual(
            updatedItems.length,
            (initialItems?.length || 0) + 1,
            "Should have one more completion item after reload"
        );
        const newFunction = updatedItems.find((item) => item.label === "newTestFunction()");
        assert.ok(newFunction, "Should include the newly added function");

        // Clean up: remove the added completion item
        const currentCompletions = vscode.workspace.getConfiguration("sslFormatter").get<any>("completions", {});
        const cleanedUpCompletions = {
            ...currentCompletions,
            "Miscellaneous Functions": (currentCompletions["Miscellaneous Functions"] || []).filter(
                (item: any) => item.label !== "newTestFunction()"
            ),
        };

        await vscode.workspace
            .getConfiguration("sslFormatter")
            .update("completions", cleanedUpCompletions, vscode.ConfigurationTarget.Global);
    });
});
interface CompletionItem {
    label: string;
    kind: string;
    detail: string;
    documentation: string;
}
