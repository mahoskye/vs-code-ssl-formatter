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

    test("SSLFoldingProvider handles nested regions", async () => {
        const provider = new SSLFoldingProvider();
        const document = await vscode.workspace.openTextDocument({
            content: `
:REGION Outer Region;
    :REGION Inner Region;
        :PROCEDURE NestedProc;
            :IF x > 0;
                y := 1;
            :ENDIF;
        :ENDPROC;
    :ENDREGION;
:ENDREGION;
            `,
            language: "ssl",
        });

        const foldingRanges = provider.provideFoldingRanges(document, {}, new vscode.CancellationTokenSource().token);

        // Should have: outer region, inner region, procedure, if block
        assert.ok(foldingRanges.length >= 4, `Should provide at least 4 nested folding ranges, found ${foldingRanges.length}`);

        // Find outer and inner regions (they may be in any order in the array)
        const regionRanges = foldingRanges.filter(r => r.start <= 1 || r.end >= 8);
        assert.ok(regionRanges.length >= 2, "Should have at least 2 region ranges");
        
        // Verify there's a range that spans most lines (outer region)
        const outerRegion = foldingRanges.find(r => r.start <= 1 && r.end >= 8);
        // And a smaller nested range (inner region)  
        const innerRegion = foldingRanges.find(r => r.start >= 2 && r.start <= 3 && r.end >= 7 && r.end <= 8);
        
        assert.ok(outerRegion, "Should have outer region spanning most lines");
        assert.ok(innerRegion, "Should have inner region");
        assert.ok(outerRegion.start < innerRegion.start, "Outer region should start before inner");
    });

    test("SSLFoldingProvider handles multiple sequential regions", async () => {
        const provider = new SSLFoldingProvider();
        const document = await vscode.workspace.openTextDocument({
            content: `
:REGION First Region;
    x := 1;
:ENDREGION;
:REGION Second Region;
    y := 2;
:ENDREGION;
:REGION Third Region;
    z := 3;
:ENDREGION;
            `,
            language: "ssl",
        });

        const foldingRanges = provider.provideFoldingRanges(document, {}, new vscode.CancellationTokenSource().token);

        assert.strictEqual(foldingRanges.length, 3, "Should provide 3 sequential folding ranges");

        // Verify they don't overlap
        for (let i = 0; i < foldingRanges.length - 1; i++) {
            assert.ok(foldingRanges[i].end < foldingRanges[i + 1].start,
                `Region ${i} should end before region ${i + 1} starts`);
        }
    });

    test("SSLFoldingProvider handles unclosed regions gracefully", async () => {
        const provider = new SSLFoldingProvider();
        const document = await vscode.workspace.openTextDocument({
            content: `
:PROCEDURE TestProc;
    :IF x > 0;
        y := 1;
    /* Missing :ENDIF;
/* Missing :ENDPROC;
:REGION Unclosed Region;
    z := 3;
            `,
            language: "ssl",
        });

        // Should not throw error
        const foldingRanges = provider.provideFoldingRanges(document, {}, new vscode.CancellationTokenSource().token);

        // Should provide ranges for what can be folded
        assert.ok(Array.isArray(foldingRanges), "Should return an array even with unclosed blocks");
    });

    test("SSLFoldingProvider handles empty regions", async () => {
        const provider = new SSLFoldingProvider();
        const document = await vscode.workspace.openTextDocument({
            content: `
:REGION Empty Region;
:ENDREGION;
:PROCEDURE EmptyProc;
:ENDPROC;
            `,
            language: "ssl",
        });

        const foldingRanges = provider.provideFoldingRanges(document, {}, new vscode.CancellationTokenSource().token);

        assert.strictEqual(foldingRanges.length, 2, "Should provide folding ranges for empty blocks");
    });

    test("SSLFoldingProvider handles comment regions correctly", async () => {
        const provider = new SSLFoldingProvider();
        const document = await vscode.workspace.openTextDocument({
            content: `
/*region Database Operations
/*endregion;
/*region UI Functions
/*endregion;
            `,
            language: "ssl",
        });

        const foldingRanges = provider.provideFoldingRanges(document, {}, new vscode.CancellationTokenSource().token);

        assert.strictEqual(foldingRanges.length, 2, "Should provide 2 comment region folding ranges");
        assert.strictEqual(foldingRanges[0].kind, vscode.FoldingRangeKind.Region,
            "Comment region should have Region kind");
        assert.strictEqual(foldingRanges[1].kind, vscode.FoldingRangeKind.Region,
            "Comment region should have Region kind");
    });

    test("SSLFoldingProvider handles TRY/CATCH blocks", async () => {
        const provider = new SSLFoldingProvider();
        const document = await vscode.workspace.openTextDocument({
            content: `
:TRY;
    DoSomething();
:CATCH;
    HandleError();
:ENDTRY;
            `,
            language: "ssl",
        });

        const foldingRanges = provider.provideFoldingRanges(document, {}, new vscode.CancellationTokenSource().token);

        // Should provide folding for the TRY block (before CATCH)
        assert.ok(foldingRanges.length > 0, "Should provide folding ranges for TRY/CATCH");
    });

    test("SSLFoldingProvider handles all control structures", async () => {
        const provider = new SSLFoldingProvider();
        const document = await vscode.workspace.openTextDocument({
            content: `
:IF x > 0;
    y := 1;
:ENDIF;
:WHILE x < 10;
    x := x + 1;
:ENDWHILE;
:FOR i := 1 TO 10;
    DoSomething(i);
:NEXT;
:BEGINCASE;
    :CASE x = 1;
        DoOne();
:ENDCASE;
            `,
            language: "ssl",
        });

        const foldingRanges = provider.provideFoldingRanges(document, {}, new vscode.CancellationTokenSource().token);

        // Should have: IF, WHILE, FOR, BEGINCASE
        assert.strictEqual(foldingRanges.length, 4,
            `Should provide folding for all 4 control structures, found ${foldingRanges.length}`);
    });
});
