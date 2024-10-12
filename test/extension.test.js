"use strict";
var awaiter =
    (this && this.awaiter) ||
    function (thisArg, _arguments, promise, generator) {
        function adopt(value) {
            return value instanceof promise
                ? value
                : new promise(function (resolve) {
                      resolve(value);
                  });
        }
        return new (promise || (promise = Promise))(function (resolve, reject) {
            function fulfilled(value) {
                try {
                    step(generator.next(value));
                } catch (e) {
                    reject(e);
                }
            }
            function rejected(value) {
                try {
                    step(generator["throw"](value));
                } catch (e) {
                    reject(e);
                }
            }
            function step(result) {
                result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected);
            }
            step((generator = generator.apply(thisArg, _arguments || [])).next());
        });
    };
Object.defineProperty(exports, "__esModule", { value: true });
const assert = require("assert");
const vscode = require("vscode");
const sslFoldingProvider = require("../src/sslFoldingProvider");
suite("SSL Extension Test Suite", () => {
    vscode.window.showInformationMessage("Start all tests.");
    test("SSLFoldingProvider Initialization", () => {
        assert.doesNotThrow(() => new sslFoldingProvider.SSLFoldingProvider());
    });
    test("SSLFoldingProvider provides folding ranges", () =>
        __awaiter(void 0, void 0, void 0, function* () {
            const provider = new sslFoldingProvider.SSLFoldingProvider();
            const document = yield vscode.workspace.openTextDocument({
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
            const foldingRanges = provider.provideFoldingRanges(
                document,
                {},
                new vscode.CancellationTokenSource().token
            );
            assert.strictEqual(foldingRanges.length, 2, "Should provide 2 folding ranges");
            assert.strictEqual(foldingRanges[0].kind, vscode.FoldingRangeKind.Region, "First range should be a Region");
            assert.strictEqual(foldingRanges[1].kind, undefined, "Second range should not have a specific kind");
        }));
});
//# sourceMappingURL=extension.test.js.map
