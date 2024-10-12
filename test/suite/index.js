"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.run = void 0;
const path = require("path");
const mocha_1 = require("mocha");
const glob_1 = require("glob");
function run() {
    const mocha = new mocha_1.default({
        ui: "tdd",
        color: true,
    });
    const testsRoot = path.resolve(__dirname, "..");
    return new Promise((c, e) => {
        (0, glob_1.glob)("**/**.test.js", { cwd: testsRoot })
            .then((files) => {
            // Add files to the test suite
            files.forEach((f) => mocha.addFile(path.resolve(testsRoot, f)));
            try {
                // Run the mocha test
                mocha.run((failures) => {
                    if (failures > 0) {
                        e(new Error(`${failures} tests failed.`));
                    }
                    else {
                        c();
                    }
                });
            }
            catch (err) {
                console.error(err);
                e(err);
            }
        })
            .catch(e);
    });
}
exports.run = run;
//# sourceMappingURL=index.js.map