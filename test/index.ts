import Mocha from "mocha"; // Changed import
import { glob } from "glob"; // Changed import
import * as path from "path";

export function run(): Promise<void> {
    // Parse command line arguments passed from runTest.ts
    const testArgs = process.env.TEST_ARGS ? JSON.parse(process.env.TEST_ARGS) : [];
    const shouldBail = testArgs.includes("--bail");

    // Create the mocha test
    const mocha = new Mocha({
        ui: "tdd",
        color: true,
        bail: shouldBail, // Stop after first failure if --bail is passed
    });

    const testsRoot = path.resolve(__dirname, "..");

    return new Promise<void>(async (c, e) => {
        // Made callback async
        try {
            let filePattern = "**/**.test.js"; // Default pattern

            // Check if specific test files are specified in arguments
            const specificTests = testArgs.filter((arg: string) => !arg.startsWith("--"));
            if (specificTests.length > 0) {
                // If specific test files are provided, use them
                filePattern =
                    specificTests.length === 1
                        ? `**/${specificTests[0]}`
                        : `**/{${specificTests.join(",")}}`;
            }

            console.log(`Running tests with pattern: ${filePattern}`);
            const files = await glob(filePattern, { cwd: testsRoot }); // Used await for glob

            if (files.length === 0) {
                console.warn(`No test files found matching pattern: ${filePattern}`);
                c(); // Complete successfully if no files found
                return;
            }

            console.log(`Found ${files.length} test file(s):`);
            files.forEach((f) => console.log(`  ${f}`));

            // Add files to the test suite
            files.forEach((f: string) => mocha.addFile(path.resolve(testsRoot, f)));

            // Run the mocha test
            mocha.run((failures: number) => {
                if (failures > 0) {
                    e(new Error(`${failures} tests failed.`));
                } else {
                    c();
                }
            });
        } catch (err) {
            console.error(err);
            e(err);
        }
    });
}
