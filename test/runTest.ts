import * as path from "path";
import { runTests } from "vscode-test";

async function main() {
    try {
        const extensionDevelopmentPath = path.resolve(__dirname, "../../");
        const extensionTestsPath = path.resolve(__dirname, "./suite/index");

        // Specify the path to the extracted VS Code folder
        const vscodeExecutablePath = "C:\\Misc\\VSCode-win32-x64-1.94.2\\Code.exe";

        await runTests({
            extensionDevelopmentPath,
            extensionTestsPath,
            vscodeExecutablePath,
        });
    } catch (err) {
        console.error("Failed to run tests", err);
        process.exit(1);
    }
}

main();
