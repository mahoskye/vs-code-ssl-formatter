// Jest test runner for VS Code extension
// This file is kept for compatibility but Jest will handle test execution
// The actual Jest configuration is in jest.config.js

import * as path from "path";

async function main() {
    try {
        console.log("Jest test runner initialized");
        console.log("Extension development path:", path.resolve(__dirname, "../../"));
        console.log("Test files will be discovered by Jest configuration");

        // Jest will handle the actual test execution
        // This file exists for compatibility with existing scripts
    } catch (err) {
        console.error("Failed to initialize test runner:", err);
        process.exit(1);
    }
}

main();
