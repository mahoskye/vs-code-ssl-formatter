// Jest setup file for VS Code extension testing
// This file runs before all tests

// Mock Node.js modules that might be used in tests
jest.mock("fs", () => ({
    existsSync: jest.fn(() => false),
    readFileSync: jest.fn(() => "{}"),
}));

jest.mock("path", () => ({
    resolve: jest.fn((...paths) => paths.join("/")),
    join: jest.fn((...paths) => paths.join("/")),
}));

// If you have other global setups, they can remain here.
// The vscode mock is now handled by test/__mocks__/vscode.ts
// and jest.mock(\'vscode\') in your test files.
