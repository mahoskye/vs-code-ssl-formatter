/**
 * Note: I moved all the tests to their own files to make it easier to run them individually.
 * This file is probably depricated now, but I left it here for reference.
 */

import * as assert from "assert";
// DO NOT import vscode here directly before mocking

import { SSLFormattingProvider } from "../../src/formatters/formattingProvider";
import { formatDocument } from "../helpers/formatDocument";

// DO NOT Import the actual mock implementation here, it will be required inside the factory
// import * as VscodeMock from "../__mocks__/vscode";

// Mock the vscode module
jest.mock("vscode", () => {
    // Use require here to ensure it's loaded at the time the factory is executed
    // and use a lint-friendly name.
    // Changed path to point to the renamed manual mock file
    const vscodeMock = jest.requireActual("../__mocks__/vscode");
    return vscodeMock; // Return the entire module exports from our manual mock
});

// NOW import vscode, it will be the mocked version specified by the factory
import * as vscode from "vscode";

const TESTTIMEOUT = 15000; // Increased timeout for potentially longer async operations
describe("SSLFormattingProvider", () => {
    let provider: SSLFormattingProvider;
    let mockTextEditReplace: jest.SpyInstance;

    beforeEach(() => {
        jest.clearAllMocks(); // Ensure mocks are reset before each test

        // Re-import vscode and the provider after resetting modules
        // to ensure they get the fresh, mocked versions.
        // Note: vscode is already imported at the top level after the mock,
        // but SSLFormattingProvider might need to be re-required if it cached 'vscode' internally on first load.
        // For now, let's assume top-level import of vscode is sufficient after resetModules.
        // If issues persist, we might need to dynamically require SSLFormattingProvider here.

        provider = new SSLFormattingProvider();

        // Explicitly spy on and mock vscode.TextEdit.replace
        // Ensure that 'vscode.TextEdit' actually refers to our mock class here
        // due to jest.mock("vscode") at the top.
        mockTextEditReplace = jest
            .spyOn(vscode.TextEdit, "replace")
            .mockImplementation((range: vscode.Range, newText: string) => {
                // Enhanced logging for the input 'range'
                // console.log(
                //     "[TEST SPY vscode.TextEdit.replace] CALLED. Input Range object (raw):",
                //     range
                // );
                if (range && range.start && range.end) {
                    // Added null checks for start/end
                    // console.log(
                    //     `[TEST SPY vscode.TextEdit.replace] Input Range: start(${range.start.line}, ${range.start.character}) to end(${range.end.line}, ${range.end.character})`
                    // );
                } else {
                    // console.log(
                    //     "[TEST SPY vscode.TextEit.replace] Input Range, range.start, or range.end is null/undefined."
                    // );
                }
                // console.log("[TEST SPY vscode.TextEdit.replace] NewText length:", newText?.length); // Can be verbose

                const mockReturnedTextEdit = new vscode.TextEdit(range, newText);

                // console.log( // Removed verbose log
                //     "[TEST SPY vscode.TextEdit.replace] Constructed mock TextEdit instance:",
                //     mockReturnedTextEdit
                // );
                if (
                    mockReturnedTextEdit.range &&
                    mockReturnedTextEdit.range.start &&
                    mockReturnedTextEdit.range.end
                ) {
                    // Added null checks
                    // console.log(
                    //     `[TEST SPY vscode.TextEdit.replace] Returned TextEdit's Range: start(${mockReturnedTextEdit.range.start.line}, ${mockReturnedTextEdit.range.start.character}) to end(${mockReturnedTextEdit.range.end.line}, ${mockReturnedTextEdit.range.end.character})`
                    // );
                } else {
                    // console.log(
                    //     "[TEST SPY vscode.TextEdit.replace] Returned TextEdit's Range, range.start or range.end is null/undefined."
                    // );
                }
                return mockReturnedTextEdit;
            });
    });

    afterEach(() => {
        // Restore the original implementation after each test if necessary, though clearAllMocks might handle it.
        // For spyOn, it's good practice to restore.
        mockTextEditReplace.mockRestore();
    });
    // Add a dummy test to satisfy Jest
    test("should be true", () => {
        expect(true).toBe(true);
    });
});
