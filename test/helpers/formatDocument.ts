import * as vscode from "vscode";
import { SSLFormattingProvider } from "../../src/formatters/formattingProvider";
import { FormattingOptions as InternalFormattingOptions } from "../../src/formatters/formattingProvider"; // Renamed to avoid conflict
/**
 * This module provides a helper function to format SSL documents using the SSLFormattingProvider.
 * It creates a mock TextDocument and uses the provider to apply formatting edits.
 * The function returns the formatted text or the original text if no edits are made.
 */

export async function formatDocument(
    provider: SSLFormattingProvider,
    text: string,
    options?: Partial<vscode.FormattingOptions> // Keep this as Partial<vscode.FormattingOptions> for the caller
): Promise<string> {
    // Ensure all paths return a string
    // Use the internal FormattingOptions type for the object passed to the provider
    const mockVscodeOptions: InternalFormattingOptions = {
        tabSize: 4,
        insertSpaces: true,
        maxLineLength: 80, // Add default for testing
        indentStyle: "space", // Add default for testing
        ...options, // Allow overriding with vscode.FormattingOptions compatible parts
    };

    // Create a mock TextDocument
    const mockDocument = {
        getText: () => text,
        lineCount: text.split(/\r?\n/).length,
        lineAt: (lineNumber: number) => {
            const lines = text.split(/\r?\n/);
            const lineText = lines[lineNumber] || "";
            return {
                text: lineText,
                range: new vscode.Range(lineNumber, 0, lineNumber, lineText.length),
                rangeIncludingLineBreak: new vscode.Range(lineNumber, 0, lineNumber + 1, 0),
                firstNonWhitespaceCharacterIndex: lineText.match(/^(\s*)/)?.[0].length || 0,
                isEmptyOrWhitespace: lineText.trim() === "",
                lineNumber: lineNumber, // Added for completeness
            };
        },
        offsetAt: (position: vscode.Position) => {
            const lines = text.split(/\r?\n/);
            let offset = 0;
            for (let i = 0; i < position.line; i++) {
                offset += (lines[i] || "").length + 1; // +1 for newline
            }
            offset += position.character;
            return offset;
        },
        positionAt: (offset: number) => {
            const lines = text.split(/\r?\n/);
            let currentOffset = 0;
            for (let line = 0; line < lines.length; line++) {
                const lineText = lines[line] || "";
                const lineLengthWithNewline = lineText.length + 1;
                if (currentOffset + lineLengthWithNewline > offset) {
                    return new vscode.Position(line, offset - currentOffset);
                }
                currentOffset += lineLengthWithNewline;
            }
            // If offset is beyond the document, return position at the end of the last line
            const lastLine = lines.length > 0 ? lines.length - 1 : 0;
            const lastLineText = lines[lastLine] || "";
            return new vscode.Position(lastLine, lastLineText.length);
        },
        validateRange: (range: vscode.Range) => range,
        validatePosition: (position: vscode.Position) => position,
        uri: vscode.Uri.file("test.ssl"),
        fileName: "test.ssl",
        isUntitled: false,
        languageId: "ssl",
        version: 1,
        isDirty: false,
        save: async () => true,
        eol: vscode.EndOfLine.LF,
        getWordRangeAtPosition: (position: vscode.Position, regex?: RegExp) => undefined,
    } as any; // Use 'as any' to simplify mock, ensure all used properties are present

    try {
        const cancellationTokenSource = new vscode.CancellationTokenSource();
        const edits = await provider.provideDocumentFormattingEdits(
            mockDocument as vscode.TextDocument,
            mockVscodeOptions,
            cancellationTokenSource.token
        );

        if (edits && edits.length > 0 && edits[0]) {
            // Add additional debug info

            const newText = edits[0].newText;

            // The assertion will be done by the caller of formatDocument
            return newText; // Return the formatted text
        } else {
            return text; // Return original text if no edits
        }
    } catch (error) {
        console.error("Error in formatDocument:", error);
        throw error;
    }
}
