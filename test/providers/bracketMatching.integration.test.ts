/**
 * Integration Tests for SSL Bracket Matching Provider
 * Tests the core functionality with actual SSL code samples
 */

import { SSLBracketMatchingProvider } from "../../src/providers/bracketMatchingProvider";
import * as vscode from "vscode";

describe("SSL Bracket Matching Provider - Integration", () => {
    let provider: SSLBracketMatchingProvider;

    beforeEach(() => {
        provider = new SSLBracketMatchingProvider();
    });

    describe("Core Functionality", () => {
        it("should create without errors", () => {
            expect(provider).toBeDefined();
            expect(provider).toBeInstanceOf(SSLBracketMatchingProvider);
        });

        it("should handle empty document", () => {
            const document = createSimpleMockDocument("");
            const pairs = provider.getAllBracketPairs(document);
            expect(pairs).toBeDefined();
            expect(Array.isArray(pairs)).toBe(true);
        });

        it("should handle document with only comments", () => {
            const document = createSimpleMockDocument("/* This is a comment; */");
            const pairs = provider.getAllBracketPairs(document);
            expect(pairs).toBeDefined();
            expect(Array.isArray(pairs)).toBe(true);
        });

        it("should provide bracket information", () => {
            const document = createSimpleMockDocument("/* sample code */");
            const info = provider.getBracketPairInfo(document);
            expect(info).toBeDefined();
            expect(typeof info).toBe("string");
        });

        it("should handle position queries", () => {
            const document = createSimpleMockDocument("/* sample code */");
            const position = new vscode.Position(0, 0);
            const result = provider.findMatchingBracket(document, position);
            // Should either return a Position or null, not throw an error
            expect(result === null || result instanceof vscode.Position).toBe(true);
        });

        it("should validate bracket pairs", () => {
            const document = createSimpleMockDocument("/* sample code */");
            const diagnostics = provider.validateBracketPairs(document);
            expect(diagnostics).toBeDefined();
            expect(Array.isArray(diagnostics)).toBe(true);
        });

        it("should provide SSL keyword pairs", () => {
            const document = createSimpleMockDocument("/* sample code */");
            const keywordPairs = provider.getSSLKeywordPairs(document);
            expect(keywordPairs).toBeDefined();
            expect(Array.isArray(keywordPairs)).toBe(true);
        });

        it("should provide position-specific bracket info", () => {
            const document = createSimpleMockDocument("/* sample code */");
            const position = new vscode.Position(0, 0);
            const info = provider.getBracketInfoAtPosition(document, position);
            expect(info).toBeDefined();
            expect(typeof info).toBe("string");
        });
    });

    describe("SSL Language Support", () => {
        const sslKeywords = [
            "PROCEDURE",
            "ENDPROC",
            "IF",
            "ENDIF",
            "WHILE",
            "ENDWHILE",
            "FOR",
            "NEXT",
            "BEGINCASE",
            "ENDCASE",
            "TRY",
            "ENDTRY",
            "REGION",
            "ENDREGION",
            "CLASS",
            "ERROR",
        ];

        it("should recognize SSL keywords", () => {
            // Test that the provider can handle SSL keyword patterns
            for (const keyword of sslKeywords) {
                const document = createSimpleMockDocument(`:${keyword};`);
                const result = provider.getAllBracketPairs(document);
                expect(result).toBeDefined();
                expect(Array.isArray(result)).toBe(true);
            }
        });

        it("should handle mixed content", () => {
            const mixedContent = `
                /* SSL code sample */
                :DECLARE sVariable;
                sVariable := "test";
                /* End of sample */
            `;
            const document = createSimpleMockDocument(mixedContent);
            const pairs = provider.getAllBracketPairs(document);
            expect(pairs).toBeDefined();
            expect(Array.isArray(pairs)).toBe(true);
        });
    });

    describe("Error Handling", () => {
        it("should handle malformed documents gracefully", () => {
            const malformedContent = ":PROCEDURE\n/* missing semicolon and endproc */";
            const document = createSimpleMockDocument(malformedContent);

            expect(() => {
                provider.getAllBracketPairs(document);
            }).not.toThrow();
        });

        it("should handle very long documents", () => {
            const longContent = "/* comment */\n".repeat(1000);
            const document = createSimpleMockDocument(longContent);

            expect(() => {
                provider.getAllBracketPairs(document);
            }).not.toThrow();
        });

        it("should handle documents with special characters", () => {
            const specialContent = `
                :DECLARE sString;
                sString := "Special chars: üöä €£¥";
                /* Comment with special chars: üöä */
            `;
            const document = createSimpleMockDocument(specialContent);

            expect(() => {
                provider.getAllBracketPairs(document);
            }).not.toThrow();
        });
    });
});

// Simplified mock document that focuses on the core functionality
function createSimpleMockDocument(content: string): vscode.TextDocument {
    return {
        getText: () => content,
        languageId: "ssl",
        uri: vscode.Uri.file("/test.ssl"),
        fileName: "/test.ssl",
        version: 1,
        lineCount: content.split("\n").length,
        isDirty: false,
        isUntitled: false,
        isClosed: false,
    } as vscode.TextDocument;
}
