import type { DocumentSelector } from "vscode";

/**
 * Canonical document selectors for SSL language features.
 * Includes both saved files and untitled editors so language
 * services stay available before the first save.
 */
export const SSL_DOCUMENT_SELECTORS: DocumentSelector = [
	{ language: "ssl", scheme: "file" },
	{ language: "ssl", scheme: "untitled" }
];
