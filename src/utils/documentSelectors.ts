import type { DocumentSelector } from "vscode";

/**
 * Canonical document selectors for SSL language features.
 * Targets the "ssl" language on any scheme (file, untitled, git, vsls, etc.)
 * to ensure features work in all contexts like diff editors and remote workspaces.
 */
export const SSL_DOCUMENT_SELECTORS: DocumentSelector = [
	{ language: "ssl" }
];
