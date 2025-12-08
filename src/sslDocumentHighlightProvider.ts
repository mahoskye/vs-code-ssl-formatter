import * as vscode from "vscode";

/**
 * SSL Document Highlight Provider
 * Highlights all occurrences of a symbol in the current document
 * Refactored to use a single-pass linear scan for performance (O(N)).
 */
export class SSLDocumentHighlightProvider implements vscode.DocumentHighlightProvider {

	public provideDocumentHighlights(
		document: vscode.TextDocument,
		position: vscode.Position,
		token: vscode.CancellationToken
	): vscode.DocumentHighlight[] {
		const range = document.getWordRangeAtPosition(position);
		if (!range) {
			return [];
		}

		const word = document.getText(range);
		if (this.isKeyword(word)) {
			return [];
		}

		const text = document.getText();
		const lines = text.split(/\r?\n/);
		const highlights: vscode.DocumentHighlight[] = [];
		const escapedWord = this.escapeRegex(word);
		const wordPattern = new RegExp(`\\b${escapedWord}\\b`, "gi");

		let inBlockComment = false;

		for (let i = 0; i < lines.length; i++) {
			if (token.isCancellationRequested) {
				return [];
			}

			const line = lines[i];
			const trimmed = line.trim();

			// Fast path for comment-only lines
			if (trimmed.startsWith('*') || trimmed.startsWith('//')) {
				continue;
			}

			// Calculate safe regions (code ranges) for this line
			const { regions, newInBlockComment } = this.computeSafeRegions(line, inBlockComment);
			inBlockComment = newInBlockComment;

			if (regions.length === 0) {
				continue;
			}

			// Find matches and check if they fall within safe regions
			wordPattern.lastIndex = 0;
			let match: RegExpExecArray | null;
			while ((match = wordPattern.exec(line)) !== null) {
				const matchStart = match.index;
				const matchEnd = match.index + word.length;

				// Check intersection with safe regions
				const isSafe = regions.some(r => matchStart >= r.start && matchEnd <= r.end);

				if (isSafe) {
					const startPos = new vscode.Position(i, matchStart);
					const endPos = new vscode.Position(i, matchEnd);
					const highlightRange = new vscode.Range(startPos, endPos);

					// Determine highlight kind
					let kind = vscode.DocumentHighlightKind.Text;
					if (this.isDefinition(line, matchStart)) {
						kind = vscode.DocumentHighlightKind.Write;
					} else if (this.isAssignment(line, matchEnd)) {
						kind = vscode.DocumentHighlightKind.Write;
					} else {
						kind = vscode.DocumentHighlightKind.Read;
					}

					highlights.push(new vscode.DocumentHighlight(highlightRange, kind));
				}
			}
		}

		return highlights;
	}

	/**
	 * Computes the regions of the line that are actual code (not comments or strings).
	 */
	private computeSafeRegions(line: string, inBlockComment: boolean): { regions: { start: number, end: number }[], newInBlockComment: boolean } {
		const regions: { start: number, end: number }[] = [];
		let state = inBlockComment ? 'COMMENT' : 'CODE';
		let regionStart = 0;
		let currentInBlockComment = inBlockComment;

		for (let i = 0; i < line.length; i++) {
			const char = line[i];
			const nextChar = line[i + 1] || '';

			if (state === 'CODE') {
				if (char === '/' && nextChar === '*') {
					// Start Block Comment
					if (i > regionStart) {
						regions.push({ start: regionStart, end: i });
					}
					state = 'COMMENT';
					currentInBlockComment = true;
					i++; // Skip *
				}
				else if (char === '/' && nextChar === '/') {
					// Single line comment //
					if (i > regionStart) {
						regions.push({ start: regionStart, end: i });
					}
					return { regions, newInBlockComment: currentInBlockComment }; // Stop processing line
				}
				else if (char === '"' || char === "'") {
					// Start String
					if (i > regionStart) {
						regions.push({ start: regionStart, end: i });
					}
					state = char === '"' ? 'STRING_DOUBLE' : 'STRING_SINGLE';
				}
			}
			else if (state === 'COMMENT') {
				if (char === ';') {
					// End Block Comment (SSL specific)
					state = 'CODE';
					currentInBlockComment = false;
					regionStart = i + 1;
				}
			}
			else if (state === 'STRING_DOUBLE') {
				if (char === '"') {
					state = 'CODE';
					regionStart = i + 1;
				}
			}
			else if (state === 'STRING_SINGLE') {
				if (char === "'") {
					state = 'CODE';
					regionStart = i + 1;
				}
			}
		}

		// Close final region if in CODE state
		if (state === 'CODE' && regionStart < line.length) {
			regions.push({ start: regionStart, end: line.length });
		}

		return { regions, newInBlockComment: currentInBlockComment };
	}

	private isKeyword(word: string): boolean {
		// Common SSL keywords to ignore
		const keywords = new Set([
			"IF", "ELSE", "ENDIF",
			"WHILE", "ENDWHILE", "FOR", "TO", "STEP", "NEXT",
			"FOREACH", "IN", "BEGINCASE", "CASE", "OTHERWISE", "ENDCASE",
			"TRY", "CATCH", "FINALLY", "ENDTRY",
			"PROCEDURE", "ENDPROC", "ENDPROCEDURE", "RETURN",
			"DECLARE", "DEFAULT", "PARAMETERS", "PUBLIC",
			"CLASS", "INHERIT", "REGION", "ENDREGION"
		]);
		return keywords.has(word.toUpperCase());
	}

	private isDefinition(line: string, position: number): boolean {
		const beforePos = line.substring(0, position);
		// :PROCEDURE Name, :DECLARE Name, :PARAMETERS Name, :CLASS Name
		// Also handling comma lists: :DECLARE A, B, C
		// Simplistic check: if the line started with a definition keyword
		const defMatch = /^\s*:(PROCEDURE|DECLARE|PARAMETERS|CLASS)\b/i.exec(beforePos);
		if (defMatch) {
			return true;
		}
		// Sub-check for :PROCEDURE ... ( ... Name ... ) ? 
		// Typically definitions are top-level or clearly marked.
		return false;
	}

	private isAssignment(line: string, matchEndIndex: number): boolean {
		// Check if there's an assignment operator after the word
		const afterPos = line.substring(matchEndIndex);

		// Skip whitespace
		const trimmed = afterPos.trimStart();
		if (!trimmed) {
			return false;
		}

		// Check for operators
		return /^(:=|\+=|-=|\*=|\/=|\^=|%=)/.test(trimmed);
	}

	private escapeRegex(str: string): string {
		return str.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
	}
}
