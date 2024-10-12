import * as vscode from "vscode";

/**
 * Provides folding ranges for SSL files.
 */
export class SSLFoldingProvider implements vscode.FoldingRangeProvider {
    /**
     * Calculates folding ranges for the given document.
     * @param document The document to provide folding ranges for.
     * @param context The folding context.
     * @param token A cancellation token.
     * @returns An array of folding ranges.
     */
    public provideFoldingRanges(
        document: vscode.TextDocument,
        context: vscode.FoldingContext,
        token: vscode.CancellationToken
    ): vscode.FoldingRange[] {
        const foldingRanges: vscode.FoldingRange[] = [];
        const stack: { type: string; line: number }[] = [];
        let inComment = false;
        let inString = false;

        for (let i = 0; i < document.lineCount; i++) {
            const line = document.lineAt(i).text;

            // Handle comments
            if (line.trim().startsWith("/*")) {
                inComment = true;
                if (line.trim().toLowerCase().startsWith("/*region")) {
                    stack.push({ type: "COMMENT_REGION", line: i });
                }
            }
            if (line.includes(";") && inComment) {
                if (line.trim().toLowerCase().startsWith("/*endregion")) {
                    const start = stack.pop();
                    if (start && start.type === "COMMENT_REGION") {
                        // Use the predefined Region kind for /*region
                        foldingRanges.push(new vscode.FoldingRange(start.line, i, vscode.FoldingRangeKind.Region));
                    }
                }
                inComment = false;
                continue;
            }
            if (inComment) {
                continue;
            }

            // Ignore content within strings
            if ((line.match(/"/g) || []).length % 2 !== 0) {
                inString = !inString;
            }
            if ((line.match(/'/g) || []).length % 2 !== 0) {
                inString = !inString;
            }
            if (inString) {
                continue;
            }

            const trimmedLine = line.trim().toLowerCase();

            // Handle all other fold start points
            if (
                trimmedLine.startsWith(":region") ||
                trimmedLine.startsWith(":procedure") ||
                trimmedLine.startsWith(":if") ||
                trimmedLine.startsWith(":while") ||
                trimmedLine.startsWith(":for") ||
                trimmedLine.startsWith(":begincase") ||
                trimmedLine.startsWith(":class") ||
                trimmedLine.startsWith(":try")
            ) {
                stack.push({ type: trimmedLine.substring(1).split(" ")[0], line: i });
            }
            // Handle all fold end points
            else if (
                trimmedLine.startsWith(":endregion") ||
                trimmedLine.startsWith(":endproc") ||
                trimmedLine.startsWith(":endif") ||
                trimmedLine.startsWith(":endwhile") ||
                trimmedLine.startsWith(":next") ||
                trimmedLine.startsWith(":endcase") ||
                trimmedLine.startsWith(":endtry")
            ) {
                const start = stack.pop();
                if (start) {
                    // All other folds don't specify a kind
                    foldingRanges.push(new vscode.FoldingRange(start.line, i));
                }
            } else if (trimmedLine.startsWith(":catch")) {
                const start = stack[stack.length - 1];
                if (start && start.type === "try") {
                    foldingRanges.push(new vscode.FoldingRange(start.line, i - 1));
                }
            }

            // Check for cancellation request
            if (token.isCancellationRequested) {
                return foldingRanges;
            }
        }

        return foldingRanges;
    }
}
