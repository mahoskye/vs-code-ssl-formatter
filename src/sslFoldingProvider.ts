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

        console.log("=== SSL Folding Provider Debug ===");

        for (let i = 0; i < document.lineCount; i++) {
            const line = document.lineAt(i).text;
            const trimmedLine = line.trim().toLowerCase();

            // Handle region comments: /*region or /* region
            if (trimmedLine.match(/^\/\*\s*region\s/)) {
                console.log(`Line ${i}: REGION comment start - "${line.trim()}"`);
                stack.push({ type: "COMMENT_REGION", line: i });
                // Check if the region comment ends on the same line
                if (line.includes(";")) {
                    inComment = false;
                } else {
                    inComment = true;
                }
                continue;
            }

            // Handle endregion comments: /*endregion or /* endregion
            if (trimmedLine.match(/^\/\*\s*endregion/)) {
                console.log(`Line ${i}: ENDREGION comment - "${line.trim()}"`);
                const start = stack.pop();
                if (start && start.type === "COMMENT_REGION") {
                    console.log(`  -> Creating region fold from ${start.line} to ${i}`);
                    // Use the predefined Region kind for /*region
                    foldingRanges.push(new vscode.FoldingRange(start.line, i, vscode.FoldingRangeKind.Region));
                }
                // Check if the endregion comment ends on the same line
                if (line.includes(";")) {
                    inComment = false;
                } else {
                    inComment = true;
                }
                continue;
            }

            // Handle regular comments
            if (line.trim().startsWith("/*")) {
                inComment = true;
            }
            if (line.includes(";") && inComment) {
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
                const type = trimmedLine.substring(1).split(/[\s;]/)[0]; // Split on space or semicolon
                console.log(`Line ${i}: START ${type.toUpperCase()} - "${trimmedLine}"`);
                stack.push({ type: type, line: i });
            }
            // Handle middle keywords that end previous section and start new one
            // Check :CASE and :OTHERWISE before checking end keywords (order matters!)
            else if (trimmedLine.match(/^:case\s/) || trimmedLine.match(/^:otherwise\s/) || trimmedLine === ":otherwise;") {
                // End the previous :CASE/:OTHERWISE section, start new one
                // Look for the most recent case/otherwise on the stack (not begincase)
                const start = stack[stack.length - 1];
                console.log(`Line ${i}: CASE/OTHERWISE - current stack top: ${start?.type} at line ${start?.line}`);
                if (start && (start.type === "case" || start.type === "otherwise")) {
                    // Pop the previous case/otherwise and create its fold
                    console.log(`  -> Creating fold from ${start.line} to ${i - 1}`);
                    foldingRanges.push(new vscode.FoldingRange(start.line, i - 1));
                    stack.pop();
                }
                // Push the new case/otherwise onto the stack (on top of begincase)
                const caseType = trimmedLine.match(/^:case\s/) ? "case" : "otherwise";
                console.log(`  -> Pushing ${caseType} at line ${i}`);
                stack.push({ type: caseType, line: i });
            }
            else if (trimmedLine.startsWith(":else")) {
                // End the :IF section, start :ELSE section
                const start = stack[stack.length - 1];
                console.log(`Line ${i}: ELSE - current stack top: ${start?.type} at line ${start?.line}`);
                if (start && start.type === "if") {
                    console.log(`  -> Creating fold from ${start.line} to ${i - 1}`);
                    foldingRanges.push(new vscode.FoldingRange(start.line, i - 1));
                    // Update the stack entry to :ELSE
                    console.log(`  -> Updating stack top to else at line ${i}`);
                    stack[stack.length - 1] = { type: "else", line: i };
                }
            }
            else if (trimmedLine.startsWith(":catch")) {
                // End the :TRY section, start :CATCH section
                const start = stack[stack.length - 1];
                console.log(`Line ${i}: CATCH - current stack top: ${start?.type} at line ${start?.line}`);
                if (start && start.type === "try") {
                    console.log(`  -> Creating fold from ${start.line} to ${i - 1}`);
                    foldingRanges.push(new vscode.FoldingRange(start.line, i - 1));
                    // Update the stack entry to :CATCH
                    console.log(`  -> Updating stack top to catch at line ${i}`);
                    stack[stack.length - 1] = { type: "catch", line: i };
                }
            }
            else if (trimmedLine.startsWith(":finally")) {
                // End the :CATCH section, start :FINALLY section
                const start = stack[stack.length - 1];
                console.log(`Line ${i}: FINALLY - current stack top: ${start?.type} at line ${start?.line}`);
                if (start && (start.type === "try" || start.type === "catch")) {
                    console.log(`  -> Creating fold from ${start.line} to ${i - 1}`);
                    foldingRanges.push(new vscode.FoldingRange(start.line, i - 1));
                    // Update the stack entry to :FINALLY
                    console.log(`  -> Updating stack top to finally at line ${i}`);
                    stack[stack.length - 1] = { type: "finally", line: i };
                }
            }
            // Handle all fold end points
            else if (
                trimmedLine.startsWith(":endregion") ||
                trimmedLine.startsWith(":endproc") ||
                trimmedLine.startsWith(":endif") ||
                trimmedLine.startsWith(":endwhile") ||
                trimmedLine.startsWith(":next") ||
                trimmedLine.startsWith(":endtry")
            ) {
                const start = stack.pop();
                console.log(`Line ${i}: END - popping ${start?.type} from line ${start?.line}`);
                if (start) {
                    console.log(`  -> Creating fold from ${start.line} to ${i}`);
                    // All other folds don't specify a kind
                    foldingRanges.push(new vscode.FoldingRange(start.line, i));
                }
            }
            else if (trimmedLine.startsWith(":endcase")) {
                // Pop all case/otherwise entries and create folds, then pop begincase
                console.log(`Line ${i}: ENDCASE - processing case stack`);
                while (stack.length > 0) {
                    const start = stack.pop();
                    console.log(`  -> Popping ${start?.type} from line ${start?.line}`);
                    if (start) {
                        console.log(`  -> Creating fold from ${start.line} to ${i}`);
                        foldingRanges.push(new vscode.FoldingRange(start.line, i));
                        if (start.type === "begincase") {
                            // Found the begincase, we're done
                            break;
                        }
                    }
                }
            }

            // Check for cancellation request
            if (token.isCancellationRequested) {
                return foldingRanges;
            }
        }

        console.log(`Total folds created: ${foldingRanges.length}`);
        console.log("=== End SSL Folding Debug ===");

        return foldingRanges;
    }
}
