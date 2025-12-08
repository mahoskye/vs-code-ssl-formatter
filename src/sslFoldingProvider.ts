import * as vscode from "vscode";

/**
 * Provides folding ranges for SSL files.
 * Refactored to use a robust stack-based parser.
 */
export class SSLFoldingProvider implements vscode.FoldingRangeProvider {

    // Config for folding blocks
    // eslint-disable-next-line @typescript-eslint/naming-convention
    private static readonly BLOCK_PAIRS: Record<string, string> = {
        'IF': 'ENDIF',
        'WHILE': 'ENDWHILE',
        'FOR': 'NEXT',
        'BEGINCASE': 'ENDCASE',
        'TRY': 'ENDTRY',
        'PROCEDURE': 'ENDPROC',
        'REGION': 'ENDREGION',

        'BEGININLINE': 'ENDINLINE' // Assuming this exists or similar
    };

    // Keywords that are "middle" parts of a block (end previous, start new)
    // eslint-disable-next-line @typescript-eslint/naming-convention
    private static readonly MIDDLE_KEYWORDS: Record<string, string[]> = {
        'ELSE': ['IF'],
        'CATCH': ['TRY'],
        'FINALLY': ['TRY', 'CATCH'],
        'CASE': ['BEGINCASE', 'CASE', 'OTHERWISE'],
        'OTHERWISE': ['BEGINCASE', 'CASE']
    };

    public provideFoldingRanges(
        document: vscode.TextDocument,
        context: vscode.FoldingContext,
        token: vscode.CancellationToken
    ): vscode.FoldingRange[] {
        const foldingRanges: vscode.FoldingRange[] = [];
        const stack: { keyword: string; line: number }[] = [];

        let inBlockComment = false;

        for (let i = 0; i < document.lineCount; i++) {
            if (token.isCancellationRequested) {
                return [];
            }

            const line = document.lineAt(i).text;
            const trimmed = line.trim();
            if (!trimmed) {
                continue;
            }

            // --- Comment & String Handling ---
            // We need to identify if the *content* of the line is code or comment
            // But we also need to support "region comments" which are essentially custom folds

            // Check region comments first (/*region ... / /*endregion)
            // They are special because they are comments but act as structural blocks
            const regionStartMatch = /^\/\*\s*region\b/i.exec(trimmed);
            if (regionStartMatch) {
                stack.push({ keyword: 'COMMENT_REGION', line: i });
                // Also update inBlockComment state if it doesn't end on this line
                if (!trimmed.endsWith(';')) {
                    inBlockComment = true;
                }
                continue;
            }

            const regionEndMatch = /^\/\*\s*endregion\b/i.exec(trimmed);
            if (regionEndMatch) {
                // Check if it ends in ; to update state
                if (!trimmed.endsWith(';')) {
                    inBlockComment = true;
                }

                // Pop matching region
                // Find the last open region
                let regionIndex = -1;
                for (let k = stack.length - 1; k >= 0; k--) {
                    if (stack[k].keyword === 'COMMENT_REGION') {
                        regionIndex = k;
                        break;
                    }
                }

                if (regionIndex !== -1) {
                    const start = stack[regionIndex];
                    foldingRanges.push(new vscode.FoldingRange(start.line, i, vscode.FoldingRangeKind.Region));
                    // Remove from stack (and potentially anything on top, incomplete blocks inside region?)
                    // Ideally regions are well-nested. But if not, we probably just pop the region.
                    // But stack is a list. Splice it out? 
                    // Or just pop providing it is top? 
                    // Let's assume strict nesting.
                    if (regionIndex === stack.length - 1) {
                        stack.pop();
                    } else {
                        // It's buried. Remove it to avoid leaks, but maybe not create range?
                        // Actually creating range is fine. 
                        stack.splice(regionIndex, 1);
                    }
                }
                continue;
            }

            // Normal Comment State logic
            // (Simpler version of computeSafeRegions for just fold detection)
            // If the line starts with /* and doesn't end with ;, it starts a block comment
            if (inBlockComment) {
                if (trimmed.endsWith(';')) {
                    inBlockComment = false;
                }
                continue;
            }
            if (trimmed.startsWith('/*')) {
                if (!trimmed.endsWith(';')) {
                    inBlockComment = true;
                }
                continue;
            }
            if (trimmed.startsWith('*') || trimmed.startsWith('//')) {
                continue;
            }

            // --- Keyword Parsing ---
            // We only care about keywords at the START of the statement (or after :)
            // SSL syntax: :KEYWORD ...
            // And usually lines start with colon if they are control structures, OR they are bare inside some contexts?
            // The previous code checked `trimmedLine.startsWith(":region")` etc. 
            // So we primarily look for `:KEYWORD`.

            // Remove strings to avoid false positives inside quotes
            const cleanLine = line.replace(/"[^"]*"/g, '""').replace(/'[^']*'/g, "''");
            const cleanTrimmed = cleanLine.trim();

            const match = /^:([A-Za-z]+)\b/i.exec(cleanTrimmed);
            if (!match) {
                continue;
            }

            const keyword = match[1].toUpperCase();

            // 1. Block Start
            if (SSLFoldingProvider.BLOCK_PAIRS[keyword]) {
                stack.push({ keyword, line: i });
            }
            else if (keyword === 'CLASS') {
                // Class extends to end of file (only one per file allowed)
                // We push it, but it will never be popped by a matching END keyword.
                // We will close it at the end of the loop.
                stack.push({ keyword, line: i });
            }
            // 2. Block Middle (Else, Case, etc.)
            else if (SSLFoldingProvider.MIDDLE_KEYWORDS[keyword]) {
                // Check if top of stack is compatible to be closed/continued
                const top = stack[stack.length - 1];
                if (top && SSLFoldingProvider.MIDDLE_KEYWORDS[keyword].includes(top.keyword)) {
                    // Close previous section
                    foldingRanges.push(new vscode.FoldingRange(top.line, i - 1));
                    stack.pop();

                    // Correction:
                    // If keyword is CASE:
                    //   Propagate if top is CASE or OTHERWISE -> Pop (close sibling).
                    //   If top is BEGINCASE -> Do NOT pop (it's parent).
                    //   Then Push CASE.

                    // Let's refine Middle Logic:
                    if (keyword === 'CASE' || keyword === 'OTHERWISE') {
                        if (top.keyword === 'CASE' || top.keyword === 'OTHERWISE') {
                            foldingRanges.push(new vscode.FoldingRange(top.line, i - 1));
                            stack.pop();
                        }
                    }
                    else if (keyword === 'ELSE' || keyword === 'CATCH' || keyword === 'FINALLY') {
                        // Standard mid-block: Close IF/TRY/CATCH, replace with ELSE/FINALLY
                        foldingRanges.push(new vscode.FoldingRange(top.line, i - 1));
                        stack.pop();
                        // Push new
                    }
                }

                // Push current keyword (effectively updating the 'current block' being tracked)
                stack.push({ keyword, line: i });
            }
            // 3. Block End
            else {
                // Check if it's an end keyword
                const startKeyword = Object.keys(SSLFoldingProvider.BLOCK_PAIRS).find(k => SSLFoldingProvider.BLOCK_PAIRS[k] === keyword);

                if (startKeyword) {
                    const top = stack.pop();
                    if (top) {
                        foldingRanges.push(new vscode.FoldingRange(top.line, i));
                    }
                }
                else if (keyword === 'ENDCASE') {
                    // ENDCASE closes CASE, OTHERWISE, and BEGINCASE
                    let top = stack.pop();
                    while (top && (top.keyword === 'CASE' || top.keyword === 'OTHERWISE')) {
                        foldingRanges.push(new vscode.FoldingRange(top.line, i));
                        top = stack.pop();
                    }
                    if (top && top.keyword === 'BEGINCASE') {
                        foldingRanges.push(new vscode.FoldingRange(top.line, i));
                    }
                }
            }
        }

        // Close any remaining blocks at the end of the file (e.g. CLASS)
        const lastLine = document.lineCount - 1;
        while (stack.length > 0) {
            const top = stack.pop();
            if (top) {
                // Only close CLASS or maybe other open blocks at EOF if appropriate
                // For CLASS, yes. For others, maybe tolerance for syntax errors?
                // Let's indiscriminately close for robustness.
                foldingRanges.push(new vscode.FoldingRange(top.line, lastLine));
            }
        }

        return foldingRanges;
    }
}
