import * as vscode from "vscode";

/**
 * Represents a folding range context on the stack
 */
interface FoldingContext {
    type: string;
    line: number;
    subType?: string; // For additional context like try/catch/finally
}

/**
 * Provides folding ranges for SSL files based on the complete SSL EBNF grammar.
 * Supports all SSL control structures, error handling, classes, procedures, and regions.
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
        const stack: FoldingContext[] = [];
        let inComment = false;
        let inString = false;
        let stringDelimiter = "";
        let commentStartLine = -1;

        for (let i = 0; i < document.lineCount; i++) {
            const line = document.lineAt(i).text;
            const trimmedLine = line.trim().toLowerCase();

            // Check for cancellation request early
            if (token.isCancellationRequested) {
                return foldingRanges;
            }

            // Handle string detection (SSL supports ", ', and [] delimiters)
            if (!inComment) {
                const stringState = this.updateStringState(line, inString, stringDelimiter);
                inString = stringState.inString;
                stringDelimiter = stringState.delimiter;
            }

            // Skip processing if we're inside a string
            if (inString) {
                continue;
            }

            // Handle comment blocks
            if (line.trim().startsWith("/*")) {
                if (!inComment) {
                    inComment = true;
                    commentStartLine = i;

                    // Check for region comments
                    if (
                        trimmedLine.startsWith("/*region") ||
                        trimmedLine.match(/\/\*\s*region\s/)
                    ) {
                        stack.push({ type: "COMMENT_REGION", line: i });
                    }
                }
            }

            // Handle comment endings
            if (line.includes(";") && inComment) {
                if (
                    trimmedLine.startsWith("/*endregion") ||
                    trimmedLine.match(/\/\*\s*endregion\s/)
                ) {
                    const start = this.popStackByType(stack, "COMMENT_REGION");
                    if (start) {
                        foldingRanges.push(
                            new vscode.FoldingRange(start.line, i, vscode.FoldingRangeKind.Region)
                        );
                    }
                } else if (commentStartLine >= 0 && i > commentStartLine) {
                    // Multi-line comment block
                    foldingRanges.push(
                        new vscode.FoldingRange(
                            commentStartLine,
                            i,
                            vscode.FoldingRangeKind.Comment
                        )
                    );
                }

                inComment = false;
                commentStartLine = -1;
                continue;
            }

            // Skip processing if we're inside a comment
            if (inComment) {
                continue;
            }

            // Handle fold start points
            this.handleFoldStart(trimmedLine, i, stack);

            // Handle fold end points and special cases
            this.handleFoldEnd(trimmedLine, i, stack, foldingRanges);
        }

        // Handle any unclosed ranges at end of document
        while (stack.length > 0) {
            const context = stack.pop();
            if (context && context.line < document.lineCount - 1) {
                foldingRanges.push(new vscode.FoldingRange(context.line, document.lineCount - 1));
            }
        }

        return foldingRanges;
    }

    /**
     * Updates the string state based on the current line content.
     * Handles SSL's three string delimiter types: ", ', and []
     */
    private updateStringState(
        line: string,
        currentlyInString: boolean,
        currentDelimiter: string
    ): { inString: boolean; delimiter: string } {
        let inString = currentlyInString;
        let delimiter = currentDelimiter;

        if (!inString) {
            // Look for string start
            for (let i = 0; i < line.length; i++) {
                const char = line[i];
                if (char === '"' || char === "'" || char === "[") {
                    inString = true;
                    delimiter = char === "[" ? "]" : char;
                    break;
                }
            }
        } else {
            // Look for string end
            const endIndex = line.indexOf(delimiter);
            if (endIndex !== -1) {
                inString = false;
                delimiter = "";
                // Check for another string starting on the same line
                const remainingLine = line.substring(endIndex + 1);
                const nextState = this.updateStringState(remainingLine, false, "");
                inString = nextState.inString;
                delimiter = nextState.delimiter;
            }
        }

        return { inString, delimiter };
    }

    /**
     * Pops the most recent context of the specified type from the stack.
     */
    private popStackByType(stack: FoldingContext[], targetType: string): FoldingContext | null {
        for (let i = stack.length - 1; i >= 0; i--) {
            if (stack[i].type === targetType) {
                return stack.splice(i, 1)[0];
            }
        }
        return null;
    }

    /**
     * Handles fold start points based on SSL keywords.
     */
    private handleFoldStart(
        trimmedLine: string,
        lineNumber: number,
        stack: FoldingContext[]
    ): void {
        // Keyword-based regions
        if (trimmedLine.startsWith(":region")) {
            stack.push({ type: "REGION", line: lineNumber });
        }
        // Procedures and classes
        else if (trimmedLine.startsWith(":procedure")) {
            stack.push({ type: "PROCEDURE", line: lineNumber });
        } else if (trimmedLine.startsWith(":class")) {
            stack.push({ type: "CLASS", line: lineNumber });
        }
        // Control structures
        else if (trimmedLine.startsWith(":if")) {
            stack.push({ type: "IF", line: lineNumber });
        } else if (trimmedLine.startsWith(":while")) {
            stack.push({ type: "WHILE", line: lineNumber });
        } else if (trimmedLine.startsWith(":for")) {
            stack.push({ type: "FOR", line: lineNumber });
        }
        // Switch case
        else if (trimmedLine.startsWith(":begincase")) {
            stack.push({ type: "BEGINCASE", line: lineNumber });
        } else if (trimmedLine.startsWith(":case")) {
            stack.push({ type: "CASE", line: lineNumber });
        } else if (trimmedLine.startsWith(":otherwise")) {
            stack.push({ type: "OTHERWISE", line: lineNumber });
        }
        // Error handling
        else if (trimmedLine.startsWith(":try")) {
            stack.push({ type: "TRY", line: lineNumber });
        } else if (trimmedLine.startsWith(":error")) {
            stack.push({ type: "ERROR", line: lineNumber });
        }
        // Inline code blocks
        else if (trimmedLine.startsWith(":begininlinecode")) {
            stack.push({ type: "BEGININLINECODE", line: lineNumber });
        }
        // Additional structures
        else if (trimmedLine.startsWith(":else")) {
            // :ELSE can be part of :IF or standalone
            stack.push({ type: "ELSE", line: lineNumber });
        }
    }

    /**
     * Handles fold end points and creates folding ranges.
     */
    private handleFoldEnd(
        trimmedLine: string,
        lineNumber: number,
        stack: FoldingContext[],
        foldingRanges: vscode.FoldingRange[]
    ): void {
        let start: FoldingContext | null = null;
        let foldKind: vscode.FoldingRangeKind | undefined;

        // Handle specific end markers
        if (trimmedLine.startsWith(":endregion")) {
            start = this.popStackByType(stack, "REGION");
            foldKind = vscode.FoldingRangeKind.Region;
        } else if (trimmedLine.startsWith(":endproc")) {
            start = this.popStackByType(stack, "PROCEDURE");
        } else if (trimmedLine.startsWith(":endif")) {
            // Close any ELSE first, then the IF
            this.popStackByType(stack, "ELSE");
            start = this.popStackByType(stack, "IF");
        } else if (trimmedLine.startsWith(":endwhile")) {
            start = this.popStackByType(stack, "WHILE");
        } else if (trimmedLine.startsWith(":next")) {
            start = this.popStackByType(stack, "FOR");
        } else if (trimmedLine.startsWith(":endcase")) {
            // Close any open CASE or OTHERWISE blocks first
            this.popStackByType(stack, "CASE");
            this.popStackByType(stack, "OTHERWISE");
            start = this.popStackByType(stack, "BEGINCASE");
        } else if (trimmedLine.startsWith(":endtry")) {
            // Handle complex try/catch/finally structure
            this.handleTryEndStructure(stack, lineNumber, foldingRanges);
            return; // Special handling, don't create additional fold
        } else if (trimmedLine.startsWith(":endinlinecode")) {
            start = this.popStackByType(stack, "BEGININLINECODE");
        }
        // Handle intermediate blocks that close previous ones and start new ones
        else if (trimmedLine.startsWith(":catch")) {
            this.handleCatchBlock(stack, lineNumber, foldingRanges);
            stack.push({ type: "CATCH", line: lineNumber });
            return;
        } else if (trimmedLine.startsWith(":finally")) {
            this.handleFinallyBlock(stack, lineNumber, foldingRanges);
            stack.push({ type: "FINALLY", line: lineNumber });
            return;
        } else if (trimmedLine.startsWith(":else")) {
            // Close any existing ELSE, then start new one
            const existingElse = this.popStackByType(stack, "ELSE");
            if (existingElse) {
                foldingRanges.push(new vscode.FoldingRange(existingElse.line, lineNumber - 1));
            }
            stack.push({ type: "ELSE", line: lineNumber });
            return;
        }
        // Handle case transitions
        else if (trimmedLine.startsWith(":case")) {
            // Close previous CASE or OTHERWISE if it exists
            const prevCase =
                this.popStackByType(stack, "CASE") || this.popStackByType(stack, "OTHERWISE");
            if (prevCase) {
                foldingRanges.push(new vscode.FoldingRange(prevCase.line, lineNumber - 1));
            }
            stack.push({ type: "CASE", line: lineNumber });
            return;
        } else if (trimmedLine.startsWith(":otherwise")) {
            // Close previous CASE if it exists
            const prevCase = this.popStackByType(stack, "CASE");
            if (prevCase) {
                foldingRanges.push(new vscode.FoldingRange(prevCase.line, lineNumber - 1));
            }
            stack.push({ type: "OTHERWISE", line: lineNumber });
            return;
        }

        // Create folding range if we found a matching start
        if (start) {
            foldingRanges.push(new vscode.FoldingRange(start.line, lineNumber, foldKind));
        }
    }

    /**
     * Handles the complex try/catch/finally end structure.
     */
    private handleTryEndStructure(
        stack: FoldingContext[],
        lineNumber: number,
        foldingRanges: vscode.FoldingRange[]
    ): void {
        // Close any open FINALLY or CATCH blocks
        const finallyBlock = this.popStackByType(stack, "FINALLY");
        if (finallyBlock) {
            foldingRanges.push(new vscode.FoldingRange(finallyBlock.line, lineNumber - 1));
        }

        const catchBlock = this.popStackByType(stack, "CATCH");
        if (catchBlock) {
            foldingRanges.push(new vscode.FoldingRange(catchBlock.line, lineNumber - 1));
        }

        // Close the TRY block
        const tryBlock = this.popStackByType(stack, "TRY");
        if (tryBlock) {
            foldingRanges.push(new vscode.FoldingRange(tryBlock.line, lineNumber));
        }
    }

    /**
     * Handles CATCH block transitions.
     */
    private handleCatchBlock(
        stack: FoldingContext[],
        lineNumber: number,
        foldingRanges: vscode.FoldingRange[]
    ): void {
        // The TRY block ends at the line before CATCH
        const tryBlock = this.findInStack(stack, "TRY");
        if (tryBlock) {
            foldingRanges.push(new vscode.FoldingRange(tryBlock.line, lineNumber - 1));
        }
    }

    /**
     * Handles FINALLY block transitions.
     */
    private handleFinallyBlock(
        stack: FoldingContext[],
        lineNumber: number,
        foldingRanges: vscode.FoldingRange[]
    ): void {
        // Close any open CATCH block
        const catchBlock = this.popStackByType(stack, "CATCH");
        if (catchBlock) {
            foldingRanges.push(new vscode.FoldingRange(catchBlock.line, lineNumber - 1));
        }
    }

    /**
     * Finds a context in the stack without removing it.
     */
    private findInStack(stack: FoldingContext[], targetType: string): FoldingContext | null {
        for (let i = stack.length - 1; i >= 0; i--) {
            if (stack[i].type === targetType) {
                return stack[i];
            }
        }
        return null;
    }
}
