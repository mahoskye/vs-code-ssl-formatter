import * as vscode from "vscode";

export class SSLFormatter implements vscode.DocumentFormattingEditProvider {
    private document!: vscode.TextDocument;
    private options!: vscode.FormattingOptions;

    // List of SSL keywords that should be capitalized
    private keywords = [
        "BEGINCASE",
        "BEGININLINECODE",
        "CASE",
        "DECLARE",
        "DEFAULT",
        "ELSE",
        "ENDCASE",
        "ENDIF",
        "ENDINLINECODE",
        "ENDWHILE",
        "ERROR",
        "EXITCASE",
        "EXITWHILE",
        "IF",
        "INCLUDE",
        "LOOP",
        "OTHERWISE",
        "PARAMETERS",
        "PROCEDURE",
        "PUBLIC",
        "REGION",
        "RESUME",
        "RETURN",
        "WHILE",
    ];

    public provideDocumentFormattingEdits(
        document: vscode.TextDocument,
        options: vscode.FormattingOptions,
        token: vscode.CancellationToken
    ): vscode.TextEdit[] {
        this.document = document;
        this.options = options;

        let text = document.getText();
        const originalLines = text.split("\n");

        // Apply formatting steps
        text = this.correctKeywordCasing(text);
        text = this.ensureSemicolonNewline(text);
        text = this.enforceOperatorSpacing(text);
        text = this.enforceLineSpacing(text);
        text = this.breakLongLines(text);
        text = this.adjustIndentation(text);

        // Create a single edit for the entire document
        const lastLineId = document.lineCount - 1;
        const lastLineLength = document.lineAt(lastLineId).text.length;
        const range = new vscode.Range(0, 0, lastLineId, lastLineLength);
        return [vscode.TextEdit.replace(range, text)];
    }

    // Correct the casing of SSL keywords
    private correctKeywordCasing(text: string): string {
        const keywordRegex = new RegExp(`:(${this.keywords.join("|")})\\b`, "gi");
        return text.replace(keywordRegex, (match) => {
            const keyword = match.slice(1).toUpperCase(); // Remove ':' and uppercase
            return `:${keyword}`;
        });
    }

    // Ensure semicolons are followed by newlines
    private ensureSemicolonNewline(text: string): string {
        return text.replace(/;(?!$)/gm, ";\n");
    }

    // Adjust the indentation of the code
    private adjustIndentation(text: string): string {
        const lines = text.split("\n");
        let indentLevel = 0;
        const indentStack: number[] = [];
        const indentIncreaseKeywords = /^:(IF|WHILE|BEGINCASE|PROCEDURE|REGION|BEGININLINECODE)\b/i;
        const indentDecreaseKeywords = /^:(ENDIF|ENDWHILE|ENDCASE|ENDPROC|ENDINLINECODE|ENDREGION)\b/i;
        const specialCaseKeywords = /^:(CASE|OTHERWISE|ELSE)\b/i;

        const formattedLines = lines.map((line, index) => {
            const trimmedLine = line.trim();

            // Handle indent decrease
            if (indentDecreaseKeywords.test(trimmedLine)) {
                if (indentStack.length > 0) {
                    indentLevel = indentStack.pop()!;
                }
            }

            // Special handling for CASE, OTHERWISE, and ELSE
            if (specialCaseKeywords.test(trimmedLine)) {
                if (indentStack.length > 0 && index > 0 && !/^:BEGINCASE\b/i.test(lines[index - 1].trim())) {
                    indentLevel--;
                }
            }

            // Calculate the new indentation using tabs
            const newIndent = "\t".repeat(indentLevel);
            const newLine = newIndent + trimmedLine;

            // Handle indent increase
            if (indentIncreaseKeywords.test(trimmedLine)) {
                indentStack.push(indentLevel);
                indentLevel++;
            }

            // Increase indent after special case keywords
            if (specialCaseKeywords.test(trimmedLine)) {
                indentLevel++;
            }

            return newLine;
        });

        return formattedLines.join("\n");
    }

    // Enforce line spacing according to LIMS style guide
    private enforceLineSpacing(text: string): string {
        const lines = text.split("\n");
        const formattedLines: string[] = [];
        let inCommentBlock = false;

        for (let i = 0; i < lines.length; i++) {
            const currentLine = lines[i].trim();
            const nextLine = i < lines.length - 1 ? lines[i + 1].trim() : "";
            const prevLine = i > 0 ? lines[i - 1].trim() : "";

            // Check if we're entering or exiting a comment block
            if (currentLine.startsWith("/*")) {
                inCommentBlock = true;
            }
            if (currentLine.endsWith(";") && inCommentBlock) {
                inCommentBlock = false;
            }

            // Add the current line
            formattedLines.push(currentLine);

            // Function to check if we need to add a blank line
            const needsBlankLine = () => {
                if (currentLine === "" || nextLine === "") {
                    return false;
                }
                if (inCommentBlock) {
                    return false;
                }
                if (
                    currentLine.startsWith(":DECLARE") ||
                    currentLine.startsWith(":PARAMETERS") ||
                    currentLine.startsWith(":DEFAULT")
                ) {
                    return (
                        nextLine &&
                        !nextLine.startsWith(":DECLARE") &&
                        !nextLine.startsWith(":PARAMETERS") &&
                        !nextLine.startsWith(":DEFAULT")
                    );
                }
                if (currentLine.startsWith(":PROCEDURE")) {
                    return true;
                }
                if (nextLine.match(/^:(IF|WHILE|BEGINCASE)/i)) {
                    return true;
                }
                if (currentLine.match(/^:(ENDIF|ENDWHILE|ENDCASE|ENDPROC)/i)) {
                    return true;
                }
                if (currentLine.startsWith(":RETURN")) {
                    return true;
                }
                if (currentLine.startsWith("/*") && !prevLine.startsWith("/*")) {
                    return true;
                }
                if (currentLine.endsWith(";") && nextLine.startsWith("/*")) {
                    return true;
                }
                return false;
            };

            // Add a blank line if needed
            if (needsBlankLine() && formattedLines[formattedLines.length - 1] !== "") {
                formattedLines.push("");
            }
        }

        // Remove extra blank lines (more than one consecutive blank line)
        const cleanedLines = formattedLines.filter(
            (line, index, array) =>
                line.trim() !== "" || (index > 0 && array[index - 1].trim() !== "") || index === array.length - 1
        );

        // Ensure the document ends with exactly one blank line
        if (cleanedLines[cleanedLines.length - 1].trim() !== "") {
            cleanedLines.push("");
        }

        return cleanedLines.join("\n");
    }

    private enforceOperatorSpacing(text: string): string {
        // Define regex patterns for operators (arithmetic, comparison, assignment, logical, commas)
        const operators = /([+\-*/%^=<>!]=?|:=|\.and\.|\.or\.|\.not\.)/gi;
        const commas = /,(?!\s)/g;

        // Regex to match quoted strings (including nested quotes) or block comments
        const stringOrCommentPattern = /("(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'|\/\*[\s\S]*?;)/g;

        // Split the text into code and non-code (strings/comments) segments
        let segments = text.split(stringOrCommentPattern);

        // Process only the code segments
        for (let i = 0; i < segments.length; i += 2) {
            let segment = segments[i];

            // Replace operators with properly spaced versions
            segment = segment.replace(operators, " $1 ");
            segment = segment.replace(commas, ", ");

            // Handle special cases
            segment = segment.replace(/\s+;/g, ";"); // Remove space before semicolon
            segment = segment.replace(/\(\s+/g, "("); // Remove space after opening parenthesis
            segment = segment.replace(/\s+\)/g, ")"); // Remove space before closing parenthesis

            // Handle negative numbers (don't add space after minus sign)
            segment = segment.replace(/(\s+)-(\d+)/g, "$1-$2");

            // Remove multiple spaces, but preserve newlines
            segment = segment.replace(/[^\S\n]+/g, " ");

            segments[i] = segment;
        }

        // Rejoin the segments
        return segments.join("");
    }

    private breakLongLines(text: string, maxLength: number = 90): string {
        const lines = text.split("\n");
        const newLines: string[] = [];

        for (let line of lines) {
            if (line.length <= maxLength) {
                newLines.push(line);
                continue;
            }

            let currentLine = line.trim();
            let isInComment = currentLine.startsWith("/*");

            while (currentLine.length > maxLength) {
                let breakIndex = maxLength;

                // Try to break at a space
                const lastSpace = currentLine.lastIndexOf(" ", maxLength);
                if (lastSpace > maxLength / 2) {
                    breakIndex = lastSpace;
                } else {
                    // If we can't break at a space, find the next space
                    const nextSpace = currentLine.indexOf(" ", maxLength);
                    if (nextSpace !== -1) {
                        breakIndex = nextSpace;
                    } else {
                        // If there's no space to break at, break at maxLength
                        breakIndex = maxLength;
                    }
                }

                // Check for operators to break after
                if (!isInComment) {
                    const operatorMatch = currentLine
                        .slice(0, breakIndex)
                        .match(/[+\-*/%^=<>!]:?=|\+|-|\.and\.|\.or\.|\.not\./g);
                    if (operatorMatch) {
                        const lastOperatorIndex = currentLine.lastIndexOf(
                            operatorMatch[operatorMatch.length - 1],
                            breakIndex
                        );
                        if (lastOperatorIndex > maxLength / 2) {
                            breakIndex = lastOperatorIndex + operatorMatch[operatorMatch.length - 1].length;
                        }
                    }
                }

                // Don't break if only a semicolon is left
                if (currentLine.slice(breakIndex).trim() === ";") {
                    breakIndex = currentLine.length;
                }

                newLines.push(currentLine.slice(0, breakIndex));
                currentLine = currentLine.slice(breakIndex).trim();
            }

            if (currentLine.length > 0) {
                newLines.push(currentLine);
            }
        }

        return newLines.join("\n");
    }
}
