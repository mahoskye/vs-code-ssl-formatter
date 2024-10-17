import * as vscode from "vscode";

type LineData = { indent: number; line: string };

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
        text = this.ensureSingleFinalNewline(text);

        // Create a single edit for the entire document
        const lastLineId = document.lineCount - 1;
        const lastLineLength = document.lineAt(lastLineId).text.length;
        const range = new vscode.Range(0, 0, lastLineId, lastLineLength);
        return [vscode.TextEdit.replace(range, text)];
    }
    private ensureSingleFinalNewline(text: string): string {
        return text.replace(/\n+$/, "\n");
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
        const formattedLines: string[] = [];

        const repeatTab = (count: number): string => "\t".repeat(count);

        const indentIncreaseKeywords = /^:(IF|WHILE|BEGINCASE|PROCEDURE|REGION|BEGININLINECODE)\b/i;
        const indentDecreaseKeywords = /^:(ENDIF|ENDWHILE|ENDCASE|ENDPROC|ENDINLINECODE|ENDREGION)\b/i;
        const specialCaseKeywords = /^:(CASE|OTHERWISE|ELSE)\b/i;
        const multiLineKeywords = /^:(PARAMETERS|DECLARE|DEFAULT)\b/i;
        const nonWhiteSpaceCharacters = /\S/;
        const blockCommentKeyword = /^\/\*\*+/;

        let isMultiLine = false;
        const multiLineBuffer: LineData[] = [];

        let indentLevel = 0;
        const indentStack: number[] = [];

        for (let i = 0; i < lines.length; i++) {
            let thisLine;
            if (blockCommentKeyword.test(lines[i])) {
                thisLine = lines[i];
            } else {
                thisLine = lines[i].trim();
            }
            const currentLine = thisLine;

            if (!isMultiLine) {
                // Check if this line starts a multi-line statement
                const isWorkable = nonWhiteSpaceCharacters.test(currentLine);

                if (isWorkable && !currentLine.endsWith(";")) {
                    isMultiLine = true;
                    multiLineBuffer.push({ indent: indentLevel, line: currentLine });
                    continue;
                }

                // Update indentLevel based on the line content
                // Handle indent decrease
                if (indentDecreaseKeywords.test(currentLine)) {
                    if (indentStack.length > 0) {
                        indentLevel = indentStack.pop()!;
                    }
                }

                // Special handling for CASE, OTHERWISE, ELSE
                if (specialCaseKeywords.test(currentLine)) {
                    const hasIndentStacks = indentStack.length > 0;
                    const isNotFirstLine = i > 0;
                    const previousLineNotBeginCase = !/^:BEGINCASE\b/i.test(lines[i - 1].trim());
                    if (hasIndentStacks && isNotFirstLine && previousLineNotBeginCase) {
                        indentLevel--;
                    }
                }

                // Process single-line statements
                const indentedLine = repeatTab(indentLevel) + currentLine;
                formattedLines.push(indentedLine.trimEnd());

                // Handle indent increase
                if (indentIncreaseKeywords.test(currentLine)) {
                    indentStack.push(indentLevel);
                    indentLevel++;
                }

                // Increase indent after special case keywords
                if (specialCaseKeywords.test(currentLine)) {
                    indentLevel++;
                }
            } else {
                // We're in a multi-line statement
                multiLineBuffer.push({ indent: indentLevel, line: currentLine });

                if (currentLine.endsWith(";")) {
                    // End of multi-line statement
                    isMultiLine = false;

                    // Process the multi-line buffer
                    const processedMultiLine = this.processMultiLineStatement(multiLineBuffer);
                    formattedLines.push(...processedMultiLine);

                    multiLineBuffer.length = 0;
                }
            }
        }
        return formattedLines.join("\n");
    }

    private processMultiLineStatement(multiLineBuffer: LineData[]) {
        const processed: string[] = [];
        const firstLine = multiLineBuffer[0].line;
        const firstIndent = multiLineBuffer[0].indent;
        const repeatTab = (count: number): string => "\t".repeat(count);

        const blockCommentKeyword = /^\/\*\*+/;
        const bracketPattern = /^[\w\s:=]*[\(\{\[]/;
        const commentKeyword = /^\/\*[^*]/;
        const keywordPattern = /^:(\w+)\b/i;
        const operatorPattern = /^[\w\s]*:=/;

        processed.push(repeatTab(firstIndent) + firstLine);

        let subIndent = firstIndent;

        // Need to find the first position of:
        const keywordMatch = firstLine.match(keywordPattern);
        const commentMatch = firstLine.match(commentKeyword);
        const bracketMatch = firstLine.match(bracketPattern);
        const operatorMatch = firstLine.match(operatorPattern);

        if (blockCommentKeyword.test(firstLine)) {
            subIndent = firstIndent;
        } else if (keywordMatch) {
            // - The first space after a :KEYWORD
            subIndent = Math.ceil((keywordMatch[0].length + 1) / 4);
        } else if (commentMatch) {
            // - The first space after a Comment operator '/*'
            subIndent = Math.ceil((commentMatch[0].length + 1) / 4);
        } else if (bracketMatch) {
            // - The first space after an opening bracket ({[
            subIndent = Math.ceil(bracketMatch[0].length / 4);
        } else if (operatorMatch) {
            // - The first space after the assignment operator ':='
            subIndent = Math.ceil((operatorMatch[0].length + 1) / 4);
        }

        subIndent += firstIndent;

        for (let i = 1; i < multiLineBuffer.length; i++) {
            const currentLine = multiLineBuffer[i].line;
            const currentIndent = multiLineBuffer[i].indent;
            processed.push(repeatTab(subIndent) + currentLine);
        }

        return processed;
    }

    // Enforce line spacing according to LIMS style guide
    private enforceLineSpacing(text: string): string {
        const lines = text.split("\n");
        const formattedLines: string[] = [];
        let inCommentBlock = false;
        let inParameterBlock = false;

        for (let i = 0; i < lines.length; i++) {
            const currentLine = lines[i].trimEnd();
            const nextLine = i < lines.length - 1 ? lines[i + 1].trimEnd() : "";
            const prevLine = i > 0 ? lines[i - 1].trimEnd() : "";

            // Check if we're entering or exiting a comment block
            if (currentLine.startsWith("/*")) {
                inCommentBlock = true;
            }
            if (currentLine.endsWith(";") && inCommentBlock) {
                inCommentBlock = false;
            }

            // Check if we're in a parameter block
            if (currentLine.startsWith(":PARAMETERS") || currentLine.startsWith(":DEFAULT")) {
                inParameterBlock = true;
            }
            if (inParameterBlock && currentLine.endsWith(";")) {
                inParameterBlock = false;
            }

            // Add the current line
            formattedLines.push(currentLine);

            // Function to check if we need to add a blank line
            const needsBlankLine = () => {
                if (currentLine === "" || nextLine === "") {
                    return false;
                }
                if (inCommentBlock || inParameterBlock) {
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
                if (nextLine.match(/^:(ELSE|OTHERWISE)/i)) {
                    return false;
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
            segment = segment.replace(/\s*:=\s*/g, " := "); // Remove extra spacing around assignment operator

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
