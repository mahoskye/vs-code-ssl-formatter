export function enforceLineSpacing(text: string): string {
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
