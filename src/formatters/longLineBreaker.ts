/**
 * TODO: This code needs to be refactored.
 * - Correctly affects long comments without attempting to format them
 * - Incorrectly handles lists.
 *      - Will favor inserting a newline into a string over moving the list item to a new line
 *      - Will cause the closing bracket for a list to move to a new line. Should move the last list item with it
 * - Will insert a newline into a string. We should never modify the value of a string
 * - Will split a variable name. Words must remain whole
 */

export function breakLongLines(text: string, maxLength: number = 90): string {
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
