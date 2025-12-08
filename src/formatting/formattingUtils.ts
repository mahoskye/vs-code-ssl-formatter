/**
 * formattingUtils.ts
 * Utility functions for string manipulation and formatting
 */

/**
 * Calculates the visual length of a string, accounting for tab expansion.
 */
export function getVisualLength(str: string, tabSize: number): number {
    let len = 0;
    for (const char of str) {
        if (char === '\t') {
            len += tabSize;
        } else {
            len += 1;
        }
    }
    return len;
}

/**
 * Splits a long string token into multiple lines with proper indentation and concatenation.
 */
export function splitStringToken(content: string, quote: string, firstLineMax: number, subsequentMax: number, indent: string): string {
    let result = "";
    let currentLine = quote;
    const words = content.split(/(\s+)/); // Keep whitespace separated

    for (const word of words) {
        // Determine constraint based on whether we are still on the first line (result empty)
        const maxLen = result.length === 0 ? firstLineMax : subsequentMax;

        // +1 accounts for the closing quote if we were to stop here
        if (currentLine.length + word.length + 1 > maxLen) {
            result += currentLine + quote + "\n" + indent + "+ ";
            currentLine = quote + word;
        } else {
            currentLine += word;
        }
    }

    return result + currentLine + quote;
}

/**
 * Wraps a line of parameters (e.g., :PARAMETERS ...) to fit within visual width limits.
 */
export function wrapParameterLine(line: string, keyword: string, currentIndent: string, maxLen: number = 80, tabSize: number = 4): string {
    // Determine where parameters start
    const alignSpaces = ' '.repeat(keyword.length + 1); // e.g. "PARAMETERS "
    const continueIndent = currentIndent + alignSpaces;

    // Handle trailing semicolon if present
    const semiPos = line.lastIndexOf(';');
    const hasSemi = semiPos > 0;
    const content = hasSemi ? line.substring(0, semiPos) : line;
    const semi = hasSemi ? ';' : '';

    const parts = content.split(',').map(p => p.trim());

    let result = "";
    let currentLine = "";
    const baseVisualIndent = getVisualLength(currentIndent, tabSize);

    for (let i = 0; i < parts.length; i++) {
        const part = parts[i];
        const separator = i === 0 ? "" : ", ";

        // Check visual length including indentation for the *current* line context
        const currentVisualLen = getVisualLength(currentLine + separator + part, tabSize);
        // Correctly account for base indentation on the very first line of output (which includes `currentIndent` implicitly from caller?)
        // Actually, caller usually appends this result to `currentIndent`. 
        // So `currentLine` starts empty. 
        // If result is empty (first line), total visual width = baseVisualIndent + currentVisualLen.
        // If result not empty (subsequent lines), total visual width = getVisualLength(continueIndent) + currentVisualLen.

        const linePrefixLen = result.length === 0 ? baseVisualIndent : getVisualLength(continueIndent, tabSize);

        if (currentLine.length > 0 && (linePrefixLen + currentVisualLen > maxLen)) {
            // Check if wrapping helps (only if currentLine isn't empty)
            result += currentLine + ",\n";
            currentLine = continueIndent + part;
        } else {
            currentLine += separator + part;
        }
    }

    return result + currentLine + semi;
}
