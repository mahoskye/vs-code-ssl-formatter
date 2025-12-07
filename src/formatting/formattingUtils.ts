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

export function splitStringToken(content: string, quote: string, firstLineMax: number, subsequentMax: number, indent: string): string {
    // Simple word-based splitting with strict alignment
    let result = "";
    let currentLine = quote;
    let currentMax = firstLineMax; // Start with first line constraint

    const words = content.split(/(\s+)/); // Split by whitespace, capturing it

    for (let i = 0; i < words.length; i++) {
        const word = words[i];

        // Check if adding this word exceeds CURRENT constraint
        // Logic:
        // 1. Fill currentLine until max.
        // 2. If full, append quote. 
        // 3. Start new line with `\n` + `indent` + `+ ` + quote.

        if (currentLine.length + word.length + 1 > currentMax) { // +1 for closing quote
            // Need to break
            result += currentLine + quote; // Close previous
            result += "\n" + indent + "+ "; // Start new line with operator

            currentLine = quote + word; // Start new string content
            currentMax = subsequentMax; // Switch constraint
        } else {
            currentLine += word;
        }
    }

    result += currentLine + quote;
    return result;
}


export function wrapParameterLine(line: string, keyword: string, currentIndent: string, maxLen: number = 80, tabSize: number = 4): string {
    // Continuation indent: Align with first parameter (keyword length + space)
    // e.g. :PARAMETERS (11) + space (1) = 12 spaces
    // If keyword starts with :, length includes it.
    const alignSpaces = ' '.repeat(keyword.length + 1);
    const continueIndent = currentIndent + alignSpaces;

    // Find semicolon and separate it
    const semiPos = line.lastIndexOf(';');
    const content = semiPos > 0 ? line.substring(0, semiPos) : line;
    const semi = semiPos > 0 ? ';' : '';

    // Split by commas while preserving them
    const parts = content.split(',');

    let result = '';
    let currentLine = '';

    // Calculate base indent visual length for first line check
    const baseIndentLen = getVisualLength(currentIndent, tabSize);

    for (let i = 0; i < parts.length; i++) {
        const part = parts[i].trim();
        const isFirst = i === 0;

        // Build what we'd add
        let addition: string;
        if (isFirst) {
            addition = part;
        } else {
            addition = ', ' + part;
        }

        // Check if adding this would exceed max length using VISUAL length
        // For first line (result is empty), include base indent length
        const effectiveCurrentLen = result.length === 0 ? getVisualLength(currentLine + addition, tabSize) + baseIndentLen : getVisualLength(currentLine + addition, tabSize);

        if (currentLine.length > 0 && effectiveCurrentLen > maxLen) {
            // Wrap: add current line (with comma if not first line) and start new
            result += currentLine + ',\n';
            currentLine = continueIndent + part;
        } else {
            currentLine += addition;
        }
    }

    // Add the last line with semicolon
    result += currentLine + semi;

    return result;
}
