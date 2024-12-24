/**
 * TODO:
 * - Add FOR, NEXT, LOOP
 * - Rename multiLineKeywords to something more appropriate
 */
type LineData = { indent: number; line: string };

const repeatTab = (count: number): string => "\t".repeat(count);
const isCommentedLine = (line: string) => line.trim().startsWith("/*");
const trackMsg = (type: string, indent: number, indentChange: number, line: string) =>
    console.log(`${type}: ${indent}>${indentChange} | ${line}`);

export function adjustIndentation(text: string): string {
    const lines = text.split("\n");
    const formattedLines: string[] = [];

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
        const currentLine = lines[i];

        if (!isMultiLine) {
            // Check if this line starts a multi-line statement
            const isWorkable = nonWhiteSpaceCharacters.test(currentLine.trim());

            if (isWorkable && !currentLine.trim().endsWith(";")) {
                isMultiLine = true;
                multiLineBuffer.push({ indent: indentLevel, line: currentLine });
                continue;
            }

            // Update indentLevel based on the line content
            // Handle indent decrease
            if (indentDecreaseKeywords.test(currentLine.trim())) {
                if (indentStack.length > 0) {
                    indentLevel = indentStack.pop()!;
                }
            }

            // Special handling for CASE, OTHERWISE, ELSE
            if (specialCaseKeywords.test(currentLine.trim())) {
                const hasIndentStacks = indentStack.length > 0;
                const isNotFirstLine = i > 0;
                const previousLineNotBeginCase = !/^:BEGINCASE\b/i.test(lines[i - 1].trim());
                if (hasIndentStacks && isNotFirstLine && previousLineNotBeginCase) {
                    indentLevel--;
                }
            }

            // Process single-line statements
            const indentedLine = repeatTab(indentLevel) + currentLine.trim();
            formattedLines.push(indentedLine);

            // Handle indent increase
            if (indentIncreaseKeywords.test(currentLine.trim())) {
                indentStack.push(indentLevel);
                indentLevel++;
            }

            // Increase indent after special case keywords
            if (specialCaseKeywords.test(currentLine.trim())) {
                indentLevel++;
            }
        } else {
            // We're in a multi-line statement
            multiLineBuffer.push({ indent: indentLevel, line: currentLine });

            if (currentLine.trim().endsWith(";")) {
                // End of multi-line statement
                isMultiLine = false;

                // Process the multi-line buffer
                const processedMultiLine = processMultiLineStatement(multiLineBuffer);
                formattedLines.push(...processedMultiLine);

                multiLineBuffer.length = 0;
            }
        }
    }
    return formattedLines.join("\n");
}

function processMultiLineStatement(multiLineBuffer: LineData[]) {
    const processed: string[] = [];
    const firstLine = multiLineBuffer[0].line;
    const firstIndent = multiLineBuffer[0].indent;

    const blockCommentKeyword = /^\/\*\*+/;
    const bracketPattern = /^[\w\s:=]*[\(\{\[]/;
    const commentKeyword = /^\/\*[^*]/;
    const keywordPattern = /^:(\w+)\b/i;
    const operatorPattern = /^[\w\s]*:=/;

    processed.push(repeatTab(firstIndent) + firstLine.trim());

    let subIndent = firstIndent;
    // Need to find the first position of:
    const keywordMatch = firstLine.trim().match(keywordPattern);
    const commentMatch = firstLine.trim().match(commentKeyword);
    const bracketMatch = firstLine.trim().match(bracketPattern);
    const operatorMatch = firstLine.trim().match(operatorPattern);

    if (blockCommentKeyword.test(firstLine.trim())) {
        subIndent = firstIndent;
    } else if (commentMatch) {
        // - The first space after a Comment operator '/*'
        subIndent = Math.ceil((commentMatch[0].length + 1) / 4);
    } else if (keywordMatch) {
        // - The first space after a :KEYWORD
        subIndent = Math.ceil((keywordMatch[0].length + 1) / 4);
    } else if (bracketMatch) {
        // - The first space after an opening bracket ({[
        subIndent = Math.ceil(bracketMatch[0].length / 4);
    } else if (operatorMatch) {
        // - The first space after the assignment operator ':='
        subIndent = Math.ceil((operatorMatch[0].length + 1) / 4);
    } else {
        subIndent = 0;
    }

    subIndent += firstIndent;

    for (let i = 1; i < multiLineBuffer.length; i++) {
        const currentLine = multiLineBuffer[i].line;
        const currentIndent = multiLineBuffer[i].indent;

        // If the first line is a block comment, we will never want to fully trim that
        if (blockCommentKeyword.test(firstLine.trim())) {
            processed.push(repeatTab(subIndent) + currentLine.trimEnd());
        } else {
            processed.push(repeatTab(subIndent) + currentLine.trim());
        }
    }

    return processed;
}
