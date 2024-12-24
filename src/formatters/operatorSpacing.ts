/**
 * TODO: This code needs to be refactored.
 * - Correctly spaces around operators
 * - Correctly respects quotes and comments
 * - Correctly handles messy string quotes (i.e. "SELECT * FROM EQUIPMENT WHERE EQUIPID='" + sEquipId + "' AND TESTCODE=")
 * - Incorrectly leaves spaces after a semi-colon
 * - Incorrectly trims lines breaking indents. First whitespace group needs to be preserved
 * - Need to revise how this code is laid out so it's easier for me to read.
 */

export function enforceOperatorSpacing(text: string): string {
    // Define regex patterns for operators (arithmetic, comparison, assignment, logical, commas)
    const operators = /([+\-*/%^=<>!]=?|:=|\.and\.|\.or\.|\.not\.)/gi;
    const commas = /,(?!\s)/g;

    // Regex to match quoted strings (including nested quotes) or block comments
    const stringOrCommentPattern = /("(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'|\/\*[\s\S]*?;)/g;

    // Tests
    const hasWSBeforeSemiColon = /\s+;/g;
    const hasWSAfterOpenBracket = /\(\s+/g;
    const hasWSBeforeCloseBracket = /\s+\)/g;
    const hasWSBeforeAfterAssignmentOperator = /\s*:=\s*/g;

    // Split the text into code and non-code (strings/comments) segments
    let segments = text.split(stringOrCommentPattern);

    // Process only the code segments
    for (let i = 0; i < segments.length; i += 2) {
        let segment = segments[i];

        // Replace operators with properly spaced versions
        segment = segment.replace(operators, " $1 ");
        segment = segment.replace(commas, ", ");

        // Handle special cases
        segment = segment.replace(hasWSBeforeSemiColon, ";"); // Remove space before semicolon
        segment = segment.replace(hasWSAfterOpenBracket, "("); // Remove space after opening parenthesis
        segment = segment.replace(hasWSBeforeCloseBracket, ")"); // Remove space before closing parenthesis
        segment = segment.replace(hasWSBeforeAfterAssignmentOperator, " := "); // Remove extra spacing around assignment operator

        // Handle negative numbers (don't add space after minus sign)
        segment = segment.replace(/(\s+)-(\d+)/g, "$1-$2");

        // Remove multiple spaces, but preserve newlines
        segment = segment.replace(/[^\S\n]+/g, " ");

        segments[i] = segment;
    }

    // Rejoin the segments
    return segments.join("");
}
