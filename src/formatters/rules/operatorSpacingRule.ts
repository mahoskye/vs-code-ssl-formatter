import { FormattingRule, FormattingContext } from "../formattingProvider";

/**
 * Handles spacing around operators
 */
export class OperatorSpacingRule implements FormattingRule {
    name = "Operator Spacing";
    description = "Ensures proper spacing around operators";
    apply(line: string, context: FormattingContext): string {
        // Special handling for :FOR loops with :TO
        // Ensure proper spacing around ":TO" in for loops
        if (line.toLowerCase().includes(":for") && line.toLowerCase().includes(":to")) {
            // Add spacing around :TO in for loops
            line = line.replace(/(\s*):to(\s*)/gi, " :TO ");
        }

        let result = line;

        // Process longer operators first to avoid conflicts
        // Logical operators (longest first)
        result = this.addSpacingAroundOperator(result, ".AND.");
        result = this.addSpacingAroundOperator(result, ".OR.");

        // Assignment operators (2-character operators first)
        result = this.addSpacingAroundOperator(result, ":=");
        result = this.addSpacingAroundOperator(result, "+=");
        result = this.addSpacingAroundOperator(result, "-=");
        result = this.addSpacingAroundOperator(result, "*=");
        result = this.addSpacingAroundOperator(result, "/=");
        result = this.addSpacingAroundOperator(result, "^=");

        // Comparison operators (2-character operators first)
        result = this.addSpacingAroundOperator(result, "==");
        result = this.addSpacingAroundOperator(result, "!=");
        result = this.addSpacingAroundOperator(result, "<=");
        result = this.addSpacingAroundOperator(result, ">=");

        // Single-character comparison operators
        result = this.addSpacingAroundOperator(result, "<");
        result = this.addSpacingAroundOperator(result, ">");
        result = this.addSpacingAroundOperator(result, "="); // Simple equals operator (last among comparison)

        // Arithmetic operators (but not when part of unary expressions)
        result = this.addSpacingAroundOperator(result, "+", true);
        result = this.addSpacingAroundOperator(result, "-", true);
        result = this.addSpacingAroundOperator(result, "*", true);
        result = this.addSpacingAroundOperator(result, "/", true);
        result = this.addSpacingAroundOperator(result, "%");
        result = this.addSpacingAroundOperator(result, "^", true);

        return result;
    }
    private addSpacingAroundOperator(text: string, operator: string, checkUnary = false): string {
        const leadingWhitespaceMatch = text.match(/^(\s*)/);
        const leadingWhitespace = leadingWhitespaceMatch ? leadingWhitespaceMatch[0] : "";
        const contentText = text.substring(leadingWhitespace.length);

        // Early return if operator not found in content
        if (!contentText.includes(operator)) {
            return text; // Return original full text, which includes its original leading whitespace
        }

        // Skip processing of single-char operators that are part of compound operators we've already processed
        if (this.shouldSkipSingleCharOperator(contentText, operator)) {
            return text;
        }

        // Don't modify operators inside strings or comments (check on contentText)
        const firstOperatorPos = contentText.indexOf(operator);
        if (this.isPositionInStringOrComment(contentText, firstOperatorPos)) {
            return text; // Return original full text
        }

        // Escape special regex characters but keep the operator as a single unit
        const escapedOperator = operator.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");

        // Special handling for multi-character operators to avoid breaking them apart
        if (operator.length > 1) {
            // Use a non-breaking regex approach for multi-character operators
            let processedContent = contentText;

            // Find all occurrences that aren't in strings or comments
            let startPos = 0;
            while (true) {
                const pos = processedContent.indexOf(operator, startPos);
                if (pos === -1) {
                    break;
                }

                // Check if this occurrence is in a string or comment
                if (this.isPositionInStringOrComment(processedContent, pos)) {
                    startPos = pos + operator.length;
                    continue;
                }

                // Check if there's a character before
                const hasBefore = pos > 0 && !processedContent[pos - 1].match(/\s/);

                // Check if there's a character after
                const hasAfter =
                    pos + operator.length < processedContent.length &&
                    !processedContent[pos + operator.length].match(/\s/);

                // Replace the operator with properly spaced version
                const before = hasBefore
                    ? processedContent.substring(0, pos) + " "
                    : processedContent.substring(0, pos);
                const after = hasAfter
                    ? " " + processedContent.substring(pos + operator.length)
                    : processedContent.substring(pos + operator.length);

                processedContent = before + operator + after;

                // Move past this occurrence
                startPos = before.length + operator.length;
            }

            // Clean up extra spaces
            processedContent = processedContent.replace(/\s{2,}/g, " ");
            return leadingWhitespace + processedContent;
        }

        // For single-character operators, use the same non-breaking approach
        let processedContent = contentText;
        let startPos = 0;

        while (true) {
            const pos = processedContent.indexOf(operator, startPos);
            if (pos === -1) {
                break;
            }

            // Check if this occurrence is in a string or comment
            if (this.isPositionInStringOrComment(processedContent, pos)) {
                startPos = pos + operator.length;
                continue;
            }

            // Check if there's a character before
            const hasBefore = pos > 0 && !processedContent[pos - 1].match(/\s/);

            // Check if there's a character after
            const hasAfter =
                pos + operator.length < processedContent.length &&
                !processedContent[pos + operator.length].match(/\s/);

            // Replace the operator with properly spaced version
            const before = hasBefore
                ? processedContent.substring(0, pos) + " "
                : processedContent.substring(0, pos);
            const after = hasAfter
                ? " " + processedContent.substring(pos + operator.length)
                : processedContent.substring(pos + operator.length);

            processedContent = before + operator + after;

            // Move past this occurrence
            startPos = before.length + operator.length;
        }

        // Clean up extra spaces
        processedContent = processedContent.replace(/\s{2,}/g, " ");

        return leadingWhitespace + processedContent;
    }
    /**
     * Determines if a single-character operator should be skipped because it's part of a compound operator
     */ private shouldSkipSingleCharOperator(text: string, operator: string): boolean {
        if (operator.length !== 1) {
            return false;
        }

        // Check if this single-char operator is part of a compound operator we've already processed
        switch (operator) {
            case "=":
                // Check if this single = is part of :=, ==, !=, <=, >=, +=, -=, *=, /=, ^=
                for (const compoundOp of [
                    ":=",
                    "==",
                    "!=",
                    "<=",
                    ">=",
                    "+=",
                    "-=",
                    "*=",
                    "/=",
                    "^=",
                ]) {
                    if (text.includes(compoundOp)) {
                        // Check each occurrence of the compound operator
                        let pos = 0;
                        while ((pos = text.indexOf(compoundOp, pos)) !== -1) {
                            // Check if our single = character is part of this compound operator
                            const eqPos = pos + compoundOp.indexOf("=");
                            if (eqPos === text.indexOf("=", pos)) {
                                return true; // This = is part of a compound operator
                            }
                            pos += compoundOp.length;
                        }
                    }
                }
                return false;
            case "+":
                return this.isCharPartOfCompoundOp(text, "+", "+=");
            case "-":
                return this.isCharPartOfCompoundOp(text, "-", "-=");
            case "*":
                return this.isCharPartOfCompoundOp(text, "*", "*=");
            case "/":
                return this.isCharPartOfCompoundOp(text, "/", "/=");
            case "^":
                return this.isCharPartOfCompoundOp(text, "^", "^=");
            case "<":
                return this.isCharPartOfCompoundOp(text, "<", "<=");
            case ">":
                return this.isCharPartOfCompoundOp(text, ">", ">=");
            default:
                return false;
        }
    }
    /**
     * Checks if a single character operator is part of a compound operator
     * @param text The text to check
     * @param char The single character to check
     * @param compoundOp The compound operator to check against
     * @returns True if the single character is part of the compound operator
     */
    private isCharPartOfCompoundOp(text: string, char: string, compoundOp: string): boolean {
        if (!text.includes(compoundOp)) {
            return false;
        }

        // Check each occurrence of the compound operator
        let pos = 0;
        while ((pos = text.indexOf(compoundOp, pos)) !== -1) {
            // Check if our single character is part of this compound operator
            const charPos = pos + compoundOp.indexOf(char);
            if (charPos === text.indexOf(char, pos)) {
                return true; // This character is part of a compound operator
            }
            pos += compoundOp.length;
        }

        return false;
    }
    private isPositionInStringOrComment(text: string, position: number): boolean {
        // If position is invalid, consider it not in a string/comment
        if (position < 0 || position >= text.length) {
            return false;
        }

        const beforePosition = text.substring(0, position);

        // Check if we're inside a comment block
        const lastCommentStart = beforePosition.lastIndexOf("/*");
        const lastCommentEnd = beforePosition.lastIndexOf(";"); // SSL comments end with ;

        if (
            lastCommentStart !== -1 &&
            (lastCommentEnd === -1 || lastCommentStart > lastCommentEnd)
        ) {
            // We're inside a comment block
            return true;
        }

        // Special case for bracket notation [e*f]
        if (beforePosition.lastIndexOf("[") > beforePosition.lastIndexOf("]")) {
            return true;
        }

        // More accurate string detection
        let inSingleQuote = false;
        let inDoubleQuote = false;
        let i = 0;

        // Walk through the string and toggle quote state
        while (i < position) {
            if (text[i] === "'" && (i === 0 || text[i - 1] !== "\\")) {
                inSingleQuote = !inSingleQuote;
            } else if (text[i] === '"' && (i === 0 || text[i - 1] !== "\\")) {
                inDoubleQuote = !inDoubleQuote;
            }
            i++;
        }

        return inSingleQuote || inDoubleQuote;
    }
}
