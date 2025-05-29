import { FormattingRule, FormattingContext } from "../formattingProvider";
import { SSLTokenizer, TokenType, Token } from "../../core/tokenizer";

/**
 * Handles spacing around operators using SSL tokenizer for accurate context detection
 * Based on SSL EBNF grammar specification
 */
export class OperatorSpacingRule implements FormattingRule {
    name = "Operator Spacing";
    description = "Ensures proper spacing around operators using SSL tokenizer";
    apply(line: string, context: FormattingContext): string {
        try {
            // Special case for .AND. and .OR. logical operators
            if (line.includes(".AND.") || line.includes(".OR.") || line.includes(".NOT.")) {
                // Use a regex-based approach for these specific operators
                line = this.handleLogicalOperators(line);
            }

            // Continue with normal tokenizer-based processing
            const tokenizer = new SSLTokenizer(line);
            const tokens = tokenizer.tokenize();

            if (!tokens || tokens.length === 0) {
                return line;
            }

            return this.formatOperatorsWithTokens(line, tokens);
        } catch (error) {
            // Fallback to legacy string-based processing
            return this.fallbackOperatorFormatting(line);
        }
    }
    /**
     * Handle the specific case of logical operators (.AND., .OR., .NOT.)
     */
    private handleLogicalOperators(line: string): string {
        // For each operator: first remove all spaces around it, then add exactly one space on each side
        // This ensures consistent spacing regardless of the input

        // Handle .AND. operator
        line = line.replace(/\s*\.AND\.\s*/g, ".AND."); // Remove all spaces
        line = line.replace(/\.AND\./g, " .AND. "); // Add one space on each side

        // Handle .OR. operator
        line = line.replace(/\s*\.OR\.\s*/g, ".OR."); // Remove all spaces
        line = line.replace(/\.OR\./g, " .OR. "); // Add one space on each side

        // Handle .NOT. operator
        line = line.replace(/\s*\.NOT\.\s*/g, ".NOT."); // Remove all spaces
        line = line.replace(/\.NOT\./g, " .NOT. "); // Add one space on each side

        // Fix cases where we added a space at the beginning of the line
        line = line.replace(/^\s*(\.AND\.|\.OR\.|\.NOT\.)/, "$1");

        // Fix double spaces that might have been created
        line = line.replace(/\s{2,}/g, " ");

        return line;
    }

    /**
     * Format operators using tokenized context for accuracy
     */
    private formatOperatorsWithTokens(line: string, tokens: Token[]): string {
        const parts: string[] = [];
        let currentPos = 0;
        // Ensure tokens are sorted by position, as the tokenizer might not guarantee order in all edge cases.
        const sortedTokens = [...tokens].sort((a, b) => a.position.offset - b.position.offset);

        for (const token of sortedTokens) {
            // Add original whitespace/text between the last token and current token
            if (token.position.offset > currentPos) {
                parts.push(line.substring(currentPos, token.position.offset));
            }

            if (
                this.isOperatorToken(token.type) &&
                !this.isStringOrCommentToken(token, sortedTokens)
            ) {
                const spacing = this.getOperatorSpacing(token, sortedTokens);
                if (spacing) {
                    parts.push(this.applySpacing(token.value, spacing));
                } else {
                    parts.push(token.value); // No specific spacing rule, use original
                }
            } else {
                parts.push(token.value); // Not an operator or in string/comment
            }
            currentPos = token.position.offset + token.length;
        }

        // Add any trailing whitespace after the last token from the original line
        if (currentPos < line.length) {
            parts.push(line.substring(currentPos));
        }

        let result = parts.join("");
        // Clean up multiple spaces that might have been introduced by combining original spacing and applied spacing.
        result = result.replace(/\s{2,}/g, " ");

        return result.trim();
    }

    /**
     * Determine if a token type represents an operator
     */
    private isOperatorToken(tokenType: TokenType): boolean {
        const operatorTypes = new Set([
            // Assignment operators
            TokenType.assign, // :=
            TokenType.plusAssign, // +=
            TokenType.minusAssign, // -=
            TokenType.multAssign, // *=
            TokenType.divAssign, // /=
            TokenType.powerAssign, // ^=

            // Comparison operators
            TokenType.equals, // ==
            TokenType.notEquals, // !=
            TokenType.lessThan, // <
            TokenType.greaterThan, // >
            TokenType.lessEqual, // <=
            TokenType.greaterEqual, // >=
            TokenType.simpleEquals, // =

            // Arithmetic operators
            TokenType.plus, // +
            TokenType.minus, // -
            TokenType.multiply, // *
            TokenType.divide, // /
            TokenType.power, // ^
            TokenType.modulo, // %

            // Increment/Decrement (special handling needed)
            TokenType.increment, // ++
            TokenType.decrement, // --            // Logical operators (handled as keywords in SSL)
            TokenType.logicalAnd, // .AND.
            TokenType.logicalOr, // .OR.
            TokenType.logicalNot, // .NOT.
            TokenType.and, // .AND.
            TokenType.or, // .OR.
            TokenType.not, // .NOT.
        ]);

        return operatorTypes.has(tokenType);
    }

    /**
     * Check if operator is inside string or comment context
     */
    private isStringOrCommentToken(operatorToken: Token, tokens: Token[]): boolean {
        // Find surrounding context tokens
        const operatorPos = operatorToken.position.offset;

        for (const token of tokens) {
            if (
                token.position.offset < operatorPos &&
                token.position.offset + token.value.length > operatorPos
            ) {
                // Operator is inside this token
                return (
                    token.type === TokenType.stringLiteral ||
                    token.type === TokenType.blockComment ||
                    token.type === TokenType.singleLineComment ||
                    token.type === TokenType.regionComment ||
                    token.type === TokenType.endregionComment
                );
            }
        }

        return false;
    }

    /**
     * Determine spacing requirements for an operator based on SSL grammar
     */
    private getOperatorSpacing(
        operatorToken: Token,
        tokens: Token[]
        // position: number // Removed unused parameter
    ): { before: boolean; after: boolean } | null {
        const tokenType = operatorToken.type;
        const operatorValue = operatorToken.value;

        // Special handling for SSL-specific operators
        switch (tokenType) {
            case TokenType.assign: // :=
            case TokenType.plusAssign: // +=
            case TokenType.minusAssign: // -=
            case TokenType.multAssign: // *=
            case TokenType.divAssign: // /=
            case TokenType.powerAssign: // ^=
            case TokenType.equals: // ==
            case TokenType.notEquals: // !=
            case TokenType.lessEqual: // <=
            case TokenType.greaterEqual: // >=
            case TokenType.lessThan: // <
            case TokenType.greaterThan: // >
            case TokenType.simpleEquals: // =
                return { before: true, after: true };

            case TokenType.plus: // +
            case TokenType.minus: // -
            case TokenType.multiply: // *
            case TokenType.divide: // /
            case TokenType.power: // ^
            case TokenType.modulo: // %
                // Check for unary context
                if (this.isUnaryOperator(operatorToken, tokens)) {
                    return { before: false, after: false };
                }
                return { before: true, after: true };

            case TokenType.increment: // ++
            case TokenType.decrement: // --
                // No spaces around increment/decrement
                return { before: false, after: false };

            case TokenType.logicalAnd: // .AND.
            case TokenType.logicalOr: // .OR.
            case TokenType.and: // .AND.
            case TokenType.or: // .OR.
                return { before: true, after: true };

            case TokenType.logicalNot: // .NOT.
            case TokenType.not: // .NOT.
                return { before: true, after: true };

            case TokenType.colon: // :
                // Special handling for property access vs. keywords
                if (this.isPropertyAccess(operatorToken, tokens)) {
                    return { before: false, after: false };
                }
                return null; // Let ColonSpacingRule handle keywords

            default:
                return null;
        }
    }

    /**
     * Check if operator is used in unary context
     */
    private isUnaryOperator(operatorToken: Token, tokens: Token[]): boolean {
        const operatorIndex = tokens.indexOf(operatorToken);
        if (operatorIndex === -1) {
            // Should not happen if token is from the tokens array
            return false;
        }

        // Find the previous non-whitespace token
        let prevSemanticToken: Token | null = null;
        for (let i = operatorIndex - 1; i >= 0; i--) {
            const currentScannedToken = tokens[i];
            if (
                currentScannedToken.type !== TokenType.whitespace &&
                currentScannedToken.type !== TokenType.newline
            ) {
                prevSemanticToken = currentScannedToken;
                break;
            }
        }

        if (prevSemanticToken === null) {
            // Operator is at the beginning of the line (or only preceded by whitespace)
            return true;
        }

        const prevTokenType = prevSemanticToken.type;

        // Unary if preceded by operators, keywords, or punctuation that can start an expression or sub-expression
        const unaryPrecedingTypes = new Set([
            TokenType.assign, // :=
            TokenType.plusAssign, // +=
            TokenType.minusAssign, // -=
            TokenType.multAssign, // *=
            TokenType.divAssign, // /=
            TokenType.powerAssign, // ^=
            TokenType.equals, // ==
            TokenType.notEquals, // !=
            TokenType.lessThan, // <
            TokenType.greaterThan, // >
            TokenType.lessEqual, // <=
            TokenType.greaterEqual, // >=
            TokenType.simpleEquals, // = (e.g. IF a = -b)
            TokenType.lparen, // (
            TokenType.lbrace, // {
            TokenType.lbracket, // [
            TokenType.comma, // ,
            TokenType.semicolon, // ; (e.g. ;-5)
            TokenType.if, // IF -a
            TokenType.while, // WHILE -a
            TokenType.for, // FOR i := -10
            TokenType.case, // CASE -1
            TokenType.return, // RETURN -1
            TokenType.to, // FOR I := 1 :TO -N
            // Other operators that can precede a unary operator
            TokenType.plus,
            TokenType.minus,
            TokenType.multiply,
            TokenType.divide,
            TokenType.power,
            TokenType.modulo,
            TokenType.and, // .AND.
            TokenType.or, // .OR.
            // TokenType.not, // .NOT. is usually unary itself, .NOT. -5 is rare.
        ]);

        return unaryPrecedingTypes.has(prevTokenType);
    }

    /**
     * Check if colon is used for property access (Object:Property)
     */
    private isPropertyAccess(colonToken: Token, tokens: Token[]): boolean {
        const colonIndex = tokens.indexOf(colonToken);

        if (colonIndex === 0 || colonIndex === tokens.length - 1) {
            return false;
        }

        const prevToken = tokens[colonIndex - 1];
        const nextToken = tokens[colonIndex + 1];

        // Property access: identifier : identifier
        return prevToken.type === TokenType.identifier && nextToken.type === TokenType.identifier;
    }
    /**
     * Apply spacing to operator
     */
    private applySpacing(
        operatorValue: string,
        spacing: { before: boolean; after: boolean }
    ): string {
        // Special handling for dot operators like .AND. .OR. .NOT.
        if (operatorValue.startsWith(".") && operatorValue.endsWith(".")) {
            // For logical operators, we want just one space before and after
            return spacing.before && spacing.after
                ? ` ${operatorValue} `
                : spacing.before
                ? ` ${operatorValue}`
                : spacing.after
                ? `${operatorValue} `
                : operatorValue;
        }

        let result = operatorValue;

        if (spacing.before) {
            result = " " + result;
        }
        if (spacing.after) {
            result = result + " ";
        }

        return result;
    }

    /**
     * Fallback formatting when tokenization fails
     */
    private fallbackOperatorFormatting(line: string): string {
        // Special handling for :FOR loops with :TO
        if (line.toLowerCase().includes(":for") && line.toLowerCase().includes(":to")) {
            line = line.replace(/(\s*):to(\s*)/gi, " :TO ");
        }

        let result = line;

        // Process longer operators first to avoid conflicts
        const operators = [
            // Logical operators (longest first)
            ".AND.",
            ".OR.",
            ".NOT.",

            // Assignment operators (2-character operators first)
            ":=",
            "+=",
            "-=",
            "*=",
            "/=",
            "^=",

            // Comparison operators (2-character operators first)
            "==",
            "!=",
            "<=",
            ">=",

            // Single-character operators
            "<",
            ">",
            "=",
            "+",
            "-",
            "*",
            "/",
            "%",
            "^",
        ];

        for (const operator of operators) {
            result = this.addSpacingAroundOperator(result, operator);
        }

        return result;
    }

    /**
     * Legacy string-based operator spacing (fallback)
     */ private addSpacingAroundOperator(text: string, operator: string): string {
        const leadingWhitespaceMatch = text.match(/^(\s*)/);
        const leadingWhitespace = leadingWhitespaceMatch ? leadingWhitespaceMatch[0] : "";
        const contentText = text.substring(leadingWhitespace.length);

        // Early return if operator not found
        if (!contentText.includes(operator)) {
            return text;
        } // Special handling for dot operators like .AND. .OR. .NOT.
        if (operator.startsWith(".") && operator.endsWith(".")) {
            // First, normalize to have exactly one space before and after
            const regex = new RegExp(`\\s*${operator.replace(/\./g, "\\.")}\\s*`, "g");
            let processedContent = contentText.replace(regex, ` ${operator} `);

            // Handle case with no spaces before
            processedContent = processedContent.replace(
                new RegExp(`([^\\s])${operator.replace(/\./g, "\\.")}\\s`, "g"),
                `$1 ${operator} `
            );

            // Handle case with no spaces after
            processedContent = processedContent.replace(
                new RegExp(`\\s${operator.replace(/\./g, "\\.")}([^\\s])`, "g"),
                ` ${operator} $1`
            );

            // Fix double spaces
            processedContent = processedContent.replace(/\s{2,}/g, " ");

            return leadingWhitespace + processedContent;
        }

        // Skip processing of single-char operators that are part of compound operators
        if (this.shouldSkipSingleCharOperator(contentText, operator)) {
            return text;
        }

        // Don't modify operators inside strings or comments
        const firstOperatorPos = contentText.indexOf(operator);
        if (this.isPositionInStringOrComment(contentText, firstOperatorPos)) {
            return text;
        }

        let processedContent = contentText;
        let startPos = 0;

        while (true) {
            const pos = processedContent.indexOf(operator, startPos);
            if (pos === -1) {
                break;
            }

            if (this.isPositionInStringOrComment(processedContent, pos)) {
                startPos = pos + operator.length;
                continue;
            }

            const hasBefore = pos > 0 && !processedContent[pos - 1].match(/\s/);
            const hasAfter =
                pos + operator.length < processedContent.length &&
                !processedContent[pos + operator.length].match(/\s/);

            const before = hasBefore
                ? processedContent.substring(0, pos) + " "
                : processedContent.substring(0, pos);
            const after = hasAfter
                ? " " + processedContent.substring(pos + operator.length)
                : processedContent.substring(pos + operator.length);

            processedContent = before + operator + after;
            startPos = before.length + operator.length;
        }

        processedContent = processedContent.replace(/\s{2,}/g, " ");
        return leadingWhitespace + processedContent;
    }

    /**
     * Check if single-char operator is part of compound operator
     */ private shouldSkipSingleCharOperator(text: string, operator: string): boolean {
        if (operator.length !== 1) {
            return false;
        }

        const compoundOperators: Record<string, string[]> = {
            opEquals: [":=", "==", "!=", "<=", ">=", "+=", "-=", "*=", "/=", "^="],
            opPlus: ["+=", "++"],
            opMinus: ["-=", "--"],
            opMultiply: ["*="],
            opDivide: ["/="],
            opPower: ["^="],
            opLessThan: ["<="],
            opGreaterThan: [">="],
            opBang: ["!="],
        };

        const compounds = compoundOperators[operator];
        if (!compounds) {
            return false;
        }

        return compounds.some((compound: string) => text.includes(compound));
    }

    /**
     * Check if position is inside string or comment (legacy)
     */
    private isPositionInStringOrComment(text: string, position: number): boolean {
        if (position < 0 || position >= text.length) {
            return false;
        }

        const beforePosition = text.substring(0, position);

        // Check SSL comment format: /* ... ;
        const lastCommentStart = beforePosition.lastIndexOf("/*");
        const lastCommentEnd = beforePosition.lastIndexOf(";");

        if (
            lastCommentStart !== -1 &&
            (lastCommentEnd === -1 || lastCommentStart > lastCommentEnd)
        ) {
            return true;
        }

        // Check string literals
        let inSingleQuote = false;
        let inDoubleQuote = false;

        for (let i = 0; i < position; i++) {
            if (text[i] === "'" && (i === 0 || text[i - 1] !== "\\")) {
                inSingleQuote = !inSingleQuote;
            } else if (text[i] === '"' && (i === 0 || text[i - 1] !== "\\")) {
                inDoubleQuote = !inDoubleQuote;
            }
        }

        return inSingleQuote || inDoubleQuote;
    }
}
