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
            // Use SSL tokenizer for accurate parsing
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
     * Format operators using tokenized context for accuracy
     */
    private formatOperatorsWithTokens(line: string, tokens: Token[]): string {
        const parts: string[] = [];
        let currentPos = 0;
        // Ensure tokens are sorted by position, as the tokenizer might not guarantee order in all edge cases.
        const sortedTokens = [...tokens].sort((a, b) => a.position.offset - b.position.offset);

        for (const token of sortedTokens) {
            // Add whitespace before this token from the original line
            if (token.position.offset > currentPos) {
                parts.push(line.substring(currentPos, token.position.offset));
            }

            // Process the token itself
            // Pass sortedTokens to helper functions consistently
            if (
                this.isOperatorToken(token.type) &&
                !this.isStringOrCommentToken(token, sortedTokens)
            ) {
                const spacing = this.getOperatorSpacing(token, sortedTokens); // Removed 'position' argument
                if (spacing) {
                    parts.push(this.applySpacing(token.value, spacing));
                } else {
                    parts.push(token.value); // No specific spacing rule, use token value as is
                }
            } else {
                parts.push(token.value); // Not an operator or should be skipped, use token value as is
            }
            currentPos = token.position.offset + token.value.length;
        }

        // Add any trailing whitespace after the last token from the original line
        if (currentPos < line.length) {
            parts.push(line.substring(currentPos));
        }

        return parts.join("");
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
            TokenType.decrement, // --

            // Logical operators (handled as keywords in SSL)
            TokenType.logicalAnd, // .AND.
            TokenType.logicalOr, // .OR.
            TokenType.logicalNot, // .NOT.
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
        if (operatorIndex === 0) {
            return true;
        }

        const prevToken = tokens[operatorIndex - 1];

        // Unary if preceded by operators, keywords, or punctuation
        const unaryPrecedingTypes = new Set([
            TokenType.assign,
            TokenType.plusAssign,
            TokenType.minusAssign,
            TokenType.multAssign,
            TokenType.divAssign,
            TokenType.powerAssign,
            TokenType.equals,
            TokenType.notEquals,
            TokenType.lessThan,
            TokenType.greaterThan,
            TokenType.lessEqual,
            TokenType.greaterEqual,
            TokenType.simpleEquals,
            TokenType.lparen,
            TokenType.lbrace,
            TokenType.lbracket,
            TokenType.comma,
            TokenType.semicolon,
            TokenType.if,
            TokenType.while,
            TokenType.for,
            TokenType.case,
            TokenType.return,
        ]);

        return unaryPrecedingTypes.has(prevToken.type);
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
     */
    private addSpacingAroundOperator(text: string, operator: string): string {
        const leadingWhitespaceMatch = text.match(/^(\s*)/);
        const leadingWhitespace = leadingWhitespaceMatch ? leadingWhitespaceMatch[0] : "";
        const contentText = text.substring(leadingWhitespace.length);

        // Early return if operator not found
        if (!contentText.includes(operator)) {
            return text;
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
