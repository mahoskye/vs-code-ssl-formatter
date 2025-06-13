/**
 * Spacing Manager
 *
 * Manages consistent spacing rules for SSL code formatting according to EBNF grammar:
 * - Rules for spacing around operators (arithmetic, comparison, logical, assignment)
 * - Rules for spacing in lists (commas, parameters, array elements)
 * - Handle special cases like skipped parameters (param1,,param3)
 * - Property access spacing (object:property - SSL uses colon without spaces)
 * - Parentheses and bracket spacing
 * - Function call parameter spacing
 */

import { TokenType } from "../tokenizer/tokenType";
import { FormatterOptions } from "./options";

/**
 * Interface for spacing rules context
 */
export interface SpacingContext {
    /** Previous token type */
    previousTokenType?: TokenType;
    /** Current token type */
    currentTokenType: TokenType;
    /** Next token type */
    nextTokenType?: TokenType;
    /** Whether we're inside a function call */
    inFunctionCall?: boolean;
    /** Whether we're inside an array literal */
    inArrayLiteral?: boolean;
    /** Whether we're processing a skipped parameter (empty) */
    isSkippedParameter?: boolean;
    /** Current nesting level for parentheses/brackets */
    nestingLevel?: number;
}

/**
 * Result of spacing analysis
 */
export interface SpacingResult {
    /** Whether to insert space before the token */
    spaceBefore: boolean;
    /** Whether to insert space after the token */
    spaceAfter: boolean;
    /** Optional explanation for debugging */
    reason?: string;
}

/**
 * SSL Spacing Manager
 *
 * Implements spacing rules according to SSL EBNF grammar and formatting conventions:
 * - Binary operators with spaces: x + y, x .AND. y, x := value
 * - Unary operators without spaces: +x, -x, !condition, .NOT.condition
 * - Property access without spaces: object:property
 * - Commas with space after: param1, param2 (except for skipped parameters: param1,,param3)
 * - Parentheses without internal spaces: Function(param)
 * - Array literals with spaces after commas: {1, 2, 3}
 * - Assignment operators with spaces: variable := value
 */
export class SpacingManager {
    private readonly options: FormatterOptions;

    constructor(options: FormatterOptions) {
        this.options = options;
    }

    /**
     * Determine spacing around a token based on context
     */
    getSpacing(context: SpacingContext): SpacingResult {
        const { currentTokenType, previousTokenType, nextTokenType } = context;

        // Handle specific token types
        switch (currentTokenType) {
            case TokenType.COMMA:
                return this.getCommaSpacing(context);

            case TokenType.COLON:
                return this.getColonSpacing(context);

            case TokenType.SEMICOLON:
                return this.getSemicolonSpacing(context);

            case TokenType.LPAREN:
            case TokenType.RPAREN:
                return this.getParenthesesSpacing(context);

            case TokenType.LBRACKET:
            case TokenType.RBRACKET:
                return this.getBracketSpacing(context);

            case TokenType.LBRACE:
            case TokenType.RBRACE:
                return this.getBraceSpacing(context);

            // Assignment operators
            case TokenType.ASSIGN:
            case TokenType.PLUS_ASSIGN:
            case TokenType.MINUS_ASSIGN:
            case TokenType.MULT_ASSIGN:
            case TokenType.DIV_ASSIGN:
            case TokenType.POWER_ASSIGN:
                return this.getAssignmentOperatorSpacing(context);

            // Comparison operators
            case TokenType.EQUAL:
            case TokenType.STRICT_EQUAL:
            case TokenType.NOT_EQUAL:
            case TokenType.LESS_THAN:
            case TokenType.GREATER_THAN:
            case TokenType.LESS_EQUAL:
            case TokenType.GREATER_EQUAL:
                return this.getComparisonOperatorSpacing(context);

            // Logical operators
            case TokenType.AND:
            case TokenType.OR:
            case TokenType.NOT:
                return this.getLogicalOperatorSpacing(context);

            // Arithmetic operators
            case TokenType.PLUS:
            case TokenType.MINUS:
            case TokenType.MULTIPLY:
            case TokenType.DIVIDE:
            case TokenType.MODULO:
            case TokenType.POWER:
                return this.getArithmeticOperatorSpacing(context);

            // Increment/decrement operators
            case TokenType.INCREMENT:
            case TokenType.DECREMENT:
                return this.getIncrementDecrementSpacing(context);

            default:
                return this.getDefaultSpacing(context);
        }
    }

    /**
     * Handle comma spacing - key for SSL parameter lists and arrays
     * Special handling for skipped parameters: param1,,param3
     */
    private getCommaSpacing(context: SpacingContext): SpacingResult {
        const { nextTokenType, isSkippedParameter } = context;

        // For skipped parameters (param1,,param3), no space after first comma
        if (isSkippedParameter || nextTokenType === TokenType.COMMA) {
            return {
                spaceBefore: false,
                spaceAfter: false,
                reason: "Skipped parameter - SSL style",
            };
        }

        // Normal comma spacing based on options
        return {
            spaceBefore: false,
            spaceAfter: this.options.insertSpacesAfterCommas,
            reason: "Standard comma spacing",
        };
    }

    /**
     * Handle colon spacing - critical for SSL property access (object:property)
     * SSL uses object:property without spaces, unlike JavaScript's object.property
     */
    private getColonSpacing(context: SpacingContext): SpacingResult {
        // SSL property access uses colon without spaces: object:property
        return {
            spaceBefore: this.options.insertSpacesAroundPropertyAccess,
            spaceAfter: this.options.insertSpacesAroundPropertyAccess,
            reason: "SSL property access colon",
        };
    }

    /**
     * Handle semicolon spacing - SSL statement terminator
     */
    private getSemicolonSpacing(context: SpacingContext): SpacingResult {
        return {
            spaceBefore: false,
            spaceAfter: false,
            reason: "SSL statement terminator",
        };
    }

    /**
     * Handle parentheses spacing
     */
    private getParenthesesSpacing(context: SpacingContext): SpacingResult {
        const { currentTokenType, inFunctionCall } = context;

        if (currentTokenType === TokenType.LPAREN) {
            return {
                spaceBefore: inFunctionCall ? false : true,
                spaceAfter: false,
                reason: inFunctionCall ? "Function call" : "Grouping parentheses",
            };
        } else {
            // RPAREN
            return {
                spaceBefore: false,
                spaceAfter: true,
                reason: "Closing parenthesis",
            };
        }
    }

    /**
     * Handle bracket spacing for arrays
     */
    private getBracketSpacing(context: SpacingContext): SpacingResult {
        return {
            spaceBefore: false,
            spaceAfter: false,
            reason: "Array access brackets",
        };
    }

    /**
     * Handle brace spacing for array literals and code blocks
     */
    private getBraceSpacing(context: SpacingContext): SpacingResult {
        const { currentTokenType } = context;

        if (currentTokenType === TokenType.LBRACE) {
            return {
                spaceBefore: true,
                spaceAfter: false,
                reason: "Opening brace",
            };
        } else {
            // RBRACE
            return {
                spaceBefore: false,
                spaceAfter: true,
                reason: "Closing brace",
            };
        }
    }

    /**
     * Handle assignment operator spacing (:=, +=, -=, etc.)
     */
    private getAssignmentOperatorSpacing(context: SpacingContext): SpacingResult {
        return {
            spaceBefore: this.options.insertSpacesAroundAssignmentOperators,
            spaceAfter: this.options.insertSpacesAroundAssignmentOperators,
            reason: "Assignment operator",
        };
    }

    /**
     * Handle comparison operator spacing (=, ==, <, >, etc.)
     */
    private getComparisonOperatorSpacing(context: SpacingContext): SpacingResult {
        return {
            spaceBefore: this.options.insertSpacesAroundComparisonOperators,
            spaceAfter: this.options.insertSpacesAroundComparisonOperators,
            reason: "Comparison operator",
        };
    }

    /**
     * Handle logical operator spacing (.AND., .OR., .NOT.)
     */
    private getLogicalOperatorSpacing(context: SpacingContext): SpacingResult {
        const { currentTokenType } = context;

        if (currentTokenType === TokenType.NOT) {
            // .NOT. is a unary operator - space after but not before when used as prefix
            return {
                spaceBefore: false,
                spaceAfter: true,
                reason: "Unary logical NOT operator",
            };
        }

        // .AND. and .OR. are binary operators
        return {
            spaceBefore: this.options.insertSpacesAroundLogicalOperators,
            spaceAfter: this.options.insertSpacesAroundLogicalOperators,
            reason: "Binary logical operator",
        };
    }

    /**
     * Handle arithmetic operator spacing (+, -, *, /, %, ^)
     */
    private getArithmeticOperatorSpacing(context: SpacingContext): SpacingResult {
        const { currentTokenType, previousTokenType } = context;

        // Check if this is a unary operator (no previous token or previous token is operator/punctuation)
        const isUnary =
            !previousTokenType ||
            this.isOperatorOrPunctuation(previousTokenType) ||
            previousTokenType === TokenType.LPAREN ||
            previousTokenType === TokenType.COMMA;

        if (
            isUnary &&
            (currentTokenType === TokenType.PLUS || currentTokenType === TokenType.MINUS)
        ) {
            return {
                spaceBefore: false,
                spaceAfter: false,
                reason: "Unary arithmetic operator",
            };
        }

        // Binary arithmetic operator
        return {
            spaceBefore: this.options.insertSpacesAroundOperators,
            spaceAfter: this.options.insertSpacesAroundOperators,
            reason: "Binary arithmetic operator",
        };
    }

    /**
     * Handle increment/decrement operator spacing (++, --)
     * These should never have spaces: ++var or var++
     */
    private getIncrementDecrementSpacing(context: SpacingContext): SpacingResult {
        return {
            spaceBefore: false,
            spaceAfter: false,
            reason: "Increment/decrement operator - no spaces",
        };
    }

    /**
     * Default spacing for other tokens
     */
    private getDefaultSpacing(context: SpacingContext): SpacingResult {
        return {
            spaceBefore: false,
            spaceAfter: false,
            reason: "Default - no spacing",
        };
    }

    /**
     * Check if a token type is an operator or punctuation
     */
    private isOperatorOrPunctuation(tokenType: TokenType): boolean {
        const operatorAndPunctuation = [
            TokenType.ASSIGN,
            TokenType.PLUS_ASSIGN,
            TokenType.MINUS_ASSIGN,
            TokenType.MULT_ASSIGN,
            TokenType.DIV_ASSIGN,
            TokenType.POWER_ASSIGN,
            TokenType.EQUAL,
            TokenType.STRICT_EQUAL,
            TokenType.NOT_EQUAL,
            TokenType.LESS_THAN,
            TokenType.GREATER_THAN,
            TokenType.LESS_EQUAL,
            TokenType.GREATER_EQUAL,
            TokenType.AND,
            TokenType.OR,
            TokenType.NOT,
            TokenType.PLUS,
            TokenType.MINUS,
            TokenType.MULTIPLY,
            TokenType.DIVIDE,
            TokenType.MODULO,
            TokenType.POWER,
            TokenType.COMMA,
            TokenType.SEMICOLON,
            TokenType.COLON,
            TokenType.LPAREN,
            TokenType.RPAREN,
            TokenType.LBRACKET,
            TokenType.RBRACKET,
            TokenType.LBRACE,
            TokenType.RBRACE,
        ];

        return operatorAndPunctuation.includes(tokenType);
    }

    /**
     * Determine if spaces should be added around a specific operator type
     * Used by expression formatters for quick operator type checking
     */
    static shouldAddSpacesAroundOperator(tokenType: TokenType, options: FormatterOptions): boolean {
        switch (tokenType) {
            case TokenType.AND:
            case TokenType.OR:
                return options.insertSpacesAroundLogicalOperators;

            case TokenType.EQUAL:
            case TokenType.STRICT_EQUAL:
            case TokenType.NOT_EQUAL:
            case TokenType.LESS_THAN:
            case TokenType.GREATER_THAN:
            case TokenType.LESS_EQUAL:
            case TokenType.GREATER_EQUAL:
                return options.insertSpacesAroundComparisonOperators;

            case TokenType.ASSIGN:
            case TokenType.PLUS_ASSIGN:
            case TokenType.MINUS_ASSIGN:
            case TokenType.MULT_ASSIGN:
            case TokenType.DIV_ASSIGN:
            case TokenType.POWER_ASSIGN:
                return options.insertSpacesAroundAssignmentOperators;

            case TokenType.PLUS:
            case TokenType.MINUS:
            case TokenType.MULTIPLY:
            case TokenType.DIVIDE:
            case TokenType.MODULO:
            case TokenType.POWER:
                return options.insertSpacesAroundOperators;

            case TokenType.COLON:
                return options.insertSpacesAroundPropertyAccess;

            case TokenType.INCREMENT:
            case TokenType.DECREMENT:
                return false; // Never spaces around increment/decrement

            default:
                return options.insertSpacesAroundOperators;
        }
    }

    /**
     * Handle special SSL list formatting cases
     * Formats parameter lists, argument lists, and array elements
     */
    static formatList<T>(
        items: T[],
        formatter: (item: T, index: number) => string,
        options: FormatterOptions,
        allowSkippedElements = true
    ): string {
        const result: string[] = [];

        for (let i = 0; i < items.length; i++) {
            const item = items[i];

            // Handle skipped parameters (null/undefined items)
            if (allowSkippedElements && (item === null || item === undefined)) {
                result.push("");
            } else {
                result.push(formatter(item, i));
            }

            // Add comma separator (except for last item)
            if (i < items.length - 1) {
                const nextItem = items[i + 1];
                const isNextSkipped =
                    allowSkippedElements && (nextItem === null || nextItem === undefined);

                // For skipped parameters: "param1,,param3" - no space after first comma
                if (allowSkippedElements && isNextSkipped) {
                    result.push(",");
                } else {
                    // Normal comma with optional space
                    result.push(options.insertSpacesAfterCommas ? ", " : ",");
                }
            }
        }

        return result.join("");
    }

    /**
     * Check if a sequence of tokens represents skipped parameters
     * Used to detect patterns like: param1,,param3
     */
    static hasSkippedParameters(tokenTypes: TokenType[]): boolean {
        for (let i = 0; i < tokenTypes.length - 1; i++) {
            if (tokenTypes[i] === TokenType.COMMA && tokenTypes[i + 1] === TokenType.COMMA) {
                return true;
            }
        }
        return false;
    }

    /**
     * Get appropriate spacing for function call parameters
     * Handles both normal parameters and DoProc/ExecFunction SSL patterns
     */
    static getFunctionParameterSpacing(
        parameterIndex: number,
        totalParameters: number,
        isLastParameter: boolean,
        options: FormatterOptions
    ): { beforeComma: string; afterComma: string } {
        if (isLastParameter) {
            return { beforeComma: "", afterComma: "" };
        }

        return {
            beforeComma: "",
            afterComma: options.insertSpacesAfterCommas ? " " : "",
        };
    }
}
