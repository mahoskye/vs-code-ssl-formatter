import { FormattingRule, FormattingContext } from "../formattingProvider";
import { TokenType } from "../../core/tokenizer";

/**
 * Handles alignment of multi-line statements in SSL code
 * Based on SSL EBNF grammar and common SSL formatting patterns
 */
export class BlockAlignmentRule implements FormattingRule {
    name = "Block Alignment";
    description = "Aligns multi-line statements, arrays, function calls, and SQL queries properly";

    private readonly INDENT_SIZE = 4;

    // SSL keywords that start blocks requiring indentation
    private readonly BLOCK_START_KEYWORDS = new Set([
        ":IF",
        ":WHILE",
        ":FOR",
        ":BEGINCASE",
        ":CASE",
        ":OTHERWISE",
        ":TRY",
        ":CATCH",
        ":FINALLY",
        ":PROCEDURE",
        ":REGION",
        ":BEGININLINECODE",
        ":CLASS",
        ":ERROR",
    ]);

    // SSL keywords that end blocks
    private readonly BLOCK_END_KEYWORDS = new Set([
        ":ENDIF",
        ":ENDWHILE",
        ":NEXT",
        ":ENDCASE",
        ":ENDTRY",
        ":ENDPROC",
        ":ENDREGION",
        ":ENDINLINECODE",
    ]);

    // Keywords that continue blocks at same level
    private readonly BLOCK_CONTINUE_KEYWORDS = new Set([
        ":ELSE",
        ":ELSEIF",
        ":CASE",
        ":OTHERWISE",
        ":CATCH",
        ":FINALLY",
    ]);

    apply(line: string, context: FormattingContext): string {
        const trimmedLine = line.trim();

        if (trimmedLine.length === 0) {
            return line; // Preserve empty lines
        }

        // Handle different types of multi-line constructs
        if (this.isArrayLiteral(trimmedLine, context)) {
            return this.alignArrayLiteral(line, context);
        }

        if (this.isFunctionCall(trimmedLine, context)) {
            return this.alignFunctionCall(line, context);
        }

        if (this.isSqlStatement(trimmedLine, context)) {
            return this.alignSqlStatement(line, context);
        }

        if (this.isBlockStructure(trimmedLine)) {
            return this.alignBlockStructure(line, context);
        }

        // Default case: apply standard indentation based on block depth
        return this.applyStandardIndentation(line, context);
    }

    private isArrayLiteral(line: string, context: FormattingContext): boolean {
        // Check if line is part of an array literal: { ... }
        return (
            line.includes("{") ||
            line.includes("}") ||
            (context.inMultiLineConstruct && context.constructType === "array")
        );
    }

    private isFunctionCall(line: string, context: FormattingContext): boolean {
        // Check for function calls with parameters spanning multiple lines
        const funcCallPattern = /\w+\s*\(/;
        return (
            funcCallPattern.test(line) ||
            (context.inMultiLineConstruct && context.constructType === "function")
        );
    }

    private isSqlStatement(line: string, context: FormattingContext): boolean {
        // Check for SQL statements (SqlExecute, LSearch)
        const sqlPattern = /(SqlExecute|LSearch)\s*\(/i;
        return (
            sqlPattern.test(line) ||
            (context.inMultiLineConstruct && context.constructType === "sql")
        );
    }

    private isBlockStructure(line: string): boolean {
        const upperLine = line.toUpperCase();
        return (
            Array.from(this.BLOCK_START_KEYWORDS).some((keyword) => upperLine.includes(keyword)) ||
            Array.from(this.BLOCK_END_KEYWORDS).some((keyword) => upperLine.includes(keyword)) ||
            Array.from(this.BLOCK_CONTINUE_KEYWORDS).some((keyword) => upperLine.includes(keyword))
        );
    }

    private alignArrayLiteral(line: string, context: FormattingContext): string {
        const trimmedLine = line.trim();
        const baseIndent = this.getBaseIndentation(context);

        // Opening brace
        if (trimmedLine.startsWith("{")) {
            return baseIndent + trimmedLine;
        }

        // Closing brace
        if (trimmedLine.startsWith("}")) {
            return baseIndent + trimmedLine;
        }

        // Array elements - indent one level from base
        if (context.inMultiLineConstruct && context.constructType === "array") {
            const elementIndent = baseIndent + " ".repeat(this.INDENT_SIZE);
            return elementIndent + trimmedLine;
        }

        return line;
    }

    private alignFunctionCall(line: string, context: FormattingContext): string {
        const trimmedLine = line.trim();
        const baseIndent = this.getBaseIndentation(context);

        // Function name and opening parenthesis
        if (trimmedLine.includes("(") && !trimmedLine.includes(")")) {
            return baseIndent + trimmedLine;
        }

        // Parameters - align with first parameter or indent from function name
        if (context.inMultiLineConstruct && context.constructType === "function") {
            // For SSL DoProc calls: DoProc("functionName", {param1, param2})
            if (trimmedLine.includes("DoProc") || trimmedLine.includes("ExecFunction")) {
                const paramIndent = baseIndent + " ".repeat(this.INDENT_SIZE);
                return paramIndent + trimmedLine;
            }

            // For regular function calls, align parameters
            const paramIndent = baseIndent + " ".repeat(this.INDENT_SIZE);
            return paramIndent + trimmedLine;
        }

        return line;
    }

    private alignSqlStatement(line: string, context: FormattingContext): string {
        const trimmedLine = line.trim();
        const baseIndent = this.getBaseIndentation(context);

        // SQL function call
        if (trimmedLine.match(/(SqlExecute|LSearch)\s*\(/i)) {
            return baseIndent + trimmedLine;
        }

        // SQL string content - apply SQL formatting
        if (context.inMultiLineConstruct && context.constructType === "sql") {
            const sqlIndent = baseIndent + " ".repeat(this.INDENT_SIZE);

            // SQL keywords should be aligned
            const sqlKeywords = [
                "SELECT",
                "FROM",
                "WHERE",
                "ORDER BY",
                "GROUP BY",
                "HAVING",
                "JOIN",
                "INNER JOIN",
                "LEFT JOIN",
                "RIGHT JOIN",
            ];
            const upperTrimmed = trimmedLine.toUpperCase();

            for (const keyword of sqlKeywords) {
                if (upperTrimmed.startsWith(keyword)) {
                    return sqlIndent + trimmedLine;
                }
            }

            // Other SQL content gets additional indentation
            return sqlIndent + " ".repeat(this.INDENT_SIZE) + trimmedLine;
        }

        return line;
    }

    private alignBlockStructure(line: string, context: FormattingContext): string {
        const trimmedLine = line.trim();
        const upperLine = trimmedLine.toUpperCase();
        let baseIndent = this.getBaseIndentation(context);

        // Block end keywords reduce indentation
        if (Array.from(this.BLOCK_END_KEYWORDS).some((keyword) => upperLine.includes(keyword))) {
            if (context.blockDepth > 0) {
                baseIndent = " ".repeat((context.blockDepth - 1) * this.INDENT_SIZE);
            }
            return baseIndent + trimmedLine;
        }

        // Block continue keywords stay at same level as their opening block
        if (
            Array.from(this.BLOCK_CONTINUE_KEYWORDS).some((keyword) => upperLine.includes(keyword))
        ) {
            if (context.blockDepth > 0) {
                baseIndent = " ".repeat((context.blockDepth - 1) * this.INDENT_SIZE);
            }
            return baseIndent + trimmedLine;
        }

        // Block start keywords
        if (Array.from(this.BLOCK_START_KEYWORDS).some((keyword) => upperLine.includes(keyword))) {
            return baseIndent + trimmedLine;
        }

        return line;
    }

    private applyStandardIndentation(line: string, context: FormattingContext): string {
        const trimmedLine = line.trim();
        const baseIndent = this.getBaseIndentation(context);

        // Comments preserve their relative positioning but respect block depth
        if (trimmedLine.startsWith("/*")) {
            return baseIndent + trimmedLine;
        }

        // Variable declarations and assignments
        if (
            trimmedLine.startsWith(":DECLARE") ||
            trimmedLine.startsWith(":PARAMETERS") ||
            trimmedLine.includes(":=")
        ) {
            return baseIndent + trimmedLine;
        }

        // Default: apply base indentation
        return baseIndent + trimmedLine;
    }

    private getBaseIndentation(context: FormattingContext): string {
        return " ".repeat(context.blockDepth * this.INDENT_SIZE);
    }
}
