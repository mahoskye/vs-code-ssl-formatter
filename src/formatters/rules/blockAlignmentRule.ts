import { FormattingRule, FormattingContext } from "../formattingProvider";
import { TokenType, SSLTokenizer, Token } from "../../core/tokenizer";

/**
 * Handles alignment of multi-line statements in SSL code
 * Based on SSL EBNF grammar and common SSL formatting patterns
 */
export class BlockAlignmentRule implements FormattingRule {
    name = "Block Alignment";
    description = "Aligns multi-line statements, arrays, function calls, and SQL queries properly";

    apply(line: string, context: FormattingContext): string {
        const trimmedLine = line.trim();

        if (trimmedLine.length === 0) {
            return line; // Preserve empty lines, indentation handled by provider
        }

        // Primary indentation is now handled by formattingProvider.ts.
        // This rule focuses on intra-line alignment for specific constructs.

        // Use tokens from context directly
        const tokens = context.lineTokens;

        // Handle different types of multi-line constructs
        if (context.constructType === "array" || this.lineSuggestsArray(tokens)) {
            return this.alignArrayLiteral(trimmedLine, context, tokens);
        }

        if (context.constructType === "functionCall" || this.lineSuggestsFunctionCall(tokens)) {
            return this.alignFunctionCall(trimmedLine, context, tokens);
        }

        if (context.constructType === "sqlStatement" || this.lineSuggestsSqlStatement(tokens)) {
            return this.alignSqlStatement(trimmedLine, context, tokens);
        }

        // Block structure alignment (e.g. :IF/:ENDIF) is primarily handled by provider's indentLevel.
        // This rule might add minor adjustments if needed in the future, but for now, it defers.
        // if (this.isBlockStructure(tokens)) {
        //     return this.alignBlockStructure(line, tokens, context);
        // }

        return trimmedLine; // Return trimmed line; provider adds indentation
    }

    private lineSuggestsArray(tokens: Token[]): boolean {
        return tokens.some(
            (token) => token.type === TokenType.lbrace || token.type === TokenType.rbrace
        );
    }

    private lineSuggestsFunctionCall(tokens: Token[]): boolean {
        for (let i = 0; i < tokens.length - 1; i++) {
            if (
                tokens[i].type === TokenType.identifier &&
                tokens[i + 1].type === TokenType.lparen
            ) {
                return true;
            }
        }
        const funcCallKeywords = [
            TokenType.doProc,
            TokenType.execFunction,
            TokenType.execUDF,
            TokenType.createUDObject,
            TokenType.sqlExecute, // Can also be SQL, but often has parens
            TokenType.lSearch,
        ];
        if (tokens.length > 0 && funcCallKeywords.includes(tokens[0].type)) {
            if (tokens.length > 1 && tokens[1].type === TokenType.lparen) {
                return true;
            }
        }
        return false;
    }

    private lineSuggestsSqlStatement(tokens: Token[]): boolean {
        return tokens.some(
            (token) => token.type === TokenType.sqlExecute || token.type === TokenType.lSearch
        );
    }

    private alignArrayLiteral(line: string, context: FormattingContext, tokens: Token[]): string {
        // Basic alignment for array elements.
        // Assumes one element per line for multi-line arrays, or aligns elements on the same line.
        // TODO: Implement more sophisticated alignment, e.g., aligning values if multiple key-value pairs are on one line.
        const trimmedLine = line.trim();
        if (trimmedLine.startsWith("{") && trimmedLine.endsWith("}")) {
            // Single line array: { elem1, elem2 }
            // Apply spacing around commas, braces (handled by other rules or basic trimming here)
            let content = trimmedLine.substring(1, trimmedLine.length - 1).trim();
            // Basic comma spacing, other rules might do more
            content = content
                .split(",")
                .map((s) => s.trim())
                .join(", ");
            return `{ ${content} }`;
        }
        if (trimmedLine.startsWith("{")) {
            return "{ " + trimmedLine.substring(1).trimStart();
        }
        if (trimmedLine.endsWith("}")) {
            return trimmedLine.substring(0, trimmedLine.length - 1).trimEnd() + " }";
        }
        // For lines within a multi-line array, ensure they are simply trimmed.
        // The main indentation is handled by the provider.
        // If a line starts with a comma, ensure one space after it.
        if (trimmedLine.startsWith(",")) {
            return ", " + trimmedLine.substring(1).trimStart();
        }
        return trimmedLine;
    }

    private alignFunctionCall(line: string, context: FormattingContext, tokens: Token[]): string {
        // Basic alignment for function call arguments.
        // TODO: Implement alignment of arguments, especially if they span multiple lines or have named parameters.
        const trimmedLine = line.trim();
        // If line starts with '(', typically a continuation of parameters
        if (
            trimmedLine.startsWith("(") &&
            tokens.length > 0 &&
            tokens[0].type === TokenType.lparen
        ) {
            // This is likely the start of parameters on a new line, or just wrapped parameters.
            // Provider handles base indent. We ensure content after '(' is trimmed.
            return "(" + trimmedLine.substring(1).trimStart();
        }
        // If line ends with ')', typically the end of parameters
        if (
            trimmedLine.endsWith(")") &&
            tokens.length > 0 &&
            tokens[tokens.length - 1].type === TokenType.rparen
        ) {
            return trimmedLine.substring(0, trimmedLine.length - 1).trimEnd() + ")";
        }
        // If a line starts with a comma (argument separator)
        if (trimmedLine.startsWith(",")) {
            return ", " + trimmedLine.substring(1).trimStart();
        }
        // For other lines within a multi-line function call, just trim.
        return trimmedLine;
    }

    private alignSqlStatement(line: string, context: FormattingContext, tokens: Token[]): string {
        // Basic alignment for SQL statements.
        // TODO: Implement more specific SQL keyword alignment (SELECT, FROM, WHERE, etc.)
        // For now, mainly ensures trimming and consistent continuation if needed.
        const trimmedLine = line.trim();
        // Example: If a line starts with a common SQL keyword that should be on its own line or indented.
        const sqlKeywords = [
            "SELECT",
            "FROM",
            "WHERE",
            "AND",
            "OR",
            "ORDER",
            "GROUP",
            "BY",
            "INSERT",
            "INTO",
            "VALUES",
            "UPDATE",
            "SET",
            "DELETE",
        ];
        const firstTokenText = tokens.length > 0 ? tokens[0].value.toUpperCase() : ""; // Changed token.text to token.value

        if (sqlKeywords.includes(firstTokenText) && !line.toUpperCase().startsWith("SQLEXECUTE")) {
            // This is a very basic heuristic. More advanced SQL formatting is complex.
            // The main indentation is handled by the provider. This rule might adjust sub-indentation for SQL parts.
            // For now, just return the trimmed line.
        }
        // If a line starts with a comma (often in SELECT lists or VALUES)
        if (trimmedLine.startsWith(",")) {
            return ", " + trimmedLine.substring(1).trimStart();
        }
        return trimmedLine;
    }

    // Removed isArrayLiteral, isFunctionCall, isSqlStatement, isBlockStructure as their logic is integrated or simplified.
    // The blockStartTokenTypes, blockEndTokenTypes, blockContinueTokenTypes are also not directly used in this simplified version
    // as the primary indentation and block detection is now in formattingProvider.
}
