import { FormattingRule, FormattingContext } from "../formattingProvider";
import { TokenType, SSLTokenizer, Token } from "../../core/tokenizer";

/**
 * Handles alignment of multi-line statements in SSL code
 * Based on SSL EBNF grammar and common SSL formatting patterns
 */
export class BlockAlignmentRule implements FormattingRule {
    name = "Block Alignment";
    description =
        "Aligns multi-line statements, arrays, function calls, and SQL queries properly. Also handles base indentation for block keywords and content lines.";

    // Define these sets for keyword categorization
    private readonly keywordsIndentCurrentDepth = new Set([
        ":IF",
        ":WHILE",
        ":FOR",
        ":BEGINCASE",
        ":TRY",
        ":PROCEDURE",
        ":REGION",
        ":ERROR",
    ]);
    private readonly keywordsIndentPreviousDepth = new Set([
        ":ELSE",
        ":ELSEIF",
        ":ENDIF",
        ":ENDWHILE",
        ":NEXT",
        ":CASE",
        ":OTHERWISE",
        ":ENDCASE",
        ":CATCH",
        ":FINALLY",
        ":ENDTRY",
        ":ENDPROC",
        ":ENDREGION",
    ]);

    apply(line: string, context: FormattingContext): string {
        const originalTrimmedLine = line.trim();

        if (originalTrimmedLine.length === 0) {
            return line; // Preserve empty lines
        }

        const indentChar = context.options.insertSpaces ? " " : "\\t";
        const indentUnitSize = context.options.insertSpaces ? context.options.tabSize : 1;

        let firstWord = "";
        // Try to extract a keyword if the line starts with ":":
        if (originalTrimmedLine.startsWith(":")) {
            let endOfKeyword = originalTrimmedLine.length;
            const spaceIdx = originalTrimmedLine.indexOf(" ");
            const semicolonIdx = originalTrimmedLine.indexOf(";");

            // Find the earliest of space, semicolon, or end of string
            if (spaceIdx !== -1) {
                endOfKeyword = spaceIdx;
            }
            if (semicolonIdx !== -1 && semicolonIdx < endOfKeyword) {
                endOfKeyword = semicolonIdx;
            }
            firstWord = originalTrimmedLine.substring(0, endOfKeyword).toUpperCase();
        }

        let indentDepth = context.blockDepth; // Default for content lines

        if (this.keywordsIndentCurrentDepth.has(firstWord)) {
            indentDepth = context.blockDepth;
        } else if (this.keywordsIndentPreviousDepth.has(firstWord)) {
            indentDepth = Math.max(0, context.blockDepth - 1);
        }
        // If firstWord was not a recognized keyword, or line didn't start with ":",
        // indentDepth remains context.blockDepth, which is correct for content lines.

        // The alignXXX methods handle internal alignment of constructs.
        // They return the processed content, which then needs to be prefixed with block indentation.
        let lineContent = originalTrimmedLine;
        const tokens = context.lineTokens; // Used by lineSuggestsXXX methods

        if (context.constructType === "array" || this.lineSuggestsArray(tokens)) {
            lineContent = this.alignArrayLiteral(originalTrimmedLine, context, tokens);
        } else if (
            context.constructType === "functionCall" ||
            this.lineSuggestsFunctionCall(tokens)
        ) {
            lineContent = this.alignFunctionCall(originalTrimmedLine, context, tokens);
        } else if (
            context.constructType === "sqlStatement" ||
            this.lineSuggestsSqlStatement(tokens)
        ) {
            lineContent = this.alignSqlStatement(originalTrimmedLine, context, tokens);
        }
        // If none of the above specific constructs, lineContent remains originalTrimmedLine.

        const indentationString = indentChar.repeat(indentDepth * indentUnitSize);
        return indentationString + lineContent;
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
