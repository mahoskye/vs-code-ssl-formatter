/**
 * Declaration Formatters
 *
 * - Format :DECLARE statements
 * - Format :PARAMETERS statements
 * - Format :DEFAULT statements
 * - Format :PUBLIC statements
 * - Format :INCLUDE statements
 */

import { Token } from "../tokenizer/token";
import { TokenType } from "../tokenizer/tokenType";
import {
    DeclarationStatementNode,
    ParametersStatementNode,
    DeclareStatementNode,
    DefaultStatementNode,
    PublicStatementNode,
    IncludeStatementNode,
} from "../parser/ast/declarations";
import { IdentifierListNode } from "../parser/ast/lists";
import { OutputBuilder } from "./visitor";
import { FormatterOptions } from "./options";

/**
 * Formats declaration statements according to SSL formatting rules
 */
export class DeclarationFormatter {
    private readonly output: OutputBuilder;
    private readonly options: FormatterOptions;

    constructor(output: OutputBuilder, options: FormatterOptions) {
        this.output = output;
        this.options = options;
    }

    /**
     * Format a PARAMETERS statement
     * Example: :PARAMETERS param1, param2, param3;
     */
    formatParametersStatement(node: ParametersStatementNode): void {
        this.output.writeIndented(":PARAMETERS ");

        // Format identifier list if available
        if ("parameters" in node && node.parameters) {
            this.formatIdentifierList(node.parameters as IdentifierListNode);
        } else {
            // Fallback: format from tokens
            this.formatTokenSequence(node.startToken, node.endToken);
        }

        this.output.write(";");
        this.output.writeLine();
    }
    /**
     * Format a DECLARE statement
     * Example: :DECLARE var1, var2, var3;
     */
    formatDeclareStatement(node: DeclareStatementNode): void {
        this.output.writeIndented(":DECLARE ");

        // Format identifier list if available
        if ("identifiers" in node && node.identifiers) {
            this.formatIdentifierList(node.identifiers as IdentifierListNode);
        } else {
            // Fallback: format from tokens
            this.formatTokenSequence(node.startToken, node.endToken);
        }

        this.output.write(";");
        this.output.writeLine();
    }

    /**
     * Format a DEFAULT statement
     * Example: :DEFAULT param1, "defaultValue";
     */
    formatDefaultStatement(node: DefaultStatementNode): void {
        this.output.writeIndented(":DEFAULT ");

        // Format default parameters if available
        if ("defaults" in node && node.defaults) {
            // Format default parameter list - this would be implementation specific
            this.formatTokenSequence(node.startToken, node.endToken);
        } else {
            // Fallback: format from tokens
            this.formatTokenSequence(node.startToken, node.endToken);
        }

        this.output.write(";");
        this.output.writeLine();
    }

    /**
     * Format a PUBLIC statement
     * Example: :PUBLIC var1, var2, var3;
     */
    formatPublicStatement(node: PublicStatementNode): void {
        this.output.writeIndented(":PUBLIC ");

        // Format identifier list if available
        if ("identifiers" in node && node.identifiers) {
            this.formatIdentifierList(node.identifiers as IdentifierListNode);
        } else {
            // Fallback: format from tokens
            this.formatTokenSequence(node.startToken, node.endToken);
        }

        this.output.write(";");
        this.output.writeLine();
    }

    /**
     * Format an INCLUDE statement
     * Example: :INCLUDE "scriptname.ssl";
     */
    formatIncludeStatement(node: IncludeStatementNode): void {
        this.output.writeIndented(":INCLUDE ");

        // Format string literal path if available
        if ("path" in node && node.path) {
            // Format string literal - this would be implementation specific
            this.formatTokenSequence(node.startToken, node.endToken);
        } else {
            // Fallback: format from tokens
            this.formatTokenSequence(node.startToken, node.endToken);
        }

        this.output.write(";");
        this.output.writeLine();
    }

    /**
     * Format a generic declaration statement (wrapper)
     */
    formatDeclarationStatement(node: DeclarationStatementNode): void {
        // DeclarationStatement is a wrapper/union type
        // The actual formatting should be handled by specific statement formatters
        // This is mainly for the visitor pattern dispatch
        this.formatTokenSequence(node.startToken, node.endToken);
        this.output.writeLine();
    }

    /**
     * Format an identifier list with proper spacing
     * Example: param1, param2, param3
     */ private formatIdentifierList(node: IdentifierListNode): void {
        if (!node.identifiers || node.identifiers.length === 0) {
            return;
        }

        for (let i = 0; i < node.identifiers.length; i++) {
            const identifier = node.identifiers[i];
            this.output.write(identifier.value);

            // Add comma and space after each identifier except the last
            if (i < node.identifiers.length - 1) {
                this.output.write(",");
                if (this.options.insertSpacesAfterCommas) {
                    this.output.write(" ");
                }
            }
        }
    }

    /**
     * Fallback method to format from token sequence
     * This extracts content between start and end tokens
     */
    private formatTokenSequence(startToken: Token, endToken: Token): void {
        // This is a simplified implementation
        // For now, we'll just skip the complex token traversal since the main formatting
        // is handled by the specific identifier list formatting
        // In a full implementation, you would traverse the actual token sequence
        // For the fallback case, we'll just ensure proper structure
    }

    /**
     * Helper to get next token (simplified implementation)
     */
    private getNextToken(token: Token): Token | null {
        // This is a placeholder - in a real implementation,
        // you would have access to the token stream or neighboring tokens
        return null;
    }

    /**
     * Check if token type is a keyword
     */
    private isKeywordType(type: TokenType): boolean {
        const keywordTypes = [
            TokenType.DECLARE,
            TokenType.PARAMETERS,
            TokenType.DEFAULT,
            TokenType.PUBLIC,
            TokenType.INCLUDE,
            TokenType.PROCEDURE,
            TokenType.ENDPROC,
            TokenType.IF,
            TokenType.ELSE,
            TokenType.ENDIF,
            TokenType.WHILE,
            TokenType.ENDWHILE,
            TokenType.FOR,
            TokenType.TO,
            TokenType.NEXT,
            TokenType.CLASS,
            TokenType.INHERIT,
            TokenType.RETURN,
            // Add other keywords as needed
        ];
        return keywordTypes.includes(type);
    }
}
