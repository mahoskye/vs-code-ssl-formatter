/**
 * Procedure Formatters
 *
 * - Format :PROCEDURE/:ENDPROC blocks
 * - Handle parameter lists and default parameters
 * - Manage procedure body indentation
 */

import { Token } from "../tokenizer/token";
import { TokenType } from "../tokenizer/tokenType";
import {
    ProcedureStatementNode,
    ParameterDeclarationNode,
    DefaultParameterDeclarationNode,
    ParameterListNode,
    DefaultParameterListNode,
} from "../parser/ast/procedures";
import { StatementNode } from "../parser/ast/base";
import { IdentifierListNode } from "../parser/ast/lists";
import { OutputBuilder } from "./visitor";
import { FormatterOptions } from "./options";

/**
 * Formats procedure statements according to SSL formatting rules
 */
export class ProcedureFormatter {
    private readonly output: OutputBuilder;
    private readonly options: FormatterOptions;

    constructor(output: OutputBuilder, options: FormatterOptions) {
        this.output = output;
        this.options = options;
    }
    /**
     * Format a complete procedure statement
     * Example:
     * :PROCEDURE ProcName;
     * :PARAMETERS param1, param2;
     * :DEFAULT param1, "defaultValue";
     *     // procedure body
     * :ENDPROC;
     */
    formatProcedureStatement(node: ProcedureStatementNode): void {
        // Format procedure header
        this.formatProcedureHeader(node);

        // Format parameters if present
        if (node.parameters) {
            this.formatParameterDeclaration(node.parameters);
        }

        // Format default parameters if present
        if (node.defaultParameters) {
            this.formatDefaultParameterDeclaration(node.defaultParameters);
        }

        // Add blank line after declarations if there are body statements
        if (node.body && node.body.length > 0) {
            this.output.writeBlankLine();
        }

        // Format procedure body
        this.formatProcedureBody(node.body);

        // Format procedure end
        this.formatProcedureEnd();
    }

    /**
     * Format procedure header
     * Example: :PROCEDURE ProcName;
     */
    private formatProcedureHeader(node: ProcedureStatementNode): void {
        this.output.writeIndented(":PROCEDURE ");
        this.output.write(node.name.value);
        this.output.write(";");
        this.output.writeLine();
    }

    /**
     * Format parameter declaration
     * Example: :PARAMETERS param1, param2, param3;
     */
    formatParameterDeclaration(node: ParameterDeclarationNode): void {
        this.output.writeIndented(":PARAMETERS ");
        this.formatParameterList(node.parameters);
        this.output.write(";");
        this.output.writeLine();
    }

    /**
     * Format default parameter declaration
     * Example: :DEFAULT param1, "defaultValue";
     */
    formatDefaultParameterDeclaration(node: DefaultParameterDeclarationNode): void {
        this.output.writeIndented(":DEFAULT ");
        this.formatDefaultParameterList(node.parameters);
        this.output.write(";");
        this.output.writeLine();
    }

    /**
     * Format parameter list
     * Example: param1, param2, param3
     */
    private formatParameterList(node: ParameterListNode): void {
        if (!node.identifiers || node.identifiers.length === 0) {
            return;
        }

        for (let i = 0; i < node.identifiers.length; i++) {
            if (i > 0) {
                this.output.write(", ");
            }
            this.output.write(node.identifiers[i].value);
        }
    }

    /**
     * Format default parameter list
     * Example: param1, "defaultValue"
     */
    private formatDefaultParameterList(node: DefaultParameterListNode): void {
        if (!node.pairs || node.pairs.length === 0) {
            return;
        }

        for (let i = 0; i < node.pairs.length; i++) {
            if (i > 0) {
                this.output.write(", ");
            }
            const pair = node.pairs[i];
            this.output.write(pair.identifier.value);
            this.output.write(", ");

            // Format the default value (simplified - in real implementation would format expression)
            // For now, just output a placeholder default value
            this.output.write('"defaultValue"');
        }
    }

    /**
     * Format procedure body with proper indentation
     */
    private formatProcedureBody(body: StatementNode[]): void {
        if (!body || body.length === 0) {
            return;
        }

        // Increase indentation for procedure body
        this.output.indent();

        // Format each statement in the body
        for (const statement of body) {
            // Note: In a real implementation, we would dispatch to appropriate
            // formatters based on statement type. For now, this is a placeholder.
            this.formatStatement(statement);
        }

        // Restore indentation
        this.output.dedent();
    }

    /**
     * Format a statement within procedure body
     * This is a placeholder - in real implementation would dispatch to appropriate formatters
     */
    private formatStatement(statement: StatementNode): void {
        // Placeholder implementation - would dispatch to appropriate formatter based on statement type
        this.output.writeIndented("/* statement */");
        this.output.writeLine();
    }

    /**
     * Format procedure end
     * Example: :ENDPROC;
     */
    private formatProcedureEnd(): void {
        this.output.writeIndented(":ENDPROC;");
        this.output.writeLine();
    }

    /**
     * Format identifier list with proper spacing
     * Helper method for parameter lists
     */
    private formatIdentifierList(identifiers: Token[]): void {
        for (let i = 0; i < identifiers.length; i++) {
            if (i > 0) {
                this.output.write(", ");
            }
            this.output.write(identifiers[i].value);
        }
    }

    /**
     * Check if a line would exceed maximum length
     */
    private wouldExceedMaxLength(additionalText: string): boolean {
        return this.output.wouldExceedMaxLength(additionalText);
    }

    /**
     * Add appropriate spacing based on formatter options
     */
    private addSpacing(): void {
        this.output.write(" ");
    }
}
