/**
 * Class Formatter
 *
 * - Format :CLASS declarations
 * - Format :INHERIT statements
 * - Format class members and methods
 */

import { Token } from "../tokenizer/token";
import { TokenType } from "../tokenizer/tokenType";
import {
    ClassDefinitionNode,
    ClassDeclarationNode,
    InheritStatementNode,
    ClassFieldDeclarationNode,
    MethodDeclarationNode,
} from "../parser/ast/classes";
import { IdentifierListNode } from "../parser/ast/lists";
import { ProcedureStatementNode } from "../parser/ast/procedures";
import { OutputBuilder } from "./outputBuilder";
import { FormatterOptions } from "./options";

/**
 * Formats class definitions according to SSL formatting rules
 */
export class ClassFormatter {
    private readonly output: OutputBuilder;
    private readonly options: FormatterOptions;

    constructor(output: OutputBuilder, options: FormatterOptions) {
        this.output = output;
        this.options = options;
    }

    /**
     * Format a complete class definition
     * Example:
     * :CLASS MyClass;
     * :INHERIT BaseClass;
     * :DECLARE field1, field2;
     * :PROCEDURE Method1;
     * // method body
     * :ENDPROC;
     */
    formatClassDefinition(node: ClassDefinitionNode): void {
        // Format the class declaration
        this.formatClassDeclaration(node.declaration);

        // Add blank line after class declaration for readability
        this.output.writeBlankLine();

        // Format inheritance statement if present
        if (node.inherit) {
            this.formatInheritStatement(node.inherit);
            this.output.writeBlankLine();
        }

        // Format class members (fields and methods)
        for (let i = 0; i < node.members.length; i++) {
            const member = node.members[i];

            if (member.kind === "ClassFieldDeclaration") {
                this.formatClassFieldDeclaration(member as ClassFieldDeclarationNode);
            } else if (member.kind === "MethodDeclaration") {
                this.formatMethodDeclaration(member as MethodDeclarationNode);
            }

            // Add blank line between members, but not after the last one
            if (i < node.members.length - 1) {
                this.output.writeBlankLine();
            }
        }
    }

    /**
     * Format a class declaration
     * Example: :CLASS MyClass;
     */
    formatClassDeclaration(node: ClassDeclarationNode): void {
        this.output.writeIndented(":CLASS ");

        if (node.name) {
            this.output.write(node.name.value);
        } else {
            // Fallback: format from tokens
            this.formatTokenSequence(node.startToken, node.endToken);
        }

        this.output.write(";");
        this.output.writeLine();
    }

    /**
     * Format an inheritance statement
     * Example: :INHERIT Research.clsVehicle;
     * Supports qualified class names (Category.ClassName)
     */
    formatInheritStatement(node: InheritStatementNode): void {
        this.output.writeIndented(":INHERIT ");

        if (node.className) {
            this.output.write(node.className.value);
        } else {
            // Fallback: format from tokens
            this.formatTokenSequence(node.startToken, node.endToken);
        }

        this.output.write(";");
        this.output.writeLine();
    }

    /**
     * Format a class field declaration
     * Example: :DECLARE MSRPPrice, Wheels, Color;
     */
    formatClassFieldDeclaration(node: ClassFieldDeclarationNode): void {
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
     * Format a method declaration (procedure within a class)
     * Example:
     * :PROCEDURE GetPrice;
     * :RETURN Me:MSRPPrice + 500;
     * :ENDPROC;
     */ formatMethodDeclaration(node: MethodDeclarationNode): void {
        // Methods are essentially procedures within a class context
        if ("procedure" in node && node.procedure) {
            this.formatProcedureStatement(node.procedure as ProcedureStatementNode);
        } else {
            // Fallback: format from tokens with proper indentation
            // For methods without procedures, output a minimal method structure
            this.output.writeIndented("/* Method structure not available */;");
            this.output.writeLine();
        }
    }

    /**
     * Format an identifier list with proper spacing
     * Example: var1, var2, var3
     */
    private formatIdentifierList(node: IdentifierListNode): void {
        if (!node.identifiers || node.identifiers.length === 0) {
            return;
        }

        for (let i = 0; i < node.identifiers.length; i++) {
            const identifier = node.identifiers[i];
            this.output.write(identifier.value);

            // Add comma and space between identifiers (not after the last one)
            if (i < node.identifiers.length - 1) {
                this.output.write(", ");
            }
        }
    }

    /**
     * Format a procedure statement (for method declarations)
     * This delegates to a procedure formatter if available
     */
    private formatProcedureStatement(node: ProcedureStatementNode): void {
        // Format procedure start
        this.output.writeIndented(":PROCEDURE ");

        if (node.name) {
            this.output.write(node.name.value);
        }
        this.output.write(";");
        this.output.writeLine();

        // Indent for procedure body
        this.output.indent(); // Format procedure body (statements)
        if (node.body && node.body.length > 0) {
            for (const statement of node.body) {
                // This would require a full statement formatter
                // For now, format as token sequence with proper indentation
                this.output.writeIndented("");
                this.formatTokenSequence(statement.startToken, statement.endToken);
                this.output.writeLine();
            }
        }

        // Dedent and format procedure end
        this.output.dedent();
        this.output.writeIndented(":ENDPROC;");
        this.output.writeLine();
    }
    /**
     * Format a sequence of tokens (fallback method)
     * This preserves the original token content while ensuring proper spacing
     */
    private formatTokenSequence(startToken: Token, endToken: Token): void {
        // Simple fallback implementation
        // In a full implementation, this would properly traverse tokens
        // and format them with appropriate spacing

        // For class declarations without names, we don't want to output
        // the structural tokens (: and ;) as they're handled by the caller
        if (startToken && endToken) {
            // Skip structural tokens like ":" and ";" for class declarations
            if (startToken.value === ":" || startToken.value === ";") {
                return;
            }
            if (endToken.value === ":" || endToken.value === ";") {
                return;
            }

            // Write meaningful content tokens
            if (startToken.value) {
                this.output.write(startToken.value);
            }

            // If there are multiple tokens between start and end,
            // they would be processed here with proper spacing
            if (endToken !== startToken && endToken.value && endToken.value !== startToken.value) {
                this.output.write(" ");
                this.output.write(endToken.value);
            }
        }
    }
}
