/**
 * SSL Class Formatter Visitor
 *
 * Concrete implementation of FormatterVisitorBase for class statements
 * Integrates with ClassFormatter to provide proper formatting
 * Complies with SSL EBNF grammar specifications
 */

import { FormatterVisitorBase, VisitorResult } from "./visitor";
import { ClassFormatter } from "./classes";
import { FormatterOptions } from "./options";
import {
    ClassDefinitionNode,
    ClassDeclarationNode,
    InheritStatementNode,
    ClassFieldDeclarationNode,
    MethodDeclarationNode,
} from "../parser/ast/classes";

/**
 * SSL Formatter with class support following EBNF grammar
 * ClassDefinition ::= ClassDeclaration [InheritStatement] {ClassMember}
 * ClassMember ::= ClassFieldDeclaration | MethodDeclaration
 */
export class SSLClassFormatterVisitor extends FormatterVisitorBase {
    private classFormatter: ClassFormatter;

    constructor(options: FormatterOptions) {
        super(options);
        this.classFormatter = new ClassFormatter(this.output, options);
    }

    /**
     * Update the internal ClassFormatter when output builder changes
     */
    public updateOutputBuilder(newOutput: any): void {
        (this as any).output = newOutput;
        this.classFormatter = new ClassFormatter(newOutput, (this as any).options);
    }

    /**
     * Visit a class-related AST node and delegate to appropriate formatter
     */
    public visit(node: any): VisitorResult {
        switch (node.kind) {
            case "ClassDefinition":
                return this.visitClassDefinition(node as ClassDefinitionNode);
            case "ClassDeclaration":
                return this.visitClassDeclaration(node as ClassDeclarationNode);
            case "InheritStatement":
                return this.visitInheritStatement(node as InheritStatementNode);
            case "ClassFieldDeclaration":
                return this.visitClassFieldDeclaration(node as ClassFieldDeclarationNode);
            case "MethodDeclaration":
                return this.visitMethodDeclaration(node as MethodDeclarationNode);
            default:
                return {
                    shouldContinue: false,
                    error: `Unsupported class node type: ${node.kind}`,
                };
        }
    }

    /**
     * Format a complete class definition according to EBNF:
     * ClassDefinition ::= ClassDeclaration [InheritStatement] {ClassMember}
     */
    protected visitClassDefinition(node: ClassDefinitionNode): VisitorResult {
        try {
            this.classFormatter.formatClassDefinition(node);
            return { shouldContinue: true };
        } catch (error) {
            return {
                shouldContinue: false,
                error: `Error formatting class definition: ${
                    error instanceof Error ? error.message : String(error)
                }`,
            };
        }
    }

    /**
     * Format a class declaration according to EBNF:
     * ClassDeclaration ::= ":" "CLASS" Identifier
     */
    protected visitClassDeclaration(node: ClassDeclarationNode): VisitorResult {
        try {
            this.classFormatter.formatClassDeclaration(node);
            return { shouldContinue: true };
        } catch (error) {
            return {
                shouldContinue: false,
                error: `Error formatting class declaration: ${
                    error instanceof Error ? error.message : String(error)
                }`,
            };
        }
    }

    /**
     * Format an inheritance statement according to EBNF:
     * InheritStatement ::= ":" "INHERIT" Identifier
     */
    protected visitInheritStatement(node: InheritStatementNode): VisitorResult {
        try {
            this.classFormatter.formatInheritStatement(node);
            return { shouldContinue: true };
        } catch (error) {
            return {
                shouldContinue: false,
                error: `Error formatting inherit statement: ${
                    error instanceof Error ? error.message : String(error)
                }`,
            };
        }
    }

    /**
     * Format a class field declaration according to EBNF:
     * ClassFieldDeclaration ::= ":" "DECLARE" IdentifierList
     */
    protected visitClassFieldDeclaration(node: ClassFieldDeclarationNode): VisitorResult {
        try {
            this.classFormatter.formatClassFieldDeclaration(node);
            return { shouldContinue: true };
        } catch (error) {
            return {
                shouldContinue: false,
                error: `Error formatting class field declaration: ${
                    error instanceof Error ? error.message : String(error)
                }`,
            };
        }
    }

    /**
     * Format a method declaration according to EBNF:
     * MethodDeclaration ::= ProcedureStatement
     */
    protected visitMethodDeclaration(node: MethodDeclarationNode): VisitorResult {
        try {
            this.classFormatter.formatMethodDeclaration(node);
            return { shouldContinue: true };
        } catch (error) {
            return {
                shouldContinue: false,
                error: `Error formatting method declaration: ${
                    error instanceof Error ? error.message : String(error)
                }`,
            };
        }
    }
}
