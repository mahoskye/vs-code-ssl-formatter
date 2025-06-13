/**
 * SSL Declaration Formatter Visitor
 *
 * Concrete implementation of FormatterVisitorBase for declaration statements
 * Integrates with DeclarationFormatter to provide proper formatting
 */

import { FormatterVisitorBase, VisitorResult } from "./visitor";
import { DeclarationFormatter } from "./declarations";
import { FormatterOptions } from "./options";
import {
    DeclarationStatementNode,
    ParametersStatementNode,
    DeclareStatementNode,
    DefaultStatementNode,
    PublicStatementNode,
    IncludeStatementNode,
} from "../parser/ast/declarations";

/**
 * SSL Formatter with declaration support
 */
export class SSLDeclarationFormatterVisitor extends FormatterVisitorBase {
    constructor(options?: FormatterOptions) {
        super(options);
    } // Declaration statements
    protected override visitDeclarationStatement(node: DeclarationStatementNode): VisitorResult {
        const formatter = new DeclarationFormatter(this.output, this.options);
        formatter.formatDeclarationStatement(node);
        return { shouldContinue: true };
    }

    protected override visitParametersStatement(node: ParametersStatementNode): VisitorResult {
        const formatter = new DeclarationFormatter(this.output, this.options);
        formatter.formatParametersStatement(node);
        return { shouldContinue: true };
    }

    protected override visitDeclareStatement(node: DeclareStatementNode): VisitorResult {
        const formatter = new DeclarationFormatter(this.output, this.options);
        formatter.formatDeclareStatement(node);
        return { shouldContinue: true };
    }

    protected override visitDefaultStatement(node: DefaultStatementNode): VisitorResult {
        const formatter = new DeclarationFormatter(this.output, this.options);
        formatter.formatDefaultStatement(node);
        return { shouldContinue: true };
    }

    protected override visitPublicStatement(node: PublicStatementNode): VisitorResult {
        const formatter = new DeclarationFormatter(this.output, this.options);
        formatter.formatPublicStatement(node);
        return { shouldContinue: true };
    }

    protected override visitIncludeStatement(node: IncludeStatementNode): VisitorResult {
        const formatter = new DeclarationFormatter(this.output, this.options);
        formatter.formatIncludeStatement(node);
        return { shouldContinue: true };
    }
}
