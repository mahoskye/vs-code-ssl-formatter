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
    private readonly declarationFormatter: DeclarationFormatter;

    constructor(options?: FormatterOptions) {
        super(options);
        this.declarationFormatter = new DeclarationFormatter(this.output, this.options);
    }

    // Declaration statements
    protected override visitDeclarationStatement(node: DeclarationStatementNode): VisitorResult {
        this.declarationFormatter.formatDeclarationStatement(node);
        return { shouldContinue: true };
    }

    protected override visitParametersStatement(node: ParametersStatementNode): VisitorResult {
        this.declarationFormatter.formatParametersStatement(node);
        return { shouldContinue: true };
    }

    protected override visitDeclareStatement(node: DeclareStatementNode): VisitorResult {
        this.declarationFormatter.formatDeclareStatement(node);
        return { shouldContinue: true };
    }

    protected override visitDefaultStatement(node: DefaultStatementNode): VisitorResult {
        this.declarationFormatter.formatDefaultStatement(node);
        return { shouldContinue: true };
    }

    protected override visitPublicStatement(node: PublicStatementNode): VisitorResult {
        this.declarationFormatter.formatPublicStatement(node);
        return { shouldContinue: true };
    }

    protected override visitIncludeStatement(node: IncludeStatementNode): VisitorResult {
        this.declarationFormatter.formatIncludeStatement(node);
        return { shouldContinue: true };
    }
}
