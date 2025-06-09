import { Token } from "../../tokenizer/token";
import { ASTNode, ASTNodeType } from "./base";

export interface ClassDefinitionNode extends ASTNode {
    kind: ASTNodeType.ClassDefinition;
    declaration: ClassDeclarationNode;
    inherit?: InheritStatementNode;
    members: (ClassFieldDeclarationNode | MethodDeclarationNode)[];
}

export interface ClassDeclarationNode extends ASTNode {
    kind: ASTNodeType.ClassDeclaration;
    name: Token; // Assuming Identifier maps to Token for name
}

export interface InheritStatementNode extends ASTNode {
    kind: ASTNodeType.InheritStatement;
    className: Token; // Assuming Identifier maps to Token
}

export interface ClassFieldDeclarationNode extends ASTNode {
    kind: ASTNodeType.ClassFieldDeclaration;
    // Assuming it contains an IdentifierList, specific structure depends on full definition
}

export interface MethodDeclarationNode extends ASTNode {
    kind: ASTNodeType.MethodDeclaration;
    // Specific structure depends on full definition, likely wraps a ProcedureStatementNode
}
