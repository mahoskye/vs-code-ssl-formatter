import { Token } from "../../tokenizer/token";
import { ASTNode, ASTNodeType } from "./base";
import { IdentifierListNode } from "./lists";
// import { StringLiteralNode } from './literals';
// import { DefaultParameterListNode } from './procedures';

export interface DeclarationStatementNode extends ASTNode {
    kind: ASTNodeType.DeclarationStatement;
} // Wrapper/Union

export interface ParametersStatementNode extends ASTNode {
    kind: ASTNodeType.ParametersStatement;
    parameters?: IdentifierListNode;
}
export interface DeclareStatementNode extends ASTNode {
    kind: ASTNodeType.DeclareStatement;
    identifiers?: IdentifierListNode;
}
export interface DefaultStatementNode extends ASTNode {
    kind: ASTNodeType.DefaultStatement;
    defaults?: any; // TODO: Define DefaultParameterListNode
}
export interface PublicStatementNode extends ASTNode {
    kind: ASTNodeType.PublicStatement;
    identifiers?: IdentifierListNode;
}
export interface IncludeStatementNode extends ASTNode {
    kind: ASTNodeType.IncludeStatement;
    path?: any; // TODO: Define StringLiteralNode
}
