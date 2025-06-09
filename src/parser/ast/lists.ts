import { Token } from "../../tokenizer/token";
import { ASTNode, ASTNodeType } from "./base";
// import { ExpressionNode } from './expressions'; // For ExpressionList

export interface IdentifierListNode extends ASTNode {
    kind: ASTNodeType.IdentifierList;
    identifiers: Token[]; // Array of Identifier tokens
}

export interface ExpressionListNode extends ASTNode {
    kind: ASTNodeType.ExpressionList;
    // expressions: ExpressionNode[];
}
