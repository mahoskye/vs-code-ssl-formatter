import { Token } from "../../tokenizer/token";
import { ASTNode, ASTNodeType } from "./base";
// import { ExpressionNode, VariableAccessNode, PropertyAccessNode } from './expressions';

export interface LogicStatementNode extends ASTNode {
    kind: ASTNodeType.LogicStatement;
} // Wrapper/Union

export interface AssignmentNode extends ASTNode {
    kind: ASTNodeType.Assignment;
    // target: VariableAccessNode | PropertyAccessNode;
    // operator: Token; // Representing :=, +=, etc.
    // value: ExpressionNode;
}

export interface ReturnStatementNode extends ASTNode {
    kind: ASTNodeType.ReturnStatement;
    // expression?: ExpressionNode;
}
