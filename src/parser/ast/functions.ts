import { Token } from "../../tokenizer/token";
import { ASTNode, ASTNodeType, ExpressionNode } from "./base";
// import { StringLiteralNode, ArrayLiteralNode } from './literals';
// import { ExpressionListNode } from './lists';

// FunctionCallNode could be a union of DirectFunctionCallNode, DoProcCallNode, ExecFunctionCallNode
export interface FunctionCallNode extends ASTNode {
    kind: ASTNodeType.FunctionCall;
}

export interface DirectFunctionCallNode extends ASTNode {
    kind: ASTNodeType.DirectFunctionCall;
    // functionName: Token; // Identifier
    // arguments?: ArgumentListNode;
}

export interface DoProcCallNode extends ASTNode {
    kind: ASTNodeType.DoProcCall;
    // procName: StringLiteralNode;
    // parameters: ArrayLiteralNode;
}

export interface ExecFunctionCallNode extends ASTNode {
    kind: ASTNodeType.ExecFunctionCall;
    // functionName: StringLiteralNode;
    // parameters: ArrayLiteralNode;
}

export interface ArgumentListNode extends ASTNode {
    kind: ASTNodeType.ArgumentList;
    arguments: ExpressionNode[]; // Array of expression arguments
}

export interface BitwiseOperationNode extends ASTNode {
    kind: ASTNodeType.BitwiseOperation;
    // functionName: Token; // _AND, _OR, _NOT
    // arguments: ExpressionNode[]; // Typically 2 for AND/OR, 1 for NOT
}
