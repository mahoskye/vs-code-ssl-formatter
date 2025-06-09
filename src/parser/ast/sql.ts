import { Token } from "../../tokenizer/token";
import { ASTNode, ASTNodeType, ExpressionNode } from "./base";
import { StringLiteralNode, ArrayLiteralNode } from "./literals";

export interface SqlStatementNode extends ASTNode {
    kind: ASTNodeType.SqlStatement;
} // Wrapper/Union

export interface SqlExecuteNode extends ASTNode {
    kind: ASTNodeType.SqlExecute;
    query: StringLiteralNode;
    parameters?: ArrayLiteralNode;
}

export interface LSearchNode extends ASTNode {
    kind: ASTNodeType.LSearch;
    query: StringLiteralNode;
    parameter1?: ExpressionNode;
    parameter2?: ExpressionNode;
    parameters?: ArrayLiteralNode;
}

export interface SqlParameterNode extends ASTNode {
    kind: ASTNodeType.SqlParameter;
    // parameterName?: Token; // Identifier if ?paramName?, otherwise undefined for ?
}
