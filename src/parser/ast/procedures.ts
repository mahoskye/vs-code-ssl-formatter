import { Token } from "../../tokenizer/token";
import { ASTNode, ASTNodeType, StatementNode } from "./base"; // Added StatementNode import
// import { ExpressionNode } from './expressions'; // For DefaultParameterList
// import { IdentifierListNode } from './lists'; // For ParameterList

export interface ProcedureStatementNode extends ASTNode {
    kind: ASTNodeType.ProcedureStatement;
    name: Token; // Added missing name property
    parameters?: ParameterDeclarationNode; // Uncommented
    defaultParameters?: DefaultParameterDeclarationNode; // Uncommented
    body: StatementNode[]; // Uncommented
    // end: ProcedureEndNode;
}

export interface ProcedureStartNode extends ASTNode {
    kind: ASTNodeType.ProcedureStart;
    // name: Token; // Identifier
}

export interface ProcedureEndNode extends ASTNode {
    kind: ASTNodeType.ProcedureEnd;
}

export interface ParameterDeclarationNode extends ASTNode {
    kind: ASTNodeType.ParameterDeclaration;
    parameters: ParameterListNode; // Uncommented and corrected type
}

export interface DefaultParameterDeclarationNode extends ASTNode {
    kind: ASTNodeType.DefaultParameterDeclaration;
    parameters: DefaultParameterListNode; // Uncommented and corrected type
}

export interface ParameterListNode extends ASTNode {
    kind: ASTNodeType.ParameterList;
    identifiers: Token[]; // Changed from IdentifierListNode to Token[] for simplicity, can be IdentifierNode[] if such a type exists
}

export interface DefaultParameterListNode extends ASTNode {
    kind: ASTNodeType.DefaultParameterList;
    pairs: { identifier: Token; defaultValue: /* ExpressionNode */ any }[]; // Renamed items to pairs and added type
}
