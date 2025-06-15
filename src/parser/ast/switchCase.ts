import { Token } from "../../tokenizer/token";
import { ASTNode, ASTNodeType, StatementNode, ExpressionNode } from "./base";

export interface SwitchStatementNode extends ASTNode {
    kind: ASTNodeType.SwitchStatement;
    expression?: ExpressionNode; // Optional for SSL BEGINCASE which doesn't have a switch expression
    cases: CaseBlockNode[];
    otherwiseBlock?: OtherwiseBlockNode;
}

export interface BeginCaseStatementNode extends ASTNode {
    kind: ASTNodeType.BeginCaseStatement;
}

export interface CaseBlockNode extends ASTNode {
    kind: ASTNodeType.CaseBlock;
    condition: ExpressionNode;
    statements: StatementNode[];
}

export interface CaseStatementNode extends ASTNode {
    kind: ASTNodeType.CaseStatement;
    expression: ExpressionNode;
}

export interface OtherwiseBlockNode extends ASTNode {
    kind: ASTNodeType.OtherwiseBlock;
    statements: StatementNode[];
}

export interface OtherwiseStatementNode extends ASTNode {
    kind: ASTNodeType.OtherwiseStatement;
}

export interface EndCaseStatementNode extends ASTNode {
    kind: ASTNodeType.EndCaseStatement;
}

export interface ExitCaseStatementNode extends ASTNode {
    kind: ASTNodeType.ExitCaseStatement;
}
