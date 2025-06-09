import { Token } from "../../tokenizer/token";
import { ASTNode, ASTNodeType } from "./base";
// import { ExpressionNode } from './expressions';
// import { StatementNode } from './base';

export interface SwitchStatementNode extends ASTNode {
    kind: ASTNodeType.SwitchStatement /* start: BeginCaseStatementNode; cases: CaseBlockNode[]; otherwise?: OtherwiseBlockNode; end: EndCaseStatementNode; */;
}
export interface BeginCaseStatementNode extends ASTNode {
    kind: ASTNodeType.BeginCaseStatement;
}
export interface CaseBlockNode extends ASTNode {
    kind: ASTNodeType.CaseBlock /* condition: CaseStatementNode; body: StatementNode[]; exit?: ExitCaseStatementNode; */;
}
export interface CaseStatementNode extends ASTNode {
    kind: ASTNodeType.CaseStatement /* expression: ExpressionNode; */;
}
export interface OtherwiseBlockNode extends ASTNode {
    kind: ASTNodeType.OtherwiseBlock /* start: OtherwiseStatementNode; body: StatementNode[]; */;
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
