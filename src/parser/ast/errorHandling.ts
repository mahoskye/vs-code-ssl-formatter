import { Token } from "../../tokenizer/token";
import { ASTNode, ASTNodeType } from "./base";
// import { StatementNode } from './base';

export interface ErrorHandlingStatementNode extends ASTNode {
    kind: ASTNodeType.ErrorHandlingStatement;
} // Wrapper/Union

export interface TryBlockNode extends ASTNode {
    kind: ASTNodeType.TryBlock /* tryStmt: TryStatementNode; tryBody: StatementNode[]; catchBlock: CatchBlockNode; finallyBlock?: FinallyBlockNode; endTry: EndTryStatementNode; */;
}
export interface TryStatementNode extends ASTNode {
    kind: ASTNodeType.TryStatement;
}
export interface CatchBlockNode extends ASTNode {
    kind: ASTNodeType.CatchBlock /* catchStmt: CatchStatementNode; body: StatementNode[]; */;
}
export interface CatchStatementNode extends ASTNode {
    kind: ASTNodeType.CatchStatement;
}
export interface FinallyBlockNode extends ASTNode {
    kind: ASTNodeType.FinallyBlock /* finallyStmt: FinallyStatementNode; body: StatementNode[]; */;
}
export interface FinallyStatementNode extends ASTNode {
    kind: ASTNodeType.FinallyStatement;
}
export interface EndTryStatementNode extends ASTNode {
    kind: ASTNodeType.EndTryStatement;
}

export interface ErrorBlockStanzaNode extends ASTNode {
    kind: ASTNodeType.ErrorBlockStanza /* marker: ErrorMarkerNode; body: StatementNode[]; */;
}
export interface ErrorMarkerNode extends ASTNode {
    kind: ASTNodeType.ErrorMarker;
} // Assuming ErrorMarker is a node if it has start/end tokens
