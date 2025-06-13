import { Token } from "../../tokenizer/token";
import { ASTNode, ASTNodeType, StatementNode } from "./base";

export interface ErrorHandlingStatementNode extends ASTNode {
    kind: ASTNodeType.ErrorHandlingStatement;
} // Wrapper/Union

export interface TryBlockNode extends ASTNode {
    kind: ASTNodeType.TryBlock;
    tryStatements: StatementNode[];
    catchBlock?: CatchBlockNode;
    finallyBlock?: FinallyBlockNode;
}

export interface TryStatementNode extends ASTNode {
    kind: ASTNodeType.TryStatement;
}

export interface CatchBlockNode extends ASTNode {
    kind: ASTNodeType.CatchBlock;
    statements: StatementNode[];
}

export interface CatchStatementNode extends ASTNode {
    kind: ASTNodeType.CatchStatement;
}

export interface FinallyBlockNode extends ASTNode {
    kind: ASTNodeType.FinallyBlock;
    statements: StatementNode[];
}

export interface FinallyStatementNode extends ASTNode {
    kind: ASTNodeType.FinallyStatement;
}

export interface EndTryStatementNode extends ASTNode {
    kind: ASTNodeType.EndTryStatement;
}

export interface ErrorBlockStanzaNode extends ASTNode {
    kind: ASTNodeType.ErrorBlockStanza;
    statements: StatementNode[];
}

export interface ErrorMarkerNode extends ASTNode {
    kind: ASTNodeType.ErrorMarker;
}
