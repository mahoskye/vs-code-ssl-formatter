import { Token } from "../../tokenizer/token";
import { ASTNode, ASTNodeType, ExpressionNode, StatementNode } from "./base";

export interface ConditionalStatementNode extends ASTNode {
    kind: ASTNodeType.ConditionalStatement;
} // Wrapper/Union type

export interface IfStatementNode extends ASTNode {
    kind: ASTNodeType.IfStatement;
    condition: ExpressionNode;
    thenBranch: StatementNode[];
    elseBranch?: ElseStatementNode;
    endIf: EndIfStatementNode;
}
export interface ElseStatementNode extends ASTNode {
    kind: ASTNodeType.ElseStatement;
    body: StatementNode[];
}
export interface EndIfStatementNode extends ASTNode {
    kind: ASTNodeType.EndIfStatement;
}

export interface LoopStatementNode extends ASTNode {
    kind: ASTNodeType.LoopStatement;
} // Wrapper/Union type

export interface WhileLoopNode extends ASTNode {
    kind: ASTNodeType.WhileLoop;
    condition: WhileStatementNode;
    body: StatementNode[];
    end: EndWhileStatementNode;
}
export interface WhileStatementNode extends ASTNode {
    kind: ASTNodeType.WhileStatement;
    condition: ExpressionNode;
}
export interface EndWhileStatementNode extends ASTNode {
    kind: ASTNodeType.EndWhileStatement;
}

export interface ForLoopNode extends ASTNode {
    kind: ASTNodeType.ForLoop;
    declaration: ForStatementNode;
    body: StatementNode[];
    next: NextStatementNode;
}
export interface ForStatementNode extends ASTNode {
    kind: ASTNodeType.ForStatement;
    variable: Token;
    startValue: ExpressionNode;
    endValue: ExpressionNode;
}
export interface NextStatementNode extends ASTNode {
    kind: ASTNodeType.NextStatement;
}

export interface ExitWhileStatementNode extends ASTNode {
    kind: ASTNodeType.ExitWhileStatement;
}
export interface ExitForStatementNode extends ASTNode {
    kind: ASTNodeType.ExitForStatement;
}
export interface LoopContinueNode extends ASTNode {
    kind: ASTNodeType.LoopContinue;
}
