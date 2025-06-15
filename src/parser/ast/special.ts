import { Token } from "../../tokenizer/token";
import { ASTNode, ASTNodeType, StatementNode } from "./base";
// import { StringLiteralNode, ArrayLiteralNode } from './literals';
// import { StatementNode } from './base'; // For InlineCodeBlock body

export interface LabelStatementNode extends ASTNode {
    kind: ASTNodeType.LabelStatement;
    // labelName: Token; // Identifier
}

export interface RegionBlockNode extends ASTNode {
    // Keyword-based :REGION / :ENDREGION
    kind: ASTNodeType.RegionBlock;
    name: Token;
    statements: StatementNode[];
}

export interface RegionStartNode extends ASTNode {
    kind: ASTNodeType.RegionStart;
    // regionName: Token; // Identifier
}

export interface RegionEndNode extends ASTNode {
    kind: ASTNodeType.RegionEnd;
}

export interface InlineCodeBlockNode extends ASTNode {
    kind: ASTNodeType.InlineCodeBlock;
    language?: Token | any; // Language specifier (identifier or string)
    statements: StatementNode[];
}

export interface InlineCodeStartNode extends ASTNode {
    kind: ASTNodeType.InlineCodeStart;
    // languageSpecifier?: StringLiteralNode | Token; // Identifier
}

export interface InlineCodeEndNode extends ASTNode {
    kind: ASTNodeType.InlineCodeEnd;
}

export interface DynamicCodeExecutionNode extends ASTNode {
    // For ExecUDF
    kind: ASTNodeType.DynamicCodeExecution;
    // functionName: StringLiteralNode;
    // parameters?: ArrayLiteralNode;
}

export interface BranchStatementNode extends ASTNode {
    kind: ASTNodeType.BranchStatement;
    // targetLabel: StringLiteralNode; // e.g., "LABEL labelname"
}
