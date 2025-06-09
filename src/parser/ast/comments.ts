import { Token } from "../../tokenizer/token";
import { ASTNode, ASTNodeType } from "./base";

export interface CommentStatementNode extends ASTNode {
    kind: ASTNodeType.CommentStatement;
} // Wrapper/Union

export interface BlockCommentNode extends ASTNode {
    kind: ASTNodeType.BlockComment;
    // content: string; // The text of the comment
}

export interface SingleLineCommentNode extends ASTNode {
    kind: ASTNodeType.SingleLineComment;
    // content: string;
}

export interface RegionCommentNode extends ASTNode {
    kind: ASTNodeType.RegionComment;
    // regionName?: string; // Text after "region"
}

export interface EndRegionCommentNode extends ASTNode {
    kind: ASTNodeType.EndRegionComment;
    // regionName?: string; // Text after "endregion"
}
