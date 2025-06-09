import { Token } from "../../tokenizer/token";
import { ASTNode, ASTNodeType, LiteralNode as BaseLiteralNode, ExpressionNode } from "./base";

// LiteralNode could be a union of all specific literal types
export interface SpecificLiteralNode extends BaseLiteralNode {
    kind: ASTNodeType.Literal;
}

export interface LiteralExpressionNode extends ExpressionNode {
    // Wrapper for literals used as expressions
    kind: ASTNodeType.LiteralExpression;
    value: any;
    token: Token;
}

export interface NumberLiteralNode extends BaseLiteralNode {
    kind: ASTNodeType.NumberLiteral;
    value: number;
    raw: string;
}

export interface StringLiteralNode extends BaseLiteralNode {
    kind: ASTNodeType.StringLiteral;
    value: string;
    token: Token;
}

export interface BooleanLiteralNode extends BaseLiteralNode {
    kind: ASTNodeType.BooleanLiteral;
    value: boolean;
}

export interface ArrayLiteralNode extends BaseLiteralNode {
    kind: ASTNodeType.ArrayLiteral;
    elements: ExpressionNode[]; // Array of expressions
}

export interface NilLiteralNode extends BaseLiteralNode {
    kind: ASTNodeType.NilLiteral;
}

export interface DateLiteralNode extends BaseLiteralNode {
    kind: ASTNodeType.DateLiteral;
    // year: NumberLiteralNode;
    // month: NumberLiteralNode;
    // day: NumberLiteralNode;
    // hour?: NumberLiteralNode;
    // minute?: NumberLiteralNode;
    // second?: NumberLiteralNode;
}

export interface CodeBlockLiteralNode extends BaseLiteralNode {
    kind: ASTNodeType.CodeBlockLiteral;
    // parameters?: IdentifierListNode;
    // body: ExpressionListNode; // Or single ExpressionNode
}
