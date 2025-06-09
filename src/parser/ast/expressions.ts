import { Token } from "../../tokenizer/token";
import { ASTNode, ASTNodeType, ExpressionNode as BaseExpressionNode } from "./base"; // Renamed to avoid conflict
// import { LiteralNode } from './literals'; // For Primary expression

export interface BinaryExpressionNode extends BaseExpressionNode {
    // kind: ASTNodeType.BinaryExpression; // This will be set by implementing interfaces
    left: BaseExpressionNode;
    operator: Token; // e.g., +, -, .AND., ==
    right: BaseExpressionNode;
}

export interface LogicalExpressionNode extends BinaryExpressionNode {
    kind: ASTNodeType.LogicalExpression /* operator: .AND. | .OR. */;
}
export interface ComparisonExpressionNode extends BinaryExpressionNode {
    kind: ASTNodeType.ComparisonExpression /* operator: ==, !=, <, >, <=, >=, = */;
}
export interface ArithmeticExpressionNode extends BinaryExpressionNode {
    kind: ASTNodeType.ArithmeticExpression /* operator: +, - */;
} // Additive
// Term, Factor, PowerOperand are part of the hierarchy leading to Primary
export interface TermNode extends BaseExpressionNode {
    kind: ASTNodeType.Term /* Typically a multiplicative expression */;
} // Factor {MultiplicativeOperator Factor}
export interface FactorNode extends BaseExpressionNode {
    kind: ASTNodeType.Factor /* Typically a power expression */;
} // PowerOperand {"^" PowerOperand}
export interface PowerOperandNode extends BaseExpressionNode {
    kind: ASTNodeType.PowerOperand /* [UnaryOperator] Primary */;
}

export interface UnaryExpressionNode extends BaseExpressionNode {
    kind: ASTNodeType.UnaryExpression;
    // operator: Token; // e.g., +, -, !, .NOT.
    // operand: BaseExpressionNode;
}

export interface IncrementExpressionNode extends BaseExpressionNode {
    kind: ASTNodeType.IncrementExpression;
    // operand: Token; // Identifier
    // isPrefix: boolean;
    // operator: Token; // ++ or --
}

export interface VariableAccessNode extends BaseExpressionNode {
    kind: ASTNodeType.VariableAccess;
    // variableName: Token; // Identifier
}

export interface PropertyAccessNode extends BaseExpressionNode {
    // SSL uses colon for property access
    kind: ASTNodeType.PropertyAccess;
    // object: Token; // Identifier
    // property: Token; // Identifier
}

export interface ArrayAccessNode extends BaseExpressionNode {
    kind: ASTNodeType.ArrayAccess;
    // arrayName: Token; // Identifier
    // subscript: ArraySubscriptNode;
}

export interface ArraySubscriptNode extends ASTNode {
    // Not an ExpressionNode directly, but part of ArrayAccess
    kind: ASTNodeType.ArraySubscript;
    dimensions: BaseExpressionNode[]; // For arr[1,2] or arr[1][2]
}

export interface PrimaryNode extends BaseExpressionNode {
    kind: ASTNodeType.Primary;
    // value: LiteralNode | MethodCallNode | VariableAccessNode | ArrayAccessNode | PropertyAccessNode | GroupedExpression (ExpressionNode)
}
