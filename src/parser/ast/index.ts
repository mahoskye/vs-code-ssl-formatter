export * from "./base";
export * from "./program";
export * from "./classes";
export * from "./procedures";
export * from "./controlFlow";
export * from "./switchCase";
export * from "./errorHandling";
export * from "./declarations";
export * from "./logic";
export * from "./functions";
export * from "./comments";
export * from "./special";
export * from "./sql";
export * from "./objects";
export {
    BinaryExpressionNode,
    LogicalExpressionNode,
    ComparisonExpressionNode,
    ArithmeticExpressionNode,
    TermNode,
    FactorNode,
    PowerOperandNode,
    UnaryExpressionNode,
    IncrementExpressionNode,
    VariableAccessNode,
    PropertyAccessNode,
    ArrayAccessNode,
    ArraySubscriptNode,
    PrimaryNode,
} from "./expressions";
export {
    SpecificLiteralNode as LiteralNode,
    LiteralExpressionNode,
    NumberLiteralNode,
    StringLiteralNode,
    BooleanLiteralNode,
    ArrayLiteralNode,
    NilLiteralNode,
    DateLiteralNode,
    CodeBlockLiteralNode,
} from "./literals";
export * from "./lists";

// Statement type refinement (example, would need all statement node imports)
// import { IfStatementNode } from './controlFlow';
// import { AssignmentNode } from './logic';
// ... other statement imports
// export type SpecificStatementNode = IfStatementNode | AssignmentNode /* | ... more specific statement types */;

// Expression type refinement (example)
// import { BinaryExpressionNode, UnaryExpressionNode } from './expressions';
// import { LiteralExpressionNode } from './literals';
// ... other expression imports
// export type SpecificExpressionNode = BinaryExpressionNode | UnaryExpressionNode | LiteralExpressionNode /* | ... more specific expression types */;
