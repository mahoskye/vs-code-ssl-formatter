// Main entry point for the parser module

// Core parser classes and interfaces
export { Parser, ParseResult, ParseError } from "./parser";

// AST node types and interfaces
export {
    ASTNode,
    ASTNodeType,
    StatementNode,
    ExpressionNode,
    LiteralNode,
    ProgramNode,
    ClassDefinitionNode,
    ProcedureStatementNode,
    IfStatementNode,
    WhileLoopNode,
    ForLoopNode,
    SwitchStatementNode,
    TryBlockNode,
    AssignmentNode,
    DirectFunctionCallNode,
    BinaryExpressionNode,
    UnaryExpressionNode,
    LiteralExpressionNode,
    VariableAccessNode,
    PropertyAccessNode,
    ArrayAccessNode,
    IdentifierListNode,
    ArgumentListNode,
    createBaseNode,
} from "./ast";

// Import for internal use
import { Parser, ParseResult } from "./parser";

// Convenience function for parsing
export function parse(tokens: any[]): ParseResult {
    const parser = new Parser(tokens);
    return parser.parse();
}
