import { Token } from "../../tokenizer/token";
import { ASTNode, ASTNodeType } from "./base";
// import { StringLiteralNode } from './literals';
// import { ArgumentListNode } from './functions'; // For method calls if they take arguments directly

export interface ObjectCreationNode extends ASTNode {
    kind: ASTNodeType.ObjectCreation;
    // className?: StringLiteralNode; // Optional for Expando objects
}

export interface MethodCallNode extends ASTNode {
    // Object:Method or Object:Method()
    kind: ASTNodeType.MethodCall;
    // object: Token; // Identifier for the object
    // methodName: Token; // Identifier for the method
    // arguments?: ArgumentListNode; // If MethodCall can have arguments like Object:Method(arg1, arg2)
}

// ObjectPropertyAccessNode is listed in ASTNodeType but no interface in snippet.
// If it exists, it would be:
// export interface ObjectPropertyAccessNode extends ASTNode {
// kind: ASTNodeType.ObjectPropertyAccess;
//     object: Token; // Identifier
//     property: Token; // Identifier
// }
// For now, PropertyAccessNode in expressions.ts covers generic property access.
