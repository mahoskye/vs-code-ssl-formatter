import { Token } from "../../tokenizer/token";
import { ASTNode, ASTNodeType } from "./base";
import { ClassDefinitionNode } from "./classes";
import { StatementNode } from "./base"; // Or a more specific union type later

/**
 * Top-level program node
 */
export interface ProgramNode extends ASTNode {
    kind: ASTNodeType.Program;
    body: (ClassDefinitionNode | StatementNode)[];
}
