/**
 * Comment Association
 *
 * - Associate comments with AST nodes
 * - Determine leading vs trailing comments
 * - Preserve comment positioning
 */

import { ASTNode } from "../parser/ast/base";
import { Token } from "../tokenizer/token";
import { TokenType } from "../tokenizer/tokenType";
import {
    CommentStatementNode,
    BlockCommentNode,
    SingleLineCommentNode,
    RegionCommentNode,
    EndRegionCommentNode,
} from "../parser/ast/comments";

/**
 * Position relationship between a comment and an AST node
 */
export enum CommentPosition {
    /** Comment appears before the associated node (leading comment) */
    Leading = "Leading",
    /** Comment appears after the associated node on the same line (trailing comment) */
    Trailing = "Trailing",
    /** Comment appears after the associated node on a different line (following comment) */
    Following = "Following",
    /** Comment appears standalone without clear association */
    Standalone = "Standalone",
}

/**
 * Association between a comment and an AST node
 */
export interface CommentAssociation {
    /** The comment node */
    comment:
        | CommentStatementNode
        | BlockCommentNode
        | SingleLineCommentNode
        | RegionCommentNode
        | EndRegionCommentNode;
    /** The AST node the comment is associated with */
    associatedNode: ASTNode | null;
    /** Position relationship between comment and node */
    position: CommentPosition;
    /** Original line number where the comment appears */
    lineNumber: number;
    /** Original column position where the comment starts */
    columnPosition: number;
    /** Whether this comment should be preserved during formatting */
    preserve: boolean;
}

/**
 * Comment association options
 */
export interface CommentAssociationOptions {
    /** Maximum line distance for leading comment association */
    maxLeadingDistance: number;
    /** Maximum line distance for following comment association */
    maxFollowingDistance: number;
    /** Whether to associate comments with the closest node */
    associateWithClosestNode: boolean;
    /** Whether to preserve standalone comments */
    preserveStandaloneComments: boolean;
    /** Whether to associate region comments with block structures */
    associateRegionComments: boolean;
}

/**
 * Default comment association options
 */
export const defaultCommentAssociationOptions: CommentAssociationOptions = {
    maxLeadingDistance: 2, // Comments up to 2 lines before a node are considered leading
    maxFollowingDistance: 1, // Comments up to 1 line after a node are considered following
    associateWithClosestNode: true,
    preserveStandaloneComments: true,
    associateRegionComments: true,
};

/**
 * Comment association engine that analyzes relationships between comments and AST nodes
 */
export class CommentAssociator {
    private readonly options: CommentAssociationOptions;
    private readonly associations: Map<
        | CommentStatementNode
        | BlockCommentNode
        | SingleLineCommentNode
        | RegionCommentNode
        | EndRegionCommentNode,
        CommentAssociation
    > = new Map();

    constructor(options: CommentAssociationOptions = defaultCommentAssociationOptions) {
        this.options = { ...defaultCommentAssociationOptions, ...options };
    }

    /**
     * Associate comments with AST nodes based on their positioning
     *
     * @param comments Array of comment nodes to associate
     * @param nodes Array of AST nodes to associate comments with
     * @returns Array of comment associations
     */
    public associateComments(
        comments: (
            | CommentStatementNode
            | BlockCommentNode
            | SingleLineCommentNode
            | RegionCommentNode
            | EndRegionCommentNode
        )[],
        nodes: ASTNode[]
    ): CommentAssociation[] {
        this.associations.clear();

        // Sort comments and nodes by position for easier processing
        const sortedComments = this.sortByPosition(comments);
        const sortedNodes = this.sortByPosition(nodes);

        const associations: CommentAssociation[] = [];

        for (const comment of sortedComments) {
            const association = this.createAssociation(comment, sortedNodes);
            this.associations.set(comment, association);
            associations.push(association);
        }

        return associations;
    }

    /**
     * Get the association for a specific comment
     */
    public getAssociation(
        comment:
            | CommentStatementNode
            | BlockCommentNode
            | SingleLineCommentNode
            | RegionCommentNode
            | EndRegionCommentNode
    ): CommentAssociation | undefined {
        return this.associations.get(comment);
    }

    /**
     * Get all leading comments for a specific AST node
     */
    public getLeadingComments(node: ASTNode): CommentAssociation[] {
        const associations: CommentAssociation[] = [];

        for (const association of this.associations.values()) {
            if (
                association.associatedNode === node &&
                association.position === CommentPosition.Leading
            ) {
                associations.push(association);
            }
        }

        // Sort by line number (earliest first)
        return associations.sort((a, b) => a.lineNumber - b.lineNumber);
    }

    /**
     * Get all trailing comments for a specific AST node
     */
    public getTrailingComments(node: ASTNode): CommentAssociation[] {
        const associations: CommentAssociation[] = [];

        for (const association of this.associations.values()) {
            if (
                association.associatedNode === node &&
                (association.position === CommentPosition.Trailing ||
                    association.position === CommentPosition.Following)
            ) {
                associations.push(association);
            }
        }

        // Sort by line number (earliest first)
        return associations.sort((a, b) => a.lineNumber - b.lineNumber);
    }

    /**
     * Get all standalone comments (not associated with any node)
     */
    public getStandaloneComments(): CommentAssociation[] {
        const associations: CommentAssociation[] = [];

        for (const association of this.associations.values()) {
            if (association.position === CommentPosition.Standalone) {
                associations.push(association);
            }
        }

        // Sort by line number
        return associations.sort((a, b) => a.lineNumber - b.lineNumber);
    }

    /**
     * Create a comment association for a given comment
     */
    private createAssociation(
        comment:
            | CommentStatementNode
            | BlockCommentNode
            | SingleLineCommentNode
            | RegionCommentNode
            | EndRegionCommentNode,
        nodes: ASTNode[]
    ): CommentAssociation {
        const commentLine = this.getLineNumber(comment);
        const commentColumn = this.getColumnNumber(comment);

        // Find the best matching node for this comment
        const { node: associatedNode, position } = this.findBestAssociation(comment, nodes);

        return {
            comment,
            associatedNode,
            position,
            lineNumber: commentLine,
            columnPosition: commentColumn,
            preserve: this.shouldPreserveComment(comment, position),
        };
    }

    /**
     * Find the best AST node to associate with a comment
     */
    private findBestAssociation(
        comment:
            | CommentStatementNode
            | BlockCommentNode
            | SingleLineCommentNode
            | RegionCommentNode
            | EndRegionCommentNode,
        nodes: ASTNode[]
    ): { node: ASTNode | null; position: CommentPosition } {
        const commentLine = this.getLineNumber(comment);

        // Special handling for region comments
        if (this.isRegionComment(comment) && this.options.associateRegionComments) {
            const regionAssociation = this.findRegionAssociation(comment, nodes);
            if (regionAssociation.node) {
                return regionAssociation;
            }
        }

        let bestNode: ASTNode | null = null;
        let bestPosition = CommentPosition.Standalone;
        let bestDistance = Infinity;

        for (const node of nodes) {
            const nodeLine = this.getLineNumber(node);
            const distance = Math.abs(commentLine - nodeLine);

            // Check for leading comment (comment before node)
            if (
                commentLine < nodeLine &&
                nodeLine - commentLine <= this.options.maxLeadingDistance &&
                (distance < bestDistance || bestPosition === CommentPosition.Standalone)
            ) {
                bestNode = node;
                bestPosition = CommentPosition.Leading;
                bestDistance = distance;
            }

            // Check for trailing comment (comment on same line as node)
            else if (
                commentLine === nodeLine &&
                this.getColumnNumber(comment) > this.getColumnNumber(node)
            ) {
                bestNode = node;
                bestPosition = CommentPosition.Trailing;
                bestDistance = 0; // Same line takes precedence
            }

            // Check for following comment (comment after node)
            else if (
                commentLine > nodeLine &&
                commentLine - nodeLine <= this.options.maxFollowingDistance &&
                (distance < bestDistance || bestPosition === CommentPosition.Standalone)
            ) {
                bestNode = node;
                bestPosition = CommentPosition.Following;
                bestDistance = distance;
            }
        }

        return { node: bestNode, position: bestPosition };
    }

    /**
     * Find association for region comments with block structures
     */
    private findRegionAssociation(
        comment:
            | CommentStatementNode
            | BlockCommentNode
            | SingleLineCommentNode
            | RegionCommentNode
            | EndRegionCommentNode,
        nodes: ASTNode[]
    ): { node: ASTNode | null; position: CommentPosition } {
        // Region comments should be associated with the next significant block structure
        const commentLine = this.getLineNumber(comment);

        for (const node of nodes) {
            const nodeLine = this.getLineNumber(node);

            // Region comment should be before the associated block
            if (nodeLine > commentLine && this.isBlockStructure(node)) {
                return { node, position: CommentPosition.Leading };
            }
        }

        return { node: null, position: CommentPosition.Standalone };
    }

    /**
     * Check if a comment is a region comment
     */
    private isRegionComment(
        comment:
            | CommentStatementNode
            | BlockCommentNode
            | SingleLineCommentNode
            | RegionCommentNode
            | EndRegionCommentNode
    ): boolean {
        return comment.kind === "RegionComment" || comment.kind === "EndRegionComment";
    }

    /**
     * Check if a node represents a block structure (procedures, classes, control flow)
     */
    private isBlockStructure(node: ASTNode): boolean {
        const blockTypes = [
            "ProcedureStatement",
            "ClassDefinition",
            "IfStatement",
            "WhileLoop",
            "ForLoop",
            "SwitchStatement",
            "TryBlock",
            "ErrorBlockStanza",
        ];

        return blockTypes.includes(node.kind);
    }

    /**
     * Determine if a comment should be preserved during formatting
     */
    private shouldPreserveComment(
        comment:
            | CommentStatementNode
            | BlockCommentNode
            | SingleLineCommentNode
            | RegionCommentNode
            | EndRegionCommentNode,
        position: CommentPosition
    ): boolean {
        // Always preserve region comments if enabled
        if (this.isRegionComment(comment) && this.options.associateRegionComments) {
            return true;
        }

        // Preserve standalone comments if enabled
        if (position === CommentPosition.Standalone && this.options.preserveStandaloneComments) {
            return true;
        }

        // Always preserve leading and trailing comments
        return (
            position === CommentPosition.Leading ||
            position === CommentPosition.Trailing ||
            position === CommentPosition.Following
        );
    }

    /**
     * Sort nodes by their position in the source code
     */
    private sortByPosition<
        T extends
            | ASTNode
            | CommentStatementNode
            | BlockCommentNode
            | SingleLineCommentNode
            | RegionCommentNode
            | EndRegionCommentNode
    >(nodes: T[]): T[] {
        return [...nodes].sort((a, b) => {
            const aLine = this.getLineNumber(a);
            const bLine = this.getLineNumber(b);

            if (aLine !== bLine) {
                return aLine - bLine;
            }

            return this.getColumnNumber(a) - this.getColumnNumber(b);
        });
    }

    /**
     * Get line number from a node's position
     */
    private getLineNumber(
        node:
            | ASTNode
            | CommentStatementNode
            | BlockCommentNode
            | SingleLineCommentNode
            | RegionCommentNode
            | EndRegionCommentNode
    ): number {
        if (node.startToken && node.startToken.range) {
            return node.startToken.range.start.line;
        }
        return 0;
    }

    /**
     * Get column number from a node's position
     */
    private getColumnNumber(
        node:
            | ASTNode
            | CommentStatementNode
            | BlockCommentNode
            | SingleLineCommentNode
            | RegionCommentNode
            | EndRegionCommentNode
    ): number {
        if (node.startToken && node.startToken.range) {
            return node.startToken.range.start.column;
        }
        return 0;
    }

    /**
     * Clear all associations
     */
    public clear(): void {
        this.associations.clear();
    }

    /**
     * Get total number of associations
     */
    public size(): number {
        return this.associations.size;
    }
}
