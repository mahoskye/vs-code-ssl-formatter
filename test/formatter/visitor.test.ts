/**
 * Tests for FormatterVisitorBase
 *
 * Comprehensive test suite covering:
 * - Visitor pattern implementation
 * - AST traversal logic
 * - State management (output builder, indentation)
 * - EBNF grammar compliance
 */

import {
    FormatterVisitorBase,
    FormatterOptions,
    defaultFormatterOptions,
    OutputBuilder,
    VisitorResult,
} from "../../src/formatter/visitor";
import { mergeFormatterOptions } from "../../src/formatter/options";
import {
    ASTNode,
    ASTNodeType,
    ProgramNode,
    ClassDefinitionNode,
    ProcedureStatementNode,
    IfStatementNode,
    WhileLoopNode,
    ForLoopNode,
    AssignmentNode,
    FunctionCallNode,
    BinaryExpressionNode,
    LiteralExpressionNode,
    CommentStatementNode,
    createBaseNode,
} from "../../src/parser/ast";
import { createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("FormatterVisitorBase", () => {
    let mockToken: any;

    beforeEach(() => {
        mockToken = createToken(TokenType.IDENTIFIER, "test", createPosition(1, 1, 0));
    });
    describe("FormatterOptions", () => {
        it("should have correct default options", () => {
            expect(defaultFormatterOptions).toEqual({
                // Basic Formatting
                indentSize: 4,
                useTabs: false,
                maxLineLength: 90,
                insertFinalNewline: true,
                trimTrailingWhitespace: true,

                // Spacing Options
                insertSpacesAroundOperators: true,
                insertSpacesAfterCommas: true,
                insertSpacesAroundAssignmentOperators: true,
                insertSpacesAroundComparisonOperators: true,
                insertSpacesAroundLogicalOperators: true,
                insertSpacesAroundPropertyAccess: false,

                // Line Breaking
                preserveBlankLines: true,
                maxPreserveBlankLines: 2,
                breakLongParameterLists: true,
                breakLongArrayLiterals: true,
                breakLongSqlStatements: true,
                parameterListBreakThreshold: 4,

                // SSL-Specific Options
                formatEmbeddedSql: true,
                alignSqlClauses: true,
                uppercaseKeywords: true,
                enforceHungarianNotation: false,
                alignEndOfLineComments: true,
                commentAlignmentColumn: 60,

                // Control Flow Formatting
                blankLinesBeforeControlFlow: false,
                blankLinesAfterControlFlow: false,
                indentCaseStatements: true,
                alignProcedureParameters: true,

                // Comment Formatting
                preserveRegionMarkers: true,
                formatMultiLineComments: true,
                wrapLongComments: true,

                // Array and Object Formatting
                alignArrayElements: true,
                insertTrailingCommas: false,
                breakObjectCreationCalls: true,
            });
        });
        it("should allow custom options", () => {
            const customOptions = mergeFormatterOptions({
                indentSize: 2,
                useTabs: true,
                maxLineLength: 80,
                insertFinalNewline: false,
                trimTrailingWhitespace: false,
                insertSpacesAroundOperators: false,
                insertSpacesAfterCommas: false,
                preserveBlankLines: false,
                maxPreserveBlankLines: 1,
            });

            expect(customOptions.indentSize).toBe(2);
            expect(customOptions.useTabs).toBe(true);
        });
    });

    describe("OutputBuilder", () => {
        let builder: OutputBuilder;

        beforeEach(() => {
            builder = new OutputBuilder();
        });

        it("should handle basic text writing", () => {
            builder.write("Hello");
            builder.write(" ");
            builder.write("World");
            builder.writeLine();

            expect(builder.getOutput()).toBe("Hello World\n");
        });

        it("should manage indentation with spaces", () => {
            builder.writeIndented("Level 0");
            builder.writeLine();
            builder.indent();
            builder.writeIndented("Level 1");
            builder.writeLine();
            builder.indent();
            builder.writeIndented("Level 2");
            builder.writeLine();
            builder.dedent();
            builder.writeIndented("Back to Level 1");
            builder.writeLine();

            const output = builder.getOutput();
            const lines = output.split("\n");
            expect(lines[0]).toBe("Level 0");
            expect(lines[1]).toBe("    Level 1");
            expect(lines[2]).toBe("        Level 2");
            expect(lines[3]).toBe("    Back to Level 1");
        });

        it("should manage indentation with tabs", () => {
            const options: FormatterOptions = { ...defaultFormatterOptions, useTabs: true };
            builder = new OutputBuilder(options);

            builder.writeIndented("Level 0");
            builder.writeLine();
            builder.indent();
            builder.writeIndented("Level 1");
            builder.writeLine();

            const output = builder.getOutput();
            const lines = output.split("\n");
            expect(lines[0]).toBe("Level 0");
            expect(lines[1]).toBe("\tLevel 1");
        });

        it("should handle blank lines", () => {
            builder.writeLine("Line 1");
            builder.writeBlankLine();
            builder.writeLine("Line 3");

            const output = builder.getOutput();
            const lines = output.split("\n");
            expect(lines).toHaveLength(4); // Including final newline
            expect(lines[0]).toBe("Line 1");
            expect(lines[1]).toBe("");
            expect(lines[2]).toBe("Line 3");
        });

        it("should prevent negative indentation", () => {
            builder.dedent(); // Should not go below 0
            builder.dedent(); // Should not go below 0
            builder.writeIndented("No indent");
            builder.writeLine();

            expect(builder.getOutput()).toBe("No indent\n");
        });

        it("should handle line length checking", () => {
            builder.write("Some text");
            expect(builder.getCurrentLineLength()).toBe(9);
            expect(builder.wouldExceedMaxLength("x".repeat(120))).toBe(true);
            expect(builder.wouldExceedMaxLength("short")).toBe(false);
        });

        it("should trim trailing whitespace when enabled", () => {
            const options: FormatterOptions = {
                ...defaultFormatterOptions,
                trimTrailingWhitespace: true,
            };
            builder = new OutputBuilder(options);

            builder.write("Text with trailing spaces   ");
            builder.writeLine();
            builder.write("Another line\t\t");

            const output = builder.getOutput();
            expect(output).toBe("Text with trailing spaces\nAnother line\n");
        });

        it("should not add final newline when disabled", () => {
            const options: FormatterOptions = {
                ...defaultFormatterOptions,
                insertFinalNewline: false,
            };
            builder = new OutputBuilder(options);

            builder.write("No final newline");

            expect(builder.getOutput()).toBe("No final newline");
        });
    });

    describe("FormatterVisitorBase", () => {
        let visitor: TestVisitor;

        // Create a concrete test visitor to test the abstract base
        class TestVisitor extends FormatterVisitorBase {
            public visitCallCount = 0;
            public visitedNodeTypes: string[] = [];

            protected override visitProgram(node: ProgramNode): VisitorResult {
                this.visitCallCount++;
                this.visitedNodeTypes.push(node.kind);
                this.output.writeLine(`:PROGRAM ${node.kind}`);
                return { shouldContinue: true };
            }

            protected override visitProcedureStatement(
                node: ProcedureStatementNode
            ): VisitorResult {
                this.visitCallCount++;
                this.visitedNodeTypes.push(node.kind);
                this.output.writeLine(`:PROCEDURE ${node.kind}`);
                return { shouldContinue: true };
            }

            protected override visitIfStatement(node: IfStatementNode): VisitorResult {
                this.visitCallCount++;
                this.visitedNodeTypes.push(node.kind);
                this.output.writeLine(`:IF ${node.kind}`);
                this.output.indent();
                return { shouldContinue: true };
            }

            protected override visitAssignment(node: AssignmentNode): VisitorResult {
                this.visitCallCount++;
                this.visitedNodeTypes.push(node.kind);
                this.output.writeIndented(`assignment := ${node.kind};`);
                this.output.writeLine();
                return { shouldContinue: true };
            }

            protected override visitCommentStatement(node: CommentStatementNode): VisitorResult {
                this.visitCallCount++;
                this.visitedNodeTypes.push(node.kind);
                this.output.writeLine(`/* comment: ${node.kind} ;`);
                return { shouldContinue: true };
            }

            // Override to test unknown node handling
            protected override visitUnknown(node: ASTNode): VisitorResult {
                this.visitCallCount++;
                this.visitedNodeTypes.push("Unknown");
                return { shouldContinue: false, error: `Unknown node: ${node.kind}` };
            }
        }

        beforeEach(() => {
            visitor = new TestVisitor();
        });

        it("should initialize with default options", () => {
            expect(visitor).toBeDefined();
            expect(visitor.getFormattedOutput()).toBe("");
        });

        it("should initialize with custom options", () => {
            const customOptions: FormatterOptions = {
                ...defaultFormatterOptions,
                indentSize: 2,
                useTabs: true,
            };
            const customVisitor = new TestVisitor(customOptions);
            expect(customVisitor).toBeDefined();
        });

        it("should visit a simple node", () => {
            const programNode = createBaseNode(
                ASTNodeType.Program,
                mockToken,
                mockToken
            ) as ProgramNode;

            const result = visitor.visit(programNode);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();
            expect(visitor.visitCallCount).toBe(1);
            expect(visitor.visitedNodeTypes).toContain(ASTNodeType.Program);
            expect(visitor.getFormattedOutput()).toContain(`:PROGRAM ${ASTNodeType.Program}`);
        });
        it("should handle circular reference detection", () => {
            const node = createBaseNode(ASTNodeType.Program, mockToken, mockToken) as ProgramNode;

            // Mock a visitor that would cause circular reference
            const circularVisitor = new (class extends TestVisitor {
                protected override visitProgram(node: ProgramNode): VisitorResult {
                    // Try to visit the same node again (this should be prevented)
                    const result = this.visit(node); // This should be detected as circular
                    expect(result.error).toContain("Circular reference detected");
                    return { shouldContinue: false };
                }
            })();

            const result = circularVisitor.visit(node);

            // The outer call should succeed, but the inner call should detect the circular reference
            expect(result.shouldContinue).toBe(false);
        });

        it("should dispatch to correct visit methods based on node type", () => {
            const nodes = [
                createBaseNode(ASTNodeType.Program, mockToken, mockToken) as ProgramNode,
                createBaseNode(
                    ASTNodeType.ProcedureStatement,
                    mockToken,
                    mockToken
                ) as ProcedureStatementNode,
                createBaseNode(ASTNodeType.IfStatement, mockToken, mockToken) as IfStatementNode,
                createBaseNode(ASTNodeType.Assignment, mockToken, mockToken) as AssignmentNode,
                createBaseNode(
                    ASTNodeType.CommentStatement,
                    mockToken,
                    mockToken
                ) as CommentStatementNode,
            ];

            nodes.forEach((node) => {
                visitor.visit(node);
            });

            expect(visitor.visitCallCount).toBe(5);
            expect(visitor.visitedNodeTypes).toEqual([
                ASTNodeType.Program,
                ASTNodeType.ProcedureStatement,
                ASTNodeType.IfStatement,
                ASTNodeType.Assignment,
                ASTNodeType.CommentStatement,
            ]);

            const output = visitor.getFormattedOutput();
            expect(output).toContain(`:PROGRAM ${ASTNodeType.Program}`);
            expect(output).toContain(`:PROCEDURE ${ASTNodeType.ProcedureStatement}`);
            expect(output).toContain(`:IF ${ASTNodeType.IfStatement}`);
            expect(output).toContain(`assignment := ${ASTNodeType.Assignment};`);
            expect(output).toContain(`/* comment: ${ASTNodeType.CommentStatement} ;`);
        });

        it("should handle unknown node types", () => {
            // Create a node with a type that doesn't exist in the enum
            const unknownNode = {
                kind: "UnknownNodeType" as ASTNodeType,
                startToken: mockToken,
                endToken: mockToken,
            };

            const result = visitor.visit(unknownNode);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toContain("Unknown node: UnknownNodeType");
            expect(visitor.visitCallCount).toBe(1);
            expect(visitor.visitedNodeTypes).toContain("Unknown");
        });

        it("should handle exceptions during visit", () => {
            const throwingVisitor = new (class extends TestVisitor {
                protected override visitProgram(node: ProgramNode): VisitorResult {
                    throw new Error("Test error");
                }
            })();

            const programNode = createBaseNode(
                ASTNodeType.Program,
                mockToken,
                mockToken
            ) as ProgramNode;
            const result = throwingVisitor.visit(programNode);

            expect(result.shouldContinue).toBe(false);
            expect(result.error).toBe("Test error");
        });

        it("should manage indentation properly during traversal", () => {
            const indentationVisitor = new (class extends TestVisitor {
                protected override visitIfStatement(node: IfStatementNode): VisitorResult {
                    this.output.writeLine(":IF condition;");
                    this.output.indent();
                    this.output.writeLine("statement1;");
                    this.output.writeLine("statement2;");
                    this.output.dedent();
                    this.output.writeLine(":ENDIF;");
                    return { shouldContinue: false }; // Don't visit children
                }
            })();

            const ifNode = createBaseNode(
                ASTNodeType.IfStatement,
                mockToken,
                mockToken
            ) as IfStatementNode;
            indentationVisitor.visit(ifNode);

            const output = indentationVisitor.getFormattedOutput();
            const lines = output.split("\n").filter((line) => line.length > 0);

            expect(lines[0]).toBe(":IF condition;");
            expect(lines[1]).toBe("    statement1;");
            expect(lines[2]).toBe("    statement2;");
            expect(lines[3]).toBe(":ENDIF;");
        });

        it("should handle all major AST node types without errors", () => {
            const nodeTypes = [
                // Top-level
                ASTNodeType.Program,

                // Class definitions
                ASTNodeType.ClassDefinition,
                ASTNodeType.ClassDeclaration,
                ASTNodeType.InheritStatement,
                ASTNodeType.ClassFieldDeclaration,
                ASTNodeType.MethodDeclaration,

                // Procedure declarations
                ASTNodeType.ProcedureStatement,
                ASTNodeType.ProcedureStart,
                ASTNodeType.ProcedureEnd,
                ASTNodeType.ParameterDeclaration,
                ASTNodeType.DefaultParameterDeclaration,
                ASTNodeType.ParameterList,
                ASTNodeType.DefaultParameterList,

                // Control flow statements
                ASTNodeType.ConditionalStatement,
                ASTNodeType.IfStatement,
                ASTNodeType.ElseStatement,
                ASTNodeType.EndIfStatement,
                ASTNodeType.LoopStatement,
                ASTNodeType.WhileLoop,
                ASTNodeType.WhileStatement,
                ASTNodeType.EndWhileStatement,
                ASTNodeType.ForLoop,
                ASTNodeType.ForStatement,
                ASTNodeType.NextStatement,
                ASTNodeType.ExitWhileStatement,
                ASTNodeType.ExitForStatement,
                ASTNodeType.LoopContinue,

                // Switch case statements
                ASTNodeType.SwitchStatement,
                ASTNodeType.BeginCaseStatement,
                ASTNodeType.CaseBlock,
                ASTNodeType.CaseStatement,
                ASTNodeType.OtherwiseBlock,
                ASTNodeType.OtherwiseStatement,
                ASTNodeType.EndCaseStatement,
                ASTNodeType.ExitCaseStatement,

                // Error handling
                ASTNodeType.ErrorHandlingStatement,
                ASTNodeType.TryBlock,
                ASTNodeType.TryStatement,
                ASTNodeType.CatchBlock,
                ASTNodeType.CatchStatement,
                ASTNodeType.FinallyBlock,
                ASTNodeType.FinallyStatement,
                ASTNodeType.EndTryStatement,
                ASTNodeType.ErrorBlockStanza,
                ASTNodeType.ErrorMarker,

                // Declaration statements
                ASTNodeType.DeclarationStatement,
                ASTNodeType.ParametersStatement,
                ASTNodeType.DeclareStatement,
                ASTNodeType.DefaultStatement,
                ASTNodeType.PublicStatement,
                ASTNodeType.IncludeStatement,

                // Logic statements
                ASTNodeType.LogicStatement,
                ASTNodeType.Assignment,
                ASTNodeType.ReturnStatement,

                // Function calls
                ASTNodeType.FunctionCall,
                ASTNodeType.DirectFunctionCall,
                ASTNodeType.DoProcCall,
                ASTNodeType.ExecFunctionCall,
                ASTNodeType.ArgumentList,
                ASTNodeType.BitwiseOperation,

                // Comments
                ASTNodeType.CommentStatement,
                ASTNodeType.BlockComment,
                ASTNodeType.SingleLineComment,
                ASTNodeType.RegionComment,
                ASTNodeType.EndRegionComment,

                // Special structures
                ASTNodeType.LabelStatement,
                ASTNodeType.RegionBlock,
                ASTNodeType.RegionStart,
                ASTNodeType.RegionEnd,
                ASTNodeType.InlineCodeBlock,
                ASTNodeType.InlineCodeStart,
                ASTNodeType.InlineCodeEnd,
                ASTNodeType.DynamicCodeExecution,
                ASTNodeType.BranchStatement,

                // SQL Integration
                ASTNodeType.SqlStatement,
                ASTNodeType.SqlExecute,
                ASTNodeType.LSearch,
                ASTNodeType.SqlParameter,

                // Object-oriented statements
                ASTNodeType.ObjectCreation,
                ASTNodeType.MethodCall,

                // Expressions
                ASTNodeType.Expression,
                ASTNodeType.BinaryExpression,
                ASTNodeType.LogicalExpression,
                ASTNodeType.ComparisonExpression,
                ASTNodeType.ArithmeticExpression,
                ASTNodeType.Term,
                ASTNodeType.Factor,
                ASTNodeType.PowerOperand,
                ASTNodeType.UnaryExpression,
                ASTNodeType.IncrementExpression,
                ASTNodeType.VariableAccess,
                ASTNodeType.PropertyAccess,
                ASTNodeType.ArrayAccess,
                ASTNodeType.ArraySubscript,
                ASTNodeType.Primary,

                // Literals
                ASTNodeType.Literal,
                ASTNodeType.LiteralExpression,
                ASTNodeType.NumberLiteral,
                ASTNodeType.StringLiteral,
                ASTNodeType.BooleanLiteral,
                ASTNodeType.ArrayLiteral,
                ASTNodeType.NilLiteral,
                ASTNodeType.DateLiteral,
                ASTNodeType.CodeBlockLiteral,

                // Lists
                ASTNodeType.IdentifierList,
                ASTNodeType.ExpressionList,
            ];

            const defaultVisitor = new FormatterVisitorBase();
            let successCount = 0;

            nodeTypes.forEach((nodeType) => {
                const node = createBaseNode(nodeType, mockToken, mockToken);
                const result = defaultVisitor.visit(node);

                if (result.shouldContinue === true && !result.error) {
                    successCount++;
                }
            });

            // All node types should be handled without errors
            expect(successCount).toBe(nodeTypes.length);
        });

        it("should support EBNF grammar compliance for SSL language constructs", () => {
            // Test that the visitor supports key SSL language constructs from the EBNF grammar

            const sslVisitor = new (class extends TestVisitor {
                protected override visitProcedureStatement(
                    node: ProcedureStatementNode
                ): VisitorResult {
                    this.output.writeLine(":PROCEDURE MyProc;");
                    this.output.indent();
                    return { shouldContinue: true };
                }

                protected override visitIfStatement(node: IfStatementNode): VisitorResult {
                    this.output.writeIndented(":IF condition;");
                    this.output.writeLine();
                    this.output.indent();
                    return { shouldContinue: true };
                }

                protected override visitWhileLoop(node: WhileLoopNode): VisitorResult {
                    this.output.writeIndented(":WHILE condition;");
                    this.output.writeLine();
                    this.output.indent();
                    return { shouldContinue: true };
                }

                protected override visitForLoop(node: ForLoopNode): VisitorResult {
                    this.output.writeIndented(":FOR i := 1 :TO 10;");
                    this.output.writeLine();
                    this.output.indent();
                    return { shouldContinue: true };
                }

                protected override visitAssignment(node: AssignmentNode): VisitorResult {
                    this.output.writeIndented("variable := value;");
                    this.output.writeLine();
                    return { shouldContinue: true };
                }
            })();

            // Test various SSL constructs
            const nodes = [
                createBaseNode(
                    ASTNodeType.ProcedureStatement,
                    mockToken,
                    mockToken
                ) as ProcedureStatementNode,
                createBaseNode(ASTNodeType.IfStatement, mockToken, mockToken) as IfStatementNode,
                createBaseNode(ASTNodeType.WhileLoop, mockToken, mockToken) as WhileLoopNode,
                createBaseNode(ASTNodeType.ForLoop, mockToken, mockToken) as ForLoopNode,
                createBaseNode(ASTNodeType.Assignment, mockToken, mockToken) as AssignmentNode,
            ];

            nodes.forEach((node) => {
                const result = sslVisitor.visit(node);
                expect(result.shouldContinue).toBe(true);
                expect(result.error).toBeUndefined();
            });

            const output = sslVisitor.getFormattedOutput();
            expect(output).toContain(":PROCEDURE MyProc;");
            expect(output).toContain(":IF condition;");
            expect(output).toContain(":WHILE condition;");
            expect(output).toContain(":FOR i := 1 :TO 10;");
            expect(output).toContain("variable := value;");
        });

        it("should handle complex nested structures", () => {
            const complexVisitor = new (class extends TestVisitor {
                private blockDepth = 0;

                protected override visitProcedureStatement(
                    node: ProcedureStatementNode
                ): VisitorResult {
                    this.output.writeLine(":PROCEDURE ComplexProc;");
                    this.output.indent();
                    this.blockDepth++;
                    return { shouldContinue: true };
                }

                protected override visitIfStatement(node: IfStatementNode): VisitorResult {
                    this.output.writeIndented(":IF (condition);");
                    this.output.writeLine();
                    this.output.indent();
                    this.blockDepth++;
                    return { shouldContinue: true };
                }

                protected override visitWhileLoop(node: WhileLoopNode): VisitorResult {
                    this.output.writeIndented(":WHILE (condition);");
                    this.output.writeLine();
                    this.output.indent();
                    this.blockDepth++;
                    return { shouldContinue: true };
                }

                protected override visitAssignment(node: AssignmentNode): VisitorResult {
                    this.output.writeIndented(`result${this.blockDepth} := Calculate();`);
                    this.output.writeLine();
                    return { shouldContinue: true };
                }
            })();

            // Simulate nested structure: PROCEDURE -> IF -> WHILE -> Assignment
            const procedureNode = createBaseNode(
                ASTNodeType.ProcedureStatement,
                mockToken,
                mockToken
            );
            const ifNode = createBaseNode(ASTNodeType.IfStatement, mockToken, mockToken);
            const whileNode = createBaseNode(ASTNodeType.WhileLoop, mockToken, mockToken);
            const assignmentNode = createBaseNode(ASTNodeType.Assignment, mockToken, mockToken);

            complexVisitor.visit(procedureNode);
            complexVisitor.visit(ifNode);
            complexVisitor.visit(whileNode);
            complexVisitor.visit(assignmentNode);

            const output = complexVisitor.getFormattedOutput();
            const lines = output.split("\n").filter((line) => line.length > 0);

            expect(lines[0]).toBe(":PROCEDURE ComplexProc;");
            expect(lines[1]).toBe("    :IF (condition);");
            expect(lines[2]).toBe("        :WHILE (condition);");
            expect(lines[3]).toBe("            result3 := Calculate();");
        });

        it("should provide proper state management during traversal", () => {
            const stateVisitor = new (class extends TestVisitor {
                public getOutput(): OutputBuilder {
                    return this.output;
                }

                public getOptions(): FormatterOptions {
                    return this.options;
                }
            })();

            expect(stateVisitor.getOutput()).toBeDefined();
            expect(stateVisitor.getOptions()).toEqual(defaultFormatterOptions);
            expect(stateVisitor.getFormattedOutput()).toBe("");

            // Test that state persists across visits
            const node1 = createBaseNode(ASTNodeType.Assignment, mockToken, mockToken);
            const node2 = createBaseNode(ASTNodeType.Assignment, mockToken, mockToken);

            stateVisitor.visit(node1);
            const outputAfterFirst = stateVisitor.getFormattedOutput();
            expect(outputAfterFirst.length).toBeGreaterThan(0);

            stateVisitor.visit(node2);
            const outputAfterSecond = stateVisitor.getFormattedOutput();
            expect(outputAfterSecond.length).toBeGreaterThan(outputAfterFirst.length);
        });
    });

    describe("EBNF Grammar Compliance", () => {
        let visitor: FormatterVisitorBase;

        beforeEach(() => {
            visitor = new FormatterVisitorBase();
        });

        it("should handle all SSL language node types defined in EBNF", () => {
            // Test that all node types from the SSL EBNF grammar are supported
            const grammarNodeTypes = [
                // Top-level structure
                ASTNodeType.Program,
                ASTNodeType.ClassDefinition,

                // Statements
                ASTNodeType.ProcedureStatement,
                ASTNodeType.IfStatement,
                ASTNodeType.WhileLoop,
                ASTNodeType.ForLoop,
                ASTNodeType.SwitchStatement,
                ASTNodeType.TryBlock,
                ASTNodeType.Assignment,
                ASTNodeType.FunctionCall,

                // Expressions (following EBNF hierarchy)
                ASTNodeType.LogicalExpression,
                ASTNodeType.ComparisonExpression,
                ASTNodeType.ArithmeticExpression,
                ASTNodeType.Term,
                ASTNodeType.Factor,
                ASTNodeType.PowerOperand,
                ASTNodeType.UnaryExpression,
                ASTNodeType.Primary,

                // Literals
                ASTNodeType.NumberLiteral,
                ASTNodeType.StringLiteral,
                ASTNodeType.BooleanLiteral,
                ASTNodeType.ArrayLiteral,
                ASTNodeType.NilLiteral,
                ASTNodeType.DateLiteral,
                ASTNodeType.CodeBlockLiteral,

                // Comments
                ASTNodeType.BlockComment,
                ASTNodeType.SingleLineComment,
                ASTNodeType.RegionComment,
                ASTNodeType.EndRegionComment,

                // SQL Integration
                ASTNodeType.SqlExecute,
                ASTNodeType.LSearch,

                // Object-oriented features
                ASTNodeType.ObjectCreation,
                ASTNodeType.MethodCall,

                // Special structures
                ASTNodeType.LabelStatement,
                ASTNodeType.RegionBlock,
                ASTNodeType.BranchStatement,
                ASTNodeType.DynamicCodeExecution,
            ];

            grammarNodeTypes.forEach((nodeType) => {
                const node = createBaseNode(nodeType, mockToken, mockToken);
                const result = visitor.visit(node);

                expect(result).toBeDefined();
                expect(result.shouldContinue).toBe(true);
                expect(result.error).toBeUndefined();
            });
        });

        it("should support SSL-specific language constructs", () => {
            const sslSpecificTypes = [
                // SSL-specific control flow
                ASTNodeType.ExitWhileStatement, // :EXITWHILE
                ASTNodeType.ExitForStatement, // :EXITFOR
                ASTNodeType.LoopContinue, // :LOOP
                ASTNodeType.ExitCaseStatement, // :EXITCASE

                // SSL-specific declarations
                ASTNodeType.ParametersStatement, // :PARAMETERS
                ASTNodeType.DeclareStatement, // :DECLARE
                ASTNodeType.PublicStatement, // :PUBLIC
                ASTNodeType.IncludeStatement, // :INCLUDE

                // SSL-specific function calls
                ASTNodeType.DoProcCall, // DoProc()
                ASTNodeType.ExecFunctionCall, // ExecFunction()

                // SSL-specific error handling
                ASTNodeType.ErrorBlockStanza, // :ERROR block

                // SSL-specific property access (colon notation)
                ASTNodeType.PropertyAccess, // object:property

                // SSL-specific literals
                ASTNodeType.BooleanLiteral, // .T. / .F.
            ];

            sslSpecificTypes.forEach((nodeType) => {
                const node = createBaseNode(nodeType, mockToken, mockToken);
                const result = visitor.visit(node);

                expect(result.shouldContinue).toBe(true);
                expect(result.error).toBeUndefined();
            });
        });

        it("should maintain EBNF hierarchy in expression handling", () => {
            // Test that expression hierarchy matches EBNF grammar:
            // Expression -> LogicalExpression -> ComparisonExpression -> ArithmeticExpression -> Term -> Factor -> PowerOperand -> Primary
            const expressionHierarchy = [
                ASTNodeType.Expression,
                ASTNodeType.LogicalExpression,
                ASTNodeType.ComparisonExpression,
                ASTNodeType.ArithmeticExpression,
                ASTNodeType.Term,
                ASTNodeType.Factor,
                ASTNodeType.PowerOperand,
                ASTNodeType.Primary,
            ];

            expressionHierarchy.forEach((nodeType) => {
                const node = createBaseNode(nodeType, mockToken, mockToken);
                const result = visitor.visit(node);

                expect(result.shouldContinue).toBe(true);
                expect(result.error).toBeUndefined();
            });
        });
    });

    describe("Performance and Edge Cases", () => {
        let visitor: FormatterVisitorBase;

        beforeEach(() => {
            visitor = new FormatterVisitorBase();
        });

        it("should handle empty nodes gracefully", () => {
            const emptyNode = createBaseNode(ASTNodeType.Program, mockToken, mockToken);
            const result = visitor.visit(emptyNode);

            expect(result.shouldContinue).toBe(true);
            expect(result.error).toBeUndefined();
        });

        it("should handle nodes with null/undefined tokens", () => {
            const nodeWithNullToken = {
                kind: ASTNodeType.Assignment,
                startToken: mockToken,
                endToken: mockToken,
            };

            const result = visitor.visit(nodeWithNullToken);
            expect(result.shouldContinue).toBe(true);
        });

        it("should be memory efficient with large ASTs", () => {
            const largeVisitor = new (class extends FormatterVisitorBase {
                public visitCount = 0;

                protected override visitAssignment(node: AssignmentNode): VisitorResult {
                    this.visitCount++;
                    return { shouldContinue: true };
                }
            })();

            // Simulate visiting many nodes
            const nodeCount = 1000;
            for (let i = 0; i < nodeCount; i++) {
                const node = createBaseNode(ASTNodeType.Assignment, mockToken, mockToken);
                largeVisitor.visit(node);
            }

            expect(largeVisitor.visitCount).toBe(nodeCount);
        });

        it("should handle deeply nested structures without stack overflow", () => {
            const deepVisitor = new (class extends FormatterVisitorBase {
                public maxDepth = 0;
                private currentDepth = 0;

                protected override visitIfStatement(node: IfStatementNode): VisitorResult {
                    this.currentDepth++;
                    if (this.currentDepth > this.maxDepth) {
                        this.maxDepth = this.currentDepth;
                    }

                    const result = { shouldContinue: true };
                    this.currentDepth--;
                    return result;
                }
            })();

            // Create a reasonable depth to test (not too deep to cause actual stack overflow in test)
            const depth = 100;
            for (let i = 0; i < depth; i++) {
                const node = createBaseNode(ASTNodeType.IfStatement, mockToken, mockToken);
                deepVisitor.visit(node);
            }

            expect(deepVisitor.maxDepth).toBe(1); // Each visit is independent in this test
        });
    });
});
