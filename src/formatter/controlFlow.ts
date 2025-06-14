/**
 * Control Flow Formatters
 *
 * Formats SSL control flow constructs including:
 * - Format :IF/:ELSE/:ENDIF blocks
 * - Format :WHILE/:ENDWHILE loops
 * - Format :FOR/:NEXT loops
 * - Format exit statements (:EXITWHILE, :EXITFOR)
 *
 * # Before/After Formatting Examples
 *
 * ## IF/ELSE/ENDIF Blocks
 * **Before:**
 * ```ssl
 * if condition1 and condition2 then
 * processData()
 * else
 * handleError()
 * endif
 * ```
 *
 * **After:**
 * ```ssl
 * If condition1 And condition2 Then
 *     ProcessData()
 * Else
 *     HandleError()
 * EndIf
 * ```
 *
 * ## Nested IF Statements
 * **Before:**
 * ```ssl
 * if outerCondition then
 * if innerCondition then
 * result = processInner()
 * else
 * result = processAlternative()
 * endif
 * else
 * result = processOuter()
 * endif
 * ```
 *
 * **After:**
 * ```ssl
 * If outerCondition Then
 *     If innerCondition Then
 *         result = ProcessInner()
 *     Else
 *         result = ProcessAlternative()
 *     EndIf
 * Else
 *     result = ProcessOuter()
 * EndIf
 * ```
 *
 * ## WHILE Loops
 * **Before:**
 * ```ssl
 * while counter<10 and active
 * doWork(counter)
 * counter=counter+1
 * endwhile
 * ```
 *
 * **After:**
 * ```ssl
 * While counter < 10 And active
 *     DoWork(counter)
 *     counter = counter + 1
 * EndWhile
 * ```
 *
 * ## FOR Loops
 * **Before:**
 * ```ssl
 * for i=1 to maxCount
 * processItem(i)
 * if shouldBreak then
 * exitfor
 * endif
 * next
 * ```
 *
 * **After:**
 * ```ssl
 * For i = 1 To maxCount
 *     ProcessItem(i)
 *     If shouldBreak Then
 *         ExitFor
 *     EndIf
 * Next
 * ```
 *
 * ## Complex Nested Control Flow
 * **Before:**
 * ```ssl
 * for outer=1 to 5
 * while condition(outer)
 * if isValid(outer) then
 * for inner=1 to 3
 * processData(outer,inner)
 * next
 * else
 * logError(outer)
 * exitwhile
 * endif
 * endwhile
 * next
 * ```
 *
 * **After:**
 * ```ssl
 * For outer = 1 To 5
 *     While condition(outer)
 *         If IsValid(outer) Then
 *             For inner = 1 To 3
 *                 ProcessData(outer, inner)
 *             Next
 *         Else
 *             LogError(outer)
 *             ExitWhile
 *         EndIf
 *     EndWhile
 * Next
 * ```
 */

import {
    FormatterVisitorBase,
    VisitorResult,
    FormatterOptions,
    defaultFormatterOptions,
} from "./visitor";
import {
    ConditionalStatementNode,
    IfStatementNode,
    ElseStatementNode,
    EndIfStatementNode,
    LoopStatementNode,
    WhileLoopNode,
    WhileStatementNode,
    EndWhileStatementNode,
    ForLoopNode,
    ForStatementNode,
    NextStatementNode,
    ExitWhileStatementNode,
    ExitForStatementNode,
    LoopContinueNode,
} from "../parser/ast/controlFlow";
import {
    SwitchStatementNode,
    BeginCaseStatementNode,
    CaseBlockNode,
    CaseStatementNode,
    OtherwiseBlockNode,
    OtherwiseStatementNode,
    EndCaseStatementNode,
    ExitCaseStatementNode,
} from "../parser/ast/switchCase";
import { ExpressionNode, StatementNode } from "../parser/ast/base";
import { Token } from "../tokenizer/token";

/**
 * SSL Control Flow Formatter Visitor
 *
 * Formats SSL control flow constructs including:
 * - IF/ELSE/ENDIF blocks with proper indentation
 * - WHILE/ENDWHILE loops with proper nesting
 * - FOR/NEXT loops with proper variable and range formatting
 * - Exit statements (EXITWHILE, EXITFOR, LOOP)
 */
export class SSLControlFlowFormatterVisitor extends FormatterVisitorBase {
    constructor(options: FormatterOptions = defaultFormatterOptions) {
        super(options);
    }

    /**
     * Format conditional statement wrapper
     * This acts as a dispatcher for different conditional types
     */
    protected override visitConditionalStatement(node: ConditionalStatementNode): VisitorResult {
        // Conditional statements are wrappers - continue to visit children
        return { shouldContinue: true };
    }

    /**
     * Format IF statement block
     * Follows EBNF: IfStatement ::= ":" "IF" Expression
     */
    protected override visitIfStatement(node: IfStatementNode): VisitorResult {
        // Format :IF condition;
        this.output.writeIndented(":IF ");
        this.formatExpression(node.condition);
        this.output.write(";");
        this.output.writeLine();

        // Indent for body
        this.output.indent();

        // Format then branch (body statements)
        this.formatStatementList(node.thenBranch);

        // Format else branch if present
        if (node.elseBranch) {
            this.output.dedent();
            this.visit(node.elseBranch);
            this.output.indent();
        }

        // Format endif
        this.output.dedent();
        this.visit(node.endIf);

        return { shouldContinue: false }; // We've handled all children manually
    }

    /**
     * Format ELSE statement
     * Follows EBNF: ElseStatement ::= ":" "ELSE"
     */
    protected override visitElseStatement(node: ElseStatementNode): VisitorResult {
        this.output.writeIndented(":ELSE;");
        this.output.writeLine();

        // Indent for else body
        this.output.indent();
        this.formatStatementList(node.body);
        this.output.dedent();

        return { shouldContinue: false }; // We've handled children manually
    }

    /**
     * Format ENDIF statement
     * Follows EBNF: EndIfStatement ::= ":" "ENDIF"
     */
    protected override visitEndIfStatement(node: EndIfStatementNode): VisitorResult {
        this.output.writeIndented(":ENDIF;");
        this.output.writeLine();
        return { shouldContinue: true };
    }

    /**
     * Format loop statement wrapper
     * This acts as a dispatcher for different loop types
     */
    protected override visitLoopStatement(node: LoopStatementNode): VisitorResult {
        // Loop statements are wrappers - continue to visit children
        return { shouldContinue: true };
    }

    /**
     * Format WHILE loop block
     * Follows EBNF: WhileLoop ::= WhileStatement {Statement} EndWhileStatement
     */
    protected override visitWhileLoop(node: WhileLoopNode): VisitorResult {
        // Format the WHILE condition
        this.visit(node.condition);

        // Indent for body
        this.output.indent();

        // Format body statements
        this.formatStatementList(node.body);

        // Dedent and format end
        this.output.dedent();
        this.visit(node.end);

        return { shouldContinue: false }; // We've handled all children manually
    }

    /**
     * Format WHILE statement (condition part)
     * Follows EBNF: WhileStatement ::= ":" "WHILE" Expression
     */
    protected override visitWhileStatement(node: WhileStatementNode): VisitorResult {
        this.output.writeIndented(":WHILE ");
        this.formatExpression(node.condition);
        this.output.write(";");
        this.output.writeLine();
        return { shouldContinue: true };
    }

    /**
     * Format ENDWHILE statement
     * Follows EBNF: EndWhileStatement ::= ":" "ENDWHILE"
     */
    protected override visitEndWhileStatement(node: EndWhileStatementNode): VisitorResult {
        this.output.writeIndented(":ENDWHILE;");
        this.output.writeLine();
        return { shouldContinue: true };
    }

    /**
     * Format FOR loop block
     * Follows EBNF: ForLoop ::= ForStatement {Statement} NextStatement
     */
    protected override visitForLoop(node: ForLoopNode): VisitorResult {
        // Format the FOR declaration
        this.visit(node.declaration);

        // Indent for body
        this.output.indent();

        // Format body statements
        this.formatStatementList(node.body);

        // Dedent and format NEXT
        this.output.dedent();
        this.visit(node.next);

        return { shouldContinue: false }; // We've handled all children manually
    }

    /**
     * Format FOR statement (declaration part)
     * Follows EBNF: ForStatement ::= ":" "FOR" Identifier ":=" Expression ":" "TO" Expression
     */
    protected override visitForStatement(node: ForStatementNode): VisitorResult {
        this.output.writeIndented(":FOR ");

        // Format variable name
        this.output.write(node.variable.value);

        // Add spacing around assignment operator if configured
        if (this.options.insertSpacesAroundAssignmentOperators) {
            this.output.write(" := ");
        } else {
            this.output.write(":=");
        }

        // Format start value
        this.formatExpression(node.startValue);

        this.output.write(" :TO ");

        // Format end value
        this.formatExpression(node.endValue);

        this.output.write(";");
        this.output.writeLine();

        return { shouldContinue: true };
    }

    /**
     * Format NEXT statement
     * Follows EBNF: NextStatement ::= ":" "NEXT"
     */
    protected override visitNextStatement(node: NextStatementNode): VisitorResult {
        this.output.writeIndented(":NEXT;");
        this.output.writeLine();
        return { shouldContinue: true };
    }

    /**
     * Format EXITWHILE statement
     * Follows EBNF: ExitWhileStatement ::= ":" "EXITWHILE"
     */
    protected override visitExitWhileStatement(node: ExitWhileStatementNode): VisitorResult {
        this.output.writeIndented(":EXITWHILE;");
        this.output.writeLine();
        return { shouldContinue: true };
    }

    /**
     * Format EXITFOR statement
     * Follows EBNF: ExitForStatement ::= ":" "EXITFOR"
     */
    protected override visitExitForStatement(node: ExitForStatementNode): VisitorResult {
        this.output.writeIndented(":EXITFOR;");
        this.output.writeLine();
        return { shouldContinue: true };
    }
    /**
     * Format LOOP continue statement
     * Follows EBNF: LoopContinue ::= ":" "LOOP"
     */
    protected override visitLoopContinue(node: LoopContinueNode): VisitorResult {
        this.output.writeIndented(":LOOP;");
        this.output.writeLine();
        return { shouldContinue: true };
    }

    // ===== SWITCH CASE FORMATTING METHODS =====

    /**
     * Format switch statement block
     * Follows EBNF: SwitchStatement ::= BeginCaseStatement {CaseBlock} [OtherwiseBlock] EndCaseStatement
     */
    protected override visitSwitchStatement(node: SwitchStatementNode): VisitorResult {
        // Format :BEGINCASE
        this.output.writeIndented(":BEGINCASE;");
        this.output.writeLine();

        // Indent for case blocks
        this.output.indent();

        // Format all case blocks
        node.cases?.forEach((caseBlock: CaseBlockNode) => {
            this.visit(caseBlock);
        });

        // Format otherwise block if present
        if (node.otherwiseBlock) {
            this.visit(node.otherwiseBlock);
        }

        // Dedent and format :ENDCASE
        this.output.dedent();
        this.output.writeIndented(":ENDCASE;");
        this.output.writeLine();

        return { shouldContinue: false }; // We've handled all children manually
    }

    /**
     * Format BEGINCASE statement
     * Follows EBNF: BeginCaseStatement ::= ":" "BEGINCASE"
     */
    protected override visitBeginCaseStatement(node: BeginCaseStatementNode): VisitorResult {
        this.output.writeIndented(":BEGINCASE;");
        this.output.writeLine();
        return { shouldContinue: true };
    }

    /**
     * Format case block (case condition + body + optional exit)
     * Follows EBNF: CaseBlock ::= CaseStatement {Statement} [ExitCaseStatement]
     */
    protected override visitCaseBlock(node: CaseBlockNode): VisitorResult {
        // Format :CASE expression;
        this.output.writeIndented(":CASE ");
        this.formatExpression(node.condition);
        this.output.write(";");
        this.output.writeLine();

        // Indent for case body
        this.output.indent();

        // Format body statements
        this.formatStatementList(node.statements);

        // Dedent after case block
        this.output.dedent();
        return { shouldContinue: false }; // We've handled all children manually
    }

    /**
     * Format CASE statement (condition part)
     * Follows EBNF: CaseStatement ::= ":" "CASE" Expression
     */
    protected override visitCaseStatement(node: CaseStatementNode): VisitorResult {
        this.output.writeIndented(":CASE ");
        this.formatExpression(node.expression);
        this.output.write(";");
        this.output.writeLine();
        return { shouldContinue: true };
    }

    /**
     * Format OTHERWISE block
     * Follows EBNF: OtherwiseBlock ::= OtherwiseStatement {Statement}
     */
    protected override visitOtherwiseBlock(node: OtherwiseBlockNode): VisitorResult {
        // Format :OTHERWISE;
        this.output.writeIndented(":OTHERWISE;");
        this.output.writeLine();

        // Indent for otherwise body
        this.output.indent();

        // Format body statements
        this.formatStatementList(node.statements);

        // Dedent after otherwise block
        this.output.dedent();

        return { shouldContinue: false }; // We've handled all children manually
    }

    /**
     * Format OTHERWISE statement
     * Follows EBNF: OtherwiseStatement ::= ":" "OTHERWISE"
     */
    protected override visitOtherwiseStatement(node: OtherwiseStatementNode): VisitorResult {
        this.output.writeIndented(":OTHERWISE;");
        this.output.writeLine();
        return { shouldContinue: true };
    }

    /**
     * Format ENDCASE statement
     * Follows EBNF: EndCaseStatement ::= ":" "ENDCASE"
     */
    protected override visitEndCaseStatement(node: EndCaseStatementNode): VisitorResult {
        this.output.writeIndented(":ENDCASE;");
        this.output.writeLine();
        return { shouldContinue: true };
    }
    /**
     * Format EXITCASE statement
     * Follows EBNF: ExitCaseStatement ::= ":" "EXITCASE"
     */
    protected override visitExitCaseStatement(node: ExitCaseStatementNode): VisitorResult {
        this.output.writeIndented(":EXITCASE;");
        this.output.writeLine();
        return { shouldContinue: true };
    }

    /**
     * Helper method to format expressions
     * This delegates to the expression formatter or handles simple cases
     */
    private formatExpression(expression: ExpressionNode): void {
        // For now, we'll use a simple approach - in a full implementation,
        // this would delegate to an expression formatter
        this.visit(expression);
    }

    /**
     * Helper method to format a list of statements
     * Handles proper spacing and indentation between statements
     */
    private formatStatementList(statements: StatementNode[]): void {
        if (!statements || statements.length === 0) {
            return;
        }

        for (let i = 0; i < statements.length; i++) {
            this.visit(statements[i]);

            // Add blank line between logical sections if configured
            if (this.options.preserveBlankLines && i < statements.length - 1) {
                // Simple heuristic: add blank line before control flow statements
                const nextStatement = statements[i + 1];
                if (this.isControlFlowStatement(nextStatement)) {
                    this.output.writeBlankLine();
                }
            }
        }
    }

    /**
     * Helper method to determine if a statement is a control flow statement
     */
    private isControlFlowStatement(statement: StatementNode): boolean {
        return ["IfStatement", "WhileLoop", "ForLoop", "SwitchStatement", "TryBlock"].includes(
            statement.kind
        );
    }
}
