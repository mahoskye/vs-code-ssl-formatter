/**
 * Expression Formatters
 *
 * Formats SSL expressions with proper spacing and operator precedence:
 * - Format binary expressions with proper spacing
 * - Format unary expressions
 * - Format property access (: operator)
 * - Format array access and subscripts
 *
 * # Before/After Formatting Examples
 *
 * ## Binary Expressions
 * **Before:**
 * ```ssl
 * result=a+b*c-d/e
 * condition=value1>=value2and flag<>true
 * ```
 *
 * **After:**
 * ```ssl
 * result = a + b * c - d / e
 * condition = value1 >= value2 And flag <> True
 * ```
 *
 * ## Complex Logical Expressions
 * **Before:**
 * ```ssl
 * if(status="active"or status="pending")and(priority>5)and not isProcessed then
 * ```
 *
 * **After:**
 * ```ssl
 * If (status = "active" Or status = "pending") And (priority > 5) And Not isProcessed Then
 * ```
 *
 * ## Property Access
 * **Before:**
 * ```ssl
 * customer:name=newValue
 * result=object:method(param1,param2)
 * ```
 *
 * **After:**
 * ```ssl
 * customer:name = newValue
 * result = object:method(param1, param2)
 * ```
 *
 * ## Array Access
 * **Before:**
 * ```ssl
 * value=myArray[index+1]
 * matrix[row,col]=newValue
 * ```
 *
 * **After:**
 * ```ssl
 * value = myArray[index + 1]
 * matrix[row, col] = newValue
 * ```
 *
 * ## Function Calls
 * **Before:**
 * ```ssl
 * result=calculateTotal(amount,taxRate,discount)
 * DoProcessData(input1,input2,output)
 * ```
 *
 * **After:**
 * ```ssl
 * result = CalculateTotal(amount, taxRate, discount)
 * DoProcessData(input1, input2, output)
 * ```
 *
 * ## Unary and Increment Expressions
 * **Before:**
 * ```ssl
 * result=-value
 * flag=not condition
 * counter++
 * --index
 * ```
 *
 * **After:**
 * ```ssl
 * result = -value
 * flag = Not condition
 * counter++
 * --index
 * ```
 *
 * ## Object Creation
 * **Before:**
 * ```ssl
 * obj=new MyClass(param1,param2)
 * ```
 *
 * **After:**
 * ```ssl
 * obj = New MyClass(param1, param2)
 * ```
 */

import {
    ASTNodeType,
    ExpressionNode,
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
    LiteralExpressionNode,
    NumberLiteralNode,
    StringLiteralNode,
    BooleanLiteralNode,
    ArrayLiteralNode,
    NilLiteralNode,
    DateLiteralNode,
    CodeBlockLiteralNode,
    MethodCallNode,
    DirectFunctionCallNode,
    DoProcCallNode,
    ExecFunctionCallNode,
    BitwiseOperationNode,
    ObjectCreationNode,
} from "../parser/ast";
import { FormatterVisitorBase, VisitorResult } from "./visitor";
import { TokenType } from "../tokenizer/tokenType";

/**
 * SSL Expression Formatter Visitor
 *
 * Formats SSL expressions according to EBNF grammar:
 * - Binary expressions with proper operator spacing
 * - Unary expressions with consistent formatting
 * - Property access using colon notation (object:property)
 * - Array access supporting both comma and bracket notation
 * - Function calls and method calls
 * - Literal expressions with proper formatting
 */
export class SSLExpressionFormatterVisitor extends FormatterVisitorBase {
    // ================================
    // Expression Hierarchy Formatting
    // ================================

    /**
     * Format logical expressions (.AND., .OR.)
     * Follows EBNF: LogicalExpression ::= ComparisonExpression {LogicalOperator ComparisonExpression}
     */
    protected override visitLogicalExpression(node: LogicalExpressionNode): VisitorResult {
        return this.formatBinaryExpression(node, true);
    }

    /**
     * Format comparison expressions (==, !=, <, >, <=, >=, =)
     * Follows EBNF: ComparisonExpression ::= ArithmeticExpression {ComparisonOperator ArithmeticExpression}
     */
    protected override visitComparisonExpression(node: ComparisonExpressionNode): VisitorResult {
        return this.formatBinaryExpression(
            node,
            this.options.insertSpacesAroundComparisonOperators
        );
    }

    /**
     * Format arithmetic expressions (+, -)
     * Follows EBNF: ArithmeticExpression ::= Term {AdditiveOperator Term}
     */
    protected override visitArithmeticExpression(node: ArithmeticExpressionNode): VisitorResult {
        return this.formatBinaryExpression(node, this.options.insertSpacesAroundOperators);
    }
    /**
     * Format term expressions (*, /, %)
     * Follows EBNF: Term ::= Factor {MultiplicativeOperator Factor}
     */
    protected override visitTerm(node: TermNode): VisitorResult {
        // Term nodes are binary expressions
        return this.formatBinaryExpression(node as any, this.options.insertSpacesAroundOperators);
    }
    /**
     * Format factor expressions (^)
     * Follows EBNF: Factor ::= PowerOperand {"^" PowerOperand}
     */
    protected override visitFactor(node: FactorNode): VisitorResult {
        // Factor nodes are binary expressions
        return this.formatBinaryExpression(node as any, this.options.insertSpacesAroundOperators);
    }
    /**
     * Format power operand expressions
     * Follows EBNF: PowerOperand ::= [UnaryOperator] Primary
     */
    protected override visitPowerOperand(node: PowerOperandNode): VisitorResult {
        // PowerOperand may have an optional unary operator followed by a primary expression
        const unaryOperator = (node as any).unaryOperator;
        const primary = (node as any).primary;

        // Format optional unary operator
        if (unaryOperator) {
            this.output.write(unaryOperator.value);
            // Add space after .NOT. but not after other unary operators like - or +
            if (unaryOperator.value === ".NOT.") {
                this.output.write(" ");
            }
        }

        // Format the primary expression
        if (primary) {
            this.visit(primary);
        }

        return { shouldContinue: false };
    }

    /**
     * Format general binary expressions
     * Handles spacing around operators based on configuration
     */
    protected override visitBinaryExpression(node: BinaryExpressionNode): VisitorResult {
        // Determine spacing based on operator type
        const needsSpacing = this.shouldAddSpacingForOperator(node.operator.type);
        return this.formatBinaryExpression(node, needsSpacing);
    }

    // ================================
    // Unary and Prefix/Postfix
    // ================================

    /**
     * Format unary expressions (!, .NOT., +, -)
     * Follows EBNF: UnaryExpression ::= UnaryOperator Primary
     */
    protected override visitUnaryExpression(node: UnaryExpressionNode): VisitorResult {
        const operatorValue = (node as any).operator?.value || "";

        // Write the operator
        this.output.write(operatorValue);

        // For .NOT., add a space after the operator
        if (operatorValue === ".NOT." || operatorValue === "!") {
            this.output.write(" ");
        }

        // Format the operand
        if ((node as any).operand) {
            this.visit((node as any).operand);
        }

        return { shouldContinue: false };
    }

    /**
     * Format increment/decrement expressions (++, --)
     * Handles both prefix (++var) and postfix (var++) forms
     */
    protected override visitIncrementExpression(node: IncrementExpressionNode): VisitorResult {
        const isPrefix = (node as any).prefix || false;
        const operator = (node as any).operator?.value || "++";
        const operand = (node as any).operand;

        if (isPrefix) {
            // Prefix: ++variable
            this.output.write(operator);
            if (operand?.value) {
                this.output.write(operand.value);
            }
        } else {
            // Postfix: variable++
            if (operand?.value) {
                this.output.write(operand.value);
            }
            this.output.write(operator);
        }

        return { shouldContinue: false };
    }

    // ================================
    // Access Expressions
    // ================================

    /**
     * Format variable access
     * Simple identifier formatting with Hungarian notation support
     */
    protected override visitVariableAccess(node: VariableAccessNode): VisitorResult {
        const name = (node as any).name?.value || "";
        this.output.write(name);
        return { shouldContinue: false };
    }

    /**
     * Format property access (object:property)
     * SSL uses colon for property access, not dot
     */
    protected override visitPropertyAccess(node: PropertyAccessNode): VisitorResult {
        const objectName = (node as any).object?.value || "";
        const propertyName = (node as any).property?.value || "";

        this.output.write(objectName);

        if (this.options.insertSpacesAroundPropertyAccess) {
            this.output.write(" : ");
        } else {
            this.output.write(":");
        }

        this.output.write(propertyName);

        return { shouldContinue: false };
    }

    /**
     * Format array access with proper bracket spacing
     * Supports both arr[1,2] and arr[1][2] notations
     */
    protected override visitArrayAccess(node: ArrayAccessNode): VisitorResult {
        // Format the array expression first
        const arrayExpr = (node as any).array;
        if (arrayExpr) {
            this.visit(arrayExpr);
        }

        // Format the indices
        const indices = (node as any).indices || [];
        if (indices.length > 0) {
            this.output.write("[");

            for (let i = 0; i < indices.length; i++) {
                if (i > 0) {
                    this.output.write(",");
                    if (this.options.insertSpacesAfterCommas) {
                        this.output.write(" ");
                    }
                }
                this.visit(indices[i]);
            }

            this.output.write("]");
        }

        return { shouldContinue: false };
    }

    /**
     * Format array subscript dimensions
     */
    protected override visitArraySubscript(node: ArraySubscriptNode): VisitorResult {
        const dimensions = node.dimensions || [];

        this.output.write("[");
        for (let i = 0; i < dimensions.length; i++) {
            if (i > 0) {
                this.output.write(",");
                if (this.options.insertSpacesAfterCommas) {
                    this.output.write(" ");
                }
            }
            this.visit(dimensions[i]);
        }
        this.output.write("]");

        return { shouldContinue: false };
    }

    // ================================
    // Literal Expressions
    // ================================

    /**
     * Format literal expressions (wrapper for specific literals)
     */
    protected override visitLiteralExpression(node: LiteralExpressionNode): VisitorResult {
        const token = (node as any).token;
        if (token) {
            this.output.write(token.value);
        } else {
            const value = (node as any).value;
            this.output.write(String(value));
        }
        return { shouldContinue: false };
    }

    /**
     * Format number literals with proper scientific notation support
     */
    protected override visitNumberLiteral(node: NumberLiteralNode): VisitorResult {
        const raw = (node as any).raw || String((node as any).value || "0");
        this.output.write(raw);
        return { shouldContinue: false };
    }

    /**
     * Format string literals preserving quotes
     */
    protected override visitStringLiteral(node: StringLiteralNode): VisitorResult {
        const token = (node as any).token;
        if (token) {
            this.output.write(token.value); // Includes quotes
        } else {
            const value = (node as any).value || "";
            this.output.write(`"${value}"`);
        }
        return { shouldContinue: false };
    }

    /**
     * Format boolean literals (.T., .F.)
     */
    protected override visitBooleanLiteral(node: BooleanLiteralNode): VisitorResult {
        const value = (node as any).value;
        this.output.write(value ? ".T." : ".F.");
        return { shouldContinue: false };
    }

    /**
     * Format NIL literal
     */
    protected override visitNilLiteral(node: NilLiteralNode): VisitorResult {
        this.output.write("NIL");
        return { shouldContinue: false };
    }

    /**
     * Format array literals with proper spacing
     */
    protected override visitArrayLiteral(node: ArrayLiteralNode): VisitorResult {
        const elements = (node as any).elements || [];

        this.output.write("{");
        const shouldBreakLongArray =
            this.options.breakLongArrayLiterals &&
            elements.length > this.options.parameterListBreakThreshold;

        if (shouldBreakLongArray) {
            this.output.writeLine();
            this.output.indent();
        }

        for (let i = 0; i < elements.length; i++) {
            if (i > 0) {
                this.output.write(",");
                if (shouldBreakLongArray) {
                    this.output.writeLine();
                } else if (this.options.insertSpacesAfterCommas) {
                    this.output.write(" ");
                }
            }

            if (shouldBreakLongArray) {
                this.output.writeIndented("");
            }

            this.visit(elements[i]);
        }

        if (shouldBreakLongArray) {
            this.output.writeLine();
            this.output.dedent();
            this.output.writeIndented("}");
        } else {
            this.output.write("}");
        }

        return { shouldContinue: false };
    }

    /**
     * Format date literals {year, month, day[, hour, minute, second]}
     */
    protected override visitDateLiteral(node: DateLiteralNode): VisitorResult {
        const components = (node as any).components || [];

        this.output.write("{");
        for (let i = 0; i < components.length; i++) {
            if (i > 0) {
                this.output.write(",");
                if (this.options.insertSpacesAfterCommas) {
                    this.output.write(" ");
                }
            }
            this.visit(components[i]);
        }
        this.output.write("}");

        return { shouldContinue: false };
    }

    /**
     * Format code block literals {|params| expressions}
     */
    protected override visitCodeBlockLiteral(node: CodeBlockLiteralNode): VisitorResult {
        this.output.write("{|");

        // Format parameters
        const parameters = (node as any).parameters;
        if (parameters && parameters.identifiers) {
            for (let i = 0; i < parameters.identifiers.length; i++) {
                if (i > 0) {
                    this.output.write(",");
                    if (this.options.insertSpacesAfterCommas) {
                        this.output.write(" ");
                    }
                }
                this.output.write(parameters.identifiers[i].value);
            }
        }

        this.output.write("|");

        // Format body expressions
        const body = (node as any).body || [];
        for (let i = 0; i < body.length; i++) {
            if (i > 0) {
                this.output.write(",");
                if (this.options.insertSpacesAfterCommas) {
                    this.output.write(" ");
                }
            }
            this.visit(body[i]);
        }

        this.output.write("}");

        return { shouldContinue: false };
    }

    // ================================
    // Function and Method Calls
    // ================================

    /**
     * Format direct function calls
     */
    protected override visitDirectFunctionCall(node: DirectFunctionCallNode): VisitorResult {
        const name = (node as any).name?.value || "";
        const args = (node as any).arguments;

        this.output.write(name);
        this.output.write("(");

        if (args && args.arguments) {
            this.formatArgumentList(args.arguments);
        }

        this.output.write(")");
        return { shouldContinue: false };
    }

    /**
     * Format DoProc calls
     */
    protected override visitDoProcCall(node: DoProcCallNode): VisitorResult {
        this.output.write("DoProc(");

        const procName = (node as any).procedureName;
        const args = (node as any).arguments;

        if (procName) {
            this.visit(procName);
        }

        if (args) {
            this.output.write(",");
            if (this.options.insertSpacesAfterCommas) {
                this.output.write(" ");
            }
            this.visit(args);
        }

        this.output.write(")");
        return { shouldContinue: false };
    }

    /**
     * Format ExecFunction calls
     */
    protected override visitExecFunctionCall(node: ExecFunctionCallNode): VisitorResult {
        this.output.write("ExecFunction(");

        const funcName = (node as any).functionName;
        const args = (node as any).arguments;

        if (funcName) {
            this.visit(funcName);
        }

        if (args) {
            this.output.write(",");
            if (this.options.insertSpacesAfterCommas) {
                this.output.write(" ");
            }
            this.visit(args);
        }

        this.output.write(")");
        return { shouldContinue: false };
    }

    /**
     * Format method calls (object:method())
     */
    protected override visitMethodCall(node: MethodCallNode): VisitorResult {
        const objectName = (node as any).object?.value || "";
        const methodName = (node as any).method?.value || "";
        const args = (node as any).arguments;

        this.output.write(objectName);

        if (this.options.insertSpacesAroundPropertyAccess) {
            this.output.write(" : ");
        } else {
            this.output.write(":");
        }

        this.output.write(methodName);
        this.output.write("(");

        if (args && args.arguments) {
            this.formatArgumentList(args.arguments);
        }

        this.output.write(")");
        return { shouldContinue: false };
    }

    /**
     * Format bitwise operations (_AND, _OR, _NOT)
     */
    protected override visitBitwiseOperation(node: BitwiseOperationNode): VisitorResult {
        const operation = (node as any).operation || "_AND";
        const operands = (node as any).operands || [];

        this.output.write(operation);
        this.output.write("(");

        for (let i = 0; i < operands.length; i++) {
            if (i > 0) {
                this.output.write(",");
                if (this.options.insertSpacesAfterCommas) {
                    this.output.write(" ");
                }
            }
            this.visit(operands[i]);
        }

        this.output.write(")");
        return { shouldContinue: false };
    }

    /**
     * Format object creation calls
     */
    protected override visitObjectCreation(node: ObjectCreationNode): VisitorResult {
        this.output.write("CreateUDObject(");

        const className = (node as any).className;
        if (className) {
            this.visit(className);
        }

        this.output.write(")");
        return { shouldContinue: false };
    }

    // ================================
    // Helper Methods
    // ================================

    /**
     * Format a binary expression with consistent spacing
     */
    private formatBinaryExpression(node: BinaryExpressionNode, addSpacing: boolean): VisitorResult {
        const left = (node as any).left;
        const operator = (node as any).operator;
        const right = (node as any).right;

        // Format left operand
        if (left) {
            this.visit(left);
        }

        // Format operator with spacing
        if (operator) {
            if (addSpacing) {
                this.output.write(" ");
            }
            this.output.write(operator.value);
            if (addSpacing) {
                this.output.write(" ");
            }
        }

        // Format right operand
        if (right) {
            this.visit(right);
        }

        return { shouldContinue: false };
    }

    /**
     * Format a list of arguments with proper comma spacing
     */
    private formatArgumentList(args: ExpressionNode[]): void {
        for (let i = 0; i < args.length; i++) {
            if (i > 0) {
                this.output.write(",");
                if (this.options.insertSpacesAfterCommas) {
                    this.output.write(" ");
                }
            }

            // Handle empty parameters (for skipped parameters like param1,,param3)
            if (args[i]) {
                this.visit(args[i]);
            }
        }
    }

    /**
     * Determine if spacing should be added around an operator
     */
    private shouldAddSpacingForOperator(tokenType: TokenType): boolean {
        switch (tokenType) {
            case TokenType.AND:
            case TokenType.OR:
                return this.options.insertSpacesAroundLogicalOperators;

            case TokenType.EQUAL:
            case TokenType.STRICT_EQUAL:
            case TokenType.NOT_EQUAL:
            case TokenType.LESS_THAN:
            case TokenType.GREATER_THAN:
            case TokenType.LESS_EQUAL:
            case TokenType.GREATER_EQUAL:
                return this.options.insertSpacesAroundComparisonOperators;
            case TokenType.ASSIGN:
            case TokenType.PLUS_ASSIGN:
            case TokenType.MINUS_ASSIGN:
            case TokenType.MULT_ASSIGN:
            case TokenType.DIV_ASSIGN:
            case TokenType.POWER_ASSIGN:
                return this.options.insertSpacesAroundAssignmentOperators;

            case TokenType.PLUS:
            case TokenType.MINUS:
            case TokenType.MULTIPLY:
            case TokenType.DIVIDE:
            case TokenType.MODULO:
            case TokenType.POWER:
                return this.options.insertSpacesAroundOperators;

            default:
                return this.options.insertSpacesAroundOperators;
        }
    }

    /**
     * Format primary expressions (identifiers, literals, parenthesized expressions)
     * Primary is the lowest level in the expression hierarchy
     */
    protected override visitPrimary(node: PrimaryNode): VisitorResult {
        // Primary nodes are the base case in expression parsing
        // They can be identifiers, literals, or parenthesized expressions
        const value = (node as any).value;
        if (value) {
            this.output.write(String(value));
        }
        return { shouldContinue: false };
    }
}
