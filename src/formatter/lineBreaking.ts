/**
 * Line Breaking Manager
 *
 * Handles line breaking decisions for SSL code formatting:
 * - Determine when to break long lines
 * - Handle line breaking in function calls
 * - Handle line breaking in expressions
 * - Handle line breaking in array literals
 * - Handle line breaking in SQL statements
 */

import { FormatterOptions } from "./options";
import { OutputBuilder } from "./outputBuilder";
import {
    ASTNode,
    ExpressionNode,
    ArgumentListNode,
    ArrayLiteralNode,
    DirectFunctionCallNode,
    DoProcCallNode,
    ExecFunctionCallNode,
    StringLiteralNode,
} from "../parser/ast";

/**
 * Information about a breakable construct (function call, array, etc.)
 */
export interface BreakableContext {
    /** Type of breakable construct */
    type: "function-call" | "array-literal" | "parameter-list" | "sql-statement";
    /** Total length if rendered on single line */
    singleLineLength: number;
    /** Elements that can be broken across lines */
    elements: ExpressionNode[];
    /** Opening bracket/parenthesis character */
    openChar: string;
    /** Closing bracket/parenthesis character */
    closeChar: string;
    /** Separator between elements (usually comma) */
    separator: string;
    /** Current indentation level */
    indentLevel: number;
}

/**
 * Strategy for breaking long constructs
 */
export type BreakingStrategy =
    | "no-break" // Keep on single line
    | "all-break" // Break all elements
    | "smart-break"; // Break only when beneficial

/**
 * Line Breaking Manager for SSL formatting
 */
export class SSLLineBreaker {
    private readonly options: FormatterOptions;

    constructor(options: FormatterOptions) {
        this.options = options;
    }

    /**
     * Determine if a line should be broken based on length
     */
    shouldBreakLine(currentLength: number, additionalContent: string = ""): boolean {
        const totalLength = currentLength + additionalContent.length;
        return totalLength > this.options.maxLineLength;
    }

    /**
     * Calculate the approximate length of text if rendered on a single line
     */
    calculateSingleLineLength(text: string): number {
        // Remove extra whitespace and calculate actual rendered length
        return text.replace(/\s+/g, " ").trim().length;
    }

    /**
     * Determine breaking strategy for a function call
     */
    getFunctionCallBreakingStrategy(
        functionName: string,
        argumentCount: number,
        estimatedLength: number
    ): BreakingStrategy {
        // Always break if estimated length exceeds max line length
        if (estimatedLength > this.options.maxLineLength) {
            return "all-break";
        }

        // Break if argument count exceeds threshold
        if (argumentCount >= this.options.parameterListBreakThreshold) {
            return "smart-break";
        }

        // Special handling for SQL functions
        if (this.isSqlFunction(functionName) && this.options.breakLongSqlStatements) {
            return "smart-break";
        }

        return "no-break";
    }

    /**
     * Determine breaking strategy for an array literal
     */
    getArrayLiteralBreakingStrategy(
        elementCount: number,
        estimatedLength: number
    ): BreakingStrategy {
        if (!this.options.breakLongArrayLiterals) {
            return "no-break";
        }

        if (estimatedLength > this.options.maxLineLength) {
            return "all-break";
        }

        // Break arrays with many elements for readability
        if (elementCount >= 5) {
            return "smart-break";
        }

        return "no-break";
    }

    /**
     * Format a function call with appropriate line breaking
     */
    formatFunctionCall(
        output: OutputBuilder,
        functionName: string,
        args: ExpressionNode[],
        renderElement: (element: ExpressionNode) => string
    ): void {
        const renderedArgs = args.map(renderElement);
        const singleLineContent = `${functionName}(${renderedArgs.join(", ")})`;
        const strategy = this.getFunctionCallBreakingStrategy(
            functionName,
            args.length,
            this.calculateSingleLineLength(singleLineContent)
        );

        if (strategy === "no-break") {
            output.write(singleLineContent);
            return;
        }

        // Multi-line function call
        output.write(`${functionName}(`);

        if (args.length > 0) {
            const shouldAlignArgs = this.options.alignProcedureParameters;

            if (shouldAlignArgs) {
                this.formatAlignedArguments(output, renderedArgs, functionName.length + 1);
            } else {
                this.formatIndentedArguments(output, renderedArgs);
            }
        }

        output.write(")");
    }

    /**
     * Format arguments aligned with opening parenthesis
     */
    private formatAlignedArguments(
        output: OutputBuilder,
        renderedArgs: string[],
        alignmentColumn: number
    ): void {
        const alignment = " ".repeat(alignmentColumn);

        for (let i = 0; i < renderedArgs.length; i++) {
            if (i === 0) {
                output.write(renderedArgs[i]);
            } else {
                output.endLine();
                output.write(alignment + renderedArgs[i]);
            }

            if (i < renderedArgs.length - 1) {
                output.write(",");
                if (this.options.insertSpacesAfterCommas) {
                    output.write(" ");
                }
            }
        }
    }

    /**
     * Format arguments with standard indentation
     */
    private formatIndentedArguments(output: OutputBuilder, renderedArgs: string[]): void {
        output.indent();

        for (let i = 0; i < renderedArgs.length; i++) {
            output.endLine();
            output.writeIndented(renderedArgs[i]);

            if (i < renderedArgs.length - 1) {
                output.write(",");
            }
        }

        output.dedent();
        output.endLine();
    }

    /**
     * Format an array literal with appropriate line breaking
     */
    formatArrayLiteral(
        output: OutputBuilder,
        elements: ExpressionNode[],
        renderElement: (element: ExpressionNode) => string
    ): void {
        const renderedElements = elements.map(renderElement);
        const singleLineContent = `{${renderedElements.join(", ")}}`;
        const strategy = this.getArrayLiteralBreakingStrategy(
            elements.length,
            this.calculateSingleLineLength(singleLineContent)
        );

        if (strategy === "no-break") {
            output.write(singleLineContent);
            return;
        }

        // Multi-line array literal
        output.write("{");

        if (elements.length > 0) {
            if (this.options.alignArrayElements) {
                this.formatAlignedArrayElements(output, renderedElements);
            } else {
                this.formatIndentedArrayElements(output, renderedElements);
            }
        }

        output.write("}");
    }

    /**
     * Format array elements aligned with opening brace
     */
    private formatAlignedArrayElements(output: OutputBuilder, renderedElements: string[]): void {
        const alignment = " "; // Single space after opening brace

        for (let i = 0; i < renderedElements.length; i++) {
            if (i === 0) {
                output.write(alignment + renderedElements[i]);
            } else {
                output.write(",");
                if (this.options.insertSpacesAfterCommas) {
                    output.write(" ");
                }
                output.endLine();
                output.write(alignment + renderedElements[i]);
            }
        }

        output.write(" ");
    }

    /**
     * Format array elements with standard indentation
     */
    private formatIndentedArrayElements(output: OutputBuilder, renderedElements: string[]): void {
        output.indent();

        for (let i = 0; i < renderedElements.length; i++) {
            output.endLine();
            output.writeIndented(renderedElements[i]);

            if (i < renderedElements.length - 1) {
                output.write(",");
            }
        }

        output.dedent();
        output.endLine();
    }

    /**
     * Format SQL statement with line breaking
     */
    formatSqlStatement(
        output: OutputBuilder,
        sqlContent: string,
        preserveOriginalFormatting: boolean = false
    ): void {
        if (!this.options.breakLongSqlStatements || preserveOriginalFormatting) {
            output.write(sqlContent);
            return;
        }

        const processedSql = this.processSqlForLineBreaking(sqlContent);

        if (this.shouldBreakLine(0, processedSql)) {
            this.formatMultiLineSql(output, processedSql);
        } else {
            output.write(processedSql);
        }
    }

    /**
     * Process SQL string for potential line breaking
     */
    private processSqlForLineBreaking(sqlContent: string): string {
        // Basic SQL keyword detection and formatting
        const sqlKeywords = [
            "SELECT",
            "FROM",
            "WHERE",
            "ORDER BY",
            "GROUP BY",
            "HAVING",
            "INSERT",
            "UPDATE",
            "DELETE",
            "JOIN",
            "LEFT JOIN",
            "RIGHT JOIN",
            "INNER JOIN",
            "OUTER JOIN",
            "UNION",
            "EXCEPT",
            "INTERSECT",
        ];

        let processed = sqlContent;

        // Add potential break points before major SQL clauses
        sqlKeywords.forEach((keyword) => {
            const regex = new RegExp(`\\b${keyword}\\b`, "gi");
            processed = processed.replace(regex, `\n${keyword}`);
        });

        return processed.trim();
    }

    /**
     * Format multi-line SQL statement
     */
    private formatMultiLineSql(output: OutputBuilder, sqlContent: string): void {
        const lines = sqlContent.split("\n").filter((line) => line.trim().length > 0);

        output.indent();
        for (let i = 0; i < lines.length; i++) {
            if (i > 0) {
                output.endLine();
            }
            output.writeIndented(lines[i].trim());
        }
        output.dedent();
    }

    /**
     * Check if a function name represents a SQL-related function
     */
    private isSqlFunction(functionName: string): boolean {
        const sqlFunctions = [
            "SqlExecute",
            "LSearch",
            "SqlExecDirect",
            "SqlPrepare",
            "SqlFetch",
            "SqlConnect",
            "SqlDisconnect",
            "SqlCommit",
            "SqlRollback",
        ];
        return sqlFunctions.includes(functionName);
    }

    /**
     * Format a complex expression with potential line breaking
     */ formatComplexExpression(
        output: OutputBuilder,
        expressionParts: string[],
        operators: string[]
    ): void {
        if (expressionParts.length === 0) {
            return;
        }
        if (expressionParts.length === 1) {
            output.write(expressionParts[0]);
            return;
        }

        // Calculate single-line length
        let singleLineContent = expressionParts[0];
        for (let i = 0; i < operators.length; i++) {
            singleLineContent += ` ${operators[i]} ${expressionParts[i + 1]}`;
        }

        if (!this.shouldBreakLine(0, singleLineContent)) {
            output.write(singleLineContent);
            return;
        }

        // Multi-line expression
        output.write(expressionParts[0]);
        output.indent();

        for (let i = 0; i < operators.length; i++) {
            output.endLine();
            output.writeIndented(`${operators[i]} ${expressionParts[i + 1]}`);
        }

        output.dedent();
    }

    /**
     * Estimate the length of a rendered expression
     */
    estimateExpressionLength(expression: ExpressionNode): number {
        // This is a simplified estimation - in a real implementation,
        // you would traverse the expression tree and calculate actual lengths
        return 20; // Placeholder estimation
    }

    /**
     * Check if an expression should be broken across lines
     */
    shouldBreakExpression(expression: ExpressionNode): boolean {
        const estimatedLength = this.estimateExpressionLength(expression);
        return this.shouldBreakLine(0, " ".repeat(estimatedLength));
    }
}
