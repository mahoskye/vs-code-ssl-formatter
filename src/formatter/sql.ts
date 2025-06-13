/**
 * SQL Formatters
 *
 * - Format SqlExecute calls with Oracle SQL support
 * - Format LSearch calls with proper Oracle syntax handling
 * - Handle SQL parameter formatting (?param?, ?)
 * - Oracle-specific formatting for PL/SQL blocks, functions, and procedures
 * - Support for both Oracle and standard SQL syntax
 */

import {
    ASTNodeType,
    SqlStatementNode,
    SqlExecuteNode,
    LSearchNode,
    SqlParameterNode,
    StringLiteralNode,
    ArrayLiteralNode,
    ExpressionNode,
} from "../parser/ast";
import { FormatterVisitorBase, VisitorResult } from "./visitor";
import { TokenType } from "../tokenizer/tokenType";

/**
 * SSL SQL Formatter Visitor
 *
 * Formats SQL-related SSL constructs according to EBNF grammar with Oracle SQL support:
 * - SqlExecute function calls with proper parameter formatting
 * - LSearch function calls with proper parameter alignment
 * - SQL parameter placeholders (?param?, ?)
 * - Multi-line SQL query formatting within string literals
 * - Oracle-specific constructs (PL/SQL blocks, Oracle functions, hints, etc.)
 * - Standard SQL formatting with Oracle conventions
 */
export class SSLSqlFormatterVisitor extends FormatterVisitorBase {
    // ================================
    // SQL Statement Formatting
    // ================================

    /**
     * Format SQL statement wrapper
     * Follows EBNF: SqlStatement ::= SqlExecute | LSearch
     */
    protected override visitSqlStatement(node: SqlStatementNode): VisitorResult {
        // SqlStatement is just a wrapper/union type, not a real node with children
        // The actual SQL nodes (SqlExecute, LSearch) should be visited directly
        return { shouldContinue: false };
    }

    /**
     * Format SqlExecute function calls
     * Follows EBNF: SqlExecute ::= "SqlExecute" "(" StringLiteral ["," ArrayLiteral] ")"
     * Format: SqlExecute("SQL query", [parameters])
     */
    protected override visitSqlExecute(node: SqlExecuteNode): VisitorResult {
        this.output.write("SqlExecute");
        this.output.write("(");

        // Format the SQL query string
        if (node.query) {
            this.formatSqlQueryString(node.query);
        }

        // Format optional parameters array
        if (node.parameters) {
            this.output.write(",");
            if (this.options.insertSpacesAfterCommas) {
                this.output.write(" ");
            }
            this.formatSqlParametersArray(node.parameters);
        }

        this.output.write(")");

        return { shouldContinue: false };
    }
    /**
     * Format LSearch function calls
     * Follows EBNF: LSearch ::= "LSearch" "(" StringLiteral ["," Expression] ["," Expression] ["," ArrayLiteral] ")"
     * Format: LSearch("SQL query", param1, param2, [parameters])
     */
    protected override visitLSearch(node: LSearchNode): VisitorResult {
        this.output.write("LSearch");
        this.output.write("(");

        // Format the SQL query string
        if (node.query) {
            this.formatSqlQueryString(node.query);
        }

        // Format optional parameter1
        if (node.parameter1) {
            this.output.write(",");
            if (this.options.insertSpacesAfterCommas) {
                this.output.write(" ");
            }
            this.visit(node.parameter1);
        }

        // Format optional parameter2
        if (node.parameter2) {
            this.output.write(",");
            if (this.options.insertSpacesAfterCommas) {
                this.output.write(" ");
            }
            this.visit(node.parameter2);
        }

        // Format optional parameters array
        if (node.parameters) {
            this.output.write(",");
            if (this.options.insertSpacesAfterCommas) {
                this.output.write(" ");
            }
            this.formatSqlParametersArray(node.parameters);
        }

        this.output.write(")");

        return { shouldContinue: false };
    }

    /**
     * Format SQL parameter placeholders
     * Supports both ?param? and ? formats
     */
    protected override visitSqlParameter(node: SqlParameterNode): VisitorResult {
        // SQL parameters are typically handled within string literals
        // This method is for standalone parameter nodes if they exist
        const parameterName = (node as any).parameterName;

        if (parameterName) {
            // Named parameter: ?paramName?
            this.output.write("?");
            this.output.write(parameterName.value);
            this.output.write("?");
        } else {
            // Unnamed parameter: ?
            this.output.write("?");
        }

        return { shouldContinue: false };
    }

    // ================================
    // SQL Query String Formatting
    // ================================

    /**
     * Format SQL query strings with proper indentation and line breaks
     * Handles multi-line SQL queries within string literals
     */
    private formatSqlQueryString(queryNode: StringLiteralNode): void {
        const queryString = queryNode.startToken.value;

        // Remove surrounding quotes to work with the actual SQL content
        const quote = queryString.charAt(0);
        const sqlContent = queryString.slice(1, -1);

        // Check if this is a multi-line SQL query or complex query that should be formatted
        if (this.shouldFormatSqlAsMultiLine(sqlContent)) {
            this.formatMultiLineSqlQuery(sqlContent, quote);
        } else {
            // Simple single-line query - just write as-is
            this.output.write(queryString);
        }
    }
    /**
     * Determine if SQL query should be formatted as multi-line
     * Enhanced with Oracle-specific patterns
     */
    private shouldFormatSqlAsMultiLine(sqlContent: string): boolean {
        // Check if the query contains SQL keywords that suggest complexity
        const multiLineIndicators = [
            // Standard SQL patterns
            /\bSELECT\b.*\bFROM\b.*\bWHERE\b/i,
            /\bJOIN\b/i,
            /\bUNION\b/i,
            /\bGROUP\s+BY\b/i,
            /\bORDER\s+BY\b/i,
            /\bHAVING\b/i,
            // Oracle-specific patterns
            /\bWITH\b.*\bAS\b/i, // Common Table Expressions (CTEs)
            /\bCONNECT\s+BY\b/i, // Hierarchical queries
            /\bSTART\s+WITH\b/i, // Hierarchical queries
            /\bPARTITION\s+BY\b/i, // Window functions
            /\bOVER\s*\(/i, // Window functions
            /\bCASE\b.*\bWHEN\b.*\bEND\b/i, // Complex CASE statements
            /\bDECODE\s*\(/i, // Oracle DECODE function
            /\bNVL\s*\(/i, // Oracle NVL function
            /\bROWNUM\b/i, // Oracle ROWNUM
            /\/\*\+.*\*\//i, // Oracle hints
            /\bMERGE\b.*\bINTO\b/i, // MERGE statements
            /\bBEGIN\b.*\bEND\b/i, // PL/SQL blocks
        ];

        // Check length threshold (Oracle queries tend to be longer)
        if (sqlContent.length > this.options.maxLineLength * 0.5) {
            return true;
        }

        // Check for complex SQL patterns
        return multiLineIndicators.some((pattern) => pattern.test(sqlContent));
    }

    /**
     * Format multi-line SQL queries with proper indentation
     */
    private formatMultiLineSqlQuery(sqlContent: string, quote: string): void {
        this.output.write(quote);

        // Split SQL into logical parts for formatting
        const sqlParts = this.parseSqlQuery(sqlContent);

        if (sqlParts.length > 1) {
            // Multi-line formatting
            this.output.writeLine();
            this.output.indent();

            for (let i = 0; i < sqlParts.length; i++) {
                const part = sqlParts[i];
                if (i > 0) {
                    this.output.writeLine();
                }
                this.output.writeIndented(part.trim());
            }

            this.output.dedent();
            this.output.writeLine();
            this.output.writeIndented(quote);
        } else {
            // Single line but long - keep inline
            this.output.write(sqlContent);
            this.output.write(quote);
        }
    }
    /**
     * Parse SQL query into logical parts for formatting
     * Enhanced with Oracle-specific keywords and constructs
     */
    private parseSqlQuery(sqlContent: string): string[] {
        // Enhanced SQL parsing for common clauses including Oracle-specific constructs
        const sqlKeywords = [
            // Standard SQL keywords
            "SELECT",
            "FROM",
            "WHERE",
            "JOIN",
            "INNER JOIN",
            "LEFT JOIN",
            "RIGHT JOIN",
            "FULL JOIN",
            "CROSS JOIN",
            "GROUP BY",
            "ORDER BY",
            "HAVING",
            "UNION",
            "UNION ALL",
            "EXCEPT",
            "INTERSECT",
            "INSERT",
            "UPDATE",
            "DELETE",
            "MERGE",
            // Oracle-specific keywords
            "WITH",
            "CONNECT BY",
            "START WITH",
            "PARTITION BY",
            "OVER",
            "CASE",
            "WHEN",
            "THEN",
            "ELSE",
            "END",
            "BEGIN",
            "DECLARE",
            "EXCEPTION",
            "USING",
            "ON",
            "MATCHED",
            "NOT MATCHED",
        ];

        // Simple split approach to avoid regex state issues
        const parts: string[] = [];
        let currentPart = sqlContent;

        // Find the first occurrence of any keyword
        for (const keyword of sqlKeywords) {
            const regex = new RegExp(`\\b${keyword}\\b`, "i");
            const match = regex.exec(currentPart);
            if (match) {
                const beforeKeyword = currentPart.slice(0, match.index).trim();
                const fromKeyword = currentPart.slice(match.index).trim();

                if (beforeKeyword) {
                    parts.push(beforeKeyword);
                }
                if (fromKeyword) {
                    parts.push(fromKeyword);
                }
                break;
            }
        }

        // If no keywords found, return the original content
        if (parts.length === 0) {
            parts.push(sqlContent.trim());
        }

        return parts.filter((part) => part.length > 0);
    }

    // ================================
    // Parameter Array Formatting
    // ================================

    /**
     * Format SQL parameter arrays with proper spacing and alignment
     */
    private formatSqlParametersArray(parametersNode: ArrayLiteralNode): void {
        this.visit(parametersNode);
    }

    // ================================
    // SQL Parameter Processing
    // ================================

    /**
     * Check if a string contains SQL parameter placeholders
     */
    private containsSqlParameters(sqlContent: string): boolean {
        // Check for ?param? or standalone ? patterns
        return /\?(\w+\?)|\?(?!\w)/.test(sqlContent);
    }
    /**
     * Extract SQL parameters from a query string for validation
     * Enhanced to handle Oracle-specific parameter patterns
     */
    private extractSqlParameters(sqlContent: string): string[] {
        const parameters: string[] = [];

        // Pattern for SSL parameters: ?param? or standalone ?
        const sslParamPattern = /\?(\w+)?\?/g;
        let match: RegExpExecArray | null;

        while ((match = sslParamPattern.exec(sqlContent)) !== null) {
            if (match[1]) {
                // Named parameter: ?paramName?
                parameters.push(match[1]);
            } else {
                // Unnamed parameter: ?
                parameters.push("?");
            }
        }

        // Also check for Oracle bind variables (:param) in case they're used
        const oracleBindPattern = /:(\w+)/g;
        while ((match = oracleBindPattern.exec(sqlContent)) !== null) {
            parameters.push(`:${match[1]}`);
        }

        return parameters;
    }

    // ================================
    // Oracle-Specific Formatting Helpers
    // ================================

    /**
     * Check if SQL contains Oracle-specific constructs
     */
    private isOracleSpecificSql(sqlContent: string): boolean {
        const oraclePatterns = [
            /\bROWNUM\b/i,
            /\bROWID\b/i,
            /\bSYSDATE\b/i,
            /\bDUAL\b/i,
            /\bDECODE\s*\(/i,
            /\bNVL\s*\(/i,
            /\bNVL2\s*\(/i,
            /\bCONNECT\s+BY\b/i,
            /\bSTART\s+WITH\b/i,
            /\bPRIOR\b/i,
            /\bLEVEL\b/i,
            /\bPARTITION\s+BY\b/i,
            /\/\*\+.*\*\//i, // Oracle hints
            /\bTO_DATE\s*\(/i,
            /\bTO_CHAR\s*\(/i,
            /\bTO_NUMBER\s*\(/i,
            /\bWM_CONCAT\s*\(/i,
            /\bLISTAGG\s*\(/i,
        ];

        return oraclePatterns.some((pattern) => pattern.test(sqlContent));
    }

    /**
     * Format Oracle hints with proper spacing
     */
    private formatOracleHints(sqlContent: string): string {
        // Oracle hints pattern: /*+ hint */
        return sqlContent.replace(/\/\*\+([^*]*)\*\//gi, (match, hintContent) => {
            // Clean up spacing in hints
            const cleanHint = hintContent.trim().replace(/\s+/g, " ");
            return `/*+ ${cleanHint} */`;
        });
    }

    /**
     * Check if SQL contains PL/SQL block
     */
    private isPLSQLBlock(sqlContent: string): boolean {
        return /\bBEGIN\b[\s\S]*\bEND\b\s*;/i.test(sqlContent);
    }
}
