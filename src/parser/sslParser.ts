/**
 * SSL Parser
 * Parses SSL code into an Abstract Syntax Tree (AST)
 * Based on the EBNF grammar
 */

import { Token, TokenType, SSLTokenizer } from "./tokenizer";
import * as AST from "./sslAst";
import * as AdditionalStatements from "./additionalStatements";
import * as ExprHelpers from "./expressionHelpers";

// Type aliases for casting
type StatementWithCondition = ExprHelpers.StatementWithCondition;
type StatementWithExpression = ExprHelpers.StatementWithExpression;
type NamedStatement = ExprHelpers.NamedStatement;
type VariableStatement = ExprHelpers.VariableStatement;
type ExpressionWithOperands = ExprHelpers.ExpressionWithOperands;
type NamedExpression = ExprHelpers.NamedExpression;

/**
 * Severity levels for parser diagnostics
 */
export enum DiagnosticSeverity {
    error,
    warning,
    information,
    hint,
}

/**
 * Represents a parser error
 */
export interface ParserError {
    message: string;
    range: AST.Range;
    severity: DiagnosticSeverity;
}

/**
 * SSL Parser class
 * Converts tokens into an AST according to the SSL grammar
 */
export class SSLParser {
    private tokens: Token[] = [];
    private current: number = 0;
    private errors: ParserError[] = [];
    private lastCommentParsedRange: AST.Range | null = null;

    /**
     * Parses SSL code into an AST
     * @param code The SSL code to parse
     * @returns An AST representing the code
     */
    public parse(code: string): { ast: AST.Program; errors: ParserError[] } {
        // Reset parser state
        this.errors = [];
        this.current = 0;
        this.lastCommentParsedRange = null;

        // Tokenize the code
        const tokenizer = new SSLTokenizer(code);
        this.tokens = tokenizer.tokenize().filter((token) => token.type !== TokenType.whitespace);

        try {
            // Parse program
            const program = this.parseProgram();
            return { ast: program, errors: this.errors };
        } catch (error) {
            if (error instanceof Error) {
                this.errors.push({
                    message: error.message,
                    range: this.peek().range,
                    severity: DiagnosticSeverity.error,
                });
            }

            // Return a partial AST with errors
            return {
                ast: {
                    type: "Program",
                    body: [],
                    isClassDefinition: false,
                    range: {
                        start: { line: 0, character: 0 },
                        end: { line: 0, character: 0 },
                    },
                },
                errors: this.errors,
            };
        }
    }

    /**
     * Parses a program (the top-level structure)
     * A program can be a class definition or a series of statements
     * @returns A Program AST node
     */
    private parseProgram(): AST.Program {
        const start = this.peek().range.start;
        const statements: AST.Statement[] = [];
        let isClassDefinition = false;

        // Check if the program starts with a class definition
        if (this.check(TokenType.keyword) && this.peek().value.toUpperCase() === ":CLASS") {
            const classDefinition = this.parseClassDefinition();
            statements.push(classDefinition);
            isClassDefinition = true;
        } else {
            // Parse statements until the end of file
            while (!this.isAtEnd()) {
                try {
                    const statement = this.parseStatement();
                    if (statement) {
                        statements.push(statement);
                    }
                } catch (error) {
                    if (error instanceof Error) {
                        this.errors.push({
                            message: error.message,
                            range: this.peek().range,
                            severity: DiagnosticSeverity.error,
                        });
                    }

                    // Skip to the next semicolon or statement to recover from errors
                    this.synchronize();
                }
            }
        }

        const end = this.isAtEnd() ? this.previous().range.end : this.peek().range.end;

        return {
            type: "Program",
            body: statements,
            isClassDefinition,
            range: { start, end },
        };
    }

    /**
     * Parses a class definition
     */
    private parseClassDefinition(): AST.ClassDefinition {
        const startToken = this.advance(); // Consume :CLASS
        const start = startToken.range.start;

        // Parse class name
        const nameToken = this.consume(TokenType.identifier, "Expected class name after :CLASS");
        const name = nameToken.value;

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after class name");

        // Parse optional inherit statement
        let inherit: string | null = null;
        if (this.check(TokenType.keyword) && this.peek().value.toUpperCase() === ":INHERIT") {
            this.advance(); // Consume :INHERIT
            const inheritNameToken = this.consume(
                TokenType.identifier,
                "Expected class name after :INHERIT"
            );
            inherit = inheritNameToken.value;
            this.consume(TokenType.semicolon, "Expected ';' after inherit class name");
        }

        // Parse class members (fields and methods)
        const members: Array<AST.ClassFieldDeclaration | AST.ProcedureStatement> = [];

        while (!this.isAtEnd()) {
            if (this.check(TokenType.keyword)) {
                const keyword = this.peek().value.toUpperCase();

                // Class field declaration
                if (keyword === ":DECLARE") {
                    this.advance(); // Consume :DECLARE
                    const fields = this.parseIdentifierList();
                    this.consume(TokenType.semicolon, "Expected ';' after field declaration");

                    members.push({
                        type: "ClassFieldDeclaration",
                        fields,
                        range: {
                            start: startToken.range.start,
                            end: this.previous().range.end,
                        },
                    });
                }
                // Method declaration
                else if (keyword === ":PROCEDURE") {
                    const procedure = this.parseProcedureStatement();
                    members.push(procedure);
                }
                // End of class definition or unexpected keyword
                else {
                    break;
                }
            } else {
                break;
            }
        }

        const end = this.previous().range.end;

        return {
            type: "ClassDefinition",
            name,
            inherit,
            members,
            range: { start, end },
        };
    }

    /**
     * Parses a statement
     * @returns A Statement AST node
     */
    private parseStatement(): AST.Statement {
        // Skip any whitespace tokens
        this.skipWhitespace();

        // Handle comments as statements
        if (this.check(TokenType.comment)) {
            return this.parseCommentStatement();
        }

        // Check for keyword statements
        if (this.check(TokenType.keyword)) {
            const keyword = this.peek().value.toUpperCase();

            switch (keyword) {
                case ":PROCEDURE":
                    return this.parseProcedureStatement();
                case ":IF":
                    return this.parseIfStatement();
                case ":ELSE":
                    return this.parseElseStatement();
                case ":ENDIF":
                    return this.parseEndIfStatement();
                case ":WHILE":
                    return this.parseWhileStatement();
                case ":ENDWHILE":
                    return this.parseEndWhileStatement();
                case ":EXITWHILE":
                    return this.parseExitWhileStatement();
                case ":FOR":
                    return this.parseForStatement();
                case ":NEXT":
                    return this.parseNextStatement();
                case ":EXITFOR":
                    return this.parseExitForStatement();
                case ":LOOP":
                    return this.parseLoopContinueStatement();
                case ":BEGINCASE":
                    return this.parseBeginCaseStatement();
                case ":CASE":
                    return this.parseCaseStatement();
                case ":OTHERWISE":
                    return this.parseOtherwiseStatement();
                case ":ENDCASE":
                    return this.parseEndCaseStatement();
                case ":EXITCASE":
                    return this.parseExitCaseStatement();
                case ":TRY":
                    return this.parseTryStatement();
                case ":CATCH":
                    return this.parseCatchStatement();
                case ":FINALLY":
                    return this.parseFinallyStatement();
                case ":ENDTRY":
                    return this.parseEndTryStatement();
                case ":ERROR":
                    return this.parseErrorStatement();
                case ":PARAMETERS":
                    return this.parseParametersStatement();
                case ":DECLARE":
                    return this.parseDeclareStatement();
                case ":DEFAULT":
                    return this.parseDefaultStatement();
                case ":PUBLIC":
                    return this.parsePublicStatement();
                case ":INCLUDE":
                    return this.parseIncludeStatement();
                case ":RETURN":
                    return this.parseReturnStatement();
                case ":LABEL":
                    return this.parseLabelStatement();
                case ":REGION":
                    return this.parseRegionStatement();
                case ":ENDREGION":
                    return this.parseEndRegionStatement();
                case ":BEGININLINECODE":
                    return this.parseBeginInlineCodeStatement();
                case ":ENDINLINECODE":
                    return this.parseEndInlineCodeStatement();
                default:
                    // Unrecognized keyword
                    this.addError(`Unexpected keyword: ${keyword}`, this.peek().range);
                    this.advance();
                    // Skip the semicolon if there is one
                    if (this.check(TokenType.semicolon)) {
                        this.advance();
                    }
                    return this.createErrorStatement(keyword);
            }
        }

        // Handle logic statements (assignments, function calls, etc.)
        return this.parseLogicStatement();
    }

    /**
     * Parses a procedure statement
     * @returns A ProcedureStatement AST node
     */
    private parseProcedureStatement(): AST.ProcedureStatement {
        const startToken = this.advance(); // Consume :PROCEDURE
        const start = startToken.range.start;

        // Parse procedure name
        const nameToken = this.consume(
            TokenType.identifier,
            "Expected procedure name after :PROCEDURE"
        );
        const name = nameToken.value;

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after procedure name");

        // Parse parameters and default parameters
        let parameters: AST.ParameterDeclaration | null = null;
        let defaultParameters: AST.DefaultParameterDeclaration | null = null;

        // Parse parameters if present
        if (this.check(TokenType.keyword) && this.peek().value.toUpperCase() === ":PARAMETERS") {
            parameters = this.parseParameterDeclaration();
        }

        // Parse default parameters if present
        if (this.check(TokenType.keyword) && this.peek().value.toUpperCase() === ":DEFAULT") {
            defaultParameters = this.parseDefaultParameterDeclaration();
        }

        // Parse procedure body until :ENDPROC
        const body: AST.Statement[] = [];
        while (
            !this.isAtEnd() &&
            !(this.check(TokenType.keyword) && this.peek().value.toUpperCase() === ":ENDPROC")
        ) {
            try {
                const statement = this.parseStatement();
                if (statement) {
                    body.push(statement);
                }
            } catch (error) {
                if (error instanceof Error) {
                    this.errors.push({
                        message: error.message,
                        range: this.peek().range,
                        severity: DiagnosticSeverity.error,
                    });
                }
                this.synchronize();
            }
        }

        // Parse :ENDPROC
        if (this.check(TokenType.keyword) && this.peek().value.toUpperCase() === ":ENDPROC") {
            this.advance(); // Consume :ENDPROC
            this.consume(TokenType.semicolon, "Expected ';' after :ENDPROC");
        } else {
            this.addError("Expected :ENDPROC to close procedure", this.peek().range);
        }

        const end = this.previous().range.end;

        return {
            type: "ProcedureStatement",
            name,
            parameters,
            defaultParameters,
            body,
            range: { start, end },
        };
    }

    /**
     * Parses a parameter declaration
     * @returns A ParameterDeclaration AST node
     */
    private parseParameterDeclaration(): AST.ParameterDeclaration {
        const startToken = this.advance(); // Consume :PARAMETERS
        const start = startToken.range.start;

        // Parse parameter list
        const parameters = this.parseIdentifierList();

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after parameter list");

        const end = this.previous().range.end;

        return {
            type: "ParameterDeclaration",
            parameters,
            range: { start, end },
        };
    }

    /**
     * Parses a default parameter declaration
     * @returns A DefaultParameterDeclaration AST node
     */
    private parseDefaultParameterDeclaration(): AST.DefaultParameterDeclaration {
        const startToken = this.advance(); // Consume :DEFAULT
        const start = startToken.range.start;

        // Parse default parameter list (name, value pairs)
        const parameters: Array<{ name: string; value: AST.Expression }> = [];

        // Parse first parameter
        const nameToken = this.consume(TokenType.identifier, "Expected parameter name");
        this.consume(TokenType.comma, "Expected ',' after parameter name");
        const value = this.parseExpression();
        parameters.push({ name: nameToken.value, value });

        // Parse additional parameters
        while (this.match(TokenType.comma)) {
            const nameToken = this.consume(TokenType.identifier, "Expected parameter name");
            this.consume(TokenType.comma, "Expected ',' after parameter name");
            const value = this.parseExpression();
            parameters.push({ name: nameToken.value, value });
        }

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after default parameter list");

        const end = this.previous().range.end;

        return {
            type: "DefaultParameterDeclaration",
            parameters,
            range: { start, end },
        };
    }

    /**
     * Parses an if statement
     * @returns An IfStatement AST node
     */
    private parseIfStatement(): AST.SimpleIfStatement {
        const startToken = this.advance(); // Consume :IF
        const start = startToken.range.start;

        // Parse condition
        const condition = this.parseExpression();

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after if condition");

        const end = this.previous().range.end;

        return {
            type: "IfStatement",
            condition,
            range: { start, end },
        };
    }
    /**
     * Parses an else statement
     * @returns An ElseStatement AST node
     */
    private parseElseStatement(): AdditionalStatements.ElseStatement {
        const startToken = this.advance(); // Consume :ELSE
        const start = startToken.range.start;

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after :ELSE");
        const end = this.previous().range.end;

        return {
            type: "ElseStatement",
            range: { start, end },
        };
    }

    /**
     * Parses an endif statement
     * @returns An EndIfStatement AST node
     */
    private parseEndIfStatement(): AdditionalStatements.EndIfStatement {
        const startToken = this.advance(); // Consume :ENDIF
        const start = startToken.range.start;

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after :ENDIF");

        const end = this.previous().range.end;

        return {
            type: "EndIfStatement",
            range: { start, end },
        };
    }
    /**
     * Parses a while statement
     * @returns A WhileStatement AST node
     */
    private parseWhileStatement(): AST.WhileStatement {
        const startToken = this.advance(); // Consume :WHILE
        const start = startToken.range.start;

        // Parse condition
        const condition = this.parseExpression();

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after while condition");

        const end = this.previous().range.end;
        return {
            type: "WhileStatement",
            condition,
            body: [], // Placeholder for body statements which will be processed later
            range: { start, end },
        };
    }

    /**
     * Parses an endwhile statement
     * @returns An EndWhileStatement AST node
     */
    private parseEndWhileStatement(): AdditionalStatements.EndWhileStatement {
        const startToken = this.advance(); // Consume :ENDWHILE
        const start = startToken.range.start;

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after :ENDWHILE");

        const end = this.previous().range.end;

        return {
            type: "EndWhileStatement",
            range: { start, end },
        };
    }

    /**
     * Parses an exitwhile statement
     * @returns An ExitWhileStatement AST node
     */
    private parseExitWhileStatement(): AST.ExitStatement {
        const startToken = this.advance(); // Consume :EXITWHILE
        const start = startToken.range.start;

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after :EXITWHILE");

        const end = this.previous().range.end;

        return {
            type: "ExitStatement",
            exitType: "WHILE",
            range: { start, end },
        };
    }

    /**
     * Parses a for statement
     * @returns A ForStatement AST node
     */
    private parseForStatement(): AST.SimpleForStatement {
        const startToken = this.advance(); // Consume :FOR
        const start = startToken.range.start;

        // Parse iterator variable
        const variableToken = this.consume(
            TokenType.identifier,
            "Expected variable name after :FOR"
        );

        // Parse assignment operator
        this.consume(TokenType.assignmentOperator, "Expected ':=' after variable name");

        // Parse initial value
        const initialValue = this.parseExpression();

        // Parse TO keyword
        this.consume(TokenType.keyword, "Expected :TO after initial value");

        // Parse final value
        const finalValue = this.parseExpression();

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after for statement");

        const end = this.previous().range.end;

        return {
            type: "ForStatement",
            variable: variableToken.value,
            initialValue,
            finalValue,
            range: { start, end },
        };
    }

    /**
     * Parses a next statement
     * @returns A NextStatement AST node
     */
    private parseNextStatement(): AdditionalStatements.NextStatement {
        const startToken = this.advance(); // Consume :NEXT
        const start = startToken.range.start;

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after :NEXT");

        const end = this.previous().range.end;

        return {
            type: "NextStatement",
            range: { start, end },
        };
    }

    /**
     * Parses an exitfor statement
     * @returns An ExitForStatement AST node
     */
    private parseExitForStatement(): AST.ExitStatement {
        const startToken = this.advance(); // Consume :EXITFOR
        const start = startToken.range.start;

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after :EXITFOR");

        const end = this.previous().range.end;

        return {
            type: "ExitStatement",
            exitType: "FOR",
            range: { start, end },
        };
    }

    /**
     * Parses a loop continue statement
     * @returns A LoopContinueStatement AST node
     */
    private parseLoopContinueStatement(): AST.LoopContinueStatement {
        const startToken = this.advance(); // Consume :LOOP
        const start = startToken.range.start;

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after :LOOP");

        const end = this.previous().range.end;

        return {
            type: "LoopContinueStatement",
            range: { start, end },
        };
    }

    /**
     * Parses a begincase statement
     * @returns A BeginCaseStatement AST node
     */
    private parseBeginCaseStatement(): AdditionalStatements.BeginCaseStatement {
        const startToken = this.advance(); // Consume :BEGINCASE
        const start = startToken.range.start;

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after :BEGINCASE");

        const end = this.previous().range.end;

        return {
            type: "BeginCaseStatement",
            range: { start, end },
        };
    }
    /**
     * Parses a case statement
     * @returns A CaseStatement AST node
     */
    private parseCaseStatement(): AdditionalStatements.CaseStatement {
        const startToken = this.advance(); // Consume :CASE
        const start = startToken.range.start;

        // Parse case condition
        const condition = this.parseExpression();

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after case condition");

        const end = this.previous().range.end;

        return {
            type: "CaseStatement",
            condition,
            range: { start, end },
        };
    }

    /**
     * Parses an otherwise statement
     * @returns An OtherwiseStatement AST node
     */
    private parseOtherwiseStatement(): AdditionalStatements.OtherwiseStatement {
        const startToken = this.advance(); // Consume :OTHERWISE
        const start = startToken.range.start;

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after :OTHERWISE");

        const end = this.previous().range.end;

        return {
            type: "OtherwiseStatement",
            range: { start, end },
        };
    }

    /**
     * Parses an endcase statement
     * @returns An EndCaseStatement AST node
     */
    private parseEndCaseStatement(): AdditionalStatements.EndCaseStatement {
        const startToken = this.advance(); // Consume :ENDCASE
        const start = startToken.range.start;

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after :ENDCASE");

        const end = this.previous().range.end;

        return {
            type: "EndCaseStatement",
            range: { start, end },
        };
    }

    /**
     * Parses an exitcase statement
     * @returns An ExitCaseStatement AST node
     */
    private parseExitCaseStatement(): AST.ExitStatement {
        const startToken = this.advance(); // Consume :EXITCASE
        const start = startToken.range.start;

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after :EXITCASE");

        const end = this.previous().range.end;

        return {
            type: "ExitStatement",
            exitType: "CASE",
            range: { start, end },
        };
    }

    /**
     * Parses a try statement
     * @returns A TryStatement AST node
     */
    private parseTryStatement(): AST.TryStatement {
        const startToken = this.advance(); // Consume :TRY
        const start = startToken.range.start;

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after :TRY");

        const end = this.previous().range.end;

        return {
            type: "TryStatement",
            tryBlock: [], // Initialize with empty array
            catchBlock: [], // Initialize with empty array
            finallyBlock: null, // Initialize with null or empty array as needed
            range: { start, end },
        };
    }

    /**
     * Parses a catch statement
     * @returns A CatchStatement AST node
     */
    private parseCatchStatement(): AdditionalStatements.CatchStatement {
        const startToken = this.advance(); // Consume :CATCH
        const start = startToken.range.start;

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after :CATCH");

        const end = this.previous().range.end;

        return {
            type: "CatchStatement",
            range: { start, end },
        };
    }

    /**
     * Parses a finally statement
     * @returns A FinallyStatement AST node
     */
    private parseFinallyStatement(): AdditionalStatements.FinallyStatement {
        const startToken = this.advance(); // Consume :FINALLY
        const start = startToken.range.start;

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after :FINALLY");

        const end = this.previous().range.end;

        return {
            type: "FinallyStatement",
            range: { start, end },
        };
    }

    /**
     * Parses an endtry statement
     * @returns An EndTryStatement AST node
     */
    private parseEndTryStatement(): AdditionalStatements.EndTryStatement {
        const startToken = this.advance(); // Consume :ENDTRY
        const start = startToken.range.start;

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after :ENDTRY");

        const end = this.previous().range.end;

        return {
            type: "EndTryStatement",
            range: { start, end },
        };
    }
    /**
     * Parses an error statement
     * @returns An ErrorStatement AST node
     */
    private parseErrorStatement(): AdditionalStatements.ErrorStatement {
        const startToken = this.advance(); // Consume :ERROR
        const start = startToken.range.start;

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after :ERROR");

        const end = this.previous().range.end;

        return {
            type: "ErrorStatement",
            message: "Error statement",
            range: { start, end },
        };
    }

    /**
     * Parses a parameters statement
     * @returns A ParametersStatement AST node
     */
    private parseParametersStatement(): AdditionalStatements.ParametersStatement {
        const declaration = this.parseParameterDeclaration();
        return {
            type: "ParametersStatement",
            parameters: declaration.parameters,
            range: declaration.range,
        };
    }

    /**
     * Parses a declare statement
     * @returns A DeclarationStatement AST node
     */
    private parseDeclareStatement(): AST.DeclarationStatement {
        const startToken = this.advance(); // Consume :DECLARE
        const start = startToken.range.start;

        // Parse identifier list
        const identifiers = this.parseIdentifierList();

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after declare statement");

        const end = this.previous().range.end;

        return {
            type: "DeclarationStatement",
            declarationType: "DECLARE",
            identifiers,
            range: { start, end },
        };
    }

    /**
     * Parses a default statement
     * @returns A DefaultStatement AST node
     */
    private parseDefaultStatement(): AST.DefaultParameterDeclaration {
        return this.parseDefaultParameterDeclaration();
    }

    /**
     * Parses a public statement
     * @returns A PublicStatement AST node
     */
    private parsePublicStatement(): AST.DeclarationStatement {
        const startToken = this.advance(); // Consume :PUBLIC
        const start = startToken.range.start;

        // Parse identifier list
        const identifiers = this.parseIdentifierList();

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after public statement");

        const end = this.previous().range.end;

        return {
            type: "DeclarationStatement",
            declarationType: "PUBLIC",
            identifiers,
            range: { start, end },
        };
    }

    /**
     * Parses an include statement
     * @returns An IncludeStatement AST node
     */
    private parseIncludeStatement(): AST.IncludeStatement {
        const startToken = this.advance(); // Consume :INCLUDE
        const start = startToken.range.start;

        // Parse string literal
        const pathToken = this.consume(
            TokenType.stringLiteral,
            "Expected string literal after :INCLUDE"
        );

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after include path");

        const end = this.previous().range.end;

        return {
            type: "IncludeStatement",
            path: this.unquoteString(pathToken.value),
            range: { start, end },
        };
    }

    /**
     * Parses a return statement
     * @returns A ReturnStatement AST node
     */
    private parseReturnStatement(): AST.ReturnStatement {
        const startToken = this.advance(); // Consume :RETURN
        const start = startToken.range.start;

        // Parse optional expression
        let value: AST.Expression | null = null;
        if (!this.check(TokenType.semicolon)) {
            value = this.parseExpression();
        }

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after return statement");

        const end = this.previous().range.end;

        return {
            type: "ReturnStatement",
            value,
            range: { start, end },
        };
    }

    /**
     * Parses a label statement
     * @returns A LabelStatement AST node
     */
    private parseLabelStatement(): AST.LabelStatement {
        const startToken = this.advance(); // Consume :LABEL
        const start = startToken.range.start;

        // Parse label name
        const nameToken = this.consume(TokenType.identifier, "Expected label name after :LABEL");

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after label name");

        const end = this.previous().range.end;

        return {
            type: "LabelStatement",
            name: nameToken.value,
            range: { start, end },
        };
    }

    /**
     * Parses a region statement
     * @returns A RegionStatement AST node
     */
    private parseRegionStatement(): AST.RegionStatement {
        const startToken = this.advance(); // Consume :REGION
        const start = startToken.range.start;

        // Parse region name (optional)
        let name = "";
        if (this.check(TokenType.identifier)) {
            name = this.advance().value;
        }

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after :REGION");

        const end = this.previous().range.end;

        return {
            type: "RegionStatement",
            name,
            body: [], // Initialize with empty array
            range: { start, end },
        };
    }

    /**
     * Parses an endregion statement
     * @returns An EndRegionStatement AST node
     */
    private parseEndRegionStatement(): AdditionalStatements.EndRegionStatement {
        const startToken = this.advance(); // Consume :ENDREGION
        const start = startToken.range.start;

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after :ENDREGION");

        const end = this.previous().range.end;

        return {
            type: "EndRegionStatement",
            range: { start, end },
        };
    }

    /**
     * Parses a begininlinecode statement
     * @returns A BeginInlineCodeStatement AST node
     */
    private parseBeginInlineCodeStatement(): AST.BeginInlineCodeStatement {
        const startToken = this.advance(); // Consume :BEGININLINECODE
        const start = startToken.range.start;

        // Parse optional name (string literal or identifier)
        let name: string | null = null;
        if (this.check(TokenType.stringLiteral)) {
            name = this.unquoteString(this.advance().value);
        } else if (this.check(TokenType.identifier)) {
            name = this.advance().value;
        }

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after :BEGININLINECODE");

        const end = this.previous().range.end;

        return {
            type: "BeginInlineCodeStatement",
            name,
            range: { start, end },
        };
    }

    /**
     * Parses an endinlinecode statement
     * @returns An EndInlineCodeStatement AST node
     */
    private parseEndInlineCodeStatement(): AST.EndInlineCodeStatement {
        const startToken = this.advance(); // Consume :ENDINLINECODE
        const start = startToken.range.start;

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after :ENDINLINECODE");

        const end = this.previous().range.end;

        return {
            type: "EndInlineCodeStatement",
            range: { start, end },
        };
    }
    /**
     * Parses a comment statement
     * @returns A CommentStatement AST node
     */
    private parseCommentStatement(): AST.CommentStatement {
        const token = this.advance(); // Consume comment token

        // Check if we've already parsed this comment to avoid duplicates
        if (
            this.lastCommentParsedRange &&
            this.lastCommentParsedRange.start.line === token.range.start.line &&
            this.lastCommentParsedRange.start.character === token.range.start.character
        ) {
            // Skip to semicolon if not already at one
            if (!this.check(TokenType.semicolon)) {
                this.synchronize();
            }

            // Create a dummy comment to avoid duplicate processing
            return {
                type: "CommentStatement",
                value: token.value,
                range: token.range,
            };
        }

        // Store this comment range to avoid duplicate processing
        this.lastCommentParsedRange = token.range;

        // Determine if this is a region or endregion comment
        const isRegionComment = token.type === TokenType.regionComment;
        const isEndRegionComment = token.type === TokenType.endRegionComment;

        return {
            type: "CommentStatement",
            value: token.value,
            range: token.range,
            isRegionComment: isRegionComment,
            isEndRegionComment: isEndRegionComment,
        };
    }

    /**
     * Parses a logic statement (assignment, function call, etc.)
     * @returns A Statement AST node
     */
    private parseLogicStatement(): AST.AssignmentStatement | AST.FunctionCallStatement {
        const start = this.peek().range.start;

        // Parse expression
        const expression = this.parseExpression();

        // Parse semicolon
        this.consume(TokenType.semicolon, "Expected ';' after statement");

        const end = this.previous().range.end;

        // Check if it's an assignment
        if (
            expression.type === "BinaryExpression" &&
            [":=", "+=", "-=", "*=", "/=", "^="].includes(
                (expression as AST.BinaryExpression).operator
            )
        ) {
            return {
                type: "AssignmentStatement",
                left: (expression as AST.BinaryExpression).left as any,
                operator: (expression as AST.BinaryExpression).operator,
                right: (expression as AST.BinaryExpression).right,
                range: { start, end },
            };
        }

        // Otherwise it's a function call or other expression
        return {
            type: "FunctionCallStatement",
            expression: expression as any, // Cast to the expected expression type
            range: { start, end },
        };
    }

    /**
     * Parses an identifier list (comma-separated list of identifiers)
     * @returns An array of identifier strings
     */
    private parseIdentifierList(): string[] {
        const identifiers: string[] = [];

        // Parse first identifier
        const firstIdentifier = this.consume(TokenType.identifier, "Expected identifier");
        identifiers.push(firstIdentifier.value);

        // Parse additional identifiers
        while (this.match(TokenType.comma)) {
            const identifier = this.consume(TokenType.identifier, "Expected identifier after ','");
            identifiers.push(identifier.value);
        }

        return identifiers;
    }

    /**
     * Parses an expression
     * @returns An Expression AST node
     */
    private parseExpression(): AST.Expression {
        return this.parseLogicalExpression();
    }
    /**
     * Parses a logical expression (AND, OR)
     * @returns An Expression AST node
     */
    private parseLogicalExpression(): AST.Expression {
        let expr = this.parseComparisonExpression();

        while (this.check(TokenType.logicalOperator)) {
            const operator = this.advance().value;
            const right = this.parseComparisonExpression();

            expr = {
                type: "BinaryExpression",
                left: expr,
                operator,
                right,
                range: {
                    start: expr.range.start,
                    end: right.range.end,
                },
            } as AST.BinaryExpression;
        }

        return expr;
    }

    /**
     * Parses a comparison expression (==, !=, <, >, <=, >=, =)
     * @returns An Expression AST node
     */
    private parseComparisonExpression(): AST.Expression {
        let expr = this.parseArithmeticExpression();

        if (this.check(TokenType.comparisonOperator)) {
            const operator = this.advance().value;
            const right = this.parseArithmeticExpression();

            expr = {
                type: "BinaryExpression",
                left: expr,
                operator,
                right,
                range: {
                    start: expr.range.start,
                    end: right.range.end,
                },
            } as AST.BinaryExpression;
        }

        return expr;
    }

    /**
     * Parses an arithmetic expression (addition, subtraction)
     * @returns An Expression AST node
     */
    private parseArithmeticExpression(): AST.Expression {
        let expr = this.parseTerm();

        while (this.check(TokenType.arithmeticOperator) && ["+", "-"].includes(this.peek().value)) {
            const operator = this.advance().value;
            const right = this.parseTerm();

            expr = {
                type: "BinaryExpression",
                left: expr,
                operator,
                right,
                range: {
                    start: expr.range.start,
                    end: right.range.end,
                },
            } as AST.BinaryExpression;
        }

        return expr;
    }

    /**
     * Parses a term (multiplication, division, modulo)
     * @returns An Expression AST node
     */
    private parseTerm(): AST.Expression {
        let expr = this.parseFactor();

        while (
            this.check(TokenType.arithmeticOperator) &&
            ["*", "/", "%"].includes(this.peek().value)
        ) {
            const operator = this.advance().value;
            const right = this.parseFactor();

            expr = {
                type: "BinaryExpression",
                left: expr,
                operator,
                right,
                range: {
                    start: expr.range.start,
                    end: right.range.end,
                },
            } as AST.BinaryExpression;
        }

        return expr;
    }

    /**
     * Parses a factor (exponentiation)
     * @returns An Expression AST node
     */
    private parseFactor(): AST.Expression {
        let expr = this.parsePowerOperand();

        if (this.check(TokenType.arithmeticOperator) && this.peek().value === "^") {
            const operator = this.advance().value;
            const right = this.parsePowerOperand();

            expr = {
                type: "BinaryExpression",
                left: expr,
                operator,
                right,
                range: {
                    start: expr.range.start,
                    end: right.range.end,
                },
            } as AST.BinaryExpression;
        }

        return expr;
    }

    /**
     * Parses a power operand (unary operations)
     * @returns An Expression AST node
     */
    private parsePowerOperand(): AST.Expression {
        // Handle unary operators
        if (
            (this.check(TokenType.arithmeticOperator) && ["+", "-"].includes(this.peek().value)) ||
            (this.check(TokenType.logicalOperator) && this.peek().value === ".NOT.")
        ) {
            const operatorToken = this.advance();
            const operator = operatorToken.value;
            const right = this.parsePrimaryExpression();

            return {
                type: "UnaryExpression",
                operator,
                argument: right,
                range: {
                    start: operatorToken.range.start,
                    end: right.range.end,
                },
            } as AST.UnaryExpression;
        }

        return this.parsePrimaryExpression();
    }

    /**
     * Parses a primary expression (literals, identifiers, etc.)
     * @returns An Expression AST node
     */
    private parsePrimaryExpression(): AST.Expression {
        // Handle literals
        if (this.check(TokenType.stringLiteral)) {
            return this.parseLiteral("String");
        }

        if (this.check(TokenType.numberLiteral)) {
            return this.parseLiteral("Number");
        }

        if (this.check(TokenType.booleanLiteral)) {
            return this.parseLiteral("Boolean");
        }

        if (this.check(TokenType.nilLiteral)) {
            return this.parseLiteral("Nil");
        }

        // Handle array literals
        if (this.check(TokenType.leftBrace)) {
            return this.parseArrayLiteral();
        }

        // Handle parenthesized expressions
        if (this.match(TokenType.leftParen)) {
            const expr = this.parseExpression();
            this.consume(TokenType.rightParen, "Expected ')' after expression");
            return expr;
        }

        // Handle identifiers, function calls, property access, array access
        if (this.check(TokenType.identifier)) {
            const identifier = this.advance();

            // Handle function call: identifier(args)
            if (this.check(TokenType.leftParen)) {
                return this.parseFunctionCall(identifier.value);
            }

            // Handle property access: identifier:property
            if (this.check(TokenType.colon)) {
                return this.parsePropertyAccess(identifier.value);
            }

            // Handle array access: identifier[index]
            if (this.check(TokenType.leftBracket)) {
                return this.parseArrayAccess(identifier.value);
            }

            // Handle assignment operators
            if (this.check(TokenType.assignmentOperator)) {
                const operator = this.advance().value;
                const right = this.parseExpression();

                return {
                    type: "BinaryExpression",
                    left: {
                        type: "VariableAccess",
                        name: identifier.value,
                        range: identifier.range,
                    } as AST.VariableAccess,
                    operator,
                    right,
                    range: {
                        start: identifier.range.start,
                        end: right.range.end,
                    },
                } as AST.BinaryExpression;
            } // Simple variable access
            return {
                type: "VariableAccess",
                name: identifier.value,
                range: identifier.range,
            } as AST.VariableAccess;
        }

        // If we got here, it's an error
        this.addError(`Unexpected token: ${this.peek().value}`, this.peek().range);
        return this.createErrorExpression();
    }

    /**
     * Parses a literal value
     * @param literalType The type of literal
     * @returns A Literal AST node
     */
    private parseLiteral(literalType: "String" | "Number" | "Boolean" | "Nil"): AST.Literal {
        const token = this.advance();

        let value: any;

        switch (literalType) {
            case "String":
                value = this.unquoteString(token.value);
                break;
            case "Number":
                value = parseFloat(token.value);
                break;
            case "Boolean":
                value = token.value.toUpperCase() === ".T.";
                break;
            case "Nil":
                value = null;
                break;
        }

        return {
            type: "Literal",
            literalType,
            value,
            range: token.range,
        };
    }

    /**
     * Parses an array literal
     * @returns A Literal AST node
     */
    private parseArrayLiteral(): AST.Literal {
        const start = this.advance().range.start; // Consume {

        const elements: AST.Expression[] = [];

        // Handle empty array
        if (this.match(TokenType.rightBrace)) {
            return {
                type: "Literal",
                literalType: "Array",
                value: elements,
                range: {
                    start,
                    end: this.previous().range.end,
                },
            };
        }

        // Parse first element
        elements.push(this.parseExpression());

        // Parse additional elements
        while (this.match(TokenType.comma)) {
            if (this.check(TokenType.rightBrace)) {
                break; // Allow trailing comma
            }
            elements.push(this.parseExpression());
        }

        const end = this.consume(TokenType.rightBrace, "Expected '}' after array elements").range
            .end;

        return {
            type: "Literal",
            literalType: "Array",
            value: elements,
            range: { start, end },
        };
    }

    /**
     * Parses a function call
     * @param name The name of the function
     * @returns A FunctionCall AST node
     */
    private parseFunctionCall(name: string): AST.FunctionCall {
        const start = this.previous().range.start; // Start from the identifier
        this.advance(); // Consume (

        const args: AST.Expression[] = [];

        // Handle empty argument list
        if (this.match(TokenType.rightParen)) {
            return {
                type: "FunctionCall",
                callType: "Direct",
                name,
                arguments: args,
                range: {
                    start,
                    end: this.previous().range.end,
                },
            };
        }

        // Parse first argument
        args.push(this.parseExpression());

        // Parse additional arguments
        while (this.match(TokenType.comma)) {
            if (this.check(TokenType.rightParen)) {
                break; // Allow trailing comma
            }
            args.push(this.parseExpression());
        }

        const end = this.consume(TokenType.rightParen, "Expected ')' after function arguments")
            .range.end;

        // Check for special function calls
        let callType: "Direct" | "DoProc" | "ExecFunction" = "Direct";
        if (name === "DoProc" || name === "ExecFunction") {
            callType = name === "DoProc" ? "DoProc" : "ExecFunction";
        }

        return {
            type: "FunctionCall",
            callType,
            name,
            arguments: args,
            range: { start, end },
        };
    }
    /**
     * Parses a property access
     * @param objectName The name of the object
     * @returns A PropertyAccess AST node
     */
    private parsePropertyAccess(objectName: string): AST.PropertyAccess {
        const objectStart = this.previous().range.start; // Start from the identifier

        this.advance(); // Consume :

        const propertyToken = this.consume(
            TokenType.identifier,
            "Expected property name after ':'"
        ); // Check if it's a method call
        if (this.check(TokenType.leftParen)) {
            const methodCall = this.parseMethodCall(objectName, propertyToken.value, objectStart);
            return methodCall as any as AST.PropertyAccess; // Force the type cast
        }

        return {
            type: "PropertyAccess",
            object: {
                type: "VariableAccess",
                name: objectName,
                range: {
                    start: objectStart,
                    end: {
                        line: objectStart.line,
                        character: objectStart.character + objectName.length,
                    },
                },
            } as AST.VariableAccess,
            property: propertyToken.value,
            range: {
                start: objectStart,
                end: propertyToken.range.end,
            },
        };
    }
    /**
     * Parses a method call
     * @param objectName The name of the object
     * @param methodName The name of the method
     * @param start The start position of the object
     * @returns A MethodCall AST node
     */
    private parseMethodCall(objectName: string, methodName: string, start: any): AST.MethodCall {
        this.advance(); // Consume (

        const args: AST.Expression[] = [];

        // Handle empty argument list
        if (this.match(TokenType.rightParen)) {
            return {
                type: "MethodCall",
                object: {
                    type: "VariableAccess",
                    name: objectName,
                    range: {
                        start,
                        end: { line: start.line, character: start.character + objectName.length },
                    },
                } as AST.VariableAccess,
                method: methodName,
                arguments: args,
                range: {
                    start,
                    end: this.previous().range.end,
                },
            };
        }

        // Parse arguments
        args.push(this.parseExpression());

        while (this.match(TokenType.comma)) {
            if (this.check(TokenType.rightParen)) {
                break; // Allow trailing comma
            }
            args.push(this.parseExpression());
        }

        const end = this.consume(TokenType.rightParen, "Expected ')' after method arguments").range
            .end;
        return {
            type: "MethodCall",
            object: {
                type: "VariableAccess",
                name: objectName,
                range: {
                    start,
                    end: { line: start.line, character: start.character + objectName.length },
                },
            } as AST.VariableAccess,
            method: methodName,
            arguments: args,
            range: { start, end },
        };
    }

    /**
     * Parses an array access
     * @param arrayName The name of the array
     * @returns An ArrayAccess AST node
     */
    private parseArrayAccess(arrayName: string): AST.ArrayAccess {
        const start = this.previous().range.start; // Start from the identifier
        this.advance(); // Consume [

        const indices: AST.Expression[] = [];
        let isMultiDimensional = false;

        // Parse first index
        indices.push(this.parseExpression());

        // Check for multi-dimensional array access with comma notation
        while (this.match(TokenType.comma)) {
            isMultiDimensional = true;
            indices.push(this.parseExpression());
        }

        const firstBracketEnd = this.consume(
            TokenType.rightBracket,
            "Expected ']' after array index"
        ).range.end;

        // Check for chained bracket notation (arr[1][2])
        let end = firstBracketEnd;
        if (!isMultiDimensional && this.check(TokenType.leftBracket)) {
            isMultiDimensional = true;

            while (this.check(TokenType.leftBracket)) {
                this.advance(); // Consume [
                indices.push(this.parseExpression());
                end = this.consume(TokenType.rightBracket, "Expected ']' after array index").range
                    .end;
            }
        }
        return {
            type: "ArrayAccess",
            array: {
                type: "VariableAccess",
                name: arrayName,
                range: {
                    start,
                    end: { line: start.line, character: start.character + arrayName.length },
                },
            } as AST.VariableAccess,
            indices,
            isMultiDimensional,
            range: { start, end },
        };
    }

    /**
     * Creates an error statement for recovery
     * @param value The value to use in the error statement
     * @returns A generic Statement AST node
     */ private createErrorStatement(value: string): AdditionalStatements.ErrorStatement {
        return {
            type: "ErrorStatement",
            message: value,
            range: this.peek().range,
        };
    }
    /**
     * Creates an error expression for recovery
     * @returns A generic Expression AST node
     */
    private createErrorExpression(): AST.Expression {
        return {
            type: "VariableAccess",
            name: "ERROR",
            range: this.peek().range,
        } as AST.VariableAccess;
    }

    /**
     * Consumes the current token if it matches the expected type
     * @param type The expected token type
     * @param message The error message to display if the token type doesn't match
     * @returns The consumed token
     */
    private consume(type: TokenType, message: string): Token {
        if (this.check(type)) {
            return this.advance();
        }

        this.addError(message, this.peek().range);
        throw new Error(message);
    }

    /**
     * Checks if the current token is of the expected type
     * @param type The expected token type
     * @returns True if the current token is of the expected type, false otherwise
     */
    private check(type: TokenType): boolean {
        if (this.isAtEnd()) {
            return false;
        }
        return this.peek().type === type;
    }

    /**
     * Advances to the next token and returns the previous token
     * @returns The previous token
     */
    private advance(): Token {
        if (!this.isAtEnd()) {
            this.current++;
        }
        return this.previous();
    }

    /**
     * Consumes the current token if it matches the expected type
     * @param type The expected token type
     * @returns True if the token was consumed, false otherwise
     */
    private match(type: TokenType): boolean {
        if (this.check(type)) {
            this.advance();
            return true;
        }
        return false;
    }

    /**
     * Gets the current token
     * @returns The current token
     */
    private peek(): Token {
        return this.tokens[this.current];
    }

    /**
     * Gets the previous token
     * @returns The previous token
     */
    private previous(): Token {
        return this.tokens[this.current - 1];
    }

    /**
     * Checks if we've reached the end of the tokens
     * @returns True if we've reached the end, false otherwise
     */
    private isAtEnd(): boolean {
        return this.peek().type === TokenType.eof;
    }

    /**
     * Removes quotes from a string literal
     * @param str The string literal
     * @returns The string without quotes
     */
    private unquoteString(str: string): string {
        if (str.startsWith('"') && str.endsWith('"')) {
            return str.slice(1, -1);
        }
        if (str.startsWith("'") && str.endsWith("'")) {
            return str.slice(1, -1);
        }
        if (str.startsWith("[") && str.endsWith("]")) {
            return str.slice(1, -1);
        }
        return str;
    }

    /**
     * Skips whitespace tokens
     */
    private skipWhitespace(): void {
        while (this.check(TokenType.whitespace)) {
            this.advance();
        }
    }

    /**
     * Adds an error to the error list
     * @param message The error message
     * @param range The range where the error occurred
     */
    private addError(message: string, range: any): void {
        this.errors.push({
            message,
            range,
            severity: DiagnosticSeverity.error,
        });
    }

    /**
     * Synchronizes the parser to recover from errors
     * Advances until it finds a semicolon or a keyword
     */ private synchronize(): void {
        this.advance();

        while (!this.isAtEnd()) {
            if (this.previous().type === TokenType.semicolon) {
                return;
            }

            if (this.check(TokenType.keyword)) {
                return;
            }
            this.advance();
        }
    }
}
