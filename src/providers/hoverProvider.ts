/**
 * SSL Hover Provider
 *
 * Provides hover information for SSL language constructs.
 * Shows documentation, type information, and usage examples when hovering over:
 * - SSL keywords and statements
 * - Built-in functions
 * - Variables and procedures
 * - Operators and literals
 */

import * as vscode from "vscode";
import { Tokenizer } from "../tokenizer";
import { TokenType } from "../tokenizer/tokenType";
import { Parser } from "../parser";

/**
 * SSL Hover Provider
 */
export class SSLHoverProvider implements vscode.HoverProvider {
    private readonly tokenizer: Tokenizer;

    constructor() {
        this.tokenizer = new Tokenizer();
    }

    /**
     * Provide hover information for the given position
     */
    public provideHover(
        document: vscode.TextDocument,
        position: vscode.Position,
        token: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.Hover> {
        try {
            // Get the word at the cursor position
            const wordRange = document.getWordRangeAtPosition(position);
            if (!wordRange) {
                return null;
            }

            const word = document.getText(wordRange);
            const line = document.lineAt(position.line);

            // Get context around the cursor
            const lineText = line.text;
            const wordStart = wordRange.start.character;
            const wordEnd = wordRange.end.character;

            // Check if we're hovering over a colon keyword
            if (wordStart > 0 && lineText[wordStart - 1] === ":") {
                return this.getKeywordHover(word);
            }

            // Check for function hover
            if (wordEnd < lineText.length && lineText[wordEnd] === "(") {
                return this.getFunctionHover(word);
            }

            // Check for operator hover
            const operatorHover = this.getOperatorHover(word, lineText, wordStart);
            if (operatorHover) {
                return operatorHover;
            }

            // Check for literal hover
            const literalHover = this.getLiteralHover(word);
            if (literalHover) {
                return literalHover;
            }

            // Try to get variable/identifier information
            return this.getIdentifierHover(word, document, position);
        } catch (error) {
            console.error("Error providing SSL hover:", error);
            return null;
        }
    }

    /**
     * Get hover information for SSL keywords
     */
    private getKeywordHover(keyword: string): vscode.Hover | null {
        const keywordUpper = keyword.toUpperCase();
        const keywordInfo = this.getSSLKeywordInfo(keywordUpper);

        if (!keywordInfo) {
            return null;
        }

        const markdown = new vscode.MarkdownString();
        markdown.appendCodeblock(`:${keywordUpper}`, "ssl");
        markdown.appendMarkdown(`**${keywordInfo.category}**\n\n`);
        markdown.appendMarkdown(keywordInfo.description);

        if (keywordInfo.syntax) {
            markdown.appendMarkdown("\n\n**Syntax:**\n");
            markdown.appendCodeblock(keywordInfo.syntax, "ssl");
        }

        if (keywordInfo.example) {
            markdown.appendMarkdown("\n\n**Example:**\n");
            markdown.appendCodeblock(keywordInfo.example, "ssl");
        }

        if (keywordInfo.relatedKeywords) {
            markdown.appendMarkdown("\n\n**Related keywords:** ");
            markdown.appendMarkdown(keywordInfo.relatedKeywords.map((k) => `\`:${k}\``).join(", "));
        }

        return new vscode.Hover(markdown);
    }

    /**
     * Get hover information for SSL functions
     */
    private getFunctionHover(functionName: string): vscode.Hover | null {
        const funcInfo = this.getSSLFunctionInfo(functionName);

        if (!funcInfo) {
            return null;
        }

        const markdown = new vscode.MarkdownString();
        markdown.appendCodeblock(`${functionName}(${funcInfo.parameters})`, "ssl");
        markdown.appendMarkdown(`**${funcInfo.category}**\n\n`);
        markdown.appendMarkdown(funcInfo.description);

        if (funcInfo.returns) {
            markdown.appendMarkdown(`\n\n**Returns:** ${funcInfo.returns}`);
        }

        if (funcInfo.parameters) {
            markdown.appendMarkdown("\n\n**Parameters:**\n");
            funcInfo.parameterDescriptions?.forEach((param) => {
                markdown.appendMarkdown(`- \`${param.name}\`: ${param.description}\n`);
            });
        }

        if (funcInfo.example) {
            markdown.appendMarkdown("\n\n**Example:**\n");
            markdown.appendCodeblock(funcInfo.example, "ssl");
        }

        return new vscode.Hover(markdown);
    }

    /**
     * Get hover information for operators
     */
    private getOperatorHover(
        word: string,
        lineText: string,
        wordStart: number
    ): vscode.Hover | null {
        // Check for multi-character operators
        const context = lineText.substring(Math.max(0, wordStart - 5), wordStart + word.length + 5);

        const operators = [
            { op: ":=", desc: "Assignment operator - assigns a value to a variable" },
            { op: "==", desc: "Equality comparison operator" },
            { op: "!=", desc: "Inequality comparison operator" },
            { op: "<=", desc: "Less than or equal comparison operator" },
            { op: ">=", desc: "Greater than or equal comparison operator" },
            { op: "+=", desc: "Add and assign operator" },
            { op: "-=", desc: "Subtract and assign operator" },
            { op: "*=", desc: "Multiply and assign operator" },
            { op: "/=", desc: "Divide and assign operator" },
            { op: "^=", desc: "Power and assign operator" },
            { op: ".AND.", desc: "Logical AND operator" },
            { op: ".OR.", desc: "Logical OR operator" },
            { op: ".NOT.", desc: "Logical NOT operator" },
        ];

        for (const opInfo of operators) {
            if (context.includes(opInfo.op)) {
                const markdown = new vscode.MarkdownString();
                markdown.appendCodeblock(opInfo.op, "ssl");
                markdown.appendMarkdown(`**SSL Operator**\n\n${opInfo.desc}`);
                return new vscode.Hover(markdown);
            }
        }

        return null;
    }

    /**
     * Get hover information for literals
     */
    private getLiteralHover(word: string): vscode.Hover | null {
        if (word === ".T." || word === ".F.") {
            const markdown = new vscode.MarkdownString();
            markdown.appendCodeblock(word, "ssl");
            markdown.appendMarkdown(`**SSL Boolean Literal**\n\n`);
            markdown.appendMarkdown(word === ".T." ? "Boolean TRUE value" : "Boolean FALSE value");
            return new vscode.Hover(markdown);
        }

        if (word === "NIL") {
            const markdown = new vscode.MarkdownString();
            markdown.appendCodeblock("NIL", "ssl");
            markdown.appendMarkdown(`**SSL Null Literal**\n\nRepresents a null or undefined value`);
            return new vscode.Hover(markdown);
        }

        return null;
    }

    /**
     * Get hover information for identifiers (variables, procedures)
     */
    private getIdentifierHover(
        identifier: string,
        document: vscode.TextDocument,
        position: vscode.Position
    ): vscode.Hover | null {
        // This is a simplified implementation
        // In a full implementation, you would parse the document to find variable declarations,
        // procedure definitions, etc.

        const markdown = new vscode.MarkdownString();

        // Check if it looks like Hungarian notation
        const hungarianInfo = this.getHungarianNotationInfo(identifier);
        if (hungarianInfo) {
            markdown.appendCodeblock(identifier, "ssl");
            markdown.appendMarkdown(`**${hungarianInfo.type}**\n\n`);
            markdown.appendMarkdown(
                `Hungarian notation suggests this is a ${hungarianInfo.description}`
            );
            return new vscode.Hover(markdown);
        }

        // Generic identifier info
        markdown.appendCodeblock(identifier, "ssl");
        markdown.appendMarkdown(`**SSL Identifier**\n\nVariable or procedure name`);

        return new vscode.Hover(markdown);
    }

    /**
     * Get SSL keyword information
     */
    private getSSLKeywordInfo(keyword: string): KeywordInfo | null {
        const keywords: { [key: string]: KeywordInfo } = {
            IF: {
                category: "Control Flow",
                description:
                    "Begins a conditional statement block. Executes the following statements if the condition is true.",
                syntax: ":IF condition;\n  // statements\n:ENDIF;",
                example: ':IF nAge >= 18;\n  sStatus := "Adult";\n:ENDIF;',
                relatedKeywords: ["ELSE", "ENDIF"],
            },
            ELSE: {
                category: "Control Flow",
                description:
                    "Provides an alternative branch in an IF statement that executes when the condition is false.",
                syntax: ":IF condition;\n  // true branch\n:ELSE;\n  // false branch\n:ENDIF;",
                example:
                    ':IF nAge >= 18;\n  sStatus := "Adult";\n:ELSE;\n  sStatus := "Minor";\n:ENDIF;',
                relatedKeywords: ["IF", "ENDIF"],
            },
            WHILE: {
                category: "Control Flow",
                description: "Creates a loop that continues as long as the condition is true.",
                syntax: ":WHILE condition;\n  // statements\n:ENDWHILE;",
                example: ":WHILE nCount < 10;\n  nCount := nCount + 1;\n:ENDWHILE;",
                relatedKeywords: ["ENDWHILE", "EXITWHILE", "LOOP"],
            },
            FOR: {
                category: "Control Flow",
                description:
                    "Creates a counting loop that iterates from a start value to an end value.",
                syntax: ":FOR variable := start :TO end;\n  // statements\n:NEXT;",
                example: ':FOR nI := 1 :TO 10;\n  ? "Number: " + Str(nI);\n:NEXT;',
                relatedKeywords: ["NEXT", "EXITFOR"],
            },
            PROCEDURE: {
                category: "Procedure",
                description:
                    "Defines a procedure (function) that can be called from other parts of the code.",
                syntax: ":PROCEDURE name;\n  :PARAMETERS param1, param2;\n  // body\n  :RETURN value;\n:ENDPROC;",
                example:
                    ":PROCEDURE CalculateAge;\n  :PARAMETERS dBirthDate;\n  :RETURN Today() - dBirthDate;\n:ENDPROC;",
                relatedKeywords: ["ENDPROC", "PARAMETERS", "RETURN"],
            },
            BEGINCASE: {
                category: "Control Flow",
                description:
                    "Begins a multi-way conditional statement similar to switch/case in other languages.",
                syntax: ":BEGINCASE;\n:CASE value1;\n  // statements\n:OTHERWISE;\n  // default\n:ENDCASE;",
                example:
                    ':BEGINCASE;\n:CASE sGrade;\n  sDescription := "Pass";\n:OTHERWISE;\n  sDescription := "Fail";\n:ENDCASE;',
                relatedKeywords: ["CASE", "OTHERWISE", "ENDCASE", "EXITCASE"],
            },
            TRY: {
                category: "Error Handling",
                description: "Begins a try-catch block for error handling.",
                syntax: ":TRY;\n  // try block\n:CATCH;\n  // catch block\n:ENDTRY;",
                example:
                    ":TRY;\n  nResult := nValue / nDivisor;\n:CATCH;\n  nResult := 0;\n:ENDTRY;",
                relatedKeywords: ["CATCH", "FINALLY", "ENDTRY"],
            },
            CLASS: {
                category: "Object-Oriented",
                description: "Defines a class for object-oriented programming.",
                syntax: ":CLASS className;\n  :DECLARE field1, field2;\n  :PROCEDURE method;\n  :ENDPROC;",
                example:
                    ':CLASS clsCar;\n  :DECLARE sModel, nYear;\n  :PROCEDURE GetInfo;\n    :RETURN sModel + " " + Str(nYear);\n  :ENDPROC;',
                relatedKeywords: ["INHERIT", "DECLARE"],
            },
        };

        return keywords[keyword] || null;
    }

    /**
     * Get SSL function information
     */
    private getSSLFunctionInfo(functionName: string): FunctionInfo | null {
        const functions: { [key: string]: FunctionInfo } = {
            Len: {
                category: "String Function",
                description: "Returns the length of a string.",
                parameters: "string",
                returns: "Number - the length of the string",
                parameterDescriptions: [{ name: "string", description: "The string to measure" }],
                example: 'nLength := Len("Hello World"); // Returns 11',
            },
            SubStr: {
                category: "String Function",
                description: "Extracts a substring from a string.",
                parameters: "string, start, length",
                returns: "String - the extracted substring",
                parameterDescriptions: [
                    { name: "string", description: "The source string" },
                    { name: "start", description: "Starting position (1-based)" },
                    { name: "length", description: "Number of characters to extract" },
                ],
                example: 'sResult := SubStr("Hello World", 7, 5); // Returns "World"',
            },
            SqlExecute: {
                category: "Database Function",
                description: "Executes an SQL statement against the database.",
                parameters: "sql, parameters",
                returns: "Result set or success indicator",
                parameterDescriptions: [
                    { name: "sql", description: "SQL statement with parameter placeholders" },
                    { name: "parameters", description: "Array of parameter values" },
                ],
                example: 'SqlExecute("SELECT * FROM Users WHERE Age > ?age?", {nMinAge});',
            },
            Today: {
                category: "Date Function",
                description: "Returns the current system date.",
                parameters: "",
                returns: "Date - current date",
                parameterDescriptions: [],
                example: "dToday := Today(); // Gets current date",
            },
            IIF: {
                category: "Logical Function",
                description:
                    "Inline conditional function - returns one of two values based on a condition.",
                parameters: "condition, trueValue, falseValue",
                returns: "The true value if condition is true, otherwise the false value",
                parameterDescriptions: [
                    { name: "condition", description: "Boolean expression to evaluate" },
                    { name: "trueValue", description: "Value returned if condition is true" },
                    { name: "falseValue", description: "Value returned if condition is false" },
                ],
                example: 'sStatus := IIF(nAge >= 18, "Adult", "Minor");',
            },
        };

        return functions[functionName] || null;
    }

    /**
     * Get Hungarian notation information
     */
    private getHungarianNotationInfo(identifier: string): HungarianInfo | null {
        const prefixMap: { [key: string]: HungarianInfo } = {
            n: { type: "Number Variable", description: "numeric value" },
            s: { type: "String Variable", description: "string value" },
            b: { type: "Boolean Variable", description: "boolean value" },
            d: { type: "Date Variable", description: "date value" },
            a: { type: "Array Variable", description: "array value" },
            o: { type: "Object Variable", description: "object reference" },
            cls: { type: "Class Definition", description: "class definition" },
            proc: { type: "Procedure", description: "procedure or function" },
        };

        // Check for multi-character prefixes first
        if (identifier.toLowerCase().startsWith("cls")) {
            return prefixMap["cls"];
        }
        if (identifier.toLowerCase().startsWith("proc")) {
            return prefixMap["proc"];
        }

        // Check single character prefixes
        const firstChar = identifier.charAt(0).toLowerCase();
        return prefixMap[firstChar] || null;
    }
}

/**
 * Keyword information interface
 */
interface KeywordInfo {
    category: string;
    description: string;
    syntax?: string;
    example?: string;
    relatedKeywords?: string[];
}

/**
 * Function information interface
 */
interface FunctionInfo {
    category: string;
    description: string;
    parameters: string;
    returns: string;
    parameterDescriptions: ParameterInfo[];
    example: string;
}

/**
 * Parameter information interface
 */
interface ParameterInfo {
    name: string;
    description: string;
}

/**
 * Hungarian notation information interface
 */
interface HungarianInfo {
    type: string;
    description: string;
}
