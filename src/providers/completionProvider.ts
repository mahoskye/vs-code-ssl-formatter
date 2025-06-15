/**
 * SSL Completion Provider
 *
 * Provides auto-completion suggestions for SSL language constructs based on the EBNF grammar.
 * Supports completion for:
 * - SSL keywords (:PROCEDURE, :IF, :WHILE, etc.)
 * - Built-in functions and operators
 * - Variable names and procedure names
 * - SQL keywords and functions
 * - Custom completions from configuration
 */

import * as vscode from "vscode";
import { TokenType } from "../tokenizer/tokenType";
import { Tokenizer } from "../tokenizer";

/**
 * SSL Completion Item Provider
 */
export class SSLCompletionProvider implements vscode.CompletionItemProvider {
    /**
     * Provide completion items for the given position
     */
    public provideCompletionItems(
        document: vscode.TextDocument,
        position: vscode.Position,
        token: vscode.CancellationToken,
        context: vscode.CompletionContext
    ): vscode.ProviderResult<vscode.CompletionItem[] | vscode.CompletionList> {
        try {
            const completions: vscode.CompletionItem[] = [];

            // Get the current line and context
            const line = document.lineAt(position.line);
            const textBeforeCursor = line.text.substring(0, position.character);
            const textAfterCursor = line.text.substring(position.character);

            // Determine completion context
            const completionContext = this.getCompletionContext(textBeforeCursor, textAfterCursor);

            // Add SSL keyword completions
            if (completionContext.shouldShowKeywords) {
                completions.push(...this.getSSLKeywordCompletions(completionContext));
            }

            // Add function completions
            if (completionContext.shouldShowFunctions) {
                completions.push(...this.getSSLFunctionCompletions());
            }

            // Add operator completions
            if (completionContext.shouldShowOperators) {
                completions.push(...this.getSSLOperatorCompletions());
            }

            // Add snippet completions
            if (completionContext.shouldShowSnippets) {
                completions.push(...this.getSSLSnippetCompletions());
            }

            // Add custom completions from configuration
            completions.push(...this.getCustomCompletions());

            return completions;
        } catch (error) {
            console.error("Error providing SSL completions:", error);
            return [];
        }
    }

    /**
     * Resolve additional details for a completion item
     */
    public resolveCompletionItem(
        item: vscode.CompletionItem,
        token: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.CompletionItem> {
        // Add additional documentation or details if needed
        return item;
    }

    /**
     * Get completion context based on the cursor position
     */
    private getCompletionContext(textBefore: string, textAfter: string): CompletionContext {
        const context: CompletionContext = {
            shouldShowKeywords: false,
            shouldShowFunctions: true,
            shouldShowOperators: true,
            shouldShowSnippets: true,
            isAfterColon: false,
            isInStringLiteral: false,
            isInComment: false,
        };

        // Check if we're after a colon (SSL keywords start with :)
        if (textBefore.trim().endsWith(":")) {
            context.isAfterColon = true;
            context.shouldShowKeywords = true;
            context.shouldShowFunctions = false;
            context.shouldShowOperators = false;
        }

        // Check if we're in a string literal
        const stringMatches = textBefore.match(/(['"])/g);
        if (stringMatches && stringMatches.length % 2 !== 0) {
            context.isInStringLiteral = true;
            context.shouldShowKeywords = false;
            context.shouldShowFunctions = false;
            context.shouldShowOperators = false;
            context.shouldShowSnippets = false;
        }

        // Check if we're in a comment
        if (textBefore.includes("/*") && !textBefore.includes(";")) {
            context.isInComment = true;
            context.shouldShowKeywords = false;
            context.shouldShowFunctions = false;
            context.shouldShowOperators = false;
            context.shouldShowSnippets = false;
        }

        return context;
    }

    /**
     * Get SSL keyword completions based on EBNF grammar
     */
    private getSSLKeywordCompletions(context: CompletionContext): vscode.CompletionItem[] {
        const keywords = [
            // Control Flow Keywords
            {
                keyword: "IF",
                detail: "Conditional statement",
                insertText: "IF ${1:condition};\n\t${2:// statements}\n:ENDIF;",
            },
            { keyword: "ELSE", detail: "Else branch", insertText: "ELSE;\n\t${1:// statements}" },
            { keyword: "ENDIF", detail: "End if statement", insertText: "ENDIF;" },
            {
                keyword: "WHILE",
                detail: "While loop",
                insertText: "WHILE ${1:condition};\n\t${2:// statements}\n:ENDWHILE;",
            },
            { keyword: "ENDWHILE", detail: "End while loop", insertText: "ENDWHILE;" },
            {
                keyword: "FOR",
                detail: "For loop",
                insertText:
                    "FOR ${1:var} := ${2:start} :TO ${3:end};\n\t${4:// statements}\n:NEXT;",
            },
            { keyword: "NEXT", detail: "Next iteration", insertText: "NEXT;" },
            { keyword: "EXITWHILE", detail: "Exit while loop", insertText: "EXITWHILE;" },
            { keyword: "EXITFOR", detail: "Exit for loop", insertText: "EXITFOR;" },
            { keyword: "LOOP", detail: "Continue loop iteration", insertText: "LOOP;" },

            // Procedure Keywords
            {
                keyword: "PROCEDURE",
                detail: "Procedure definition",
                insertText: "PROCEDURE ${1:name};\n\t${2:// body}\n:ENDPROC;",
            },
            { keyword: "ENDPROC", detail: "End procedure", insertText: "ENDPROC;" },
            {
                keyword: "PARAMETERS",
                detail: "Parameter declaration",
                insertText: "PARAMETERS ${1:param1}, ${2:param2};",
            },
            {
                keyword: "DEFAULT",
                detail: "Default parameter values",
                insertText: "DEFAULT ${1:param}, ${2:value};",
            },
            { keyword: "RETURN", detail: "Return statement", insertText: "RETURN ${1:value};" },

            // Declaration Keywords
            {
                keyword: "DECLARE",
                detail: "Variable declaration",
                insertText: "DECLARE ${1:var1}, ${2:var2};",
            },
            {
                keyword: "PUBLIC",
                detail: "Public declaration",
                insertText: "PUBLIC ${1:var1}, ${2:var2};",
            },
            {
                keyword: "INCLUDE",
                detail: "Include statement",
                insertText: 'INCLUDE "${1:filename}";',
            },

            // Switch Case Keywords
            {
                keyword: "BEGINCASE",
                detail: "Begin case statement",
                insertText: "BEGINCASE;\n:CASE ${1:value};\n\t${2:// statements}\n:ENDCASE;",
            },
            {
                keyword: "CASE",
                detail: "Case branch",
                insertText: "CASE ${1:value};\n\t${2:// statements}",
            },
            {
                keyword: "OTHERWISE",
                detail: "Default case",
                insertText: "OTHERWISE;\n\t${1:// statements}",
            },
            { keyword: "ENDCASE", detail: "End case statement", insertText: "ENDCASE;" },
            { keyword: "EXITCASE", detail: "Exit case statement", insertText: "EXITCASE;" },

            // Error Handling Keywords
            {
                keyword: "TRY",
                detail: "Try block",
                insertText:
                    "TRY;\n\t${1:// try statements}\n:CATCH;\n\t${2:// catch statements}\n:ENDTRY;",
            },
            {
                keyword: "CATCH",
                detail: "Catch block",
                insertText: "CATCH;\n\t${1:// catch statements}",
            },
            {
                keyword: "FINALLY",
                detail: "Finally block",
                insertText: "FINALLY;\n\t${1:// finally statements}",
            },
            { keyword: "ENDTRY", detail: "End try block", insertText: "ENDTRY;" },
            {
                keyword: "ERROR",
                detail: "Error handling",
                insertText: "ERROR;\n\t${1:// error handling}",
            },

            // Class Keywords
            { keyword: "CLASS", detail: "Class definition", insertText: "CLASS ${1:className};" },
            {
                keyword: "INHERIT",
                detail: "Class inheritance",
                insertText: "INHERIT ${1:baseClass};",
            },

            // Region Keywords
            { keyword: "REGION", detail: "Code region", insertText: "REGION ${1:regionName};" },
            { keyword: "ENDREGION", detail: "End code region", insertText: "ENDREGION;" },

            // Inline Code Keywords
            {
                keyword: "BEGININLINECODE",
                detail: "Begin inline code",
                insertText: "BEGININLINECODE;\n\t${1:// inline code}\n:ENDINLINECODE;",
            },
            { keyword: "ENDINLINECODE", detail: "End inline code", insertText: "ENDINLINECODE;" },

            // Label
            { keyword: "LABEL", detail: "Label statement", insertText: "LABEL ${1:labelName};" },
        ];

        return keywords.map((k) => {
            const item = new vscode.CompletionItem(k.keyword, vscode.CompletionItemKind.Keyword);
            item.detail = k.detail;
            item.insertText = new vscode.SnippetString(k.insertText);
            item.documentation = new vscode.MarkdownString(
                `SSL keyword: **:${k.keyword}**\n\n${k.detail}`
            );
            return item;
        });
    }

    /**
     * Get SSL function completions
     */
    private getSSLFunctionCompletions(): vscode.CompletionItem[] {
        const functions = [
            // String Functions
            { name: "LTrim", detail: "Remove leading spaces", insertText: "LTrim(${1:string})" },
            { name: "RTrim", detail: "Remove trailing spaces", insertText: "RTrim(${1:string})" },
            {
                name: "AllTrim",
                detail: "Remove leading and trailing spaces",
                insertText: "AllTrim(${1:string})",
            },
            {
                name: "SubStr",
                detail: "Extract substring",
                insertText: "SubStr(${1:string}, ${2:start}, ${3:length})",
            },
            { name: "Len", detail: "String length", insertText: "Len(${1:string})" },
            { name: "Upper", detail: "Convert to uppercase", insertText: "Upper(${1:string})" },
            { name: "Lower", detail: "Convert to lowercase", insertText: "Lower(${1:string})" },
            { name: "Str", detail: "Convert to string", insertText: "Str(${1:value})" },
            { name: "Val", detail: "Convert to number", insertText: "Val(${1:string})" },

            // Date Functions
            { name: "Today", detail: "Current date", insertText: "Today()" },
            { name: "Time", detail: "Current time", insertText: "Time()" },
            { name: "CtoD", detail: "Convert string to date", insertText: "CtoD(${1:dateString})" },
            { name: "DtoC", detail: "Convert date to string", insertText: "DtoC(${1:date})" },

            // Array Functions
            { name: "ALen", detail: "Array length", insertText: "ALen(${1:array})" },
            {
                name: "AAdd",
                detail: "Add element to array",
                insertText: "AAdd(${1:array}, ${2:element})",
            },
            {
                name: "ADel",
                detail: "Delete array element",
                insertText: "ADel(${1:array}, ${2:index})",
            },
            { name: "AScan", detail: "Search array", insertText: "AScan(${1:array}, ${2:value})" },

            // Database Functions
            {
                name: "SqlExecute",
                detail: "Execute SQL statement",
                insertText: "SqlExecute(${1:sql}, ${2:parameters})",
            },
            {
                name: "LSearch",
                detail: "Database search",
                insertText: "LSearch(${1:table}, ${2:condition})",
            },

            // Object Functions
            {
                name: "CreateUDObject",
                detail: "Create user-defined object",
                insertText: "CreateUDObject(${1:className})",
            },
            {
                name: "DoProc",
                detail: "Execute procedure",
                insertText: "DoProc(${1:procName}, ${2:parameters})",
            },
            {
                name: "ExecFunction",
                detail: "Execute function",
                insertText: "ExecFunction(${1:funcName}, ${2:parameters})",
            },
            {
                name: "ExecUDF",
                detail: "Execute user-defined function",
                insertText: "ExecUDF(${1:code}, ${2:parameters})",
            },

            // Logical Functions
            {
                name: "IIF",
                detail: "Inline if function",
                insertText: "IIF(${1:condition}, ${2:trueValue}, ${3:falseValue})",
            },
            { name: "IsNull", detail: "Check for null value", insertText: "IsNull(${1:value})" },
            { name: "Empty", detail: "Check if empty", insertText: "Empty(${1:value})" },

            // Bitwise Operations (as functions)
            { name: "_AND", detail: "Bitwise AND", insertText: "_AND(${1:value1}, ${2:value2})" },
            { name: "_OR", detail: "Bitwise OR", insertText: "_OR(${1:value1}, ${2:value2})" },
            { name: "_NOT", detail: "Bitwise NOT", insertText: "_NOT(${1:value})" },

            // Utility Functions
            { name: "Branch", detail: "Branch to label", insertText: "Branch(${1:label})" },
            { name: "Type", detail: "Get variable type", insertText: "Type(${1:variable})" },
        ];

        return functions.map((f) => {
            const item = new vscode.CompletionItem(f.name, vscode.CompletionItemKind.Function);
            item.detail = f.detail;
            item.insertText = new vscode.SnippetString(f.insertText);
            item.documentation = new vscode.MarkdownString(
                `SSL function: **${f.name}**\n\n${f.detail}`
            );
            return item;
        });
    }

    /**
     * Get SSL operator completions
     */
    private getSSLOperatorCompletions(): vscode.CompletionItem[] {
        const operators = [
            { op: ":=", detail: "Assignment operator", kind: vscode.CompletionItemKind.Operator },
            { op: "+=", detail: "Add and assign", kind: vscode.CompletionItemKind.Operator },
            { op: "-=", detail: "Subtract and assign", kind: vscode.CompletionItemKind.Operator },
            { op: "*=", detail: "Multiply and assign", kind: vscode.CompletionItemKind.Operator },
            { op: "/=", detail: "Divide and assign", kind: vscode.CompletionItemKind.Operator },
            { op: "^=", detail: "Power and assign", kind: vscode.CompletionItemKind.Operator },
            { op: "==", detail: "Equality comparison", kind: vscode.CompletionItemKind.Operator },
            { op: "!=", detail: "Inequality comparison", kind: vscode.CompletionItemKind.Operator },
            { op: "<=", detail: "Less than or equal", kind: vscode.CompletionItemKind.Operator },
            { op: ">=", detail: "Greater than or equal", kind: vscode.CompletionItemKind.Operator },
            { op: ".AND.", detail: "Logical AND", kind: vscode.CompletionItemKind.Operator },
            { op: ".OR.", detail: "Logical OR", kind: vscode.CompletionItemKind.Operator },
            { op: ".NOT.", detail: "Logical NOT", kind: vscode.CompletionItemKind.Operator },
            { op: ".T.", detail: "Boolean TRUE", kind: vscode.CompletionItemKind.Value },
            { op: ".F.", detail: "Boolean FALSE", kind: vscode.CompletionItemKind.Value },
            { op: "NIL", detail: "Null value", kind: vscode.CompletionItemKind.Value },
        ];

        return operators.map((o) => {
            const item = new vscode.CompletionItem(o.op, o.kind);
            item.detail = o.detail;
            item.insertText = o.op;
            item.documentation = new vscode.MarkdownString(
                `SSL operator: **${o.op}**\n\n${o.detail}`
            );
            return item;
        });
    }

    /**
     * Get SSL snippet completions
     */
    private getSSLSnippetCompletions(): vscode.CompletionItem[] {
        const snippets = [
            {
                label: "proc",
                detail: "Procedure template",
                insertText:
                    ":PROCEDURE ${1:name};\n\t:PARAMETERS ${2:param1}, ${3:param2};\n\t:DECLARE ${4:localVar};\n\t\n\t${5:// procedure body}\n\t\n\t:RETURN ${6:result};\n:ENDPROC;",
                documentation: "Complete procedure template with parameters and local variables",
            },
            {
                label: "class",
                detail: "Class template",
                insertText:
                    ":CLASS ${1:className};\n\t:DECLARE ${2:field1}, ${3:field2};\n\t\n\t:PROCEDURE ${4:methodName};\n\t\t${5:// method body}\n\t:ENDPROC;\n",
                documentation: "Complete class template with fields and methods",
            },
            {
                label: "sql",
                detail: "SQL execution template",
                insertText:
                    'SqlExecute("${1:SELECT * FROM table WHERE condition = ?param1?}", {${2:param1}});',
                documentation: "SQL execution with parameterized query",
            },
            {
                label: "try",
                detail: "Try-catch template",
                insertText:
                    ":TRY;\n\t${1:// try block}\n:CATCH;\n\t${2:// catch block}\n:FINALLY;\n\t${3:// finally block}\n:ENDTRY;",
                documentation: "Complete try-catch-finally block",
            },
        ];

        return snippets.map((s) => {
            const item = new vscode.CompletionItem(s.label, vscode.CompletionItemKind.Snippet);
            item.detail = s.detail;
            item.insertText = new vscode.SnippetString(s.insertText);
            item.documentation = new vscode.MarkdownString(s.documentation);
            return item;
        });
    }

    /**
     * Get custom completions from configuration
     */
    private getCustomCompletions(): vscode.CompletionItem[] {
        const config = vscode.workspace.getConfiguration("sslFormatter");
        const customCompletions = config.get("completions", {}) as any;

        const completions: vscode.CompletionItem[] = [];

        // Process each category of custom completions
        Object.keys(customCompletions).forEach((category) => {
            const items = customCompletions[category];
            if (Array.isArray(items)) {
                items.forEach((item: any) => {
                    const completion = new vscode.CompletionItem(
                        item.label,
                        this.getCompletionItemKind(item.kind)
                    );
                    completion.detail = item.detail || category;
                    completion.documentation = item.documentation;
                    if (item.insertText) {
                        completion.insertText = new vscode.SnippetString(item.insertText);
                    }
                    completions.push(completion);
                });
            }
        });

        return completions;
    }

    /**
     * Convert string kind to VS Code CompletionItemKind
     */
    private getCompletionItemKind(kind: string): vscode.CompletionItemKind {
        switch (kind?.toLowerCase()) {
            case "function":
                return vscode.CompletionItemKind.Function;
            case "method":
                return vscode.CompletionItemKind.Method;
            case "variable":
                return vscode.CompletionItemKind.Variable;
            case "field":
                return vscode.CompletionItemKind.Field;
            case "class":
                return vscode.CompletionItemKind.Class;
            case "keyword":
                return vscode.CompletionItemKind.Keyword;
            case "operator":
                return vscode.CompletionItemKind.Operator;
            case "snippet":
                return vscode.CompletionItemKind.Snippet;
            case "value":
                return vscode.CompletionItemKind.Value;
            default:
                return vscode.CompletionItemKind.Text;
        }
    }
}

/**
 * Completion context information
 */
interface CompletionContext {
    shouldShowKeywords: boolean;
    shouldShowFunctions: boolean;
    shouldShowOperators: boolean;
    shouldShowSnippets: boolean;
    isAfterColon: boolean;
    isInStringLiteral: boolean;
    isInComment: boolean;
}
