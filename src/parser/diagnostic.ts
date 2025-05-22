/**
 * SSL Diagnostic Provider
 * Provides diagnostics for SSL code
 */

import * as vscode from "vscode";
import { SSLParser, DiagnosticSeverity as ParserDiagnosticSeverity } from "./sslParser";

/**
 * Provides diagnostic information for SSL code
 */
export class SSLDiagnosticProvider {
    private diagnosticCollection: vscode.DiagnosticCollection;
    private parser: SSLParser;

    /**
     * Creates a new instance of the SSLDiagnosticProvider
     */
    constructor() {
        this.diagnosticCollection = vscode.languages.createDiagnosticCollection("ssl");
        this.parser = new SSLParser();
    }

    /**
     * Disposes of the diagnostic collection
     */
    public dispose(): void {
        this.diagnosticCollection.dispose();
    }
    /**
     * Updates diagnostics for a document
     * @param document The document to update diagnostics for
     */
    public updateDiagnostics(document: vscode.TextDocument): void {
        if (document.languageId !== "ssl") {
            return;
        }

        const text = document.getText();
        const { errors } = this.parser.parse(text);

        // Basic EBNF grammar validation errors from parser
        const diagnostics: vscode.Diagnostic[] = errors.map((error) => {
            const range = new vscode.Range(
                error.range.start.line,
                error.range.start.character,
                error.range.end.line,
                error.range.end.character
            );

            const diagnostic = new vscode.Diagnostic(
                range,
                error.message,
                this.convertSeverity(error.severity)
            );

            diagnostic.source = "SSL Validator";
            return diagnostic;
        });

        // Add additional EBNF grammar validation
        this.validateAgainstEBNF(document, text, diagnostics);

        this.diagnosticCollection.set(document.uri, diagnostics);
    }

    /**
     * Additional validation against the EBNF grammar
     * @param document The document
     * @param text The document text
     * @param diagnostics The diagnostics collection to add to
     */
    private validateAgainstEBNF(
        document: vscode.TextDocument,
        text: string,
        diagnostics: vscode.Diagnostic[]
    ): void {
        // Check for unmatched block structures based on EBNF grammar
        this.validateBlockStructures(document, text, diagnostics);

        // Check for incorrect comment syntax
        this.validateCommentSyntax(document, text, diagnostics);

        // Check for correct property access syntax (using colon)
        this.validatePropertyAccess(document, text, diagnostics);

        // Check for special SSL string literals
        this.validateStringLiterals(document, text, diagnostics);

        // Validate boolean literals (.T. and .F.)
        this.validateBooleanLiterals(document, text, diagnostics);
    }

    /**
     * Validates block structures like IF/ENDIF, WHILE/ENDWHILE, etc.
     */
    private validateBlockStructures(
        document: vscode.TextDocument,
        text: string,
        diagnostics: vscode.Diagnostic[]
    ): void {
        // Stacks for tracking block structures
        const ifStack: vscode.Position[] = [];
        const whileStack: vscode.Position[] = [];
        const forStack: vscode.Position[] = [];
        const procedureStack: vscode.Position[] = [];
        const tryStack: vscode.Position[] = [];
        const caseStack: vscode.Position[] = [];

        // Regex for finding block start/end keywords
        const blockRegex =
            /:(IF|ELSE|ENDIF|WHILE|ENDWHILE|FOR|NEXT|PROCEDURE|ENDPROC|TRY|CATCH|FINALLY|ENDTRY|BEGINCASE|CASE|OTHERWISE|ENDCASE)\b/g;

        let match: RegExpExecArray | null;
        while ((match = blockRegex.exec(text)) !== null) {
            const keyword = match[1];
            const position = document.positionAt(match.index);

            // Validate block structure based on keyword
            switch (keyword) {
                case "IF":
                    ifStack.push(position);
                    break;
                case "ENDIF":
                    if (ifStack.length === 0) {
                        diagnostics.push(
                            this.createMismatchedBlockDiagnostic(position, "ENDIF", "IF")
                        );
                    } else {
                        ifStack.pop();
                    }
                    break;
                case "WHILE":
                    whileStack.push(position);
                    break;
                case "ENDWHILE":
                    if (whileStack.length === 0) {
                        diagnostics.push(
                            this.createMismatchedBlockDiagnostic(position, "ENDWHILE", "WHILE")
                        );
                    } else {
                        whileStack.pop();
                    }
                    break;
                case "PROCEDURE":
                    procedureStack.push(position);
                    break;
                case "ENDPROC":
                    if (procedureStack.length === 0) {
                        diagnostics.push(
                            this.createMismatchedBlockDiagnostic(position, "ENDPROC", "PROCEDURE")
                        );
                    } else {
                        procedureStack.pop();
                    }
                    break;
                // Add other block validations as needed
            }
        }

        // Report unclosed blocks
        ifStack.forEach((position) => {
            diagnostics.push(this.createUnclosedBlockDiagnostic(position, "IF", "ENDIF"));
        });

        whileStack.forEach((position) => {
            diagnostics.push(this.createUnclosedBlockDiagnostic(position, "WHILE", "ENDWHILE"));
        });

        procedureStack.forEach((position) => {
            diagnostics.push(this.createUnclosedBlockDiagnostic(position, "PROCEDURE", "ENDPROC"));
        });

        // Add other unclosed block checks as needed
    }

    /**
     * Creates a diagnostic for mismatched block structures
     */
    private createMismatchedBlockDiagnostic(
        position: vscode.Position,
        found: string,
        expected: string
    ): vscode.Diagnostic {
        const range = new vscode.Range(position, position.translate(0, found.length + 1));
        return new vscode.Diagnostic(
            range,
            `Found :${found} without matching :${expected}`,
            vscode.DiagnosticSeverity.Error
        );
    }

    /**
     * Creates a diagnostic for unclosed block structures
     */
    private createUnclosedBlockDiagnostic(
        position: vscode.Position,
        blockType: string,
        closing: string
    ): vscode.Diagnostic {
        const range = new vscode.Range(position, position.translate(0, blockType.length + 1));
        return new vscode.Diagnostic(
            range,
            `Unclosed :${blockType} block, expected :${closing}`,
            vscode.DiagnosticSeverity.Error
        );
    }

    /**
     * Validates SSL comment syntax (/* ... ;)
     */
    private validateCommentSyntax(
        document: vscode.TextDocument,
        text: string,
        diagnostics: vscode.Diagnostic[]
    ): void {
        // Find potential comments
        const commentStartRegex = /\/\*/g;
        let match: RegExpExecArray | null;

        while ((match = commentStartRegex.exec(text)) !== null) {
            const startPos = document.positionAt(match.index);
            const startLineText = document.lineAt(startPos.line).text;

            // Find the end of this comment
            let found = false;
            let searchLine = startPos.line;
            const maxLines = 100; // Limit search to avoid infinite loops

            for (let i = 0; i < maxLines && searchLine < document.lineCount; i++) {
                const lineText = document.lineAt(searchLine).text;
                const semicolonIndex = lineText.indexOf(
                    ";",
                    searchLine === startPos.line ? startPos.character + 2 : 0
                );

                if (semicolonIndex >= 0) {
                    found = true;
                    break;
                }

                searchLine++;
            }

            if (!found) {
                // Comment does not end with semicolon
                const endOfLine = document.lineAt(startPos.line).range.end;
                const range = new vscode.Range(startPos, endOfLine);

                diagnostics.push(
                    new vscode.Diagnostic(
                        range,
                        "Comment starts with /* but does not end with ;",
                        vscode.DiagnosticSeverity.Error
                    )
                );
            }
        }
    }

    /**
     * Validates property access (using colon instead of dot)
     */
    private validatePropertyAccess(
        document: vscode.TextDocument,
        text: string,
        diagnostics: vscode.Diagnostic[]
    ): void {
        // Look for potential incorrect property access (using dot instead of colon)
        const incorrectPropAccessRegex = /\b(\w+)\.(\w+)\b/g;
        let match: RegExpExecArray | null;

        while ((match = incorrectPropAccessRegex.exec(text)) !== null) {
            // Exclude cases that might be valid like numbers with decimal points
            const objectName = match[1];
            const propName = match[2];

            // Skip if left side looks like a number or right side is numeric
            if (!/^\d+$/.test(objectName) && !/^\d+$/.test(propName)) {
                const startPos = document.positionAt(match.index + objectName.length);
                const endPos = document.positionAt(match.index + objectName.length + 1);
                const range = new vscode.Range(startPos, endPos);

                diagnostics.push(
                    new vscode.Diagnostic(
                        range,
                        "SSL uses colon (:) for property access, not dot (.)",
                        vscode.DiagnosticSeverity.Warning
                    )
                );
            }
        }
    }

    /**
     * Validates string literals (can be in double quotes, single quotes, or square brackets)
     */
    private validateStringLiterals(
        document: vscode.TextDocument,
        text: string,
        diagnostics: vscode.Diagnostic[]
    ): void {
        // Validate square bracket strings
        const bracketStringRegex = /\[([^\]]*)\]/g;
        let match: RegExpExecArray | null;

        while ((match = bracketStringRegex.exec(text)) !== null) {
            // Square bracket strings are valid in SSL, but check for nesting
            const content = match[1];
            if (content.includes("[") || content.includes("]")) {
                const startPos = document.positionAt(match.index);
                const endPos = document.positionAt(match.index + match[0].length);
                const range = new vscode.Range(startPos, endPos);

                diagnostics.push(
                    new vscode.Diagnostic(
                        range,
                        "Nested square brackets in string literals are not supported",
                        vscode.DiagnosticSeverity.Error
                    )
                );
            }
        }
    }

    /**
     * Validates boolean literals (.T. and .F.)
     */
    private validateBooleanLiterals(
        document: vscode.TextDocument,
        text: string,
        diagnostics: vscode.Diagnostic[]
    ): void {
        // Find potential incorrect boolean literals (TRUE, FALSE instead of .T., .F.)
        const incorrectBooleanRegex = /\b(TRUE|FALSE)\b/g;
        let match: RegExpExecArray | null;

        while ((match = incorrectBooleanRegex.exec(text)) !== null) {
            const startPos = document.positionAt(match.index);
            const endPos = document.positionAt(match.index + match[0].length);
            const range = new vscode.Range(startPos, endPos);

            const replacement = match[1] === "TRUE" ? ".T." : ".F.";

            diagnostics.push(
                new vscode.Diagnostic(
                    range,
                    `SSL uses ${replacement} instead of ${match[1]} for boolean literals`,
                    vscode.DiagnosticSeverity.Warning
                )
            );
        }
    }

    /**
     * Clears diagnostics for a document
     * @param document The document to clear diagnostics for
     */
    public clearDiagnostics(document: vscode.TextDocument): void {
        this.diagnosticCollection.delete(document.uri);
    }

    /**
     * Converts a parser diagnostic severity to a vscode diagnostic severity
     * @param severity The parser diagnostic severity
     * @returns The vscode diagnostic severity
     */ private convertSeverity(severity: ParserDiagnosticSeverity): vscode.DiagnosticSeverity {
        switch (severity) {
            case ParserDiagnosticSeverity.error:
                return vscode.DiagnosticSeverity.Error;
            case ParserDiagnosticSeverity.warning:
                return vscode.DiagnosticSeverity.Warning;
            case ParserDiagnosticSeverity.information:
                return vscode.DiagnosticSeverity.Information;
            case ParserDiagnosticSeverity.hint:
                return vscode.DiagnosticSeverity.Hint;
            default:
                return vscode.DiagnosticSeverity.Error;
        }
    }
}
