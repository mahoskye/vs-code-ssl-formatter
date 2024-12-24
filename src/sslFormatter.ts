import * as vscode from "vscode";
import { correctKeywordCasing } from "./formatters/keywordCasing";
import { ensureSemicolonNewline } from "./formatters/semicolonNewline";
import { enforceOperatorSpacing } from "./formatters/operatorSpacing";
import { lineSpacingFormatter } from "./formatters/lineSpacing";
import { breakLongLines } from "./formatters/longLineBreaker";
import { adjustIndentation } from "./formatters/indentation";
import { ensureSingleFinalNewline } from "./formatters/finalNewline";

export class SSLFormatter implements vscode.DocumentFormattingEditProvider {
    private document!: vscode.TextDocument;
    private options!: vscode.FormattingOptions;

    // List of SSL keywords that should be capitalized
    private keywords = [
        "BEGINCASE",
        "BEGININLINECODE",
        "CASE",
        "DECLARE",
        "DEFAULT",
        "ELSE",
        "ENDCASE",
        "ENDIF",
        "ENDINLINECODE",
        "ENDWHILE",
        "ERROR",
        "EXITCASE",
        "EXITWHILE",
        "IF",
        "INCLUDE",
        "LOOP",
        "OTHERWISE",
        "PARAMETERS",
        "PROCEDURE",
        "PUBLIC",
        "REGION",
        "RESUME",
        "RETURN",
        "WHILE",
    ];

    public provideDocumentFormattingEdits(
        document: vscode.TextDocument,
        options: vscode.FormattingOptions,
        token: vscode.CancellationToken
    ): vscode.TextEdit[] {
        this.document = document;
        this.options = options;

        let text = document.getText();
        const originalLines = text.split("\n");

        // Apply formatting steps
        // Correct the casing of SSL keywords
        // text = correctKeywordCasing(text, this.keywords);

        // Ensure semicolons are followed by newlines
        // text = ensureSemicolonNewline(text);

        // Enforce spacing around operators
        // text = enforceOperatorSpacing(text);

        // Enforce line spacing according to style guide
        text = lineSpacingFormatter(text);

        // Break lines over 90 characters
        // text = breakLongLines(text);

        // Adjust the indentation of the code
        // text = adjustIndentation(text);

        // Make sure the code ends in a single blank line
        // text = ensureSingleFinalNewline(text);

        // Create a single edit for the entire document
        const lastLineId = document.lineCount - 1;
        const lastLineLength = document.lineAt(lastLineId).text.length;
        const range = new vscode.Range(0, 0, lastLineId, lastLineLength);
        return [vscode.TextEdit.replace(range, text)];
    }
}
