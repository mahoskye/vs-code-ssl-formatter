import * as vscode from "vscode";

interface BlockFamily {
    openerLine: RegExp;
    openerKeyword: RegExp;
    closerKeyword: RegExp;
    closerText: string;
}

const FAMILIES: ReadonlyArray<BlockFamily> = [
    {
        openerLine: /^\s*:IF\b.*;\s*$/i,
        openerKeyword: /^\s*:IF\b/i,
        closerKeyword: /^\s*:ENDIF\b/i,
        closerText: ":ENDIF;",
    },
    {
        openerLine: /^\s*:WHILE\b.*;\s*$/i,
        openerKeyword: /^\s*:WHILE\b/i,
        closerKeyword: /^\s*:ENDWHILE\b/i,
        closerText: ":ENDWHILE;",
    },
    {
        openerLine: /^\s*:FOR\b.*;\s*$/i,
        openerKeyword: /^\s*:FOR\b/i,
        closerKeyword: /^\s*:NEXT\b/i,
        closerText: ":NEXT;",
    },
    {
        openerLine: /^\s*:BEGINCASE\b.*;\s*$/i,
        openerKeyword: /^\s*:BEGINCASE\b/i,
        closerKeyword: /^\s*:ENDCASE\b/i,
        closerText: ":ENDCASE;",
    },
    {
        openerLine: /^\s*:TRY\b.*;\s*$/i,
        openerKeyword: /^\s*:TRY\b/i,
        closerKeyword: /^\s*:ENDTRY\b/i,
        closerText: ":ENDTRY;",
    },
    {
        openerLine: /^\s*:PROCEDURE\b.*;\s*$/i,
        openerKeyword: /^\s*:PROCEDURE\b/i,
        closerKeyword: /^\s*:ENDPROC\b/i,
        closerText: ":ENDPROC;",
    },
    {
        openerLine: /^\s*:CLASS\b.*;\s*$/i,
        openerKeyword: /^\s*:CLASS\b/i,
        closerKeyword: /^\s*:ENDCLASS\b/i,
        closerText: ":ENDCLASS;",
    },
    {
        openerLine: /^\s*:REGION\b.*;\s*$/i,
        openerKeyword: /^\s*:REGION\b/i,
        closerKeyword: /^\s*:ENDREGION\b/i,
        closerText: ":ENDREGION;",
    },
];

const inFlightDocs = new WeakSet<vscode.TextDocument>();

function familyForOpener(line: string): BlockFamily | undefined {
    return FAMILIES.find(f => f.openerLine.test(line));
}

function leadingIndent(line: string): string {
    const match = line.match(/^\s*/);
    return match ? match[0] : "";
}

/**
 * Returns true if a matching closer for the opener at `openerLineNumber` already exists
 * later in the document, accounting for nested same-family blocks.
 *
 * Starts with balance = 1 (the just-typed opener). Walks forward; each same-family opener
 * increments the balance, each same-family closer decrements it. If balance hits 0, the
 * closer that brought it there matches our opener — so a closer is already present.
 */
function closerAlreadyExists(
    document: vscode.TextDocument,
    family: BlockFamily,
    openerLineNumber: number
): boolean {
    let balance = 1;
    for (let i = openerLineNumber + 1; i < document.lineCount; i++) {
        const line = document.lineAt(i).text;
        // Closer check first so a line that is both (impossible in practice) errs toward "exists".
        if (family.closerKeyword.test(line)) {
            balance--;
            if (balance === 0) {
                return true;
            }
            continue;
        }
        if (family.openerKeyword.test(line)) {
            balance++;
        }
    }
    return false;
}

export function registerBlockCloser(context: vscode.ExtensionContext): void {
    const config = () => vscode.workspace.getConfiguration("ssl");
    const enabled = () => config().get<boolean>("editor.autoInsertBlockClosers", true);

    context.subscriptions.push(
        vscode.workspace.onDidChangeTextDocument(async event => {
            if (!enabled()) {
                return;
            }
            if (event.document.languageId !== "ssl") {
                return;
            }
            if (inFlightDocs.has(event.document)) {
                return;
            }
            if (event.contentChanges.length !== 1) {
                return;
            }
            const change = event.contentChanges[0];
            if (change.rangeLength !== 0) {
                return;
            }
            if (!/^\r?\n[\t ]*$/.test(change.text)) {
                return;
            }

            const editor = vscode.window.activeTextEditor;
            if (!editor || editor.document !== event.document) {
                return;
            }

            const openerLineNumber = change.range.start.line;
            const openerLine = event.document.lineAt(openerLineNumber).text;
            const family = familyForOpener(openerLine);
            if (!family) {
                return;
            }

            const cursor = editor.selection.active;
            if (cursor.line !== openerLineNumber + 1) {
                return;
            }

            if (closerAlreadyExists(event.document, family, openerLineNumber)) {
                return;
            }

            const indent = leadingIndent(openerLine);
            const insertText = `\n${indent}${family.closerText}`;

            inFlightDocs.add(event.document);
            try {
                await editor.edit(
                    builder => {
                        builder.insert(cursor, insertText);
                    },
                    { undoStopBefore: false, undoStopAfter: false }
                );
                editor.selection = new vscode.Selection(cursor, cursor);
            } finally {
                inFlightDocs.delete(event.document);
            }
        })
    );
}
