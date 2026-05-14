import * as vscode from "vscode";

export interface BlockFamily {
    openerLine: RegExp;
    openerKeyword: RegExp;
    closerKeyword: RegExp;
    closerText: string;
}

export const FAMILIES: ReadonlyArray<BlockFamily> = [
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

export function familyForOpener(line: string): BlockFamily | undefined {
    return FAMILIES.find(f => f.openerLine.test(line));
}

export function leadingIndent(line: string): string {
    const match = line.match(/^\s*/);
    return match ? match[0] : "";
}

/**
 * Returns, for each line, whether that line BEGINS in SSL "code" context as
 * opposed to inside a string or block comment that started on an earlier
 * line.
 *
 * The block-closer's regex-based balance scan needs this so a `:ENDIF;` that
 * literally appears inside a multi-line string or comment is not counted as
 * a real closer. The mini-lexer tracks three multi-line constructs:
 *
 *   - `"…"` strings (no SSL escape sequence; the next `"` closes)
 *   - `'…'` strings (same; the next `'` closes)
 *   - `/* … ;` block comments (closed by the next `;`)
 *
 * The third bracket-string form `[…]` is context-sensitive in the real SSL
 * lexer (array indexing vs literal) and almost never spans lines in
 * practice, so we don't track it here — a line that opens with `[` and
 * doesn't close it on the same line will produce a slightly off
 * classification, but only matters when block-keyword lines also happen to
 * sit inside such an unclosed bracket string. That's not a real shape.
 */
export function classifyLineStarts(lines: ReadonlyArray<string>): boolean[] {
    const startsInCode: boolean[] = new Array(lines.length);
    type State = "code" | "comment" | "string-d" | "string-s";
    let state: State = "code";

    for (let i = 0; i < lines.length; i++) {
        startsInCode[i] = state === "code";
        const line = lines[i];
        for (let j = 0; j < line.length; j++) {
            const c = line[j];
            if (state === "code") {
                if (c === "/" && line[j + 1] === "*") {
                    state = "comment";
                    j++; // skip the '*'
                } else if (c === '"') {
                    state = "string-d";
                } else if (c === "'") {
                    state = "string-s";
                }
            } else if (state === "comment") {
                if (c === ";") {
                    state = "code";
                }
            } else if (state === "string-d") {
                if (c === '"') {
                    state = "code";
                }
            } else if (state === "string-s") {
                if (c === "'") {
                    state = "code";
                }
            }
        }
    }
    return startsInCode;
}

/**
 * Returns true if a matching closer for the opener at `openerLineNumber` already exists
 * later in `lines`, accounting for nested same-family blocks.
 *
 * Starts with balance = 1 (the just-typed opener). Walks forward; each same-family opener
 * increments the balance, each same-family closer decrements it. If balance hits 0, the
 * closer that brought it there matches our opener — so a closer is already present.
 *
 * Lines that begin inside a string or block comment (per `startsInCode`) are skipped
 * entirely: a `:ENDIF;` that literally sits inside a multi-line string is text, not a
 * keyword.
 */
export function closerAlreadyExistsInLines(
    lines: ReadonlyArray<string>,
    family: BlockFamily,
    openerLineNumber: number,
    startsInCode?: ReadonlyArray<boolean>
): boolean {
    const codeMask = startsInCode ?? classifyLineStarts(lines);
    let balance = 1;
    for (let i = openerLineNumber + 1; i < lines.length; i++) {
        if (!codeMask[i]) {
            continue;
        }
        const line = lines[i];
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

/**
 * Pure decision function: given a snapshot of document lines, the opener
 * line number, and a candidate cursor position, return the text the
 * extension should insert (and the family that fired), or null if no
 * insertion should occur.
 *
 * Inputs are deliberately primitive so this can be exercised from unit
 * tests without spinning up a real `vscode.TextDocument` / `TextEditor`.
 */
export interface BlockCloserDecision {
    family: BlockFamily;
    insertText: string;
}

export function decideBlockCloser(
    lines: ReadonlyArray<string>,
    openerLineNumber: number,
    cursorLine: number
): BlockCloserDecision | null {
    if (openerLineNumber < 0 || openerLineNumber >= lines.length) {
        return null;
    }
    if (cursorLine !== openerLineNumber + 1) {
        return null;
    }
    const openerLine = lines[openerLineNumber];
    const family = familyForOpener(openerLine);
    if (!family) {
        return null;
    }
    // If the opener line itself starts INSIDE a string or comment that
    // began earlier, the `:IF` (or other keyword) is literal text, not a
    // block header. Don't insert.
    const codeMask = classifyLineStarts(lines);
    if (!codeMask[openerLineNumber]) {
        return null;
    }
    if (closerAlreadyExistsInLines(lines, family, openerLineNumber, codeMask)) {
        return null;
    }
    const indent = leadingIndent(openerLine);
    return {
        family,
        insertText: `\n${indent}${family.closerText}`,
    };
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
            const cursor = editor.selection.active;

            const lines: string[] = [];
            for (let i = 0; i < event.document.lineCount; i++) {
                lines.push(event.document.lineAt(i).text);
            }
            const decision = decideBlockCloser(lines, openerLineNumber, cursor.line);
            if (!decision) {
                return;
            }
            const insertText = decision.insertText;

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
