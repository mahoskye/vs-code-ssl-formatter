import * as vscode from "vscode";
import { SQL_CONTEXT_FUNCTIONS } from "../constants/language";

/**
 * Context of the SQL to be formatted
 */
export interface SqlContext {
    content: string;
    quoteChar: string; // " or ' or [ or empty
    prefix: string;    // Text to preserve before content (e.g. 'SqlExecute("')
    suffix: string;    // Text to preserve after content (e.g. '")')
    type: 'raw' | 'quoted' | 'function';
}

// Generate the regex dynamically from the constant
// Matches: FunctionName \s* ( \s* (["']) (content) \2
const SQL_FUNC_NAMES = SQL_CONTEXT_FUNCTIONS.join('|');
const SQL_FUNC_REGEX = new RegExp(`^(${SQL_FUNC_NAMES})\\s*\\(\\s*(["'])([\\s\\S]*)\\2\\s*\\);?$`, 'i');

/**
 * Detect the context of the SQL selection
 */
export function detectSqlContext(text: string): SqlContext {
    const trimmed = text.trim();

    // 1. Check for function call wrapper: Function("...")
    const funcMatch = trimmed.match(SQL_FUNC_REGEX);
    if (funcMatch) {
        // match[1] = FuncName
        // match[2] = Quote
        // match[3] = Content
        const quote = funcMatch[2];
        const innerContent = funcMatch[3];

        // Calculate prefix/suffix based on original text
        // We find the first occurrence of innerContent? 
        // Be careful if innerContent appears in prefix (unlikely for quoted string content)
        const contentStart = text.indexOf(innerContent); // First occurrence
        const prefix = text.substring(0, contentStart);
        const suffix = text.substring(contentStart + innerContent.length);

        return {
            content: innerContent,
            quoteChar: quote,
            prefix: prefix,
            suffix: suffix,
            type: 'function'
        };
    }

    // 2. Check for quoted string: "..." or '...' or [...]
    const stringMatch = trimmed.match(/^((["'\[])([\s\S]*)(["'\]]))$/);
    if (stringMatch) {
        const quote = stringMatch[2];
        const content = stringMatch[3];
        const close = stringMatch[4];

        if ((quote === '[' && close === ']') || (quote !== '[' && quote === close)) {
            const contentStart = text.indexOf(content);
            const prefix = text.substring(0, contentStart);
            const suffix = text.substring(contentStart + content.length);

            return {
                content: content,
                quoteChar: quote,
                prefix: prefix,
                suffix: suffix,
                type: 'quoted'
            };
        }
    }

    // 3. Raw SQL
    return {
        content: text,
        quoteChar: '',
        prefix: '',
        suffix: '',
        type: 'raw'
    };
}

/**
 * Check if text looks like SQL
 */
export function looksLikeSql(text: string): boolean {
    const normalizedText = text.toUpperCase();
    const sqlIndicators = ["SELECT", "INSERT", "UPDATE", "DELETE", "FROM", "WHERE"];
    return sqlIndicators.some(keyword => normalizedText.includes(keyword));
}

/**
 * Find a SQL string literal in a line
 */
export function findSqlStringInLine(
    document: vscode.TextDocument,
    lineNumber: number
): { range: vscode.Range; content: string } | null {
    const line = document.lineAt(lineNumber);
    const text = line.text;

    // Look for SQL function calls with string literals
    // \b(Functions...)\s*\(\s*(["'])(.*?)\2
    // Use non-greedy match for content
    const sqlFunctionPattern = new RegExp(`\\b(${SQL_FUNC_NAMES})\\s*\\(\\s*(["'])([\\s\\S]*?)\\2`, 'gi');

    let match: RegExpExecArray | null;

    while ((match = sqlFunctionPattern.exec(text)) !== null) {
        // match[0] = Full match: SqlExecute("SELECT...")
        // match[1] = FuncName
        // match[2] = Quote
        // match[3] = Content

        // Where does the content start? 
        // Index of match[0] + offset of quote + 1
        // Simpler: find the quote in the match text
        const quoteIndex = match[0].indexOf(match[2], match[1].length); // Start searching after function name
        const stringStart = match.index + quoteIndex;
        const stringEnd = stringStart + 1 + match[3].length + 1; // +1s for quotes

        // Wait, logic in original was:
        // const stringStart = match.index + match[0].indexOf(match[2]);
        // That finds the first quote. Correct.

        return {
            range: new vscode.Range(
                new vscode.Position(lineNumber, stringStart),
                new vscode.Position(lineNumber, stringEnd)
            ),
            // Return full string with quotes
            content: match[2] + match[3] + match[2]
        };
    }

    // Look for any string that contains SQL keywords
    const stringPattern = /(["'])([\s\S]*?)\1/g;
    while ((match = stringPattern.exec(text)) !== null) {
        const content = match[2];
        if (looksLikeSql(content)) {
            return {
                range: new vscode.Range(
                    new vscode.Position(lineNumber, match.index),
                    new vscode.Position(lineNumber, match.index + match[0].length)
                ),
                content: match[0]
            };
        }
    }

    return null;
}
