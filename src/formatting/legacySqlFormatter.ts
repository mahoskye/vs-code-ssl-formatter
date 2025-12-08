import { SQL } from '../constants/sql';

// Additional keywords/functions specific to this formatter implementation
// or not yet in the shared SQL constant.
const SQL_AGGREGATES = [
    "COUNT", "SUM", "AVG", "MIN", "MAX",
    "COALESCE", "NULLIF", "CAST", "CONVERT"
];

const SQL_CLAUSE_KEYWORDS = [
    "INNER JOIN",
    "LEFT JOIN",
    "RIGHT JOIN",
    "FULL JOIN",
    "CROSS JOIN",
    "GROUP BY",
    "ORDER BY",
    "UNION ALL",
    "INSERT INTO",
    "DELETE FROM",
    "EXCEPT",
    "INTERSECT",
    "HAVING",
    "VALUES",
    "WHERE",
    "UNION",
    "FROM",
    "JOIN",
    "UPDATE",
    "SET",
    "ON"
].sort((a, b) => b.length - a.length);

// Combine shared keywords and local ones for general casing
const SQL_GENERAL_KEYWORDS = [
    ...Object.values(SQL.KEYWORDS),
    ...SQL_AGGREGATES
];

export type SqlFormattingStyle =
    | "compact"
    | "canonicalCompact"
    | "expanded"
    | "hangingOperators"
    | "knr"
    | "knrCompact"
    | "ormFriendly";

/**
 * Apply keyword case preference
 */
function applySqlKeywordCase(keyword: string, style: string, original: string): string {
    if (style === "lower") {
        return keyword.toLowerCase();
    }
    if (style === "upper") {
        return keyword.toUpperCase();
    }
    return original;
}

/**
 * Format SQL with a specific style
 */
export function formatSqlWithStyleImpl(
    content: string,
    style: SqlFormattingStyle,
    keywordCase: string,
    indentSpaces: number,
    wrapLength: number = 0,
    indentString?: string
): string {
    const effectiveIndentString = indentString ?? " ".repeat(indentSpaces);
    let sql = content;

    // Clean up initial/trailing whitespace
    sql = sql.trim();
    if (sql.length === 0) { return ""; }

    // Collapse multiple whitespace to single space
    sql = sql.replace(/\s+/g, " ");

    // Normalize comparison operator spacing
    sql = normalizeOperators(sql);

    // Format expressions inside ?...? parameter placeholders
    sql = formatParameterPlaceholders(sql);

    // Mask literals and parameters
    const masks: string[] = [];
    const maskLiteral = (s: string) => {
        masks.push(s);
        return `__MASK${masks.length - 1}__`;
    };

    sql = sql.replace(/(["'])(?:(?=(\\?))\2[\s\S])*?\1/g, maskLiteral);
    sql = sql.replace(/\?([^?]+)\?/g, maskLiteral);

    // Apply casing to unmasked parts
    sql = applyCasing(sql, keywordCase);

    // Restore masks
    for (let i = masks.length - 1; i >= 0; i--) {
        sql = sql.replace(`__MASK${i}__`, masks[i]);
    }

    switch (style) {
        case "compact":
            return formatCompactStyle(sql, keywordCase, effectiveIndentString);
        case "canonicalCompact":
            return formatCanonicalCompactStyle(sql, keywordCase, effectiveIndentString, wrapLength);
        case "expanded":
            return formatExpandedStyle(sql, keywordCase, effectiveIndentString);
        case "hangingOperators":
            return formatHangingOperatorsStyle(sql, keywordCase, effectiveIndentString);
        case "knr":
            return formatKnrStyle(sql, keywordCase, effectiveIndentString, false);
        case "knrCompact":
            return formatKnrStyle(sql, keywordCase, effectiveIndentString, true);
        case "ormFriendly":
            return formatOrmFriendlyStyle(sql, keywordCase, effectiveIndentString);
        default:
            return formatCompactStyle(sql, keywordCase, effectiveIndentString);
    }
}

function normalizeOperators(sql: string): string {
    let result = sql;
    result = result.replace(/\s*<>\s*/g, " <> ");
    result = result.replace(/\s*<=\s*/g, " <= ");
    result = result.replace(/\s*>=\s*/g, " >= ");
    result = result.replace(/\s*!=\s*/g, " != ");
    result = result.replace(/(?<![<>!=])=(?!=)/g, " = ");
    result = result.replace(/\s+/g, " ");
    return result;
}

function formatParameterPlaceholders(sql: string): string {
    return sql.replace(/\?([^?]+)\?/g, (_match, expr: string) => {
        // Simple tokenizer for basic arithmetic spacing
        // ... (implementation preserved from original but cleaned up) ...
        const parts: string[] = [];
        let current = '';
        let inString = false;
        let quoteChar = '';

        for (let i = 0; i < expr.length; i++) {
            const char = expr[i];
            if (!inString) {
                if (char === '"' || char === "'") {
                    parts.push(current);
                    current = char;
                    inString = true;
                    quoteChar = char;
                } else {
                    current += char;
                }
            } else {
                current += char;
                if (char === quoteChar) {
                    parts.push(current);
                    current = '';
                    inString = false;
                    quoteChar = '';
                }
            }
        }
        parts.push(current);

        const formattedParts = parts.map(part => {
            if (part.startsWith('"') || part.startsWith("'")) {
                return part;
            }
            let formatted = part;
            formatted = formatted.replace(/([A-Za-z0-9_\]\)])\s*\+\s*([A-Za-z0-9_\[\(])/g, '$1 + $2');
            formatted = formatted.replace(/([A-Za-z0-9_\]\)])\s*-\s*([A-Za-z0-9_\[\(])/g, '$1 - $2');
            formatted = formatted.replace(/([A-Za-z0-9_\]\)])\s*\*\s*([A-Za-z0-9_\[\(])/g, '$1 * $2');
            formatted = formatted.replace(/([A-Za-z0-9_\]\)])\s*\/\s*([A-Za-z0-9_\[\(])/g, '$1 / $2');
            return formatted;
        });

        return `?${formattedParts.join('')}?`;
    });
}

function applyCasing(sql: string, keywordCase: string): string {
    const processToken = (match: string, offset: number, fullText: string): string => {
        const upper = match.toUpperCase();
        if (SQL_GENERAL_KEYWORDS.includes(upper)) {
            return applySqlKeywordCase(upper, keywordCase, match);
        }
        const remainder = fullText.substring(offset + match.length);
        if (/^\s*\(/.test(remainder)) {
            // Likely a function call
            return match.charAt(0).toUpperCase() + match.slice(1).toLowerCase();
        }
        return match.toLowerCase();
    };

    return sql.replace(/\b[a-zA-Z_][a-zA-Z0-9_]*\b/g, (match, offset, fullText) => {
        if (match.startsWith('__MASK') && match.endsWith('__')) {
            return match;
        }
        return processToken(match, offset, fullText);
    });
}

// --- Specific Style Formatters ---

function formatCompactStyle(sql: string, keywordCase: string, indentString: string): string {
    const indent = indentString;
    let result = sql;
    SQL_CLAUSE_KEYWORDS.forEach(keyword => {
        if (keyword === "ON") { return; }
        const pattern = keyword.replace(/\s+/g, "\\s+");
        const clauseRegex = new RegExp(`\\s+(${pattern})\\b`, "gi");
        result = result.replace(clauseRegex, (_match, clause) => {
            return `\n${applySqlKeywordCase(keyword, keywordCase, clause)}`;
        });
    });
    const lines = result.split('\n').map(line => line.trim());
    return lines.map((line, index) => {
        if (index === 0) { return line; }
        return indent + line;
    }).join('\n');
}

function formatCanonicalCompactStyle(sql: string, keywordCase: string, indentString: string, wrapLength: number = 0): string {
    let result = sql;
    const hangingIndent = indentString;

    // 1. Format Single-Line Clauses (SELECT ...)
    result = formatCanonicalSelect(result, keywordCase, wrapLength);

    // 2. Break Major Clauses
    result = formatMajorClauses(result, keywordCase);

    // 3. Format SET Clauses
    result = formatCanonicalSet(result, keywordCase, indentString);

    // 4. Format INSERT/VALUES and SELECT Sub-blocks
    result = formatCanonicalInsert(result, keywordCase, indentString, wrapLength);

    // 5. Format JOIN ON
    result = result.replace(/((?:INNER |LEFT |RIGHT |FULL )?JOIN[^\n]*?)\s+ON\s+(.+?)(?=\n|$)/gi,
        (_match, joinPart, condition) => {
            return `${joinPart}\n${hangingIndent}${applySqlKeywordCase("ON", keywordCase, "ON")} ${condition.trim()}`;
        }
    );

    // 6. Format AND/OR
    const columnAlignIndent = 7; // For visual alignment with previous legacy behavior?
    result = result.replace(/\s+(AND)\s+/gi, (_match, op) => {
        return `\n${hangingIndent}${applySqlKeywordCase("AND", keywordCase, op)} `;
    });
    result = result.replace(/\s+(OR)\s+/gi, (_match, op) => {
        return `\n${" ".repeat(columnAlignIndent)}${applySqlKeywordCase("OR", keywordCase, op)} `;
    });

    // 7. Format Subqueries (EXISTS/IN)
    result = result.replace(/(EXISTS|IN)\s*(\()(\s*)(SELECT\b)/gi, (_match, keyword, paren, space, selectKw) => {
        return `${keyword.toUpperCase()} ${paren}\n${applySqlKeywordCase("SELECT", keywordCase, selectKw)}`;
    });
    result = result.replace(/([^A-Z])(\()(\s*)(SELECT\b)/gi, (_match, before, paren, space, selectKw) => {
        return `${before}${paren}\n${applySqlKeywordCase("SELECT", keywordCase, selectKw)}`;
    });

    // 8. Final Indentation Pass (Parenthesis Balancing)
    return formatIndentationRecursive(result, indentString);
}


function formatExpandedStyle(sql: string, keywordCase: string, indentString: string): string {
    const indent = indentString;

    // Parse SELECT columns
    const selectMatch = sql.match(/^(SELECT\s+(?:DISTINCT\s+)?)(.*?)(?=\s+FROM\b)/i);
    let result = sql;

    if (selectMatch) {
        const selectKeyword = applySqlKeywordCase("SELECT", keywordCase, selectMatch[1].trim());
        const distinctMatch = selectMatch[1].match(/DISTINCT/i);
        const distinctPart = distinctMatch ? " " + applySqlKeywordCase("DISTINCT", keywordCase, distinctMatch[0]) : "";
        const columns = parseColumns(selectMatch[2]);

        const formattedSelect = selectKeyword + distinctPart + "\n" +
            columns.map(col => indent + col).join(",\n");

        result = sql.replace(selectMatch[0], formattedSelect + " ");
    }

    // Break at clause keywords
    SQL_CLAUSE_KEYWORDS.forEach(keyword => {
        const pattern = keyword.replace(/\s+/g, "\\s+");
        const clauseRegex = new RegExp(`\\s+(${pattern})\\b`, "gi");
        result = result.replace(clauseRegex, (_match, clause) => {
            return `\n${applySqlKeywordCase(keyword, keywordCase, clause)}`;
        });
    });

    // Handle WHERE with expanded conditions
    result = result.replace(/\bWHERE\b\s+/gi, (match) => {
        return applySqlKeywordCase("WHERE", keywordCase, match.trim()) + "\n" + indent;
    });

    // Handle AND/OR
    result = result.replace(/\s+(AND|OR)\b/gi, (_match, op) => {
        return `\n${indent}${applySqlKeywordCase(op.toUpperCase(), keywordCase, op)}`;
    });

    // Handle GROUP BY and ORDER BY
    result = expandClauseColumns(result, "GROUP BY", keywordCase, indent);
    result = expandClauseColumns(result, "ORDER BY", keywordCase, indent);

    return result.trim();
}

function formatHangingOperatorsStyle(sql: string, keywordCase: string, indentString: string): string {
    const hangingIndent = "  "; // 2 spaces for hanging operators

    let result = sql;
    SQL_CLAUSE_KEYWORDS.forEach(keyword => {
        const pattern = keyword.replace(/\s+/g, "\\s+");
        const clauseRegex = new RegExp(`\\s+(${pattern})\\b`, "gi");
        result = result.replace(clauseRegex, (_match, clause) => {
            return `\n${applySqlKeywordCase(keyword, keywordCase, clause)}`;
        });
    });

    result = result.replace(/\s+(AND|OR)\b/gi, (_match, op) => {
        return `\n${hangingIndent}${applySqlKeywordCase(op.toUpperCase(), keywordCase, op)}`;
    });

    result = result.replace(/\n(ON)\b/gi, (_match, on) => {
        return `\n${hangingIndent}${applySqlKeywordCase("ON", keywordCase, on)}`;
    });

    return result.trim();
}

function formatOrmFriendlyStyle(sql: string, keywordCase: string, indentString: string): string {
    const indent = indentString;
    const hangingIndent = "  ";
    let result = sql;

    const selectMatch = result.match(/^(SELECT\s+(?:DISTINCT\s+)?)(.*?)(?=\s+FROM\b)/i);
    if (selectMatch) {
        const selectKeyword = applySqlKeywordCase("SELECT", keywordCase, selectMatch[1].trim());
        const distinctMatch = selectMatch[1].match(/DISTINCT/i);
        const distinctPart = distinctMatch ? " " + applySqlKeywordCase("DISTINCT", keywordCase, distinctMatch[0]) : "";
        const columns = parseColumns(selectMatch[2]);

        const formattedSelect = selectKeyword + distinctPart + "\n" +
            columns.map(col => indent + col).join(",\n");

        result = result.replace(selectMatch[0], formattedSelect + " ");
    }

    result = result.replace(/\s+FROM\s+/gi, () => `\n${applySqlKeywordCase("FROM", keywordCase, "FROM")} `);
    result = result.replace(/\s+WHERE\s+/gi, () => `\n${applySqlKeywordCase("WHERE", keywordCase, "WHERE")} `);
    result = result.replace(/\s+GROUP BY\s+/gi, () => `\n${applySqlKeywordCase("GROUP BY", keywordCase, "GROUP BY")} `);
    result = result.replace(/\s+ORDER BY\s+/gi, () => `\n${applySqlKeywordCase("ORDER BY", keywordCase, "ORDER BY")} `);

    result = result.replace(/\b(INNER JOIN|LEFT JOIN|RIGHT JOIN|FULL JOIN|CROSS JOIN|JOIN)\b/gi,
        (_match, clause) => applySqlKeywordCase((clause as string).toUpperCase(), keywordCase, clause));

    result = result.replace(/\s+(AND|OR)\b/gi, (_match, op) => `\n${hangingIndent}${applySqlKeywordCase((op as string).toUpperCase(), keywordCase, op)}`);

    const lines = result.split('\n').map(line => line.trimEnd());
    return lines.join('\n').trim();
}

function formatKnrStyle(sql: string, keywordCase: string, indentString: string, compactSelect: boolean): string {
    const indent = indentString;

    const selectMatch = sql.match(/^(SELECT\s+(?:DISTINCT\s+)?)(.*?)(?=\s+FROM\b)/i);
    let result = sql;

    if (selectMatch) {
        const selectKeyword = applySqlKeywordCase("SELECT", keywordCase, selectMatch[1].trim());
        const distinctMatch = selectMatch[1].match(/DISTINCT/i);
        const distinctPart = distinctMatch ? " " + applySqlKeywordCase("DISTINCT", keywordCase, distinctMatch[0]) : "";
        const columns = parseColumns(selectMatch[2]);

        let formattedSelect: string;
        if (compactSelect && columns.join(", ").length < 60) {
            formattedSelect = selectKeyword + distinctPart + "\n" + indent + columns.join(", ");
        } else {
            formattedSelect = selectKeyword + distinctPart + "\n" +
                columns.map(col => indent + col).join(",\n");
        }

        result = sql.replace(selectMatch[0], formattedSelect + " ");
    }

    ["FROM", "JOIN", "INNER JOIN", "LEFT JOIN", "RIGHT JOIN", "FULL JOIN",
        "GROUP BY", "ORDER BY", "HAVING", "UNION", "UNION ALL"].forEach(keyword => {
            const pattern = keyword.replace(/\s+/g, "\\s+");
            const clauseRegex = new RegExp(`\\s+(${pattern})\\b`, "gi");
            result = result.replace(clauseRegex, (_match, clause) => {
                return `\n${applySqlKeywordCase(keyword, keywordCase, clause)}`;
            });
        });

    result = result.replace(/\s+ON\s+([^)]+?)(?=\s+(?:WHERE|JOIN|INNER|LEFT|RIGHT|FULL|GROUP|ORDER|HAVING|UNION|$))/gi,
        (_match, condition) => {
            const onKeyword = applySqlKeywordCase("ON", keywordCase, "ON");
            return `\n${onKeyword} (\n${indent}${condition.trim()}\n)`;
        }
    );

    result = result.replace(/\s+WHERE\s+(.+?)(?=\s+(?:GROUP|ORDER|HAVING|UNION|$)|$)/gi,
        (_match, condition) => {
            const whereKeyword = applySqlKeywordCase("WHERE", keywordCase, "WHERE");
            const formattedCondition = formatKnrConditions(condition, keywordCase, indent);
            return `\n${whereKeyword} (\n${formattedCondition}\n)`;
        }
    );

    result = result.replace(/\nGROUP BY\s+([^)]+?)(?=\s+(?:ORDER|HAVING|$)|$)/gi,
        (_match, columns) => {
            const groupByKeyword = applySqlKeywordCase("GROUP BY", keywordCase, "GROUP BY");
            const cols = parseColumns(columns);
            if (cols.length > 1) {
                return `\n${groupByKeyword} (\n${cols.map(c => indent + c).join(",\n")}\n)`;
            }
            return `\n${groupByKeyword} ${columns.trim()}`;
        }
    );

    result = result.replace(/\nORDER BY\s+(.+?)$/gi,
        (_match, columns) => {
            const orderByKeyword = applySqlKeywordCase("ORDER BY", keywordCase, "ORDER BY");
            const cols = parseColumns(columns.replace(/;$/, ''));
            if (cols.length > 1) {
                return `\n${orderByKeyword} (\n${cols.map(c => indent + c).join(",\n")}\n)`;
            }
            return `\n${orderByKeyword} ${columns.trim()}`;
        }
    );

    return result.trim();
}

function formatKnrConditions(condition: string, keywordCase: string, indent: string): string {
    let result = condition.trim();

    result = result.replace(/\s+(AND|OR)\s+/gi, (_match, op) => {
        return `\n${indent}${applySqlKeywordCase(op.toUpperCase(), keywordCase, op)} `;
    });

    const lines = result.split('\n');
    return lines.map((line, index) => {
        if (index === 0) { return indent + line; }
        return line;
    }).join('\n');
}

// --- Helpers for Canonical Compact ---

function formatCanonicalSelect(sql: string, keywordCase: string, wrapLength: number): string {
    const columnAlignIndent = 7;
    let result = sql;
    const selectStartMatch = result.match(/^(SELECT\s+(?:DISTINCT\s+)?)/i);
    let selectMatch: RegExpMatchArray | null = null;

    if (selectStartMatch) {
        // Determine end of SELECT list (FROM) accounting for parentheses/quotes
        const startIdx = selectStartMatch[0].length;
        let nesting = 0;
        let inString = false;
        let stringChar = '';
        let foundEnd = false;
        let endIdx = -1;

        for (let i = startIdx; i < result.length; i++) {
            const char = result[i];

            if (inString) {
                if (char === stringChar) {
                    inString = false;
                }
            } else {
                if (char === '"' || char === "'") {
                    inString = true;
                    stringChar = char;
                } else if (char === '(') {
                    nesting++;
                } else if (char === ')') {
                    nesting--;
                } else if (nesting === 0) {
                    if (result.substring(i, i + 6).toUpperCase() === " FROM ") {
                        endIdx = i;
                        foundEnd = true;
                        break;
                    }
                }
            }
        }

        if (foundEnd) {
            const selectListString = result.substring(startIdx, endIdx);
            const fullMatchString = result.substring(0, endIdx);
            selectMatch = [fullMatchString, selectStartMatch[1], selectListString];
        }
    }

    if (selectMatch) {
        const selectKeyword = applySqlKeywordCase("SELECT", keywordCase, selectMatch[1].trim());
        const distinctMatch = selectMatch[1].match(/DISTINCT/i);
        const distinctPart = distinctMatch ? " " + applySqlKeywordCase("DISTINCT", keywordCase, distinctMatch[0]) : "";
        const columns = parseColumns(selectMatch[2]);

        const firstLinePrefix = selectKeyword + distinctPart + " ";
        const continuationPrefix = " ".repeat(columnAlignIndent);

        const formattedColumns: string[] = [];
        let currentLine = firstLinePrefix;
        let currentVisualLength = firstLinePrefix.length;
        const maxLineLength = 80;

        for (let i = 0; i < columns.length; i++) {
            const col = columns[i];
            const separator = i < columns.length - 1 ? ", " : "";
            const addition = col + separator;

            const isSubquery = /\(\s*SELECT\b/i.test(col);
            let additionVisualLength = addition.length;

            if (isSubquery) {
                additionVisualLength = 10;
            }

            if (currentLine === firstLinePrefix) {
                currentLine += addition;
                if (isSubquery && col.match(/\)\s*[^)]*$/)) {
                    const tailMatch = col.match(/\)\s*[^)]*$/);
                    const tailLength = tailMatch ? tailMatch[0].length : 15;
                    currentVisualLength = continuationPrefix.length + tailLength;
                } else {
                    currentVisualLength += addition.length;
                }
            } else if (currentVisualLength + additionVisualLength <= maxLineLength) {
                currentLine += addition;
                if (isSubquery && col.match(/\)\s*[^)]*$/)) {
                    const tailMatch = col.match(/\)\s*[^)]*$/);
                    const tailLength = tailMatch ? tailMatch[0].length : 15;
                    currentVisualLength = continuationPrefix.length + tailLength;
                } else {
                    currentVisualLength += addition.length;
                }
            } else {
                formattedColumns.push(currentLine.trimEnd());
                currentLine = continuationPrefix + addition;
                if (isSubquery && col.match(/\)\s*[^)]*$/)) {
                    const tailMatch = col.match(/\)\s*[^)]*$/);
                    const tailLength = tailMatch ? tailMatch[0].length : 15;
                    currentVisualLength = continuationPrefix.length + tailLength;
                } else {
                    currentVisualLength = continuationPrefix.length + addition.length;
                }
            }
        }
        if (currentLine.trim()) {
            formattedColumns.push(currentLine.trimEnd());
        }

        result = result.replace(selectMatch[0], formattedColumns.join("\n") + " ");
    }
    return result;
}

function formatMajorClauses(sql: string, keywordCase: string): string {
    let result = sql;
    const majorClauses = [
        "UPDATE", "DELETE FROM",
        "FROM", "JOIN", "INNER JOIN", "LEFT JOIN", "RIGHT JOIN", "FULL JOIN", "CROSS JOIN",
        "WHERE", "GROUP BY", "ORDER BY", "HAVING", "UNION", "UNION ALL", "EXCEPT", "INTERSECT",
        "CASE", "WHEN", "ELSE", "END"
    ];

    majorClauses.forEach(keyword => {
        let pattern = keyword.replace(/\s+/g, "\\s+");

        if (keyword === "FROM") {
            pattern = `(?<!DELETE\\s+)FROM`;
        }

        if (keyword === "CASE") {
            const clauseRegex = new RegExp(`(\\s*\\(\\s*)(${pattern})\\b`, "gi");
            result = result.replace(clauseRegex, (_match, prefix, clause) => {
                return `${prefix}\n${applySqlKeywordCase(keyword, keywordCase, clause)}`;
            });
        }

        if (keyword !== "CASE") {
            const clauseRegex = new RegExp(`\\s+(${pattern})\\b`, "gi");
            result = result.replace(clauseRegex, (_match, clause) => {
                return `\n${applySqlKeywordCase(keyword, keywordCase, clause)}`;
            });
        } else {
            const caseRegex = new RegExp(`([\\s(]+)(${pattern})\\b`, "gi");
            result = result.replace(caseRegex, (match, prefix, clause) => {
                if (prefix.includes('\n')) { return match; }

                if (prefix.includes('(')) {
                    const lastParenIndex = prefix.lastIndexOf('(');
                    const preParen = prefix.substring(0, lastParenIndex + 1);
                    return `${preParen}\n${applySqlKeywordCase(keyword, keywordCase, clause)}`;
                } else {
                    return `\n${applySqlKeywordCase(keyword, keywordCase, clause)}`;
                }
            });
        }
    });
    return result;
}

function formatCanonicalSet(sql: string, keywordCase: string, indentString: string): string {
    let result = sql;
    result = result.replace(/\s+SET\b/gi, ' SET');
    const setRegex = /(\bSET\b)([\s\S]+?)(?=\b(WHERE|FROM|JOIN|INNER|LEFT|RIGHT|FULL|CROSS|GROUP|ORDER|HAVING|UNION|EXCEPT|INTERSECT|$))/gi;
    result = result.replace(setRegex, (match, setKw, content, lookahead) => {
        const assignments = parseColumns(content);
        if (assignments.length > 0) {
            const formattedAssignments = assignments.map(a => indentString + a.trim());
            return `${applySqlKeywordCase("SET", keywordCase, "SET")}\n${formattedAssignments.join(',\n')}\n`;
        }
        return match;
    });
    return result;
}

function formatCanonicalInsert(sql: string, keywordCase: string, indentString: string, wrapLength: number): string {
    let result = sql;
    const insertRegex = /(\bINSERT\s+INTO\b[\s\S]+?\))\s*(\b(?:VALUES|SELECT)\b)([\s\S]+?)(?=(?:\b(UPDATE|DELETE|WHERE|GROUP|ORDER|HAVING|UNION|EXCEPT|INTERSECT)\b)|$|;)/gi;

    result = result.replace(insertRegex, (match, insertPart, keywordKw, contentPart) => {
        let formattedInsert = insertPart.trim();
        const parenStart = formattedInsert.indexOf('(');
        let insertBlock = "";

        if (parenStart !== -1) {
            const insertIntoTable = formattedInsert.substring(0, parenStart).trim();
            const columnsContent = formattedInsert.substring(parenStart);
            const innerCols = columnsContent.replace(/^\s*\(\s*/, '').replace(/\s*\)\s*$/, '');
            const columns = parseColumns(innerCols);
            const indentLen = indentString.indexOf('\t') !== -1 ? 4 : indentString.length;
            const effectiveWrap = wrapLength > 0 ? Math.max(20, wrapLength - indentLen) : 0;
            const wrappedCols = wrapSqlList(columns, "", effectiveWrap);
            const tablePart = insertIntoTable.replace(/^\s*INSERT\s+INTO\s*/i, '');
            const insertKw = applySqlKeywordCase("INSERT INTO", keywordCase, "INSERT INTO");
            insertBlock = `\n${insertKw} ${tablePart}\n(\n${wrappedCols}\n)`;
        } else {
            const insertKw = applySqlKeywordCase("INSERT INTO", keywordCase, "INSERT INTO");
            const remainder = formattedInsert.replace(/^\s*INSERT\s+INTO\s*/i, '');
            insertBlock = `\n${insertKw} ${remainder}`;
        }

        let formattedContentPart = contentPart.trim();
        let contentBlock = "";
        const kwUpper = keywordKw.toUpperCase().trim();

        if (kwUpper === "VALUES") {
            const valParenStart = formattedContentPart.indexOf('(');
            if (valParenStart !== -1) {
                const valuesContent = formattedContentPart;
                const innerVals = valuesContent.replace(/^\s*\(\s*/, '').replace(/\s*\)\s*$/, '');
                const vals = parseColumns(innerVals);
                const indentLen = indentString.indexOf('\t') !== -1 ? 4 : indentString.length;
                const effectiveWrap = wrapLength > 0 ? Math.max(20, wrapLength - indentLen) : 0;
                const wrappedVals = wrapSqlList(vals, "", effectiveWrap);
                contentBlock = `\n${applySqlKeywordCase("VALUES", keywordCase, "VALUES")}\n(\n${wrappedVals}\n)`;
            } else {
                contentBlock = `\n${applySqlKeywordCase("VALUES", keywordCase, "VALUES")} ${formattedContentPart}`;
            }
        } else if (kwUpper === "SELECT") {
            const indent = indentString;
            const indentLen = indentString.indexOf('\t') !== -1 ? 4 : indentString.length;
            const effectiveWrap = wrapLength > 0 ? Math.max(20, wrapLength - indentLen) : 0;
            const colList = parseColumns(formattedContentPart);
            const wrappedSelect = wrapSqlList(colList, indent, effectiveWrap);
            contentBlock = `\n${applySqlKeywordCase("SELECT", keywordCase, "SELECT")}\n${wrappedSelect}\n`;
        } else {
            contentBlock = `\n${applySqlKeywordCase(keywordKw, keywordCase, keywordKw)} ${formattedContentPart}`;
        }

        return `${insertBlock}${contentBlock}`;
    });

    return result;
}

function formatIndentationRecursive(sql: string, indentString: string): string {
    const preLines = sql.split('\n');
    let openParenAtEndOfLine = 0;
    const processedLines: string[] = [];
    const openParenHadContent: boolean[] = [];

    // Pre-processing to handle trailing parentheses and split them if necessary
    for (let i = 0; i < preLines.length; i++) {
        const line = preLines[i];
        const trimmedEnd = line.trimEnd();
        const trimmed = line.trim();

        if (trimmedEnd.endsWith('(') && !trimmedEnd.match(/\([^)]*\)$/)) {
            openParenAtEndOfLine++;
            const hasContentBefore = trimmed !== '(';
            openParenHadContent.push(hasContentBefore);
        }

        if (trimmed.startsWith(')')) {
            openParenAtEndOfLine = Math.max(0, openParenAtEndOfLine - 1);
            openParenHadContent.pop();
            processedLines.push(line);
            continue;
        }

        if (openParenAtEndOfLine > 0) {
            let masked = line;
            masked = masked.replace(/(["'])(?:(?=(\\?))\2[\s\S])*?\1/g, (m) => " ".repeat(m.length));
            masked = masked.replace(/\[[^\]]*\]/g, (m) => " ".repeat(m.length));
            masked = masked.replace(/--.*$/, (m) => " ".repeat(m.length));
            masked = masked.replace(/\/\*[\s\S]*?\*\//g, (m) => " ".repeat(m.length));

            let balance = 0;
            let splitIndex = -1;

            for (let j = 0; j < masked.length; j++) {
                const char = masked[j];
                if (char === '(') { balance++; }
                else if (char === ')') {
                    balance--;
                    if (balance < 0) {
                        splitIndex = j;
                        break;
                    }
                }
            }

            if (splitIndex !== -1) {
                const before = line.substring(0, splitIndex);
                const after = line.substring(splitIndex);

                if (before.trim().length > 0) {
                    processedLines.push(before);
                }
                processedLines.push(after.trim());

                openParenAtEndOfLine--;
                openParenHadContent.pop();
                continue;
            }
        }

        processedLines.push(line);
    }

    const processedResult = processedLines.join('\n');
    const lines = processedResult.split('\n');
    let currentDepth = 0;
    const indentStack: number[] = [];

    const indentedLines = lines.map((line) => {
        let maskedLine = line;
        maskedLine = maskedLine.replace(/(["'])(?:(?=(\\?))\2[\s\S])*?\1/g, '""');
        maskedLine = maskedLine.replace(/\[[^\]]*\]/g, '[]');
        maskedLine = maskedLine.replace(/\/\*[\s\S]*?\*\//g, '');
        maskedLine = maskedLine.replace(/--.*$/, '');

        const openParens = (maskedLine.match(/\(/g) || []).length;
        const closeParens = (maskedLine.match(/\)/g) || []).length;
        const caseKeywords = (maskedLine.match(/\bCASE\b/gi) || []).length;
        const endKeywords = (maskedLine.match(/\bEND\b/gi) || []).length;
        const netChange = (openParens + caseKeywords) - (closeParens + endKeywords);

        const trimmed = line.trim();
        let effectiveDepth = currentDepth;
        const startsWithParen = trimmed.startsWith(')');
        const startsWithEnd = /^\b(END|WHEN|ELSE)\b/i.test(trimmed);

        if (startsWithParen || startsWithEnd) {
            effectiveDepth = Math.max(0, currentDepth - 1);
        }

        let thisLineIndentString = "";
        let thisLineIndentCount = 0;

        const canAlignWithStack = (startsWithParen || startsWithEnd) && indentStack.length > effectiveDepth;

        if (canAlignWithStack) {
            thisLineIndentCount = indentStack[effectiveDepth];
            thisLineIndentString = " ".repeat(thisLineIndentCount);
        } else if (effectiveDepth === 0) {
            const match = line.match(/^(\s*)/);
            thisLineIndentString = match ? match[1] : "";
            thisLineIndentCount = thisLineIndentString.length;
        } else {
            const parentIndent = indentStack[effectiveDepth - 1] || 0;
            const indentLen = indentString.indexOf('\t') !== -1 ? 4 : indentString.length;
            thisLineIndentCount = parentIndent + indentLen;
            thisLineIndentString = " ".repeat(thisLineIndentCount);
        }

        let extraIndent = "";
        if (netChange > 0) {
            for (let i = 0; i < netChange; i++) {
                indentStack.push(thisLineIndentCount);
            }
        } else if (netChange < 0) {
            for (let i = 0; i < Math.abs(netChange); i++) {
                indentStack.pop();
            }
        }

        currentDepth += netChange;
        if (currentDepth < 0) { currentDepth = 0; }

        return thisLineIndentString + extraIndent + trimmed;
    });

    return indentedLines.join('\n').trim();
}

function wrapSqlList(items: string[], indent: string, wrapLength: number): string {
    if (items.length === 0) { return ""; }
    if (wrapLength <= 0) { return indent + items.join(", "); }

    let currentLine = indent;
    const lines: string[] = [];

    for (let i = 0; i < items.length; i++) {
        const item = items[i];
        const separator = (i < items.length - 1) ? ", " : "";

        if (currentLine.length + item.length + separator.length > wrapLength && currentLine.trim() !== "") {
            lines.push(currentLine.trimEnd());
            currentLine = indent + item + separator;
        } else {
            currentLine += item + separator;
        }
    }
    if (currentLine.trim()) {
        lines.push(currentLine.trimEnd());
    }
    return lines.join("\n");
}

// --- Common Utils ---

function parseColumns(columnStr: string): string[] {
    const columns: string[] = [];
    let current = "";
    let depth = 0;

    for (const char of columnStr) {
        if (char === '(' || char === '[') {
            depth++;
            current += char;
        } else if (char === ')' || char === ']') {
            depth--;
            current += char;
        } else if (char === ',' && depth === 0) {
            if (current.trim()) {
                columns.push(current.trim());
            }
            current = "";
        } else {
            current += char;
        }
    }

    if (current.trim()) {
        columns.push(current.trim());
    }

    return columns;
}

function expandClauseColumns(sql: string, clause: string, keywordCase: string, indent: string): string {
    const pattern = new RegExp(`\\b${clause}\\b\\s+([^\\n]+)`, "gi");
    return sql.replace(pattern, (_match, columns) => {
        const keyword = applySqlKeywordCase(clause, keywordCase, clause);
        const cols = parseColumns(columns);
        if (cols.length > 2) {
            return keyword + "\n" + cols.map(c => indent + c).join(",\n");
        }
        return keyword + " " + columns;
    });
}

export function formatSqlContent(content: string, keywordCase: string, indentSpaces: number): string {
    const indentString = " ".repeat(indentSpaces);
    return formatSqlWithStyleImpl(content, "compact", keywordCase, indentSpaces, 0, indentString);
}

export function formatAsMultilineString(sql: string, quoteChar: string, wasStringLiteral: boolean): string {
    if (!wasStringLiteral) {
        return sql;
    }
    const closeQuote = quoteChar === '[' ? ']' : quoteChar;
    return `${quoteChar}${sql}${closeQuote}`;
}

export function formatAsConcatenatedString(sql: string, quoteChar: string): string {
    const closeQuote = quoteChar === '[' ? ']' : quoteChar;
    const lines = sql.split('\n');

    if (lines.length === 1) {
        return `${quoteChar}${sql}${closeQuote}`;
    }

    // Concatenate lines
    // Concatenate lines
    return lines.map((line, index) => {
        const prefix = quoteChar;
        const isLast = index === lines.length - 1;
        let content = line;

        if (!isLast && !content.endsWith(' ')) {
            content += ' ';
        }

        const suffix = isLast ? closeQuote : `${closeQuote} +`;
        return `${prefix}${content}${suffix}`;
    }).join('\n');
}
