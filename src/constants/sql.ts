/**
 * SQL Language Constants
 * Centralized definitions for SQL language elements used in formatting
 */

export const SQL = {
    KEYWORDS: {
        SELECT: "SELECT",
        UPDATE: "UPDATE",
        DELETE: "DELETE",
        INSERT: "INSERT",
        VALUES: "VALUES",
        SET: "SET",
        WHERE: "WHERE",
        FROM: "FROM",
        JOIN: "JOIN",
        INTO: "INTO",
        ON: "ON",
        AND: "AND",
        OR: "OR",
        AS: "AS",
        IN: "IN",
        DISTINCT: "DISTINCT",
        TOP: "TOP",
        NOT: "NOT",
        BETWEEN: "BETWEEN",
        LIKE: "LIKE",
        IS: "IS",
        NULL: "NULL",
        INNER: "INNER",
        LEFT: "LEFT",
        RIGHT: "RIGHT",
        FULL: "FULL",
        CROSS: "CROSS",
        GROUP: "GROUP",
        BY: "BY",
        ORDER: "ORDER",
        HAVING: "HAVING",
        UNION: "UNION",
        ALL: "ALL",
        EXCEPT: "EXCEPT",
        INTERSECT: "INTERSECT",
        ASC: "ASC",
        DESC: "DESC",
        LIMIT: "LIMIT",
        OFFSET: "OFFSET",
        EXISTS: "EXISTS",
        CASE: "CASE",
        WHEN: "WHEN",
        THEN: "THEN",
        ELSE: "ELSE",
        END: "END"
    },

    FORMATTING: {
        // Keywords that trigger line breaks BEFORE them in the formatter
        BREAK_BEFORE: new Set([
            "FROM", "WHERE", "INNER", "LEFT", "RIGHT", "FULL", "CROSS",
            "ORDER", "GROUP", "HAVING", "UNION", "VALUES", "INTO", "ON",
            "CASE", "WHEN", "ELSE", "END"
        ]),

        // Join modifiers - if followed by JOIN, don't break before JOIN
        JOIN_MODIFIERS: new Set([
            "INNER", "LEFT", "RIGHT", "FULL", "CROSS"
        ]),

        // Keywords that get extra indentation (under WHERE)
        INDENTED: new Set([
            "AND", "OR", "ON", "WHEN", "ELSE"
        ]),
    }
} as const;

export const SQL_KEYWORDS = new Set(Object.values(SQL.KEYWORDS));
