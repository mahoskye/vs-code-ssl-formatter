/**
 * SQL Language Constants
 * Centralized definitions for SQL language elements used in formatting
 */

export const SQL_KW_SELECT = "SELECT";
export const SQL_KW_UPDATE = "UPDATE";
export const SQL_KW_DELETE = "DELETE";
export const SQL_KW_INSERT = "INSERT";
export const SQL_KW_VALUES = "VALUES";
export const SQL_KW_SET = "SET";
export const SQL_KW_WHERE = "WHERE";
export const SQL_KW_FROM = "FROM";
export const SQL_KW_JOIN = "JOIN";
export const SQL_KW_INTO = "INTO";
export const SQL_KW_ON = "ON";
export const SQL_KW_AND = "AND";
export const SQL_KW_OR = "OR";
export const SQL_KW_AS = "AS";
export const SQL_KW_IN = "IN";

export const SQL_KEYWORDS = new Set([
    SQL_KW_SELECT, "DISTINCT", "TOP", SQL_KW_AS, SQL_KW_ON, SQL_KW_IN, "NOT", "BETWEEN", "LIKE", "IS", "NULL",
    "INNER", "LEFT", "RIGHT", "FULL", "CROSS", SQL_KW_JOIN, SQL_KW_INSERT, SQL_KW_INTO, SQL_KW_VALUES,
    SQL_KW_UPDATE, SQL_KW_SET, SQL_KW_DELETE, SQL_KW_FROM, SQL_KW_WHERE, "GROUP", "BY", "ORDER", "HAVING",
    "UNION", "ALL", "EXCEPT", "INTERSECT", SQL_KW_AND, SQL_KW_OR, "ASC", "DESC", "LIMIT",
    "OFFSET", "EXISTS", "CASE", "WHEN", "THEN", "ELSE", "END"
]);

// Keywords that trigger line breaks BEFORE them in the formatter
export const SQL_BREAK_BEFORE_KEYWORDS = new Set([
    SQL_KW_FROM, SQL_KW_WHERE, 'INNER', 'LEFT', 'RIGHT', 'FULL', 'CROSS',
    'ORDER', 'GROUP', 'HAVING', 'UNION', SQL_KW_VALUES, SQL_KW_INTO, SQL_KW_ON
]);

// Join modifiers - if followed by JOIN, don't break before JOIN
export const SQL_JOIN_MODIFIERS = new Set([
    'INNER', 'LEFT', 'RIGHT', 'FULL', 'CROSS'
]);

// Keywords that get extra indentation (under WHERE)
export const SQL_INDENTED_KEYWORDS = new Set([
    SQL_KW_AND, SQL_KW_OR, SQL_KW_ON
]);
