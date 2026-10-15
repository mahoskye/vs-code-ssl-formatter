/**
 * SSL Language Constants
 *
 * The language data the extension's own features need. Everything else
 * (keywords, built-in functions and classes, their documentation) comes from
 * the language server, which renders it from the SSL reference.
 */

/** Built-ins whose first argument is SQL text with `?name?` placeholders. */
const INLINE_SQL_FUNCTIONS = [
    "SQLExecute",
    "GetDataSet",
    "GetDataSetWithSchemaFromSelect",
    "GetDataSetXMLFromSelect",
    "GetNETDataSet"
] as const;

/** Built-ins whose first argument is SQL text with positional `?` placeholders. */
const PARAMETERIZED_SQL_FUNCTIONS = [
    "RunSQL",
    "LSearch",
    "LSelect",
    "LSelect1",
    "LSelectC",
    "GetDataSetEx"
] as const;

/** Built-ins whose first argument the Format SQL command treats as SQL. */
export const SQL_CONTEXT_FUNCTIONS = [
    ...INLINE_SQL_FUNCTIONS,
    ...PARAMETERIZED_SQL_FUNCTIONS
] as const;
