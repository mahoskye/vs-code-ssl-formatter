# SQL Placeholder Reference

This document captures current knowledge about STARLIMS SQL placeholder support so we can build better IntelliSense, hover hints, and diagnostics.

## Placeholder Types

| Placeholder | Example | Description | When to use |
| --- | --- | --- | --- |
| Anonymous | `SELECT * FROM TPatient WHERE PatientID = ?;` | Position-based placeholder resolved by passing an array of values as the final argument to the SQL function. | Quick prototypes or single-parameter statements where readability is not critical. |
| Named | `SELECT * FROM TPatient WHERE PatientID = ?nPatientId?;` | Placeholder embeds a descriptive name between `?` markers and maps to a same-named SSL variable. | Preferred in production code—improves readability and lets diagnostics verify that a variable exists before execution. |

Guidelines:

- Stick to lowercase Hungarian prefixes when naming parameters (e.g., `?sName?`, `?nPatientId?`). The diagnostic provider enforces lowercase names.
- Avoid string concatenation inside SQL literals; rely on placeholders plus the value array/arguments to pass data.
- Always include whitespace around the placeholder to keep SQL linters and formatters happy.
- **Rule of thumb:** Each helper has a single placeholder style: either positional `?` (when it accepts an `aParameters` argument) or named `?param?` (no parameter array). There are no mixed modes—if the function exposes an array parameter at all, it always uses positional placeholders.

## Functions by Placeholder Style

### Named `?param?` Placeholders (SQL string only)

| Function | Signature (from `ssl_agent_instructions.md`) | Notes |
| --- | --- | --- |
| `SQLExecute(sSQL, sConnectionName)` | Inline placeholders resolve against in-scope SSL variables. |

### Anonymous `?` Placeholders (SQL string + parameter array)

| Function | Signature | Notes |
| --- | --- | --- |
| `RunSQL(sSQL, sConnectionName, bReturnRecordsAffected, aParameters)` | Provide ordered values in `aParameters` to match each `?`. |
| `LSearch(sSQL, nMaxRows, sConnectionName, aParameters)` | Pass an array matching positional placeholders; optional row cap. |
| `LSelect(sSQL, sConnectionName, aParameters)` | Returns first column of first row; positional placeholders only. |
| `LSelect1(sSQL, sConnectionName, aParameters)` | Returns first row as an array. |
| `LSelectC(sSQL, sConnectionName, aParameters)` | Returns count/numeric values. |
| `GetDataSet(sSQL, sConnectionName, aParameters)` | Dataset helper that always binds positional `?` placeholders. |
| `GetDataSetWithSchemaFromSelect(sSQL, sConnectionName, aParameters)` | Schema-aware dataset helper; placeholders must be positional. |
| `GetDataSetXMLFromSelect(sSQL, sConnectionName, aParameters)` | XML-producing wrapper; uses positional placeholders. |
| `GetNETDataSet(sSQL, sConnectionName, aParameters)` | .NET dataset helper; accepts only positional placeholders. |
| `GetDataSetEx(sSQL, sConnectionName, aParameters)` | Dataset helper requiring positional placeholders. |

> Source: Canonical signatures recorded in `ssl_agent_instructions.md` (Function Reference section).

## Validation Behavior

- `sslDiagnosticProvider` searches each string literal for `?name?` patterns (see `PATTERNS.SQL_PARAMETER_PLACEHOLDER`) and verifies the referenced variable exists in scope.
- SQL injection warnings trigger when concatenation (`+`) is used inside SQL strings without placeholders, especially when the string contains `WHERE`, `SET`, or `VALUES`.
- Linting can be relaxed via `ssl.security.preventSqlInjection=false`, but the placeholder reference check still runs to prevent typos.

## Upcoming Work

The following GitHub issues depend on this reference:

- #17 (this document) – baseline research.
- #15 / #13 – hover hints explaining placeholder purpose and matching variables.
- #10 – SQL formatting needs to preserve placeholders during reflow.
- #29 – formatter alignment rules for multi-line SQL argument lists.

Open questions:

1. Do other SQL helper procedures (e.g., project-specific wrappers) follow the same placeholder semantics? If so, add them above.
2. Should we expose configuration to declare custom SQL functions that expect placeholders?
3. Do any functions accept both positional arrays and dictionary-style maps? Current assumption: arrays only.

Please update this document whenever we discover new functions or conventions. It keeps tooling behavior transparent to end users.
