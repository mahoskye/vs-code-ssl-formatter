# SQLExecute Parameter Substitution in SSL

This document provides comprehensive documentation of the `?var?` parameter substitution system used by `SQLExecute` in STARLIMS Scripting Language (SSL).

## Overview

`SQLExecute` supports dynamic variable substitution using the `?expression?` placeholder syntax. Unlike `RunSQL` and `LSearch` which use positional `?` parameters with separate value arrays, `SQLExecute` parses expressions directly from the SQL string and resolves them at runtime.

> **Important:** `SQLExecute` is the **only** SSL database function that supports `?var?` parameter substitution. All other database functions (`RunSQL`, `LSearch`, `LSelect`, `GetDataSet`, etc.) require positional `?` placeholders with an explicit values array.

## Syntax

```
?expression?
```

The expression between the question marks is parsed and classified into one of seven types, each with specific behavior.

## Expression Types

### 1. Local Variable (`Local`)

The most common use case. A simple variable name resolves to its current value.

**Pattern:** `^\w+$` (word characters only, no special characters)

```ssl
:DECLARE sStatus, nLimit, dStartDate;
sStatus := "A";
nLimit := 100;
dStartDate := Today() - 30;

sSQL := "SELECT * FROM Sample WHERE Status = ?sStatus? AND RowNum <= ?nLimit? AND CreateDate >= ?dStartDate?";
aResults := SQLExecute(sSQL);
```

**Behavior:**
- Retrieves the value from the local scope via `RTS.getLocal()`
- If the variable is NIL, passes NULL to the database
- Works with all SSL types: SSLString, SSLDouble, SSLBool, SSLDate

---

### 2. Array Variable (`Array`)

When a local variable is an `SSLArray`, it automatically expands into multiple parameter placeholders.

**Pattern:** Same as Local, but the variable's runtime type is `SSLArray`

```ssl
:DECLARE aStatusCodes, aSampleIDs;
aStatusCodes := {"A", "P", "C"};
aSampleIDs := {"S001", "S002", "S003", "S004"};

/* ?aStatusCodes? expands to ?,?,? (3 placeholders);
sSQL := "SELECT * FROM Sample WHERE Status IN (?aStatusCodes?)";
aResults := SQLExecute(sSQL);

/* ?aSampleIDs? expands to ?,?,?,? (4 placeholders);
sSQL := "SELECT * FROM Sample WHERE SampleID IN (?aSampleIDs?)";
aResults := SQLExecute(sSQL);
```

**Behavior:**
- Counts array elements and generates one `?` placeholder per element
- Each element becomes a separate parameterized value
- Empty arrays generate a single NULL placeholder
- Ideal for dynamic `IN` clauses

---

### 3. Array Index Access (`ArrayIndex`)

Access specific elements of an array using bracket notation.

**Pattern:** `^\s*(?<varName>\w+)(\s*\[(?<index>(\s*\w+\s*,?\s*)+)\s*\])+\s*$`

```ssl
:DECLARE aTypes, aMatrix, nRow, nCol;
aTypes := {"Sample", "Reagent", "Standard"};
aMatrix := {{"A1", "B1"}, {"A2", "B2"}, {"A3", "B3"}};
nRow := 2;
nCol := 1;

/* Direct index (literal);
sSQL := "SELECT * FROM Items WHERE Type = ?aTypes[1]?";

/* Variable index;
sSQL := "SELECT * FROM Items WHERE Type = ?aTypes[nRow]?";

/* Multi-dimensional array access;
sSQL := "SELECT * FROM Grid WHERE Cell = ?aMatrix[nRow, nCol]?";

/* Alternative bracket syntax;
sSQL := "SELECT * FROM Grid WHERE Cell = ?aMatrix[2][1]?";
```

**Behavior:**
- Supports both literal integers and variable names as indices
- Supports multi-dimensional arrays with comma-separated indices
- Supports chained bracket notation `[i][j]`
- Index variables are resolved at runtime via `RTS.getLocal()`
- Throws `ArgumentNullException` if the array variable is NIL
- Throws `ArgumentException` if a variable index is not an integer

---

### 4. Object Property/Method Access (`Object`)

Access properties or call parameterless methods on objects using colon notation.

**Pattern:** `^\s*(?<objectName>\w+)(\s*:(?<memberName>(\s*\w+\s*(\(\s*\))?\s*)+)\s*)+\s*$`

```ssl
:DECLARE oUser, oSample, oConfig;

/* Property access;
sSQL := "SELECT * FROM AuditLog WHERE UserID = ?oUser:ID?";
sSQL := "SELECT * FROM Sample WHERE Status = ?oSample:Status?";

/* Method call (parameterless only);
sSQL := "SELECT * FROM Sample WHERE OwnerID = ?oUser:GetID()?";
sSQL := "SELECT * FROM Config WHERE Key = ?oConfig:GetCurrentKey()?";

/* Chained property access;
sSQL := "SELECT * FROM Sample WHERE LabID = ?oSample:Laboratory:ID?";

/* Mixed property and method;
sSQL := "SELECT * FROM Users WHERE DeptID = ?oUser:Department:GetID()?";
```

**Behavior:**
- Supports property access via `GetProperty()`
- Supports parameterless method calls via `Invoke()`
- Supports chained member access (multiple `:` separators)
- Special case: `me` is converted to `#me` for self-reference
- Returns the final resolved value

**Limitations:**
- Methods with parameters are NOT supported (e.g., `?oObj:GetValue(1)?` will fail)
- Only parameterless methods: `?oObj:Method()?`

---

### 5. Built-in Function Call (`FuncCall`)

Call parameterless built-in SSL functions directly in the expression.

**Pattern:** `^\s*\w+\s*\(\s*\)\s*$`

```ssl
/* Date/time functions;
sSQL := "SELECT * FROM Sample WHERE CreateDate = ?Today()?";
sSQL := "SELECT * FROM AuditLog WHERE Timestamp < ?Now()?";

/* Other parameterless built-ins;
sSQL := "SELECT * FROM Session WHERE GUID = ?CreateGUID()?";
```

**Behavior:**
- Only supports functions with zero parameters
- Function name is looked up in the built-in function library
- Uses reflection to find and invoke the method
- Results are cached for performance

**Supported Functions:**
Any built-in SSL function that takes no arguments, including:
- `Today()` - Current date
- `Now()` - Current date/time
- `CreateGUID()` - Generate new GUID
- Other zero-parameter built-ins

**Limitations:**
- Functions with parameters are NOT supported
- `?SubStr(sValue, 1, 5)?` will NOT work as a FuncCall
- Custom/user-defined functions are NOT supported

---

### 6. Constant Value (`Const`)

Embed literal values directly in the expression.

**Pattern:** Numeric value or single-quoted string

```ssl
/* String constant;
sSQL := "SELECT * FROM Sample WHERE Status = ?'A'?";

/* Numeric constants;
sSQL := "SELECT * FROM Sample WHERE Priority = ?1?";
sSQL := "SELECT * FROM Results WHERE Value > ?99.5?";
```

**Behavior:**
- Single-quoted strings: Strips quotes and passes as string parameter
- Integers: Parsed via `int.TryParse()`, passed as integer
- Decimals: Parsed via `double.TryParse()` with invariant culture

**Use Cases:**
- Rarely needed since you can just put the literal in the SQL
- May be useful for consistency or generated SQL patterns

---

### 7. Complex Expression (`Complex`)

Any expression containing non-word characters that doesn't match other patterns.

**Pattern:** Contains `[^\w]` (non-word characters) and doesn't match other specific patterns

```ssl
:DECLARE sPrefix, sSuffix, nBase, nOffset;
sPrefix := "SAMP";
sSuffix := "001";
nBase := 100;
nOffset := 50;

/* String concatenation;
sSQL := "SELECT * FROM Sample WHERE SampleID = ?sPrefix + sSuffix?";

/* Arithmetic expression;
sSQL := "SELECT * FROM Sample WHERE Priority > ?nBase + nOffset?";

/* Complex expressions;
sSQL := "SELECT * FROM Sample WHERE Code = ?Upper(sPrefix) + '-' + sSuffix?";
```

**Behavior:**
- Dynamically compiles and executes the expression as SSL code
- Wraps expression in `:RETURN expression;` and calls `execudf()`
- **Triggers a performance warning** in non-production mode

**Warning Message:**
```
Performance warning:
    Try not to use complex expressions in SqlExecute:
    sPrefix + sSuffix
```

**Recommendations:**
- Avoid complex expressions for performance-critical code
- Pre-compute complex values into a variable before the SQL
- Use simple variable substitution when possible

```ssl
/* Instead of this (triggers warning):;
sSQL := "SELECT * FROM Sample WHERE ID = ?sPrefix + sSuffix?";

/* Do this (no warning, better performance):;
sFullID := sPrefix + sSuffix;
sSQL := "SELECT * FROM Sample WHERE ID = ?sFullID?";
```

---

## Parameter Binding

All expressions are converted to parameterized queries for SQL injection protection:

1. Each `?expression?` is replaced with a database-specific parameter placeholder
2. Parameter names follow the pattern: `{prefix}Param_{index}`
3. Array expansions use: `{prefix}Param_{index}_{elementIndex}`
4. The prefix is determined by the database provider (e.g., `@` for SQL Server)

## Comparison with Other Database Functions

`SQLExecute` is the **only** function that supports `?var?` substitution. All other database functions use positional `?` placeholders.

### Feature Comparison

| Feature | SQLExecute | RunSQL / LSearch / LSelect / GetDataSet |
|---------|------------|----------------------------------------|
| Parameter syntax | `?varName?` | `?` (positional) |
| Value binding | Automatic from scope | Explicit array parameter |
| Array expansion | Automatic | Manual |
| Object access | Supported | Not supported |
| Function calls | Supported (no params) | Not supported |
| Complex expressions | Supported (with warning) | Not supported |

### Why Only SQLExecute?

In the C# implementation, only `SQLExecute` calls `GetPreparedStatement()`, which creates a `SqlParamsSsl` parser object. This parser:
1. Scans the SQL string for `?...?` patterns
2. Classifies each expression by type
3. Resolves values from the runtime scope
4. Replaces expressions with database-specific parameter placeholders

Other functions pass the SQL string directly to the database layer, expecting standard positional `?` parameters with values provided in an explicit array.

### Syntax Examples by Function

```ssl
:DECLARE sStatus, sSampleID, aResults, sName, nCount;
sStatus := "A";
sSampleID := "S001";

/* SQLExecute - uses ?varName? syntax, no values array needed;
aResults := SQLExecute("SELECT * FROM Sample WHERE Status = ?sStatus?");

/* RunSQL - uses positional ? with explicit values array;
RunSQL("UPDATE Sample SET Status = ? WHERE SampleID = ?",, {sStatus, sSampleID});

/* LSearch - uses positional ? with explicit values array;
sName := LSearch("SELECT Name FROM Sample WHERE SampleID = ?", "",, {sSampleID});

/* LSelect / LSelect1 - uses positional ? with explicit values array;
aResults := LSelect1("SELECT * FROM Sample WHERE Status = ?",, {sStatus});

/* GetDataSet - uses positional ? with explicit values array;
sXml := GetDataSet("SELECT * FROM Sample WHERE Status = ?", {sStatus});
```

### When to Use Each Function

| Function | Purpose | Supports SELECT | Supports DML | Returns |
|----------|---------|-----------------|--------------|---------|
| `SQLExecute` | Universal - routes automatically | Yes | Yes | Array, XML, Dataset, or Bool |
| `RunSQL` | **DML only** (INSERT/UPDATE/DELETE) | No* | Yes | `SSLBool` (success/failure) |
| `LSearch` | Single value lookups | Yes | No | Single value with default |
| `LSelect1` | Multi-row SELECT queries | Yes | No | 2D Array |
| `GetDataSet` | XML dataset output | Yes | No | XML String |

\* `RunSQL` will execute a SELECT statement but returns only `SSLBool`, not the result data.

### How SQLExecute Routes Internally

`SQLExecute` inspects the SQL string and routes to the appropriate handler:

```
SQLExecute(sSQL)
    │
    ├─► Starts with SELECT? ──► Yes ──► returnType = "array"? ──► LSelect1() ──► Returns 2D Array
    │                                   returnType = "xml"?   ──► GetDataSetEx() ──► Returns XML
    │                                   returnType = "dataset"? ──► SQLReturnDataSet() ──► Returns Dataset
    │
    └─► Not SELECT (DML) ──► RunSQL() ──► Returns SSLBool
```

This is why `SQLExecute` is the most versatile function - it handles both queries and DML with automatic routing.

## Best Practices

1. **Use simple variables** for best performance
2. **Pre-compute complex expressions** into variables before the SQL
3. **Use array expansion** for dynamic `IN` clauses instead of string building
4. **Avoid complex expressions** in production code
5. **Use `RunSQL`** for DML statements (INSERT, UPDATE, DELETE)
6. **Use `LSearch`** for single-value lookups with explicit parameters

## Source Reference

Implementation details derived from `DatabaseLib.cs`:
- `SqlParamsSsl` class (lines 19-302)
- `ReplaceMatch` method (lines 88-158) - Expression classification
- `ReplaceValues` method (lines 160-286) - Value resolution
- Pattern regex: `\?[^\?]+\?` (line 85)
