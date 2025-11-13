/**
 * SSL Language Constants
 * Centralized definitions for SSL language elements
 */

export const SSL_KEYWORDS = [
    "IF", "ELSE", "ENDIF",
    "WHILE", "ENDWHILE",
    "FOR", "TO", "STEP", "NEXT", "EXITFOR",
    "EXITWHILE", "LOOP",
    "BEGINCASE", "CASE", "OTHERWISE", "ENDCASE", "EXITCASE",
    "TRY", "CATCH", "FINALLY", "ENDTRY",
    "DECLARE", "DEFAULT", "PARAMETERS", "PUBLIC",
    "INCLUDE", "PROCEDURE", "ENDPROC", "RETURN",
    "CLASS", "INHERIT",
    "REGION", "ENDREGION",
    "BEGININLINECODE", "ENDINLINECODE",
    "ERROR", "LABEL"
];

export const BLOCK_START_KEYWORDS = [
    "IF", "WHILE", "FOR", "BEGINCASE", "TRY", "PROCEDURE", "CLASS", "REGION"
];

export const BLOCK_END_KEYWORDS = [
    "ENDIF", "ENDWHILE", "NEXT", "ENDCASE", "ENDTRY", "ENDPROC", "ENDREGION"
];

export const BLOCK_MIDDLE_KEYWORDS = [
    "ELSE", "CATCH", "FINALLY"
];

export const CASE_KEYWORDS = [
    "CASE", "OTHERWISE"
];

export const PROCEDURE_LEVEL_KEYWORDS = [
    "PARAMETERS", "DECLARE", "DEFAULT", "RETURN", "PUBLIC"
];

export const MULTILINE_CONSTRUCT_KEYWORDS = [
    "IF", "ELSE", "WHILE", "FOR", "TO", "STEP", "BEGINCASE", "CASE", "OTHERWISE", "EXITCASE",
    "TRY", "CATCH", "FINALLY", "PROCEDURE", "PARAMETERS", "DEFAULT", "CLASS", "INHERIT", "REGION",
    "ENDIF", "ENDWHILE", "NEXT", "ENDCASE", "ENDTRY", "ENDPROC", "ENDREGION",
    "LOOP", "EXITWHILE", "RETURN"
];

export interface SSLFunction {
    name: string;
    description: string;
    params: string;
    returns?: string;
    signature?: string; // Full typed signature like "SQLExecute(any commandString, any friendlyName, ...)"
    returnType?: string; // Specific return type like "any", "boolean", "string", etc.
    category?: string; // Category like "Database Functions", "String Functions", etc.
    frequency?: string; // Usage frequency like "Very High", "High", "Moderate", "Low"
    untypedSignature?: string; // Untyped signature like "SQLExecute(commandString, friendlyName, ...)"
}

export const SSL_BUILTIN_FUNCTIONS: SSLFunction[] = [
    {
        "name": "SQLExecute",
        "description": "Executes a SQL command and returns the result as an SSLValue.",
        "params": "(any commandString, any friendlyName, any rollbackExistingTransaction, any nullAsBlank, any invariantDateColumns, any returnType, any tableName, any includeSchema, any includeHeader)",
        "returns": "any",
        "signature": "SQLExecute(any commandString, any friendlyName, any rollbackExistingTransaction, any nullAsBlank, any invariantDateColumns, any returnType, any tableName, any includeSchema, any includeHeader)",
        "returnType": "any",
        "category": "Database Functions",
        "frequency": "Very High",
        "untypedSignature": "SQLExecute(commandString, friendlyName, rollbackExistingTransaction, nullAsBlank, invariantDateColumns, returnType, tableName, includeSchema, includeHeader)"
    },
    {
        "name": "Empty",
        "description": "Checks if the given value is empty or null.",
        "params": "(any value)",
        "returns": "boolean",
        "signature": "Empty(any value)",
        "returnType": "boolean",
        "category": "String Functions",
        "frequency": "Very High",
        "untypedSignature": "Empty(value)"
    },
    {
        "name": "DoProc",
        "description": "Executes a stored procedure with the provided arguments.",
        "params": "(any[] args)",
        "returns": "any",
        "signature": "DoProc(any[] args)",
        "returnType": "any",
        "category": "Runtime Functions",
        "frequency": "Very High",
        "untypedSignature": "DoProc(args)"
    },
    {
        "name": "Len",
        "description": "Returns the length of the input string.",
        "params": "(any source)",
        "returns": "number",
        "signature": "Len(any source)",
        "returnType": "number",
        "category": "String Functions",
        "frequency": "Very High",
        "untypedSignature": "Len(source)"
    },
    {
        "name": "LimsString",
        "description": "Converts the input value to a string representation.",
        "params": "(any source)",
        "returns": "string",
        "signature": "LimsString(any source)",
        "returnType": "string",
        "category": "String Functions",
        "frequency": "Very High",
        "untypedSignature": "LimsString(source)"
    },
    {
        "name": "ExecFunction",
        "description": "Executes a function by name with given parameters.",
        "params": "(string name, object[] parameters)",
        "returns": "any",
        "signature": "ExecFunction(string name, object[] parameters)",
        "returnType": "any",
        "category": "Runtime Functions",
        "frequency": "High",
        "untypedSignature": "ExecFunction(name, parameters)"
    },
    {
        "name": "usrmes",
        "description": "Displays a user message with two arguments.",
        "params": "(a, b)",
        "returns": "void",
        "signature": "usrmes(a, b)",
        "returnType": "void",
        "category": "System Functions",
        "frequency": "High",
        "untypedSignature": "usrmes(a, b)"
    },
    {
        "name": "Upper",
        "description": "Converts the input string to uppercase.",
        "params": "(string source)",
        "returns": "string",
        "signature": "Upper(string source)",
        "returnType": "string",
        "category": "String Functions",
        "frequency": "High",
        "untypedSignature": "Upper(source)"
    },
    {
        "name": "aadd",
        "description": "Adds an element to the end of an array.",
        "params": "(any[] target, any element)",
        "returns": "any",
        "signature": "aadd(any[] target, any element)",
        "returnType": "any",
        "category": "Array Functions",
        "frequency": "High",
        "untypedSignature": "aadd(target, element)"
    },
    {
        "name": "Chr",
        "description": "Returns the character corresponding to the given ASCII code.",
        "params": "(number asciiCode)",
        "returns": "string",
        "signature": "Chr(number asciiCode)",
        "returnType": "string",
        "category": "String Functions",
        "frequency": "High",
        "untypedSignature": "Chr(asciiCode)"
    },
    {
        "name": "AllTrim",
        "description": "Removes all leading and trailing whitespace from the string.",
        "params": "(string source)",
        "returns": "string",
        "signature": "AllTrim(string source)",
        "returnType": "string",
        "category": "String Functions",
        "frequency": "High",
        "untypedSignature": "AllTrim(source)"
    },
    {
        "name": "RunSQL",
        "description": "Executes a SQL command without returning data.",
        "params": "(string commandString, string friendlyName, any arrayOfValues)",
        "returns": "boolean",
        "signature": "RunSQL(string commandString, string friendlyName, any arrayOfValues)",
        "returnType": "boolean",
        "category": "Database Functions",
        "frequency": "High",
        "untypedSignature": "RunSQL(commandString, friendlyName, arrayOfValues)"
    },
    {
        "name": "SubStr",
        "description": "Extracts a substring from the source string.",
        "params": "(string source, number startPos, number length)",
        "returns": "string",
        "signature": "SubStr(string source, number startPos, number length)",
        "returnType": "string",
        "category": "String Functions",
        "frequency": "High",
        "untypedSignature": "SubStr(source, startPos, length)"
    },
    {
        "name": "Now",
        "description": "Returns the current date and time.",
        "params": "()",
        "returns": "Date",
        "signature": "Now()",
        "returnType": "Date",
        "category": "Date Functions",
        "frequency": "High",
        "untypedSignature": "Now()"
    },
    {
        "name": "GetSetting",
        "description": "Retrieves the value of a system setting by name.",
        "params": "(string name)",
        "returns": "any",
        "signature": "GetSetting(string name)",
        "returnType": "any",
        "category": "System Functions",
        "frequency": "High",
        "untypedSignature": "GetSetting(name)"
    },
    {
        "name": "Left",
        "description": "Returns the leftmost characters of the string.",
        "params": "(string source, number length)",
        "returns": "string",
        "signature": "Left(string source, number length)",
        "returnType": "string",
        "category": "String Functions",
        "frequency": "High",
        "untypedSignature": "Left(source, length)"
    },
    {
        "name": "Val",
        "description": "Converts a string to a numeric value.",
        "params": "(string sNumber)",
        "returns": "number",
        "signature": "Val(string sNumber)",
        "returnType": "number",
        "category": "Numeric Functions",
        "frequency": "High",
        "untypedSignature": "Val(sNumber)"
    },
    {
        "name": "LSearch",
        "description": "Performs a search query on the database.",
        "params": "(string sql, object[] parameters)",
        "returns": "string",
        "signature": "LSearch(string sql, object[] parameters)",
        "returnType": "string",
        "category": "Database Functions",
        "frequency": "High",
        "untypedSignature": "LSearch(sql, parameters)"
    },
    {
        "name": "extractcol",
        "description": "Extracts a column from a 2D array.",
        "params": "(any[] target, number column)",
        "returns": "any[]",
        "signature": "extractcol(any[] target, number column)",
        "returnType": "any[]",
        "category": "Array Functions",
        "frequency": "High",
        "untypedSignature": "extractcol(target, column)"
    },
    {
        "name": "At",
        "description": "Finds the position of a substring within a string.",
        "params": "(string subString, string source)",
        "returns": "number",
        "signature": "At(string subString, string source)",
        "returnType": "number",
        "category": "String Functions",
        "frequency": "High",
        "untypedSignature": "At(subString, source)"
    },
    {
        "name": "Trim",
        "description": "Removes leading and trailing whitespace from the string.",
        "params": "(string source)",
        "returns": "string",
        "signature": "Trim(string source)",
        "returnType": "string",
        "category": "String Functions",
        "frequency": "High",
        "untypedSignature": "Trim(source)"
    },
    {
        "name": "Max",
        "description": "Returns the maximum of two values.",
        "params": "(any value1, any value2)",
        "returns": "any",
        "signature": "Max(any value1, any value2)",
        "returnType": "any",
        "category": "Numeric Functions",
        "frequency": "High",
        "untypedSignature": "Max(value1, value2)"
    },
    {
        "name": "Time",
        "description": "Returns the current time as a string.",
        "params": "()",
        "returns": "void",
        "signature": "Time()",
        "returnType": "void",
        "category": "Date Functions",
        "frequency": "High",
        "untypedSignature": "Time()"
    },
    {
        "name": "IIf",
        "description": "Returns one of two values based on a condition.",
        "params": "(boolean condition, any trueValue, any falseValue)",
        "returns": "any",
        "signature": "IIf(boolean condition, any trueValue, any falseValue)",
        "returnType": "any",
        "category": "System Functions",
        "frequency": "High",
        "untypedSignature": "IIf(condition, trueValue, falseValue)"
    },
    {
        "name": "StrTran",
        "description": "Replaces occurrences of a substring in a string.",
        "params": "(string source, string searchFor, string replaceWith)",
        "returns": "string",
        "signature": "StrTran(string source, string searchFor, string replaceWith)",
        "returnType": "string",
        "category": "String Functions",
        "frequency": "High",
        "untypedSignature": "StrTran(source, searchFor, replaceWith)"
    },
    {
        "name": "CreateUdObject",
        "description": "Creates a user-defined object with arguments.",
        "params": "(any[] args)",
        "returns": "object",
        "signature": "CreateUdObject(any[] args)",
        "returnType": "object",
        "frequency": "High",
        "untypedSignature": "CreateUdObject(args)"
    },
    {
        "name": "RaiseError",
        "description": "Raises an error with message, location, and code.",
        "params": "(string message, string location, number errorCode, Error innerException)",
        "returns": "boolean",
        "signature": "RaiseError(string message, string location, number errorCode, Error innerException)",
        "returnType": "boolean",
        "category": "Error Functions",
        "frequency": "High",
        "untypedSignature": "RaiseError(message, location, errorCode, innerException)"
    },
    {
        "name": "Today",
        "description": "Returns the current date.",
        "params": "()",
        "returns": "Date",
        "signature": "Today()",
        "returnType": "Date",
        "category": "Date Functions",
        "frequency": "High",
        "untypedSignature": "Today()"
    },
    {
        "name": "GetDataset",
        "description": "Retrieves a dataset from a data source.",
        "params": "(string dataSourceId, object[] parameters, string returnType)",
        "returns": "any",
        "signature": "GetDataset(string dataSourceId, object[] parameters, string returnType)",
        "returnType": "any",
        "category": "Database Functions",
        "frequency": "High",
        "untypedSignature": "GetDataset(dataSourceId, parameters, returnType)"
    },
    {
        "name": "buildstring",
        "description": "Builds a string from array elements with a delimiter.",
        "params": "(any[] target, number start, number count, string delimiter)",
        "returns": "string",
        "signature": "buildstring(any[] target, number start, number count, string delimiter)",
        "returnType": "string",
        "category": "Array Functions",
        "frequency": "High",
        "untypedSignature": "buildstring(target, start, count, delimiter)"
    },
    {
        "name": "Str",
        "description": "Formats a number as a string with specified length and decimals.",
        "params": "(number number, number length, number decimals)",
        "returns": "string",
        "signature": "Str(number number, number length, number decimals)",
        "returnType": "string",
        "category": "String Functions",
        "frequency": "High",
        "untypedSignature": "Str(number, length, decimals)"
    },
    {
        "name": "EndLimsTransaction",
        "description": "Ends a LIMS database transaction.",
        "params": "(string friendlyName, boolean commit)",
        "returns": "boolean",
        "signature": "EndLimsTransaction(string friendlyName, boolean commit)",
        "returnType": "boolean",
        "category": "Database Functions",
        "frequency": "Moderate",
        "untypedSignature": "EndLimsTransaction(friendlyName, commit)"
    },
    {
        "name": "Replace",
        "description": "Replaces all occurrences of a substring in a string.",
        "params": "(string source, string searchFor, string replaceWith)",
        "returns": "string",
        "signature": "Replace(string source, string searchFor, string replaceWith)",
        "returnType": "string",
        "category": "String Functions",
        "frequency": "Moderate",
        "untypedSignature": "Replace(source, searchFor, replaceWith)"
    },
    {
        "name": "FileSupport",
        "description": "Performs file operations based on the request type.",
        "params": "(any fileIdentifier, any request, any arg1, any arg2, any encoding)",
        "returns": "any",
        "signature": "FileSupport(any fileIdentifier, any request, any arg1, any arg2, any encoding)",
        "returnType": "any",
        "category": "File Functions",
        "frequency": "Moderate",
        "untypedSignature": "FileSupport(fileIdentifier, request, arg1, arg2, encoding)"
    },
    {
        "name": "LimsNETConnect",
        "description": "Connects to a .NET assembly and type.",
        "params": "(string assembly, string typeName, any[] args, any asStatic)",
        "returns": "any",
        "signature": "LimsNETConnect(string assembly, string typeName, any[] args, any asStatic)",
        "returnType": "any",
        "category": "Builtin Functions",
        "frequency": "Moderate",
        "untypedSignature": "LimsNETConnect(assembly, typeName, args, asStatic)"
    },
    {
        "name": "Right",
        "description": "Returns the rightmost characters of the string.",
        "params": "(string source, number length)",
        "returns": "string",
        "signature": "Right(string source, number length)",
        "returnType": "string",
        "category": "String Functions",
        "frequency": "Moderate",
        "untypedSignature": "Right(source, length)"
    },
    {
        "name": "GetDataSetXMLFromArray",
        "description": "Converts an array to XML dataset format.",
        "params": "(any[] arrayOfValues, any[] arrayFields, string tableName, boolean includeHeader, boolean includeSchema)",
        "returns": "string",
        "signature": "GetDataSetXMLFromArray(any[] arrayOfValues, any[] arrayFields, string tableName, boolean includeHeader, boolean includeSchema)",
        "returnType": "string",
        "category": "Database Functions",
        "frequency": "Moderate",
        "untypedSignature": "GetDataSetXMLFromArray(arrayOfValues, arrayFields, tableName, includeHeader, includeSchema)"
    },
    {
        "name": "LimsTypeEx",
        "description": "Returns the extended type information of a value.",
        "params": "(any source)",
        "returns": "string",
        "signature": "LimsTypeEx(any source)",
        "returnType": "string",
        "category": "Datatype Functions",
        "frequency": "Moderate",
        "untypedSignature": "LimsTypeEx(source)"
    },
    {
        "name": "GetLastSSLError",
        "description": "Retrieves the last SSL error that occurred.",
        "params": "()",
        "returns": "Error",
        "signature": "GetLastSSLError()",
        "returnType": "Error",
        "category": "Error Functions",
        "frequency": "Moderate",
        "untypedSignature": "GetLastSSLError()"
    },
    {
        "name": "BuildStringForIn",
        "description": "Builds a comma-separated string for SQL IN clause.",
        "params": "(any[] target)",
        "returns": "string",
        "signature": "BuildStringForIn(any[] target)",
        "returnType": "string",
        "category": "Array Functions",
        "frequency": "Moderate",
        "untypedSignature": "BuildStringForIn(target)"
    },
    {
        "name": "LimsTime",
        "description": "Returns the current time in LIMS format.",
        "params": "()",
        "returns": "string",
        "signature": "LimsTime()",
        "returnType": "string",
        "category": "Date Functions",
        "frequency": "Moderate",
        "untypedSignature": "LimsTime()"
    },
    {
        "name": "buildarray",
        "description": "Builds an array from a string with delimiters.",
        "params": "(string text, boolean crlfOk, string delimiter, boolean unique, boolean trimSpaces)",
        "returns": "any[]",
        "signature": "buildarray(string text, boolean crlfOk, string delimiter, boolean unique, boolean trimSpaces)",
        "returnType": "any[]",
        "category": "Array Functions",
        "frequency": "Moderate",
        "untypedSignature": "buildarray(text, crlfOk, delimiter, unique, trimSpaces)"
    },
    {
        "name": "BeginLimsTransaction",
        "description": "Begins a LIMS database transaction.",
        "params": "(any friendlyName, any isoLevel)",
        "returns": "any",
        "signature": "BeginLimsTransaction(any friendlyName, any isoLevel)",
        "returnType": "any",
        "category": "Database Functions",
        "frequency": "Moderate",
        "untypedSignature": "BeginLimsTransaction(friendlyName, isoLevel)"
    },
    {
        "name": "getregion",
        "description": "Retrieves a region from a string based on delimiters.",
        "params": "(any s, any src, any dst)",
        "returns": "string",
        "signature": "getregion(any s, any src, any dst)",
        "returnType": "string",
        "category": "Builtin Functions",
        "frequency": "Moderate",
        "untypedSignature": "getregion(s, src, dst)"
    },
    {
        "name": "ascan",
        "description": "Scans an array for a value and returns its index.",
        "params": "(any[] target, any value, number start, number count)",
        "returns": "number",
        "signature": "ascan(any[] target, any value, number start, number count)",
        "returnType": "number",
        "category": "Array Functions",
        "frequency": "Moderate",
        "untypedSignature": "ascan(target, value, start, count)"
    },
    {
        "name": "LimsSetCounter",
        "description": "Sets a counter value in the database.",
        "params": "(string tableName, string fieldName, string prefix, any[] arrayOfFields, any[] arrayOfValues, any incrementWith = null)",
        "returns": "number",
        "signature": "LimsSetCounter(string tableName, string fieldName, string prefix, any[] arrayOfFields, any[] arrayOfValues, any incrementWith = null)",
        "returnType": "number",
        "category": "Database Functions",
        "frequency": "Moderate",
        "untypedSignature": "LimsSetCounter(tableName, fieldName, prefix, arrayOfFields, arrayOfValues, incrementWith)"
    },
    {
        "name": "Directory",
        "description": "Lists files in a directory matching a pattern.",
        "params": "(string filePattern, string attributes)",
        "returns": "any",
        "signature": "Directory(string filePattern, string attributes)",
        "returnType": "any",
        "category": "File Functions",
        "frequency": "Moderate",
        "untypedSignature": "Directory(filePattern, attributes)"
    },
    {
        "name": "ToXml",
        "description": "Converts a value to XML string.",
        "params": "(any o, string typeName)",
        "returns": "string",
        "signature": "ToXml(any o, string typeName)",
        "returnType": "string",
        "category": "Xml Functions",
        "frequency": "Moderate",
        "untypedSignature": "ToXml(o, typeName)"
    },
    {
        "name": "LimsRecordsAffected",
        "description": "Returns the number of records affected by the last SQL operation.",
        "params": "()",
        "returns": "number",
        "signature": "LimsRecordsAffected()",
        "returnType": "number",
        "category": "Database Functions",
        "frequency": "Moderate",
        "untypedSignature": "LimsRecordsAffected()"
    },
    {
        "name": "LimsSqlConnect",
        "description": "Connects to a SQL database by friendly name.",
        "params": "(string friendlyName)",
        "returns": "boolean",
        "signature": "LimsSqlConnect(string friendlyName)",
        "returnType": "boolean",
        "category": "Database Functions",
        "frequency": "Moderate",
        "untypedSignature": "LimsSqlConnect(friendlyName)"
    },
    {
        "name": "WriteText",
        "description": "Writes text to a file.",
        "params": "(string fileName, string charsToWrite, string confirmRequired, string append, any encoding)",
        "returns": "string",
        "signature": "WriteText(string fileName, string charsToWrite, string confirmRequired, string append, any encoding)",
        "returnType": "string",
        "category": "File Functions",
        "frequency": "Moderate",
        "untypedSignature": "WriteText(fileName, charsToWrite, confirmRequired, append, encoding)"
    },
    {
        "name": "Year",
        "description": "Extracts the year from a date.",
        "params": "(Date date)",
        "returns": "number",
        "signature": "Year(Date date)",
        "returnType": "number",
        "category": "Date Functions",
        "frequency": "Moderate",
        "untypedSignature": "Year(date)"
    },
    {
        "name": "LimsSqlDisconnect",
        "description": "Disconnects from a SQL database.",
        "params": "(string friendlyName)",
        "returns": "boolean",
        "signature": "LimsSqlDisconnect(string friendlyName)",
        "returnType": "boolean",
        "category": "Database Functions",
        "frequency": "Moderate",
        "untypedSignature": "LimsSqlDisconnect(friendlyName)"
    },
    {
        "name": "arraycalc",
        "description": "Performs calculations on array elements.",
        "params": "(any[] target, string operation, any value, number start, number count)",
        "returns": "any",
        "signature": "arraycalc(any[] target, string operation, any value, number start, number count)",
        "returnType": "any",
        "category": "Array Functions",
        "frequency": "Moderate",
        "untypedSignature": "arraycalc(target, operation, value, start, count)"
    },
    {
        "name": "ReturnLastSQLError",
        "description": "Returns the last SQL error.",
        "params": "()",
        "returns": "Error",
        "signature": "ReturnLastSQLError()",
        "returnType": "Error",
        "category": "Database Functions",
        "frequency": "Moderate",
        "untypedSignature": "ReturnLastSQLError()"
    },
    {
        "name": "Min",
        "description": "Returns the minimum of two values.",
        "params": "(any value1, any value2)",
        "returns": "any",
        "signature": "Min(any value1, any value2)",
        "returnType": "any",
        "category": "Numeric Functions",
        "frequency": "Moderate",
        "untypedSignature": "Min(value1, value2)"
    },
    {
        "name": "Asc",
        "description": "Returns the ASCII code of the first character in a string.",
        "params": "(string source)",
        "returns": "number",
        "signature": "Asc(string source)",
        "returnType": "number",
        "category": "String Functions",
        "frequency": "Moderate",
        "untypedSignature": "Asc(source)"
    },
    {
        "name": "StrZero",
        "description": "Formats a number as a zero-padded string.",
        "params": "(number number, number length, number decimals)",
        "returns": "string",
        "signature": "StrZero(number number, number length, number decimals)",
        "returnType": "string",
        "category": "String Functions",
        "frequency": "Moderate",
        "untypedSignature": "StrZero(number, length, decimals)"
    },
    {
        "name": "IsTable",
        "description": "Checks if a table exists in the database.",
        "params": "(string friendlyName, string tableName)",
        "returns": "boolean",
        "signature": "IsTable(string friendlyName, string tableName)",
        "returnType": "boolean",
        "category": "Database Functions",
        "frequency": "Moderate",
        "untypedSignature": "IsTable(friendlyName, tableName)"
    },
    {
        "name": "Lower",
        "description": "Converts the input string to lowercase.",
        "params": "(string source)",
        "returns": "string",
        "signature": "Lower(string source)",
        "returnType": "string",
        "category": "String Functions",
        "frequency": "Moderate",
        "untypedSignature": "Lower(source)"
    },
    {
        "name": "CreateGUID",
        "description": "Generates a new globally unique identifier.",
        "params": "()",
        "returns": "string",
        "signature": "CreateGUID()",
        "returnType": "string",
        "category": "System Functions",
        "frequency": "Moderate",
        "untypedSignature": "CreateGUID()"
    },
    {
        "name": "LSelect1",
        "description": "Executes a SELECT query and returns results as array.",
        "params": "(string commandString, string friendlyName, any[] arrayOfValues, boolean nullAsBlank, any[] invariantDateColumns)",
        "returns": "any[]",
        "signature": "LSelect1(string commandString, string friendlyName, any[] arrayOfValues, boolean nullAsBlank, any[] invariantDateColumns)",
        "returnType": "any[]",
        "category": "Database Functions",
        "frequency": "Moderate",
        "untypedSignature": "LSelect1(commandString, friendlyName, arrayOfValues, nullAsBlank, invariantDateColumns)"
    },
    {
        "name": "LimsType",
        "description": "Returns the type information of a value.",
        "params": "(any param)",
        "returns": "string",
        "signature": "LimsType(any param)",
        "returnType": "string",
        "category": "Datatype Functions",
        "frequency": "Moderate",
        "untypedSignature": "LimsType(param)"
    },
    {
        "name": "LIMSDate",
        "description": "Formats a date according to a specified format.",
        "params": "(any date, string format)",
        "returns": "string",
        "signature": "LIMSDate(any date, string format)",
        "returnType": "string",
        "category": "Date Functions",
        "frequency": "Moderate",
        "untypedSignature": "LIMSDate(date, format)"
    },
    {
        "name": "FromXml",
        "description": "Converts an XML string to a value.",
        "params": "(string xml)",
        "returns": "any",
        "signature": "FromXml(string xml)",
        "returnType": "any",
        "category": "Xml Functions",
        "frequency": "Moderate",
        "untypedSignature": "FromXml(xml)"
    },
    {
        "name": "LKill",
        "description": "Kills a variable or session.",
        "params": "(string varName)",
        "returns": "string",
        "signature": "LKill(string varName)",
        "returnType": "string",
        "category": "System Functions",
        "frequency": "Moderate",
        "untypedSignature": "LKill(varName)"
    },
    {
        "name": "GetDataSetWithSchemaFromSelect",
        "description": "Retrieves dataset with schema from SELECT.",
        "params": "(string commandString, string friendlyName, any[] arrayOfValues, any[] arrayOfPrimaryKeys, any[] arrayOfUniqueConstraints)",
        "returns": "string",
        "signature": "GetDataSetWithSchemaFromSelect(string commandString, string friendlyName, any[] arrayOfValues, any[] arrayOfPrimaryKeys, any[] arrayOfUniqueConstraints)",
        "returnType": "string",
        "category": "Database Functions",
        "frequency": "Moderate",
        "untypedSignature": "GetDataSetWithSchemaFromSelect(commandString, friendlyName, arrayOfValues, arrayOfPrimaryKeys, arrayOfUniqueConstraints)"
    },
    {
        "name": "GetFromSession",
        "description": "Retrieves a value from the session.",
        "params": "(string key)",
        "returns": "any",
        "signature": "GetFromSession(string key)",
        "returnType": "any",
        "category": "Web Functions",
        "frequency": "Moderate",
        "untypedSignature": "GetFromSession(key)"
    },
    {
        "name": "Nothing",
        "description": "Checks if a value is null or nothing.",
        "params": "(any val)",
        "returns": "boolean",
        "signature": "Nothing(any val)",
        "returnType": "boolean",
        "category": "System Functions",
        "frequency": "Moderate",
        "untypedSignature": "Nothing(val)"
    },
    {
        "name": "ShowSqlErrors",
        "description": "Toggles display of SQL errors.",
        "params": "(boolean flag)",
        "returns": "boolean",
        "signature": "ShowSqlErrors(boolean flag)",
        "returnType": "boolean",
        "category": "Database Functions",
        "frequency": "Moderate",
        "untypedSignature": "ShowSqlErrors(flag)"
    },
    {
        "name": "alen",
        "description": "Returns the length of an array.",
        "params": "(any[] target)",
        "returns": "number",
        "signature": "alen(any[] target)",
        "returnType": "number",
        "category": "Array Functions",
        "frequency": "Moderate",
        "untypedSignature": "alen(target)"
    },
    {
        "name": "SubmitToBatch",
        "description": "Submits a code block to batch processing.",
        "params": "(string code, any parameters, string mode, string userName, string password)",
        "returns": "string",
        "signature": "SubmitToBatch(string code, any parameters, string mode, string userName, string password)",
        "returnType": "string",
        "category": "Process Functions",
        "frequency": "Moderate",
        "untypedSignature": "SubmitToBatch(code, parameters, mode, userName, password)"
    },
    {
        "name": "Integer",
        "description": "Truncates a decimal to an integer.",
        "params": "(number decimalValue)",
        "returns": "number",
        "signature": "Integer(number decimalValue)",
        "returnType": "number",
        "category": "Numeric Functions",
        "frequency": "Moderate",
        "untypedSignature": "Integer(decimalValue)"
    },
    {
        "name": "DosSupport",
        "description": "Provides DOS-style directory listing.",
        "params": "(string.op_Implicit(\"DIR\")",
        "returns": "void",
        "signature": "DosSupport(string.op_Implicit(\"DIR\"), filePattern, (any)(object)attributes)",
        "returnType": "void",
        "category": "File Functions",
        "frequency": "Moderate",
        "untypedSignature": "DosSupport(SSLString.op_Implicit(\"DIR\"), filePattern, (SSLValue)(object)attributes)"
    },
    {
        "name": "ReadText",
        "description": "Reads text from a file.",
        "params": "(string fileName, number charsToRead, any encoding)",
        "returns": "string",
        "signature": "ReadText(string fileName, number charsToRead, any encoding)",
        "returnType": "string",
        "category": "File Functions",
        "frequency": "Moderate",
        "untypedSignature": "ReadText(fileName, charsToRead, encoding)"
    },
    {
        "name": "ValidateNumeric",
        "description": "Validates if a string represents a number.",
        "params": "(string sNumber)",
        "returns": "boolean",
        "signature": "ValidateNumeric(string sNumber)",
        "returnType": "boolean",
        "category": "Numeric Functions",
        "frequency": "Moderate",
        "untypedSignature": "ValidateNumeric(sNumber)"
    },
    {
        "name": "PrmCount",
        "description": "Returns the number of parameters.",
        "params": "()",
        "returns": "any",
        "signature": "PrmCount()",
        "returnType": "any",
        "category": "Process Functions",
        "frequency": "Moderate",
        "untypedSignature": "PrmCount()"
    },
    {
        "name": "ascanexact",
        "description": "Scans array for exact value match.",
        "params": "(any[] target, any value, number start, number count)",
        "returns": "number",
        "signature": "ascanexact(any[] target, any value, number start, number count)",
        "returnType": "number",
        "category": "Array Functions",
        "frequency": "Moderate",
        "untypedSignature": "ascanexact(target, value, start, count)"
    },
    {
        "name": "Day",
        "description": "Extracts the day from a date.",
        "params": "(Date date)",
        "returns": "number",
        "signature": "Day(Date date)",
        "returnType": "number",
        "category": "Date Functions",
        "frequency": "Moderate",
        "untypedSignature": "Day(date)"
    },
    {
        "name": "Rat",
        "description": "Finds the last occurrence of a substring.",
        "params": "(string subStr, string source)",
        "returns": "number",
        "signature": "Rat(string subStr, string source)",
        "returnType": "number",
        "category": "String Functions",
        "frequency": "Moderate",
        "untypedSignature": "Rat(subStr, source)"
    },
    {
        "name": "Month",
        "description": "Extracts the month from a date.",
        "params": "(Date date)",
        "returns": "number",
        "signature": "Month(Date date)",
        "returnType": "number",
        "category": "Date Functions",
        "frequency": "Moderate",
        "untypedSignature": "Month(date)"
    },
    {
        "name": "Seconds",
        "description": "Returns the current seconds since midnight.",
        "params": "()",
        "returns": "number",
        "signature": "Seconds()",
        "returnType": "number",
        "category": "Date Functions",
        "frequency": "Moderate",
        "untypedSignature": "Seconds()"
    },
    {
        "name": "Second",
        "description": "Extracts the second from a date.",
        "params": "(Date date)",
        "returns": "number",
        "signature": "Second(Date date)",
        "returnType": "number",
        "category": "Date Functions",
        "frequency": "Moderate",
        "untypedSignature": "Second(date)"
    },
    {
        "name": "arraynew",
        "description": "Creates a new multi-dimensional array.",
        "params": "(number dim1, number dim2, number dim3)",
        "returns": "any[]",
        "signature": "arraynew(number dim1, number dim2, number dim3)",
        "returnType": "any[]",
        "category": "Array Functions",
        "frequency": "Moderate",
        "untypedSignature": "arraynew(dim1, dim2, dim3)"
    },
    {
        "name": "GetDataSetEx",
        "description": "Retrieves dataset with extended options.",
        "params": "(string commandString, string friendlyName, any[] arrayOfValues, boolean includeSchema, boolean includeHeader, string tableName, boolean nullAsBlank, any[] invariantDateColumns)",
        "returns": "string",
        "signature": "GetDataSetEx(string commandString, string friendlyName, any[] arrayOfValues, boolean includeSchema, boolean includeHeader, string tableName, boolean nullAsBlank, any[] invariantDateColumns)",
        "returnType": "string",
        "category": "Database Functions",
        "frequency": "Moderate",
        "untypedSignature": "GetDataSetEx(commandString, friendlyName, arrayOfValues, includeSchema, includeHeader, tableName, nullAsBlank, invariantDateColumns)"
    },
    {
        "name": "AddToSession",
        "description": "Adds a value to the session.",
        "params": "(string key, any value)",
        "returns": "any",
        "signature": "AddToSession(string key, any value)",
        "returnType": "any",
        "category": "Web Functions",
        "frequency": "Moderate",
        "untypedSignature": "AddToSession(key, value)"
    },
    {
        "name": "execudf",
        "description": "Executes a user-defined function.",
        "params": "(any[] args, any cacheCode)",
        "returns": "any",
        "signature": "execudf(any[] args, any cacheCode)",
        "returnType": "any",
        "category": "Runtime Functions",
        "frequency": "Moderate",
        "untypedSignature": "execudf(args, cacheCode)"
    },
    {
        "name": "DToS",
        "description": "Converts a date to string.",
        "params": "(Date date)",
        "returns": "string",
        "signature": "DToS(Date date)",
        "returnType": "string",
        "category": "Date Functions",
        "frequency": "Moderate",
        "untypedSignature": "DToS(date)"
    },
    {
        "name": "LTrim",
        "description": "Removes leading whitespace from a string.",
        "params": "(string source)",
        "returns": "string",
        "signature": "LTrim(string source)",
        "returnType": "string",
        "category": "String Functions",
        "frequency": "Moderate",
        "untypedSignature": "LTrim(source)"
    },
    {
        "name": "LSelect",
        "description": "Executes a SELECT query with field list.",
        "params": "(string commandString, any[] fieldList, string friendlyName, any[] arrayOfValues, boolean nullAsBlank, any[] invariantDateColumns)",
        "returns": "any[]",
        "signature": "LSelect(string commandString, any[] fieldList, string friendlyName, any[] arrayOfValues, boolean nullAsBlank, any[] invariantDateColumns)",
        "returnType": "any[]",
        "category": "Database Functions",
        "frequency": "Moderate",
        "untypedSignature": "LSelect(commandString, fieldList, friendlyName, arrayOfValues, nullAsBlank, invariantDateColumns)"
    },
    {
        "name": "GetLastSQLError",
        "description": "Returns the last SQL error.",
        "params": "()",
        "returns": "Error",
        "signature": "GetLastSQLError()",
        "returnType": "Error",
        "category": "Database Functions",
        "frequency": "Moderate",
        "untypedSignature": "GetLastSQLError()"
    },
    {
        "name": "BRANCH",
        "description": "Branching logic (placeholder).",
        "params": "()",
        "returns": "any",
        "frequency": "Moderate"
    },
    {
        "name": "lWait",
        "description": "Waits for a specified number of seconds.",
        "params": "(number seconds)",
        "returns": "string",
        "signature": "lWait(number seconds)",
        "returnType": "string",
        "category": "Process Functions",
        "frequency": "Moderate",
        "untypedSignature": "lWait(seconds)"
    },
    {
        "name": "GetDataSetFromArray",
        "description": "Converts array to dataset format.",
        "params": "(any[] arrayOfValues, any[] arrayFields)",
        "returns": "string",
        "signature": "GetDataSetFromArray(any[] arrayOfValues, any[] arrayFields)",
        "returnType": "string",
        "category": "Database Functions",
        "frequency": "Moderate",
        "untypedSignature": "GetDataSetFromArray(arrayOfValues, arrayFields)"
    },
    {
        "name": "DateAdd",
        "description": "Adds a time interval to a date.",
        "params": "(any date, any number, any datepart)",
        "returns": "Date",
        "signature": "DateAdd(any date, any number, any datepart)",
        "returnType": "Date",
        "category": "Date Functions",
        "frequency": "Moderate",
        "untypedSignature": "DateAdd(date, number, datepart)"
    },
    {
        "name": "FormatErrorMessage",
        "description": "Formats an error message.",
        "params": "(v)",
        "returns": "void",
        "signature": "FormatErrorMessage(v)",
        "returnType": "void",
        "category": "Error Functions",
        "frequency": "Moderate",
        "untypedSignature": "FormatErrorMessage(v)"
    },
    {
        "name": "AddProperty",
        "description": "Adds a property to an object.",
        "params": "(object o, any propName)",
        "returns": "any",
        "signature": "AddProperty(object o, any propName)",
        "returnType": "any",
        "category": "Udo Functions",
        "frequency": "Moderate",
        "untypedSignature": "AddProperty(o, propName)"
    },
    {
        "name": "DateDiff",
        "description": "Calculates the difference between two dates.",
        "params": "(any startDate, any endDate, any datepart)",
        "returns": "number",
        "signature": "DateDiff(any startDate, any endDate, any datepart)",
        "returnType": "number",
        "category": "Date Functions",
        "frequency": "Moderate",
        "untypedSignature": "DateDiff(startDate, endDate, datepart)"
    },
    {
        "name": "GetDataSetXMLFromSelect",
        "description": "Retrieves XML dataset from SELECT.",
        "params": "(commandString, friendlyName, new boolean(true)",
        "returns": "void",
        "signature": "GetDataSetXMLFromSelect(commandString, friendlyName, new boolean(true), arrayOfValues, new boolean(true), null, null, null)",
        "returnType": "void",
        "category": "Database Functions",
        "frequency": "Moderate",
        "untypedSignature": "GetDataSetXMLFromSelect(commandString, friendlyName, new SSLBool(true), arrayOfValues, new SSLBool(true), null, null, null)"
    },
    {
        "name": "GetDataSetFromArrayEx",
        "description": "Extended array to dataset conversion.",
        "params": "(any[] arrayOfValues, any[] arrayFields, string tableName, boolean includeHeader, boolean includeSchema)",
        "returns": "string",
        "signature": "GetDataSetFromArrayEx(any[] arrayOfValues, any[] arrayFields, string tableName, boolean includeHeader, boolean includeSchema)",
        "returnType": "string",
        "category": "Database Functions",
        "frequency": "Moderate",
        "untypedSignature": "GetDataSetFromArrayEx(arrayOfValues, arrayFields, tableName, includeHeader, includeSchema)"
    },
    {
        "name": "DecryptData",
        "description": "Decrypts data using a password.",
        "params": "(string inputData, string password)",
        "returns": "string",
        "signature": "DecryptData(string inputData, string password)",
        "returnType": "string",
        "category": "Security Functions",
        "frequency": "Moderate",
        "untypedSignature": "DecryptData(inputData, password)"
    },
    {
        "name": "Round",
        "description": "Rounds a number to specified digits.",
        "params": "(any value, any digits, any midPointRounding)",
        "returns": "any",
        "signature": "Round(any value, any digits, any midPointRounding)",
        "returnType": "any",
        "category": "Numeric Functions",
        "frequency": "Moderate",
        "untypedSignature": "Round(value, digits, midPointRounding)"
    },
    {
        "name": "IsNumeric",
        "description": "Checks if a value is numeric.",
        "params": "(any sNumber, any allowHex)",
        "returns": "boolean",
        "signature": "IsNumeric(any sNumber, any allowHex)",
        "returnType": "boolean",
        "category": "Numeric Functions",
        "frequency": "Low",
        "untypedSignature": "IsNumeric(sNumber, allowHex)"
    },
    {
        "name": "Abs",
        "description": "Returns the absolute value of a number.",
        "params": "(number numericValue)",
        "returns": "number",
        "signature": "Abs(number numericValue)",
        "returnType": "number",
        "category": "Numeric Functions",
        "frequency": "Low",
        "untypedSignature": "Abs(numericValue)"
    },
    {
        "name": "Hour",
        "description": "Extracts the hour from a date.",
        "params": "(Date date)",
        "returns": "number",
        "signature": "Hour(Date date)",
        "returnType": "number",
        "category": "Date Functions",
        "frequency": "Low",
        "untypedSignature": "Hour(date)"
    },
    {
        "name": "IgnoreSqlErrors",
        "description": "Toggles ignoring of SQL errors.",
        "params": "(boolean flag)",
        "returns": "boolean",
        "signature": "IgnoreSqlErrors(boolean flag)",
        "returnType": "boolean",
        "category": "Database Functions",
        "frequency": "Low",
        "untypedSignature": "IgnoreSqlErrors(flag)"
    },
    {
        "name": "Replicate",
        "description": "Repeats a string a specified number of times.",
        "params": "(string source, number count)",
        "returns": "string",
        "signature": "Replicate(string source, number count)",
        "returnType": "string",
        "category": "String Functions",
        "frequency": "Low",
        "untypedSignature": "Replicate(source, count)"
    },
    {
        "name": "LSelectC",
        "description": "Executes SELECT with caching.",
        "params": "(string commandString, any[] fieldList, string friendlyName, any[] arrayOfValues, boolean nullAsBlank, any[] invariantDateColumns)",
        "returns": "any[]",
        "signature": "LSelectC(string commandString, any[] fieldList, string friendlyName, any[] arrayOfValues, boolean nullAsBlank, any[] invariantDateColumns)",
        "returnType": "any[]",
        "category": "Database Functions",
        "frequency": "Low",
        "untypedSignature": "LSelectC(commandString, fieldList, friendlyName, arrayOfValues, nullAsBlank, invariantDateColumns)"
    },
    {
        "name": "DateToString",
        "description": "Converts date to string with format.",
        "params": "(any date, any format)",
        "returns": "string",
        "signature": "DateToString(any date, any format)",
        "returnType": "string",
        "category": "Date Functions",
        "frequency": "Low",
        "untypedSignature": "DateToString(date, format)"
    },
    {
        "name": "RunDS",
        "description": "Runs a data source query.",
        "params": "(any dataSourceName, any parameters, any returnType)",
        "returns": "any",
        "signature": "RunDS(any dataSourceName, any parameters, any returnType)",
        "returnType": "any",
        "category": "Database Functions",
        "frequency": "Low",
        "untypedSignature": "RunDS(dataSourceName, parameters, returnType)"
    },
    {
        "name": "Break",
        "description": "Breaks execution flow.",
        "params": "()",
        "returns": "any",
        "signature": "Break()",
        "returnType": "any",
        "category": "System Functions",
        "frequency": "Low",
        "untypedSignature": "Break()"
    },
    {
        "name": "Minute",
        "description": "Extracts the minute from a date.",
        "params": "(Date date)",
        "returns": "number",
        "signature": "Minute(Date date)",
        "returnType": "number",
        "category": "Date Functions",
        "frequency": "Low",
        "untypedSignature": "Minute(date)"
    },
    {
        "name": "LimsAt",
        "description": "Finds substring position with offset.",
        "params": "(string subString, string source, number offset)",
        "returns": "number",
        "signature": "LimsAt(string subString, string source, number offset)",
        "returnType": "number",
        "category": "String Functions",
        "frequency": "Low",
        "untypedSignature": "LimsAt(subString, source, offset)"
    },
    {
        "name": "MatFunc",
        "description": "Performs mathematical functions.",
        "params": "(string functionName, number number)",
        "returns": "number",
        "signature": "MatFunc(string functionName, number number)",
        "returnType": "number",
        "category": "Numeric Functions",
        "frequency": "Low",
        "untypedSignature": "MatFunc(functionName, number)"
    },
    {
        "name": "aeval",
        "description": "Evaluates a code block for each array element.",
        "params": "(any[] target, Function codeBlock, number start, number count)",
        "returns": "any",
        "signature": "aeval(any[] target, Function codeBlock, number start, number count)",
        "returnType": "any",
        "category": "Array Functions",
        "frequency": "Low",
        "untypedSignature": "aeval(target, codeBlock, start, count)"
    },
    {
        "name": "buildstring2",
        "description": "Builds string from array with line and column delimiters.",
        "params": "(any[] target, string lineDelimiter, string colDelimiter)",
        "returns": "string",
        "signature": "buildstring2(any[] target, string lineDelimiter, string colDelimiter)",
        "returnType": "string",
        "category": "Array Functions",
        "frequency": "Low",
        "untypedSignature": "buildstring2(target, lineDelimiter, colDelimiter)"
    },
    {
        "name": "LimsNETCast",
        "description": "Casts a value to a .NET type.",
        "params": "(any val, string newType)",
        "returns": "any",
        "signature": "LimsNETCast(any val, string newType)",
        "returnType": "any",
        "category": "Datatype Functions",
        "frequency": "Low",
        "untypedSignature": "LimsNETCast(val, newType)"
    },
    {
        "name": "MimeDecode",
        "description": "Decodes MIME-encoded data.",
        "params": "(any v)",
        "returns": "any",
        "signature": "MimeDecode(any v)",
        "returnType": "any",
        "category": "String Functions",
        "frequency": "Low",
        "untypedSignature": "MimeDecode(v)"
    },
    {
        "name": "LCase",
        "description": "Conditional lowercase conversion.",
        "params": "(boolean condition, string trueValue, string falseValue)",
        "returns": "any",
        "signature": "LCase(boolean condition, string trueValue, string falseValue)",
        "returnType": "any",
        "category": "System Functions",
        "frequency": "Low",
        "untypedSignature": "LCase(condition, trueValue, falseValue)"
    },
    {
        "name": "LimsExec",
        "description": "Executes an external application.",
        "params": "(string application, boolean show, string arguments)",
        "returns": "boolean",
        "signature": "LimsExec(string application, boolean show, string arguments)",
        "returnType": "boolean",
        "category": "Process Functions",
        "frequency": "Low",
        "untypedSignature": "LimsExec(application, show, arguments)"
    },
    {
        "name": "StationName",
        "description": "Returns the workstation name.",
        "params": "()",
        "returns": "any",
        "signature": "StationName()",
        "returnType": "any",
        "category": "Builtin Functions",
        "frequency": "Low",
        "untypedSignature": "StationName()"
    },
    {
        "name": "StdRound",
        "description": "Rounds to standard significant figures.",
        "params": "(string standard, number nrDigits, number number)",
        "returns": "string",
        "signature": "StdRound(string standard, number nrDigits, number number)",
        "returnType": "string",
        "category": "Numeric Functions",
        "frequency": "Low",
        "untypedSignature": "StdRound(standard, nrDigits, number)"
    },
    {
        "name": "ToJson",
        "description": "Converts a value to JSON string.",
        "params": "(any value)",
        "returns": "any",
        "signature": "ToJson(any value)",
        "returnType": "any",
        "category": "Web Functions",
        "frequency": "Low",
        "untypedSignature": "ToJson(value)"
    },
    {
        "name": "delarray",
        "description": "Deletes an element from an array.",
        "params": "(any[] target, number index)",
        "returns": "any[]",
        "signature": "delarray(any[] target, number index)",
        "returnType": "any[]",
        "category": "Array Functions",
        "frequency": "Low",
        "untypedSignature": "delarray(target, index)"
    },
    {
        "name": "SigFig",
        "description": "Formats to significant figures.",
        "params": "(string standard, number nrDigits, number number)",
        "returns": "string",
        "signature": "SigFig(string standard, number nrDigits, number number)",
        "returnType": "string",
        "category": "Numeric Functions",
        "frequency": "Low",
        "untypedSignature": "SigFig(standard, nrDigits, number)"
    },
    {
        "name": "CToD",
        "description": "Converts string to date.",
        "params": "(string dateString)",
        "returns": "Date",
        "signature": "CToD(string dateString)",
        "returnType": "Date",
        "category": "Date Functions",
        "frequency": "Low",
        "untypedSignature": "CToD(dateString)"
    },
    {
        "name": "buildarray2",
        "description": "Builds array from string with advanced delimiters.",
        "params": "(string text, string lineDelimiter, string colDelimiter, boolean crlfOk, boolean trimSpaces)",
        "returns": "any[]",
        "signature": "buildarray2(string text, string lineDelimiter, string colDelimiter, boolean crlfOk, boolean trimSpaces)",
        "returnType": "any[]",
        "category": "Array Functions",
        "frequency": "Low",
        "untypedSignature": "buildarray2(text, lineDelimiter, colDelimiter, crlfOk, trimSpaces)"
    },
    {
        "name": "GetInternal",
        "description": "Retrieves internal property of an object.",
        "params": "(any o, string propName)",
        "returns": "any",
        "signature": "GetInternal(any o, string propName)",
        "returnType": "any",
        "category": "Udo Functions",
        "frequency": "Low",
        "untypedSignature": "GetInternal(o, propName)"
    },
    {
        "name": "LimsCleanup",
        "description": "Performs cleanup operations.",
        "params": "()",
        "returns": "any",
        "signature": "LimsCleanup()",
        "returnType": "any",
        "category": "Builtin Functions",
        "frequency": "Low",
        "untypedSignature": "LimsCleanup()"
    },
    {
        "name": "StrSrch",
        "description": "Searches for substring with options.",
        "params": "(string subStr, string source, number indexOrOccurence, boolean flag)",
        "returns": "number",
        "signature": "StrSrch(string subStr, string source, number indexOrOccurence, boolean flag)",
        "returnType": "number",
        "category": "String Functions",
        "frequency": "Low",
        "untypedSignature": "StrSrch(subStr, source, indexOrOccurence, flag)"
    },
    {
        "name": "DocGetErrorMessage",
        "description": "Retrieves Documentum error message.",
        "params": "()",
        "returns": "string",
        "signature": "DocGetErrorMessage()",
        "returnType": "string",
        "category": "Documentum Functions",
        "frequency": "Low",
        "untypedSignature": "DocGetErrorMessage()"
    },
    {
        "name": "IsDefined",
        "description": "Checks if a variable is defined.",
        "params": "(any varName)",
        "returns": "any",
        "signature": "IsDefined(any varName)",
        "returnType": "any",
        "category": "Datatype Functions",
        "frequency": "Low",
        "untypedSignature": "IsDefined(varName)"
    },
    {
        "name": "UpdLong",
        "description": "Updates long data in database.",
        "params": "(string friendlyName, string tableName, string columnName, string whereCondition, string inputFilePath, boolean isCompressed)",
        "returns": "boolean",
        "signature": "UpdLong(string friendlyName, string tableName, string columnName, string whereCondition, string inputFilePath, boolean isCompressed)",
        "returnType": "boolean",
        "category": "Database Functions",
        "frequency": "Low",
        "untypedSignature": "UpdLong(friendlyName, tableName, columnName, whereCondition, inputFilePath, isCompressed)"
    },
    {
        "name": "EncryptData",
        "description": "Encrypts data with password and algorithm.",
        "params": "(string inputData, string password, string algorithm, string key, string retType)",
        "returns": "string",
        "signature": "EncryptData(string inputData, string password, string algorithm, string key, string retType)",
        "returnType": "string",
        "category": "Security Functions",
        "frequency": "Low",
        "untypedSignature": "EncryptData(inputData, password, algorithm, key, retType)"
    },
    {
        "name": "IsGuid",
        "description": "Checks if a string is a valid GUID.",
        "params": "(string guid)",
        "returns": "boolean",
        "signature": "IsGuid(string guid)",
        "returnType": "boolean",
        "category": "System Functions",
        "frequency": "Low",
        "untypedSignature": "IsGuid(guid)"
    },
    {
        "name": "SetInternal",
        "description": "Sets internal property of an object.",
        "params": "(any o, string propName, any propValue)",
        "returns": "any",
        "signature": "SetInternal(any o, string propName, any propValue)",
        "returnType": "any",
        "category": "Udo Functions",
        "frequency": "Low",
        "untypedSignature": "SetInternal(o, propName, propValue)"
    },
    {
        "name": "HashData",
        "description": "Hashes data with specified algorithm.",
        "params": "(string inputData, string algorithm)",
        "returns": "string",
        "signature": "HashData(string inputData, string algorithm)",
        "returnType": "string",
        "category": "Security Functions",
        "frequency": "Low",
        "untypedSignature": "HashData(inputData, algorithm)"
    },
    {
        "name": "SendLimsEmail",
        "description": "Sends an email via LIMS.",
        "params": "(string SMTP, any[] recipients, string fromWho, string subject, string messageBody, any[] attachList, any[] cClist, any[] bCClist, string replyTo, number nPort, string uName, string uPass, boolean ignoreErrors, boolean useCDO, number timeout, boolean useSSL, boolean isBodyHTML, string encryptedData)",
        "returns": "boolean",
        "signature": "SendLimsEmail(string SMTP, any[] recipients, string fromWho, string subject, string messageBody, any[] attachList, any[] cClist, any[] bCClist, string replyTo, number nPort, string uName, string uPass, boolean ignoreErrors, boolean useCDO, number timeout, boolean useSSL, boolean isBodyHTML, string encryptedData)",
        "returnType": "boolean",
        "category": "Email Functions",
        "frequency": "Low",
        "untypedSignature": "SendLimsEmail(SMTP, recipients, fromWho, subject, messageBody, attachList, cClist, bCClist, replyTo, nPort, uName, uPass, ignoreErrors, useCDO, timeout, useSSL, isBodyHTML, encryptedData)"
    },
    {
        "name": "ChkPassword",
        "description": "Checks user password.",
        "params": "(string userName, string password)",
        "returns": "boolean",
        "signature": "ChkPassword(string userName, string password)",
        "returnType": "boolean",
        "category": "Security Functions",
        "frequency": "Low",
        "untypedSignature": "ChkPassword(userName, password)"
    },
    {
        "name": "DateFormat",
        "description": "Sets the date format.",
        "params": "(string newFormat)",
        "returns": "string",
        "signature": "DateFormat(string newFormat)",
        "returnType": "string",
        "category": "Date Functions",
        "frequency": "Low",
        "untypedSignature": "DateFormat(newFormat)"
    },
    {
        "name": "lDir",
        "description": "Lists directory contents.",
        "params": "(string filePattern, string attributes)",
        "returns": "any[]",
        "signature": "lDir(string filePattern, string attributes)",
        "returnType": "any[]",
        "category": "File Functions",
        "frequency": "Low",
        "untypedSignature": "lDir(filePattern, attributes)"
    },
    {
        "name": "StringToDate",
        "description": "Converts string to date with format.",
        "params": "(string dateString, string dateFormat)",
        "returns": "Date",
        "signature": "StringToDate(string dateString, string dateFormat)",
        "returnType": "Date",
        "category": "Date Functions",
        "frequency": "Low",
        "untypedSignature": "StringToDate(dateString, dateFormat)"
    },
    {
        "name": "GetConnectionByName",
        "description": "Retrieves database connection by name.",
        "params": "(string friendlyName)",
        "returns": "any",
        "signature": "GetConnectionByName(string friendlyName)",
        "returnType": "any",
        "category": "Database Functions",
        "frequency": "Low",
        "untypedSignature": "GetConnectionByName(friendlyName)"
    },
    {
        "name": "DateFromString",
        "description": "Parses date from string with options.",
        "params": "(any dateAsString, any format, any useLocalCulture, any makeInvariant)",
        "returns": "any",
        "signature": "DateFromString(any dateAsString, any format, any useLocalCulture, any makeInvariant)",
        "returnType": "any",
        "category": "Date Functions",
        "frequency": "Low",
        "untypedSignature": "DateFromString(dateAsString, format, useLocalCulture, makeInvariant)"
    },
    {
        "name": "ErrorMes",
        "description": "Displays an error message.",
        "params": "(any a, any b)",
        "returns": "any",
        "signature": "ErrorMes(any a, any b)",
        "returnType": "any",
        "category": "System Functions",
        "frequency": "Low",
        "untypedSignature": "ErrorMes(a, b)"
    },
    {
        "name": "TraceOn",
        "description": "Enables tracing.",
        "params": "()",
        "returns": "any",
        "signature": "TraceOn()",
        "returnType": "any",
        "category": "Process Functions",
        "frequency": "Low",
        "untypedSignature": "TraceOn()"
    },
    {
        "name": "RetrieveLong",
        "description": "Retrieves long data from database.",
        "params": "(string friendlyName, string tableName, string columnName, string whereCondition, string outputFilePath, boolean isCompressed)",
        "returns": "boolean",
        "signature": "RetrieveLong(string friendlyName, string tableName, string columnName, string whereCondition, string outputFilePath, boolean isCompressed)",
        "returnType": "boolean",
        "category": "Database Functions",
        "frequency": "Low",
        "untypedSignature": "RetrieveLong(friendlyName, tableName, columnName, whereCondition, outputFilePath, isCompressed)"
    },
    {
        "name": "HasProperty",
        "description": "Checks if an object has a property.",
        "params": "(any o, string propName)",
        "returns": "boolean",
        "signature": "HasProperty(any o, string propName)",
        "returnType": "boolean",
        "category": "Udo Functions",
        "frequency": "Low",
        "untypedSignature": "HasProperty(o, propName)"
    },
    {
        "name": "SendToOutbox",
        "description": "Sends email to outbox.",
        "params": "(string SMTP, any[] recipients, string fromWho, string subject, string messageBody, any[] attachList, any[] cClist, any[] bCClist, string replyTo, number nPort, string uName, string uPass, boolean ignoreErrors, boolean useSSL, boolean isBodyHTML, string encryptedData)",
        "returns": "boolean",
        "signature": "SendToOutbox(string SMTP, any[] recipients, string fromWho, string subject, string messageBody, any[] attachList, any[] cClist, any[] bCClist, string replyTo, number nPort, string uName, string uPass, boolean ignoreErrors, boolean useSSL, boolean isBodyHTML, string encryptedData)",
        "returnType": "boolean",
        "category": "Email Functions",
        "frequency": "Low",
        "untypedSignature": "SendToOutbox(SMTP, recipients, fromWho, subject, messageBody, attachList, cClist, bCClist, replyTo, nPort, uName, uPass, ignoreErrors, useSSL, isBodyHTML, encryptedData)"
    },
    {
        "name": "GetByName",
        "description": "Retrieves value by name.",
        "params": "(string name)",
        "returns": "any",
        "signature": "GetByName(string name)",
        "returnType": "any",
        "category": "System Functions",
        "frequency": "Low",
        "untypedSignature": "GetByName(name)"
    },
    {
        "name": "GetSettings",
        "description": "Retrieves multiple settings.",
        "params": "(any[] names)",
        "returns": "any[]",
        "signature": "GetSettings(any[] names)",
        "returnType": "any[]",
        "category": "System Functions",
        "frequency": "Low",
        "untypedSignature": "GetSettings(names)"
    },
    {
        "name": "SortArray",
        "description": "Sorts an array.",
        "params": "(any[] target, any numeric)",
        "returns": "any[]",
        "signature": "SortArray(any[] target, any numeric)",
        "returnType": "any[]",
        "category": "Array Functions",
        "frequency": "Low",
        "untypedSignature": "SortArray(target, numeric)"
    },
    {
        "name": "DocCommandFailed",
        "description": "Checks if Documentum command failed.",
        "params": "()",
        "returns": "boolean",
        "signature": "DocCommandFailed()",
        "returnType": "boolean",
        "category": "Documentum Functions",
        "frequency": "Low",
        "untypedSignature": "DocCommandFailed()"
    },
    {
        "name": "CMonth",
        "description": "Returns month name from date.",
        "params": "(Date date)",
        "returns": "string",
        "signature": "CMonth(Date date)",
        "returnType": "string",
        "category": "Date Functions",
        "frequency": "Low",
        "untypedSignature": "CMonth(date)"
    },
    {
        "name": "GetLogsFolder",
        "description": "Returns the logs folder path.",
        "params": "()",
        "returns": "string",
        "signature": "GetLogsFolder()",
        "returnType": "string",
        "category": "System Functions",
        "frequency": "Low",
        "untypedSignature": "GetLogsFolder()"
    },
    {
        "name": "LPrint",
        "description": "Prints to LIMS output.",
        "params": "(string source)",
        "returns": "any",
        "signature": "LPrint(string source)",
        "returnType": "any",
        "category": "Reporting Functions",
        "frequency": "Low",
        "untypedSignature": "LPrint(source)"
    },
    {
        "name": "DToC",
        "description": "Converts date to string.",
        "params": "(Date date)",
        "returns": "string",
        "signature": "DToC(Date date)",
        "returnType": "string",
        "category": "Date Functions",
        "frequency": "Low",
        "untypedSignature": "DToC(date)"
    },
    {
        "name": "ServerStartOfDay",
        "description": "Returns server start of day.",
        "params": "(date)",
        "returns": "void",
        "signature": "ServerStartOfDay(date)",
        "returnType": "void",
        "category": "Date Functions",
        "frequency": "Low",
        "untypedSignature": "ServerStartOfDay(date)"
    },
    {
        "name": "GetUserData",
        "description": "Retrieves user data.",
        "params": "()",
        "returns": "string",
        "signature": "GetUserData()",
        "returnType": "string",
        "category": "Security Functions",
        "frequency": "Low",
        "untypedSignature": "GetUserData()"
    },
    {
        "name": "FormatSqlErrorMessage",
        "description": "Formats SQL error message.",
        "params": "(any v)",
        "returns": "any",
        "signature": "FormatSqlErrorMessage(any v)",
        "returnType": "any",
        "category": "Error Functions",
        "frequency": "Low",
        "untypedSignature": "FormatSqlErrorMessage(v)"
    },
    {
        "name": "Rand",
        "description": "Generates a random number.",
        "params": "(number seed)",
        "returns": "number",
        "signature": "Rand(number seed)",
        "returnType": "number",
        "category": "Numeric Functions",
        "frequency": "Low",
        "untypedSignature": "Rand(seed)"
    },
    {
        "name": "ClientStartOfDay",
        "description": "Returns client start of day.",
        "params": "(any date)",
        "returns": "any",
        "signature": "ClientStartOfDay(any date)",
        "returnType": "any",
        "category": "Date Functions",
        "frequency": "Low",
        "untypedSignature": "ClientStartOfDay(date)"
    },
    {
        "name": "eval",
        "description": "Evaluates an expression.",
        "params": "(params any[] args)",
        "returns": "any",
        "signature": "eval(params any[] args)",
        "returnType": "any",
        "frequency": "Low",
        "untypedSignature": "eval(args)"
    },
    {
        "name": "ResetApplication",
        "description": "Resets the application state.",
        "params": "()",
        "returns": "any",
        "signature": "ResetApplication()",
        "returnType": "any",
        "category": "System Functions",
        "frequency": "Low",
        "untypedSignature": "ResetApplication()"
    },
    {
        "name": "DetectSqlInjections",
        "description": "Toggles SQL injection detection.",
        "params": "(any onOff, any connectionName)",
        "returns": "any",
        "signature": "DetectSqlInjections(any onOff, any connectionName)",
        "returnType": "any",
        "category": "Database Functions",
        "frequency": "Low",
        "untypedSignature": "DetectSqlInjections(onOff, connectionName)"
    },
    {
        "name": "ClientEndOfDay",
        "description": "Returns client end of day.",
        "params": "(any date)",
        "returns": "any",
        "signature": "ClientEndOfDay(any date)",
        "returnType": "any",
        "category": "Date Functions",
        "frequency": "Low",
        "untypedSignature": "ClientEndOfDay(date)"
    },
    {
        "name": "GetInternalC",
        "description": "Retrieves internal collection property.",
        "params": "(any o, string collectionName, any arg1, any arg2, any arg3, any arg4, any arg5, any arg6)",
        "returns": "any",
        "signature": "GetInternalC(any o, string collectionName, any arg1, any arg2, any arg3, any arg4, any arg5, any arg6)",
        "returnType": "any",
        "category": "Udo Functions",
        "frequency": "Low",
        "untypedSignature": "GetInternalC(o, collectionName, arg1, arg2, arg3, arg4, arg5, arg6)"
    },
    {
        "name": "IsProductionModeOn",
        "description": "Checks if production mode is on.",
        "params": "()",
        "returns": "any",
        "signature": "IsProductionModeOn()",
        "returnType": "any",
        "category": "System Functions",
        "frequency": "Low",
        "untypedSignature": "IsProductionModeOn()"
    },
    {
        "name": "limsoleconnect",
        "description": "Connects to LIMS OLE.",
        "params": "(any v)",
        "returns": "any",
        "signature": "limsoleconnect(any v)",
        "returnType": "any",
        "category": "Builtin Functions",
        "frequency": "Low",
        "untypedSignature": "limsoleconnect(v)"
    },
    {
        "name": "RunApp",
        "description": "Runs an external application.",
        "params": "(string application, string arguments)",
        "returns": "boolean",
        "signature": "RunApp(string application, string arguments)",
        "returnType": "boolean",
        "category": "Process Functions",
        "frequency": "Low",
        "untypedSignature": "RunApp(application, arguments)"
    },
    {
        "name": "Sqrt",
        "description": "Returns the square root of a number.",
        "params": "(number number)",
        "returns": "number",
        "signature": "Sqrt(number number)",
        "returnType": "number",
        "category": "Numeric Functions",
        "frequency": "Low",
        "untypedSignature": "Sqrt(number)"
    },
    {
        "name": "DateFromNumbers",
        "description": "Creates date from numeric components.",
        "params": "(any year, any month, any day, any hour, any minute, any second, any millisecond, any makeInvariant)",
        "returns": "Date",
        "signature": "DateFromNumbers(any year, any month, any day, any hour, any minute, any second, any millisecond, any makeInvariant)",
        "returnType": "Date",
        "category": "Date Functions",
        "frequency": "Low",
        "untypedSignature": "DateFromNumbers(year, month, day, hour, minute, second, millisecond, makeInvariant)"
    },
    {
        "name": "FromJson",
        "description": "Converts JSON string to value.",
        "params": "(any value)",
        "returns": "any",
        "signature": "FromJson(any value)",
        "returnType": "any",
        "category": "Web Functions",
        "frequency": "Low",
        "untypedSignature": "FromJson(value)"
    },
    {
        "name": "getinlinecode",
        "description": "Retrieves inline code.",
        "params": "(any s, any[] variables)",
        "returns": "string",
        "signature": "getinlinecode(any s, any[] variables)",
        "returnType": "string",
        "category": "Builtin Functions",
        "frequency": "Low",
        "untypedSignature": "getinlinecode(s, variables)"
    },
    {
        "name": "JDay",
        "description": "Returns Julian day number.",
        "params": "(any date)",
        "returns": "number",
        "signature": "JDay(any date)",
        "returnType": "number",
        "category": "Date Functions",
        "frequency": "Low",
        "untypedSignature": "JDay(date)"
    },
    {
        "name": "SetSqlTimeout",
        "description": "Sets SQL timeout.",
        "params": "(any timeout, any connection)",
        "returns": "any",
        "signature": "SetSqlTimeout(any timeout, any connection)",
        "returnType": "any",
        "category": "Database Functions",
        "frequency": "Low",
        "untypedSignature": "SetSqlTimeout(timeout, connection)"
    },
    {
        "name": "endlimsoleconnect",
        "description": "Ends LIMS OLE connection.",
        "params": "(any v)",
        "returns": "any",
        "signature": "endlimsoleconnect(any v)",
        "returnType": "any",
        "category": "Builtin Functions",
        "frequency": "Low",
        "untypedSignature": "endlimsoleconnect(v)"
    },
    {
        "name": "HtmlEncode",
        "description": "Encodes string for HTML.",
        "params": "(string data)",
        "returns": "string",
        "signature": "HtmlEncode(string data)",
        "returnType": "string",
        "category": "Xml Functions",
        "frequency": "Low",
        "untypedSignature": "HtmlEncode(data)"
    },
    {
        "name": "InfoMes",
        "description": "Displays an info message.",
        "params": "(any a, any b)",
        "returns": "any",
        "signature": "InfoMes(any a, any b)",
        "returnType": "any",
        "category": "System Functions",
        "frequency": "Low",
        "untypedSignature": "InfoMes(a, b)"
    },
    {
        "name": "SetAmPm",
        "description": "Sets AM/PM display.",
        "params": "(boolean flag)",
        "returns": "boolean",
        "signature": "SetAmPm(boolean flag)",
        "returnType": "boolean",
        "category": "Date Functions",
        "frequency": "Low",
        "untypedSignature": "SetAmPm(flag)"
    },
    {
        "name": "WriteBytesBase64",
        "description": "Writes base64 data to file.",
        "params": "(string fileName, string base64Data)",
        "returns": "string",
        "signature": "WriteBytesBase64(string fileName, string base64Data)",
        "returnType": "string",
        "category": "File Functions",
        "frequency": "Low",
        "untypedSignature": "WriteBytesBase64(fileName, base64Data)"
    },
    {
        "name": "ArrayToTVP",
        "description": "Converts array to table-valued parameter.",
        "params": "(any values, any dataType, any connectionName)",
        "returns": "any",
        "signature": "ArrayToTVP(any values, any dataType, any connectionName)",
        "returnType": "any",
        "category": "Array Functions",
        "frequency": "Low",
        "untypedSignature": "ArrayToTVP(values, dataType, connectionName)"
    },
    {
        "name": "ReadBytesBase64",
        "description": "Reads file as base64 string.",
        "params": "(string fileName)",
        "returns": "string",
        "signature": "ReadBytesBase64(string fileName)",
        "returnType": "string",
        "category": "File Functions",
        "frequency": "Low",
        "untypedSignature": "ReadBytesBase64(fileName)"
    },
    {
        "name": "CreateZip",
        "description": "Creates a ZIP archive.",
        "params": "(string zipFileName, string sourceDirectory, boolean recurse, string fileFilter, string password)",
        "returns": "any",
        "signature": "CreateZip(string zipFileName, string sourceDirectory, boolean recurse, string fileFilter, string password)",
        "returnType": "any",
        "category": "System Functions",
        "frequency": "Low",
        "untypedSignature": "CreateZip(zipFileName, sourceDirectory, recurse, fileFilter, password)"
    },
    {
        "name": "DocSearchUsingDql",
        "description": "Searches Documentum using DQL.",
        "params": "(string dql, number resultSetSize)",
        "returns": "any[]",
        "signature": "DocSearchUsingDql(string dql, number resultSetSize)",
        "returnType": "any[]",
        "category": "Documentum Functions",
        "frequency": "Low",
        "untypedSignature": "DocSearchUsingDql(dql, resultSetSize)"
    },
    {
        "name": "DOW",
        "description": "Returns day of week.",
        "params": "(Date date)",
        "returns": "number",
        "signature": "DOW(Date date)",
        "returnType": "number",
        "category": "Date Functions",
        "frequency": "Low",
        "untypedSignature": "DOW(date)"
    },
    {
        "name": "ExtractZip",
        "description": "Extracts a ZIP archive.",
        "params": "(string zipFileName, string targetDirectory, string fileFilter, string password)",
        "returns": "any",
        "signature": "ExtractZip(string zipFileName, string targetDirectory, string fileFilter, string password)",
        "returnType": "any",
        "category": "System Functions",
        "frequency": "Low",
        "untypedSignature": "ExtractZip(zipFileName, targetDirectory, fileFilter, password)"
    }
];

export interface SSLClass {
    name: string;
    description: string;
    instantiation: string;
    usage: string;
    methods: string[];
    properties: string[];
}

export const SSL_BUILTIN_CLASSES: SSLClass[] = [
    {
        name: "Email",
        description: "Built-in email class for sending emails",
        instantiation: "Email{}",
        usage: "oEmail := Email{}; oEmail:Subject := 'Test'; oEmail:Send();",
        methods: ["Send", "AddAttachment", "SetRecipient"],
        properties: ["Subject", "Body", "From", "To"]
    },
    {
        name: "SSLRegex",
        description: "Built-in regular expression class for pattern matching",
        instantiation: "SSLRegex{}",
        usage: "oRegex := SSLRegex{}; result := oRegex:Match(pattern, text);",
        methods: ["Match", "Replace", "Test"],
        properties: ["Pattern", "IgnoreCase", "Multiline"]
    }
];

export const SSL_KEYWORD_DESCRIPTIONS: Record<string, string> = {
    "IF": "Conditional statement - executes code block if condition is true",
    "ELSE": "Alternative code path when IF condition is false (use nested :IF for else-if logic)",
    "ENDIF": "Marks the end of an IF conditional block",
    "WHILE": "Loop that executes while condition is true",
    "ENDWHILE": "Marks the end of a WHILE loop",
    "FOR": "Loop with counter variable",
    "TO": "Specifies the upper bound of a FOR loop",
    "STEP": "Specifies the increment for a FOR loop",
    "NEXT": "Marks the end of a FOR loop",
    "BEGINCASE": "Start of a CASE statement for multiple conditions",
    "CASE": "Individual condition in a CASE statement",
    "OTHERWISE": "Default case when no other CASE conditions match",
    "ENDCASE": "Marks the end of a CASE statement",
    "TRY": "Begin error handling block",
    "CATCH": "Handle errors from TRY block",
    "FINALLY": "Code that always executes after TRY/CATCH",
    "ENDTRY": "Marks the end of TRY/CATCH block",
    "PROCEDURE": "Defines a reusable code procedure/function",
    "ENDPROC": "Marks the end of a PROCEDURE",
    "PARAMETERS": "Declares procedure parameters",
    "DEFAULT": "Sets default value for a parameter",
    "RETURN": "Returns a value from a procedure",
    "DECLARE": "Declares local variables",
    "PUBLIC": "Declares public/global variables",
    "INCLUDE": "Includes external SSL file",
    "REGION": "Marks the beginning of a code region for organization",
    "ENDREGION": "Marks the end of a code region",
    "CLASS": "Defines a class",
    "INHERIT": "Specifies base class for inheritance",
    "BEGININLINECODE": "Marks the beginning of an inline code block",
    "ENDINLINECODE": "Marks the end of an inline code block",
    "EXITFOR": "Exits a FOR loop immediately",
    "EXITWHILE": "Exits a WHILE loop immediately",
    "EXITCASE": "Exits a CASE statement immediately",
    "LOOP": "Jump back to start of loop",
    "ERROR": "Mark error handling point",
    "LABEL": "Define a label for GOTO"
};