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
] as const;

export type SSLKeyword = typeof SSL_KEYWORDS[number];

export const BLOCK_START_KEYWORDS = [
    "IF", "WHILE", "FOR", "BEGINCASE", "TRY", "PROCEDURE", "CLASS", "REGION"
] as const;

export const BLOCK_END_KEYWORDS = [
    "ENDIF", "ENDWHILE", "NEXT", "ENDCASE", "ENDTRY", "ENDPROC", "ENDREGION"
] as const;

export const BLOCK_MIDDLE_KEYWORDS = [
    "ELSE", "CATCH", "FINALLY"
] as const;

export const CASE_KEYWORDS = [
    "CASE", "OTHERWISE"
] as const;

export const PROCEDURE_LEVEL_KEYWORDS = [
    "PARAMETERS", "DEFAULT", "PUBLIC", "DECLARE"
] as const;

export const MAJOR_BLOCK_START_KEYWORDS = [
    "PROCEDURE", "CLASS", "REGION"
] as const;

export const MAJOR_BLOCK_END_KEYWORDS = [
    "ENDPROC", "ENDREGION"
] as const;

export const REGION_START_PATTERN = /^\/\*\s*region/i;
export const REGION_END_PATTERN = /^\/\*\s*endregion/i;

export const INLINE_SQL_FUNCTIONS = [
    "SQLExecute",
    "GetDataSet",
    "GetDataSetWithSchemaFromSelect",
    "GetDataSetXMLFromSelect",
    "GetNETDataSet"
] as const;

export const PARAMETERIZED_SQL_FUNCTIONS = [
    "RunSQL",
    "LSearch",
    "LSelect",
    "LSelect1",
    "LSelectC",
    "GetDataSetEx"
] as const;

export const SQL_CONTEXT_FUNCTIONS = [
    ...INLINE_SQL_FUNCTIONS,
    ...PARAMETERIZED_SQL_FUNCTIONS
] as const;

export const MULTILINE_CONSTRUCT_KEYWORDS = [
    "IF", "ELSE", "WHILE", "FOR", "TO", "STEP", "BEGINCASE", "CASE", "OTHERWISE", "EXITCASE",
    "TRY", "CATCH", "FINALLY", "PROCEDURE", "PARAMETERS", "DEFAULT", "CLASS", "INHERIT", "REGION",
    "ENDIF", "ENDWHILE", "NEXT", "ENDCASE", "ENDTRY", "ENDPROC", "ENDREGION",
    "LOOP", "EXITWHILE", "RETURN"
] as const;

export const SSL_OPERATORS = [
    ".AND.", ".OR.", ".NOT."
] as const;

export const SSL_COMPOUND_OPERATORS = [
    ":=", "+=", "-=", "*=", "/=", "==", "!=", ">=", "<=", "<>", "^=", "%=", "++", "--"
] as const;

export const SSL_LITERALS = [
    ".T.", ".F.", "NIL"
] as const;


/**
 * Common loop counter variable names
 * These are exceptions to Hungarian notation rules
 */
export const LOOP_COUNTER_EXCEPTIONS = [
    "i", "j", "k", "x", "y", "z"
] as const;

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
        "description": "Executes a SQL command.",
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
        "returns": "bool",
        "signature": "Empty(any value)",
        "returnType": "bool",
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
        "params": "(string name, any[] parameters)",
        "returns": "any",
        "signature": "ExecFunction(string name, any[] parameters)",
        "returnType": "any",
        "category": "Runtime Functions",
        "frequency": "High",
        "untypedSignature": "ExecFunction(name, parameters)"
    },
    {
        "name": "UsrMes",
        "description": "Displays a user message.",
        "params": "(any a, any b)",
        "returns": "any",
        "signature": "UsrMes(any a, any b)",
        "returnType": "any",
        "category": "System Functions",
        "frequency": "High",
        "untypedSignature": "UsrMes(a, b)"
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
        "name": "AAdd",
        "description": "Adds an element to the end of an array.",
        "params": "(array target, any element)",
        "returns": "any",
        "signature": "AAdd(array target, any element)",
        "returnType": "any",
        "category": "Array Functions",
        "frequency": "High",
        "untypedSignature": "AAdd(target, element)"
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
        "description": "Executes a SQL command.",
        "params": "(string commandString, string friendlyName, any arrayOfValues)",
        "returns": "bool",
        "signature": "RunSQL(string commandString, string friendlyName, any arrayOfValues)",
        "returnType": "bool",
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
        "returns": "date",
        "signature": "Now()",
        "returnType": "date",
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
        "params": "(string commandString, any defaultValue, string friendlyName, array arrayOfValues)",
        "returns": "any",
        "signature": "LSearch(string commandString, any defaultValue, string friendlyName, array arrayOfValues)",
        "returnType": "any",
        "category": "Database Functions",
        "frequency": "High",
        "untypedSignature": "LSearch(commandString, defaultValue, friendlyName, arrayOfValues)"
    },
    {
        "name": "ExtractCol",
        "description": "Extracts a column from a 2D array.",
        "params": "(array target, number column)",
        "returns": "array",
        "signature": "ExtractCol(array target, number column)",
        "returnType": "array",
        "category": "Array Functions",
        "frequency": "High",
        "untypedSignature": "ExtractCol(target, column)"
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
        "returns": "string",
        "signature": "Time()",
        "returnType": "string",
        "category": "Date Functions",
        "frequency": "High",
        "untypedSignature": "Time()"
    },
    {
        "name": "IIf",
        "description": "Returns one of two values based on a condition.",
        "params": "(bool condition, any trueValue, any falseValue)",
        "returns": "any",
        "signature": "IIf(bool condition, any trueValue, any falseValue)",
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
        "category": "System Functions",
        "frequency": "High",
        "untypedSignature": "CreateUdObject(args)"
    },
    {
        "name": "RaiseError",
        "description": "Raises an error with message, location, and code.",
        "params": "(string message, string location, number errorCode, error innerException)",
        "returns": "bool",
        "signature": "RaiseError(string message, string location, number errorCode, error innerException)",
        "returnType": "bool",
        "category": "Error Functions",
        "frequency": "High",
        "untypedSignature": "RaiseError(message, location, errorCode, innerException)"
    },
    {
        "name": "Today",
        "description": "Returns the current date.",
        "params": "()",
        "returns": "date",
        "signature": "Today()",
        "returnType": "date",
        "category": "Date Functions",
        "frequency": "High",
        "untypedSignature": "Today()"
    },
    {
        "name": "GetDataSet",
        "description": "Retrieves a dataset from a data source.",
        "params": "(string commandString, array arrayOfValues, bool includeSchema, string tableName, bool nullAsBlank, array invariantDateColumns)",
        "returns": "string",
        "signature": "GetDataSet(string commandString, array arrayOfValues, bool includeSchema, string tableName, bool nullAsBlank, array invariantDateColumns)",
        "returnType": "string",
        "category": "Database Functions",
        "frequency": "High",
        "untypedSignature": "GetDataSet(commandString, arrayOfValues, includeSchema, tableName, nullAsBlank, invariantDateColumns)"
    },
    {
        "name": "BuildString",
        "description": "Builds a string from array elements with a delimiter.",
        "params": "(array target, number start, number count, string delimiter)",
        "returns": "string",
        "signature": "BuildString(array target, number start, number count, string delimiter)",
        "returnType": "string",
        "category": "Array Functions",
        "frequency": "High",
        "untypedSignature": "BuildString(target, start, count, delimiter)"
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
        "params": "(string friendlyName, bool commit)",
        "returns": "bool",
        "signature": "EndLimsTransaction(string friendlyName, bool commit)",
        "returnType": "bool",
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
        "params": "(string assembly, string typeName, array args, any asStatic)",
        "returns": "any",
        "signature": "LimsNETConnect(string assembly, string typeName, array args, any asStatic)",
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
        "params": "(array arrayOfValues, array arrayFields, string tableName, bool includeHeader, bool includeSchema)",
        "returns": "string",
        "signature": "GetDataSetXMLFromArray(array arrayOfValues, array arrayFields, string tableName, bool includeHeader, bool includeSchema)",
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
        "returns": "error",
        "signature": "GetLastSSLError()",
        "returnType": "error",
        "category": "Error Functions",
        "frequency": "Moderate",
        "untypedSignature": "GetLastSSLError()"
    },
    {
        "name": "BuildStringForIn",
        "description": "Builds a comma-separated string for SQL IN clause.",
        "params": "(array target)",
        "returns": "string",
        "signature": "BuildStringForIn(array target)",
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
        "name": "BuildArray",
        "description": "Builds an array from a string with delimiters.",
        "params": "(string text, bool crlfOk, string delimiter, bool unique, bool trimSpaces)",
        "returns": "array",
        "signature": "BuildArray(string text, bool crlfOk, string delimiter, bool unique, bool trimSpaces)",
        "returnType": "array",
        "category": "Array Functions",
        "frequency": "Moderate",
        "untypedSignature": "BuildArray(text, crlfOk, delimiter, unique, trimSpaces)"
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
        "name": "GetRegion",
        "description": "Retrieves a region from a string based on delimiters.",
        "params": "(any s, any src, any dst)",
        "returns": "string",
        "signature": "GetRegion(any s, any src, any dst)",
        "returnType": "string",
        "category": "Builtin Functions",
        "frequency": "Moderate",
        "untypedSignature": "GetRegion(s, src, dst)"
    },
    {
        "name": "AScan",
        "description": "Scans an array for a value and returns its index.",
        "params": "(array target, any value, number start, number count)",
        "returns": "number",
        "signature": "AScan(array target, any value, number start, number count)",
        "returnType": "number",
        "category": "Array Functions",
        "frequency": "Moderate",
        "untypedSignature": "AScan(target, value, start, count)"
    },
    {
        "name": "LimsSetCounter",
        "description": "Sets a counter value in the database.",
        "params": "(string tableName, string fieldName, string prefix, array arrayOfFields, array arrayOfValues, any incrementWith)",
        "returns": "number",
        "signature": "LimsSetCounter(string tableName, string fieldName, string prefix, array arrayOfFields, array arrayOfValues, any incrementWith)",
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
        "returns": "bool",
        "signature": "LimsSqlConnect(string friendlyName)",
        "returnType": "bool",
        "category": "Database Functions",
        "frequency": "Moderate",
        "untypedSignature": "LimsSqlConnect(friendlyName)"
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

export const ALL_SQL_FUNCTIONS = Array.from(new Set([
    ...INLINE_SQL_FUNCTIONS,
    ...PARAMETERIZED_SQL_FUNCTIONS,
    ...SSL_BUILTIN_FUNCTIONS.map(f => f.name)
]));
