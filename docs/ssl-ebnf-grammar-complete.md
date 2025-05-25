# Complete SSL Language EBNF Grammar

The following EBNF grammar formally defines the syntax of the STARLIMS Scripting Language (SSL) version 11 based on analysis of the formatter implementation, the SSL v10 tutorial, and additional language specifications.

## Purpose and Usage

This EBNF (Extended Backus-Naur Form) grammar serves as a formal definition of the SSL language syntax. It is particularly useful for:

1. **Language Tool Development**: Creating formatters, linters, syntax highlighters, and other tools for SSL.
2. **Parser Implementation**: Building parsers that can validate and process SSL code.
3. **Reference Documentation**: Providing a definitive reference for valid language constructs.

While this grammar defines what is syntactically valid in SSL, it does not prescribe specific formatting preferences or coding conventions - those are covered in the separate SSL Style Guide.

## Common SSL Code Patterns

The following patterns are frequently seen in SSL code and represent idiomatic usage:

1. **Variable Declaration and Initialization**:

    ```ssl
    :DECLARE sVariable;
    sVariable := "value";
    ```

2. **Procedure Definition and Calling**:

    ```ssl
    :PROCEDURE DoSomething;
    :PARAMETERS param1, param2;
    /* Procedure body */
    :RETURN result;
    :ENDPROC;

    /* Calling the procedure */
    result := DoProc("DoSomething", {value1, value2});
    ```

3. **Conditional Logic**:

    ```ssl
    :IF condition;
        /* True branch */
    :ELSE;
        /* False branch */
    :ENDIF;
    ```

4. **Array Processing**:

    ```ssl
    :DECLARE arItems, nCount;
    arItems := {1, 2, 3, 4, 5};
    nCount := Len(arItems);

    /* Process array elements */
    :DECLARE i;
    i := 0;
    :WHILE (i += 1) <= nCount;
        /* Process arItems[i] */
    :ENDWHILE;
    ```

5. **Error Handling**:

    ```ssl
    :TRY;
        /* Code that might cause an error */
    :CATCH;
        /* Error handling code */
    :ENDTRY;
    ```

6. **SQL Database Access**:
    ```ssl
    arResults := SqlExecute("SELECT field1, field2 FROM tablename WHERE condition = ?param?", {paramValue});
    ```

## Notation

This grammar uses Extended Backus-Naur Form (EBNF) with the following conventions:

-   Terminal symbols are enclosed in double quotes: `"keyword"`
-   Non-terminal symbols are written as identifiers: `Statement`
-   Alternatives are separated by vertical bars: `A | B`
-   Optional elements are enclosed in square brackets: `[A]`
-   Elements that can be repeated zero or more times are enclosed in braces: `{A}`
-   Parentheses are used for grouping: `(A | B) C`
-   The definition symbol is `::=`
-   Comments are preceded by `(* ` and followed by ` *)`

## Grammar Definition

```ebnf
(*
    SSL (STARLIMS Scripting Language) Version 11 EBNF Grammar
    Last updated: 2024-12-23
    Based on analysis of SSL v10 tutorial, function references, style guides,
    code examples, and direct feedback on SSL v11 behavior.
*)

(* Top-level structure *)
Program ::= ClassDefinition | {Statement} (* A script can be a class definition or a series of statements *)

(* Statement types *)
Statement ::= (
    ProcedureStatement |
    ConditionalStatement |
    LoopStatement |
    SwitchStatement |
    ErrorHandlingStatement | (* :TRY/:CATCH blocks *)
    ErrorBlockStanza |     (* :ERROR blocks *)
    DeclarationStatement |
    LogicStatement |
    CommentStatement |
    LabelStatement |
    RegionBlock |          (* :REGION/:ENDREGION keywords *)
    InlineCodeBlock |      (* :BEGININLINECODE/:ENDINLINECODE keywords *)
    BranchStatement |
    SqlStatement
) ";"

(* Class definitions *)
ClassDefinition ::= ClassDeclaration [InheritStatement] {ClassMember}
ClassDeclaration ::= ":" "CLASS" Identifier
InheritStatement ::= ":" "INHERIT" Identifier (* Identifier should handle qualified names like "Category.ClassName" *)
ClassMember ::= ClassFieldDeclaration | MethodDeclaration
ClassFieldDeclaration ::= ":" "DECLARE" IdentifierList (* Used for class fields *)
MethodDeclaration ::= ProcedureStatement (* Methods are defined like procedures within a class context *)

(* Procedure declarations *)
ProcedureStatement ::= ProcedureStart [ParameterDeclaration] [DefaultParameterDeclaration] {Statement} ProcedureEnd
ProcedureStart ::= ":" "PROCEDURE" Identifier
ProcedureEnd ::= ":" "ENDPROC"

(* Parameter declarations *)
ParameterDeclaration ::= ":" "PARAMETERS" ParameterList
DefaultParameterDeclaration ::= ":" "DEFAULT" DefaultParameterList

(* Parameter lists *)
ParameterList ::= Identifier {"," Identifier}
DefaultParameterList ::= Identifier "," Expression {"," Identifier "," Expression}

(* Conditional statements *)
ConditionalStatement ::= IfStatement | ElseStatement | EndIfStatement
IfStatement ::= ":" "IF" Expression
ElseStatement ::= ":" "ELSE"
EndIfStatement ::= ":" "ENDIF"

(* Loop statements *)
LoopStatement ::= WhileLoop | ForLoop | ExitWhileStatement | ExitForStatement | LoopContinue
WhileLoop ::= WhileStatement {Statement} EndWhileStatement
WhileStatement ::= ":" "WHILE" Expression
EndWhileStatement ::= ":" "ENDWHILE"
ExitWhileStatement ::= ":" "EXITWHILE"
ForLoop ::= ForStatement {Statement} NextStatement
ForStatement ::= ":" "FOR" Identifier ":=" Expression ":" "TO" Expression
NextStatement ::= ":" "NEXT"
ExitForStatement ::= ":" "EXITFOR"
LoopContinue ::= ":" "LOOP" (* Represents a 'continue' for the current loop iteration *)

(* Switch case statements *)
SwitchStatement ::= BeginCaseStatement {CaseBlock} [OtherwiseBlock] EndCaseStatement
BeginCaseStatement ::= ":" "BEGINCASE"
CaseBlock ::= CaseStatement {Statement} [ExitCaseStatement]
CaseStatement ::= ":" "CASE" Expression
OtherwiseBlock ::= OtherwiseStatement {Statement}
OtherwiseStatement ::= ":" "OTHERWISE"
EndCaseStatement ::= ":" "ENDCASE"
ExitCaseStatement ::= ":" "EXITCASE"

(* Error handling statements *)
ErrorHandlingStatement ::= TryBlock (* For :TRY/:CATCH/:FINALLY structure *)
TryBlock ::= TryStatement {Statement} CatchBlock [FinallyBlock] EndTryStatement
TryStatement ::= ":" "TRY"
CatchBlock ::= CatchStatement {Statement}
CatchStatement ::= ":" "CATCH"
FinallyBlock ::= FinallyStatement {Statement}
FinallyStatement ::= ":" "FINALLY"
EndTryStatement ::= ":" "ENDTRY"

ErrorBlockStanza ::= ErrorMarker {Statement} (* For :ERROR structure *)
ErrorMarker ::= ":" "ERROR"

(* Declaration statements *)
DeclarationStatement ::= (
    ParametersStatement |
    DeclareStatement |
    DefaultStatement |
    PublicStatement |
    IncludeStatement
)
ParametersStatement ::= ":" "PARAMETERS" IdentifierList (* Parameters are comma-separated *)
DeclareStatement ::= ":" "DECLARE" IdentifierList
DefaultStatement ::= ":" "DEFAULT" DefaultParameterList (* Default values for parameters *)
PublicStatement ::= ":" "PUBLIC" IdentifierList
IncludeStatement ::= ":" "INCLUDE" StringLiteral (* StringLiteral usually contains a script path/name *)

(* Logic statements *)
LogicStatement ::= Assignment | FunctionCall | Expression | ReturnStatement
Assignment ::= (VariableAccess | PropertyAccess) AssignmentOperator Expression
AssignmentOperator ::= ":=" | "+=" | "-=" | "*=" | "/=" | "^=" (* *)
ReturnStatement ::= ":" "RETURN" [Expression]

(* Function calls *)
FunctionCall ::= DirectFunctionCall | DoProcCall | ExecFunctionCall
DirectFunctionCall ::= Identifier "(" [ArgumentList] ")"
DoProcCall ::= "DoProc" "(" StringLiteral "," ArrayLiteral ")" (* *)
ExecFunctionCall ::= "ExecFunction" "(" StringLiteral "," ArrayLiteral ")" (* *)
ArgumentList ::= Expression {"," Expression}

(* Comment statements *)
CommentStatement ::= BlockComment | SingleLineComment | RegionComment | EndRegionComment
BlockComment ::= "/*" {Character} ";" (* SSL block comments span multiple lines *)
SingleLineComment ::= "/*" {Character} ";" (* SSL single line comments appear on a single line *)
RegionComment ::= "/*" "region" {Character} ";" (* Comments used for region markers *)
EndRegionComment ::= "/*" "endregion" {Character} ";" (* Comments used for endregion markers *)

(* Special structures *)
LabelStatement ::= ":" "LABEL" Identifier (* Used with Branch() *)
RegionBlock ::= RegionStart {Character} RegionEnd (* Keyword-based regions :REGION / :ENDREGION *)
RegionStart ::= ":" "REGION" Identifier ";"
RegionEnd ::= ":" "ENDREGION" ";"
InlineCodeBlock ::= InlineCodeStart {Statement} InlineCodeEnd (* *)
InlineCodeStart ::= ":" "BEGININLINECODE" [StringLiteral | Identifier] ";"
InlineCodeEnd ::= ":" "ENDINLINECODE" ";"
DynamicCodeExecution ::= "ExecUDF" "(" StringLiteral ["," ArrayLiteral] ")" (* *)
BranchStatement ::= "Branch" "(" StringLiteral ")" (* StringLiteral usually "LABEL labelname" *)

(* SQL Integration *)
SqlStatement ::= SqlExecute | LSearch
SqlExecute ::= "SqlExecute" "(" StringLiteral ["," ArrayLiteral] ")" (* Parameters typically ?paramName? *)
LSearch ::= "LSearch" "(" StringLiteral ["," Expression] ["," Expression] ["," ArrayLiteral] ")" (* Parameters typically ? *)
SqlParameter ::= "?" Identifier "?" | "?"

(* Object-oriented statements specific to SSL classes/UDOs *)
ObjectCreation ::= "CreateUDObject" "(" [StringLiteral] ")" (* StringLiteral is ClassName, or empty for Expando *)
MethodCall ::= Identifier ":" Identifier (* Object:Method *)
ObjectPropertyAccess ::= Identifier ":" Identifier (* Object:Property *)

(* Expressions *)
Expression ::= LogicalExpression
LogicalExpression ::= ComparisonExpression {LogicalOperator ComparisonExpression}
LogicalOperator ::= ".AND." | ".OR." (* *)
ComparisonExpression ::= ArithmeticExpression {ComparisonOperator ArithmeticExpression}
ComparisonOperator ::= "==" | "!=" | "<" | ">" | "<=" | ">=" | "=" (* '#' removed based on feedback *)
ArithmeticExpression ::= Term {AdditiveOperator Term}
AdditiveOperator ::= "+" | "-"
Term ::= Factor {MultiplicativeOperator Factor}
MultiplicativeOperator ::= "*" | "/" | "%"
Factor ::= PowerOperand {"^" PowerOperand} (* Assuming ^ for exponentiation *)
PowerOperand ::= [UnaryOperator] Primary
UnaryOperator ::= "+" | "-" | "!" | ".NOT." (* *)

(* Bitwise operations - as functions, not operators *)
BitwiseOperation ::= "_AND" "(" Expression "," Expression ")" |
                    "_OR" "(" Expression "," Expression ")" |
                    "_XOR" "(" Expression "," Expression ")" |
                    "_NOT" "(" Expression ")"

(* Primary expressions *)
Primary ::=
    Literal |
    VariableAccess |
    PropertyAccess |     (* Object:Property syntax for UDOs *)
    ObjectPropertyAccess | (* For clarity, may be merged with PropertyAccess depending on context *)
    ArrayAccess |
    FunctionCall |
    BitwiseOperation |
    "(" Expression ")" |
    IncrementExpression |
    MethodCall (* Object:Method() syntax *)

IncrementExpression ::= Identifier ("++" | "--") | ("++" | "--") Identifier (* *)
VariableAccess ::= Identifier
PropertyAccess ::= Identifier ":" Identifier (* SSL uses colon for property access of UDOs and system objects *)
ArrayAccess ::= Identifier ArraySubscript
ArraySubscript ::= "[" Expression {"," Expression} "]" | "[" Expression "]" {("[" Expression "]")} (* Supports arr[1,2] and arr[1][2] *)

(* Literals *)
Literal ::= NumberLiteral | StringLiteral | BooleanLiteral | ArrayLiteral | NilLiteral | DateLiteral | CodeBlockLiteral
NumberLiteral ::= IntegerPart ( DecimalPart Exponent? )? | IntegerPart (* Revised based on '7e2' not working, '7.0e2' working *)
IntegerPart ::= Digit {Digit}
DecimalPart ::= "." Digit {Digit} (* Ensures at least one digit after the decimal point *)
Exponent    ::= ("e" | "E") ["-"] Digit {Digit}

StringLiteral ::= '"' {Character} '"' | "'" {Character} "'" | "[" {Character} "]" (* *)
BooleanLiteral ::= ".T." | ".F." (* TRUE and FALSE also mentioned in EBNF notes but .T./.F. are canonical *)
ArrayLiteral ::= "{" [ExpressionList] "}" | "{" ArrayLiteral {"," ArrayLiteral} "}"
NilLiteral ::= "NIL" (* *)
DateLiteral ::= "{" NumberLiteral "," NumberLiteral "," NumberLiteral ["," NumberLiteral "," NumberLiteral "," NumberLiteral] "}" (* Specific format; common usage is via CtoD() or Today() *)
CodeBlockLiteral ::= "{|" [IdentifierList] "|" ExpressionList "}" (* e.g. {|x| x*x} *)


(* Lists *)
IdentifierList ::= Identifier {"," Identifier}
ExpressionList ::= Expression {"," Expression}

(* Basic elements *)
Identifier ::= (Letter | "_") {Letter | Digit | "_"}
Letter ::= "A" | "B" | ... | "Z" | "a" | "b" | ... | "z"
Digit ::= "0" | "1" | ... | "9"
Character ::= Letter | Digit | Symbol (* Define Symbol appropriately, excluding delimiter of current context *)
Symbol ::= (* Any printable character, specific exclusions depend on context like string delimiters *)
```

## Notes on the Grammar

1. **Hungarian Notation**: While not explicitly defined in the grammar, SSL uses Hungarian notation for variable naming (e.g., `sName`, `nValue`, `bIsValid`).

2. **Case Insensitivity**: Keywords in SSL are case-insensitive. For clarity, they are shown in uppercase in this grammar.

3. **Statement Termination**: All statements in SSL are terminated with a semicolon (`;`).

4. **Property Access**: SSL uses a colon (`:`) for property access rather than the more common dot notation.

5. **Logical Literals**: Boolean values true and false are represented as `.T.` and `.F.` with surrounding periods.

6. **Increment/Decrement**: The language supports both prefix and postfix increment/decrement operators.

7. **Block Structure**: Control structures like conditionals and loops follow a block-based structure with explicit end markers (`:ENDIF`, `:ENDWHILE`, etc.).

8. **Comments**: Comments in SSL start with `/*` and end with a semicolon, rather than using traditional comment delimiters like `*/`.

9. **Regions and Comments**: The language supports two types of regions:

    - Code regions marked with `:REGION` and `:ENDREGION` keywords
    - Comment regions marked with `/* region` and `/* endregion`

10. **SQL Integration**: SQL queries are typically represented as string literals. Parameters in SQL statements can be represented as `?PARAMETER?` (for SqlExecute) or simply `?` (for LSearch and other functions).

11. **For Loop Structure**: For loops require an immediate iterator assignment (`:FOR i := 1 :TO 10;`) which cannot be set outside the loop declaration.

12. **Array Access**: Multi-dimensional arrays can be accessed using comma notation `array[1,2]` or chained bracket notation `array[1][2]`. Array indexing is 1-based (the first element is at index 1, not 0).

13. **Date Literals**: Dates can be represented using special array-like notation with components for year, month, day, and optionally hour, minute, second. The `Now()` function returns current date in MM/DD/YYYY HH:MM:SS format.

14. **Scientific Notation**: Number literals can include scientific notation using 'e' or 'E' followed by an optional negative sign and exponent. The formats `1.23e5`, `4.56E-3`, and `0.5e1` are supported, while formats with an explicit plus sign (`9E+1`), without a decimal point before the 'e' (`7e2`), or with a leading decimal point without a zero (`.5e1`) are not supported.

15. **Function Calls**: Functions are called using one of three methods:

    - Direct call for built-in functions: `functionName(param1, param2)`
    - DoProc for internal functions: `DoProc("functionName", {param1, param2})`
    - ExecFunction for external functions: `ExecFunction("functionName", {param1, param2})`

16. **Parameter Passing**: Parameters are passed positionally as an array. To skip parameters, empty array positions are used: `DoProc("function", {param1, , param3})`.

17. **Default Parameters**: Default values for parameters can be specified using the `:DEFAULT` keyword after the `:PARAMETERS` declaration.

18. **Object Creation**: Objects are created using functions like `CreateUDObject()` and properties are accessed using colon notation: `object:property`.

19. **Error Handling**: SSL supports both traditional error handling with `:ERROR` and structured exception handling with `:TRY`/`:CATCH`/`:FINALLY`/`:ENDTRY`.

20. **Labels and Branching**: While `:LABEL` is supported for backward compatibility, the preferred method for branching is using the `Branch()` function. The `StringLiteral` argument for `Branch()` should typically be of the format `"LABEL actualLabelName"`.

21. **Dynamic Code**: SSL supports dynamic code execution through `ExecUDF()` function and code blocks.

22. **Inline Code and Regions**: Special blocks for storing and retrieving code snippets can be created using `:BEGININLINECODE`/`:ENDINLINECODE` and `:REGION`/`:ENDREGION`.

23. **String Delimiters**: Strings can be delimited using double quotes (`"`), single quotes (`'`), or square brackets (`[` and `]`).

24. **Assignment Operators**: In addition to the standard assignment operator (`:=`), SSL supports compound assignment operators (`+=`, `-=`, `*=`, `/=`, `^=`).

25. **Object-Oriented Programming**: SSL supports class definitions with inheritance and methods using the `:CLASS`, `:INHERIT`, and `:PROCEDURE` keywords. A class definition encompasses the script in which it is declared, and there is no explicit `:ENDCLASS` keyword; the class structure ends with the script.

26. **Comparison Operators**: The primary comparison operators are `=` (equality with type coercion) and `==` (exact equality). The operators `===` and `!==` are not supported.

27. **Bitwise Operations**: Bitwise operations are performed using function calls (`_AND()`, `_OR()`, `_XOR()`, `_NOT()`) rather than operators (`&`, `|`, `^`, `~`).

## Implementation Considerations for Formatting Tools

When implementing a formatter for SSL, consider the following specifics that may not be explicitly defined in the grammar but affect code readability:

1. **Indentation**: Code blocks within control structures (`:IF`/`:ENDIF`, `:WHILE`/`:ENDWHILE`, etc.) should be indented consistently, typically by 4 spaces.

2. **Alignment**: Parameters in multi-line function calls or array declarations are often aligned for readability.

3. **Empty Lines**: Use empty lines to separate logical sections of code, particularly between procedure definitions.

4. **Line Length**: Break long lines at logical points (typically around 90 characters), especially for:

    - Long parameter lists in function calls
    - Complex logical expressions
    - SQL queries
    - Array declarations

5. **SQL Formatting**: SQL statements embedded in strings should follow SQL formatting conventions, with clauses (SELECT, FROM, WHERE) aligned and properly indented.

6. **Comment Alignment**: End-of-line comments should be aligned at a consistent column position when appearing on consecutive lines.

7. **Section Headers**: Consider preserving region markers and section headers as structural elements of the code.

8. **Special Case Handling**: Take special care with:
    - String concatenation operators
    - Spacing around property access colons
    - Logical operators preceded by dots (`.AND.`, `.OR.`, `.T.`, `.F.`)
    - Increment/decrement operators which should not have spaces
