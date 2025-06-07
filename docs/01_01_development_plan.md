# Project Plan: SSL Formatter - Tier 1 Core Foundation

This document outlines the detailed plan for completing Tier 1: "Core Formatter & Language Foundation (EBNF-Driven)" for the SSL Formatter VS Code extension. The successful completion of this tier will result in a robust understanding of SSL code structure based on the ssl-ebnf-grammar-complete.md and will deliver essential formatting capabilities.

## Part 1: EBNF-Compliant Tokenizer

**Purpose:** To decompose raw SSL code strings into a stream of recognized tokens (keywords, identifiers, literals, operators, punctuation, comments) based on the lexical definitions in ssl-ebnf-grammar-complete.md.

### Chapter 1.1: Identifying Terminal Symbols from EBNF

#### Section 1.1.1: Process and List

The first step is to meticulously scan the ssl-ebnf-grammar-complete.md and extract all explicitly defined terminal symbols. These are typically enclosed in double quotes (e.g., ":", "PROCEDURE") or defined by lexical rules (e.g., Identifier, NumberLiteral).

**Action:** Create a comprehensive list of all such terminal symbols.

**Example Fragment from EBNF:**

```ebnf
ProcedureStart ::= ":" "PROCEDURE" Identifier
AssignmentOperator ::= ":=" | "+="
BooleanLiteral ::= ".T." | ".F."
```

**Derived Terminals:** ":", "PROCEDURE", Identifier (defined by its own rule), ":=", "+=", ".T.", ".F."

#### Section 1.1.2: Categorization

Group the identified terminal symbols into logical categories for easier management and implementation in the tokenizer.

-   **Keywords:** e.g., "PROCEDURE", "IF", "WHILE", ".AND.", ".T."
-   **Operators:** e.g., ":=", "+", "==", "<"
-   **Literals:** Rules defining NumberLiteral, StringLiteral, BooleanLiteral, NilLiteral, DateLiteral, CodeBlockLiteral
-   **Punctuation:** e.g., ":", ";", "(", ")", "[", "]", ","
-   **Comments:** Patterns for SSL's unique comment style
-   **Identifiers:** The rule for Identifier

### Chapter 1.2: Designing Token Data Structures

**Developer Context:** This chapter explains how we'll define the structure of the "tokens" our tokenizer will create. We'll use two important TypeScript features: `enum` and `interface`. Understanding these will help clarify why we structure our data this way.

#### Section 1.2.1: Understanding enum for Token Types

**What is an enum?**

An enum (short for enumeration) in TypeScript allows us to define a set of named constants. Think of it as creating a custom type that can only be one of a few predefined values. For example, if you were modeling a traffic light, you might create an enum called `TrafficLightState` with members `RED`, `YELLOW`, and `GREEN`.

**Why use an enum for TokenType?**

Our tokenizer will identify many different kinds of tokens (like keywords, operators, identifiers, punctuation, etc.). Instead of representing these kinds with raw numbers (e.g., 0 for a keyword, 1 for an operator) or simple strings (e.g., "keyword", "operator"), an enum provides:

-   **Readability:** Code like `if (token.kind === TokenType.KEYWORD_PROCEDURE)` is much clearer than `if (token.kind === 0)`
-   **Type Safety:** TypeScript ensures that a variable of TokenType can only hold one of the values defined in the TokenType enum. This catches typos and incorrect assignments at compile time (before you run the code)
-   **Maintainability:** If you add or change a token type, you do it in one place (the enum definition), and the TypeScript compiler can help find all places that use it

#### Section 1.2.2: Defining the TokenType Enumeration

Based on the categories identified in Chapter 1.1, we will create a comprehensive enum called `TokenType`. Each member of this enum will represent a distinct kind of token our tokenizer can produce.

**Conceptual Example (TypeScript):**

```typescript
enum TokenType {
    // --- Keywords ---
    KEYWORD_PROCEDURE,
    KEYWORD_IF,
    KEYWORD_WHILE,
    KEYWORD_DECLARE,
    KEYWORD_CLASS,
    KEYWORD_DOT_AND, // For .AND.
    KEYWORD_DOT_OR, // For .OR.
    KEYWORD_DOT_NOT, // For .NOT.
    KEYWORD_TRUE, // For .T.
    KEYWORD_FALSE, // For .F.
    // ... add all other SSL keywords (ELSE, ENDIF, FOR, RETURN, etc.)

    // --- Operators ---
    OPERATOR_ASSIGN, // :=
    OPERATOR_PLUS_ASSIGN, // +=
    // ... add all other assignment, comparison, arithmetic, logical, power operators

    // --- Literals ---
    IDENTIFIER, // For variable names, procedure names, etc.
    STRING_LITERAL, // For "text" or 'text'
    NUMBER_LITERAL, // For numbers like 123, 45.67, 1.0e2
    NIL_LITERAL, // For NIL

    // --- Punctuation ---
    COLON, // :
    SEMICOLON, // ;
    L_PAREN, // (
    R_PAREN, // )
    L_BRACKET, // [
    R_BRACKET, // ]
    L_CURLY, // {
    R_CURLY, // }
    COMMA, // ,

    // --- Comments ---
    BLOCK_COMMENT, // For /* ... ; */ including region/endregion variations

    // --- Special / Meta Tokens ---
    WHITESPACE, // If choosing to emit whitespace tokens (often skipped)
    EOL, // End Of Line (useful for formatting and line counting)
    EOF, // End Of File (signals the end of input)
}
```

**Action:** Create the complete TokenType enum in your codebase, ensuring every distinct token category and specific keyword/operator from the EBNF has a corresponding member.

#### Section 1.2.3: Understanding interface for Token Structure

**What is an interface?**

An interface in TypeScript is like a blueprint or a contract for the shape of an object. It defines what properties an object should have and what the data types of those properties should be. It describes what an object should look like but doesn't create the object itself.

**Why use an interface for our Token objects?**

Each token our tokenizer produces isn't just a TokenType; it also carries other important information:

-   The actual text from the source code that forms the token (e.g., the word "PROCEDURE", or the variable name "sMyVar"). This is called the "lexeme."
-   Where the token was found in the source file (line number, starting column, ending column).

An interface (which we'll call `Token`) allows us to define this common structure precisely. This brings:

-   **Consistency:** Every token object will have the same set of properties
-   **Clarity:** When you pass a Token object around, its structure is well-defined and easy to understand
-   **Type Checking:** TypeScript will ensure that any object we claim is a Token actually has all the required properties (like type, lexeme, line, startColumn) and that these properties have the correct data types (e.g., line must be a number). This catches many errors early

#### Section 1.2.4: Defining the Token Interface

We will define an interface named `Token`. Any object that represents a token produced by our tokenizer must conform to this interface.

**Conceptual Example (TypeScript):**

```typescript
interface Token {
    type: TokenType; // The kind of token, using our TokenType enum from above
    lexeme: string; // The actual text of the token from the source code
    // e.g., for a KEYWORD_PROCEDURE token, lexeme would be "PROCEDURE"
    line: number; // The 1-indexed line number where the token begins
    startColumn: number; // The 1-indexed column number where the token begins on that line
    endColumn: number; // The 1-indexed column number where the token ends on that line
    // For a single character token like ';', startColumn and endColumn might be the same

    // Optional:
    // startIndex?: number; // Overall 0-indexed start position in the entire input string
    // endIndex?: number;   // Overall 0-indexed end position in the entire input string
}
```

**Action:** Define the Token interface in your codebase. Ensure it includes all necessary attributes to fully describe a token and its position in the source.

By using `enum TokenType` and `interface Token`, we establish a clear, type-safe, and maintainable way to represent the fundamental units of SSL code that our tokenizer will identify. This strong foundation is crucial for building a reliable parser and formatter.

### Chapter 1.3: Implementing Token Recognition Logic

This is the core of the tokenizer, where patterns are implemented to match sequences of characters to the defined token types.

#### Section 1.3.1: Keywords

-   Implement logic to recognize all SSL keywords (e.g., PROCEDURE, IF, WHILE, DECLARE, CLASS, .AND., .OR., .T., .F.)
-   **Case-Insensitivity:** As per SSL specifications, keyword recognition must be case-insensitive. The stored lexeme should ideally be normalized (e.g., to uppercase) or the original casing preserved if needed for specific style guides (though normalization is common for parsers)
-   Keywords often overlap with identifier rules, so a common strategy is to first match an identifier, then check if it's a reserved keyword

**Example Keywords from EBNF:**

```
"CLASS", "INHERIT", "PROCEDURE", "PARAMETERS", "DEFAULT", "IF", "ELSE", "ENDIF",
"WHILE", "ENDWHILE", "FOR", "TO", "NEXT", "EXITWHILE", "EXITFOR", "LOOP",
"BEGINCASE", "CASE", "OTHERWISE", "ENDCASE", "EXITCASE", "TRY", "CATCH",
"FINALLY", "ENDTRY", "ERROR", "DECLARE", "PUBLIC", "INCLUDE", "RETURN",
"LABEL", "REGION", "ENDREGION", "BEGININLINECODE", "ENDINLINECODE"
```

#### Section 1.3.2: Identifiers

-   Implement the EBNF rule for Identifier: `(Letter | "_") {Letter | Digit | "_"}`
-   This means an identifier must start with a letter or an underscore, followed by zero or more letters, digits, or underscores

#### Section 1.3.3: Literals

##### Section 1.3.3.1: Number Literals

-   Implement recognition based on EBNF: `NumberLiteral ::= IntegerPart ( DecimalPart Exponent? )? | IntegerPart`
-   `IntegerPart ::= Digit {Digit}`
-   `DecimalPart ::= "." Digit {Digit}` (Ensures at least one digit after decimal)
-   `Exponent ::= ("e" | "E") ["-"] Digit {Digit}`
-   Pay attention to SSL's scientific notation specifics mentioned in EBNF notes (e.g., 7.0e2 is valid, 7e2 might not be if it means 7.0e2 implicitly or is an error). The EBNF note 14 states: "formats with an explicit plus sign (9E+1), without a decimal point before the 'e' (7e2), or with a leading decimal point without a zero (.5e1) are not supported." Ensure tokenizer adheres to this

##### Section 1.3.3.2: String Literals

-   Implement recognition based on EBNF: `StringLiteral ::= '"' {Character} '"' | "'" {Character} "'"`
-   Strings can be delimited by double quotes (") or single quotes (')
-   The tokenizer must handle characters within the string until the matching closing delimiter is found
-   Consider handling of escaped characters within strings (e.g., \" inside a double-quoted string, or '' inside a single-quoted string), although the provided EBNF Character definition is generic. If SSL has specific escape sequences, they must be handled

##### Section 1.3.3.3: Boolean Literals

-   Recognize `.T.` and `.F.` as boolean literals. Assign them appropriate TokenType members (e.g., `KEYWORD_TRUE`, `KEYWORD_FALSE`)

##### Section 1.3.3.4: NIL Literal

-   Recognize `NIL` as the nil literal. Assign it the `TokenType.NIL_LITERAL`

##### Section 1.3.3.5: Date Literals

-   Recognize the specific array-like format from EBNF: `DateLiteral ::= "{" NumberLiteral "," NumberLiteral "," NumberLiteral ["," NumberLiteral "," NumberLiteral "," NumberLiteral] "}"""
-   This means the tokenizer needs to recognize the opening `{`, then expect a sequence of numbers and commas, and a closing `}`
-   This is complex for a tokenizer and might involve some limited lookahead or be partially handled by the parser expecting this structure after seeing `{`
-   A simpler tokenizer might emit `{`, `NumberLiteral`, `,`, etc., as separate tokens

**Decision Point:** Decide if DateLiteral is a single token or if its components are tokenized separately for the parser to assemble. For simplicity and robustness, tokenizing components (`L_CURLY`, `NUMBER_LITERAL`, `COMMA`, `R_CURLY`) is often preferred. The parser will then be responsible for assembling these into a "DateLiteral" concept.

##### Section 1.3.3.6: Code Block Literals

-   Recognize the EBNF format: `CodeBlockLiteral ::= "{|" [IdentifierList] "|" ExpressionList "}"""
-   Similar to DateLiteral, this is structurally complex for a single token. It's more common to tokenize the delimiters `{|` (as perhaps `L_CURLY_BAR`), `|` (as `BAR`), `}` (as `R_CURLY`) and the internal identifiers/expressions separately

**Decision Point:** Similar to DateLiteral, opt for tokenizing components. The EBNF should ideally define `{|` and `|` as distinct terminal symbols if they are to be tokenized this way. If `{|` is not a single terminal, then it's `{` then `|`. Clarify if `{|` should be treated as one multi-character operator/punctuation token or two separate ones.

#### Section 1.3.4: Operators

Implement recognition for all SSL operators:

-   **Assignment Operators:** `:=`, `+=`, `-=`, `*=`, `/=`, `^=`
-   **Comparison Operators:** `==`, `!=`, `<`, `>`, `<=`, `>=`, `=` (Note: EBNF notes mention `=` for equality with type coercion and `==` for exact equality. `#` was removed)
-   **Logical Operators:** `.AND.`, `.OR.` (These will likely be tokenized as keywords like `KEYWORD_DOT_AND` based on their structure, but the parser will treat them as operators). Also `.NOT.` (unary, likely `KEYWORD_DOT_NOT`)
-   **Arithmetic Operators:** `+`, `-`, `*`, `/`, `%`
-   **Power Operator:** `^`
-   **Unary Operators:** `+`, `-`, `!`, `.NOT.` (as per EBNF Primary production. `!` needs its own `OPERATOR_BANG` or similar TokenType)
-   **Increment/Decrement:** `++`, `--` (can be prefix or postfix). Tokenize as `OPERATOR_INCREMENT`, `OPERATOR_DECREMENT`

#### Section 1.3.5: Punctuation

-   **Colon:** `:` (Token Type: `COLON`)
-   **Semicolon:** `;` (Token Type: `SEMICOLON`)
-   **Parentheses:** `(`, `)` (Token Types: `L_PAREN`, `R_PAREN`)
-   **Square Brackets:** `[`, `]` (Token Types: `L_BRACKET`, `R_BRACKET`)
-   **Curly Braces:** `{`, `}` (Token Types: `L_CURLY`, `R_CURLY`)
-   **Comma:** `,` (Token Type: `COMMA`)

#### Section 1.3.6: Comments

**Critically Important:** Implement SSL's unique comment style.

-   `BlockComment ::= "/\*" {Character} ";"""
-   `SingleLineComment ::= "/*" {Character} ";"` (Structurally the same as block comments, but contextually on one line)
-   `RegionComment ::= "/\*" "region" {Character} ";"""
-   `EndRegionComment ::= "/*" "endregion" {Character} ";"`

The tokenizer must correctly identify the start `/*` and scan until the terminating `;` is found for that comment block.

It needs to differentiate between a semicolon terminating a comment and a semicolon terminating a statement. This implies that once `/*` is seen, the tokenizer is in a "comment scanning mode" until it finds the corresponding `;` for that specific comment.

The content of the comment, including the region/endregion markers if present, should be part of the token's lexeme or accessible via a subtype (e.g., `TokenType.BLOCK_COMMENT` with a `commentContent` property on the Token object, or even specific types like `REGION_COMMENT_TOKEN`). For simplicity, a single `BLOCK_COMMENT` type where the lexeme includes the full `/* ... ;` is common.

#### Section 1.3.7: Whitespace and End-of-Line (EOL) Handling Strategy

-   **Whitespace:** Spaces and tabs are generally insignificant to the SSL parser except when inside string literals. The tokenizer will usually skip/ignore them
-   **EOL:** Line breaks (like `\n`, `\r\n`) are important for:
    -   Incrementing the line number for token locations
    -   Potentially for formatting rules (e.g., preserving a certain number of blank lines, or rules about statements per line)

**Decision:**

-   **Skip most whitespace:** Do not emit tokens for spaces/tabs
-   **Emit EOL tokens:** Emit an EOL token for each line break. This is very useful for a formatter to understand line structure
-   **Emit EOF token:** A final EOF token signals the end of the input

For a formatter, having EOL tokens is highly beneficial. The formatter can then decide whether to preserve, add, or remove EOLs based on its rules.

### Chapter 1.4: Tokenizer Error Handling

#### Section 1.4.1: Identifying Lexical Errors

Implement mechanisms to detect and report lexical errors, such as:

-   **Unterminated String Literals:** e.g., `"abc` without a matching closing `"`. The tokenizer should consume characters until EOL or EOF if a closing quote isn't found, then report the error
-   **Unterminated Comments:** e.g., `/* abc` without a matching closing `;` for that comment block. Similar to strings, consume until EOL/EOF and report
-   **Invalid Characters:** Characters that do not fit any defined SSL token pattern in the current context (e.g., a stray `@` symbol if it's not part of any SSL construct)

#### Section 1.4.2: Reporting and Basic Recovery

When a lexical error is found, the tokenizer should:

-   Create an error object or diagnostic record containing the error message, line number, and column number
-   Store this error information. **Note:** The collected error information will be used by a later component (detailed in a future Part of this plan, e.g., Part 5: Diagnostic Reporting) to display these errors to the user in the VS Code interface (e.g., in the "Problems" panel)
-   Attempt basic recovery: usually by skipping the problematic character(s) and trying to resume tokenization from the next plausible point. This allows the tokenizer to find multiple lexical errors in a single pass

### Chapter 1.5: Tokenizer Output and Integration

#### Section 1.5.1: Producing a Token Stream/List

The tokenizer's primary output will be a list (or an iterable stream) of Token objects (conforming to the Token interface defined in Section 1.2.4). This list will be the input for the Parser (Part 2). The stream must be terminated by an EOF token.

### Chapter 1.6: Tokenizer Testing Strategy

#### Section 1.6.1: Unit Test Cases for Each Token Category

-   Create specific tests for keywords, identifiers, each type of literal (numbers, strings, booleans, NIL), each operator, and all punctuation marks
-   For each test case, provide an input SSL snippet and assert that the tokenizer produces the expected sequence of Token objects, verifying:
    -   Correct TokenType
    -   Correct lexeme
    -   Accurate line, startColumn, and endColumn numbers

**Example Test (Conceptual):**

Input: `:PROCEDURE MyProc;`

Expected Token Sequence:

```typescript
[
    { type: TokenType.COLON, lexeme: ":", line: 1, startColumn: 1, endColumn: 1 },
    {
        type: TokenType.KEYWORD_PROCEDURE,
        lexeme: "PROCEDURE",
        line: 1,
        startColumn: 2,
        endColumn: 10,
    },
    { type: TokenType.IDENTIFIER, lexeme: "MyProc", line: 1, startColumn: 12, endColumn: 17 },
    { type: TokenType.SEMICOLON, lexeme: ";", line: 1, startColumn: 18, endColumn: 18 },
    { type: TokenType.EOF, lexeme: "", line: 1, startColumn: 19, endColumn: 19 },
];
```

#### Section 1.6.2: Edge Case Testing

Test thoroughly with various edge cases:

-   Empty input file (should produce only EOF)
-   File with only comments
-   File with only whitespace and EOLs (should produce EOLs and EOF)
-   Combinations of tokens with no separating whitespace (e.g., `sVar:=10;`)
-   **Maximal Munch Principle:** Ensure the tokenizer consumes the longest possible valid token. For example, `:=` should be one `OPERATOR_ASSIGN` token, not a `COLON` followed by an `=`. Similarly for `+=`, `++`, etc.
-   All supported number literal formats from EBNF note 14, including valid and invalid ones (testing error reporting)
-   Strings with internal delimiters (e.g., `"a 'b' c"` or `'a "b" c'`)
-   Comments immediately adjacent to other tokens, or nested (if SSL allowed nested comments, which it typically doesn't in the `/* ... ;` style)
-   Comments containing characters that look like other tokens (e.g., `/* :IF x := 10;`)

#### Section 1.6.3: Leveraging AI for Test Case Generation

-   Use AI to generate diverse SSL code snippets based on the EBNF's lexical rules to broaden test coverage
-   Prompt AI with specific EBNF rules for terminals (keywords, operators, literal patterns) and ask for:
    -   A variety of valid examples
    -   Examples that are intentionally lexically incorrect to test error detection (e.g., invalid characters, unterminated constructs)

---

Part 2: EBNF-Compliant Parser & Abstract Syntax Tree (AST)

Purpose: To consume the token stream from the tokenizer, validate its structure against the EBNF grammar rules, and build an Abstract Syntax Tree (AST) that represents the hierarchical structure of the SSL code. This phase also includes identifying and packaging syntax errors for later reporting.
Chapter 2.1: Parser Strategy and Setup
Sub-section 2.1.1: Choosing a Parsing Strategy

We will implement a Recursive Descent Parser. This top-down parsing strategy is highly suitable for hand-writing a parser from an EBNF grammar. The structure of the parser code will closely mirror the structure of the grammar rules, making it intuitive to develop and debug.

    Key Characteristics:

        A set of mutually recursive functions, where each function corresponds to a non-terminal symbol in the grammar (e.g., a parseStatement() function, a parseExpression() function).

        The parser maintains its state by advancing a cursor through the token stream produced by the tokenizer (Part 1).

Sub-section 2.1.2: Parser Class Structure

We will create a Parser class that encapsulates the parsing logic.

    Input: The constructor will accept the list of Tokens from the tokenizer.

    State: It will maintain internal state, including:

        The list of tokens.

        A cursor or index pointing to the current token being processed.

        A list to collect syntax errors (diagnostics) as they are found.

    Core Methods: It will include helper methods like:

        peek(): Look at the current token without consuming it.

        advance(): Consume the current token and move the cursor to the next one.

        match(...types): Check if the current token matches any of the given TokenTypes. If so, consume it and return true; otherwise, return false.

        consume(type, message): Expect the current token to be of a specific TokenType. If it is, consume it. If not, throw or record a syntax error with the given message.

    Main Entry Point: A public method, e.g., parse(), will initiate the parsing process (starting with the top-level EBNF rule, Program) and return the completed AST.

Chapter 2.2: Abstract Syntax Tree (AST) Node Design

Developer Context: The AST is the most crucial data structure we will build. It's a tree-like representation of the code's structure. Each "node" in the tree represents a construct in the code, like a statement, an expression, or a declaration. This tree is what our formatter will use to understand the code's logic and apply styling rules correctly.
Sub-section 2.2.1: Base AST Node Interface

All AST nodes will share a common set of properties. We will define a base interface for this.

// All nodes in our tree will have a type and location.
interface ASTNode {
    kind: ASTNodeType; // An enum to identify the type of node.
    // Location information derived from the start/end tokens of this construct.
    startToken: Token;
    endToken: Token;
}

// An enum to list all possible kinds of AST nodes.
enum ASTNodeType {
    Program,
    ClassDefinition,
    ProcedureStatement,
    IfStatement,
    AssignmentExpression,
    BinaryExpression, // For things like 'a + b'
    UnaryExpression,  // For things like '-a' or '!b'
    FunctionCallExpression,
    VariableExpression,
    LiteralExpression,
    // ... and so on for every distinct grammar construct.
}

Sub-section 2.2.2: Specific AST Node Interfaces

For each significant non-terminal rule in the EBNF, we will define a specific interface that extends the ASTNode base. This provides a strongly-typed structure for each language construct.

Example Node Definitions (Conceptual TypeScript):

    For ProcedureStatement:

    interface ProcedureStatementNode extends ASTNode {
        kind: ASTNodeType.ProcedureStatement;
        name: Token; // The IDENTIFIER token for the procedure's name.
        parameters: ParameterDeclarationNode | null;
        body: StatementNode[]; // A list of statements inside the procedure.
    }

    For IfStatement:

    interface IfStatementNode extends ASTNode {
        kind: ASTNodeType.IfStatement;
        condition: ExpressionNode;
        thenBranch: StatementNode[]; // Statements in the "true" block.
        elseBranch: StatementNode[] | null; // Statements in the optional "else" block.
    }

    For a binary expression like a + b:

    interface BinaryExpressionNode extends ASTNode {
        kind: ASTNodeType.BinaryExpression;
        left: ExpressionNode;  // The AST for the left-hand side ('a').
        operator: Token;       // The operator token ('+').
        right: ExpressionNode; // The AST for the right-hand side ('b').
    }

Action: Define the ASTNodeType enum and the specific interface for each major non-terminal symbol in the EBNF. This is a design-heavy task that maps the grammar directly to data structures.
Chapter 2.3: Implementing Parsing Logic

This chapter details the creation of the recursive functions that build the AST. Each function will correspond to one or more EBNF rules.
Sub-section 2.3.1: Top-Level and Statement Parsing

    parseProgram(): The main entry point. It will implement the Program ::= ClassDefinition | {Statement} rule. It will loop, calling parseStatement() until it reaches the EOF token, collecting statements into a ProgramNode.

    parseStatement(): This will be a large function that determines which specific kind of statement is next based on the current token. It will look at tokens like KEYWORD_PROCEDURE, KEYWORD_IF, KEYWORD_WHILE, KEYWORD_DECLARE, or an IDENTIFIER (which could start an assignment or function call) and delegate to the appropriate parsing function.

    Specific Statement Parsers:

        parseProcedureStatement(): Implements ProcedureStatement ::= ProcedureStart [ParameterDeclaration] ... ProcedureEnd. It will consume :PROCEDURE, an Identifier, optionally parse parameters, parse the body statements until :ENDPROC, and construct a ProcedureStatementNode.

        parseConditionalStatement(): Implements :IF ... [:ELSE] ... :ENDIF. It will construct an IfStatementNode.

        parseLoopStatement(): Implements :WHILE ... :ENDWHILE and :FOR ... :NEXT loops, constructing corresponding WhileLoopNode or ForLoopNode.

        parseDeclarationStatement(): Handles :DECLARE, :PARAMETERS, :PUBLIC, etc.

Sub-section 2.3.2: Expression Parsing and Operator Precedence

Developer Context: Parsing expressions like a + b * c correctly is critical. The parser must understand that multiplication has higher "precedence" than addition, so it should be evaluated as a + (b * c). The EBNF grammar structure already defines this hierarchy. We will implement functions that mirror this structure.

    The EBNF defines the precedence hierarchy:
    Expression (lowest precedence) -> LogicalExpression -> ComparisonExpression -> ArithmeticExpression -> Term -> Factor -> PowerOperand -> Primary (highest precedence).

    Implementation Strategy: We will create a function for each level of this hierarchy.

        parseExpression() will call parseLogicalExpression().

        parseLogicalExpression() will parse ComparisonExpressions separated by .AND. or .OR. operators.

        parseComparisonExpression() will parse ArithmeticExpressions separated by ==, >, <, etc.

        parseArithmeticExpression() will parse Terms separated by + or -.

        parseTerm() will parse Factors separated by * or /.

        ...and so on, down to parsePrimary().

    This structure naturally handles operator precedence. For example, parseArithmeticExpression will greedily consume Terms (b * c), effectively grouping them before the addition is considered.

Sub-section 2.3.3: Primary Expression and List Parsing

    parsePrimary(): This function handles the building blocks of expressions. Based on the current token, it will:

        Parse literals (numbers, strings, .T., .F., NIL) and create LiteralNodes.

        Parse Identifiers, which could be a VariableExpressionNode or the start of a FunctionCallExpressionNode or PropertyAccess.

        Handle parenthesized expressions by consuming (, calling parseExpression() recursively, and then consuming ).

        Parse function calls (FunctionCall ::= Identifier "(" [ArgumentList] ")"), constructing a FunctionCallNode that holds the function name and a list of argument expressions.

    parseArgumentList(): A helper function to parse a comma-separated list of expressions inside a function call.

Chapter 2.4: Parser Error Handling and Recovery
Sub-section 2.4.1: Syntax Error Detection

    A syntax error occurs when the parser encounters a token that is not valid according to the current grammar rule (e.g., expecting a semicolon ; to end a statement but finding a keyword instead).

    The consume() helper method is the primary mechanism for this. If the current token doesn't match what is expected, a syntax error is generated.

Sub-section 2.4.2: Error Reporting

    When a syntax error is detected, the parser will create a diagnostic object containing:

        A clear error message (e.g., "Expected ';' after statement.").

        The location of the error, using the line and column info from the unexpected token.

    These diagnostics will be stored in the parser's list of errors.

Sub-section 2.4.3: Basic Error Recovery (Synchronization)

Purpose: To allow the parser to continue and find more than one error in a file, rather than stopping at the first one.

    We will implement a simple "panic mode" recovery strategy. When a syntax error occurs, the parser enters panic mode.

    It will then advance tokens, discarding them, until it finds a token that is likely to mark the beginning of a new, valid construct (a "synchronization point").

    Good synchronization points in SSL are tokens like the start of the next statement (e.g., :IF, :WHILE, :DECLARE) or the statement-terminating semicolon ;.

    After synchronizing, the parser exits panic mode and attempts to continue parsing normally. This prevents a single error from causing a cascade of dozens of follow-on errors.

Chapter 2.5: Parser Output and Integration
Sub-section 2.5.1: Final Output

The parse() method of the Parser class will return an object containing:

    The root ProgramNode of the completed AST.

    A list of all diagnostics (both lexical errors from the tokenizer and syntax errors from the parser).

This combined output provides everything needed for the subsequent formatting and error-reporting stages.
Chapter 2.6: Parser Testing Strategy
Sub-section 2.6.1: Unit Tests for Grammar Rules

    For each major parsing function (e.g., parseProcedureStatement, parseConditionalStatement, parseExpression), write specific unit tests.

    Provide a token stream for a small, valid snippet and assert that the function returns the correctly structured AST node.

Sub-section 2.6.2: Integration Tests with Valid Code

    Create a suite of tests using complete, valid SSL files.

    Run the full tokenizer and parser pipeline.

    Assert that the final AST is structured as expected and that the list of diagnostics is empty.

    This can be done effectively using "snapshot testing," where the generated AST for a given input is saved, and subsequent test runs fail if the AST structure changes unexpectedly.

Sub-section 2.6.3: Error Condition and Recovery Tests

    Create a suite of tests using SSL code that is intentionally malformed.

    Error Detection: For each invalid snippet, assert that the parser produces the expected error message at the correct location.

    Error Recovery: For files with multiple errors, assert that the parser identifies all of them (or a reasonable number of them), proving that the synchronization strategy is working.

This concludes the detailed plan for Part 2: EBNF-Compliant Parser & Abstract Syntax Tree (AST).