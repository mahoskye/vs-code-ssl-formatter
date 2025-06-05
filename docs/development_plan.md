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
-   Comments containing characters that look like other tokens (e.g., `/* :IF x := 10; */`)

#### Section 1.6.3: Leveraging AI for Test Case Generation

-   Use AI to generate diverse SSL code snippets based on the EBNF's lexical rules to broaden test coverage
-   Prompt AI with specific EBNF rules for terminals (keywords, operators, literal patterns) and ask for:
    -   A variety of valid examples
    -   Examples that are intentionally lexically incorrect to test error detection (e.g., invalid characters, unterminated constructs)

---

This concludes the detailed plan for **Part 1: EBNF-Compliant Tokenizer**. We can proceed to detail Part 2 when you're ready.
