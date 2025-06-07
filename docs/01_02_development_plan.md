# Part 2: EBNF-Compliant Parser & Abstract Syntax Tree (AST)

**Purpose:** To consume the token stream from the tokenizer, validate its structure against the EBNF grammar rules, and build an Abstract Syntax Tree (AST) that represents the hierarchical structure of the SSL code. This phase also includes identifying and packaging syntax errors for later reporting.

## Chapter 2.1: Parser Strategy and Setup

### Sub-section 2.1.1: Choosing a Parsing Strategy

We will implement a **Recursive Descent Parser**. This top-down parsing strategy is highly suitable for hand-writing a parser from an EBNF grammar. The structure of the parser code will closely mirror the structure of the grammar rules, making it intuitive to develop and debug.

**Key Characteristics:**

*   A set of mutually recursive functions, where each function corresponds to a non-terminal symbol in the grammar (e.g., a `parseStatement()` function, a `parseExpression()` function).
*   The parser maintains its state by advancing a cursor through the token stream produced by the tokenizer (Part 1).

### Sub-section 2.1.2: Parser Class Structure

We will create a `Parser` class that encapsulates the parsing logic.

*   **Input:** The constructor will accept the list of `Tokens` from the tokenizer.
*   **State:** It will maintain internal state, including:
    *   The list of tokens.
    *   A cursor or index pointing to the current token being processed.
    *   A list to collect syntax errors (diagnostics) as they are found.
*   **Core Methods:** It will include helper methods like:
    *   `peek()`: Look at the current token without consuming it.
    *   `advance()`: Consume the current token and move the cursor to the next one.
    *   `match(...types)`: Check if the current token matches any of the given `TokenTypes`. If so, consume it and return `true`; otherwise, return `false`.
    *   `consume(type, message)`: Expect the current token to be of a specific `TokenType`. If it is, consume it. If not, throw or record a syntax error with the given message.
*   **Main Entry Point:** A public method, e.g., `parse()`, will initiate the parsing process (starting with the top-level EBNF rule, `Program`) and return the completed AST.

## Chapter 2.2: Abstract Syntax Tree (AST) Node Design

> **Developer Context:** The AST is the most crucial data structure we will build. It's a tree-like representation of the code's structure. Each "node" in the tree represents a construct in the code, like a statement, an expression, or a declaration. This tree is what our formatter will use to understand the code's logic and apply styling rules correctly.

### Sub-section 2.2.1: Base AST Node Interface

All AST nodes will share a common set of properties. We will define a base interface for this.

```typescript
// All nodes in our tree will have a type and location.
interface ASTNode {
    kind: ASTNodeType; // An enum to identify the type of node.
    // Location information derived from the start/end tokens of this construct.
    startToken: Token;
    endToken: Token;
}
```

```typescript
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
```

### Sub-section 2.2.2: Specific AST Node Interfaces

For each significant non-terminal rule in the EBNF, we will define a specific interface that extends the `ASTNode` base. This provides a strongly-typed structure for each language construct.

**Example Node Definitions (Conceptual TypeScript):**

*   For `ProcedureStatement`:

    ```typescript
    interface ProcedureStatementNode extends ASTNode {
        kind: ASTNodeType.ProcedureStatement;
        name: Token; // The IDENTIFIER token for the procedure's name.
        parameters: ParameterDeclarationNode | null;
        body: StatementNode[]; // A list of statements inside the procedure.
    }
    ```

*   For `IfStatement`:

    ```typescript
    interface IfStatementNode extends ASTNode {
        kind: ASTNodeType.IfStatement;
        condition: ExpressionNode;
        thenBranch: StatementNode[]; // Statements in the "true" block.
        elseBranch: StatementNode[] | null; // Statements in the optional "else" block.
    }
    ```

*   For a binary expression like `a + b`:

    ```typescript
    interface BinaryExpressionNode extends ASTNode {
        kind: ASTNodeType.BinaryExpression;
        left: ExpressionNode;  // The AST for the left-hand side ('a').
        operator: Token;       // The operator token ('+').
        right: ExpressionNode; // The AST for the right-hand side ('b').
    }
    ```

**Action:** Define the `ASTNodeType` enum and the specific interface for each major non-terminal symbol in the EBNF. This is a design-heavy task that maps the grammar directly to data structures.

## Chapter 2.3: Implementing Parsing Logic

This chapter details the creation of the recursive functions that build the AST. Each function will correspond to one or more EBNF rules.

### Sub-section 2.3.1: Top-Level and Statement Parsing

*   `parseProgram()`: The main entry point. It will implement the `Program ::= ClassDefinition | {Statement}` rule. It will loop, calling `parseStatement()` until it reaches the `EOF` token, collecting statements into a `ProgramNode`.
*   `parseStatement()`: This will be a large function that determines which specific kind of statement is next based on the current token. It will look at tokens like `KEYWORD_PROCEDURE`, `KEYWORD_IF`, `KEYWORD_WHILE`, `KEYWORD_DECLARE`, or an `IDENTIFIER` (which could start an assignment or function call) and delegate to the appropriate parsing function.
*   **Specific Statement Parsers:**
    *   `parseProcedureStatement()`: Implements `ProcedureStatement ::= ProcedureStart [ParameterDeclaration] ... ProcedureEnd`. It will consume `:PROCEDURE`, an `Identifier`, optionally parse parameters, parse the body statements until `:ENDPROC`, and construct a `ProcedureStatementNode`.
    *   `parseConditionalStatement()`: Implements `:IF ... [:ELSE] ... :ENDIF`. It will construct an `IfStatementNode`.
    *   `parseLoopStatement()`: Implements `:WHILE ... :ENDWHILE` and `:FOR ... :NEXT` loops, constructing corresponding `WhileLoopNode` or `ForLoopNode`.
    *   `parseDeclarationStatement()`: Handles `:DECLARE`, `:PARAMETERS`, `:PUBLIC`, etc.

### Sub-section 2.3.2: Expression Parsing and Operator Precedence

> **Developer Context:** Parsing expressions like `a + b * c` correctly is critical. The parser must understand that multiplication has higher "precedence" than addition, so it should be evaluated as `a + (b * c)`. The EBNF grammar structure already defines this hierarchy. We will implement functions that mirror this structure.

*   The EBNF defines the precedence hierarchy: `Expression` (lowest precedence) -> `LogicalExpression` -> `ComparisonExpression` -> `ArithmeticExpression` -> `Term` -> `Factor` -> `PowerOperand` -> `Primary` (highest precedence).
*   **Implementation Strategy:** We will create a function for each level of this hierarchy.
    *   `parseExpression()` will call `parseLogicalExpression()`.
    *   `parseLogicalExpression()` will parse `ComparisonExpressions` separated by `.AND.` or `.OR.` operators.
    *   `parseComparisonExpression()` will parse `ArithmeticExpressions` separated by `==`, `>`, `<`, etc.
    *   `parseArithmeticExpression()` will parse `Terms` separated by `+` or `-`.
    *   `parseTerm()` will parse `Factors` separated by `*` or `/`.
    *   ...and so on, down to `parsePrimary()`.
*   This structure naturally handles operator precedence. For example, `parseArithmeticExpression` will greedily consume `Terms` (`b * c`), effectively grouping them before the addition is considered.

### Sub-section 2.3.3: Primary Expression and List Parsing

*   `parsePrimary()`: This function handles the building blocks of expressions. Based on the current token, it will:
    *   Parse literals (numbers, strings, `.T.`, `.F.`, `NIL`) and create `LiteralNodes`.
    *   Parse `Identifiers`, which could be a `VariableExpressionNode` or the start of a `FunctionCallExpressionNode` or `PropertyAccess`.
    *   Handle parenthesized expressions by consuming `(`, calling `parseExpression()` recursively, and then consuming `)`.
    *   Parse function calls (`FunctionCall ::= Identifier "(" [ArgumentList] ")"`), constructing a `FunctionCallNode` that holds the function name and a list of argument expressions.
*   `parseArgumentList()`: A helper function to parse a comma-separated list of expressions inside a function call.

## Chapter 2.4: Parser Error Handling and Recovery

### Sub-section 2.4.1: Syntax Error Detection

*   A syntax error occurs when the parser encounters a token that is not valid according to the current grammar rule (e.g., expecting a semicolon `;` to end a statement but finding a keyword instead).
*   The `consume()` helper method is the primary mechanism for this. If the current token doesn't match what is expected, a syntax error is generated.

### Sub-section 2.4.2: Error Reporting

*   When a syntax error is detected, the parser will create a diagnostic object containing:
    *   A clear error message (e.g., "Expected ';' after statement.").
    *   The location of the error, using the line and column info from the unexpected token.
*   These diagnostics will be stored in the parser's list of errors.

### Sub-section 2.4.3: Basic Error Recovery (Synchronization)

**Purpose:** To allow the parser to continue and find more than one error in a file, rather than stopping at the first one.

*   We will implement a simple **"panic mode"** recovery strategy. When a syntax error occurs, the parser enters panic mode.
*   It will then advance tokens, discarding them, until it finds a token that is likely to mark the beginning of a new, valid construct (a "synchronization point").
*   Good synchronization points in SSL are tokens like the start of the next statement (e.g., `:IF`, `:WHILE`, `:DECLARE`) or the statement-terminating semicolon `;`.
*   After synchronizing, the parser exits panic mode and attempts to continue parsing normally. This prevents a single error from causing a cascade of dozens of follow-on errors.

## Chapter 2.5: Parser Output and Integration

### Sub-section 2.5.1: Final Output

The `parse()` method of the `Parser` class will return an object containing:

*   The root `ProgramNode` of the completed AST.
*   A list of all diagnostics (both lexical errors from the tokenizer and syntax errors from the parser).

This combined output provides everything needed for the subsequent formatting and error-reporting stages.

## Chapter 2.6: Parser Testing Strategy

### Sub-section 2.6.1: Unit Tests for Grammar Rules

*   For each major parsing function (e.g., `parseProcedureStatement`, `parseConditionalStatement`, `parseExpression`), write specific unit tests.
*   Provide a token stream for a small, valid snippet and assert that the function returns the correctly structured AST node.

### Sub-section 2.6.2: Integration Tests with Valid Code

*   Create a suite of tests using complete, valid SSL files.
*   Run the full tokenizer and parser pipeline.
*   Assert that the final AST is structured as expected and that the list of diagnostics is empty.
*   This can be done effectively using "snapshot testing," where the generated AST for a given input is saved, and subsequent test runs fail if the AST structure changes unexpectedly.

### Sub-section 2.6.3: Error Condition and Recovery Tests

*   Create a suite of tests using SSL code that is intentionally malformed.
*   **Error Detection:** For each invalid snippet, assert that the parser produces the expected error message at the correct location.
*   **Error Recovery:** For files with multiple errors, assert that the parser identifies all of them (or a reasonable number of them), proving that the synchronization strategy is working.

This concludes the detailed plan for Part 2: EBNF-Compliant Parser & Abstract Syntax Tree (AST).