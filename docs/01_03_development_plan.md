# Project Plan: SSL Formatter - Tier 1 Core Foundation

This document details Parts 3, 4, and 5 of the Tier 1 plan, focusing on building the formatting engine, configuring the language support, and reporting diagnostics to the user.

## Part 3: Essential Formatting Engine (AST-Based)

**Purpose:** To traverse the valid Abstract Syntax Tree (AST) produced by the parser and reconstruct the SSL code as a perfectly formatted string. This engine will apply a consistent set of styling rules based on the code's structure.

### Chapter 3.1: Formatter Strategy and Architecture

#### Sub-section 3.1.1: Choosing an AST Traversal Strategy

We will implement the **Visitor Pattern** to traverse the AST.

**What is the Visitor Pattern?**
It's a design pattern that allows us to add new operations to existing object structures (our AST) without modifying those structures. We will create a `FormatterVisitor` class. This class will have a "visit" method for each specific type of AST node (e.g., `visitProcedureStatementNode`, `visitIfStatementNode`, `visitBinaryExpressionNode`).

**Why use the Visitor Pattern?**

-   **Separation of Concerns:** The formatting logic is completely separate from the AST node data structures. The AST nodes just hold data; the visitor knows how to format it.

-   **Organization:** Logic for formatting a specific language construct (like an IfStatement) is neatly contained within a single method (`visitIfStatementNode`).

-   **Extensibility:** If we later want to perform another operation on the AST (like collecting symbols for code navigation), we can write a new visitor (e.g., `SymbolCollectorVisitor`) without touching the formatter or the AST nodes.

#### Sub-section 3.1.2: Formatter State Management

The `FormatterVisitor` will need to manage state as it traverses the tree to make correct formatting decisions.

**Core State Attributes:**

-   **output (string builder):** An efficient mechanism to build the final formatted code string.

-   **indentationLevel (number):** A counter that tracks the current indentation depth. It will be incremented when entering a block and decremented when exiting.

-   **options (object):** A configuration object for formatting rules (e.g., `indentSize: 4`, `useTabs: false`, `maxLineLength: 90`). This allows for future user-configurable formatting.

### Chapter 3.2: Implementation of Core Formatting Features

This chapter details how the `FormatterVisitor` will handle each formatting task during its traversal of the AST.

#### Sub-section 3.2.1: Syntax Highlighting (Validation Step)

-   **Role of the Formatter:** While VS Code handles the actual rendering of syntax highlighting via the TextMate grammar, this is a good point to ensure our tooling is consistent.

-   **Action:** Review the existing `ssl.tmLanguage.json` file. Compare its defined patterns and scopes against the token types produced by our new Tokenizer (Part 1). Ensure that all keywords, operators, literals, and comment types are correctly scoped so they receive the proper highlighting from the user's theme. This is a validation task, not a code generation one.

#### Sub-section 3.2.2: Indentation Management**Logic:** The `FormatterVisitor` will manage the `indentationLevel`.

**Implementation:**

-   When the visitor's visit method enters a node that represents a code block (e.g., `ProcedureStatementNode`, the `thenBranch` of an `IfStatementNode`), it will increment `indentationLevel`.

-   Before writing a new line to the output string builder, it will first append the appropriate indentation (e.g., `indentationLevel * options.indentSize` spaces).

-   When the visitor finishes with that block, it will decrement `indentationLevel`.

**Example (Conceptual `visitIfStatementNode`):**

```javascript
visitIfStatementNode(node: IfStatementNode) {
    this.output.append(":IF ");
    this.visit(node.condition); // Format the condition expression
    this.output.append(";\n");

    this.indentationLevel++;
    node.thenBranch.forEach(statement => this.visit(statement)); // Format statements in the "then" block
    this.indentationLevel--;

    if (node.elseBranch) {
        this.output.appendWithIndentation(":ELSE;\n");
        this.indentationLevel++;
        node.elseBranch.forEach(statement => this.visit(statement));
        this.indentationLevel--;
    }

    this.output.appendWithIndentation(":ENDIF;\n");
}
```

#### Sub-section 3.2.3: Block Alignment**Logic:** Proper block alignment is a natural result of correct indentation management.

**Implementation:** By ensuring that block-closing keywords (like `:ENDIF`, `:ENDWHILE`) are written to the output after the `indentationLevel` has been decremented, they will automatically align with their corresponding opening keywords.

#### Sub-section 3.2.4: Spacing Management**Logic:** The visitor will enforce consistent spacing around operators, commas, and other tokens during code reconstruction.

**Implementation:**

-   **Binary Operators (`visitBinaryExpressionNode`):** When visiting a `BinaryExpressionNode`, the visitor will format the left side, append a single space, append the operator's lexeme, append another single space, and then format the right side.

    **Example:** `this.visit(node.left); this.output.append(' ${node.operator.lexeme} '); this.visit(node.right);`

-   **Commas:** When visiting a list construct (e.g., `ParameterList`, `ArgumentList`), the visitor will append a comma followed by a single space after each element except the last one. It will pay attention to special SSL rules like `param1,,param3` for skipped parameters.

-   **Blank Lines:** Rules for blank lines will be implemented in the visitor. For example, the `visitProcedureStatementNode` might always write two newlines to the output after it's done, ensuring a blank line between procedures.

#### Sub-section 3.2.5: Comment Handling**Challenge:** Comments are often not part of the formal AST structure because they can appear almost anywhere. A common approach is to associate comments with nearby AST nodes during or after parsing.

**Implementation Strategy:**

-   **Comment Association:** Enhance the Parser (Part 2) or add a post-processing step to attach comment tokens to the "closest" or "owning" AST node (e.g., a comment on the line before a statement belongs to that statement node).

-   **Formatting:** The `FormatterVisitor`, when visiting any node, will first check if it has any associated comments.

    -   It will format and write any "leading" comments before formatting the node itself, preserving their relative line breaks.

    -   It will format and write any "trailing" (end-of-line) comments after the node.

    -   Indentation rules will be applied to the comments to align them with the surrounding code.

#### Sub-section 3.2.6: Data Provisioning for Code Folding & Bracket Matching**Logic:** The formatter itself doesn't perform live folding or bracket matching in the editor. Instead, the language server provides this information to VS Code. The AST is the source of truth for this data.

**Implementation:**

-   **Code Folding:** A separate provider (e.g., `FoldingRangeProvider`) will be created. It will run the parser to get the AST for a document. It will then traverse the AST and, for each node that represents a foldable block (e.g., `ProcedureStatementNode`, `IfStatementNode`), it will create a `FoldingRange` object using the start and end line numbers from the node's stored tokens.

-   **Bracket/Block Matching:** Similarly, a provider will use the AST to identify matching keyword pairs (e.g., `:IF` and `:ENDIF`) and provide their locations to VS Code for highlighting. The parent-child relationships in the AST make it trivial to find which `:ENDIF` belongs to which `:IF`.

### Chapter 3.3: Formatter Testing Strategy

#### Sub-section 3.3.1: Snapshot Testing**Strategy:** For a wide variety of valid SSL code snippets, run the full pipeline (tokenizer → parser → formatter). The formatted output string will be saved as a "snapshot" file.

**Execution:** On subsequent test runs, the new output is compared to the saved snapshot. The test fails if there are any differences. This is extremely effective for catching unintended formatting changes.

#### Sub-section 3.3.2: Idempotency Testing

**What is Idempotency?** Formatting an already-formatted file should produce no changes.

**Strategy:** Create a test that takes a file, formats it once to get `output1`, then formats `output1` again to get `output2`. The test will assert that `output1` is identical to `output2`.

#### Sub-section 3.3.3: Logic Preservation Testing

**Strategy:** While harder to automate perfectly, the goal is to ensure formatting doesn't change the code's logic. Snapshot testing is the primary defense here. Manual review of formatted complex files is also crucial. Ensure all non-whitespace tokens and comments are present in the output.

## Part 4: Basic Language Configuration

**Purpose:** To configure VS Code's built-in editor behaviors to align with SSL syntax and conventions, providing a smoother user experience.

### Chapter 4.1: Reviewing and Updating language-configuration.json

**Action:** We will create and refine the `language-configuration.json` file for the extension.

#### Sub-section 4.1.1: Comment Toggling

Define the editor's behavior for comment toggling (the Cmd+/ or Ctrl+/ shortcut).

**Configuration:**

```json
"comments": { "blockComment": [ "/*", " ;" ] }
```

This tells VS Code to wrap a selected block of text with `/*` and ` ;` when the user triggers the "Add Block Comment" command.

#### Sub-section 4.1.2: Bracket and Brace Matching

Define the standard pairs of brackets that VS Code should highlight and match.

**Configuration:**

```json
"brackets": [ ["{", "}"], ["[", "]"], ["(", ")"] ]
```

#### Sub-section 4.1.3: Auto-Closing Pairs

Configure pairs of characters where typing the opening character should automatically insert the closing one.

**Configuration:**

```json
"autoClosingPairs": [
    { "open": "{", "close": "}" },
    { "open": "[", "close": "]" },
    { "open": "(", "close": ")" },
    { "open": "\"", "close": "\"" },
    { "open": "'", "close": "'" }
]
```

#### Sub-section 4.1.4: Surrounding Pairs

Configure pairs for surrounding a selected piece of text.

**Configuration:**

```json
"surroundingPairs": [
    ["{", "}"],
    ["[", "]"],
    ["(", ")"],
    ["\"", "\""],
    ["'", "'"]
]
```

## Part 5: Basic Diagnostic Reporting

**Purpose:** To take the lexical and syntax errors identified in Parts 1 and 2 and display them to the user as diagnostics (e.g., squiggly lines, entries in the "Problems" panel) within the VS Code editor.

### Chapter 5.1: Consolidating Diagnostic Information

**Logic:** The main extension logic that orchestrates the process will be responsible for this.

**Implementation:**

1. When a document is opened or changed, trigger a re-parse.

2. Run the Tokenizer (Part 1) on the document text. It returns a list of tokens and a list of lexical errors.

3. Run the Parser (Part 2) on the token list. It returns an AST and a list of syntax errors.

4. Combine the list of lexical errors and the list of syntax errors into a single, consolidated list of diagnostics.

### Chapter 5.2: Publishing Diagnostics to VS Code

**Logic:** We will use the official VS Code API to publish the collected diagnostics.

**Implementation:**

1. Create a `DiagnosticCollection` for our extension using `vscode.languages.createDiagnosticCollection("ssl")`.

2. For each error in our consolidated list from Chapter 5.1, create a `vscode.Diagnostic` object. This object requires a `vscode.Range` (specifying the location of the error in the document) and the error message.

3. Set the `DiagnosticCollection` for the document's URI with the array of `vscode.Diagnostic` objects. VS Code will automatically handle displaying them.

4. Ensure that the diagnostics are cleared when the document is closed or when a re-parse results in zero errors.
