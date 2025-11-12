# SSL VS Code Plugin - Complete Specification & Build Guide

## Project Overview

A comprehensive Visual Studio Code extension providing language support for STARLIMS Scripting Language (SSL). This plugin addresses the limitations of the integrated STARLIMS IDE by enabling developers to work with SSL code in VS Code with full IDE features including syntax highlighting, code intelligence, formatting, and diagnostics.

### Supported File Extensions
- `.ssl`
- `.srvscr`
- `.ds`
- `.ds.txt`
- `.ssl.txt`

---

## Complete Feature Scope

### 1. Syntax & Highlighting

#### Keywords, Operators, Literals
Color-codes all SSL language constructs (`:PROCEDURE`, `:IF`, `:=`, `.AND.`, etc.) to make code structure immediately visible. Uses the provided EBNF grammar to ensure accurate, case-insensitive matching of all SSL syntax.

#### Strings, Comments, Numbers
Highlights string literals (both `"` and `'`), comments (`/* ... ;`), and numeric values including scientific notation. Makes it easy to distinguish data from code at a glance.

#### Multi-file Extension Support
Automatically recognizes and applies SSL syntax highlighting to `.ssl`, `.srvscr`, `.ds`, `.ds.txt`, and `.ssl.txt` files. No manual language selection needed.

### 2. Editor Features

#### Bracket/Brace Matching & Auto-closing
Highlights matching pairs of `()`, `{}`, `[]` when cursor is near them, and automatically inserts closing brackets when you type opening ones. Prevents unbalanced bracket errors and speeds up typing.

#### Comment Toggling
Press `Ctrl+/` (or `Cmd+/` on Mac) to instantly add or remove `/* ... ;` comments from selected lines. Essential for quickly commenting out code during debugging.

#### Code Folding
Click arrows in the gutter to collapse/expand procedures, loops, conditionals, and regions, letting you hide implementation details and focus on relevant sections. Particularly useful for navigating large files.

#### Smart Indentation
Automatically indents new lines based on SSL block structure (`:PROCEDURE`, `:IF`, etc.) and unindents when you type closing keywords. Maintains consistent formatting without manual spacing.

### 3. Code Formatting

#### Auto-format SSL Code
Command to instantly reformat entire files or selections according to consistent style rules. Eliminates manual formatting work and ensures team-wide consistency.

#### Consistent Indentation
Enforces uniform indentation levels (typically 4 spaces) for all block structures. Makes code hierarchy visually clear and easier to scan.

#### Spacing Around Operators
Standardizes spacing around `:=`, `.AND.`, comparison operators, etc. (e.g., `x := 5` not `x:=5`). Improves readability across your codebase.

#### Line Breaking for Long Statements
Intelligently breaks long function calls, parameter lists, and expressions across multiple lines at logical points. Keeps code within readable line lengths (typically ~90 characters).

#### SQL Query Formatting
Formats embedded SQL strings with proper indentation and clause alignment (`SELECT`, `FROM`, `WHERE` on separate lines). Makes complex queries much easier to read and debug.

### 4. Code Intelligence (User's Code Only)

#### Outline View
Shows hierarchical tree of all procedures, variables, and regions in the current file in VS Code's outline panel. Provides quick navigation and overview of file structure without scrolling.

#### Go to Definition
Right-click any procedure or variable name and jump directly to where it's declared/defined in your workspace. Saves time hunting through files and enables rapid code exploration.

#### Find All References
Shows every location where a procedure or variable is used across your entire workspace. Essential for understanding dependencies before making changes or for impact analysis.

#### Hover Tooltips
Hover mouse over any procedure name to see its signature (parameters, return type) in a popup without navigating away. Quick reference that keeps you in flow.

#### Rename Symbol
Right-click a procedure/variable and rename it; all references across all files update automatically. Safe refactoring that prevents breaking code with missed updates.

#### Auto-completion
As you type, suggests procedure names, variables in scope, and SSL keywords with descriptions. Speeds up coding, prevents typos, and helps discover available functionality.

#### Signature Help
When typing a procedure call, shows a popup with parameter names and types as you fill them in (e.g., `MyProc(param1: █, param2: ...)`). Eliminates need to look up parameter order or jump to definitions.

#### Workspace Symbol Search
Press `Ctrl+T` / `Cmd+T` and type any procedure or variable name to instantly jump to it anywhere in your workspace. Fast navigation for large multi-file projects.

#### Breadcrumbs
Navigation bar at the top of the editor showing current file path and location within code structure (e.g., `file.ssl > CalculateTotal > IF block`). Always know where you are, click to jump to any level.

#### Code Lens
Shows inline annotations above procedures like "3 references" or "2 implementations" that you can click to see the list. Quick visibility into how much your code is used without running searches.

#### Call Hierarchy
View a tree showing which procedures call the current one (incoming calls) and which procedures it calls (outgoing calls). Understand code flow and dependencies visually.

### 5. Diagnostics & Quick Fixes

#### Diagnostics

**Mismatched Block Keywords**
Flags `:IF` without corresponding `:ENDIF`, `:PROCEDURE` without `:ENDPROC`, etc. with red squiggles. Catches structural errors before you run code in STARLIMS.

**Missing Semicolons**
Highlights statements that should end with `;` but don't. SSL requires semicolons, so this catches a common syntax error immediately.

**Unclosed Strings/Brackets**
Detects strings missing closing quotes or brackets without pairs. Prevents confusing parse errors that cascade through the file.

**Undefined Variables/Procedures**
Warns when you use a variable or call a procedure that's never declared in your code. Catches typos and forgotten declarations early.

**Basic Syntax Errors**
Identifies invalid token sequences, malformed expressions, and other violations of SSL grammar rules. Provides immediate feedback like a compiler would.

#### Code Actions/Quick Fixes

**Add Missing End Keywords**
Click the lightbulb next to an error and select "Add :ENDIF" or "Add :ENDPROC" to automatically insert the missing closing keyword. Fixes structural issues in one click.

**Declare Undefined Variables**
When you use an undeclared variable, quick fix generates the `:DECLARE variableName;` statement at the appropriate location. Saves manual typing and ensures correct placement.

**Remove Unused Variables**
Lightbulb suggests deleting variables that are declared but never used, keeping your code clean. Helps maintain clarity and reduces clutter.

**Auto-fix Common Syntax Errors**
Offers automatic corrections for issues like incorrect operator syntax, missing semicolons, or malformed expressions. One-click fixes for routine mistakes.

### 6. Testing & Quality Assurance

#### Unit Tests
Isolated tests for core components like the SSL parser, symbol analyzer, formatter, and diagnostic engine. Ensures each component works correctly in isolation with known inputs/outputs and catches regressions when making changes.

#### Integration Tests
Tests for VS Code provider implementations (completion provider, hover provider, definition provider, etc.) to verify they correctly interact with the extension API. Validates that components work together properly and handle VS Code's data structures correctly.

#### End-to-End Tests
Full workflow tests using VS Code's extension testing framework that simulate real user interactions (open file, trigger completion, format document, etc.). Catches issues that only appear in the full integrated environment.

#### Test Fixtures
Collection of sample SSL files covering various language features, edge cases, and real-world code patterns. Provides consistent test data and ensures comprehensive coverage of SSL syntax variations.

#### Grammar Validation Tests
Tests that verify syntax highlighting rules correctly tokenize SSL code, checking that keywords, operators, strings, and comments are properly classified. Ensures the TextMate grammar matches the EBNF specification.

#### Formatter Tests
Tests that verify code formatting produces expected output for various input styles (nested blocks, long lines, SQL queries, etc.). Ensures consistent formatting behavior and prevents regressions.

#### Diagnostics Tests
Tests that verify error detection correctly identifies problems (mismatched blocks, undefined symbols, syntax errors) and doesn't produce false positives. Validates that quick fixes generate correct code.

#### CI/CD Integration
Automated test execution on every commit/pull request using GitHub Actions or similar, with coverage reporting and test result tracking. Prevents broken code from being merged and maintains quality standards over time.

### Explicitly Excluded (For Now)

- ❌ **Built-in STARLIMS function knowledge** - No IntelliSense for STARLIMS API functions like `DatabaseFunction()`, avoiding confidentiality concerns. User's code only for now.
- ❌ **User-provided signature files** - No support for loading external function definition files yet, though the architecture will allow adding this later.
- ❌ **Code snippets** - No templates for common code patterns (procedure boilerplate, loop structures, etc.).
- ❌ **Semantic highlighting** - No advanced coloring based on variable scope or type (parameters vs locals vs globals).
- ❌ **Inlay hints** - No inline parameter name annotations in function calls.
- ❌ **Advanced refactoring** - No "Extract Procedure" or "Change Signature" operations beyond basic rename.

---

## Technical Architecture

### Recommended Approach: Language Server Protocol (LSP)

For optimal performance and portability, implement this extension using the Language Server Protocol architecture:

```
┌─────────────────────────────────────┐
│  VS Code Extension (Client)         │
│  - Activates language server         │
│  - Registers language configuration  │
│  - Provides UI integration           │
└──────────────┬──────────────────────┘
               │ LSP Communication
┌──────────────▼──────────────────────┐
│  Language Server                     │
│  - SSL Parser                        │
│  - Symbol Indexer                    │
│  - Diagnostic Engine                 │
│  - Formatter                         │
│  - All Language Intelligence         │
└─────────────────────────────────────┘
```

**Benefits:**
- Separates language logic from VS Code specifics
- Better performance (separate process)
- Reusable for other editors (Vim, Emacs, etc.)
- Easier to test in isolation
- Industry standard approach

### Project Structure

```
ssl-vscode-extension/
├── client/                          # VS Code extension (client)
│   ├── src/
│   │   └── extension.ts            # Extension entry point
│   ├── package.json                # Client dependencies
│   └── tsconfig.json               # TypeScript config
│
├── server/                          # Language server
│   ├── src/
│   │   ├── server.ts               # LSP server entry point
│   │   ├── parser/
│   │   │   ├── sslParser.ts        # SSL parser (from EBNF)
│   │   │   ├── lexer.ts            # Tokenizer
│   │   │   └── ast.ts              # Abstract Syntax Tree definitions
│   │   ├── analysis/
│   │   │   ├── symbolTable.ts      # Symbol index and scoping
│   │   │   ├── diagnostics.ts      # Error detection
│   │   │   └── references.ts       # Find references logic
│   │   ├── formatting/
│   │   │   └── formatter.ts        # Code formatting engine
│   │   ├── providers/
│   │   │   ├── completionProvider.ts
│   │   │   ├── hoverProvider.ts
│   │   │   ├── definitionProvider.ts
│   │   │   ├── referencesProvider.ts
│   │   │   ├── renameProvider.ts
│   │   │   ├── documentSymbolProvider.ts
│   │   │   ├── workspaceSymbolProvider.ts
│   │   │   ├── signatureHelpProvider.ts
│   │   │   ├── codeLensProvider.ts
│   │   │   ├── codeActionProvider.ts
│   │   │   └── callHierarchyProvider.ts
│   │   └── utils/
│   │       ├── sslUtils.ts         # SSL-specific utilities
│   │       └── logger.ts           # Logging utilities
│   ├── package.json
│   └── tsconfig.json
│
├── syntaxes/
│   └── ssl.tmLanguage.json         # TextMate grammar
│
├── language-configuration/
│   └── ssl-language-configuration.json  # Brackets, comments, folding
│
├── test/
│   ├── fixtures/                   # Test SSL files
│   │   ├── basic.ssl
│   │   ├── procedures.ssl
│   │   ├── errors.ssl
│   │   └── ...
│   ├── unit/                       # Unit tests
│   │   ├── parser.test.ts
│   │   ├── symbolTable.test.ts
│   │   ├── formatter.test.ts
│   │   └── diagnostics.test.ts
│   ├── integration/                # Integration tests
│   │   ├── completion.test.ts
│   │   ├── hover.test.ts
│   │   └── ...
│   └── e2e/                        # End-to-end tests
│       └── extension.test.ts
│
├── .github/
│   └── workflows/
│       └── ci.yml                  # CI/CD configuration
│
├── package.json                    # Root package.json
├── tsconfig.json                   # Root TypeScript config
├── .vscodeignore                   # Files to exclude from package
├── .eslintrc.json                  # ESLint configuration
├── .prettierrc                     # Prettier configuration
├── README.md                       # User documentation
├── CHANGELOG.md                    # Version history
└── LICENSE                         # License file
```

### Key Technologies

- **TypeScript** - Type-safe development
- **vscode-languageserver** - LSP implementation
- **vscode-languageclient** - VS Code client for LSP
- **Mocha** or **Jest** - Testing framework
- **@vscode/test-electron** - VS Code extension testing
- **ESLint** - Code linting
- **Prettier** - Code formatting

---

## Development Setup

### Prerequisites

1. **Node.js** (v16 or higher)
   ```bash
   node --version  # Should be v16+
   ```

2. **npm** (comes with Node.js)
   ```bash
   npm --version
   ```

3. **Visual Studio Code** (latest version)
   ```bash
   code --version
   ```

4. **Git** (for version control)
   ```bash
   git --version
   ```

### Initial Project Setup

1. **Create project directory:**
   ```bash
   mkdir ssl-vscode-extension
   cd ssl-vscode-extension
   ```

2. **Initialize npm project:**
   ```bash
   npm init -y
   ```

3. **Install VS Code extension generator (optional but recommended):**
   ```bash
   npm install -g yo generator-code
   yo code
   # Select "New Language Support" then "New Language Server"
   # This generates the basic structure
   ```

4. **Or manually create the structure:**
   ```bash
   mkdir -p client/src server/src syntaxes language-configuration test/{fixtures,unit,integration,e2e}
   ```

5. **Install dependencies:**

   **Root package.json:**
   ```bash
   npm install --save-dev typescript @types/node @types/vscode @types/mocha mocha
   npm install --save-dev eslint @typescript-eslint/parser @typescript-eslint/eslint-plugin
   npm install --save-dev prettier eslint-config-prettier
   npm install --save-dev @vscode/test-electron
   ```

   **Client dependencies (client/package.json):**
   ```bash
   cd client
   npm install vscode-languageclient
   npm install --save-dev @types/vscode
   cd ..
   ```

   **Server dependencies (server/package.json):**
   ```bash
   cd server
   npm install vscode-languageserver vscode-languageserver-textdocument
   npm install --save-dev @types/node
   cd ..
   ```

6. **Configure TypeScript:**

   Create `tsconfig.json` in root, client, and server directories with appropriate settings.

   **Root tsconfig.json:**
   ```json
   {
     "compilerOptions": {
       "module": "commonjs",
       "target": "ES2020",
       "lib": ["ES2020"],
       "outDir": "out",
       "sourceMap": true,
       "strict": true,
       "esModuleInterop": true,
       "skipLibCheck": true
     },
     "exclude": ["node_modules", ".vscode-test"]
   }
   ```

7. **Create package.json manifest:**

   Update root `package.json` with VS Code extension properties (see Build Instructions section).

---

## Build Instructions

### 1. Configure Extension Manifest (package.json)

Create/update the root `package.json`:

```json
{
  "name": "ssl-language-support",
  "displayName": "STARLIMS Scripting Language (SSL)",
  "description": "Comprehensive language support for STARLIMS Scripting Language",
  "version": "0.1.0",
  "publisher": "your-publisher-name",
  "repository": {
    "type": "git",
    "url": "https://github.com/yourusername/ssl-vscode-extension"
  },
  "engines": {
    "vscode": "^1.75.0"
  },
  "categories": [
    "Programming Languages",
    "Formatters",
    "Linters"
  ],
  "activationEvents": [
    "onLanguage:ssl"
  ],
  "main": "./client/out/extension.js",
  "contributes": {
    "languages": [
      {
        "id": "ssl",
        "aliases": ["SSL", "STARLIMS Scripting Language"],
        "extensions": [".ssl", ".srvscr", ".ds", ".ds.txt", ".ssl.txt"],
        "configuration": "./language-configuration/ssl-language-configuration.json"
      }
    ],
    "grammars": [
      {
        "language": "ssl",
        "scopeName": "source.ssl",
        "path": "./syntaxes/ssl.tmLanguage.json"
      }
    ],
    "configuration": {
      "type": "object",
      "title": "SSL",
      "properties": {
        "ssl.maxNumberOfProblems": {
          "type": "number",
          "default": 100,
          "description": "Controls the maximum number of problems reported"
        },
        "ssl.trace.server": {
          "type": "string",
          "enum": ["off", "messages", "verbose"],
          "default": "off",
          "description": "Traces communication between VS Code and language server"
        },
        "ssl.format.indentSize": {
          "type": "number",
          "default": 4,
          "description": "Number of spaces for indentation"
        }
      }
    }
  },
  "scripts": {
    "vscode:prepublish": "npm run compile",
    "compile": "npm run compile:client && npm run compile:server",
    "compile:client": "cd client && tsc -p ./",
    "compile:server": "cd server && tsc -p ./",
    "watch": "npm run watch:client & npm run watch:server",
    "watch:client": "cd client && tsc -watch -p ./",
    "watch:server": "cd server && tsc -watch -p ./",
    "lint": "eslint client/src server/src --ext .ts",
    "test": "npm run test:unit && npm run test:integration",
    "test:unit": "mocha -r ts-node/register test/unit/**/*.test.ts",
    "test:integration": "node ./out/test/runTest.js",
    "package": "vsce package",
    "publish": "vsce publish"
  },
  "devDependencies": {
    "@types/mocha": "^10.0.0",
    "@types/node": "^18.0.0",
    "@types/vscode": "^1.75.0",
    "@typescript-eslint/eslint-plugin": "^6.0.0",
    "@typescript-eslint/parser": "^6.0.0",
    "@vscode/test-electron": "^2.3.0",
    "eslint": "^8.0.0",
    "eslint-config-prettier": "^9.0.0",
    "mocha": "^10.0.0",
    "prettier": "^3.0.0",
    "ts-node": "^10.0.0",
    "typescript": "^5.0.0"
  }
}
```

### 2. Create TextMate Grammar (syntaxes/ssl.tmLanguage.json)

Use the EBNF grammar to create comprehensive syntax highlighting rules. The grammar should include:

- Keywords (case-insensitive patterns)
- Operators (assignment, comparison, logical, arithmetic)
- Literals (strings, numbers, booleans)
- Comments
- Function/method calls
- Property access

**Key patterns to implement:**
```json
{
  "name": "SSL",
  "scopeName": "source.ssl",
  "patterns": [
    { "include": "#comments" },
    { "include": "#keywords" },
    { "include": "#strings" },
    { "include": "#numbers" },
    { "include": "#constants" },
    { "include": "#operators" },
    { "include": "#function-calls" }
  ],
  "repository": {
    "keywords": {
      "patterns": [
        {
          "name": "keyword.control.ssl",
          "match": "(?i)\\:(PROCEDURE|IF|WHILE|FOR|...)\\b"
        }
      ]
    }
    // ... more patterns
  }
}
```

### 3. Create Language Configuration

Create `language-configuration/ssl-language-configuration.json`:

```json
{
  "comments": {
    "lineComment": "/*",
    "blockComment": ["/*", ";"]
  },
  "brackets": [
    ["{", "}"],
    ["[", "]"],
    ["(", ")"]
  ],
  "autoClosingPairs": [
    { "open": "{", "close": "}" },
    { "open": "[", "close": "]" },
    { "open": "(", "close": ")" },
    { "open": "\"", "close": "\"", "notIn": ["string"] },
    { "open": "'", "close": "'", "notIn": ["string"] }
  ],
  "surroundingPairs": [
    ["{", "}"],
    ["[", "]"],
    ["(", ")"],
    ["\"", "\""],
    ["'", "'"]
  ],
  "folding": {
    "markers": {
      "start": "^\\s*:(PROCEDURE|IF|WHILE|FOR|SWITCH|TRY|ERROR|REGION)\\b",
      "end": "^\\s*:(ENDPROC|ENDIF|ENDWHILE|ENDFOR|ENDSWITCH|ENDTRY|ENDERROR|ENDREGION)\\b"
    }
  },
  "indentationRules": {
    "increaseIndentPattern": "^\\s*:(PROCEDURE|IF|WHILE|FOR|SWITCH|CASE|TRY|CATCH|ERROR|ELSE)\\b",
    "decreaseIndentPattern": "^\\s*:(ENDPROC|ENDIF|ENDWHILE|ENDFOR|ENDSWITCH|ENDTRY|ENDERROR|ELSE|CASE)\\b"
  }
}
```

### 4. Implement Language Server

**server/src/server.ts** (main entry point):

```typescript
import {
  createConnection,
  TextDocuments,
  ProposedFeatures,
  InitializeParams,
  InitializeResult,
  TextDocumentSyncKind
} from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';

// Create connection
const connection = createConnection(ProposedFeatures.all);

// Create document manager
const documents = new TextDocuments(TextDocument);

connection.onInitialize((params: InitializeParams): InitializeResult => {
  return {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      completionProvider: { resolveProvider: true },
      hoverProvider: true,
      definitionProvider: true,
      referencesProvider: true,
      documentSymbolProvider: true,
      workspaceSymbolProvider: true,
      renameProvider: { prepareProvider: true },
      documentFormattingProvider: true,
      signatureHelpProvider: {
        triggerCharacters: ['(', ',']
      },
      codeLensProvider: { resolveProvider: true },
      codeActionProvider: true,
      callHierarchyProvider: true
    }
  };
});

// Register providers
connection.onCompletion(/* implement completion */);
connection.onHover(/* implement hover */);
connection.onDefinition(/* implement go-to-definition */);
// ... more handlers

documents.listen(connection);
connection.listen();
```

### 5. Implement Client Extension

**client/src/extension.ts**:

```typescript
import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  const serverModule = context.asAbsolutePath(
    path.join('server', 'out', 'server.js')
  );

  const serverOptions: ServerOptions = {
    run: { module: serverModule, transport: TransportKind.ipc },
    debug: {
      module: serverModule,
      transport: TransportKind.ipc,
      options: { execArgv: ['--nolazy', '--inspect=6009'] }
    }
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'ssl' }],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher('**/*.{ssl,srvscr,ds}')
    }
  };

  client = new LanguageClient(
    'sslLanguageServer',
    'SSL Language Server',
    serverOptions,
    clientOptions
  );

  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
```

### 6. Build the Extension

```bash
# Compile TypeScript
npm run compile

# Or watch for changes during development
npm run watch
```

### 7. Package the Extension

```bash
# Install vsce (VS Code Extension Manager)
npm install -g @vscode/vsce

# Package extension
npm run package

# This creates ssl-language-support-0.1.0.vsix
```

### 8. Install Locally for Testing

```bash
# Install the VSIX
code --install-extension ssl-language-support-0.1.0.vsix

# Or copy to extensions directory
# Windows: %USERPROFILE%\.vscode\extensions\
# macOS/Linux: ~/.vscode/extensions/
```

---

## Testing Instructions

### Test Structure

All tests are in the `test/` directory with three categories:

1. **Unit Tests** (`test/unit/`) - Test individual components
2. **Integration Tests** (`test/integration/`) - Test VS Code providers
3. **End-to-End Tests** (`test/e2e/`) - Test full workflows

### Setting Up Tests

#### 1. Create Test Fixtures

Create sample SSL files in `test/fixtures/`:

**test/fixtures/basic.ssl:**
```ssl
:DECLARE sName, nCount;

sName := "Test";
nCount := 42;

/* This is a comment ;
```

**test/fixtures/procedures.ssl:**
```ssl
:PROCEDURE CalculateTotal;
    :PARAMETERS nQty, nPrice;
    :DECLARE nTotal;
    
    nTotal := nQty * nPrice;
    
    :RETURN nTotal;
:ENDPROC;

:PROCEDURE Main;
    :DECLARE nResult;
    
    nResult := CalculateTotal(10, 5.99);
:ENDPROC;
```

**test/fixtures/errors.ssl:**
```ssl
/* File with intentional errors for testing diagnostics ;

:PROCEDURE TestProc;
    :IF .T.;
        /* Missing :ENDIF ;
:ENDPROC

:DECLARE nValue
/* Missing semicolon ;

sUndeclared := 10;
/* Using undeclared variable ;
```

#### 2. Unit Tests Configuration

**test/unit/parser.test.ts:**
```typescript
import * as assert from 'assert';
import { parseSSL } from '../../server/src/parser/sslParser';

describe('SSL Parser', () => {
  it('should parse variable declaration', () => {
    const code = ':DECLARE sName;';
    const ast = parseSSL(code);
    assert.strictEqual(ast.statements.length, 1);
    assert.strictEqual(ast.statements[0].type, 'Declaration');
  });

  it('should parse procedure definition', () => {
    const code = `
      :PROCEDURE Test;
        :RETURN .T.;
      :ENDPROC;
    `;
    const ast = parseSSL(code);
    assert.strictEqual(ast.statements[0].type, 'Procedure');
  });

  it('should handle case-insensitive keywords', () => {
    const code1 = ':DECLARE sVar;';
    const code2 = ':declare sVar;';
    const ast1 = parseSSL(code1);
    const ast2 = parseSSL(code2);
    assert.deepStrictEqual(ast1, ast2);
  });
});
```

**test/unit/symbolTable.test.ts:**
```typescript
import * as assert from 'assert';
import { SymbolTable } from '../../server/src/analysis/symbolTable';

describe('Symbol Table', () => {
  it('should track declared variables', () => {
    const symbolTable = new SymbolTable();
    symbolTable.declare('sName', { type: 'variable', line: 1 });
    
    assert.strictEqual(symbolTable.isDeclared('sName'), true);
    assert.strictEqual(symbolTable.isDeclared('sOther'), false);
  });

  it('should find procedure definitions', () => {
    const symbolTable = new SymbolTable();
    symbolTable.declareProcedure('MyProc', {
      parameters: ['param1', 'param2'],
      line: 5
    });
    
    const proc = symbolTable.findProcedure('MyProc');
    assert.strictEqual(proc.parameters.length, 2);
  });
});
```

**test/unit/formatter.test.ts:**
```typescript
import * as assert from 'assert';
import { formatSSL } from '../../server/src/formatting/formatter';

describe('SSL Formatter', () => {
  it('should indent procedure body', () => {
    const input = ':PROCEDURE Test;\n:RETURN .T.;\n:ENDPROC;';
    const expected = ':PROCEDURE Test;\n    :RETURN .T.;\n:ENDPROC;';
    const result = formatSSL(input);
    assert.strictEqual(result, expected);
  });

  it('should add spaces around assignment operator', () => {
    const input = 'x:=5;';
    const expected = 'x := 5;';
    const result = formatSSL(input);
    assert.strictEqual(result, expected);
  });

  it('should format nested blocks', () => {
    const input = ':IF .T.;\n:IF .T.;\nx:=1;\n:ENDIF;\n:ENDIF;';
    const result = formatSSL(input);
    assert.ok(result.includes('    :IF'));
    assert.ok(result.includes('        x := 1;'));
  });
});
```

**test/unit/diagnostics.test.ts:**
```typescript
import * as assert from 'assert';
import { analyzeDiagnostics } from '../../server/src/analysis/diagnostics';

describe('Diagnostics', () => {
  it('should detect mismatched blocks', () => {
    const code = ':IF .T.;\n/* Missing :ENDIF ;';
    const diagnostics = analyzeDiagnostics(code);
    
    assert.strictEqual(diagnostics.length, 1);
    assert.ok(diagnostics[0].message.includes('ENDIF'));
  });

  it('should detect undefined variables', () => {
    const code = 'sUndefined := 10;';
    const diagnostics = analyzeDiagnostics(code);
    
    assert.ok(diagnostics.some(d => 
      d.message.includes('undefined') || d.message.includes('not declared')
    ));
  });

  it('should detect missing semicolons', () => {
    const code = ':DECLARE sVar\n/* Missing semicolon above ;';
    const diagnostics = analyzeDiagnostics(code);
    
    assert.ok(diagnostics.some(d => d.message.includes(';')));
  });
});
```

#### 3. Integration Tests Configuration

**test/integration/completion.test.ts:**
```typescript
import * as vscode from 'vscode';
import * as assert from 'assert';
import { getDocUri, activate } from './helper';

describe('Completion Provider', () => {
  const docUri = getDocUri('procedures.ssl');

  it('should suggest declared procedures', async () => {
    await activate(docUri);
    
    const position = new vscode.Position(10, 15);
    const completions = await vscode.commands.executeCommand<vscode.CompletionList>(
      'vscode.executeCompletionItemProvider',
      docUri,
      position
    );
    
    assert.ok(completions.items.length > 0);
    assert.ok(completions.items.some(item => 
      item.label === 'CalculateTotal'
    ));
  });

  it('should suggest keywords', async () => {
    await activate(docUri);
    
    const position = new vscode.Position(5, 5);
    const completions = await vscode.commands.executeCommand<vscode.CompletionList>(
      'vscode.executeCompletionItemProvider',
      docUri,
      position
    );
    
    assert.ok(completions.items.some(item => 
      item.label.toUpperCase() === 'PROCEDURE'
    ));
  });
});
```

**test/integration/hover.test.ts:**
```typescript
import * as vscode from 'vscode';
import * as assert from 'assert';
import { getDocUri, activate } from './helper';

describe('Hover Provider', () => {
  const docUri = getDocUri('procedures.ssl');

  it('should show procedure signature on hover', async () => {
    await activate(docUri);
    
    // Hover over procedure call
    const position = new vscode.Position(12, 15);
    const hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
      'vscode.executeHoverProvider',
      docUri,
      position
    );
    
    assert.strictEqual(hovers.length, 1);
    const content = hovers[0].contents[0] as vscode.MarkdownString;
    assert.ok(content.value.includes('CalculateTotal'));
    assert.ok(content.value.includes('nQty'));
    assert.ok(content.value.includes('nPrice'));
  });
});
```

**test/integration/helper.ts:**
```typescript
import * as vscode from 'vscode';
import * as path from 'path';

export let doc: vscode.TextDocument;
export let editor: vscode.TextEditor;
export let documentEol: string;
export let platformEol: string;

export async function activate(docUri: vscode.Uri) {
  const ext = vscode.extensions.getExtension('your-publisher.ssl-language-support');
  await ext!.activate();
  
  doc = await vscode.workspace.openTextDocument(docUri);
  editor = await vscode.window.showTextDocument(doc);
  
  await sleep(2000); // Wait for language server
}

export function getDocUri(docName: string): vscode.Uri {
  return vscode.Uri.file(
    path.join(__dirname, '../../test/fixtures', docName)
  );
}

function sleep(ms: number): Promise<void> {
  return new Promise(resolve => setTimeout(resolve, ms));
}
```

#### 4. End-to-End Tests Configuration

**test/e2e/extension.test.ts:**
```typescript
import * as assert from 'assert';
import * as vscode from 'vscode';

describe('Extension End-to-End Tests', () => {
  it('should activate extension on SSL file', async () => {
    const ext = vscode.extensions.getExtension('your-publisher.ssl-language-support');
    assert.ok(ext);
    
    const docUri = vscode.Uri.file(
      path.join(__dirname, '../fixtures/basic.ssl')
    );
    
    await vscode.workspace.openTextDocument(docUri);
    await ext!.activate();
    
    assert.strictEqual(ext!.isActive, true);
  });

  it('should provide syntax highlighting', async () => {
    const docUri = vscode.Uri.file(
      path.join(__dirname, '../fixtures/basic.ssl')
    );
    
    const doc = await vscode.workspace.openTextDocument(docUri);
    const editor = await vscode.window.showTextDocument(doc);
    
    // Check that document language is set to SSL
    assert.strictEqual(doc.languageId, 'ssl');
  });

  it('should format document', async () => {
    const docUri = vscode.Uri.file(
      path.join(__dirname, '../fixtures/basic.ssl')
    );
    
    const doc = await vscode.workspace.openTextDocument(docUri);
    await vscode.window.showTextDocument(doc);
    
    const edits = await vscode.commands.executeCommand<vscode.TextEdit[]>(
      'vscode.executeFormatDocumentProvider',
      docUri
    );
    
    assert.ok(edits.length > 0);
  });
});
```

### Running Tests

#### Run All Tests
```bash
npm test
```

#### Run Unit Tests Only
```bash
npm run test:unit
```

#### Run Integration Tests Only
```bash
npm run test:integration
```

#### Run Specific Test File
```bash
npx mocha -r ts-node/register test/unit/parser.test.ts
```

#### Run Tests with Coverage
```bash
npm install --save-dev nyc

# Add to package.json scripts:
# "test:coverage": "nyc npm test"

npm run test:coverage
```

#### Watch Mode (for development)
```bash
npx mocha -r ts-node/register test/unit/**/*.test.ts --watch
```

### Debugging Tests

**VS Code launch.json for debugging:**

```json
{
  "version": "0.2.0",
  "configurations": [
    {
      "name": "Run Extension Tests",
      "type": "extensionHost",
      "request": "launch",
      "runtimeExecutable": "${execPath}",
      "args": [
        "--extensionDevelopmentPath=${workspaceFolder}",
        "--extensionTestsPath=${workspaceFolder}/out/test/e2e"
      ],
      "outFiles": ["${workspaceFolder}/out/test/**/*.js"]
    },
    {
      "name": "Debug Unit Tests",
      "type": "node",
      "request": "launch",
      "program": "${workspaceFolder}/node_modules/mocha/bin/_mocha",
      "args": [
        "-r", "ts-node/register",
        "${workspaceFolder}/test/unit/**/*.test.ts"
      ],
      "console": "integratedTerminal"
    }
  ]
}
```

### CI/CD Configuration

**.github/workflows/ci.yml:**

```yaml
name: CI

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  test:
    strategy:
      matrix:
        os: [ubuntu-latest, windows-latest, macos-latest]
        node-version: [16.x, 18.x]
    
    runs-on: ${{ matrix.os }}
    
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Node.js ${{ matrix.node-version }}
        uses: actions/setup-node@v3
        with:
          node-version: ${{ matrix.node-version }}
      
      - name: Install dependencies
        run: npm ci
      
      - name: Lint
        run: npm run lint
      
      - name: Compile
        run: npm run compile
      
      - name: Run tests
        run: npm test
      
      - name: Upload coverage
        if: matrix.os == 'ubuntu-latest' && matrix.node-version == '18.x'
        uses: codecov/codecov-action@v3
        with:
          files: ./coverage/lcov.info

  package:
    needs: test
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: 18.x
      
      - name: Install dependencies
        run: npm ci
      
      - name: Package extension
        run: npm run package
      
      - name: Upload VSIX
        uses: actions/upload-artifact@v3
        with:
          name: ssl-vscode-extension
          path: '*.vsix'
```

### Test Coverage Goals

Aim for the following coverage targets:

- **Overall**: 80%+ code coverage
- **Parser**: 90%+ (critical component)
- **Diagnostics**: 85%+ (catches user errors)
- **Formatter**: 80%+ (visual quality)
- **Providers**: 75%+ (integration points)

### Writing Good Tests

**Best Practices:**

1. **One assertion per test** (when possible)
2. **Descriptive test names** that explain what's being tested
3. **Arrange-Act-Assert** pattern
4. **Test edge cases** (empty input, malformed input, etc.)
5. **Use fixtures** for realistic test data
6. **Mock external dependencies**
7. **Test error paths** not just happy paths

**Example:**
```typescript
describe('SSL Parser', () => {
  describe('Variable Declarations', () => {
    it('should parse single variable declaration', () => {
      // Arrange
      const code = ':DECLARE sName;';
      
      // Act
      const ast = parseSSL(code);
      
      // Assert
      assert.strictEqual(ast.statements.length, 1);
      assert.strictEqual(ast.statements[0].type, 'Declaration');
    });

    it('should parse multiple variable declaration', () => {
      const code = ':DECLARE sName, nCount, bFlag;';
      const ast = parseSSL(code);
      
      assert.strictEqual(ast.statements.length, 1);
      assert.strictEqual(ast.statements[0].variables.length, 3);
    });

    it('should handle empty declaration gracefully', () => {
      const code = ':DECLARE;';
      
      assert.throws(() => parseSSL(code), /expected identifier/i);
    });
  });
});
```

---

## Development Workflow

### Daily Development
```bash
# 1. Start watch mode
npm run watch

# 2. Press F5 in VS Code to launch Extension Development Host

# 3. Make changes to code

# 4. Reload Extension Development Host (Ctrl+R)

# 5. Test changes

# 6. Run tests
npm test

# 7. Commit when tests pass
git commit -m "Add feature X"
```

### Before Committing
```bash
# Lint code
npm run lint

# Format code
npx prettier --write "**/*.ts"

# Run all tests
npm test

# Build extension
npm run compile

# Test package
npm run package
```

---

## Implementation Priorities

### Phase 1: Foundation (Week 1-2)
1. Project setup and structure
2. TextMate grammar (syntax highlighting)
3. Language configuration (brackets, comments)
4. Basic LSP server skeleton
5. SSL parser from EBNF
6. Unit tests for parser

### Phase 2: Core Intelligence (Week 3-4)
1. Symbol table and indexing
2. Document symbols (outline view)
3. Go to definition
4. Find references
5. Hover tooltips
6. Unit tests for analysis

### Phase 3: Advanced Features (Week 5-6)
1. Auto-completion
2. Signature help
3. Workspace symbol search
4. Code lens
5. Call hierarchy
6. Integration tests

### Phase 4: Quality (Week 7-8)
1. Diagnostics engine
2. Code actions/quick fixes
3. Formatter implementation
4. Comprehensive testing
5. Documentation
6. CI/CD setup

---

## Key Implementation Notes

### SSL-Specific Considerations

1. **Case Insensitivity**: All keyword matching must be case-insensitive
2. **Comment Syntax**: Comments start with `/*` and end with `;` (not `*/`)
3. **Property Access**: Uses colon `:` not dot `.` for property access
4. **Array Indexing**: 1-based, not 0-based
5. **Boolean Literals**: `.T.` and `.F.` with periods
6. **Assignment**: Uses `:=` not `=`
7. **String Delimiters**: Both `"` and `'` are valid

### Performance Considerations

1. **Incremental Parsing**: Only re-parse changed sections
2. **Caching**: Cache symbol tables and ASTs
3. **Background Parsing**: Parse on background thread
4. **Debouncing**: Debounce diagnostics on typing
5. **Indexing**: Build workspace index incrementally

### Error Handling

1. **Graceful Degradation**: Partial results better than none
2. **Error Recovery**: Parser should continue after errors
3. **User-Friendly Messages**: Clear error descriptions
4. **Source Locations**: Accurate line/column information

---

## Resources

### VS Code Extension Development
- [VS Code Extension API](https://code.visualstudio.com/api)
- [Language Server Protocol](https://microsoft.github.io/language-server-protocol/)
- [Extension Samples](https://github.com/microsoft/vscode-extension-samples)

### Testing
- [VS Code Extension Testing](https://code.visualstudio.com/api/working-with-extensions/testing-extension)
- [Mocha Documentation](https://mochajs.org/)
- [VS Code Test Helper](https://github.com/microsoft/vscode-test)

### SSL Reference
- SSL EBNF Grammar (provided)
- SSL Language Specification (if available)
- STARLIMS Documentation

---

## Success Criteria

The extension is complete when:

1. ✅ All syntax highlighting works correctly
2. ✅ Code intelligence features work for user's code
3. ✅ Formatting produces consistent, readable output
4. ✅ Diagnostics catch common errors accurately
5. ✅ Quick fixes resolve issues correctly
6. ✅ All tests pass with >80% coverage
7. ✅ Extension works on Windows, macOS, and Linux
8. ✅ Performance is acceptable for large files (>1000 lines)
9. ✅ Documentation is complete and accurate
10. ✅ No confidential STARLIMS information included

---

## Getting Help

When working with Claude Code:

1. **Reference this document** for architectural decisions
2. **Provide context** about what you're working on
3. **Share error messages** in full
4. **Include relevant code snippets**
5. **Mention the EBNF grammar** when discussing parsing
6. **Ask for specific examples** when unclear

---

**This specification should provide everything needed to build a production-quality SSL VS Code extension. Good luck with development!**
