# STARLIMS Scripting Language Formatter

This Visual Studio Code extension provides syntax highlighting, folding, auto-completion, error detection, and semantic highlighting for STARLIMS Scripting Language (SSL).

## Disclaimer

This extension is unofficial and not affiliated with or endorsed by Abbott Informatics or any of its subsidiaries. It is provided "as is" without warranty of any kind, express or implied. Use at your own risk.

STARLIMS® is a registered trademark of Abbott Informatics Corporation. This extension is an independent project created to enhance the development experience for STARLIMS users and is not officially associated with STARLIMS or Abbott Informatics products.

## Features

-   Syntax highlighting for SSL files
-   Bracketing, Closing pairs, and Comments handling
-   Basic code folding for SSL-specific constructs

# v0.4.0 Milestone: SSL Parser Implementation

## Goals

-   [ ] Design AST (Abstract Syntax Tree) structure
-   [ ] Implement recursive descent parser
-   [ ] Handle SSL syntax rules and precedence
-   [ ] Add parser error handling and recovery
-   [ ] Create parser unit tests
-   [ ] Update documentation

## Parser Components to Implement

-   [ ] Expression parsing
-   [ ] Statement parsing
-   [ ] Block structure parsing
-   [ ] Error reporting with line/column info

## Installation

1. Open Visual Studio Code
2. Press `Ctrl+P` (or `Cmd+P` on macOS) to open the Quick Open dialog
3. Type `ext install mahoskye.vs-code-ssl-formatter` and press Enter

## Usage

Once installed, the extension will automatically activate for files with `.ssl` or `.ssl.txt` extensions.

### Custom Completions

You can add custom completion items by modifying the `sslFormatter.completions` setting in your VS Code settings. For example:

```json
"sslFormatter.completions": [
    {
        "label": "myCustomFunction()",
        "kind": "function",
        "detail": "A custom function",
        "documentation": "This is a custom function added through settings."
    }
]
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This extension is licensed under the [MIT License](LICENSE).
