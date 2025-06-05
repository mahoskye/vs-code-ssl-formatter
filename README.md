# STARLIMS Scripting Language Formatter

This Visual Studio Code extension provides syntax highlighting, folding, auto-completion, error detection, and semantic highlighting for STARLIMS Scripting Language (SSL).

## Disclaimer

This extension is unofficial and not affiliated with or endorsed by Abbott Informatics or any of its subsidiaries. It is provided "as is" without warranty of any kind, express or implied. Use at your own risk.

STARLIMS® is a registered trademark of Abbott Informatics Corporation. This extension is an independent project created to enhance the development experience for STARLIMS users and is not officially associated with STARLIMS or Abbott Informatics products.

## Features

-   Syntax highlighting for SSL files
-   Code folding for SSL-specific constructs
-   Special highlighting for folds in the minimap
-   Auto-completion support for SSL files
-   Custom completion items configurable through VS Code settings
-   Error detection based on SSL EBNF grammar
-   Semantic highlighting for better code visualization
-   Proper comment handling (/\* ... ; format)
-   Support for SSL-specific language constructs
-   Validation against formal SSL grammar

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

### Reloading Completions

To reload completion items after modifying the settings, you can use the "Reload SSL Completions" command from the Command Palette (Ctrl+Shift+P or Cmd+Shift+P).

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This extension is licensed under the [MIT License](LICENSE).
