# STARLIMS Scripting Language Formatter

This Visual Studio Code extension provides syntax highlighting, folding, and auto-completion for STARLIMS Scripting Language (SSL).

## Features

-   Syntax highlighting for SSL files
-   Code folding for SSL-specific constructs
-   Special highlighting for folds in the minimap
-   Auto-completion support for SSL files
-   Custom completion items configurable through VS Code settings

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

```

```
