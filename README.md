# STARLIMS Scripting Language Formatter

This Visual Studio Code extension provides syntax highlighting and folding for STARLIMS Scripting Language (SSL).

## Features

-   Syntax highlighting for SSL files
-   Code folding for SSL-specific constructs
-   Special highlighting for folds in the minimap

## Installation

1. Open Visual Studio Code
2. Press `Ctrl+P` (or `Cmd+P` on macOS) to open the Quick Open dialog
3. Type `ext install mahoskye.vs-code-ssl-formatter` and press Enter

## Usage

Once installed, the extension will automatically activate for files with `.ssl` or `.ssl.txt` extensions.

## Building from Source

To build the extension from source:

1. Clone the repository
2. Install dependencies: `npm install`
3. Compile TypeScript: `npm run compile`
4. Package the extension: `npm run package`

The packaged `.vsix` file can then be installed in VS Code manually.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This extension is licensed under the [MIT License](LICENSE).
