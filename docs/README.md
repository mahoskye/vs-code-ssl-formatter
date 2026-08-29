# Developer Documentation

This directory contains documentation for developers contributing to the SSL VS Code extension.

## 📚 Documentation Files

### Architecture & Development

**[ARCHITECTURE.md](ARCHITECTURE.md)**
- Complete extension architecture and design
- Feature implementation details
- Build and development guide
- Configuration reference

### CI/CD & Testing

**[CI_SETUP.md](CI_SETUP.md)**
- GitHub Actions workflow documentation
- CI/CD pipeline setup
- Testing infrastructure
- Publishing workflow

**[CORPUS_TESTING.md](CORPUS_TESTING.md)**
- Running the bundled LSP against thousands of real SSL files
- Formatter idempotency and safety invariants
- Diagnostic baselines for reviewing an LSP bundle bump

### Language Reference

**[grammar/ssl-ebnf.md](grammar/ssl-ebnf.md)**
- Complete EBNF grammar for SSL v11
- Language syntax reference
- Parser implementation guide

## 🚀 Quick Start

### For Contributors

1. Read [ARCHITECTURE.md](ARCHITECTURE.md) to understand the codebase
2. Check [CI_SETUP.md](CI_SETUP.md) for testing and CI information
3. See [grammar/ssl-ebnf.md](grammar/ssl-ebnf.md) for language details

### Running Tests

```bash
npm run compile
npm run lint
npm run test:unit
```

Optionally, against a local corpus of real SSL — see
[CORPUS_TESTING.md](CORPUS_TESTING.md):

```bash
npm run test:corpus
```

### Building the Extension

```bash
npm run package
```

Creates a `.vsix` file you can install locally.

## 📖 Additional Resources

- [README.md](../README.md) - User-facing documentation
- [CHANGELOG.md](../CHANGELOG.md) - Version history
- [LICENSE.txt](../LICENSE.txt) - MIT License

## 🤝 Contributing

See [ARCHITECTURE.md](ARCHITECTURE.md) for:
- Project structure
- Coding conventions
- Testing guidelines
- Pull request process
