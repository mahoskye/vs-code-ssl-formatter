# Developer Documentation

Start with [AGENTS.md](../AGENTS.md): what the extension is, the decisions to
keep, how to build and test, and the release process.

| Document | Contents |
| --- | --- |
| [ARCHITECTURE.md](ARCHITECTURE.md) | How the extension is put together: the language client, the extension's own providers, settings flow, the grammar sync, and the tests. |
| [CORPUS_TESTING.md](CORPUS_TESTING.md) | Running the bundled server over the private corpus to review a server bump. |
| [../.github/workflows/README.md](../.github/workflows/README.md) | The CI, publish and CodeQL workflows. |
| [archive/](archive/README.md) | Historical documents that no longer describe the extension. |

User documentation is the [README](../README.md); release history is the
[CHANGELOG](../CHANGELOG.md). The extension is MIT-licensed ([LICENSE](../LICENSE)).

For the SSL language itself, see the
[SSL reference](https://github.com/mahoskye/starlims-ssl-reference) (written for
STARLIMS 11, verified on STARLIMS 12) and
[ssl-style-guide](https://github.com/mahoskye/ssl-style-guide) for the style
rules and grammars.
