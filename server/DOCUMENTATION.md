# Bundled language server

> **Bundled version:** `starlims-lsp` v0.28.0 (built 2026-10-15). Run `./starlims-lsp-<platform>-<arch> --version` to confirm. Source: https://github.com/mahoskye/starlims-lsp.

This folder holds the [starlims-lsp](https://github.com/mahoskye/starlims-lsp)
language server that the extension starts for SSL files. Each Marketplace
package carries the build for its platform (`starlims-lsp-<os>-<arch>`); Windows
on ARM runs the `windows-amd64` build through x64 emulation. The binaries come
from the starlims-lsp GitHub release and are checked against its SHA-256
digests when they are fetched.

The extension sends the server its settings, mapped from the `ssl.*` settings
in VS Code. For the server's own options, its command-line interface
(`--validate`, `--format`) and its diagnostic catalog, see the starlims-lsp
[README](https://github.com/mahoskye/starlims-lsp#readme) and
[configuration reference](https://github.com/mahoskye/starlims-lsp/blob/main/docs/configuration/CONFIGURATION.md).

Licenses for the server's dependencies are in `THIRD-PARTY-NOTICES.md`.
