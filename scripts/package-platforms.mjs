#!/usr/bin/env node
// Builds platform-specific vsix packages, each containing only the server
// binary its target actually runs, plus one universal package (all binaries)
// as the fallback for targets without a dedicated build (e.g. alpine).
// Cuts the per-user download from ~23 MB to ~5 MB.
//
// Usage: node scripts/package-platforms.mjs [outDir]   (default: dist-vsix/)

import { execFileSync } from "node:child_process";
import { mkdirSync, mkdtempSync, renameSync, rmSync, readdirSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { readFileSync } from "node:fs";

const repoRoot = fileURLToPath(new URL("..", import.meta.url));
const serverDir = join(repoRoot, "server");
const pkg = JSON.parse(readFileSync(join(repoRoot, "package.json"), "utf8"));
const outDir = join(repoRoot, process.argv[2] || "dist-vsix");

// vsce target → server binary that target runs. win32-arm64 ships the amd64
// binary because no windows-arm64 build exists; Windows on ARM emulates x64.
const TARGETS = {
    "win32-x64": "starlims-lsp-windows-amd64.exe",
    "win32-arm64": "starlims-lsp-windows-amd64.exe",
    "linux-x64": "starlims-lsp-linux-amd64",
    "linux-arm64": "starlims-lsp-linux-arm64",
    "darwin-x64": "starlims-lsp-darwin-amd64",
    "darwin-arm64": "starlims-lsp-darwin-arm64",
};

const allBinaries = readdirSync(serverDir).filter((n) => n.startsWith("starlims-lsp-"));

function vsce(args) {
    execFileSync("npx", ["vsce", ...args], { cwd: repoRoot, stdio: "inherit" });
}

rmSync(outDir, { recursive: true, force: true });
mkdirSync(outDir, { recursive: true });

// Universal package first: everything in place, no --target.
console.log("\n=== packaging universal ===");
vsce(["package", "-o", join(outDir, `${pkg.name}-${pkg.version}.vsix`)]);

// Platform packages: stash every binary the target doesn't need, package,
// restore. The stash lives outside server/ so vsce never sees it.
for (const [target, binary] of Object.entries(TARGETS)) {
    const stash = mkdtempSync(join(tmpdir(), "ssl-pkg-"));
    const stashed = allBinaries.filter((n) => n !== binary);
    try {
        for (const name of stashed) renameSync(join(serverDir, name), join(stash, name));
        console.log(`\n=== packaging ${target} (${binary}) ===`);
        vsce([
            "package",
            "--target", target,
            "-o", join(outDir, `${pkg.name}-${pkg.version}-${target}.vsix`),
        ]);
    } finally {
        for (const name of stashed) renameSync(join(stash, name), join(serverDir, name));
        rmSync(stash, { recursive: true, force: true });
    }
}

console.log(`\nPackages written to ${outDir}:`);
for (const f of readdirSync(outDir).sort()) console.log(`  ${f}`);
