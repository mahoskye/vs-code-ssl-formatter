#!/usr/bin/env node
// Downloads the starlims-lsp release binaries (+ third-party notices) into
// server/ and verifies each file's SHA-256 against the digest GitHub records
// for the release asset. Replaces the manual `gh release download` step so a
// tampered or truncated download can never be committed unnoticed.
//
// Usage: node scripts/fetch-lsp-release.mjs v0.14.1
//
// On success it records the bundled tag in server/LSP_VERSION, which
// scripts/generate-diagnostic-slugs.mjs reads, and stamps the "Bundled
// version" line in server/DOCUMENTATION.md.

import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import { readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

const REPO = "mahoskye/starlims-lsp";
const PATTERNS = [/^starlims-lsp-/, /^THIRD-PARTY-NOTICES\.md$/];

const version = process.argv[2];
if (!version || !/^v\d+\.\d+\.\d+$/.test(version)) {
    console.error("Usage: node scripts/fetch-lsp-release.mjs vX.Y.Z");
    process.exit(1);
}

const serverDir = join(fileURLToPath(new URL("..", import.meta.url)), "server");

const release = JSON.parse(
    execFileSync("gh", ["api", `repos/${REPO}/releases/tags/${version}`], { encoding: "utf8" })
);
const assets = release.assets.filter((a) => PATTERNS.some((re) => re.test(a.name)));
if (assets.length === 0) {
    console.error(`No matching assets on ${REPO} release ${version}`);
    process.exit(1);
}

const missingDigests = assets.filter((a) => !a.digest?.startsWith("sha256:"));
if (missingDigests.length > 0) {
    console.error(
        `Refusing to fetch: no sha256 digest recorded for ${missingDigests.map((a) => a.name).join(", ")}`
    );
    process.exit(1);
}

console.log(`Downloading ${assets.length} assets from ${REPO} ${version} into server/ …`);
execFileSync(
    "gh",
    [
        "release", "download", version,
        "--repo", REPO,
        "--dir", serverDir,
        "--clobber",
        ...assets.flatMap((a) => ["--pattern", a.name]),
    ],
    { stdio: "inherit" }
);

let failed = false;
for (const asset of assets) {
    const expected = asset.digest.slice("sha256:".length);
    const actual = createHash("sha256").update(readFileSync(join(serverDir, asset.name))).digest("hex");
    if (actual === expected) {
        console.log(`  ok      ${asset.name}  ${actual}`);
    } else {
        failed = true;
        console.error(`  MISMATCH ${asset.name}\n    expected ${expected}\n    actual   ${actual}`);
    }
}

if (failed) {
    console.error("\nChecksum verification FAILED — do not commit these binaries.");
    process.exit(1);
}
console.log(`\nAll ${assets.length} assets verified against GitHub release digests.`);

writeFileSync(join(serverDir, "LSP_VERSION"), `${version}\n`, "utf8");

const docPath = join(serverDir, "DOCUMENTATION.md");
const doc = readFileSync(docPath, "utf8");
const built = (release.published_at ?? "").slice(0, 10);
const stamped = doc.replace(
    /`starlims-lsp` v\d+\.\d+\.\d+ \(built [^)]*\)/,
    `\`starlims-lsp\` ${version} (built ${built})`
);
if (stamped === doc && !doc.includes(`\`starlims-lsp\` ${version} `)) {
    console.error("Could not find the \"Bundled version\" line in server/DOCUMENTATION.md to stamp.");
    process.exit(1);
}
writeFileSync(docPath, stamped, "utf8");
console.log(`Recorded ${version} in server/LSP_VERSION and server/DOCUMENTATION.md.`);
