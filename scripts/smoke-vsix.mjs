#!/usr/bin/env node
// Clean-profile install check for the packaged VSIX.
// Runs `vsce package`, installs into an isolated user-data + extensions dir,
// opens a .ssl file, then scans the extension-host log for activation failures.
// Catches packaging regressions (missing runtime deps, broken main entry, etc.)
// that the Run Extension debug launcher misses because it loads from source.

import { spawn, spawnSync } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import { mkdtempSync, readFileSync, readdirSync, rmSync, writeFileSync, existsSync, statSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

const repoRoot = new URL("..", import.meta.url).pathname;
const pkg = JSON.parse(readFileSync(join(repoRoot, "package.json"), "utf8"));
const vsixName = `${pkg.name}-${pkg.version}.vsix`;
const vsixPath = join(repoRoot, vsixName);

const codeBin = process.env.VSCODE_BIN || "code";

function run(cmd, args, opts = {}) {
    const r = spawnSync(cmd, args, { stdio: "inherit", cwd: repoRoot, ...opts });
    if (r.status !== 0) {
        throw new Error(`${cmd} ${args.join(" ")} exited with ${r.status}`);
    }
}

console.log(`[smoke] packaging ${vsixName}…`);
run("npx", ["vsce", "package"]);
if (!existsSync(vsixPath)) {
    throw new Error(`expected ${vsixPath} after vsce package`);
}

const profile = mkdtempSync(join(tmpdir(), "ssl-smoke-"));
const userData = join(profile, "data");
const extDir = join(profile, "ext");
const sslFile = join(profile, "smoke.ssl");
writeFileSync(sslFile, ":PROCEDURE Smoke;\n:RETURN .T.;\n:ENDPROC;\n");

let failed = false;
try {
    console.log(`[smoke] installing into ${profile}…`);
    run(codeBin, [
        "--user-data-dir", userData,
        "--extensions-dir", extDir,
        "--install-extension", vsixPath,
    ]);

    console.log("[smoke] launching VS Code to trigger activation…");
    const child = spawn(codeBin, [
        "--user-data-dir", userData,
        "--extensions-dir", extDir,
        "--disable-workspace-trust",
        "--wait",
        sslFile,
    ], { stdio: "ignore", detached: true });
    child.unref();
    // Give the window time to boot, load the SSL file, and run extension activation.
    await sleep(20_000);
    // VS Code spawns many descendant processes that escape the initial group;
    // match by the unique user-data-dir path to clean them all up reliably.
    spawnSync("pkill", ["-TERM", "-f", userData], { stdio: "ignore" });
    await sleep(1_500);
    spawnSync("pkill", ["-KILL", "-f", userData], { stdio: "ignore" });

    const logsRoot = join(userData, "logs");
    if (!existsSync(logsRoot)) {
        throw new Error(`no logs directory at ${logsRoot}`);
    }
    const sessions = readdirSync(logsRoot)
        .map((n) => ({ n, p: join(logsRoot, n) }))
        .filter((e) => statSync(e.p).isDirectory())
        .sort((a, b) => b.n.localeCompare(a.n));
    if (sessions.length === 0) {
        throw new Error("no log sessions recorded");
    }
    const exthostLog = join(sessions[0].p, "window1", "exthost", "exthost.log");
    if (!existsSync(exthostLog)) {
        throw new Error(`extension host log missing at ${exthostLog}`);
    }
    const log = readFileSync(exthostLog, "utf8");

    const fatalPatterns = [
        /Cannot find module/i,
        /MODULE_NOT_FOUND/,
        /Activating extension '[^']*mahoskye[^']*' failed/i,
    ];
    const hits = fatalPatterns.flatMap((re) => {
        const m = log.match(new RegExp(re.source, re.flags + "g")) || [];
        return m;
    });
    if (hits.length > 0) {
        failed = true;
        console.error("[smoke] FAIL — extension host log contains:");
        for (const h of hits.slice(0, 10)) console.error("  ", h);
    } else {
        console.log("[smoke] OK — no activation errors in extension host log");
    }
} finally {
    rmSync(profile, { recursive: true, force: true });
}

if (failed) process.exit(1);
