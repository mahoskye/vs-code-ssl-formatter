#!/usr/bin/env node
// Clean-profile install check for the packaged VSIX.
// Installs the vsix into an isolated user-data + extensions dir, opens a .ssl
// file, then scans the extension-host log for activation failures. Catches
// packaging regressions (missing runtime deps, broken main entry, etc.) that
// the Run Extension debug launcher misses because it loads from source.
//
// Usage: node scripts/smoke-vsix.mjs [path/to/file.vsix | target]
// With a vsix path, that exact artifact is tested; with a target name like
// `linux-x64`, tests dist-vsix/<name>-<version>-<target>.vsix (the CI form —
// no shell interpolation needed); with no arguments, packages first and
// tests the result.

import { spawn, spawnSync } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import { mkdtempSync, openSync, readFileSync, readdirSync, rmSync, writeFileSync, existsSync, statSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

const repoRoot = new URL("..", import.meta.url).pathname;
const pkg = JSON.parse(readFileSync(join(repoRoot, "package.json"), "utf8"));

// Resolve the VS Code CLI: $VSCODE_BIN, then `code` on PATH, then download a
// copy via @vscode/test-electron (the CI path — runners have no VS Code).
let codeBin = process.env.VSCODE_BIN || "code";
let codeBaseArgs = [];
if (spawnSync(codeBin, ["--version"], { stdio: "ignore" }).status !== 0) {
    console.log("[smoke] no VS Code CLI found — downloading via @vscode/test-electron…");
    const { downloadAndUnzipVSCode, resolveCliArgsFromVSCodeExecutablePath } =
        await import("@vscode/test-electron");
    const exe = await downloadAndUnzipVSCode("stable");
    [codeBin, ...codeBaseArgs] = resolveCliArgsFromVSCodeExecutablePath(exe);
    // We pass our own isolated dirs; drop the defaults the resolver adds.
    codeBaseArgs = codeBaseArgs.filter(
        (a) => !a.startsWith("--user-data-dir") && !a.startsWith("--extensions-dir")
    );
}

function run(cmd, args, opts = {}) {
    const r = spawnSync(cmd, args, { stdio: "inherit", cwd: repoRoot, ...opts });
    if (r.status !== 0) {
        throw new Error(`${cmd} ${args.join(" ")} exited with ${r.status}`);
    }
}

let vsixPath = process.argv[2];
if (vsixPath && !vsixPath.endsWith(".vsix")) {
    vsixPath = join(repoRoot, "dist-vsix", `${pkg.name}-${pkg.version}-${vsixPath}.vsix`);
}
if (vsixPath) {
    if (!existsSync(vsixPath)) {
        throw new Error(`vsix not found: ${vsixPath}`);
    }
    console.log(`[smoke] testing existing artifact ${vsixPath}`);
} else {
    const vsixName = `${pkg.name}-${pkg.version}.vsix`;
    vsixPath = join(repoRoot, vsixName);
    console.log(`[smoke] packaging ${vsixName}…`);
    run("npx", ["vsce", "package"]);
    if (!existsSync(vsixPath)) {
        throw new Error(`expected ${vsixPath} after vsce package`);
    }
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
        ...codeBaseArgs,
        "--user-data-dir", userData,
        "--extensions-dir", extDir,
        "--install-extension", vsixPath,
    ]);

    console.log("[smoke] launching VS Code to trigger activation…");
    const launchErrLog = join(profile, "launch-stderr.log");
    const child = spawn(codeBin, [
        ...codeBaseArgs,
        "--user-data-dir", userData,
        "--extensions-dir", extDir,
        "--disable-workspace-trust",
        // CI runners restrict unprivileged user namespaces, which breaks
        // Electron's sandbox; these flags are no-ops for the check itself.
        "--no-sandbox",
        "--disable-gpu",
        "--disable-telemetry",
        "--disable-updates",
        "--skip-welcome",
        "--skip-release-notes",
        "--wait",
        sslFile,
    ], { stdio: ["ignore", "ignore", openSync(launchErrLog, "w")], detached: true });
    child.unref();

    // Wait for the extension host log to appear (cold starts on CI runners
    // are far slower than local), then give activation a moment to finish.
    const logsRoot = join(userData, "logs");
    const findExthostLogs = () => {
        if (!existsSync(logsRoot)) { return []; }
        const logs = [];
        for (const session of readdirSync(logsRoot)) {
            const sessionDir = join(logsRoot, session);
            if (!statSync(sessionDir).isDirectory()) { continue; }
            for (const win of readdirSync(sessionDir)) {
                const candidate = join(sessionDir, win, "exthost", "exthost.log");
                if (existsSync(candidate)) { logs.push(candidate); }
            }
        }
        return logs;
    };
    const deadline = Date.now() + 120_000;
    let exthostLogs = [];
    while (exthostLogs.length === 0 && Date.now() < deadline) {
        await sleep(2_000);
        exthostLogs = findExthostLogs();
    }
    if (exthostLogs.length > 0) {
        await sleep(10_000); // let activation run and flush to the log
    }
    // VS Code spawns many descendant processes that escape the initial group;
    // match by the unique user-data-dir path to clean them all up reliably.
    spawnSync("pkill", ["-TERM", "-f", userData], { stdio: "ignore" });
    await sleep(1_500);
    spawnSync("pkill", ["-KILL", "-f", userData], { stdio: "ignore" });

    exthostLogs = findExthostLogs();
    if (exthostLogs.length === 0) {
        if (existsSync(launchErrLog)) {
            const err = readFileSync(launchErrLog, "utf8").trim();
            if (err) {
                console.error("[smoke] VS Code launch stderr:");
                console.error(err.split("\n").slice(-40).join("\n"));
            }
        }
        throw new Error(`no extension host log appeared under ${logsRoot}`);
    }
    const log = exthostLogs.map((p) => readFileSync(p, "utf8")).join("\n");

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
    } else if (!/mahoskye\.vs-code-ssl-formatter/i.test(log)) {
        failed = true;
        console.error("[smoke] FAIL — extension host log never mentions the extension; activation did not happen");
    } else {
        console.log("[smoke] OK — extension activated with no errors in extension host log");
    }
} finally {
    rmSync(profile, { recursive: true, force: true });
}

if (failed) process.exit(1);
