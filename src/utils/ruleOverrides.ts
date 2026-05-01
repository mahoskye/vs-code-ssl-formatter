import * as vscode from 'vscode';

/**
 * Reads the per-rule severity-override map from settings. Keys are LSP rule
 * slugs (e.g. `parameters_first`); values are `"off" | "info" | "warn" | "error"`.
 */
function getRuleOverrides(): Record<string, string> {
    return vscode.workspace
        .getConfiguration('ssl')
        .get<Record<string, string>>('diagnostics.rules', {});
}

function diagnosticCodeString(diag: vscode.Diagnostic): string | undefined {
    const code = diag.code;
    if (typeof code === 'string') {
        return code;
    }
    if (typeof code === 'number') {
        return String(code);
    }
    if (code && typeof code === 'object' && 'value' in code) {
        const v = (code as { value: string | number }).value;
        return typeof v === 'string' ? v : String(v);
    }
    return undefined;
}

const SEVERITY_MAP: Record<string, vscode.DiagnosticSeverity | 'off'> = {
    off: 'off',
    info: vscode.DiagnosticSeverity.Information,
    warn: vscode.DiagnosticSeverity.Warning,
    warning: vscode.DiagnosticSeverity.Warning,
    error: vscode.DiagnosticSeverity.Error,
};

/**
 * Applies per-rule overrides to an incoming batch of LSP diagnostics. Rules
 * mapped to `off` are dropped; other mappings replace the LSP-supplied
 * severity. Diagnostics with no `code` (or with a code not in the override
 * map) pass through unchanged.
 *
 * This runs on the client even if the LSP doesn't yet honor the
 * `ssl.diagnostics.rules` initializationOption, so users get immediate
 * filtering without waiting on upstream.
 *
 * Pure with respect to vscode-languageclient — kept out of `lspClient.ts`
 * so unit tests can exercise it without resolving the language-client
 * package through the ts-node ESM loader chain.
 */
export function applyRuleOverrides(
    diagnostics: vscode.Diagnostic[]
): vscode.Diagnostic[] {
    const overrides = getRuleOverrides();
    if (!overrides || Object.keys(overrides).length === 0) {
        return diagnostics;
    }
    const result: vscode.Diagnostic[] = [];
    for (const diag of diagnostics) {
        const code = diagnosticCodeString(diag);
        if (!code) {
            result.push(diag);
            continue;
        }
        const override = overrides[code];
        if (!override) {
            result.push(diag);
            continue;
        }
        const mapped = SEVERITY_MAP[override.toLowerCase()];
        if (mapped === undefined) {
            // Unknown override value — pass through rather than silently drop.
            result.push(diag);
            continue;
        }
        if (mapped === 'off') {
            continue;
        }
        const cloned = new vscode.Diagnostic(diag.range, diag.message, mapped);
        cloned.code = diag.code;
        cloned.source = diag.source;
        cloned.tags = diag.tags;
        cloned.relatedInformation = diag.relatedInformation;
        result.push(cloned);
    }
    return result;
}
