import * as assert from 'assert';
import * as vscode from 'vscode';
import { describe, it, beforeEach } from 'mocha';
import { applyRuleOverrides } from '../src/utils/ruleOverrides';

/**
 * Verifies the diagnostic-batch transform that powers `ssl.diagnostics.rules`.
 * The middleware reads the user's rule-override map and either drops a
 * diagnostic (when value is "off") or remaps its severity. Anything not
 * matching an entry passes through unchanged.
 *
 * The map is read inside applyRuleOverrides via vscode.workspace
 * .getConfiguration('ssl').get('diagnostics.rules', {}); we drive that via
 * the mock workspace's configuration.update.
 */
describe('applyRuleOverrides middleware', () => {
    function diag(code: string, severity = vscode.DiagnosticSeverity.Warning): vscode.Diagnostic {
        const d = new vscode.Diagnostic(
            new vscode.Range(0, 0, 0, 1),
            `synthetic for ${code}`,
            severity
        );
        d.code = code;
        d.source = 'ssl-lsp';
        return d;
    }

    beforeEach(async () => {
        // Reset rules before each test so they don't bleed across describes.
        const config = vscode.workspace.getConfiguration('ssl');
        (config as any).update('diagnostics.rules', undefined);
    });

    it('passes diagnostics through unchanged when no overrides are set', async () => {
        const input = [diag('parameters_first'), diag('keyword_uppercase')];
        const out = applyRuleOverrides(input);
        assert.strictEqual(out.length, 2);
        assert.strictEqual(out[0], input[0], 'should reuse original diagnostic when not overridden');
    });

    it('drops diagnostics whose code is mapped to "off"', async () => {
        const config = vscode.workspace.getConfiguration('ssl');
        await config.update('diagnostics.rules', { keyword_uppercase: 'off' });

        const out = applyRuleOverrides([diag('keyword_uppercase'), diag('parameters_first')]);
        assert.strictEqual(out.length, 1);
        assert.strictEqual(out[0].code, 'parameters_first');
    });

    it('remaps severity for "info"/"warn"/"error"', async () => {
        const config = vscode.workspace.getConfiguration('ssl');
        await config.update('diagnostics.rules', {
            parameters_first: 'info',
            keyword_uppercase: 'error',
            prefer_exitcase: 'warning'
        });

        const out = applyRuleOverrides([
            diag('parameters_first'),
            diag('keyword_uppercase'),
            diag('prefer_exitcase')
        ]);

        const byCode = Object.fromEntries(out.map(d => [d.code as string, d.severity]));
        assert.strictEqual(byCode['parameters_first'], vscode.DiagnosticSeverity.Information);
        assert.strictEqual(byCode['keyword_uppercase'], vscode.DiagnosticSeverity.Error);
        assert.strictEqual(byCode['prefer_exitcase'], vscode.DiagnosticSeverity.Warning);
    });

    it('passes through unknown override values without dropping the diagnostic', async () => {
        const config = vscode.workspace.getConfiguration('ssl');
        await config.update('diagnostics.rules', { parameters_first: 'silly' });

        const out = applyRuleOverrides([diag('parameters_first', vscode.DiagnosticSeverity.Warning)]);
        assert.strictEqual(out.length, 1, 'unknown value should not silently drop');
        assert.strictEqual(out[0].severity, vscode.DiagnosticSeverity.Warning);
    });

    it('passes through diagnostics with no code, regardless of overrides', async () => {
        const config = vscode.workspace.getConfiguration('ssl');
        await config.update('diagnostics.rules', { parameters_first: 'off' });

        const noCode = new vscode.Diagnostic(
            new vscode.Range(0, 0, 0, 1),
            'no code attached',
            vscode.DiagnosticSeverity.Warning
        );
        const out = applyRuleOverrides([noCode]);
        assert.strictEqual(out.length, 1);
    });
});
