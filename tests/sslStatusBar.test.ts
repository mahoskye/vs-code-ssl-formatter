import * as assert from 'assert';
import { describe, it } from 'mocha';
import { renderStatus, type StatusBarState } from '../src/utils/statusRenderer';

/**
 * The runtime renderer takes the server state (running, version) and
 * produces a status-bar text + tooltip. Tested as a pure function so we
 * don't have to drive the full vscode-languageclient lifecycle.
 */
describe('renderStatus (status-bar renderer)', () => {

    function s(overrides: Partial<StatusBarState> = {}): StatusBarState {
        return {
            lspRunning: true,
            lspVersion: '0.28.0',
            ...overrides
        };
    }

    it('shows LSP and the server version when running', () => {
        const out = renderStatus(s());
        assert.ok(out.text.includes('SSL'));
        assert.ok(out.text.includes('LSP 0.28.0'));
        assert.ok(out.tooltip.includes('Language server: running (starlims-lsp 0.28.0)'));
    });

    it('says the server is stopped, and how to retry, when it is not running', () => {
        const out = renderStatus(s({ lspRunning: false, lspVersion: null }));
        assert.ok(out.text.includes('server stopped'));
        assert.ok(!out.text.includes('LSP'));
        assert.ok(out.tooltip.includes('not running'));
        assert.ok(out.tooltip.includes('Restart Language Server'));
    });

    it('shows bare LSP when running without a version (rare)', () => {
        const out = renderStatus(s({ lspVersion: null }));
        assert.ok(/SSL · LSP$/.test(out.text), `expected bare LSP segment: ${out.text}`);
    });
});
