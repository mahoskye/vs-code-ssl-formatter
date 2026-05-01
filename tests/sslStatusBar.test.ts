import * as assert from 'assert';
import { describe, it } from 'mocha';
import { renderStatus, StatusBarState } from '../src/utils/statusRenderer';

/**
 * The runtime renderer pulls four state inputs (LSP running, LSP version,
 * inventory counts, inventory-ready flag) and produces a status-bar text
 * + tooltip. Tested as a pure function so we don't have to drive the
 * full vscode-languageclient lifecycle.
 */
describe('renderStatus (status-bar renderer)', () => {

    function s(overrides: Partial<StatusBarState> = {}): StatusBarState {
        return {
            lspRunning: true,
            lspVersion: 'v0.5.0',
            fnCount: 330,
            classCount: 29,
            inventoryReady: true,
            ...overrides
        };
    }

    it('shows LSP + version + inventory counts when fully loaded', () => {
        const out = renderStatus(s());
        assert.ok(out.text.includes('SSL'));
        assert.ok(out.text.includes('LSP v0.5.0'));
        assert.ok(out.text.includes('330 fns'));
        assert.ok(out.text.includes('29 classes'));
        assert.ok(out.tooltip.includes('Language server: running (v0.5.0)'));
    });

    it('falls back to "native" when LSP is not running', () => {
        const out = renderStatus(s({ lspRunning: false, lspVersion: null }));
        assert.ok(out.text.includes('native'));
        assert.ok(!out.text.includes('LSP v'));
        assert.ok(out.tooltip.includes('not running'));
        assert.ok(out.tooltip.includes('native fallback'));
    });

    it('omits inventory counts when the inventory load failed', () => {
        const out = renderStatus(s({ inventoryReady: false, fnCount: 53, classCount: 2 }));
        assert.ok(!out.text.includes(' fns'), `text should not include fn count: ${out.text}`);
        assert.ok(!out.text.includes(' classes'));
        assert.ok(out.tooltip.includes('hardcoded fallback subset'));
    });

    it('shows bare LSP when running without a version (rare)', () => {
        const out = renderStatus(s({ lspVersion: null }));
        assert.ok(/SSL · LSP\b/.test(out.text), `expected bare LSP segment: ${out.text}`);
        assert.ok(!out.text.includes('LSP v'));
    });
});
