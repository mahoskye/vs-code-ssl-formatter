/**
 * Pure renderer for the SSL status-bar item.
 *
 * Lives in `utils/` rather than `sslStatusBar.ts` so unit tests can import
 * it without dragging in the language-client module graph through the
 * status-bar registration code.
 */

export interface StatusBarState {
    lspRunning: boolean;
    lspVersion: string | null;
    fnCount: number;
    classCount: number;
    inventoryReady: boolean;
}

export interface RenderedStatus {
    text: string;
    tooltip: string;
}

export function renderStatus(state: StatusBarState): RenderedStatus {
    const segments: string[] = ['$(symbol-namespace) SSL'];
    if (state.lspRunning) {
        segments.push(state.lspVersion ? `LSP ${state.lspVersion}` : 'LSP');
    } else {
        segments.push('native');
    }
    if (state.inventoryReady) {
        segments.push(`${state.fnCount} fns · ${state.classCount} classes`);
    }
    const text = segments.join(' · ');

    const tooltipLines = [
        `STARLIMS SSL extension`,
        state.lspRunning
            ? `Language server: running${state.lspVersion ? ` (${state.lspVersion})` : ''}`
            : `Language server: not running (using native fallback)`,
        state.inventoryReady
            ? `Inventory: ${state.fnCount} functions, ${state.classCount} classes (loaded from bundled LSP)`
            : `Inventory: hardcoded fallback subset`,
        `Click to open the SSL output channel.`
    ];
    return { text, tooltip: tooltipLines.join('\n') };
}
