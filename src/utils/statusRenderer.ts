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
}

export interface RenderedStatus {
    text: string;
    tooltip: string;
}

export function renderStatus(state: StatusBarState): RenderedStatus {
    if (!state.lspRunning) {
        return {
            text: "$(warning) SSL · server stopped",
            tooltip: [
                "STARLIMS SSL extension",
                "Language server: not running, so SSL language features are unavailable.",
                "Run \"SSL: Restart Language Server\" to try again, or click to open the SSL output channel.",
            ].join("\n"),
        };
    }

    const version = state.lspVersion ? ` ${state.lspVersion}` : "";
    return {
        text: `$(symbol-namespace) SSL · LSP${version}`,
        tooltip: [
            "STARLIMS SSL extension",
            `Language server: running${state.lspVersion ? ` (starlims-lsp ${state.lspVersion})` : ""}`,
            "Click to open the SSL output channel.",
        ].join("\n"),
    };
}
