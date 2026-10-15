import * as vscode from 'vscode';
import { getServerVersion, isClientRunning } from './lspClient';
import { Logger } from './utils/logger';
import { renderStatus } from './utils/statusRenderer';

/**
 * Compact status-bar item shown when an SSL document is the active editor.
 *
 * Conveys at a glance:
 *   - whether the language server is running
 *   - the server version it reports (when running)
 *
 * Clicking opens the SSL output channel so users can see startup logs.
 */
const SSL_LANGUAGE_ID = 'ssl';
const SHOW_OUTPUT_COMMAND = 'ssl.showStatusOutput';

let item: vscode.StatusBarItem | undefined;

export function registerStatusBar(context: vscode.ExtensionContext): void {
    item = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 80);
    item.command = SHOW_OUTPUT_COMMAND;
    context.subscriptions.push(item);

    context.subscriptions.push(
        vscode.commands.registerCommand(SHOW_OUTPUT_COMMAND, () => Logger.show())
    );
    context.subscriptions.push(
        vscode.window.onDidChangeActiveTextEditor(() => refresh())
    );

    refresh();
    // The inventory loads asynchronously after activation; refresh shortly
    // after so the entry counts populate without requiring an editor switch.
    const t = setTimeout(refresh, 1500);
    context.subscriptions.push({ dispose: () => clearTimeout(t) });
}

export function refreshStatusBar(): void {
    refresh();
}

function refresh(): void {
    if (!item) {
        return;
    }
    const editor = vscode.window.activeTextEditor;
    if (!editor || editor.document.languageId !== SSL_LANGUAGE_ID) {
        item.hide();
        return;
    }

    const rendered = renderStatus({
        lspRunning: isClientRunning(),
        lspVersion: getServerVersion() ?? null
    });
    item.text = rendered.text;
    item.tooltip = rendered.tooltip;
    item.show();
}
