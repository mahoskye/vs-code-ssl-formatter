import * as vscode from 'vscode';
import { isClientRunning } from './lspClient';
import {
    getBuiltinFunctions,
    getBuiltinClasses,
    getInventorySourceVersion,
    isInventoryLoaded
} from './utils/inventory';
import { Logger } from './utils/logger';

/**
 * Compact status-bar item shown when an SSL document is the active editor.
 *
 * Conveys at a glance:
 *   - whether the LSP is running, or the extension is in native-fallback mode
 *   - the LSP version (when running)
 *   - whether the bundled-LSP-sourced inventory loaded (and how many entries)
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

    const lspRunning = isClientRunning();
    const lspVersion = getInventorySourceVersion();
    const fnCount = getBuiltinFunctions().length;
    const classCount = getBuiltinClasses().length;
    const inventoryReady = isInventoryLoaded();

    const segments: string[] = ['$(symbol-namespace) SSL'];
    if (lspRunning) {
        segments.push(lspVersion ? `LSP ${lspVersion}` : 'LSP');
    } else {
        segments.push('native');
    }
    if (inventoryReady) {
        segments.push(`${fnCount} fns · ${classCount} classes`);
    }
    item.text = segments.join(' · ');

    const tooltipLines = [
        `STARLIMS SSL extension`,
        lspRunning
            ? `Language server: running${lspVersion ? ` (${lspVersion})` : ''}`
            : `Language server: not running (using native fallback)`,
        inventoryReady
            ? `Inventory: ${fnCount} functions, ${classCount} classes (loaded from bundled LSP)`
            : `Inventory: hardcoded fallback subset`,
        `Click to open the SSL output channel.`
    ];
    item.tooltip = tooltipLines.join('\n');
    item.show();
}
