import * as path from 'path';
import * as os from 'os';
import * as fs from 'fs';
import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

/**
 * Gets the platform-specific binary name for the starlims-lsp server.
 */
function getServerBinaryName(): string {
    const platform = os.platform();
    const arch = os.arch();

    let platformStr: string;
    switch (platform) {
        case 'win32':
            platformStr = 'windows';
            break;
        case 'darwin':
            platformStr = 'darwin';
            break;
        case 'linux':
            platformStr = 'linux';
            break;
        default:
            throw new Error(`Unsupported platform: ${platform}`);
    }

    let archStr: string;
    switch (arch) {
        case 'x64':
            archStr = 'amd64';
            break;
        case 'arm64':
            archStr = 'arm64';
            break;
        default:
            throw new Error(`Unsupported architecture: ${arch}`);
    }

    const ext = platform === 'win32' ? '.exe' : '';
    return `starlims-lsp-${platformStr}-${archStr}${ext}`;
}

/**
 * Gets the full path to the server binary.
 */
function getServerPath(context: vscode.ExtensionContext): string {
    const binaryName = getServerBinaryName();
    return context.asAbsolutePath(path.join('server', binaryName));
}

/**
 * Creates server options for the language client.
 */
function createServerOptions(serverPath: string): ServerOptions {
    return {
        run: {
            command: serverPath,
            args: ['--stdio'],
            transport: TransportKind.stdio
        },
        debug: {
            command: serverPath,
            args: ['--stdio'],
            transport: TransportKind.stdio
        }
    };
}

/**
 * Gets the current SSL configuration formatted for the LSP server.
 */
function getSSLConfiguration(): object {
    const config = vscode.workspace.getConfiguration('ssl');
    const editorConfig = vscode.workspace.getConfiguration('editor');
    const indentStyle = config.get<string>('format.indentStyle', 'tab');
    const tabSizeValue = editorConfig.get<number | string>('tabSize', 4);
    const tabSize = typeof tabSizeValue === 'number' ? tabSizeValue : 4;
    const indentWidth = config.get<number>('format.indentWidth', 1);
    const indentSize = indentStyle === 'tab' ? tabSize : indentWidth;
    const hungarianPrefixes = config.get<string[]>('naming.hungarianNotation.prefixes', ['s', 'n', 'b', 'd', 'a', 'o', 'fn', 'v']);
    const rules = config.get<Record<string, string>>('diagnostics.rules', {});

    return {
        ssl: {
            format: {
                indentStyle,
                indentSize,
                maxLineLength: config.get<number>('format.wrapLength', 90),
                operatorSpacing: config.get<boolean>('format.operatorSpacing', true),
                commaSpacing: config.get<boolean>('format.commaSpacing', true),
                semicolonEnforcement: config.get<boolean>('format.semicolonEnforcement', true),
                blankLinesBetweenProcs: config.get<number>('format.blankLinesBetweenProcs', 1),
                sql: {
                    enabled: config.get<boolean>('format.sql.enabled', true),
                    style: config.get<string>('format.sql.style', 'canonicalCompact'),
                    keywordCase: config.get<string>('format.sql.keywordCase', 'upper'),
                    indentSize: config.get<number>('format.sql.indentSpaces', 4),
                    maxLineLength: config.get<number>('format.wrapLength', 90)
                }
            },
            diagnostics: {
                hungarianNotation: config.get<boolean>('naming.hungarianNotation.enabled', true),
                hungarianPrefixes,
                globals: config.get<string[]>('globals', []),
                maxBlockDepth: config.get<number>('styleGuide.limitBlockDepth', 4),
                rules
            },
            inlayHints: {
                enabled: config.get<boolean>('intellisense.inlayHints.enabled', true),
                minParameterCount: config.get<number>('intellisense.inlayHints.minParameterCount', 2)
            }
        }
    };
}

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
 */
function applyRuleOverrides(
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

/**
 * Creates client options for the language client.
 */
function createClientOptions(): LanguageClientOptions {
    return {
        documentSelector: [
            { scheme: 'file', language: 'ssl' },
            { scheme: 'untitled', language: 'ssl' }
        ],
        synchronize: {
            configurationSection: 'ssl',
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.{ssl,srvscr,ds}')
        },
        initializationOptions: getSSLConfiguration(),
        middleware: {
            handleDiagnostics: (uri, diagnostics, next) => {
                next(uri, applyRuleOverrides(diagnostics));
            }
        }
    };
}

/**
 * Starts the SSL Language Server client.
 * @param context The extension context.
 * @returns The started language client.
 */
export async function startClient(context: vscode.ExtensionContext): Promise<LanguageClient> {
    if (client) {
        return client;
    }

    const serverPath = getServerPath(context);

    // Check if server binary exists
    try {
        await vscode.workspace.fs.stat(vscode.Uri.file(serverPath));
    } catch {
        throw new Error(
            `SSL language server binary not found at ${serverPath}. ` +
            `Reinstall the extension or verify the server/ binaries are present.`
        );
    }

    if (os.platform() !== 'win32') {
        try {
            await fs.promises.access(serverPath, fs.constants.X_OK);
        } catch {
            throw new Error(
                `SSL language server binary is not executable at ${serverPath}. ` +
                `Run: chmod +x "${serverPath}"`
            );
        }
    }

    const serverOptions = createServerOptions(serverPath);
    const clientOptions = createClientOptions();

    client = new LanguageClient(
        'sslLanguageServer',
        'SSL Language Server',
        serverOptions,
        clientOptions
    );

    // Register configuration change listener
    context.subscriptions.push(
        vscode.workspace.onDidChangeConfiguration(e => {
            if (
                e.affectsConfiguration('ssl.format') ||
                e.affectsConfiguration('ssl.naming') ||
                e.affectsConfiguration('ssl.globals') ||
                e.affectsConfiguration('ssl.intellisense.inlayHints') ||
                e.affectsConfiguration('ssl.styleGuide') ||
                e.affectsConfiguration('ssl.diagnostics.rules')
            ) {
                sendConfigurationUpdate();
            }
        })
    );

    // Start the client
    await client.start();

    return client;
}

/**
 * Sends a configuration update to the server.
 */
function sendConfigurationUpdate(): void {
    if (client && client.isRunning()) {
        client.sendNotification('workspace/didChangeConfiguration', {
            settings: getSSLConfiguration()
        });
    }
}

/**
 * Stops the SSL Language Server client.
 */
export async function stopClient(): Promise<void> {
    if (client) {
        await client.stop();
        client = undefined;
    }
}

/**
 * Restarts the SSL Language Server client.
 * @param context The extension context.
 */
export async function restartClient(context: vscode.ExtensionContext): Promise<void> {
    await stopClient();
    await startClient(context);
}

/**
 * Gets the current language client instance.
 */
export function getClient(): LanguageClient | undefined {
    return client;
}

/**
 * Checks if the language client is running.
 */
export function isClientRunning(): boolean {
    return client !== undefined && client.isRunning();
}
