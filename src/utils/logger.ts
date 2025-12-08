import * as vscode from 'vscode';

/**
 * Log levels for the SSL extension configuration
 */
enum ConfigLogLevel {
    Off,
    Info,
    Debug
}

/**
 * Logger utility for the SSL extension
 * Wraps VS Code's native LogOutputChannel with config-based filtering
 */
export class Logger {
    private static outputChannel: vscode.LogOutputChannel | null = null;
    private static configLevel: ConfigLogLevel = ConfigLogLevel.Off;

    /**
     * Initialize the logger with a LogOutputChannel
     */
    static initialize(context: vscode.ExtensionContext): void {
        if (!this.outputChannel) {
            // Create a LogOutputChannel (requires VS Code 1.66+)
            this.outputChannel = vscode.window.createOutputChannel('SSL Extension', { log: true });
            context.subscriptions.push(this.outputChannel);
        }
        this.updateLogLevel();

        // Listen for configuration changes to update log level dynamically
        context.subscriptions.push(
            vscode.workspace.onDidChangeConfiguration(e => {
                if (e.affectsConfiguration('ssl.trace.server')) {
                    this.updateLogLevel();
                }
            })
        );
    }

    /**
     * Update log level from configuration
     */
    static updateLogLevel(): void {
        const config = vscode.workspace.getConfiguration('ssl');
        const traceLevel = config.get<string>('trace.server', 'off');

        switch (traceLevel) {
            case 'verbose':
                this.configLevel = ConfigLogLevel.Debug;
                break;
            case 'messages':
                this.configLevel = ConfigLogLevel.Info;
                break;
            case 'off':
            default:
                this.configLevel = ConfigLogLevel.Off;
                break;
        }
    }

    /**
     * Log a debug message
     */
    static debug(message: string, ...args: any[]): void {
        if (this.configLevel >= ConfigLogLevel.Debug) {
            this.outputChannel?.debug(this.formatMessage(message, args));
        }
    }

    /**
     * Log an info message
     */
    static info(message: string, ...args: any[]): void {
        if (this.configLevel >= ConfigLogLevel.Info) {
            this.outputChannel?.info(this.formatMessage(message, args));
        }
    }

    /**
     * Log a warning message
     */
    static warn(message: string, ...args: any[]): void {
        if (this.configLevel >= ConfigLogLevel.Info) {
            this.outputChannel?.warn(this.formatMessage(message, args));
        }
    }

    /**
     * Log an error message
     */
    static error(message: string | Error, ...args: any[]): void {
        if (this.configLevel >= ConfigLogLevel.Info) {
            this.outputChannel?.error(this.formatMessage(message, args));
        }
    }

    /**
     * Format message with args
     */
    private static formatMessage(message: string | Error, args: any[]): string {
        const msgStr = message instanceof Error ? message.message : message;
        if (args.length > 0) {
            return `${msgStr} ${args.map(a => typeof a === 'object' ? JSON.stringify(a) : String(a)).join(' ')}`;
        }
        return msgStr;
    }

    /**
     * Show the output channel
     */
    static show(): void {
        this.outputChannel?.show();
    }

    /**
     * Dispose the logger
     */
    static dispose(): void {
        this.outputChannel?.dispose();
        this.outputChannel = null;
    }
}
