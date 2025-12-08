import * as vscode from 'vscode';

/**
 * Log levels for the SSL extension
 */
export enum LogLevel {
    OFF = 0,
    ERROR = 1,
    WARN = 2,
    INFO = 3,
    DEBUG = 4
}

/**
 * Logger utility for the SSL extension
 * Provides structured logging with configurable log levels
 */
export class Logger {
    private static outputChannel: vscode.OutputChannel | null = null;
    private static logLevel: LogLevel = LogLevel.OFF;

    /**
     * Initialize the logger with an output channel
     */
    static initialize(context: vscode.ExtensionContext): void {
        if (!this.outputChannel) {
            this.outputChannel = vscode.window.createOutputChannel('SSL Extension');
            context.subscriptions.push(this.outputChannel);
        }
        this.updateLogLevel();
    }

    /**
     * Update log level from configuration
     */
    static updateLogLevel(): void {
        const config = vscode.workspace.getConfiguration('ssl');
        const traceLevel = config.get<string>('trace.server', 'off');

        switch (traceLevel) {
            case 'verbose':
                this.logLevel = LogLevel.DEBUG;
                break;
            case 'messages':
                this.logLevel = LogLevel.INFO;
                break;
            case 'off':
            default:
                this.logLevel = LogLevel.OFF;
                break;
        }
    }

    /**
     * Log a debug message
     */
    static debug(message: string, ...args: any[]): void {
        this.log(LogLevel.DEBUG, message, ...args);
    }

    /**
     * Log an info message
     */
    static info(message: string, ...args: any[]): void {
        this.log(LogLevel.INFO, message, ...args);
    }

    /**
     * Log a warning message
     */
    static warn(message: string, ...args: any[]): void {
        this.log(LogLevel.WARN, message, ...args);
    }

    /**
     * Log an error message
     */
    static error(message: string, ...args: any[]): void {
        this.log(LogLevel.ERROR, message, ...args);
    }

    /**
     * Internal log method
     */
    private static log(level: LogLevel, message: string, ...args: any[]): void {
        if (level > this.logLevel || !this.outputChannel) {
            return;
        }

        const timestamp = new Date().toISOString();
        const levelName = LogLevel[level];
        const formattedArgs = args.length > 0 ? ` ${JSON.stringify(args)}` : '';
        const logMessage = `[${timestamp}] [${levelName}] ${message}${formattedArgs}`;

        this.outputChannel.appendLine(logMessage);
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
