import * as vscode from "vscode";
import { FormatterPipeline, FormatterConfig } from "./formatters/formattingPipeline";
import { KeywordCasingFormatter } from "./formatters/keywordCasing";
import { SemicolonNewlineFormatter } from "./formatters/semicolonNewline";
import { OperatorSpacingFormatter } from "./formatters/operatorSpacing";
import { LineSplitter } from "./formatters/lineSplitter";

/**
 * Configuration options for the SSL Formatter.
 */
interface SSLFormatterConfig {
    /**
     * Indicates whether the table view should be displayed.
     */
    showTableView: boolean;

    /**
     * Indicates whether the detailed view should be displayed.
     */
    showDetailedView: boolean;

    /**
     * Indicates whether insights should be displayed.
     */
    showInsights: boolean;
}

/**
 * SSLFormatter class implements the vscode.DocumentFormattingEditProvider and vscode.Disposable interfaces.
 * It provides document formatting capabilities and manages output channels for debugging and insights.
 */
export class SSLFormatter implements vscode.DocumentFormattingEditProvider, vscode.Disposable {
    private pipeline: FormatterPipeline;
    private tableChannel: vscode.OutputChannel | undefined;
    private debugChannel: vscode.OutputChannel | undefined;
    private insightsChannel: vscode.OutputChannel | undefined;
    private config: SSLFormatterConfig;

    /**
     * Initializes a new instance of the class.
     *
     * - Loads the debug configuration from VS Code settings.
     * - Creates a formatter pipeline with the specified configuration.
     * - Adds formatters to the pipeline in the desired order.
     * - Listens for configuration changes in the VS Code workspace.
     *
     * @constructor
     */
    constructor() {
        // Initialize debug configuration from VS Code settings
        this.config = this.loadConfig();

        /**
         * Configuration object for the SSL Formatter pipeline.
         *
         * @property {boolean} debug - Enables or disables debug mode.
         * @property {boolean} showSegmentDetails - Determines whether to show segment details.
         * @property {boolean} useSpacingContext - Enables or disables the use of spacing context.
         * @property {boolean} useSpacingPostProcessing - Enables or disables post-processing of spacing.
         * @property {boolean} preserveUserSpacing - Determines whether to preserve user-defined spacing.
         * @property {number} maxConsecutiveBlankLines - Sets the maximum number of consecutive blank lines allowed.
         * @property {number} maxLineLength - Sets the maximum length of a line.
         * @property {number} tabSize - Defines the number of spaces per tab.
         * @property {"space" | "tab"} indentStyle - Specifies the indentation style, either "space" or "tab".
         */
        const pipelineConfig: FormatterConfig = {
            debug: true,
            showSegmentDetails: true,
            useSpacingContext: true,
            useSpacingPostProcessing: true,
            preserveUserSpacing: false,
            maxConsecutiveBlankLines: 2,
            maxLineLength: 90,
            tabSize: 4,
            indentStyle: "space",
        };

        this.pipeline = new FormatterPipeline(pipelineConfig);

        // Add formatters in desired order
        // this.pipeline.addFormatter(new KeywordCasingFormatter());
        // this.pipeline.addFormatter(new SemicolonNewlineFormatter(this.pipeline));
        // this.pipeline.addFormatter(new OperatorSpacingFormatter());
        this.pipeline.addFormatter(new LineSplitter(pipelineConfig.maxLineLength ?? 90, 6));

        // Listen for configuration changes
        vscode.workspace.onDidChangeConfiguration(this.handleConfigChange, this);
    }

    /**
     * Loads the SSL Formatter configuration from the VS Code workspace settings.
     *
     * @returns {SSLFormatterConfig} The configuration object containing the following properties:
     * - `showTableView`: A boolean indicating whether to show the table view. Defaults to `true`.
     * - `showDetailedView`: A boolean indicating whether to show the detailed view. Defaults to `true`.
     * - `showInsights`: A boolean indicating whether to show insights. Defaults to `true`.
     */
    private loadConfig(): SSLFormatterConfig {
        const config = vscode.workspace.getConfiguration("sslFormatter.debug");
        return {
            showTableView: config.get("showTableView", true),
            showDetailedView: config.get("showDetailedView", true),
            showInsights: config.get("showInsights", true),
        };
    }

    /**
     * Handles changes to the extension's configuration.
     *
     * @param event - The configuration change event.
     * @remarks
     * This method checks if the "sslFormatter.debug" configuration has been changed.
     * If it has, the method reloads the configuration settings.
     */
    private handleConfigChange(event: vscode.ConfigurationChangeEvent): void {
        if (event.affectsConfiguration("sslFormatter.debug")) {
            this.config = this.loadConfig();
        }
    }

    /**
     * Retrieves the output channel for the SSL Formatter Table.
     * If the channel does not already exist, it creates a new one.
     *
     * @returns {vscode.OutputChannel} The output channel for the SSL Formatter Table.
     */
    private getTableChannel(): vscode.OutputChannel {
        if (!this.tableChannel) {
            this.tableChannel = vscode.window.createOutputChannel("SSL Formatter Table");
        }
        return this.tableChannel;
    }

    /**
     * Retrieves the debug output channel for the SSL Formatter.
     * If the debug channel does not already exist, it creates a new one.
     *
     * @returns {vscode.OutputChannel} The debug output channel.
     */
    private getDebugChannel(): vscode.OutputChannel {
        if (!this.debugChannel) {
            this.debugChannel = vscode.window.createOutputChannel("SSL Formatter Debug");
        }
        return this.debugChannel;
    }

    /**
     * Retrieves the insights output channel for the SSL Formatter.
     * If the channel does not already exist, it creates a new one.
     *
     * @returns {vscode.OutputChannel} The insights output channel.
     */
    private getInsightsChannel(): vscode.OutputChannel {
        if (!this.insightsChannel) {
            this.insightsChannel = vscode.window.createOutputChannel("SSL Formatter Insights");
        }
        return this.insightsChannel;
    }

    /**
     * Provides formatting edits for a given document.
     *
     * This method is called by the VS Code editor to format the entire document.
     * It processes the document text through a formatting pipeline and returns the
     * formatted text as a series of text edits.
     *
     * If the formatting pipeline returns debug information instead of a formatted
     * string, this method will handle the debug output based on the configuration
     * settings and return an empty array of text edits.
     *
     * @param document - The document to format.
     * @param options - Formatting options such as tab size and insert spaces.
     * @param token - A cancellation token.
     * @returns A promise that resolves to an array of text edits to apply to the document.
     *          If an error occurs during formatting, an error message is shown and an empty
     *          array is returned.
     */
    public async provideDocumentFormattingEdits(
        document: vscode.TextDocument,
        options: vscode.FormattingOptions,
        token: vscode.CancellationToken
    ): Promise<vscode.TextEdit[]> {
        try {
            const text = document.getText();
            const result = await this.pipeline.process(text);

            // Handle debug output
            if (typeof result !== "string") {
                // Show debug info in table format if enabled
                if (this.config.showTableView) {
                    const tableChannel = this.getTableChannel();
                    tableChannel.clear();
                    tableChannel.appendLine(result.tableView);
                    tableChannel.show(true);
                }

                // Show debug info in detail format if enabled
                if (this.config.showDetailedView) {
                    const debugChannel = this.getDebugChannel();
                    debugChannel.clear();
                    debugChannel.appendLine(result.detailedView);
                    debugChannel.show(false);
                }

                // Show formatter insights if enabled
                if (this.config.showInsights) {
                    const insightsChannel = this.getInsightsChannel();
                    insightsChannel.clear();
                    if (result.blocks.some((b) => b.formatterInsights?.length)) {
                        insightsChannel.appendLine("Formatter Insights:");
                        insightsChannel.appendLine("==================");

                        result.blocks.forEach((block, blockIndex) => {
                            if (block.formatterInsights && block.formatterInsights.length > 0) {
                                insightsChannel.appendLine(`\nBlock ${blockIndex + 1}:`);
                                block.formatterInsights.forEach((insight) => {
                                    insightsChannel.appendLine(`  ${insight.formatterName}:`);
                                    insightsChannel.appendLine(
                                        `    Line ${insight.sourceLineNumber}: ${insight.description}`
                                    );
                                    insightsChannel.appendLine(`    Before: "${insight.before}"`);
                                    insightsChannel.appendLine(`    After:  "${insight.after}"`);
                                });
                            }
                        });
                        insightsChannel.show(false);
                    }
                }

                // Return original text when in debug mode
                return [];
            }

            // Normal formatting
            const lastLineId = document.lineCount - 1;
            const lastLineLength = document.lineAt(lastLineId).text.length;
            const range = new vscode.Range(0, 0, lastLineId, lastLineLength);
            return [vscode.TextEdit.replace(range, result)];
        } catch (error) {
            vscode.window.showErrorMessage(
                `Formatting error: ${error instanceof Error ? error.message : String(error)}`
            );
            return [];
        }
    }

    /**
     * Disposes of the resources used by the SSL formatter.
     * This includes the table channel, debug channel, and insights channel.
     * Each channel is disposed of if it has been initialized.
     */
    public dispose(): void {
        this.tableChannel?.dispose();
        this.debugChannel?.dispose();
        this.insightsChannel?.dispose();
    }
}
