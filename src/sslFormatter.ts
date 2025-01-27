import * as vscode from "vscode";
import { FormatterPipeline, FormatterConfig } from "./formatters/formattingPipeline";
import { KeywordCasingFormatter } from "./formatters/keywordCasing";
import { SemicolonNewlineFormatter } from "./formatters/semicolonNewline";
import { OperatorSpacingFormatter } from "./formatters/operatorSpacing";
import { LongLineBreaker } from "./formatters/longLineBreaker";

interface SSLFormatterConfig {
    showTableView: boolean;
    showDetailedView: boolean;
    showInsights: boolean;
}

export class SSLFormatter implements vscode.DocumentFormattingEditProvider, vscode.Disposable {
    private pipeline: FormatterPipeline;
    private tableChannel: vscode.OutputChannel | undefined;
    private debugChannel: vscode.OutputChannel | undefined;
    private insightsChannel: vscode.OutputChannel | undefined;
    private config: SSLFormatterConfig;

    constructor() {
        // Initialize debug configuration from VS Code settings
        this.config = this.loadConfig();

        // Create pipeline with VS Code specific configuration
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
        this.pipeline.addFormatter(new LongLineBreaker(pipelineConfig.maxLineLength));

        // Listen for configuration changes
        vscode.workspace.onDidChangeConfiguration(this.handleConfigChange, this);
    }

    private loadConfig(): SSLFormatterConfig {
        const config = vscode.workspace.getConfiguration("sslFormatter.debug");
        return {
            showTableView: config.get("showTableView", true),
            showDetailedView: config.get("showDetailedView", true),
            showInsights: config.get("showInsights", true),
        };
    }

    private handleConfigChange(event: vscode.ConfigurationChangeEvent): void {
        if (event.affectsConfiguration("sslFormatter.debug")) {
            this.config = this.loadConfig();
        }
    }

    private getTableChannel(): vscode.OutputChannel {
        if (!this.tableChannel) {
            this.tableChannel = vscode.window.createOutputChannel("SSL Formatter Table");
        }
        return this.tableChannel;
    }

    private getDebugChannel(): vscode.OutputChannel {
        if (!this.debugChannel) {
            this.debugChannel = vscode.window.createOutputChannel("SSL Formatter Debug");
        }
        return this.debugChannel;
    }

    private getInsightsChannel(): vscode.OutputChannel {
        if (!this.insightsChannel) {
            this.insightsChannel = vscode.window.createOutputChannel("SSL Formatter Insights");
        }
        return this.insightsChannel;
    }

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

    public dispose(): void {
        this.tableChannel?.dispose();
        this.debugChannel?.dispose();
        this.insightsChannel?.dispose();
    }
}
