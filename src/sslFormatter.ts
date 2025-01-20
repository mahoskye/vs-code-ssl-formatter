import * as vscode from "vscode";
import { FormatterPipeline, FormatterConfig } from "./formatters/formattingPipeline";
import { KeywordCasingFormatter } from "./formatters/keywordCasing";
import { SemicolonNewlineFormatter } from "./formatters/semicolonNewline";
// Import other formatters as they're converted

export class SSLFormatter implements vscode.DocumentFormattingEditProvider {
    private pipeline: FormatterPipeline;
    private tableChannel: vscode.OutputChannel | undefined;
    private debugChannel: vscode.OutputChannel | undefined;

    constructor() {
        // Create pipeline with VS Code specific configuration
        const config: FormatterConfig = {
            debug: false,
            showSegmentDetails: false,
            useSpacingContext: true,
            useSpacingPostProcessing: true,
            preserveUserSpacing: false,
            maxConsecutiveBlankLines: 2,
            maxLineLength: 90,
            tabSize: 4,
            indentStyle: "space",
        };

        this.pipeline = new FormatterPipeline(config);

        // Add formatters in desired order
        this.pipeline.addFormatter(new KeywordCasingFormatter());
        this.pipeline.addFormatter(new SemicolonNewlineFormatter());
        // Add other formatters as they're converted:
        // this.pipeline.addFormatter(new OperatorSpacingFormatter());
        // this.pipeline.addFormatter(new LineBreakFormatter());
        // this.pipeline.addFormatter(new IndentationFormatter());
        // this.pipeline.addFormatter(new LineSpacingFormatter());
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
                // Show debug info in table format
                const tableChannel = this.getTableChannel();
                tableChannel.clear();
                tableChannel.appendLine(result.tableView);
                tableChannel.show(true);

                // Show debug info in detail format
                const debugChannel = this.getDebugChannel();
                debugChannel.clear();
                debugChannel.appendLine(result.detailedView);
                debugChannel.show(false);

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
     * Update pipeline configuration based on VS Code formatting options
     */
    private updateConfig(options: vscode.FormattingOptions): void {
        const config = this.pipeline.getConfig();

        // Update relevant options
        config.tabSize = options.tabSize;
        config.indentStyle = options.insertSpaces ? "space" : "tab";

        // Could add more VS Code specific configuration here
    }
}
