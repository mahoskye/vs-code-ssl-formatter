import * as vscode from "vscode";

export class SSLHoverProvider implements vscode.HoverProvider {
    public provideHover(
        document: vscode.TextDocument,
        position: vscode.Position,
        token: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.Hover> {
        const range = document.getWordRangeAtPosition(position);
        const word = document.getText(range).replace(/\(\)$/, "");

        // Fetch hover data from the sslFormatter configuration
        const hoverData = vscode.workspace.getConfiguration("sslFormatter").get<any>("completions", {});

        // Loop through each section in the configuration
        for (const section in hoverData) {
            const completions = hoverData[section];
            const foundCompletion = completions.find((item: any) => item.label.replace(/\(\)$/, "") === word);

            if (foundCompletion) {
                const markdownString = new vscode.MarkdownString(foundCompletion.documentation);
                markdownString.isTrusted = true;
                return new vscode.Hover(markdownString);
            }
        }

        return undefined; // No hover info found for this word
    }
}
