import * as vscode from "vscode";
import { CONFIG_KEYS, CONFIG_DEFAULTS } from "../constants/config";

export function registerConfigureNamespacesCommand(context: vscode.ExtensionContext): void {
	const disposable = vscode.commands.registerCommand("ssl.configureDocumentNamespaces", async () => {
		if (!vscode.workspace.workspaceFolders || vscode.workspace.workspaceFolders.length === 0) {
			vscode.window.showErrorMessage("Open a workspace folder before configuring document namespaces.");
			return;
		}

		const alias = await vscode.window.showInputBox({
			prompt: "Namespace alias (e.g., Reporting)",
			placeHolder: "Reporting",
			validateInput: value => value && value.trim().length > 0 ? undefined : "Namespace alias is required"
		});

		if (!alias) {
			return;
		}

		const folderSelections = await vscode.window.showOpenDialog({
			canSelectFiles: false,
			canSelectFolders: true,
			canSelectMany: false,
			openLabel: "Select namespace folder",
			defaultUri: vscode.workspace.workspaceFolders[0].uri
		});

		if (!folderSelections || folderSelections.length === 0) {
			return;
		}

		const targetUri = folderSelections[0];
		const relativePath = vscode.workspace.asRelativePath(targetUri, false);
		if (!relativePath || relativePath === targetUri.fsPath || relativePath.startsWith("..")) {
			vscode.window.showErrorMessage("Select a folder inside the current workspace.");
			return;
		}

		const normalizedPath = relativePath.replace(/\\/g, "/");
		const workspaceConfig = vscode.workspace.getConfiguration();
		const existing = workspaceConfig.get<Record<string, string>>(
			CONFIG_KEYS.DOCUMENT_NAMESPACES,
			CONFIG_DEFAULTS[CONFIG_KEYS.DOCUMENT_NAMESPACES] as Record<string, string>
		) || {};

		const updated = {
			...existing,
			[alias.trim()]: normalizedPath
		};

		await workspaceConfig.update(
			CONFIG_KEYS.DOCUMENT_NAMESPACES,
			updated,
			vscode.ConfigurationTarget.Workspace
		);

		vscode.window.showInformationMessage(`Mapped namespace "${alias}" to "${normalizedPath}".`);
	});

	context.subscriptions.push(disposable);
}
