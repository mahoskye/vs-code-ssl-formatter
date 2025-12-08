import * as vscode from "vscode";
import * as path from "path";
import { CONFIG_KEYS, CONFIG_DEFAULTS } from "../constants/config";
import { PATTERNS } from "../constants/patterns";

export function registerConfigureNamespacesCommand(context: vscode.ExtensionContext): void {
	const disposable = vscode.commands.registerCommand("ssl.configureDocumentNamespaces", async () => {
		if (!vscode.workspace.workspaceFolders || vscode.workspace.workspaceFolders.length === 0) {
			vscode.window.showErrorMessage("Open a workspace folder before configuring document namespaces.");
			return;
		}

		// 1. Select Folder First to suggest Alias
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
		// Use the workspace folder that contains the target URI to calculate relative path
		const workspaceFolder = vscode.workspace.getWorkspaceFolder(targetUri);

		if (!workspaceFolder) {
			vscode.window.showErrorMessage("Selected folder must be inside the current workspace.");
			return;
		}

		const relativePath = path.posix.relative(
			workspaceFolder.uri.fsPath.replace(/\\/g, "/"),
			targetUri.fsPath.replace(/\\/g, "/")
		);

		if (relativePath.startsWith("..") || path.isAbsolute(relativePath)) {
			// Should be covered by getWorkspaceFolder check, but double check
			vscode.window.showErrorMessage("Selected folder must be inside the current workspace.");
			return;
		}

		// Suggest folder name as alias
		const defaultAlias = path.basename(targetUri.fsPath);

		const alias = await vscode.window.showInputBox({
			prompt: "Namespace alias (e.g., Reporting)",
			placeHolder: defaultAlias || "Reporting",
			value: defaultAlias,
			validateInput: value => {
				if (!value || !value.trim()) {
					return "Namespace alias is required";
				}
				if (!PATTERNS.VALIDATION.VALID_IDENTIFIER.test(value.trim())) {
					return "Alias must be a valid identifier (alphanumeric, starts with letter/underscore)";
				}
				return undefined;
			}
		});

		if (!alias) {
			return;
		}

		const normalizedPath = relativePath.length === 0 ? "." : relativePath; // Handle root folder
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
