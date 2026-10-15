import * as vscode from "vscode";
import { Logger } from "./logger";

/**
 * A setting value the extension no longer offers, and its replacement.
 */
interface RetiredValue {
    /** Key under the `ssl` section, e.g. `format.sql.style`. */
    key: string;
    retired: string;
    replacement: string;
}

/**
 * Values removed from a setting's enum. `compact` was retired in the
 * 2026-08-28 SQL style review and starlims-lsp treats it as an alias for
 * `canonicalCompact` only until the extension stops offering it (#115).
 */
export const RETIRED_VALUES: ReadonlyArray<RetiredValue> = [
    { key: "format.sql.style", retired: "compact", replacement: "canonicalCompact" },
];

/** Returns the replacement for a retired value, or the value unchanged. */
export function replaceRetiredValue<T>(key: string, value: T): T {
    const match = RETIRED_VALUES.find(r => r.key === key && r.retired === value);
    return match ? (match.replacement as unknown as T) : value;
}

type Scope = "globalValue" | "workspaceValue" | "workspaceFolderValue";

/**
 * Rewrites any stored retired value to its replacement, at every scope where
 * it is set (user, workspace, and each workspace folder), and tells the user
 * once per activation. Failures are logged and otherwise ignored: callers map
 * retired values with `replaceRetiredValue` before using them, so an
 * unmigrated value still behaves correctly.
 */
export async function migrateRetiredSettings(): Promise<void> {
    const migrated: string[] = [];

    const migrate = async (
        config: vscode.WorkspaceConfiguration,
        scope: Scope,
        target: vscode.ConfigurationTarget,
        where: string
    ): Promise<void> => {
        for (const { key, retired, replacement } of RETIRED_VALUES) {
            if (config.inspect(key)?.[scope] !== retired) {
                continue;
            }
            try {
                await config.update(key, replacement, target);
                migrated.push(`ssl.${key}: "${retired}" → "${replacement}" (${where})`);
            } catch (error) {
                const message = error instanceof Error ? error.message : String(error);
                Logger.warn(`Could not migrate ssl.${key} in ${where} settings: ${message}`);
            }
        }
    };

    const config = vscode.workspace.getConfiguration("ssl");
    await migrate(config, "globalValue", vscode.ConfigurationTarget.Global, "user");
    await migrate(config, "workspaceValue", vscode.ConfigurationTarget.Workspace, "workspace");
    for (const folder of vscode.workspace.workspaceFolders ?? []) {
        await migrate(
            vscode.workspace.getConfiguration("ssl", folder.uri),
            "workspaceFolderValue",
            vscode.ConfigurationTarget.WorkspaceFolder,
            `folder ${folder.name}`
        );
    }

    if (migrated.length > 0) {
        Logger.info(`Migrated retired setting values: ${migrated.join("; ")}`);
        void vscode.window.showInformationMessage(
            `SSL: updated a retired setting value (${migrated.join("; ")}). The formatted output is unchanged.`
        );
    }
}
