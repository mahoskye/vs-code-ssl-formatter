import * as assert from 'assert';
import * as vscode from 'vscode';
import { migrateRetiredSettings } from '../../src/utils/settingsMigration';

suite('Settings Migration Integration Test', () => {
    const key = 'format.sql.style';

    teardown(async () => {
        await vscode.workspace.getConfiguration('ssl').update(key, undefined, vscode.ConfigurationTarget.Global);
    });

    test('Migrates a stored compact SQL style to canonicalCompact', async () => {
        const config = vscode.workspace.getConfiguration('ssl');
        await config.update(key, 'compact', vscode.ConfigurationTarget.Global);
        assert.strictEqual(vscode.workspace.getConfiguration('ssl').inspect(key)?.globalValue, 'compact');

        await migrateRetiredSettings();

        assert.strictEqual(
            vscode.workspace.getConfiguration('ssl').inspect(key)?.globalValue,
            'canonicalCompact',
            'The retired value should be rewritten at the scope where it was set'
        );
    });

    test('Leaves a current SQL style alone', async () => {
        await vscode.workspace.getConfiguration('ssl').update(key, 'expanded', vscode.ConfigurationTarget.Global);

        await migrateRetiredSettings();

        assert.strictEqual(vscode.workspace.getConfiguration('ssl').inspect(key)?.globalValue, 'expanded');
    });
});
