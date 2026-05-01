import * as path from 'path';
import { runTests } from '@vscode/test-electron';

async function main() {
    try {
        const extensionDevelopmentPath = path.resolve(__dirname, '../../');
        const extensionTestsPath = path.resolve(__dirname, './fallbackIndex');
        const fallbackWorkspacePath = path.resolve(extensionDevelopmentPath, 'tests/fallback-workspace');
        const fallbackUserDataPath = path.resolve(extensionDevelopmentPath, 'tests/fallback-user-data');
        const fallbackExtensionsPath = path.resolve(extensionDevelopmentPath, 'tests/fallback-extensions');

        await runTests({
            extensionDevelopmentPath,
            extensionTestsPath,
            launchArgs: [
                '--disable-workspace-trust',
                '--user-data-dir',
                fallbackUserDataPath,
                '--extensions-dir',
                fallbackExtensionsPath,
                fallbackWorkspacePath
            ]
        });
    } catch (err) {
        console.error('Failed to run fallback tests');
        process.exit(1);
    }
}

main();
