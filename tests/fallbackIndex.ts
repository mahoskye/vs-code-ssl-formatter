import * as path from 'path';
import { fileURLToPath } from 'url';
import Mocha from 'mocha';
import { glob } from 'glob';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

export function run(): Promise<void> {
    const mocha = new Mocha({
        ui: 'tdd',
        color: true,
        timeout: 20000
    });

    const testsRoot = path.resolve(__dirname, 'integration-fallback');

    return new Promise(async (resolve, reject) => {
        try {
            const files = await glob('**/*.test.js', { cwd: testsRoot });

            files.forEach(f => mocha.addFile(path.resolve(testsRoot, f)));

            try {
                mocha.run(failures => {
                    if (failures > 0) {
                        reject(new Error(`${failures} tests failed.`));
                    } else {
                        resolve();
                    }
                });
            } catch (err) {
                console.error(err);
                reject(err);
            }
        } catch (err) {
            reject(err);
        }
    });
}
