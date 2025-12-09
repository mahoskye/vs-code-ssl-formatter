import * as path from 'path';
import Mocha = require('mocha');
import { glob } from 'glob';

export function run(): Promise<void> {
    // Create the mocha test
    const mocha = new Mocha({
        ui: 'tdd',
        color: true,
        timeout: 20000 // Increase timeout for integration tests
    });

    const testsRoot = path.resolve(__dirname, 'integration');

    return new Promise(async (resolve, reject) => {
        try {
            const files = await glob('**/*.test.js', { cwd: testsRoot });

            // Add files to the test suite
            files.forEach(f => mocha.addFile(path.resolve(testsRoot, f)));

            try {
                // Run the mocha test
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
