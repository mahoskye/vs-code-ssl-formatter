import { pathToFileURL } from 'url';
import { resolve as pathResolve } from 'path';

const mockUrl = pathToFileURL(pathResolve(process.cwd(), 'tests/setup.ts')).href;

export async function resolve(specifier, context, defaultResolve) {
    if (specifier === 'vscode') {
        return { url: mockUrl, shortCircuit: true };
    }
    return defaultResolve(specifier, context, defaultResolve);
}
