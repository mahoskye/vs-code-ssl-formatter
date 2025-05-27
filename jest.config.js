module.exports = {
    preset: 'ts-jest',
    testEnvironment: 'node',
    roots: ['<rootDir>/test'],
    testMatch: [
        '**/test/**/*.test.ts'
    ],
    transform: {
        '^.+\\.ts$': 'ts-jest',
    },
    collectCoverageFrom: [
        'src/**/*.ts',
        '!src/**/*.d.ts',
    ],
    moduleFileExtensions: ['ts', 'js'],
    setupFilesAfterEnv: ['<rootDir>/test/jest-setup.ts'],
    verbose: true,
    testTimeout: 30000,
    // Add these for debugging hanging tests
    forceExit: true,
    detectOpenHandles: true,
    // Module name mapping for VS Code API
    moduleNameMapper: {
        '^vscode$': '<rootDir>/test/__mocks__/vscode.ts'
    }
};