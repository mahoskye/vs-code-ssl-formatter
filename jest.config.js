module.exports = {
    preset: 'ts-jest',
    testEnvironment: './test/failFastEnvironment.ts',
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
    // bail: 1, // Stop running tests after the first failure
    // Add these for debugging hanging tests
    // forceExit: true, // Commented out to see if Jest provides more info
    detectOpenHandles: true,
    // Module name mapping for VS Code API
    moduleNameMapper: {
        '^vscode$': '<rootDir>/test/__mocks__/vscode.ts'
    }
};