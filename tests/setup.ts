/**
 * Test setup file - runs before all tests
 * Mocks the VSCode API for unit testing
 */

import { Module } from 'module';
import {
	MockPosition,
	MockRange,
	MockTextEdit,
	MockUri,
	MockEndOfLine,
	MockDiagnosticSeverity,
	MockDiagnostic,
	MockDiagnosticCollection,
	createSSLConfig,
	MockWorkspaceConfiguration
} from './helpers/mockVSCode';

// Create the mock vscode module
const mockVscode = {
	workspace: {
		configuration: createSSLConfig(),
		getConfiguration(section?: string): MockWorkspaceConfiguration {
			return this.configuration;
		}
	},
	Range: MockRange,
	Position: MockPosition,
	TextEdit: MockTextEdit,
	Uri: MockUri,
	EndOfLine: MockEndOfLine,
	DiagnosticSeverity: MockDiagnosticSeverity,
	Diagnostic: MockDiagnostic,
	languages: {
		createDiagnosticCollection(name: string): MockDiagnosticCollection {
			return new MockDiagnosticCollection();
		},
		getDiagnostics(uri: MockUri): MockDiagnostic[] {
			return [];
		}
	}
};

// Override require() to return our mock when 'vscode' is imported
const originalRequire = Module.prototype.require;
(Module.prototype.require as any) = function(id: string) {
	if (id === 'vscode') {
		return mockVscode;
	}
	return originalRequire.apply(this, arguments as any);
};
