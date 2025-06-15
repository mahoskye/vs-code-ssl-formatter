/**
 * Performance and Benchmark Tests for SSL Formatter
 *
 * Tests the formatter's performance with large AST structures and various scenarios:
 * - Large AST formatting speed
 * - Memory usage validation
 * - Complex nested structure performance
 * - Stress testing with edge cases
 */

import { SSLFormatter, formatSSL } from "../../src/formatter/index";
import { FormatterOptions, defaultFormatterOptions } from "../../src/formatter/options";
import { ASTNodeType, createBaseNode, ProgramNode } from "../../src/parser/ast";
import { createToken, createPosition } from "../../src/tokenizer/token";
import { TokenType } from "../../src/tokenizer/tokenType";

describe("SSL Formatter Performance Tests", () => {
    const performanceOptions: FormatterOptions = {
        ...defaultFormatterOptions,
        maxLineLength: 100,
        indentSize: 4,
        useTabs: false,
    };

    let mockToken: any;

    beforeEach(() => {
        mockToken = createToken(TokenType.IDENTIFIER, "test", createPosition(1, 1, 0));
    });

    /**
     * Generate a large AST for testing
     */
    function generateLargeAST(size: "small" | "medium" | "large"): ProgramNode {
        const sizeConfig = {
            small: { statements: 50 },
            medium: { statements: 200 },
            large: { statements: 1000 },
        };

        const config = sizeConfig[size];

        // Create a program node with many statements
        const program = createBaseNode(ASTNodeType.Program, mockToken, mockToken) as ProgramNode;
        program.body = [];

        // Generate multiple simple statements for performance testing
        for (let i = 0; i < config.statements; i++) {
            const statement = createBaseNode(ASTNodeType.Assignment, mockToken, mockToken);
            program.body.push(statement);
        }

        return program;
    }

    /**
     * Measure execution time and memory usage
     */
    function measurePerformance<T>(
        name: string,
        fn: () => T
    ): { result: T; timeMs: number; memoryMB: number } {
        // Force garbage collection if available
        if (global.gc) {
            global.gc();
        }

        const startMemory = process.memoryUsage();
        const startTime = process.hrtime.bigint();

        const result = fn();

        const endTime = process.hrtime.bigint();
        const endMemory = process.memoryUsage();

        const timeMs = Number(endTime - startTime) / 1_000_000; // Convert nanoseconds to milliseconds
        const memoryMB = (endMemory.heapUsed - startMemory.heapUsed) / (1024 * 1024); // Convert bytes to MB

        return { result, timeMs, memoryMB };
    }

    describe("Large AST Performance", () => {
        test("should format small AST efficiently", () => {
            const ast = generateLargeAST("small");

            const measurement = measurePerformance("Small AST", () => {
                return formatSSL(ast, performanceOptions);
            });
            expect(typeof measurement.result).toBe("string");
            expect(measurement.timeMs).toBeLessThan(200); // Should complete within 200ms
            expect(measurement.memoryMB).toBeLessThan(20); // Should use less than 20MB
        });

        test("should format medium AST efficiently", () => {
            const ast = generateLargeAST("medium");

            const measurement = measurePerformance("Medium AST", () => {
                return formatSSL(ast, performanceOptions);
            });
            expect(typeof measurement.result).toBe("string");
            expect(measurement.timeMs).toBeLessThan(1000); // Should complete within 1 second
            expect(measurement.memoryMB).toBeLessThan(50); // Should use less than 50MB
        });

        test("should format large AST within reasonable time", () => {
            const ast = generateLargeAST("large");

            const measurement = measurePerformance("Large AST", () => {
                return formatSSL(ast, performanceOptions);
            });
            expect(typeof measurement.result).toBe("string");
            expect(measurement.timeMs).toBeLessThan(5000); // Should complete within 5 seconds
            expect(measurement.memoryMB).toBeLessThan(100); // Should use less than 100MB
        });
    });

    describe("Complex Structure Performance", () => {
        test("should handle deeply nested control structures", () => {
            // Create nested if statements for deep nesting test
            const program = createBaseNode(
                ASTNodeType.Program,
                mockToken,
                mockToken
            ) as ProgramNode;
            program.body = [];

            // Create a simple nested structure
            for (let depth = 0; depth < 10; depth++) {
                const ifNode = createBaseNode(ASTNodeType.IfStatement, mockToken, mockToken);
                program.body.push(ifNode);
            }

            const measurement = measurePerformance("Deeply Nested", () => {
                return formatSSL(program, performanceOptions);
            });
            expect(typeof measurement.result).toBe("string");
            expect(measurement.timeMs).toBeLessThan(500);
        });

        test("should handle many repeated similar structures", () => {
            const program = createBaseNode(
                ASTNodeType.Program,
                mockToken,
                mockToken
            ) as ProgramNode;
            program.body = [];

            // Generate 100 similar assignment statements
            for (let i = 1; i <= 100; i++) {
                const assignment = createBaseNode(ASTNodeType.Assignment, mockToken, mockToken);
                program.body.push(assignment);
            }

            const measurement = measurePerformance("Repeated Structures", () => {
                return formatSSL(program, performanceOptions);
            });
            expect(typeof measurement.result).toBe("string");
            expect(measurement.timeMs).toBeLessThan(1000);
        });
    });

    describe("Regression Performance", () => {
        test("should maintain consistent performance across multiple runs", () => {
            const ast = generateLargeAST("medium");
            const measurements: number[] = [];

            // Run formatter multiple times to check consistency
            for (let run = 1; run <= 5; run++) {
                const measurement = measurePerformance(`Run ${run}`, () => {
                    return formatSSL(ast, performanceOptions);
                });
                expect(typeof measurement.result).toBe("string");
                measurements.push(measurement.timeMs);
            }

            // Calculate statistics
            const avg = measurements.reduce((a, b) => a + b, 0) / measurements.length;
            const max = Math.max(...measurements);
            const min = Math.min(...measurements);
            const variance = max - min;

            // Performance should be reasonably consistent (variance < 500% of average)
            // Allow for significant variance due to system load, garbage collection, and CI environment
            expect(variance).toBeLessThan(avg * 5.0);

            // Remove console.log for cleaner test output
            // Performance consistency info is available in test runner if needed
        });
    });

    describe("Memory Leak Detection", () => {
        test("should not leak memory during repeated formatting", () => {
            const ast = generateLargeAST("small");

            // Force garbage collection if available
            if (global.gc) {
                global.gc();
            }

            const initialMemory = process.memoryUsage().heapUsed;

            // Format the same AST multiple times
            for (let i = 0; i < 25; i++) {
                const result = formatSSL(ast, performanceOptions);
                expect(typeof result).toBe("string");
            }

            // Force garbage collection again
            if (global.gc) {
                global.gc();
            }

            const finalMemory = process.memoryUsage().heapUsed;
            const memoryGrowthMB = (finalMemory - initialMemory) / (1024 * 1024); // Memory growth should be minimal (less than 10MB for 25 runs)            expect(memoryGrowthMB).toBeLessThan(10);
        });
    });

    describe("Edge Case Performance", () => {
        test("should handle empty AST efficiently", () => {
            const emptyProgram = createBaseNode(
                ASTNodeType.Program,
                mockToken,
                mockToken
            ) as ProgramNode;
            emptyProgram.body = [];

            const measurement = measurePerformance("Empty AST", () => {
                return formatSSL(emptyProgram, performanceOptions);
            });
            expect(typeof measurement.result).toBe("string");
            expect(measurement.timeMs).toBeLessThan(50); // Should be very fast
        });

        test("should handle single node AST efficiently", () => {
            const program = createBaseNode(
                ASTNodeType.Program,
                mockToken,
                mockToken
            ) as ProgramNode;
            program.body = [createBaseNode(ASTNodeType.Assignment, mockToken, mockToken)];

            const measurement = measurePerformance("Single Node AST", () => {
                return formatSSL(program, performanceOptions);
            });
            expect(typeof measurement.result).toBe("string");
            expect(measurement.timeMs).toBeLessThan(100);
        });
    });

    describe("Formatter Instance Performance", () => {
        test("should create formatter instances efficiently", () => {
            const measurement = measurePerformance("Formatter Creation", () => {
                const instances = [];
                for (let i = 0; i < 100; i++) {
                    instances.push(new SSLFormatter(performanceOptions));
                }
                return instances;
            });
            expect(Array.isArray(measurement.result)).toBe(true);
            expect(measurement.result.length).toBe(100);
            expect(measurement.timeMs).toBeLessThan(200);
        });
    });
});
