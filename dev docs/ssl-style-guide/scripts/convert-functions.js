#!/usr/bin/env node

/**
 * Script to convert top50_by_count.json function data into SSLFunction[] format
 * for use in the VS Code SSL formatter extension
 */

const fs = require('fs');
const path = require('path');

// Read the JSON data
const jsonPath = path.join(__dirname, '..', 'top50_by_count.json');
const jsonData = JSON.parse(fs.readFileSync(jsonPath, 'utf8'));

// Convert function data to SSLFunction format
function convertFunction(func) {
    // Extract parameter names from untyped signature or signature
    let params = '';
    if (func.untypedSignature && func.untypedSignature !== 'null') {
        const paramMatch = func.untypedSignature.match(/\(([^)]*)\)/);
        params = paramMatch ? paramMatch[1] : '';
    } else if (func.signature && func.signature !== 'null') {
        const paramMatch = func.signature.match(/\(([^)]*)\)/);
        params = paramMatch ? paramMatch[1] : '';
    }

    // Create parameter description from signature
    let paramDesc = '';
    if (func.signature && func.signature !== 'null') {
        const sigMatch = func.signature.match(/\(([^)]*)\)/);
        if (sigMatch) {
            const paramList = sigMatch[1].split(',').map(p => p.trim());
            paramDesc = `(${paramList.join(', ')})`;
        }
    } else if (params) {
        paramDesc = `(${params})`;
    }

    return {
        name: func.name,
        description: func.description || '',
        params: paramDesc || '()',
        returns: (func.returnType && func.returnType !== 'null') ? func.returnType : 'any',
        signature: (func.signature && func.signature !== 'null') ? func.signature : undefined,
        returnType: (func.returnType && func.returnType !== 'null') ? func.returnType : undefined,
        category: (func.category && func.category !== 'null') ? func.category : undefined,
        frequency: func.frequency || undefined,
        untypedSignature: (func.untypedSignature && func.untypedSignature !== 'null') ? func.untypedSignature : undefined
    };
}

// Convert all functions
const sslFunctions = jsonData.functions.map(convertFunction);

// Generate TypeScript output
const output = `/**
 * SSL Built-in Functions
 * Auto-generated from top50_by_count.json
 * Generated: ${new Date().toISOString()}
 * Total functions: ${sslFunctions.length}
 */

import { SSLFunction } from '../../src/constants/language';

export const SSL_BUILTIN_FUNCTIONS: SSLFunction[] = ${JSON.stringify(sslFunctions, null, 4)};
`;

// Write to output file
const outputPath = path.join(__dirname, '..', 'ssl-builtin-functions.ts');
fs.writeFileSync(outputPath, output, 'utf8');

console.log(`Generated ${sslFunctions.length} SSL functions in ${outputPath}`);