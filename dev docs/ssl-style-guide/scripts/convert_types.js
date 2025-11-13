#!/usr/bin/env node

// Converts SSL types to common naming conventions in top50_by_count.json.
// SSLValue -> any, SSLString -> string, etc.

const fs = require('fs');
const path = require('path');

const ROOT = path.resolve(__dirname, '..');
const INPUT = path.join(ROOT, 'top50_by_count.json');

const typeMap = {
  "SSLValue": "any",
  "SSLString": "string",
  "SSLBool": "boolean",
  "SSLDouble": "number",
  "SSLDate": "Date",
  "SSLArray": "any[]",
  "SSLObject": "object",
  "SSLError": "Error",
  "SSLSQLError": "Error",
  "SQLConnection": "any",
  "SSLNetObject": "any",
  "SSLFunction": "Function",
  "return": "void"
};

function replaceTypes(text) {
  if (!text || typeof text !== 'string') return text;
  let result = text;
  for (const [sslType, commonType] of Object.entries(typeMap)) {
    // Use word boundaries to avoid partial matches
    const regex = new RegExp(`\\b${sslType}\\b`, 'g');
    result = result.replace(regex, commonType);
  }
  return result;
}

function main() {
  const raw = fs.readFileSync(INPUT, 'utf8');
  const data = JSON.parse(raw);

  if (!data || !Array.isArray(data.functions)) {
    console.error('Invalid top50_by_count.json: missing functions array');
    process.exit(1);
  }

  let updated = 0;

  for (const fn of data.functions) {
    if (fn.signature) {
      const newSig = replaceTypes(fn.signature);
      if (newSig !== fn.signature) {
        fn.signature = newSig;
        updated++;
      }
    }
    if (fn.returnType) {
      const newRet = replaceTypes(fn.returnType);
      if (newRet !== fn.returnType) {
        fn.returnType = newRet;
        updated++;
      }
    }
  }

  fs.writeFileSync(INPUT, JSON.stringify(data, null, 2) + '\n', 'utf8');
  console.log(`Updated types in ${updated} fields.`);
}

if (require.main === module) {
  main();
}