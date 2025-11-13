#!/usr/bin/env node

// Adds `frequency` to each function entry in top50_by_count.json.
// Categorizes based on count:
// - Very High: > 10000
// - High: 1000 - 10000
// - Moderate: 100 - 1000
// - Low: < 100
// If `frequency` already exists, leave it as-is.

const fs = require('fs');
const path = require('path');

const ROOT = path.resolve(__dirname, '..');
const INPUT = path.join(ROOT, 'top50_by_count.json');

function getFrequency(count) {
  if (count > 10000) return "Very High";
  if (count > 1000) return "High";
  if (count > 100) return "Moderate";
  return "Low";
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
    if (fn.frequency) continue; // respect existing

    fn.frequency = getFrequency(fn.count);
    updated++;
  }

  fs.writeFileSync(INPUT, JSON.stringify(data, null, 2) + '\n', 'utf8');
  console.log(`Added frequency to ${updated} functions.`);
}

if (require.main === module) {
  main();
}