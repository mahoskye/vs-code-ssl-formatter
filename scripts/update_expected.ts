
import * as fs from 'fs';
import * as path from 'path';
import { SSLFormatter } from '../src/formatting/formatter';

const formatter = new SSLFormatter({ tabSize: 4, insertSpaces: true, 'ssl.format.wrapLength': 90 });
const inputPath = path.resolve(__dirname, '../formatter-tests/ADDQCSAMPLE_Rungroup.srvscr');
const outputPath = path.resolve(__dirname, '../formatter-tests/ADDQCSAMPLE_Rungroup.expected.srvscr');

try {
    const input = fs.readFileSync(inputPath, 'utf8');
    const formatted = formatter.format(input);
    fs.writeFileSync(outputPath, formatted);
    console.log(`Updated ${outputPath}`);
} catch (e) {
    console.error('Error updating expected file:', e);
    process.exit(1);
}
