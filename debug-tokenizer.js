const { tokenize } = require('./dist/tokenizer/tokenizer.js');

console.log('Testing tokenization of ":INHERIT Research.clsVehicle;"');
const tokens = tokenize(':INHERIT Research.clsVehicle;');
console.log(tokens.map(t => ({ type: t.type, value: t.value })));
