const { Tokenizer } = require('./out/src/tokenizer');

const content = `:PROCEDURE ComplexProc;
    :PARAMETERS data;
    
    :IF IsValid(data);
        :TRY;
            :FOR i := 1 :TO Len(data);
                :BEGINCASE data[i];
                    :CASE "TYPE_A";
                        :WHILE processing;
                            processTypeA();
                        :ENDWHILE;
                    :CASE "TYPE_B";
                        processTypeB();
                    :OTHERWISE;
                        logUnknownType();
                :ENDCASE;
            :NEXT;
        :CATCH;
            logError();
        :ENDTRY;
    :ELSE;
        :RETURN false;
    :ENDIF;
:ENDPROC;`;

const tokenizer = new Tokenizer();
const result = tokenizer.tokenize(content);

console.log('Tokenization success:', !result.hasErrors);
console.log('Total tokens:', result.tokens.length);
console.log('Errors:', result.errors?.length || 0);

// Show just the keywords
const keywords = result.tokens
    .filter(token => token.value.startsWith(':'))
    .map(token => ({
        value: token.value,
        type: token.type,
        line: token.range.start.line,
        column: token.range.start.character
    }));

console.log('\nKeyword tokens found:');
keywords.forEach((token, index) => {
    console.log(`${index + 1}. ${token.value} (${token.type}) at line ${token.line + 1}, col ${token.column + 1}`);
});
