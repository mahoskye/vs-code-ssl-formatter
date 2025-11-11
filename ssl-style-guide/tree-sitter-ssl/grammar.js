module.exports = grammar({
  name: 'ssl',

  extras: $ => [
    /\s/,
    $.comment
  ],

  word: $ => $.identifier,

  conflicts: $ => [
    [$.property_access, $.labeled_identifier],
  ],

  rules: {
    source_file: $ => repeat($._statement),

    // ───── Comments ─────
    // SSL comments start with "/*" and terminate at the first semicolon `;`
    // Example:  /* This is a comment;
    comment: _ => token(seq('/*', /[\s\S]*?/, ';')),

    // ───── Lexical atoms ─────
    identifier: _ => /[A-Za-z_][A-Za-z0-9_]*/,

    number: _ => token(seq(
      // Integer or decimal number (at least one digit after decimal for scientific notation)
      /[0-9]+(?:\.[0-9]+)?/,
      // Optional scientific notation
      optional(seq(/[eE]/, optional(/[+-]/), /[0-9]+/))
    )),

    string: $ => choice(
      seq('"', repeat(choice(/[^"\\\n\r]/, /\\./)), '"'),
      seq("'", repeat(choice(/[^'\\\n\r]/, /\\./)), "'"),
      // Bracket strings: [text]
      seq('[', repeat(/[^\]\n\r]/), ']')
    ),

    // Arrays: { expr, expr, ... }
    array: $ => seq('{', optional(commaSep($, $.expression)), '}'),

    // Indexers: arr[1], arr[1,2], arr[1][2]
    indexer: $ => seq('[', commaSep1($, $.expression), ']'),

    // Property / method on UDOs: object:property or object:method(args)
    property_access: $ => seq($.identifier, ':', field('property', $.identifier)),

    method_call: $ => seq(
      field('receiver', $.identifier), ':', field('method', $.identifier),
      field('arguments', $.arguments)
    ),

    arguments: $ => seq('(', optional(commaSep($, $.expression)), ')'),

    // Function calls: fn(args)
    call_expression: $ => seq(field('function', $.identifier), field('arguments', $.arguments)),

    // Assignment and operators
    assignment_operator: _ => choice(':=', '+=', '-=', '*=', '/=', '^=', '%='),

    comparison_operator: _ => choice('==', '!=', '<>', '#', '<', '>', '<=', '>=', '='),

    logical_operator: _ => choice('.AND.', '.OR.'),

    unary_operator: _ => choice('+', '-', '!', '.NOT.'),

    additive_operator: _ => choice('+', '-'),
    multiplicative_operator: _ => choice('*', '/', '%'),
    power_operator: _ => choice('^', '**'),

    assignment: $ => prec.right(seq(
      choice($.identifier, $.property_access),
      $.assignment_operator,
      $.expression
    )),

    parenthesized_expression: $ => seq('(', $.expression, ')'),

    primary_expression: $ => choice(
      $.string,
      $.number,
      $.nil,
      $.boolean,
      $.array,
      $.property_access,
      $.method_call,
      $.call_expression,
      seq($.identifier, repeat1($.indexer)),
      $.identifier,
      $.parenthesized_expression
    ),

    nil: _ => 'NIL',
    boolean: _ => choice('.T.', '.F.'),

    // Exponentiation has higher precedence than multiplicative
    expression: $ => prec.left(seq(
      $.logical_expression,
    )),

    logical_expression: $ => seq(
      $.comparison_expression,
      repeat(seq($.logical_operator, $.comparison_expression))
    ),

    comparison_expression: $ => seq(
      $.arithmetic_expression,
      repeat(seq($.comparison_operator, $.arithmetic_expression))
    ),

    arithmetic_expression: $ => seq(
      $.term,
      repeat(seq($.additive_operator, $.term))
    ),

    term: $ => seq(
      $.power,
      repeat(seq($.multiplicative_operator, $.power))
    ),

    power: $ => seq(
      $.unary,
      repeat(seq($.power_operator, $.unary))
    ),

    unary: $ => choice(
      seq($.unary_operator, $.unary),
      $.increment_expression,
      $.primary_expression
    ),

    increment_expression: $ => choice(
      seq($.identifier, choice('++', '--')),
      seq(choice('++', '--'), $.identifier)
    ),

    // ───── Statements (each ends with ;) ─────
    _statement: $ => choice(
      $.declaration_statement,
      $.procedure_declaration,
      $.class_declaration,
      $.if_block,
      $.while_block,
      $.for_block,
      $.switch_block,
      $.try_catch_block,
      $.error_marker,
      $.region_block,
      $.inline_code_block,
      $.label_statement,
      $.return_statement,
      $.assignment_statement,
      $.expression_statement,
      $.loop_control_statement
    ),

    // Generic expression statement (must end with ;) 
    expression_statement: $ => seq($.expression, ';'),

    assignment_statement: $ => seq($.assignment, ';'),

    return_statement: $ => seq($._kw_return, optional($.expression), ';'),

    label_statement: $ => seq($._kw_label, field('name', $.identifier), ';'),

    // :REGION name; ... :ENDREGION;
    region_block: $ => seq(
      $._kw_region, field('name', $.identifier), ';',
      repeat($._statement),
      $._kw_endregion, ';'
    ),

    // :BEGININLINECODE id?; ... :ENDINLINECODE;
    inline_code_block: $ => seq(
      $._kw_begininlinecode,
      optional(choice($.string, $.identifier)), ';',
      repeat($._statement),
      $._kw_endinlinecode, ';'
    ),

    // :ERROR;  (legacy error stanza marker)
    error_marker: $ => seq($._kw_error, ';'),

    // :DECLARE a, b, c;
    declaration_statement: $ => choice(
      seq($._kw_declare, commaSep1($, $.identifier), ';'),
      seq($._kw_parameters, commaSep1($, $.identifier), ';'),
      seq($._kw_default, $.identifier, ',', $.expression, ';'),
      seq($._kw_public, commaSep1($, $.identifier), ';'),
      seq($._kw_include, $.string, ';')
    ),

    // :PROCEDURE Name; (:PARAMETERS ...;)? (:DEFAULT ...;)* body :ENDPROC;
    procedure_declaration: $ => seq(
      $._kw_procedure, field('name', $.identifier), ';',
      optional(seq($._kw_parameters, commaSep1($, $.identifier), ';')),
      repeat(seq($._kw_default, $.identifier, ',', $.expression, ';')),
      repeat($._statement),
      $._kw_endproc, ';'
    ),

    // :CLASS Name; (:INHERIT Base;)? { members }  (methods are procedures)
    class_declaration: $ => seq(
      $._kw_class, field('name', $.identifier), ';',
      optional(seq($._kw_inherit, $.identifier, ';')),
      repeat(choice(
        seq($._kw_declare, commaSep1($, $.identifier), ';'),
        $.procedure_declaration
      )),
    ),

    // :IF expr; ... (:ELSE; ...)? :ENDIF;
    if_block: $ => seq(
      $._kw_if, $.expression, ';',
      repeat($._statement),
      optional(seq($._kw_else, ';', repeat($._statement))),
      $._kw_endif, ';'
    ),

    // :WHILE expr; ... :ENDWHILE;
    while_block: $ => seq(
      $._kw_while, $.expression, ';',
      repeat($._statement),
      $._kw_endwhile, ';'
    ),

    // :FOR i := 1 :TO 10 [:STEP 1]; ... :NEXT;
    for_block: $ => seq(
      $._kw_for, $.identifier, ':=', $.expression, ':', $._kw_to, $.expression,
      optional(seq(':', $._kw_step, $.expression)), ';',
      repeat($._statement),
      $._kw_next, ';'
    ),

    // :BEGINCASE; (:CASE expr; ... :EXITCASE;)* (:OTHERWISE; ...)? :ENDCASE;
    switch_block: $ => seq(
      $._kw_begincase, ';',
      repeat(seq($._kw_case, $.expression, ';', repeat($._statement), optional(seq($._kw_exitcase, ';')))),
      optional(seq($._kw_otherwise, ';', repeat($._statement))),
      $._kw_endcase, ';'
    ),

    // :TRY; ... :CATCH; ... (:FINALLY; ...)? :ENDTRY;
    try_catch_block: $ => seq(
      $._kw_try, ';',
      repeat($._statement),
      $._kw_catch, ';',
      repeat($._statement),
      optional(seq($._kw_finally, ';', repeat($._statement))),
      $._kw_endtry, ';'
    ),

    // Loop control statements
    loop_control_statement: $ => choice(
      $.exit_for_statement,
      $.exit_while_statement,
      $.loop_continue_statement,
      $.resume_statement
    ),

    exit_for_statement: $ => seq($._kw_exitfor, ';'),
    exit_while_statement: $ => seq($._kw_exitwhile, ';'),
    loop_continue_statement: $ => seq($._kw_loop, ';'),
    resume_statement: $ => seq($._kw_resume, ';'),

    // ───── Keywords (case-insensitive) ─────
    _kw_if: _ => token(seq(':', /IF/i)),
    _kw_else: _ => token(seq(':', /ELSE/i)),
    _kw_endif: _ => token(seq(':', /ENDIF/i)),

    _kw_while: _ => token(seq(':', /WHILE/i)),
    _kw_endwhile: _ => token(seq(':', /ENDWHILE/i)),

    _kw_for: _ => token(seq(':', /FOR/i)),
    _kw_next: _ => token(seq(':', /NEXT/i)),
    _kw_to: _ => token(seq(':', /TO/i)),
    _kw_step: _ => token(seq(':', /STEP/i)),

    _kw_begincase: _ => token(seq(':', /BEGINCASE/i)),
    _kw_case: _ => token(seq(':', /CASE/i)),
    _kw_otherwise: _ => token(seq(':', /OTHERWISE/i)),
    _kw_endcase: _ => token(seq(':', /ENDCASE/i)),
    _kw_exitcase: _ => token(seq(':', /EXITCASE/i)),

    _kw_try: _ => token(seq(':', /TRY/i)),
    _kw_catch: _ => token(seq(':', /CATCH/i)),
    _kw_finally: _ => token(seq(':', /FINALLY/i)),
    _kw_endtry: _ => token(seq(':', /ENDTRY/i)),

    _kw_declare: _ => token(seq(':', /DECLARE/i)),
    _kw_default: _ => token(seq(':', /DEFAULT/i)),
    _kw_parameters: _ => token(seq(':', /PARAMETERS/i)),
    _kw_public: _ => token(seq(':', /PUBLIC/i)),
    _kw_include: _ => token(seq(':', /INCLUDE/i)),

    _kw_procedure: _ => token(seq(':', /PROCEDURE/i)),
    _kw_endproc: _ => token(seq(':', /ENDPROC/i)),
    _kw_return: _ => token(seq(':', /RETURN/i)),

    _kw_class: _ => token(seq(':', /CLASS/i)),
    _kw_inherit: _ => token(seq(':', /INHERIT/i)),

    _kw_region: _ => token(seq(':', /REGION/i)),
    _kw_endregion: _ => token(seq(':', /ENDREGION/i)),

    _kw_error: _ => token(seq(':', /ERROR/i)),
    _kw_label: _ => token(seq(':', /LABEL/i)),
    _kw_begininlinecode: _ => token(seq(':', /BEGININLINECODE/i)),
    _kw_endinlinecode: _ => token(seq(':', /ENDINLINECODE/i)),

    // Additional loop control keywords
    _kw_exitfor: _ => token(seq(':', /EXITFOR/i)),
    _kw_exitwhile: _ => token(seq(':', /EXITWHILE/i)),
    _kw_loop: _ => token(seq(':', /LOOP/i)),
    _kw_resume: _ => token(seq(':', /RESUME/i)),
    _kw_step: _ => token(seq(':', /STEP/i)),

  }
});

// Utility: comma-separated lists
function commaSep($, rule) {
  return seq(rule, repeat(seq(',', rule)));
}
function commaSep1($, rule) {
  return seq(rule, repeat(seq(',', rule)));
}