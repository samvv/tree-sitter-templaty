
const Py = require('tree-sitter-python/grammar')

const MAX_PREC = Py.PREC.call;

module.exports = grammar(Py, {

  name: 'templatypython',

  // extras: $ => [],

  // word: $ => $.identifier,

  conflicts: $ => [

    // Templaty
    [$.expression, $._templaty_statement],

    // Python
    [$.primary_expression, $.pattern],
    [$.primary_expression, $.list_splat_pattern],
    [$.tuple, $.tuple_pattern],
    [$.list, $.list_pattern],
    [$.with_item, $._collection_elements],
    [$.named_expression, $.as_pattern],
    [$.print_statement, $.primary_expression],
    [$.type_alias_statement, $.primary_expression],

  ],

  rules: {

    source_file: $ => repeat($._statement),

    expression: ($, original) => choice(
      original,
      $.templaty_expression_statement
    ),

    _statement: ($, original) => choice(
      original,
      $._templaty_statement,
    ),

    _newline: $ => /\n/,

    _templaty_statement: $ => choice(
      $.templaty_for_in_statement,
      $.templaty_expression_statement,
      $.templaty_join_statement,
      $.templaty_code_statement,
      //$.templaty_text_statement,
    ),

    _ws: $ => /\s*/,

    templaty_expression_statement: $ => seq('{{', $.expression, '}}'),

    templaty_code_statement: $ => seq('{!', repeat($._statement), '!}'),

    templaty_join_statement: $ => seq(
      '{%',
      'join',
      $.pattern,
      'in',
      $.expression,
      'with',
      $.expression,
      '%}',
      repeat($._statement),
      '{%',
      'endjoin',
      '%}'
    ),

    templaty_for_in_statement: $ => seq(
      '{%',
      'for',
      $.pattern,
      'in',
      $.expression,
      '%}',
      repeat($._statement),
      '{%',
      'endfor',
      '%}'
    ),

    //templaty_text_statement: $ => /[^{]+|\{/
  }
});
