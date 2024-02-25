
const Py = require('tree-sitter-python/grammar')

const PREC = {
  // this resolves a conflict between the usage of ':' in a lambda vs in a
  // typed parameter. In the case of a lambda, we don't allow typed parameters.
  lambda: -2,
  typed_parameter: -1,
  conditional: -1,
  parenthesized_expression: 1,
  parenthesized_list_splat: 1,
  or: 10,
  and: 11,
  not: 12,
  compare: 13,
  bitwise_or: 14,
  bitwise_and: 15,
  xor: 16,
  shift: 17,
  plus: 18,
  times: 19,
  unary: 20,
  power: 21,
  call: 22
};

const SEMICOLON = ';';

module.exports = grammar(Py, {

  name: 'templatypython',
  
  extras: $ => [
    $.comment,
    /[\s\f\uFEFF\u2060\u200B]|\r?\n/,
    $.line_continuation,
  ],

  conflicts: $ => [

    // Templaty grammar
    [$.expression, $._templaty_statement],

    // Python grammar
    [$.primary_expression, $.pattern],
    [$.primary_expression, $.list_splat_pattern],
    [$.tuple, $.tuple_pattern],
    [$.list, $.list_pattern],
    [$.with_item, $._collection_elements],
    [$.named_expression, $.as_pattern],
    [$.print_statement, $.primary_expression],
    [$.type_alias_statement, $.primary_expression],

    // Embedded Python grammar
    [$.py_primary_expression, $.py_pattern],
    [$.py_primary_expression, $.py_list_splat_pattern],
    [$.py_tuple, $.py_tuple_pattern],
    [$.py_list, $.py_list_pattern],
    [$.py_with_item, $._py_collection_elements],
    [$.py_named_expression, $.py_as_pattern],
    [$.py_print_statement, $.py_primary_expression],
    [$.py_type_alias_statement, $.py_primary_expression],
  ],

  supertypes: $ => [
    // Python grammar
    $._simple_statement,
    $._compound_statement,
    $.expression,
    $.primary_expression,
    $.pattern,
    $.parameter,
    // Embedded python grammar
    $._py_simple_statement,
    $._py_compound_statement,
    $.py_expression,
    $.py_primary_expression,
    $.py_pattern,
    $.py_parameter,
  ],

  externals: $ => [
    $._newline,
    $._indent,
    $._dedent,
    $.string_start,
    $._string_content,
    $.escape_interpolation,
    $.string_end,

    // Mark comments as external tokens so that the external scanner is always
    // invoked, even if no external token is expected. This allows for better
    // error recovery, because the external scanner can maintain the overall
    // structure by returning dedent tokens whenever a dedent occurs, even
    // if no dedent is expected.
    $.comment,

    // Allow the external scanner to check for the validity of closing brackets
    // so that it can avoid returning dedent tokens between brackets.
    ']',
    ')',
    '}',
    'except',
  ],

  inline: $ => [

    // Python grammar
    $._simple_statement,
    $._compound_statement,
    $._suite,
    $._expressions,
    $._left_hand_side,
    $.keyword_identifier,

    // Embedded python grammar
    $._py_simple_statement,
    $._py_compound_statement,
    $._py_suite,
    $._py_expressions,
    $._py_left_hand_side,
    $.py_keyword_identifier,

  ],

  word: $ => $.identifier,

  rules: {

    /// Templaty grammar

    source_file: $ => repeat($._statement),

    _templaty_statement: $ => choice(
      $.templaty_for_in_statement,
      $.templaty_expression_statement,
      $.templaty_join_statement,
      $.templaty_code_statement,
    ),

    templaty_expression_statement: $ => seq('{{', $.py_expression, '}}'),

    templaty_code_statement: $ => seq('{!', repeat($._py_statement), '!}'),

    templaty_join_statement: $ => seq(
      '{%',
      'join',
      $.py_pattern,
      'in',
      $.py_expression,
      'with',
      $.py_expression,
      '%}',
      repeat($._templaty_statement),
      '{%',
      'endjoin',
      '%}'
    ),

    templaty_for_in_statement: $ => seq(
      '{%',
      'for',
      $.py_pattern,
      'in',
      $.py_expression,
      '%}',
      repeat($._templaty_statement),
      '{%',
      'endfor',
      '%}'
    ),

    /// Integration with parent gramamar

    expression: ($, original) => choice(
      original,
      $.templaty_expression_statement
    ),

    _statement: ($, original) => choice(
      original,
      $._templaty_statement,
    ),

    /// Embedded Python grammar

    py_module: $ => repeat($._py_statement),
    _py_statement: $ => choice($._py_simple_statements, $._py_compound_statement),

    // Simple statements

    _py_simple_statements: $ => seq(sep1($._py_simple_statement, SEMICOLON), optional(SEMICOLON), $._newline),
    _py_simple_statement: $ => choice($.py_future_import_statement, $.py_import_statement, $.py_import_from_statement, $.py_print_statement, $.py_assert_statement, $.py_expression_statement, $.py_return_statement, $.py_delete_statement, $.py_raise_statement, $.py_pass_statement, $.py_break_statement, $.py_continue_statement, $.py_global_statement, $.py_nonlocal_statement, $.py_exec_statement, $.py_type_alias_statement),
    py_import_statement: $ => seq('import', $._py_import_list),
    py_import_prefix: _ => repeat1('.'),
    py_relative_import: $ => seq($.py_import_prefix, optional($.py_dotted_name)),
    py_future_import_statement: $ => seq('from', '__future__', 'import', choice($._py_import_list, seq('(', $._py_import_list, ')'))),
    py_import_from_statement: $ => seq('from', field('module_name', choice($.py_relative_import, $.py_dotted_name)), 'import', choice($.py_wildcard_import, $._py_import_list, seq('(', $._py_import_list, ')'))),
    _py_import_list: $ => seq(commaSep1(field('name', choice($.py_dotted_name, $.py_aliased_import))), optional(',')),
    py_aliased_import: $ => seq(field('name', $.py_dotted_name), 'as', field('alias', $.identifier)),
    py_wildcard_import: _ => '*',
    py_print_statement: $ => choice(prec(1, seq('print', $.py_chevron, repeat(seq(',', field('argument', $.py_expression))), optional(','))), prec(-3, prec.dynamic(-1, seq('print', commaSep1(field('argument', $.py_expression)), optional(','))))),
    py_chevron: $ => seq('>>', $.py_expression),
    py_assert_statement: $ => seq('assert', commaSep1($.py_expression)),
    py_expression_statement: $ => choice($.py_expression, seq(commaSep1($.py_expression), optional(',')), $.py_assignment, $.py_augmented_assignment, $.py_yield),
    py_named_expression: $ => seq(field('name', $._py_named_expression_lhs), ':=', field('value', $.py_expression)),
    _py_named_expression_lhs: $ => choice($.identifier, $.py_keyword_identifier),
    py_return_statement: $ => seq('return', optional($._py_expressions)),
    py_delete_statement: $ => seq('del', $._py_expressions),
    _py_expressions: $ => choice($.py_expression, $.py_expression_list),
    py_raise_statement: $ => seq('raise', optional($._py_expressions), optional(seq('from', field('cause', $.py_expression)))),
    py_pass_statement: _ => prec.left('pass'),
    py_break_statement: _ => prec.left('break'),
    py_continue_statement: _ => prec.left('continue'),
    // Compound statements

    _py_compound_statement: $ => choice($.py_if_statement, $.py_for_statement, $.py_while_statement, $.py_try_statement, $.py_with_statement, $.py_function_definition, $.py_class_definition, $.py_decorated_definition, $.py_match_statement),
    py_if_statement: $ => seq('if', field('condition', $.py_expression), ':', field('consequence', $._py_suite), repeat(field('alternative', $.py_elif_clause)), optional(field('alternative', $.py_else_clause))),
    py_elif_clause: $ => seq('elif', field('condition', $.py_expression), ':', field('consequence', $._py_suite)),
    py_else_clause: $ => seq('else', ':', field('body', $._py_suite)),
    py_match_statement: $ => seq('match', commaSep1(field('subject', $.py_expression)), optional(','), ':', field('body', alias($._py_match_block, $.py_block))),
    _py_match_block: $ => choice(seq($._indent, repeat(field('alternative', $.py_case_clause)), $._dedent), $._newline),
    py_case_clause: $ => seq('case', commaSep1($.py_case_pattern), optional(','), optional(field('guard', $.py_if_clause)), ':', field('consequence', $._py_suite)),
    py_for_statement: $ => seq(optional('async'), 'for', field('left', $._py_left_hand_side), 'in', field('right', $._py_expressions), ':', field('body', $._py_suite), field('alternative', optional($.py_else_clause))),
    py_while_statement: $ => seq('while', field('condition', $.py_expression), ':', field('body', $._py_suite), optional(field('alternative', $.py_else_clause))),
    py_try_statement: $ => seq('try', ':', field('body', $._py_suite), choice(seq(repeat1($.py_except_clause), optional($.py_else_clause), optional($.py_finally_clause)), seq(repeat1($.py_except_group_clause), optional($.py_else_clause), optional($.py_finally_clause)), $.py_finally_clause)),
    py_except_clause: $ => seq('except', optional(seq($.py_expression, optional(seq(choice('as', ','), $.py_expression)))), ':', $._py_suite),
    py_except_group_clause: $ => seq('except*', seq($.py_expression, optional(seq('as', $.py_expression))), ':', $._py_suite),
    py_finally_clause: $ => seq('finally', ':', $._py_suite),
    py_with_statement: $ => seq(optional('async'), 'with', $.py_with_clause, ':', field('body', $._py_suite)),
    py_with_clause: $ => choice(seq(commaSep1($.py_with_item), optional(',')), seq('(', commaSep1($.py_with_item), optional(','), ')')),
    py_with_item: $ => prec.dynamic(1, seq(field('value', $.py_expression))),
    py_function_definition: $ => seq(optional('async'), 'def', field('name', $.identifier), field('type_parameters', optional($.py_type_parameter)), field('parameters', $.py_parameters), optional(seq('->', field('return_type', $.py_type))), ':', field('body', $._py_suite)),
    py_parameters: $ => seq('(', optional($._py_parameters), ')'),
    py_lambda_parameters: $ => $._py_parameters,
    py_list_splat: $ => seq('*', $.py_expression),
    py_dictionary_splat: $ => seq('**', $.py_expression),
    py_global_statement: $ => seq('global', commaSep1($.identifier)),
    py_nonlocal_statement: $ => seq('nonlocal', commaSep1($.identifier)),
    py_exec_statement: $ => seq('exec', field('code', choice($.py_string, $.identifier)), optional(seq('in', commaSep1($.py_expression)))),
    py_type_alias_statement: $ => prec.dynamic(1, seq('type', $.py_type, '=', $.py_type)),
    py_class_definition: $ => seq('class', field('name', $.identifier), field('type_parameters', optional($.py_type_parameter)), field('superclasses', optional($.py_argument_list)), ':', field('body', $._py_suite)),
    py_type_parameter: $ => seq('[', commaSep1($.py_type), ']'),
    py_parenthesized_list_splat: $ => prec(PREC.parenthesized_list_splat, seq('(', choice(alias($.py_parenthesized_list_splat, $.py_parenthesized_expression), $.py_list_splat), ')')),
    py_argument_list: $ => seq('(', optional(commaSep1(choice($.py_expression, $.py_list_splat, $.py_dictionary_splat, alias($.py_parenthesized_list_splat, $.py_parenthesized_expression), $.py_keyword_argument))), optional(','), ')'),
    py_decorated_definition: $ => seq(repeat1($.py_decorator), field('definition', choice($.py_class_definition, $.py_function_definition))),
    py_decorator: $ => seq('@', $.py_expression, $._newline),
    _py_suite: $ => choice(alias($._py_simple_statements, $.py_block), seq($._indent, $.py_block), alias($._newline, $.py_block)),
    py_block: $ => seq(repeat($._py_statement), $._dedent),
    py_expression_list: $ => prec.right(seq($.py_expression, choice(',', seq(repeat1(seq(',', $.py_expression)), optional(','))))),
    py_dotted_name: $ => prec(1, sep1($.identifier, '.')),
    // Match cases

    py_case_pattern: $ => prec(1, choice(alias($._py_as_pattern, $.py_as_pattern), $.py_keyword_pattern, $._py_simple_pattern)),
    _py_simple_pattern: $ => prec(1, choice($.py_class_pattern, $.py_splat_pattern, $.py_union_pattern, alias($._py_list_pattern, $.py_list_pattern), alias($._py_tuple_pattern, $.py_tuple_pattern), $.py_dict_pattern, $.py_string, $.py_concatenated_string, $.py_true, $.py_false, $.py_none, seq(optional('-'), choice($.py_integer, $.py_float)), $.py_complex_pattern, $.py_dotted_name, '_')),
    _py_as_pattern: $ => seq($.py_case_pattern, 'as', $.identifier),
    py_union_pattern: $ => prec.right(seq($._py_simple_pattern, repeat1(prec.left(seq('|', $._py_simple_pattern))))),
    _py_list_pattern: $ => seq('[', optional(seq(commaSep1($.py_case_pattern), optional(','))), ']'),
    _py_tuple_pattern: $ => seq('(', optional(seq(commaSep1($.py_case_pattern), optional(','))), ')'),
    py_dict_pattern: $ => seq('{', optional(seq(commaSep1(choice($._py_key_value_pattern, $.py_splat_pattern)), optional(','))), '}'),
    _py_key_value_pattern: $ => seq(field('key', $._py_simple_pattern), ':', field('value', $.py_case_pattern)),
    py_keyword_pattern: $ => seq($.identifier, '=', $._py_simple_pattern),
    py_splat_pattern: $ => prec(1, seq(choice('*', '**'), choice($.identifier, '_'))),
    py_class_pattern: $ => seq($.py_dotted_name, '(', optional(seq(commaSep1($.py_case_pattern), optional(','))), ')'),
    py_complex_pattern: $ => prec(1, seq(optional('-'), choice($.py_integer, $.py_float), choice('+', '-'), choice($.py_integer, $.py_float))),
    // Patterns

    _py_parameters: $ => seq(commaSep1($.py_parameter), optional(',')),
    _py_patterns: $ => seq(commaSep1($.py_pattern), optional(',')),
    py_parameter: $ => choice($.identifier, $.py_typed_parameter, $.py_default_parameter, $.py_typed_default_parameter, $.py_list_splat_pattern, $.py_tuple_pattern, $.py_keyword_separator, $.py_positional_separator, $.py_dictionary_splat_pattern),
    py_pattern: $ => choice($.identifier, $.py_keyword_identifier, $.py_subscript, $.py_attribute, $.py_list_splat_pattern, $.py_tuple_pattern, $.py_list_pattern),
    py_tuple_pattern: $ => seq('(', optional($._py_patterns), ')'),
    py_list_pattern: $ => seq('[', optional($._py_patterns), ']'),
    py_default_parameter: $ => seq(field('name', choice($.identifier, $.py_tuple_pattern)), '=', field('value', $.py_expression)),
    py_typed_default_parameter: $ => prec(PREC.typed_parameter, seq(field('name', $.identifier), ':', field('type', $.py_type), '=', field('value', $.py_expression))),
    py_list_splat_pattern: $ => seq('*', choice($.identifier, $.py_keyword_identifier, $.py_subscript, $.py_attribute)),
    py_dictionary_splat_pattern: $ => seq('**', choice($.identifier, $.py_keyword_identifier, $.py_subscript, $.py_attribute)),
    // Extended patterns (patterns allowed in match statement are far more flexible than simple patterns though still a subset of "expression")

    py_as_pattern: $ => prec.left(seq($.py_expression, 'as', field('alias', alias($.py_expression, $.py_as_pattern_target)))),
    // Expressions

    _py_expression_within_for_in_clause: $ => choice($.py_expression, alias($.py_lambda_within_for_in_clause, $.py_lambda)),
    py_expression: $ => choice($.py_comparison_operator, $.py_not_operator, $.py_boolean_operator, $.py_lambda, $.py_primary_expression, $.py_conditional_expression, $.py_named_expression, $.py_as_pattern),
    py_primary_expression: $ => choice($.py_await, $.py_binary_operator, $.identifier, $.py_keyword_identifier, $.py_string, $.py_concatenated_string, $.py_integer, $.py_float, $.py_true, $.py_false, $.py_none, $.py_unary_operator, $.py_attribute, $.py_subscript, $.py_call, $.py_list, $.py_list_comprehension, $.py_dictionary, $.py_dictionary_comprehension, $.py_set, $.py_set_comprehension, $.py_tuple, $.py_parenthesized_expression, $.py_generator_expression, $.py_ellipsis, alias($.py_list_splat_pattern, $.py_list_splat)),
    py_not_operator: $ => prec(PREC.not, seq('not', field('argument', $.py_expression))),
    py_boolean_operator: $ => choice(prec.left(PREC.and, seq(field('left', $.py_expression), field('operator', 'and'), field('right', $.py_expression))), prec.left(PREC.or, seq(field('left', $.py_expression), field('operator', 'or'), field('right', $.py_expression)))),
    py_binary_operator: $ => {
      const table = [[prec.left, '+', PREC.plus], [prec.left, '-', PREC.plus], [prec.left, '*', PREC.times], [prec.left, '@', PREC.times], [prec.left, '/', PREC.times], [prec.left, '%', PREC.times], [prec.left, '//', PREC.times], [prec.right, '**', PREC.power], [prec.left, '|', PREC.bitwise_or], [prec.left, '&', PREC.bitwise_and], [prec.left, '^', PREC.xor], [prec.left, '<<', PREC.shift], [prec.left, '>>', PREC.shift]];

      // @ts-ignore
      return choice(...table.map(([fn, operator, precedence]) => fn(precedence, seq(field('left', $.py_primary_expression),
      // @ts-ignore
      field('operator', operator), field('right', $.py_primary_expression)))));
    },
    py_unary_operator: $ => prec(PREC.unary, seq(field('operator', choice('+', '-', '~')), field('argument', $.py_primary_expression))),
    py_comparison_operator: $ => prec.left(PREC.compare, seq($.py_primary_expression, repeat1(seq(field('operators', choice('<', '<=', '==', '!=', '>=', '>', '<>', 'in', alias(seq('not', 'in'), 'not in'), 'is', alias(seq('is', 'not'), 'is not'))), $.py_primary_expression)))),
    py_lambda: $ => prec(PREC.lambda, seq('lambda', field('parameters', optional($.py_lambda_parameters)), ':', field('body', $.py_expression))),
    py_lambda_within_for_in_clause: $ => seq('lambda', field('parameters', optional($.py_lambda_parameters)), ':', field('body', $._py_expression_within_for_in_clause)),
    py_assignment: $ => seq(field('left', $._py_left_hand_side), choice(seq('=', field('right', $._py_right_hand_side)), seq(':', field('type', $.py_type)), seq(':', field('type', $.py_type), '=', field('right', $._py_right_hand_side)))),
    py_augmented_assignment: $ => seq(field('left', $._py_left_hand_side), field('operator', choice('+=', '-=', '*=', '/=', '@=', '//=', '%=', '**=', '>>=', '<<=', '&=', '^=', '|=')), field('right', $._py_right_hand_side)),
    _py_left_hand_side: $ => choice($.py_pattern, $.py_pattern_list),
    py_pattern_list: $ => seq($.py_pattern, choice(',', seq(repeat1(seq(',', $.py_pattern)), optional(',')))),
    _py_right_hand_side: $ => choice($.py_expression, $.py_expression_list, $.py_assignment, $.py_augmented_assignment, $.py_pattern_list, $.py_yield),
    py_yield: $ => prec.right(seq('yield', choice(seq('from', $.py_expression), optional($._py_expressions)))),
    py_attribute: $ => prec(PREC.call, seq(field('object', $.py_primary_expression), '.', field('attribute', $.identifier))),
    py_subscript: $ => prec(PREC.call, seq(field('value', $.py_primary_expression), '[', commaSep1(field('subscript', choice($.py_expression, $.py_slice))), optional(','), ']')),
    py_slice: $ => seq(optional($.py_expression), ':', optional($.py_expression), optional(seq(':', optional($.py_expression)))),
    py_ellipsis: _ => '...',
    py_call: $ => prec(PREC.call, seq(field('function', $.py_primary_expression), field('arguments', choice($.py_generator_expression, $.py_argument_list)))),
    py_typed_parameter: $ => prec(PREC.typed_parameter, seq(choice($.identifier, $.py_list_splat_pattern, $.py_dictionary_splat_pattern), ':', field('type', $.py_type))),
    py_type: $ => choice($.py_expression, $.py_splat_type, $.py_generic_type, $.py_union_type, $.py_constrained_type, $.py_member_type),
    py_splat_type: $ => prec(1, seq(choice('*', '**'), $.identifier)),
    py_generic_type: $ => prec(1, seq($.identifier, $.py_type_parameter)),
    py_union_type: $ => prec.left(seq($.py_type, '|', $.py_type)),
    py_constrained_type: $ => prec.right(seq($.py_type, ':', $.py_type)),
    py_member_type: $ => seq($.py_type, '.', $.identifier),
    py_keyword_argument: $ => seq(field('name', choice($.identifier, $.py_keyword_identifier)), '=', field('value', $.py_expression)),
    // Literals

    py_list: $ => seq('[', optional($._py_collection_elements), ']'),
    py_set: $ => seq('{', $._py_collection_elements, '}'),
    py_tuple: $ => seq('(', optional($._py_collection_elements), ')'),
    py_dictionary: $ => seq('{', optional(commaSep1(choice($.py_pair, $.py_dictionary_splat))), optional(','), '}'),
    py_pair: $ => seq(field('key', $.py_expression), ':', field('value', $.py_expression)),
    py_list_comprehension: $ => seq('[', field('body', $.py_expression), $._py_comprehension_clauses, ']'),
    py_dictionary_comprehension: $ => seq('{', field('body', $.py_pair), $._py_comprehension_clauses, '}'),
    py_set_comprehension: $ => seq('{', field('body', $.py_expression), $._py_comprehension_clauses, '}'),
    py_generator_expression: $ => seq('(', field('body', $.py_expression), $._py_comprehension_clauses, ')'),
    _py_comprehension_clauses: $ => seq($.py_for_in_clause, repeat(choice($.py_for_in_clause, $.py_if_clause))),
    py_parenthesized_expression: $ => prec(PREC.parenthesized_expression, seq('(', choice($.py_expression, $.py_yield), ')')),
    _py_collection_elements: $ => seq(commaSep1(choice($.py_expression, $.py_yield, $.py_list_splat, $.py_parenthesized_list_splat)), optional(',')),
    py_for_in_clause: $ => prec.left(seq(optional('async'), 'for', field('left', $._py_left_hand_side), 'in', field('right', commaSep1($._py_expression_within_for_in_clause)), optional(','))),
    py_if_clause: $ => seq('if', $.py_expression),
    py_conditional_expression: $ => prec.right(PREC.conditional, seq($.py_expression, 'if', $.py_expression, 'else', $.py_expression)),
    py_concatenated_string: $ => seq($.py_string, repeat1($.py_string)),
    py_string: $ => seq($.string_start, repeat(choice($.py_interpolation, $.py_string_content)), $.string_end),
    py_string_content: $ => prec.right(repeat1(choice($.escape_interpolation, $.py_escape_sequence, $._py_not_escape_sequence, $._string_content))),
    py_interpolation: $ => seq('{', field('expression', $._py_f_expression), optional('='), optional(field('type_conversion', $.py_type_conversion)), optional(field('format_specifier', $.py_format_specifier)), '}'),
    _py_f_expression: $ => choice($.py_expression, $.py_expression_list, $.py_pattern_list, $.py_yield),
    py_escape_sequence: _ => token.immediate(prec(1, seq('\\', choice(/u[a-fA-F\d]{4}/, /U[a-fA-F\d]{8}/, /x[a-fA-F\d]{2}/, /\d{3}/, /\r?\n/, /['"abfrntv\\]/, /N\{[^}]+\}/)))),
    _py_not_escape_sequence: _ => token.immediate('\\'),
    py_format_specifier: $ => seq(':', repeat(choice(token(prec(1, /[^{}\n]+/)), alias($.py_interpolation, $.py_format_expression)))),
    py_type_conversion: _ => /![a-z]/,
    py_integer: _ => token(choice(seq(choice('0x', '0X'), repeat1(/_?[A-Fa-f0-9]+/), optional(/[Ll]/)), seq(choice('0o', '0O'), repeat1(/_?[0-7]+/), optional(/[Ll]/)), seq(choice('0b', '0B'), repeat1(/_?[0-1]+/), optional(/[Ll]/)), seq(repeat1(/[0-9]+_?/), choice(optional(/[Ll]/),
    // long numbers
    optional(/[jJ]/) // complex numbers
    )))),
    py_float: _ => {
      const digits = repeat1(/[0-9]+_?/);
      const exponent = seq(/[eE][\+-]?/, digits);
      return token(seq(choice(seq(digits, '.', optional(digits), optional(exponent)), seq(optional(digits), '.', digits, optional(exponent)), seq(digits, exponent)), optional(choice(/[Ll]/, /[jJ]/))));
    },
    py_keyword_identifier: $ => choice(prec(-3, alias(choice('print', 'exec', 'async', 'await', 'match'), $.identifier)), alias('type', $.identifier)),
    py_true: _ => 'True',
    py_false: _ => 'False',
    py_none: _ => 'None',
    py_await: $ => prec(PREC.unary, seq('await', $.py_primary_expression)),
    py_line_continuation: _ => token(seq('\\', choice(seq(optional('\r'), '\n'), '\0'))),
    py_positional_separator: _ => '/',
    py_keyword_separator: _ => '*'

  }

});

/**
 * Creates a rule to match one or more of the rules separated by a comma
 *
 * @param {RuleOrLiteral} rule
 *
 * @return {SeqRule}
 *
 */
function commaSep1(rule) {
  return sep1(rule, ',');
}

/**
 * Creates a rule to match one or more occurrences of `rule` separated by `sep`
 *
 * @param {RuleOrLiteral} rule
 *
 * @param {RuleOrLiteral} separator
 *
 * @return {SeqRule}
 *
 */
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}
