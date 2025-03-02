// Tree-sitter grammar for Traverse DSL

module.exports = grammar({
  name: 'traverse',

  extras: $ => [
    /\s/,
    $.comment
  ],

  conflicts: $ => [
    [$.expr, $.sexpr],
    [$.sexpr, $.expr]
  ],

  rules: {
    // Top-level rule
    rulebook: $ => repeat(choice(
      $.rules_block,
      $.comment
    )),

    rules_block: $ => seq(
      '(',
      field('keyword', 'rules'),
      repeat($.inference),
      repeat($.capture_form),
      repeat($.emit),
      ')'
    ),

    // Comments
    comment: $ => seq(
      '/*',
      /[^*]*\*+([^/*][^*]*\*+)*/,
      '/'
    ),

    // Basic elements
    symbol: $ => /[a-zA-Z0-9_+\-*\/%^=<>!$]+/,
    num: $ => /[+\-]?\d+/,
    string_literal: $ => seq(
      '"',
      repeat(choice(
        /[^"\\]+/,
        /\\./
      )),
      '"'
    ),

    // Capture patterns
    capture: $ => seq(
      '@',
      optional($.provenance_spec),
      $.capture_name
    ),
    capture_name: $ => /[a-zA-Z0-9_]+/,
    provenance_spec: $ => seq(
      ':',
      optional($.provenance_type),
      ':'
    ),
    provenance_type: $ => choice(
      'path',
      'span',
      'full',
      'default'
    ),

    // Quantifiers
    quantifier: $ => choice('*', '+', '?'),

    // Key-value pairs
    key_value: $ => seq(
      '(',
      $.symbol,
      choice($.capture, $.symbol, $.key_value),
      ')'
    ),

    // Inference rules
    inference: $ => seq(
      '(',
      field('keyword', 'infer'),
      $.relation,
      $.parameters,
      repeat1($.inference_paths),
      ')'
    ),
    relation: $ => /[a-zA-Z][a-zA-Z0-9_]*/,
    parameters: $ => seq(
      '(',
      $.parameter_list,
      ')'
    ),
    parameter_list: $ => seq(
      $.variable,
      repeat(seq(',', $.variable))
    ),

    // Inference paths
    inference_paths: $ => seq(
      field('keyword', 'via'),
      $.inference_path_list
    ),
    inference_path_list: $ => seq(
      $.path_definition,
      repeat(seq(',', $.path_definition))
    ),
    path_definition: $ => seq(
      '(',
      $.path_body,
      ')'
    ),
    path_body: $ => seq(
      repeat1($.predicate_expr),
      optional($.computation)
    ),

    // Predicate expressions
    predicate_expr: $ => choice(
      $.prefix_predicate,
      $.predicate
    ),
    prefix_predicate: $ => seq(
      $.prefix_operator,
      $.predicate
    ),
    prefix_operator: $ => choice(
      'not',
      'max',
      'min',
      'count',
      'sum'
    ),
    predicate: $ => seq(
      '(',
      $.identifier,
      $.argument_list,
      ')'
    ),
    computation: $ => seq(
      '(',
      field('keyword', 'compute'),
      $.variable,
      $.qexpr,
      ')'
    ),
    argument_list: $ => seq(
      $.argument,
      repeat(seq(',', $.argument))
    ),
    argument: $ => choice(
      $.variable,
      $.constant,
      $.placeholder
    ),

    // Basic elements
    variable: $ => seq('?', $.identifier),
    constant: $ => choice($.identifier, $.number),
    identifier: $ => /[a-zA-Z][a-zA-Z0-9_]*/,
    number: $ => /\d+/,
    placeholder: $ => '_',

    // Emit form
    emit: $ => seq(
      '(',
      field('keyword', 'emit'),
      $.symbol,
      repeat1($.capture),
      optional($.emit),
      optional($.when_form),
      optional($.do_form),
      ')'
    ),

    // Capture form
    capture_form: $ => seq(
      '(',
      field('keyword', 'capture'),
      $.symbol,
      repeat(choice($.key_value, $.capture)),
      optional($.capture_form),
      optional($.when_form),
      optional($.quantifier),
      optional($.do_form),
      ')'
    ),

    // Do form
    do_form: $ => seq(
      '(',
      field('keyword', 'do'),
      repeat1($.qexpr),
      ')'
    ),

    // When form
    when_form: $ => seq(
      '(',
      field('keyword', 'when'),
      choice($.logical, $.qexpr),
      ')'
    ),

    // Logical expressions
    logical: $ => seq(
      '(',
      $.predicate_operator,
      repeat1(choice(
        $.num,
        $.string_literal,
        $.symbol,
        $.capture,
        $.logical
      )),
      ')'
    ),

    predicate_operator: $ => choice(
      // Logical Operators
      'and', 'or', 'not',
      // Comparison Operators
      'eq', 'neq', 'lt', 'leq', 'gt', 'geq',
      // Membership Operators
      'in', 'within',
      // Existence Checks
      'exists',
      // String Comparisons
      'contains', 'startswith', 'endswith'
    ),

    // S-expressions and Q-expressions
    sexpr: $ => prec.left(seq(
      '(',
      repeat1(choice($.expr, $.capture)),
      ')',
      optional($.quantifier)
    )),

    qexpr: $ => seq(
      '{',
      repeat($.expr),
      '}'
    ),

    // Expressions
    expr: $ => choice(
      $.sexpr,
      $.qexpr,
      $.num,
      $.symbol,
      $.string_literal,
      $.capture
    ),

    // Explicitly define precedence for expressions
    _expr_with_precedence: $ => prec(1, $.expr),
    _sexpr_with_precedence: $ => prec(2, $.sexpr)
  }
});
