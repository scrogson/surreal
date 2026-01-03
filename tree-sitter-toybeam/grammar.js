/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "toybeam",

  extras: ($) => [/\s/, $.line_comment, $.block_comment],

  conflicts: ($) => [
    [$.tuple_pattern, $.unit],
    [$.tuple_expression, $.unit],
    [$.tuple_expression, $.parenthesized_expression],
  ],

  rules: {
    // Source file can be either a wrapped module or items directly (file-based module)
    source_file: ($) => choice($.module, repeat($._item)),

    module: ($) =>
      seq("mod", field("name", $.identifier), "{", repeat($._item), "}"),

    _item: ($) =>
      choice(
        $.function_definition,
        $.struct_definition,
        $.enum_definition,
        $.mod_declaration,
        $.use_declaration,
        $.impl_block,
        $.trait_definition,
        $.trait_impl
      ),

    // Module declaration: `mod foo;`
    mod_declaration: ($) =>
      seq(optional($.visibility), "mod", field("name", $.identifier), ";"),

    // Use declaration: `use foo::bar;` or `use foo::{a, b};` or `use foo::*;`
    use_declaration: ($) => seq("use", $.use_tree, ";"),

    use_tree: ($) =>
      choice(
        // Simple path: use foo::bar; or use foo::bar as baz;
        seq(
          field("module", $.identifier),
          "::",
          field("name", $.identifier),
          optional(seq("as", field("alias", $.identifier)))
        ),
        // Glob import: use foo::*;
        seq(field("module", $.identifier), "::", "*"),
        // Group import: use foo::{a, b as c};
        seq(
          field("module", $.identifier),
          "::",
          "{",
          commaSep($.use_tree_item),
          "}"
        )
      ),

    use_tree_item: ($) =>
      seq(
        field("name", $.identifier),
        optional(seq("as", field("alias", $.identifier)))
      ),

    // Impl block: `impl Point { ... }`
    impl_block: ($) =>
      seq(
        "impl",
        field("type", $.type_identifier),
        "{",
        repeat($.function_definition),
        "}"
      ),

    // Trait definition: `trait Display { fn display(self) -> String; }`
    trait_definition: ($) =>
      seq(
        "trait",
        field("name", $.type_identifier),
        "{",
        repeat($.trait_method),
        "}"
      ),

    trait_method: ($) =>
      seq(
        "fn",
        field("name", $.identifier),
        $.parameters,
        optional(seq("->", field("return_type", $._type))),
        ";"
      ),

    // Trait implementation: `impl Display for Point { ... }`
    trait_impl: ($) =>
      seq(
        "impl",
        field("trait", $.type_identifier),
        "for",
        field("type", $.type_identifier),
        "{",
        repeat($.function_definition),
        "}"
      ),

    // Function definition
    function_definition: ($) =>
      seq(
        optional($.visibility),
        "fn",
        field("name", $.identifier),
        optional($.type_parameters),
        $.parameters,
        optional(seq("->", field("return_type", $._type))),
        field("body", $.block)
      ),

    visibility: ($) => "pub",

    type_parameters: ($) => seq("<", commaSep1($.type_parameter), ">"),

    type_parameter: ($) => $.type_identifier,

    parameters: ($) => seq("(", commaSep($.parameter), ")"),

    parameter: ($) =>
      choice(
        // Self parameter with optional type: `self` or `self: Type`
        // (must come first as separate alternative to avoid conflict with pattern)
        $.self_parameter,
        // Regular parameter: `name: Type`
        seq(field("pattern", $._pattern), ":", field("type", $._type))
      ),

    self_parameter: ($) =>
      seq("self", optional(seq(":", field("type", $._type)))),

    // Struct definition
    struct_definition: ($) =>
      seq(
        optional($.visibility),
        "struct",
        field("name", $.type_identifier),
        optional($.type_parameters),
        "{",
        commaSep($.struct_field),
        optional(","),
        "}"
      ),

    struct_field: ($) => seq(field("name", $.identifier), ":", field("type", $._type)),

    // Enum definition
    enum_definition: ($) =>
      seq(
        optional($.visibility),
        "enum",
        field("name", $.type_identifier),
        optional($.type_parameters),
        "{",
        commaSep($.enum_variant),
        optional(","),
        "}"
      ),

    enum_variant: ($) =>
      seq(
        field("name", $.type_identifier),
        optional(seq("(", commaSep($._type), ")"))
      ),

    // Types
    _type: ($) =>
      choice(
        $.type_identifier,
        $.generic_type,
        $.tuple_type,
        $.list_type,
        $.primitive_type
      ),

    generic_type: ($) =>
      seq(field("name", $.type_identifier), "<", commaSep1($._type), ">"),

    tuple_type: ($) => seq("(", commaSep($._type), ")"),

    list_type: ($) => seq("[", $._type, "]"),

    primitive_type: ($) => choice("int", "bool", "string", "atom", "pid", "binary"),

    // Statements
    block: ($) => seq("{", repeat($._statement), optional($._expression), "}"),

    _statement: ($) => choice($.let_statement, $.expression_statement),

    let_statement: ($) =>
      seq(
        "let",
        optional("mut"),
        field("pattern", $._pattern),
        optional(seq(":", field("type", $._type))),
        "=",
        field("value", $._expression),
        ";"
      ),

    expression_statement: ($) => seq($._expression, ";"),

    // Expressions
    _expression: ($) =>
      choice(
        $.identifier,
        $.self_expression,
        $._literal,
        $.binary_expression,
        $.unary_expression,
        $.call_expression,
        $.method_call_expression,
        $.field_expression,
        $.index_expression,
        $.if_expression,
        $.match_expression,
        $.block,
        $.tuple_expression,
        $.list_expression,
        $.struct_expression,
        $.path_expression,
        $.spawn_expression,
        $.send_expression,
        $.receive_expression,
        $.return_expression,
        $.bitstring_expression,
        $.pipe_expression,
        $.parenthesized_expression
      ),

    parenthesized_expression: ($) => seq("(", $._expression, ")"),

    self_expression: ($) => "self",

    // Pipe expression: `expr |> func(args)`
    pipe_expression: ($) =>
      prec.left(
        0,
        seq(
          field("left", $._expression),
          "|>",
          field("right", $._expression)
        )
      ),

    binary_expression: ($) =>
      choice(
        prec.left(1, seq(field("left", $._expression), field("operator", "||"), field("right", $._expression))),
        prec.left(2, seq(field("left", $._expression), field("operator", "&&"), field("right", $._expression))),
        prec.left(3, seq(field("left", $._expression), field("operator", "=="), field("right", $._expression))),
        prec.left(3, seq(field("left", $._expression), field("operator", "!="), field("right", $._expression))),
        prec.left(4, seq(field("left", $._expression), field("operator", "<"), field("right", $._expression))),
        prec.left(4, seq(field("left", $._expression), field("operator", "<="), field("right", $._expression))),
        prec.left(4, seq(field("left", $._expression), field("operator", ">"), field("right", $._expression))),
        prec.left(4, seq(field("left", $._expression), field("operator", ">="), field("right", $._expression))),
        prec.left(5, seq(field("left", $._expression), field("operator", "+"), field("right", $._expression))),
        prec.left(5, seq(field("left", $._expression), field("operator", "-"), field("right", $._expression))),
        prec.left(6, seq(field("left", $._expression), field("operator", "*"), field("right", $._expression))),
        prec.left(6, seq(field("left", $._expression), field("operator", "/"), field("right", $._expression))),
        prec.left(6, seq(field("left", $._expression), field("operator", "%"), field("right", $._expression))),
      ),

    unary_expression: ($) =>
      prec(
        7,
        seq(field("operator", choice("-", "!")), field("operand", $._expression))
      ),

    call_expression: ($) =>
      prec(
        8,
        seq(field("function", $._expression), "(", commaSep($._expression), ")")
      ),

    method_call_expression: ($) =>
      prec.left(
        9,
        seq(
          field("receiver", $._expression),
          ".",
          field("method", $.identifier),
          "(",
          commaSep($._expression),
          ")"
        )
      ),

    field_expression: ($) =>
      prec(8, seq(field("value", $._expression), ".", field("field", $.identifier))),

    index_expression: ($) =>
      prec(8, seq(field("value", $._expression), "[", field("index", $._expression), "]")),

    if_expression: ($) =>
      prec.right(
        seq(
          "if",
          field("condition", $._expression),
          field("consequence", $.block),
          optional(seq("else", field("alternative", choice($.block, $.if_expression))))
        )
      ),

    match_expression: ($) =>
      seq("match", field("value", $._expression), "{", repeat($.match_arm), "}"),

    match_arm: ($) =>
      seq(
        field("pattern", $._pattern),
        optional(seq("if", field("guard", $._expression))),
        "=>",
        field("body", $._expression),
        optional(",")
      ),

    tuple_expression: ($) => seq("(", commaSep($._expression), ")"),

    list_expression: ($) => seq("[", commaSep($._expression), "]"),

    struct_expression: ($) =>
      prec.dynamic(1, seq(
        field("name", $.type_identifier),
        "{",
        commaSep($.field_initializer),
        optional(","),
        "}"
      )),

    field_initializer: ($) =>
      seq(field("name", $.identifier), ":", field("value", $._expression)),

    path_expression: ($) =>
      prec(
        9,
        seq(
          field("path", $.type_identifier),
          "::",
          field("name", choice($.identifier, $.type_identifier))
        )
      ),

    spawn_expression: ($) =>
      choice(
        seq("spawn", "(", $._expression, ")"),
        seq("spawn", "||", $.block)
      ),

    send_expression: ($) =>
      prec.right(
        0,
        seq(field("target", $._expression), "!", field("message", $._expression))
      ),

    receive_expression: ($) =>
      seq(
        "receive",
        "{",
        repeat($.match_arm),
        optional(seq("after", $._expression, "=>", $.block)),
        "}"
      ),

    return_expression: ($) => prec.right(seq("return", optional($._expression))),

    // Bitstring/Binary expressions
    bitstring_expression: ($) => seq("<<", commaSep($.bitstring_segment), ">>"),

    bitstring_segment: ($) =>
      seq(
        field("value", $._expression),
        optional(seq(":", field("size", $._expression))),
        optional(seq("/", $.segment_specifiers))
      ),

    segment_specifiers: ($) => sep1($.segment_specifier, "-"),

    segment_specifier: ($) =>
      choice(
        "big",
        "little",
        "signed",
        "unsigned",
        "integer",
        "float",
        "binary",
        "bytes",
        "utf8"
      ),

    // Patterns
    _pattern: ($) =>
      choice(
        $.identifier,
        $.wildcard_pattern,
        $._literal,
        $.tuple_pattern,
        $.list_pattern,
        $.list_cons_pattern,
        $.struct_pattern,
        $.enum_pattern,
        $.bitstring_pattern
      ),

    wildcard_pattern: ($) => "_",

    tuple_pattern: ($) => seq("(", commaSep($._pattern), ")"),

    list_pattern: ($) => seq("[", commaSep($._pattern), "]"),

    list_cons_pattern: ($) =>
      seq("[", field("head", $._pattern), "|", field("tail", $._pattern), "]"),

    struct_pattern: ($) =>
      seq(
        field("name", $.type_identifier),
        "{",
        commaSep(choice($.field_pattern, $.shorthand_field_pattern, $.rest_pattern)),
        optional(","),
        "}"
      ),

    // Full field pattern: `x: pattern`
    field_pattern: ($) =>
      seq(field("name", $.identifier), ":", field("pattern", $._pattern)),

    // Shorthand field pattern: just `x` binds field x to variable x
    shorthand_field_pattern: ($) => field("name", $.identifier),

    // Rest pattern: `..` ignores remaining fields
    rest_pattern: ($) => "..",

    enum_pattern: ($) =>
      seq(
        field("type", $.type_identifier),
        "::",
        field("variant", $.type_identifier),
        optional(seq("(", commaSep($._pattern), ")"))
      ),

    bitstring_pattern: ($) => seq("<<", commaSep($.bitstring_segment_pattern), ">>"),

    bitstring_segment_pattern: ($) =>
      seq(
        field("pattern", $._pattern),
        optional(seq(":", field("size", $._expression))),
        optional(seq("/", $.segment_specifiers))
      ),

    // Literals
    _literal: ($) =>
      choice($.integer, $.string, $.atom, $.boolean, $.unit),

    integer: ($) => /[0-9]+/,

    string: ($) =>
      seq('"', repeat(choice(/[^"\\]+/, $.escape_sequence)), '"'),

    escape_sequence: ($) => /\\./,

    atom: ($) => seq(":", /[a-z_][a-z0-9_]*/),

    boolean: ($) => choice("true", "false"),

    unit: ($) => seq("(", ")"),

    // Identifiers
    identifier: ($) => token(/[a-z_][a-z0-9_]*/),

    type_identifier: ($) => /[A-Z][a-zA-Z0-9_]*/,

    // Comments
    line_comment: ($) => seq("//", /[^\n]*/),

    block_comment: ($) => seq("/*", /[^*]*\*+([^/*][^*]*\*+)*/, "/"),
  },
});

function commaSep(rule) {
  return optional(commaSep1(rule));
}

function commaSep1(rule) {
  return seq(rule, repeat(seq(",", rule)));
}

function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}
