; Keywords
[
  "mod"
  "fn"
  "let"
  "mut"
  "if"
  "else"
  "match"
  "struct"
  "enum"
  "spawn"
  "receive"
  "after"
  "return"
  "true"
  "false"
  "use"
  "as"
  "impl"
  "trait"
  "for"
  "self"
] @keyword

; Operators
[
  "+"
  "-"
  "*"
  "/"
  "%"
  "=="
  "!="
  "<"
  "<="
  ">"
  ">="
  "&&"
  "||"
  "!"
  "="
  "->"
  "=>"
  "::"
  "|"
  "|>"
] @operator

; Binary syntax operators
[
  "<<"
  ">>"
] @operator

; Binary segment specifiers (these are inside segment_specifier node)
(segment_specifier) @keyword.modifier

; Rest pattern
(rest_pattern) @punctuation.special

; Punctuation
[
  "("
  ")"
  "{"
  "}"
  "["
  "]"
] @punctuation.bracket

[
  ","
  ";"
  ":"
  "."
] @punctuation.delimiter

; Literals
(integer) @number
(string) @string
(escape_sequence) @string.escape
(atom) @string.special.symbol
(boolean) @boolean
(unit) @constant.builtin

; Types
(type_identifier) @type
(primitive_type) @type.builtin

; Functions
(function_definition
  name: (identifier) @function)

(trait_method
  name: (identifier) @function)

(call_expression
  function: (identifier) @function.call)

(method_call_expression
  method: (identifier) @function.method.call)

; Variables and parameters
(parameter
  pattern: (identifier) @variable.parameter)

(let_statement
  pattern: (identifier) @variable)

(identifier) @variable

; Self keyword
(self_expression) @variable.builtin
(self_parameter) @variable.builtin

; Struct and enum names
(struct_definition
  name: (type_identifier) @type.definition)

(enum_definition
  name: (type_identifier) @type.definition)

(enum_variant
  name: (type_identifier) @constructor)

(struct_expression
  name: (type_identifier) @type)

(struct_pattern
  name: (type_identifier) @type)

(enum_pattern
  type: (type_identifier) @type
  variant: (type_identifier) @constructor)

; Impl blocks
(impl_block
  type: (type_identifier) @type)

; Trait definitions
(trait_definition
  name: (type_identifier) @type.definition)

; Trait implementations
(trait_impl
  trait: (type_identifier) @type
  type: (type_identifier) @type)

; Fields
(field_expression
  field: (identifier) @property)

(struct_field
  name: (identifier) @property)

(field_initializer
  name: (identifier) @property)

(field_pattern
  name: (identifier) @property)

(shorthand_field_pattern
  name: (identifier) @property)

; Module
(module
  name: (identifier) @module)

(mod_declaration
  name: (identifier) @module)

; Use declarations
(use_declaration
  (use_tree
    module: (identifier) @module))

(use_tree_item
  name: (identifier) @variable)

; Path expressions
(path_expression
  path: (type_identifier) @type)

; Comments
(line_comment) @comment
(block_comment) @comment

; Wildcards
(wildcard_pattern) @variable.builtin

; Visibility (pub keyword)
(visibility) @keyword.modifier
