; Functions
(function_definition) @function.outer

(function_definition
  body: (block) @function.inner)

; Parameters
(parameter) @parameter.inner

(parameters
  (parameter) @parameter.outer)

; Classes (structs and enums)
(struct_definition) @class.outer

(struct_definition
  name: (type_identifier)
  "{" @class.inner.start
  "}" @class.inner.end)

(enum_definition) @class.outer

(enum_definition
  name: (type_identifier)
  "{" @class.inner.start
  "}" @class.inner.end)

; Blocks
(block) @block.outer

; Conditionals
(if_expression) @conditional.outer

(if_expression
  consequence: (block) @conditional.inner)

; Match/switch
(match_expression) @conditional.outer

(match_arm) @conditional.inner

; Loops (receive with after acts like a loop)
(receive_expression) @loop.outer

; Comments
(line_comment) @comment.outer
(block_comment) @comment.outer

; Calls
(call_expression) @call.outer

(call_expression
  arguments: ((_) @call.inner))

; Statements
(let_statement) @statement.outer
(expression_statement) @statement.outer
