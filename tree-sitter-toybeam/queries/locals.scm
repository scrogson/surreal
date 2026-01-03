; Scopes
(module) @local.scope
(function_definition) @local.scope
(block) @local.scope
(impl_block) @local.scope
(trait_definition) @local.scope
(trait_impl) @local.scope
(match_arm) @local.scope

; Definitions
(function_definition
  name: (identifier) @local.definition.function)

(trait_method
  name: (identifier) @local.definition.function)

(parameter
  pattern: (identifier) @local.definition.parameter)

(let_statement
  pattern: (identifier) @local.definition.var)

(struct_definition
  name: (type_identifier) @local.definition.type)

(enum_definition
  name: (type_identifier) @local.definition.type)

(trait_definition
  name: (type_identifier) @local.definition.type)

; Pattern bindings
(shorthand_field_pattern
  name: (identifier) @local.definition.var)

(field_pattern
  pattern: (identifier) @local.definition.var)

; References
(identifier) @local.reference
(type_identifier) @local.reference
