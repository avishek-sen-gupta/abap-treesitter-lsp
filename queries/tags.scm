; tags.scm - Tree-sitter code navigation tags for ABAP

; Class definitions
(class_definition
  (identifier) @name) @definition.class

; Class implementations
(class_implementation
  (identifier) @name) @definition.class

; Interface definitions
(interface_definition
  (identifier) @name) @definition.interface

; Method implementations
(method_implementation
  (identifier) @name) @definition.method

; Method declarations
(method_statement
  (identifier) @name) @definition.method

; Form definitions
(form_definition
  (identifier) @name) @definition.function

; Function implementations
(function_implementation
  (identifier) @name) @definition.function

; Module implementations
(module_implementation
  (identifier) @name) @definition.function

; Macro definitions
(define_statement
  (identifier) @name) @definition.macro

; Data declarations
(data_statement
  (identifier) @name) @definition.variable

; Constants declarations
(constants_statement
  (identifier) @name) @definition.constant

; Type declarations
(types_statement
  (identifier) @name) @definition.type

; Method calls
(method_call_expression
  (identifier) @name) @reference.call

; PERFORM calls
(perform_statement
  (_) @name) @reference.call

; CALL FUNCTION
(call_function_statement
  (_) @name) @reference.call
