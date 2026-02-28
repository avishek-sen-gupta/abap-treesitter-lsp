; locals.scm - Tree-sitter local variable tracking for ABAP

; ==========================================================================
; Scope definitions
; ==========================================================================

; Methods create a local scope
(method_implementation) @local.scope

; Forms create a local scope
(form_definition) @local.scope

; Functions create a local scope
(function_implementation) @local.scope

; DO/WHILE/LOOP blocks
(do_statement) @local.scope
(while_statement) @local.scope
(loop_statement) @local.scope

; TRY blocks
(try_statement) @local.scope

; Class definitions
(class_definition) @local.scope
(class_implementation) @local.scope

; Interface definitions
(interface_definition) @local.scope

; ==========================================================================
; Variable definitions
; ==========================================================================

; DATA declarations
(data_statement (identifier) @local.definition)

; CONSTANTS
(constants_statement (identifier) @local.definition)

; STATICS
(statics_statement (identifier) @local.definition)

; FIELD-SYMBOLS
(field_symbols_statement (field_symbol) @local.definition)

; Inline DATA(var)
(inline_data_declaration (identifier) @local.definition)

; Inline FIELD-SYMBOL(<fs>)
(inline_field_symbol (field_symbol) @local.definition)

; Method parameters (from method declaration)
(method_param (identifier) @local.definition)

; FORM parameters
(form_definition (identifier) @local.definition)

; ==========================================================================
; Variable references
; ==========================================================================

(identifier) @local.reference
(field_symbol) @local.reference
