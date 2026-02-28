; folds.scm - Tree-sitter code folding for ABAP

; Block statements
(if_statement) @fold
(case_statement) @fold
(case_type_statement) @fold
(do_statement) @fold
(while_statement) @fold
(loop_statement) @fold
(try_statement) @fold
(select_loop_statement) @fold

; OOP constructs
(class_definition) @fold
(class_implementation) @fold
(interface_definition) @fold
(method_implementation) @fold

; Modularization
(form_definition) @fold
(function_implementation) @fold
(module_implementation) @fold
(define_statement) @fold

; Nested blocks
(catch_clause) @fold
(cleanup_clause) @fold
(elseif_clause) @fold
(else_clause) @fold
(when_clause) @fold
(when_type_clause) @fold
(when_others_clause) @fold
(at_block) @fold

; Visibility sections
(visibility_section) @fold

; Obsolete blocks
(on_change_block) @fold
(provide_block) @fold
