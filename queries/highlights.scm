; highlights.scm - Tree-sitter highlighting queries for ABAP

; ==========================================================================
; Comments
; ==========================================================================
(comment) @comment
(line_comment) @comment

; ==========================================================================
; Literals
; ==========================================================================
(string_literal) @string
(hex_literal) @string.special
(integer_literal) @number
(float_literal) @number.float

; ==========================================================================
; Identifiers & Variables
; ==========================================================================
(identifier) @variable
(field_symbol) @variable.special
(namespace_identifier) @variable

; Data declarations - highlight declared names
(data_statement (identifier) @variable.definition)
(constants_statement (identifier) @variable.definition)
(statics_statement (identifier) @variable.definition)
(class_data_statement (identifier) @variable.definition)
(field_symbols_statement (field_symbol) @variable.definition)
(parameters_statement (identifier) @variable.definition)
(select_options_statement (identifier) @variable.definition)
(tables_statement (identifier) @variable.definition)

; Inline declarations
(inline_data_declaration (identifier) @variable.definition)
(inline_field_symbol (field_symbol) @variable.definition)

; ==========================================================================
; Types & Classes
; ==========================================================================
(class_definition (identifier) @type.definition)
(class_implementation (identifier) @type)
(interface_definition (identifier) @type.definition)

; ==========================================================================
; Functions & Methods
; ==========================================================================
(method_implementation (identifier) @function.method)
(method_statement (identifier) @function.method)
(form_definition (identifier) @function)
(function_implementation (identifier) @function)
(module_implementation (identifier) @function)
(define_statement (identifier) @function.macro)

; Method calls
(method_call_expression
  (identifier) @function.call)
(method_call_expression
  (component_access
    (identifier) @function.call .))

; Named arguments
(named_argument (identifier) @variable.parameter)

; ==========================================================================
; Operators
; ==========================================================================
[
  "="
  "<>"
  "<"
  ">"
  "<="
  ">="
  "+"
  "-"
  "*"
  "/"
  "**"
  "&&"
  "+="
  "-="
  "*="
  "/="
  "&&="
] @operator

; ==========================================================================
; Punctuation
; ==========================================================================
["." "," ":"] @punctuation.delimiter
["(" ")" "[" "]"] @punctuation.bracket
["->" "=>" "~"] @punctuation.delimiter

; ==========================================================================
; Keywords - Logical & comparison operators as keywords
; ==========================================================================
[
  "AND"
  "OR"
  "NOT"
  "EQUIV"
  "EQ"
  "NE"
  "LT"
  "GT"
  "LE"
  "GE"
  "CO"
  "CN"
  "CA"
  "CS"
  "NA"
  "NS"
  "CP"
  "NP"
  "MOD"
  "DIV"
  "BIT-AND"
  "BIT-OR"
  "BIT-XOR"
  "BIT-NOT"
  "IS"
  "INITIAL"
  "ASSIGNED"
  "BOUND"
  "SUPPLIED"
  "REQUESTED"
  "INSTANCE"
  "BETWEEN"
  "NOT BETWEEN"
] @keyword.operator

; ==========================================================================
; Keywords - Flow exit
; ==========================================================================
[
  "RETURN"
  "EXIT"
  "CHECK"
  "CONTINUE"
  "LEAVE"
] @keyword.return

; ==========================================================================
; Keywords - Exception
; ==========================================================================
[
  "RAISE"
  "EXCEPTION"
  "SHORTDUMP"
  "TRY"
  "CATCH"
  "CLEANUP"
  "ENDTRY"
  "BEFORE UNWIND"
] @keyword.exception

; ==========================================================================
; Keywords - Control flow
; ==========================================================================
[
  "IF"
  "ELSEIF"
  "ELSE"
  "ENDIF"
  "CASE"
  "WHEN"
  "OTHERS"
  "ENDCASE"
  "DO"
  "TIMES"
  "ENDDO"
  "WHILE"
  "ENDWHILE"
  "LOOP"
  "AT"
  "ENDLOOP"
  "ENDAT"
  "FIRST"
  "LAST"
  "END OF"
] @keyword.control

; ==========================================================================
; Keywords - OOP
; ==========================================================================
[
  "CLASS"
  "ENDCLASS"
  "INTERFACE"
  "ENDINTERFACE"
  "METHOD"
  "ENDMETHOD"
  "METHODS"
  "CLASS-METHODS"
  "DEFINITION"
  "IMPLEMENTATION"
  "INHERITING"
  "ABSTRACT"
  "FINAL"
  "CREATE"
  "FOR TESTING"
  "RISK LEVEL"
  "FRIENDS"
  "DEFERRED"
  "LOCAL FRIENDS"
  "PUBLIC"
  "PROTECTED"
  "PRIVATE"
  "SECTION"
  "INTERFACES"
  "ALIASES"
  "EVENTS"
  "CLASS-EVENTS"
  "REDEFINITION"
  "OPTIONAL"
  "REFERENCE"
] @keyword

; ==========================================================================
; Keywords - Declarations
; ==========================================================================
[
  "DATA"
  "TYPES"
  "CONSTANTS"
  "STATICS"
  "CLASS-DATA"
  "FIELD-SYMBOLS"
  "FIELD-SYMBOL"
  "PARAMETERS"
  "SELECT-OPTIONS"
  "TABLES"
  "TYPE-POOLS"
  "TYPE"
  "LIKE"
  "REF"
  "VALUE"
  "BEGIN"
  "END"
  "OF"
  "RANGE"
  "LINE"
  "LENGTH"
  "DECIMALS"
  "READ-ONLY"
  "DEFAULT"
  "OBLIGATORY"
  "STRUCTURE"
  "ENUM"
] @keyword

; ==========================================================================
; Keywords - Table types
; ==========================================================================
[
  "TABLE"
  "STANDARD"
  "SORTED"
  "HASHED"
  "KEY"
  "UNIQUE"
  "NON-UNIQUE"
  "EMPTY"
  "WITH"
  "WITH KEY"
  "WITH TABLE KEY"
] @keyword

; ==========================================================================
; Keywords - Program introductions
; ==========================================================================
[
  "REPORT"
  "PROGRAM"
  "FUNCTION-POOL"
  "CLASS-POOL"
  "INTERFACE-POOL"
  "TYPE-POOL"
  "INCLUDE"
] @keyword

; ==========================================================================
; Keywords - Modularization
; ==========================================================================
[
  "FORM"
  "ENDFORM"
  "USING"
  "FUNCTION"
  "ENDFUNCTION"
  "MODULE"
  "ENDMODULE"
  "INPUT"
  "OUTPUT"
  "DEFINE"
  "END-OF-DEFINITION"
  "PERFORM"
  "PERFORMING"
  "IF FOUND"
  "IN"
  "IMPORTING"
  "EXPORTING"
  "CHANGING"
  "RETURNING"
  "RAISING"
  "EXCEPTIONS"
  "RECEIVING"
] @keyword

; ==========================================================================
; Keywords - Assignments & operations
; ==========================================================================
[
  "MOVE"
  "MOVE-CORRESPONDING"
  "ASSIGN"
  "UNASSIGN"
  "CLEAR"
  "FREE"
  "GET"
  "INTO"
  "ASSIGNING"
  "REFERENCE INTO"
  "TRANSPORTING NO FIELDS"
  "CASTING"
  "HANDLE"
  "TO"
  "FROM"
  "NEW"
  "SET"
] @keyword

; ==========================================================================
; Keywords - Internal tables
; ==========================================================================
[
  "READ TABLE"
  "APPEND"
  "MODIFY"
  "DELETE"
  "SORT"
  "COLLECT"
  "DESCRIBE TABLE"
  "INSERT"
  "INDEX"
  "BINARY SEARCH"
  "TRANSPORTING"
  "COMPARING"
  "ASCENDING"
  "DESCENDING"
  "STABLE"
  "AS TEXT"
  "LINES OF"
  "ADJACENT DUPLICATES"
  "ALL"
  "FIELDS"
] @keyword

; ==========================================================================
; Keywords - String operations
; ==========================================================================
[
  "CONCATENATE"
  "SPLIT"
  "FIND"
  "REPLACE"
  "SEARCH"
  "CONDENSE"
  "TRANSLATE"
  "SHIFT"
  "OVERLAY"
  "SEPARATED BY"
  "RESPECTING BLANKS"
  "REGEX"
  "SUBSTRING"
  "OCCURRENCES"
  "IGNORING CASE"
  "RESPECTING CASE"
  "UPPER"
  "LOWER"
  "LEFT"
  "RIGHT"
  "CIRCULAR"
  "PLACES"
  "DELETING LEADING"
  "DELETING TRAILING"
  "NO-GAPS"
  "ONLY"
  "MATCH"
  "RESULTS"
  "SUBMATCHES"
  "OFFSET"
] @keyword

; ==========================================================================
; Keywords - SQL
; ==========================================================================
[
  "SELECT"
  "SINGLE"
  "DISTINCT"
  "WHERE"
  "GROUP BY"
  "HAVING"
  "ORDER BY"
  "UP"
  "ROWS"
  "ENDSELECT"
  "INNER JOIN"
  "LEFT OUTER JOIN"
  "LEFT JOIN"
  "RIGHT OUTER JOIN"
  "RIGHT JOIN"
  "CROSS JOIN"
  "APPENDING"
  "CORRESPONDING FIELDS OF"
  "VALUES"
  "UPDATE"
  "COMMIT WORK"
  "AND WAIT"
  "ROLLBACK WORK"
  "OPEN CURSOR"
  "WITH HOLD"
  "FETCH NEXT CURSOR"
  "CLOSE CURSOR"
  "AS"
  "ON"
] @keyword

; SQL aggregate functions
[
  "COUNT"
  "SUM"
  "AVG"
  "MIN"
  "MAX"
] @function.builtin

; ==========================================================================
; Keywords - Constructor expressions (7.40+)
; ==========================================================================
[
  "CONV"
  "CAST"
  "EXACT"
  "CORRESPONDING"
  "COND"
  "SWITCH"
  "FILTER"
  "REDUCE"
  "INIT"
  "NEXT"
  "THEN"
  "UNTIL"
  "BASE"
  "LET"
  "FOR"
] @keyword

; ==========================================================================
; Keywords - Dialogs & messages
; ==========================================================================
[
  "SELECTION-SCREEN"
  "BLOCK"
  "FRAME"
  "TITLE"
  "SKIP"
  "ULINE"
  "COMMENT"
  "PUSHBUTTON"
  "TAB"
  "USER-COMMAND"
  "SCREEN"
  "SUBSCREEN"
  "WRITE"
  "MESSAGE"
  "ID"
  "NUMBER"
  "DISPLAY LIKE"
  "COLOR"
  "INTENSIFIED"
  "INTENSIFIED OFF"
  "INVERSE"
  "INVERSE OFF"
  "HOTSPOT"
  "HOTSPOT OFF"
  "NO-GAP"
  "NO-ZERO"
  "NO-SIGN"
  "LEFT-JUSTIFIED"
  "CENTERED"
  "RIGHT-JUSTIFIED"
  "FORMAT"
  "RESET"
  "NEW-LINE"
  "NEW-PAGE"
  "PF-STATUS"
  "TITLEBAR"
  "NO-HEADING"
  "NO-TITLE"
  "WITH-HEADING"
  "WITH-TITLE"
  "LINE-SIZE"
  "LINE-COUNT"
  "PRINT ON"
  "PRINT OFF"
  "INPUT"
  "INPUT OFF"
  "UNDER"
  "CURRENCY"
  "ROUND"
  "NO-GROUPING"
  "DD/MM/YY"
  "DD/MM/YYYY"
  "MM/DD/YY"
  "MM/DD/YYYY"
] @keyword

; ==========================================================================
; Keywords - Remaining
; ==========================================================================
[
  "AUTHORITY-CHECK OBJECT"
  "FIELD"
  "DUMMY"
  "PARAMETER"
  "CALL TRANSFORMATION"
  "OPTIONS"
  "SOURCE"
  "RESULT"
  "XML"
  "EXPORT"
  "IMPORT"
  "DATA BUFFER"
  "MEMORY"
  "DATABASE"
  "CLIENT"
  "SHARED MEMORY"
  "SHARED BUFFER"
  "INTERNAL TABLE"
  "CREATE OBJECT"
  "SET HANDLER"
  "ALL INSTANCES"
  "ACTIVATION"
  "DESCRIBE FIELD"
  "OUTPUT-LENGTH"
  "HELP-ID"
  "COMPONENTS"
  "EDIT MASK"
  "SET SCREEN"
  "CALL METHOD"
  "CALL FUNCTION"
  "DESTINATION"
  "IN UPDATE TASK"
  "IN BACKGROUND TASK"
  "STARTING NEW TASK"
  "TASK"
  "SUBMIT"
  "VIA SELECTION-SCREEN"
  "AND RETURN"
  "SELECTION-SET"
  "SAP-SPOOL"
  "SPOOL PARAMETERS"
  "CALL TRANSACTION"
  "AND SKIP FIRST SCREEN"
  "CALL SCREEN"
  "STARTING AT"
  "ENDING AT"
  "ACCEPTING DUPLICATE KEYS"
  "BYTE MODE"
  "CHARACTER MODE"
  "EXCLUDING"
  "OF PROGRAM"
  "IMMEDIATELY"
  "MESSAGES"
  "MODE"
  "BY"
  "OBJECT"
  "MATCHCODE"
  "MODIF"
  "AS CHECKBOX"
  "AS LISTBOX"
  "RADIOBUTTON GROUP"
  "NO-DISPLAY"
  "NO-EXTENSION"
  "NO INTERVALS"
  "SIGN"
  "OPTION"
  "LIST-PROCESSING"
  "TRANSACTION"
  "MESSAGE-ID"
  "STANDARD PAGE"
  "HEADING"
  "NO"
  "RENAMING"
  "SUFFIX"
  "PROVIDE"
  "ENDPROVIDE"
  "CHANGE OF"
  "ENDON"
  "TESTING"
  "EVENT"
  "FAIL"
  "IGNORE"
  "EDIT"
  "MASK"
  "KIND"
  "LINES"
  "ABBREVIATED"
  "AND MARK"
  "WITH NULL"
  "IS INITIAL"
  "CRITICAL"
  "DANGEROUS"
  "HARMLESS"
  "SHORT"
  "MEDIUM"
  "LONG"
  "DURATION"
] @keyword

; ==========================================================================
; Keywords - Event blocks
; ==========================================================================
[
  "INITIALIZATION"
  "START-OF-SELECTION"
  "END-OF-SELECTION"
  "TOP-OF-PAGE"
  "END-OF-PAGE"
  "AT LINE-SELECTION"
  "AT USER-COMMAND"
  "AT SELECTION-SCREEN"
  "VALUE-REQUEST"
  "HELP-REQUEST"
  "DURING"
  "LINE-SELECTION"
] @keyword

; ==========================================================================
; Builtin types
; ==========================================================================
[
  "i" "f" "p" "c" "n" "d" "t" "x"
  "string" "xstring"
  "decfloat16" "decfloat34"
  "int8" "utclong"
  "any" "any table"
  "clike" "csequence" "numeric" "xsequence"
  "data" "object"
] @type.builtin

; ==========================================================================
; Visibility sections
; ==========================================================================
(visibility_section) @keyword

; ==========================================================================
; Error recovery
; ==========================================================================
(ERROR) @error
