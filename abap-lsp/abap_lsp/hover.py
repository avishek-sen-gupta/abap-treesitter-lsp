"""Hover information for ABAP keywords and built-in types."""

from lsprotocol.types import Hover, MarkupContent, MarkupKind, Position, Range

from tree_sitter import Node

# ---------------------------------------------------------------------------
# ABAP keyword documentation, organized by category
# ---------------------------------------------------------------------------

_DECLARATIONS = {
    "DATA": "**DATA** *var* **TYPE** *type* [**VALUE** *val*].\n\nDeclare a variable.",
    "CLASS-DATA": "**CLASS-DATA** *var* **TYPE** *type*.\n\nDeclare a static class attribute.",
    "TYPES": "**TYPES** *type* **TYPE** *base_type*.\n\nDefine a local type.",
    "CONSTANTS": "**CONSTANTS** *const* **TYPE** *type* **VALUE** *val*.\n\nDeclare a constant.",
    "STATICS": "**STATICS** *var* **TYPE** *type*.\n\nDeclare a static variable within a method/form.",
    "FIELD-SYMBOLS": "**FIELD-SYMBOLS** *<fs>* **TYPE** *type*.\n\nDeclare a field symbol (pointer-like reference).",
    "PARAMETERS": "**PARAMETERS** *p* **TYPE** *type*.\n\nDeclare a selection screen input parameter.",
    "SELECT-OPTIONS": "**SELECT-OPTIONS** *so* **FOR** *field*.\n\nDeclare a selection screen range table.",
    "TABLES": "**TABLES** *dbtab*.\n\nDeclare a work area for a database table (obsolete).",
    "TYPE-POOLS": "**TYPE-POOLS** *pool*.\n\nInclude a type pool (obsolete in ABAP Objects).",
    "INCLUDE": "**INCLUDE** *program*.\n\nInclude another program's source code.",
}

_OOP = {
    "CLASS": "**CLASS** *name* **DEFINITION** [**INHERITING FROM** *super*].\n\nDefine a class.",
    "ENDCLASS": "**ENDCLASS.**\n\nEnd of class definition or implementation.",
    "INTERFACE": "**INTERFACE** *name*.\n\nDefine an interface.",
    "ENDINTERFACE": "**ENDINTERFACE.**\n\nEnd of interface definition.",
    "METHOD": "**METHOD** *name*.\n\nBegin a method implementation.",
    "ENDMETHOD": "**ENDMETHOD.**\n\nEnd of method implementation.",
    "METHODS": "**METHODS** *name* [**IMPORTING** ...] [**RETURNING** **VALUE(** *rv* **)**].\n\nDeclare a method signature.",
    "CLASS-METHODS": "**CLASS-METHODS** *name* ...\n\nDeclare a static method.",
    "INTERFACES": "**INTERFACES** *intf*.\n\nInclude an interface in a class definition.",
    "ALIASES": "**ALIASES** *alias* **FOR** *intf~method*.\n\nCreate an alias for an interface method.",
    "EVENTS": "**EVENTS** *evt* [**EXPORTING** ...].\n\nDeclare an instance event.",
    "CLASS-EVENTS": "**CLASS-EVENTS** *evt* ...\n\nDeclare a static event.",
    "CREATE": "**CREATE OBJECT** *ref* [**TYPE** *class*] [**EXPORTING** ...].\n\nInstantiate an object.",
    "INHERITING": "**INHERITING FROM** *superclass*\n\nInheritance clause in class definition.",
    "ABSTRACT": "**ABSTRACT**\n\nModifier: class or method is abstract.",
    "FINAL": "**FINAL**\n\nModifier: class or method is final (cannot be overridden/inherited).",
    "REDEFINITION": "**REDEFINITION**\n\nRedeclare an inherited method.",
    "PUBLIC": "**PUBLIC SECTION.**\n\nPublic visibility section in a class.",
    "PROTECTED": "**PROTECTED SECTION.**\n\nProtected visibility section in a class.",
    "PRIVATE": "**PRIVATE SECTION.**\n\nPrivate visibility section in a class.",
    "FRIENDS": "**FRIENDS** *class1* *class2* ...\n\nGrant friend access to other classes.",
    "DEFINITION": "**DEFINITION**\n\nClass/interface definition part.",
    "IMPLEMENTATION": "**IMPLEMENTATION**\n\nClass implementation part.",
    "SET": "**SET HANDLER** *handler* **FOR** *obj*.\n\nRegister an event handler.",
    "RAISE": "**RAISE** [**EXCEPTION**] [**TYPE** *cx_class*].\n\nRaise an exception or classic event.",
}

_CONTROL_FLOW = {
    "IF": "**IF** *condition*.\n\nConditional execution.",
    "ELSEIF": "**ELSEIF** *condition*.\n\nAlternate condition in IF block.",
    "ELSE": "**ELSE.**\n\nDefault branch in IF/CASE block.",
    "ENDIF": "**ENDIF.**\n\nEnd of IF block.",
    "CASE": "**CASE** *expression*.\n\nMulti-way conditional.",
    "WHEN": "**WHEN** *value*.\n\nBranch in CASE statement.",
    "ENDCASE": "**ENDCASE.**\n\nEnd of CASE block.",
    "DO": "**DO** [*n* **TIMES**].\n\nUnconditional loop.",
    "ENDDO": "**ENDDO.**\n\nEnd of DO loop.",
    "WHILE": "**WHILE** *condition*.\n\nConditional loop.",
    "ENDWHILE": "**ENDWHILE.**\n\nEnd of WHILE loop.",
    "LOOP": "**LOOP AT** *itab* [**INTO** *wa*] [**ASSIGNING** *<fs>*] [**WHERE** *cond*].\n\nLoop over internal table.",
    "ENDLOOP": "**ENDLOOP.**\n\nEnd of LOOP block.",
    "EXIT": "**EXIT.**\n\nLeave current loop or processing block.",
    "CONTINUE": "**CONTINUE.**\n\nSkip to next loop iteration.",
    "CHECK": "**CHECK** *condition*.\n\nContinue only if condition is true; otherwise exit loop/event.",
    "RETURN": "**RETURN.**\n\nReturn from current method/form.",
    "STOP": "**STOP.**\n\nStop list processing and go to END-OF-SELECTION.",
    "LEAVE": "**LEAVE** [**PROGRAM** | **TO SCREEN** *n* | **LIST-PROCESSING**].\n\nLeave current processing.",
}

_EXCEPTIONS = {
    "TRY": "**TRY.**\n\nBegin exception handling block.",
    "CATCH": "**CATCH** *cx_class* [**INTO** *ref*].\n\nCatch an exception.",
    "CLEANUP": "**CLEANUP.**\n\nCleanup section in TRY block (runs before propagating exception).",
    "ENDTRY": "**ENDTRY.**\n\nEnd of TRY block.",
    "RAISE EXCEPTION": "**RAISE EXCEPTION TYPE** *cx_class* [**EXPORTING** ...].\n\nRaise a class-based exception.",
    "RETRY": "**RETRY.**\n\nRetry the TRY block from the beginning (inside CATCH).",
    "RESUME": "**RESUME.**\n\nResume after a resumable exception.",
}

_INTERNAL_TABLES = {
    "APPEND": "**APPEND** [*wa* **TO** | **INITIAL LINE TO**] *itab*.\n\nAppend a line to an internal table.",
    "INSERT": "**INSERT** [*wa* **INTO**] [**TABLE**] *itab* [**INDEX** *idx*].\n\nInsert a line into an internal table.",
    "MODIFY": "**MODIFY** [**TABLE**] *itab* **FROM** *wa* [**WHERE** *cond*].\n\nModify lines of an internal table.",
    "DELETE": "**DELETE** [**TABLE**] *itab* [**WHERE** *cond*] [**INDEX** *idx*].\n\nDelete lines from an internal table.",
    "READ": "**READ TABLE** *itab* [**INTO** *wa*] [**WITH KEY** *key*] [**INDEX** *idx*].\n\nRead a single line from an internal table.",
    "SORT": "**SORT** *itab* [**BY** *comp1* **ASCENDING** | **DESCENDING**].\n\nSort an internal table.",
    "COLLECT": "**COLLECT** *wa* **INTO** *itab*.\n\nInsert or aggregate a line in a sorted/hashed table.",
    "CLEAR": "**CLEAR** *var*.\n\nReset a variable to its type-appropriate initial value.",
    "FREE": "**FREE** *var*.\n\nRelease memory for a variable/internal table.",
    "REFRESH": "**REFRESH** *itab*.\n\nClear all lines from an internal table (obsolete, use CLEAR).",
    "DESCRIBE": "**DESCRIBE TABLE** *itab* **LINES** *count*.\n\nGet the number of lines in an internal table.",
    "ASSIGN": "**ASSIGN** *source* **TO** *<fs>*.\n\nAssign a data object to a field symbol.",
    "UNASSIGN": "**UNASSIGN** *<fs>*.\n\nUnassign a field symbol.",
}

_STRING_OPS = {
    "CONCATENATE": "**CONCATENATE** *s1* *s2* ... **INTO** *result* [**SEPARATED BY** *sep*].\n\nConcatenate strings.",
    "SPLIT": "**SPLIT** *string* **AT** *sep* **INTO** *s1* *s2* ... | **TABLE** *itab*.\n\nSplit a string at a separator.",
    "FIND": "**FIND** [**ALL OCCURRENCES OF**] *pattern* **IN** *string* [**RESULTS** *tab*].\n\nSearch for a pattern in a string.",
    "REPLACE": "**REPLACE** [**ALL OCCURRENCES OF**] *pattern* **IN** *string* **WITH** *new*.\n\nReplace occurrences in a string.",
    "SEARCH": "**SEARCH** *string* **FOR** *pattern*.\n\nSearch for a pattern (obsolete, use FIND).",
    "CONDENSE": "**CONDENSE** *string* [**NO-GAPS**].\n\nRemove leading/trailing spaces; compress internal spaces.",
    "TRANSLATE": "**TRANSLATE** *string* **TO** **UPPER**|**LOWER CASE** | **USING** *mask*.\n\nConvert case or translate characters.",
    "SHIFT": "**SHIFT** *string* [**BY** *n* **PLACES**] [**LEFT**|**RIGHT**] [**DELETING LEADING** *mask*].\n\nShift a string left or right.",
    "OVERLAY": "**OVERLAY** *string1* **WITH** *string2* [**ONLY** *mask*].\n\nOverlay characters in a string.",
}

_SQL = {
    "SELECT": "**SELECT** *fields* **FROM** *dbtab* [**WHERE** *cond*] [**INTO** *target*].\n\nRead data from the database.",
    "ENDSELECT": "**ENDSELECT.**\n\nEnd of SELECT loop.",
    "UPDATE": "**UPDATE** *dbtab* **SET** *field* **=** *value* [**WHERE** *cond*].\n\nUpdate database records.",
    "MODIFY": "**MODIFY** *dbtab* **FROM** [**TABLE**] *wa*.\n\nInsert or update database records.",
    "DELETE FROM": "**DELETE FROM** *dbtab* **WHERE** *cond*.\n\nDelete database records.",
    "INSERT INTO": "**INSERT INTO** *dbtab* **VALUES** *wa*.\n\nInsert a database record.",
    "OPEN CURSOR": "**OPEN CURSOR** *cursor* **FOR SELECT** ...\n\nOpen a database cursor.",
    "FETCH": "**FETCH NEXT CURSOR** *cursor* **INTO** *wa*.\n\nFetch next row from a cursor.",
    "CLOSE CURSOR": "**CLOSE CURSOR** *cursor*.\n\nClose a database cursor.",
    "COMMIT": "**COMMIT WORK** [**AND WAIT**].\n\nCommit the current database LUW.",
    "ROLLBACK": "**ROLLBACK WORK.**\n\nRollback the current database LUW.",
}

_SUBROUTINES = {
    "FORM": "**FORM** *name* [**USING** ...] [**CHANGING** ...].\n\nDefine a subroutine (obsolete, use methods).",
    "ENDFORM": "**ENDFORM.**\n\nEnd of FORM subroutine.",
    "PERFORM": "**PERFORM** *name* [**USING** ...] [**CHANGING** ...].\n\nCall a subroutine.",
    "FUNCTION": "**FUNCTION** *name*.\n\nBegin a function module implementation.",
    "ENDFUNCTION": "**ENDFUNCTION.**\n\nEnd of function module.",
    "CALL": "**CALL** **FUNCTION** *name* [**EXPORTING** ...] [**IMPORTING** ...] [**TABLES** ...] [**EXCEPTIONS** ...].\n\nCall a function module.",
    "MODULE": "**MODULE** *name* [**INPUT**|**OUTPUT**].\n\nBegin a dialog module.",
    "ENDMODULE": "**ENDMODULE.**\n\nEnd of dialog module.",
    "DEFINE": "**DEFINE** *name*.\n\nBegin a macro definition.",
    "END-OF-DEFINITION": "**END-OF-DEFINITION.**\n\nEnd of macro definition.",
}

_ASSIGNMENTS = {
    "MOVE": "**MOVE** *source* **TO** *target*.\n\nAssign a value (obsolete, use `=`).",
    "MOVE-CORRESPONDING": "**MOVE-CORRESPONDING** *struct1* **TO** *struct2*.\n\nCopy matching components between structures.",
    "COMPUTE": "**COMPUTE** *target* **=** *expression*.\n\nCompute and assign (obsolete, use `=`).",
    "ADD": "**ADD** *n* **TO** *var*.\n\nAdd a value to a variable.",
    "SUBTRACT": "**SUBTRACT** *n* **FROM** *var*.\n\nSubtract a value from a variable.",
    "MULTIPLY": "**MULTIPLY** *var* **BY** *n*.\n\nMultiply a variable by a value.",
    "DIVIDE": "**DIVIDE** *var* **BY** *n*.\n\nDivide a variable by a value.",
    "GET": "**GET REFERENCE OF** *dobj* **INTO** *ref*.\n\nGet a data reference.",
}

_CONSTRUCTOR_EXPRESSIONS = {
    "NEW": "**NEW** *type* **(** ... **)**\n\nConstructor expression: create a new object or data object.",
    "VALUE": "**VALUE** *type* **(** ... **)**\n\nConstructor expression: construct a value.",
    "REF": "**REF** *type* **(** *dobj* **)**\n\nConstructor expression: create a data reference.",
    "CONV": "**CONV** *type* **(** *expr* **)**\n\nConstructor expression: convert to target type.",
    "CAST": "**CAST** *type* **(** *ref* **)**\n\nConstructor expression: downcast a reference.",
    "EXACT": "**EXACT** *type* **(** *expr* **)**\n\nConstructor expression: lossless conversion.",
    "CORRESPONDING": "**CORRESPONDING** *type* **(** *struct* [**MAPPING** ...] **)**\n\nConstructor expression: component mapping.",
    "COND": "**COND** *type* **(** **WHEN** *c1* **THEN** *v1* ... [**ELSE** *vn*] **)**\n\nConditional expression.",
    "SWITCH": "**SWITCH** *type* **(** *expr* **WHEN** *v1* **THEN** *r1* ... **)**\n\nSwitch expression.",
    "FILTER": "**FILTER** *type* **(** *itab* [**WHERE** *cond*] **)**\n\nFilter expression for internal tables.",
    "REDUCE": "**REDUCE** *type* **(** **INIT** *x* = ... **FOR** *wa* **IN** *itab* **NEXT** *x* = ... **)**\n\nReduce expression.",
    "LET": "**LET** *var* **=** *expr* **IN**\n\nLocal binding in constructor expressions.",
    "FOR": "**FOR** *wa* **IN** *itab* [**WHERE** *cond*]\n\nIteration expression in VALUE/REDUCE.",
}

_EVENTS = {
    "INITIALIZATION": "**INITIALIZATION.**\n\nEvent: before selection screen is displayed.",
    "START-OF-SELECTION": "**START-OF-SELECTION.**\n\nEvent: begin main processing.",
    "END-OF-SELECTION": "**END-OF-SELECTION.**\n\nEvent: after all data has been selected.",
    "TOP-OF-PAGE": "**TOP-OF-PAGE.**\n\nEvent: beginning of a new page in list output.",
    "END-OF-PAGE": "**END-OF-PAGE.**\n\nEvent: end of a page in list output.",
    "AT LINE-SELECTION": "**AT LINE-SELECTION.**\n\nEvent: user double-clicks a list line.",
    "AT USER-COMMAND": "**AT USER-COMMAND.**\n\nEvent: user triggers a function code.",
    "AT SELECTION-SCREEN": "**AT SELECTION-SCREEN** [**ON** *field*].\n\nEvent: selection screen processing.",
    "RAISE EVENT": "**RAISE EVENT** *evt* [**EXPORTING** ...].\n\nTrigger an event.",
    "SET HANDLER": "**SET HANDLER** *handler* **FOR** *obj*.\n\nRegister an event handler method.",
}

_SCREEN = {
    "WRITE": "**WRITE** [**AT** *pos*] *value* [**COLOR** *col*].\n\nOutput to list.",
    "ULINE": "**ULINE** [**AT** *pos*].\n\nDraw a horizontal line.",
    "SKIP": "**SKIP** [*n*].\n\nSkip lines in list output.",
    "NEW-LINE": "**NEW-LINE.**\n\nStart a new line in list output.",
    "NEW-PAGE": "**NEW-PAGE** [**PRINT ON**|**OFF**].\n\nStart a new page in list output.",
    "FORMAT": "**FORMAT** [**COLOR** *col*] [**INTENSIFIED** **ON**|**OFF**].\n\nSet output formatting.",
    "SET PF-STATUS": "**SET PF-STATUS** *status*.\n\nSet the GUI status (menu bar, toolbar).",
    "SET TITLEBAR": "**SET TITLEBAR** *title*.\n\nSet the title bar text.",
    "CALL SCREEN": "**CALL SCREEN** *dynnr*.\n\nCall a dialog screen.",
    "SET SCREEN": "**SET SCREEN** *dynnr*.\n\nSet the next screen number.",
    "LEAVE SCREEN": "**LEAVE SCREEN.**\n\nLeave the current screen.",
    "SUPPRESS DIALOG": "**SUPPRESS DIALOG.**\n\nSuppress screen display in dialog processing.",
    "SELECTION-SCREEN": "**SELECTION-SCREEN** **BEGIN OF BLOCK** *name* ...\n\nDefine selection screen layout.",
}

_MISC = {
    "REPORT": "**REPORT** *name* [**LINE-SIZE** *n*] [**LINE-COUNT** *n*] [**NO STANDARD PAGE HEADING**].\n\nProgram header for executable program.",
    "PROGRAM": "**PROGRAM** *name*.\n\nProgram header (alternative to REPORT).",
    "FUNCTION-POOL": "**FUNCTION-POOL** *name*.\n\nFunction group header.",
    "MESSAGE": "**MESSAGE** *msg* [**TYPE** *t*] [**DISPLAY LIKE** *t*].\n\nDisplay a message.",
    "AUTHORITY-CHECK": "**AUTHORITY-CHECK OBJECT** *obj* **ID** *field* **FIELD** *value* ...\n\nCheck user authorization.",
    "SUBMIT": "**SUBMIT** *program* [**WITH** *param* **=** *value*] [**AND RETURN**].\n\nRun another program.",
    "CALL TRANSACTION": "**CALL TRANSACTION** *tcode* [**USING** *bdc_tab*] [**AND SKIP FIRST SCREEN**].\n\nCall a transaction.",
    "CALL TRANSFORMATION": "**CALL TRANSFORMATION** *id* **SOURCE** ... **RESULT** ...\n\nApply an XSLT/ST transformation.",
    "EXPORT": "**EXPORT** *data* **TO** **MEMORY**|**DATABASE** ...\n\nExport data to memory or database.",
    "IMPORT": "**IMPORT** *data* **FROM** **MEMORY**|**DATABASE** ...\n\nImport data from memory or database.",
    "SET PARAMETER": "**SET PARAMETER ID** *pid* **FIELD** *value*.\n\nSet SPA/GPA parameter.",
    "GET PARAMETER": "**GET PARAMETER ID** *pid* **FIELD** *var*.\n\nGet SPA/GPA parameter.",
    "ASSERT": "**ASSERT** *condition*.\n\nRuntime assertion (dumps if false).",
    "BREAK-POINT": "**BREAK-POINT.**\n\nSet a soft breakpoint for debugging.",
    "LOG-POINT": "**LOG-POINT ID** *checkpoint_group*.\n\nSet a log point for debugging.",
}

_PREDICATES = {
    "IS INITIAL": "**IS INITIAL**\n\nTest if a variable has its initial value.",
    "IS NOT INITIAL": "**IS NOT INITIAL**\n\nTest if a variable is not initial.",
    "IS ASSIGNED": "**IS ASSIGNED**\n\nTest if a field symbol is assigned.",
    "IS BOUND": "**IS BOUND**\n\nTest if a reference is bound (not null).",
    "IS INSTANCE OF": "**IS INSTANCE OF** *class*\n\nTest if an object is an instance of a class.",
    "IS SUPPLIED": "**IS SUPPLIED**\n\nTest if an optional parameter was supplied.",
    "IS REQUESTED": "**IS REQUESTED**\n\nTest if an exporting parameter is requested by the caller.",
    "BETWEEN": "**BETWEEN** *low* **AND** *high*\n\nRange check predicate.",
    "IN": "**IN** *range_tab*\n\nTest membership in a range table.",
}

_LOGICAL = {
    "AND": "**AND**\n\nLogical AND operator.",
    "OR": "**OR**\n\nLogical OR operator.",
    "NOT": "**NOT**\n\nLogical NOT operator.",
    "EQ": "**EQ** (or **=**)\n\nEqual comparison.",
    "NE": "**NE** (or **<>**)\n\nNot-equal comparison.",
    "LT": "**LT** (or **<**)\n\nLess-than comparison.",
    "GT": "**GT** (or **>**)\n\nGreater-than comparison.",
    "LE": "**LE** (or **<=**)\n\nLess-than-or-equal comparison.",
    "GE": "**GE** (or **>=**)\n\nGreater-than-or-equal comparison.",
    "CO": "**CO** (Contains Only)\n\nAll characters of left operand are in right operand.",
    "CN": "**CN** (Contains Not Only)\n\nNegation of CO.",
    "CA": "**CA** (Contains Any)\n\nLeft operand contains at least one character from right.",
    "NA": "**NA** (Contains Not Any)\n\nNegation of CA.",
    "CS": "**CS** (Contains String)\n\nLeft operand contains right operand as substring.",
    "NS": "**NS** (Contains No String)\n\nNegation of CS.",
    "CP": "**CP** (Covers Pattern)\n\nLeft operand matches pattern (* and + wildcards).",
    "NP": "**NP** (No Pattern)\n\nNegation of CP.",
}

# Built-in type documentation
_BUILTIN_TYPES = {
    "I": "**i** — Integer (4 bytes, -2,147,483,648 to 2,147,483,647)",
    "INT8": "**int8** — 8-byte integer",
    "F": "**f** — Floating point (8 bytes, IEEE 754)",
    "P": "**p** — Packed number (BCD, 1-16 bytes, up to 14 decimal places)",
    "C": "**c** — Character (fixed-length, 1-262143 characters)",
    "N": "**n** — Numeric text (fixed-length, digits only)",
    "D": "**d** — Date (8 characters, YYYYMMDD)",
    "T": "**t** — Time (6 characters, HHMMSS)",
    "X": "**x** — Hexadecimal (fixed-length byte sequence)",
    "STRING": "**string** — Variable-length character string",
    "XSTRING": "**xstring** — Variable-length byte string",
    "DECFLOAT16": "**decfloat16** — Decimal floating point (16 digits, IEEE 754r)",
    "DECFLOAT34": "**decfloat34** — Decimal floating point (34 digits, IEEE 754r)",
    "UTCLONG": "**utclong** — UTC timestamp with sub-second precision",
    "ANY": "**any** — Generic type (any data type)",
    "DATA": "**data** — Generic type (any data object)",
    "OBJECT": "**object** — Root class of all ABAP objects",
    "CLIKE": "**clike** — Generic type for character-like types (c, n, d, t, string)",
    "CSEQUENCE": "**csequence** — Generic type for c and string",
    "NUMERIC": "**numeric** — Generic type for numeric types (i, f, p, decfloat*)",
    "XSEQUENCE": "**xsequence** — Generic type for x and xstring",
    "SIMPLE": "**simple** — Generic type for elementary non-structured types",
}

# Merge all keyword categories into a single lookup
KEYWORD_INFO: dict[str, str] = {}
for _category in (
    _DECLARATIONS,
    _OOP,
    _CONTROL_FLOW,
    _EXCEPTIONS,
    _INTERNAL_TABLES,
    _STRING_OPS,
    _SQL,
    _SUBROUTINES,
    _ASSIGNMENTS,
    _CONSTRUCTOR_EXPRESSIONS,
    _EVENTS,
    _SCREEN,
    _MISC,
    _PREDICATES,
    _LOGICAL,
):
    KEYWORD_INFO.update(_category)


def get_hover(root_node: Node, line: int, column: int) -> Hover | None:
    """Get hover information for the node at the given position."""
    node = _node_at_position(root_node, line, column)
    if node is None:
        return Hover(
            contents=MarkupContent(kind=MarkupKind.Markdown, value=""),
        )

    text = node.text.decode()
    text_upper = text.upper()

    # Check keywords
    if text_upper in KEYWORD_INFO:
        return _make_hover(KEYWORD_INFO[text_upper], node)

    # Check built-in types
    if text_upper in _BUILTIN_TYPES:
        return _make_hover(_BUILTIN_TYPES[text_upper], node)

    # Field symbol
    if node.type == "field_symbol":
        decl_info = _find_declaration_info(root_node, text_upper)
        content = decl_info if decl_info else f"**Field Symbol** `{text}`"
        return _make_hover(content, node)

    # Identifier — show declaration if found
    if node.type == "identifier":
        decl_info = _find_declaration_info(root_node, text_upper)
        content = decl_info if decl_info else f"**Identifier** `{text}`"
        return _make_hover(content, node)

    return Hover(
        contents=MarkupContent(kind=MarkupKind.Markdown, value=""),
    )


def _find_declaration_info(root_node: Node, name_upper: str) -> str:
    """Search the tree for the declaration of a name and return a description."""
    result = _search_declaration(root_node, name_upper)
    return result


def _search_declaration(node: Node, name_upper: str) -> str:
    """Recursively search for a declaration matching name_upper."""
    declaration_map = {
        "data_statement": "DATA",
        "class_data_statement": "CLASS-DATA",
        "statics_statement": "STATICS",
        "constants_statement": "CONSTANTS",
        "types_statement": "TYPES",
        "field_symbols_statement": "FIELD-SYMBOLS",
        "parameters_statement": "PARAMETERS",
        "select_options_statement": "SELECT-OPTIONS",
        "method_implementation": "METHOD",
        "method_statement": "METHODS",
        "form_definition": "FORM",
        "function_implementation": "FUNCTION",
        "class_definition": "CLASS",
        "class_implementation": "CLASS",
        "interface_definition": "INTERFACE",
    }

    if node.type in declaration_map:
        declared_name = _get_declared_name(node)
        if declared_name and declared_name.upper() == name_upper:
            keyword = declaration_map[node.type]
            source_line = _first_line(node)
            return f"**{keyword}** declaration\n\n`{source_line}`"

    for child in node.children:
        result = _search_declaration(child, name_upper)
        if result:
            return result
    return ""


def _get_declared_name(node: Node) -> str:
    """Extract the declared name from a node."""
    if node.type == "field_symbols_statement":
        for child in node.children:
            if child.type == "field_symbol":
                return child.text.decode()

    for child in node.children:
        if child.type == "identifier":
            return child.text.decode()
    return ""


def _first_line(node: Node) -> str:
    """Get the first line of a node's text."""
    text = (node.text or b"").decode()
    first = text.split("\n")[0]
    return first[:120] + "..." if len(first) > 120 else first


def _node_at_position(root_node: Node, line: int, column: int) -> Node | None:
    """Find the most specific node at the given position."""
    cursor = root_node.walk()

    def _search() -> Node | None:
        node = cursor.node
        start = node.start_point
        end = node.end_point
        if line < start.row or line > end.row:
            return None
        if line == start.row and column < start.column:
            return None
        if line == end.row and column >= end.column:
            return None

        if cursor.goto_first_child():
            while True:
                result = _search()
                if result is not None:
                    return result
                if not cursor.goto_next_sibling():
                    break
            cursor.goto_parent()

        if node.type in ("identifier", "field_symbol"):
            return node
        if node.child_count == 0:
            return node
        return None

    return _search()


def _make_hover(content: str, node: Node) -> Hover:
    return Hover(
        contents=MarkupContent(kind=MarkupKind.Markdown, value=content),
        range=Range(
            start=Position(
                line=node.start_point.row, character=node.start_point.column
            ),
            end=Position(line=node.end_point.row, character=node.end_point.column),
        ),
    )
