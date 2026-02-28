"""Completion provider for ABAP keywords, types, and document symbols."""

from lsprotocol.types import (
    CompletionItem,
    CompletionItemKind,
    CompletionList,
    InsertTextFormat,
)

from tree_sitter import Node

# ABAP keywords organized by category for completion
_DECLARATION_KEYWORDS = [
    "DATA",
    "CLASS-DATA",
    "TYPES",
    "CONSTANTS",
    "STATICS",
    "FIELD-SYMBOLS",
    "PARAMETERS",
    "SELECT-OPTIONS",
    "TABLES",
    "TYPE-POOLS",
    "INCLUDE",
]

_OOP_KEYWORDS = [
    "CLASS",
    "ENDCLASS",
    "INTERFACE",
    "ENDINTERFACE",
    "METHOD",
    "ENDMETHOD",
    "METHODS",
    "CLASS-METHODS",
    "INTERFACES",
    "ALIASES",
    "EVENTS",
    "CLASS-EVENTS",
    "CREATE OBJECT",
    "INHERITING FROM",
    "PUBLIC",
    "PROTECTED",
    "PRIVATE",
    "ABSTRACT",
    "FINAL",
    "REDEFINITION",
    "DEFINITION",
    "IMPLEMENTATION",
    "FRIENDS",
]

_CONTROL_KEYWORDS = [
    "IF",
    "ELSEIF",
    "ELSE",
    "ENDIF",
    "CASE",
    "WHEN",
    "ENDCASE",
    "DO",
    "ENDDO",
    "WHILE",
    "ENDWHILE",
    "LOOP",
    "ENDLOOP",
    "EXIT",
    "CONTINUE",
    "CHECK",
    "RETURN",
    "STOP",
    "LEAVE",
]

_EXCEPTION_KEYWORDS = [
    "TRY",
    "CATCH",
    "CLEANUP",
    "ENDTRY",
    "RAISE",
    "RAISE EXCEPTION",
    "RETRY",
    "RESUME",
]

_TABLE_KEYWORDS = [
    "APPEND",
    "INSERT",
    "MODIFY",
    "DELETE",
    "READ TABLE",
    "SORT",
    "COLLECT",
    "CLEAR",
    "FREE",
    "REFRESH",
    "DESCRIBE TABLE",
    "ASSIGN",
    "UNASSIGN",
]

_STRING_KEYWORDS = [
    "CONCATENATE",
    "SPLIT",
    "FIND",
    "REPLACE",
    "CONDENSE",
    "TRANSLATE",
    "SHIFT",
    "OVERLAY",
]

_SQL_KEYWORDS = [
    "SELECT",
    "ENDSELECT",
    "UPDATE",
    "INSERT INTO",
    "DELETE FROM",
    "OPEN CURSOR",
    "FETCH",
    "CLOSE CURSOR",
    "COMMIT WORK",
    "ROLLBACK WORK",
]

_SUBROUTINE_KEYWORDS = [
    "FORM",
    "ENDFORM",
    "PERFORM",
    "FUNCTION",
    "ENDFUNCTION",
    "CALL FUNCTION",
    "MODULE",
    "ENDMODULE",
    "DEFINE",
    "END-OF-DEFINITION",
]

_CONSTRUCTOR_KEYWORDS = [
    "NEW",
    "VALUE",
    "REF",
    "CONV",
    "CAST",
    "EXACT",
    "CORRESPONDING",
    "COND",
    "SWITCH",
    "FILTER",
    "REDUCE",
    "LET",
]

_MISC_KEYWORDS = [
    "REPORT",
    "PROGRAM",
    "FUNCTION-POOL",
    "MESSAGE",
    "AUTHORITY-CHECK",
    "SUBMIT",
    "CALL TRANSACTION",
    "EXPORT",
    "IMPORT",
    "WRITE",
    "ULINE",
    "SKIP",
    "NEW-LINE",
    "FORMAT",
    "SET PF-STATUS",
    "SET TITLEBAR",
    "MOVE",
    "MOVE-CORRESPONDING",
    "COMPUTE",
    "ADD",
    "SUBTRACT",
    "MULTIPLY",
    "DIVIDE",
    "ASSERT",
    "BREAK-POINT",
    "INITIALIZATION",
    "START-OF-SELECTION",
    "END-OF-SELECTION",
    "TOP-OF-PAGE",
    "AT LINE-SELECTION",
    "AT USER-COMMAND",
    "SET HANDLER",
    "RAISE EVENT",
    "CALL SCREEN",
    "SET SCREEN",
    "LEAVE SCREEN",
    "SELECTION-SCREEN",
    "IMPORTING",
    "EXPORTING",
    "CHANGING",
    "RETURNING",
    "USING",
    "EXCEPTIONS",
    "INTO",
    "FROM",
    "WHERE",
    "TYPE",
    "TYPE TABLE OF",
    "TYPE REF TO",
    "LIKE",
    "LIKE TABLE OF",
    "LIKE REF TO",
    "BEGIN OF",
    "END OF",
    "ASSIGNING",
    "REFERENCE INTO",
    "ASCENDING",
    "DESCENDING",
    "AND",
    "OR",
    "NOT",
    "IS INITIAL",
    "IS NOT INITIAL",
    "IS ASSIGNED",
    "IS BOUND",
    "IS INSTANCE OF",
    "BETWEEN",
    "IN",
]

ALL_KEYWORDS = sorted(
    set(
        _DECLARATION_KEYWORDS
        + _OOP_KEYWORDS
        + _CONTROL_KEYWORDS
        + _EXCEPTION_KEYWORDS
        + _TABLE_KEYWORDS
        + _STRING_KEYWORDS
        + _SQL_KEYWORDS
        + _SUBROUTINE_KEYWORDS
        + _CONSTRUCTOR_KEYWORDS
        + _MISC_KEYWORDS
    )
)

# Built-in ABAP types
BUILTIN_TYPES = [
    "i",
    "int8",
    "f",
    "p",
    "c",
    "n",
    "d",
    "t",
    "x",
    "string",
    "xstring",
    "decfloat16",
    "decfloat34",
    "utclong",
    "any",
    "data",
    "object",
    "clike",
    "csequence",
    "numeric",
    "xsequence",
    "simple",
    "abap_bool",
    "abap_true",
    "abap_false",
    "sy",
    "syst",
]

# Snippet completions for common patterns
_SNIPPETS = [
    ("IF...ENDIF", "IF ${1:condition}.\n  ${0}\nENDIF.", "IF block"),
    (
        "LOOP...ENDLOOP",
        "LOOP AT ${1:itab} INTO ${2:DATA(wa)}.\n  ${0}\nENDLOOP.",
        "LOOP block",
    ),
    ("DO...ENDDO", "DO ${1:n} TIMES.\n  ${0}\nENDDO.", "DO block"),
    ("WHILE...ENDWHILE", "WHILE ${1:condition}.\n  ${0}\nENDWHILE.", "WHILE block"),
    (
        "TRY...ENDTRY",
        "TRY.\n    ${0}\n  CATCH ${1:cx_root} INTO DATA(${2:lx_error}).\nENDTRY.",
        "TRY/CATCH block",
    ),
    (
        "CASE...ENDCASE",
        "CASE ${1:variable}.\n  WHEN ${2:value}.\n    ${0}\n  WHEN OTHERS.\nENDCASE.",
        "CASE block",
    ),
    (
        "CLASS...ENDCLASS",
        "CLASS ${1:lcl_class} DEFINITION.\n  PUBLIC SECTION.\n    ${0}\nENDCLASS.\n\nCLASS $1 IMPLEMENTATION.\nENDCLASS.",
        "Class definition + implementation",
    ),
    (
        "METHOD...ENDMETHOD",
        "METHOD ${1:method_name}.\n  ${0}\nENDMETHOD.",
        "Method implementation",
    ),
    (
        "SELECT...ENDSELECT",
        "SELECT ${1:*}\n  FROM ${2:dbtab}\n  INTO ${3:TABLE @DATA(lt_result)}\n  WHERE ${0:condition}.",
        "SELECT statement",
    ),
    (
        "FORM...ENDFORM",
        "FORM ${1:form_name} ${2:USING p1}.\n  ${0}\nENDFORM.",
        "Form subroutine",
    ),
]


def get_completions(root_node: Node = None) -> CompletionList:
    """Return completion items for keywords, types, snippets, and document variables."""
    items: list[CompletionItem] = []

    # Keywords
    items.extend(
        CompletionItem(label=kw, kind=CompletionItemKind.Keyword) for kw in ALL_KEYWORDS
    )

    # Built-in types
    items.extend(
        CompletionItem(
            label=t, kind=CompletionItemKind.TypeParameter, detail="Built-in type"
        )
        for t in BUILTIN_TYPES
    )

    # Snippets
    items.extend(
        CompletionItem(
            label=label,
            kind=CompletionItemKind.Snippet,
            insert_text=body,
            insert_text_format=InsertTextFormat.Snippet,
            detail=description,
        )
        for label, body, description in _SNIPPETS
    )

    # Document-local identifiers
    if root_node is not None:
        keyword_set = {kw.upper() for kw in ALL_KEYWORDS}
        seen: set[str] = set()
        _collect_identifiers(root_node, seen, keyword_set)
        items.extend(
            CompletionItem(label=name, kind=CompletionItemKind.Variable)
            for name in sorted(seen)
        )

    return CompletionList(is_incomplete=False, items=items)


def _collect_identifiers(node: Node, seen: set[str], keyword_set: set[str]):
    """Collect unique identifiers from the tree, excluding keywords."""
    if node.type in ("identifier", "field_symbol"):
        name = node.text.decode()
        if name.upper() not in keyword_set:
            seen.add(name)

    for child in node.children:
        _collect_identifiers(child, seen, keyword_set)
