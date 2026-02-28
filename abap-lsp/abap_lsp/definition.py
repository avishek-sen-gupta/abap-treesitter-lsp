"""Go-to-definition logic for ABAP."""

from lsprotocol.types import Location, Position, Range

from tree_sitter import Node

# Node types that declare variables/data
_DATA_DECLARATION_TYPES = {
    "data_statement",
    "class_data_statement",
    "statics_statement",
    "constants_statement",
    "field_symbols_statement",
    "parameters_statement",
    "select_options_statement",
    "tables_statement",
}

# Node types that declare types
_TYPE_DECLARATION_TYPES = {
    "types_statement",
}

# Node types that declare callable units
_CALLABLE_DECLARATION_TYPES = {
    "method_implementation",
    "form_definition",
    "function_implementation",
    "module_implementation",
    "define_statement",
}

# Node types that declare classes/interfaces
_CLASS_DECLARATION_TYPES = {
    "class_definition",
    "class_implementation",
    "interface_definition",
}


def find_definition(
    root_node: Node, uri: str, line: int, column: int
) -> Location | None:
    """Find the definition of the symbol at the given position."""
    target_node = _node_at_position(root_node, line, column)
    if target_node is None:
        return Location(
            uri=uri,
            range=Range(
                start=Position(line=0, character=0),
                end=Position(line=0, character=0),
            ),
        )

    name = target_node.text.decode()
    name_upper = name.upper()

    # Try each declaration category in order
    result = _find_in_declarations(root_node, name_upper, uri, _CLASS_DECLARATION_TYPES)
    if result:
        return result

    result = _find_in_declarations(
        root_node, name_upper, uri, _CALLABLE_DECLARATION_TYPES
    )
    if result:
        return result

    result = _find_in_declarations(root_node, name_upper, uri, _TYPE_DECLARATION_TYPES)
    if result:
        return result

    result = _find_in_declarations(root_node, name_upper, uri, _DATA_DECLARATION_TYPES)
    if result:
        return result

    # Search method declarations inside class definitions
    result = _find_method_declaration(root_node, name_upper, uri)
    if result:
        return result

    # Search inline declarations: DATA(var) or FIELD-SYMBOL(<fs>)
    result = _find_inline_declaration(root_node, name_upper, uri)
    if result:
        return result

    return Location(
        uri=uri,
        range=Range(
            start=Position(line=0, character=0),
            end=Position(line=0, character=0),
        ),
    )


def _find_in_declarations(
    node: Node, name_upper: str, uri: str, declaration_types: set[str]
) -> Location | None:
    """Recursively search for a declaration matching name_upper in the given node types."""
    if node.type in declaration_types:
        declared_name = _get_declared_name(node)
        if declared_name and declared_name.upper() == name_upper:
            return _make_location(uri, node)

    for child in node.children:
        result = _find_in_declarations(child, name_upper, uri, declaration_types)
        if result:
            return result
    return None


def _find_method_declaration(node: Node, name_upper: str, uri: str) -> Location | None:
    """Find a METHODS/CLASS-METHODS declaration inside a class definition."""
    if node.type == "method_statement":
        declared_name = _get_declared_name(node)
        if declared_name and declared_name.upper() == name_upper:
            return _make_location(uri, node)

    for child in node.children:
        result = _find_method_declaration(child, name_upper, uri)
        if result:
            return result
    return None


def _find_inline_declaration(node: Node, name_upper: str, uri: str) -> Location | None:
    """Find inline DATA(var) or FIELD-SYMBOL(<fs>) declarations."""
    if node.type == "inline_data_declaration":
        for child in node.children:
            if child.type == "identifier" and child.text.decode().upper() == name_upper:
                return _make_location(uri, node)

    if node.type == "inline_field_symbol":
        for child in node.children:
            if (
                child.type == "field_symbol"
                and child.text.decode().upper() == name_upper
            ):
                return _make_location(uri, node)

    for child in node.children:
        result = _find_inline_declaration(child, name_upper, uri)
        if result:
            return result
    return None


def _get_declared_name(node: Node) -> str:
    """Extract the declared identifier name from a declaration node."""
    if node.type == "field_symbols_statement":
        for child in node.children:
            if child.type == "field_symbol":
                return child.text.decode()

    for child in node.children:
        if child.type == "identifier":
            return child.text.decode()
    return ""


def _node_at_position(root_node: Node, line: int, column: int) -> Node | None:
    """Find the most specific node at the given position."""
    cursor = root_node.walk()

    def _search() -> Node | None:
        node = cursor.node
        if not _contains_position(node, line, column):
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


def _contains_position(node: Node, line: int, column: int) -> bool:
    start = node.start_point
    end = node.end_point
    if line < start.row or line > end.row:
        return False
    if line == start.row and column < start.column:
        return False
    if line == end.row and column >= end.column:
        return False
    return True


def _make_location(uri: str, node: Node) -> Location:
    return Location(
        uri=uri,
        range=Range(
            start=Position(
                line=node.start_point.row, character=node.start_point.column
            ),
            end=Position(line=node.end_point.row, character=node.end_point.column),
        ),
    )
