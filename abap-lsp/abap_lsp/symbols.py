"""Extract hierarchical document symbols from the ABAP parse tree."""

from lsprotocol.types import DocumentSymbol, Position, Range, SymbolKind

from tree_sitter import Node

# Map ABAP node types to SymbolKind values
_SYMBOL_KIND_MAP = {
    "class_definition": SymbolKind.Class,
    "class_implementation": SymbolKind.Class,
    "interface_definition": SymbolKind.Interface,
    "method_implementation": SymbolKind.Method,
    "form_definition": SymbolKind.Function,
    "function_implementation": SymbolKind.Function,
    "module_implementation": SymbolKind.Function,
    "define_statement": SymbolKind.Function,
    "data_statement": SymbolKind.Variable,
    "class_data_statement": SymbolKind.Variable,
    "statics_statement": SymbolKind.Variable,
    "constants_statement": SymbolKind.Constant,
    "types_statement": SymbolKind.Struct,
    "field_symbols_statement": SymbolKind.Variable,
    "parameters_statement": SymbolKind.Property,
    "select_options_statement": SymbolKind.Property,
    "method_statement": SymbolKind.Method,
    "events_statement": SymbolKind.Event,
    "class_events_statement": SymbolKind.Event,
}

# Node types that can contain child symbols
_CONTAINER_TYPES = {
    "class_definition",
    "class_implementation",
    "interface_definition",
    "form_definition",
    "function_implementation",
    "module_implementation",
    "method_implementation",
    "visibility_section",
}


def get_document_symbols(root_node: Node) -> list[DocumentSymbol]:
    """Build a hierarchical list of document symbols from the parse tree."""
    return _extract_children_symbols(root_node)


def _extract_children_symbols(node: Node) -> list[DocumentSymbol]:
    """Extract symbols from the children of a node."""
    symbols: list[DocumentSymbol] = []
    for child in node.children:
        sym = _extract_symbol(child)
        if sym is not None:
            symbols.append(sym)
    return symbols


def _extract_symbol(node: Node) -> DocumentSymbol | None:
    """Extract a DocumentSymbol from a node, with children if it's a container."""
    if node.type == "visibility_section":
        return _extract_visibility_section(node)

    kind = _SYMBOL_KIND_MAP.get(node.type)
    if kind is None:
        return None

    name = _get_symbol_name(node)
    if not name:
        return None

    children = _extract_children_symbols(node) if node.type in _CONTAINER_TYPES else []

    r = _node_range(node)
    return DocumentSymbol(
        name=name,
        kind=kind,
        range=r,
        selection_range=r,
        children=children if children else None,
    )


def _extract_visibility_section(node: Node) -> DocumentSymbol | None:
    """Extract a visibility section (PUBLIC/PROTECTED/PRIVATE SECTION) as a namespace."""
    label = _get_visibility_label(node)
    children = _extract_children_symbols(node)
    if not children:
        return None

    r = _node_range(node)
    return DocumentSymbol(
        name=label,
        kind=SymbolKind.Namespace,
        range=r,
        selection_range=r,
        children=children,
    )


def _get_visibility_label(node: Node) -> str:
    """Determine the visibility label from the section's first child tokens."""
    for child in node.children:
        text = (child.text or b"").decode().upper()
        if text in ("PUBLIC", "PROTECTED", "PRIVATE"):
            return f"{text} SECTION"
    return "SECTION"


def _get_symbol_name(node: Node) -> str:
    """Extract the declared name from a node."""
    # For field_symbols_statement, look for field_symbol node
    if node.type == "field_symbols_statement":
        for child in node.children:
            if child.type == "field_symbol":
                return child.text.decode()

    # For most nodes, the first identifier child is the name
    for child in node.children:
        if child.type == "identifier":
            return child.text.decode()

    return ""


def _node_range(node: Node) -> Range:
    return Range(
        start=Position(line=node.start_point.row, character=node.start_point.column),
        end=Position(line=node.end_point.row, character=node.end_point.column),
    )
