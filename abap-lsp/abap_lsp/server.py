"""ABAP Language Server using pygls."""

from lsprotocol.types import (
    TEXT_DOCUMENT_COMPLETION,
    TEXT_DOCUMENT_DEFINITION,
    TEXT_DOCUMENT_DID_CHANGE,
    TEXT_DOCUMENT_DID_CLOSE,
    TEXT_DOCUMENT_DID_OPEN,
    TEXT_DOCUMENT_DOCUMENT_SYMBOL,
    TEXT_DOCUMENT_HOVER,
    TEXT_DOCUMENT_REFERENCES,
    CompletionList,
    CompletionOptions,
    CompletionParams,
    DefinitionParams,
    DidChangeTextDocumentParams,
    DidCloseTextDocumentParams,
    DidOpenTextDocumentParams,
    DocumentSymbolParams,
    Hover,
    HoverParams,
    Location,
    ReferenceParams,
    TextDocumentSyncKind,
)

from pygls.lsp.server import LanguageServer

from abap_lsp import completion as comp_mod
from abap_lsp import definition as def_mod
from abap_lsp import diagnostics as diag_mod
from abap_lsp import hover as hover_mod
from abap_lsp import parser as parser_mod
from abap_lsp import references as ref_mod
from abap_lsp import symbols as sym_mod

server = LanguageServer("abap-lsp", "v0.1.0")


def _publish_diagnostics(ls: LanguageServer, uri: str):
    tree = parser_mod.get_tree(uri)
    if tree is None:
        return
    diags = diag_mod.get_diagnostics(tree.root_node)
    ls.publish_diagnostics(uri, diags)


@server.feature(TEXT_DOCUMENT_DID_OPEN)
def did_open(ls: LanguageServer, params: DidOpenTextDocumentParams):
    uri = params.text_document.uri
    source = params.text_document.text.encode("utf-8")
    parser_mod.parse(uri, source)
    _publish_diagnostics(ls, uri)


@server.feature(TEXT_DOCUMENT_DID_CHANGE)
def did_change(ls: LanguageServer, params: DidChangeTextDocumentParams):
    uri = params.text_document.uri
    if params.content_changes:
        source = params.content_changes[-1].text.encode("utf-8")
        parser_mod.parse(uri, source)
        _publish_diagnostics(ls, uri)


@server.feature(TEXT_DOCUMENT_DID_CLOSE)
def did_close(ls: LanguageServer, params: DidCloseTextDocumentParams):
    uri = params.text_document.uri
    parser_mod.remove_tree(uri)


@server.feature(TEXT_DOCUMENT_DOCUMENT_SYMBOL)
def document_symbol(ls: LanguageServer, params: DocumentSymbolParams):
    tree = parser_mod.get_tree(params.text_document.uri)
    if tree is None:
        return []
    return sym_mod.get_document_symbols(tree.root_node)


@server.feature(TEXT_DOCUMENT_DEFINITION)
def definition(ls: LanguageServer, params: DefinitionParams) -> Location | None:
    tree = parser_mod.get_tree(params.text_document.uri)
    if tree is None:
        return None
    return def_mod.find_definition(
        tree.root_node,
        params.text_document.uri,
        params.position.line,
        params.position.character,
    )


@server.feature(TEXT_DOCUMENT_REFERENCES)
def references(ls: LanguageServer, params: ReferenceParams) -> list[Location]:
    tree = parser_mod.get_tree(params.text_document.uri)
    if tree is None:
        return []
    return ref_mod.find_references(
        tree.root_node,
        params.text_document.uri,
        params.position.line,
        params.position.character,
        include_declaration=params.context.include_declaration,
    )


@server.feature(TEXT_DOCUMENT_HOVER)
def hover(ls: LanguageServer, params: HoverParams) -> Hover | None:
    tree = parser_mod.get_tree(params.text_document.uri)
    if tree is None:
        return None
    return hover_mod.get_hover(
        tree.root_node,
        params.position.line,
        params.position.character,
    )


@server.feature(
    TEXT_DOCUMENT_COMPLETION,
    CompletionOptions(trigger_characters=[".", "-", ">"]),
)
def completions(ls: LanguageServer, params: CompletionParams) -> CompletionList:
    tree = parser_mod.get_tree(params.text_document.uri)
    root = tree.root_node if tree else None
    return comp_mod.get_completions(root_node=root)
