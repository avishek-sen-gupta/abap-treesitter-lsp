"""Tree-sitter parsing wrapper for ABAP."""

import ctypes
import subprocess
import sys
from pathlib import Path

from tree_sitter import Language, Parser

_language = None
_parser = None
_trees: dict[str, "Tree"] = {}

GRAMMAR_DIR = Path(__file__).resolve().parent.parent.parent
LIB_PATH = Path(__file__).resolve().parent / "abap.so"


def _build_library() -> Path:
    """Compile the tree-sitter-abap shared library from source.

    ABAP has an external scanner (scanner.c) that must be compiled
    alongside parser.c into the shared library.
    """
    src_dir = GRAMMAR_DIR / "src"
    parser_c = src_dir / "parser.c"
    scanner_c = src_dir / "scanner.c"
    if not parser_c.exists():
        raise FileNotFoundError(f"Cannot find parser.c at {parser_c}")
    if not scanner_c.exists():
        raise FileNotFoundError(f"Cannot find scanner.c at {scanner_c}")

    if sys.platform == "darwin":
        shared_flag = "-dynamiclib"
    else:
        shared_flag = "-shared"

    subprocess.check_call(
        [
            "cc",
            shared_flag,
            "-fPIC",
            "-O2",
            "-I",
            str(src_dir),
            "-o",
            str(LIB_PATH),
            str(parser_c),
            str(scanner_c),
        ],
        stderr=subprocess.PIPE,
    )
    return LIB_PATH


def get_language() -> Language:
    """Get the ABAP tree-sitter Language object."""
    global _language
    if _language is not None:
        return _language

    if not LIB_PATH.exists():
        _build_library()

    lib = ctypes.cdll.LoadLibrary(str(LIB_PATH))
    func = lib.tree_sitter_abap
    func.restype = ctypes.c_void_p
    lang_ptr = func()
    _language = Language(lang_ptr)
    return _language


def get_parser() -> Parser:
    """Get a configured Parser instance."""
    global _parser
    if _parser is not None:
        return _parser
    _parser = Parser(get_language())
    return _parser


def parse(uri: str, source: bytes) -> "Tree":
    """Parse source and cache the tree for the given document URI."""
    parser = get_parser()
    old_tree = _trees.get(uri)
    if old_tree is not None:
        tree = parser.parse(source, old_tree)
    else:
        tree = parser.parse(source)
    _trees[uri] = tree
    return tree


def get_tree(uri: str):
    """Get the cached parse tree for a document URI."""
    return _trees.get(uri)


def remove_tree(uri: str):
    """Remove the cached tree when a document is closed."""
    _trees.pop(uri, None)
