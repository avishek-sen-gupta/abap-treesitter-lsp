#include "tree_sitter/parser.h"

/**
 * External scanner for ABAP tree-sitter grammar.
 *
 * Handles tokens that require context not available in the grammar:
 * - LINE_COMMENT: * at column 0 (start of line) followed by rest of line
 */

enum TokenType {
  LINE_COMMENT,
};

void *tree_sitter_abap_external_scanner_create(void) {
  return NULL;
}

void tree_sitter_abap_external_scanner_destroy(void *payload) {
}

unsigned tree_sitter_abap_external_scanner_serialize(void *payload, char *buffer) {
  return 0;
}

void tree_sitter_abap_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
}

bool tree_sitter_abap_external_scanner_scan(
  void *payload,
  TSLexer *lexer,
  const bool *valid_symbols
) {
  if (!valid_symbols[LINE_COMMENT]) {
    return false;
  }

  // Skip any whitespace (spaces, tabs, newlines) to find the next real character.
  // We need to check if a `*` appears at column 0 (start of a line).
  while (lexer->lookahead == ' ' || lexer->lookahead == '\t' ||
         lexer->lookahead == '\n' || lexer->lookahead == '\r') {
    lexer->advance(lexer, true);  // true = skip (don't include in token)
  }

  // After skipping whitespace, check if we have * at column 0
  if (lexer->get_column(lexer) != 0 || lexer->lookahead != '*') {
    return false;
  }

  // Mark the start of the token
  lexer->result_symbol = LINE_COMMENT;

  // Consume the * and the rest of the line
  lexer->advance(lexer, false);
  while (lexer->lookahead != '\n' && lexer->lookahead != '\0' && !lexer->eof(lexer)) {
    lexer->advance(lexer, false);
  }

  return true;
}
