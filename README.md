# tree-sitter-abap

A [Tree-sitter](https://tree-sitter.github.io/tree-sitter/) grammar for the ABAP programming language.

## Coverage

The grammar covers ABAP through Release 6.40 (based on "The Official ABAP Reference" by Horst Keller) plus modern 7.40+/7.50+ features.

### Supported constructs

| Category | Statements |
|---|---|
| **Program structure** | `REPORT`, `PROGRAM`, `FUNCTION-POOL`, `CLASS-POOL`, `INTERFACE-POOL`, `TYPE-POOL`, `INCLUDE` |
| **Declarations** | `DATA`, `TYPES`, `CONSTANTS`, `STATICS`, `CLASS-DATA`, `FIELD-SYMBOLS`, `PARAMETERS`, `SELECT-OPTIONS`, `TABLES`, `TYPE-POOLS` |
| **OOP** | `CLASS DEFINITION/IMPLEMENTATION`, `INTERFACE`, `METHOD/ENDMETHOD`, `METHODS`, `CLASS-METHODS`, visibility sections, `INTERFACES`, `ALIASES`, `EVENTS` |
| **Modularization** | `FORM/ENDFORM`, `FUNCTION/ENDFUNCTION`, `MODULE/ENDMODULE`, `DEFINE/END-OF-DEFINITION` |
| **Control flow** | `IF/ELSEIF/ELSE/ENDIF`, `CASE/WHEN/ENDCASE`, `CASE TYPE OF`, `DO/ENDDO`, `WHILE/ENDWHILE`, `LOOP AT/ENDLOOP` (with `AT FIRST/LAST/NEW/END OF`) |
| **Exception handling** | `TRY/CATCH/CLEANUP/ENDTRY`, `RAISE EXCEPTION`, `RAISE EVENT` |
| **Calling** | Method calls (`obj->method( )`), `CALL METHOD`, `CALL FUNCTION`, `PERFORM`, `SUBMIT`, `CALL TRANSACTION`, `CALL SCREEN` |
| **Exit** | `RETURN`, `EXIT`, `CHECK`, `CONTINUE`, `LEAVE` |
| **Assignments** | `=`, `+=`, `-=`, `MOVE`, `MOVE-CORRESPONDING`, `ASSIGN`, `UNASSIGN`, `CLEAR`, `FREE`, `GET REFERENCE OF` |
| **Internal tables** | `READ TABLE`, `INSERT`, `APPEND`, `MODIFY`, `DELETE`, `SORT`, `COLLECT`, `DESCRIBE TABLE` |
| **String operations** | `CONCATENATE`, `SPLIT`, `FIND`, `REPLACE`, `SEARCH`, `CONDENSE`, `TRANSLATE`, `SHIFT`, `OVERLAY` |
| **Open SQL** | `SELECT` (single, loop, joins, aggregates, `ORDER BY`, `GROUP BY`), `INSERT`, `UPDATE`, `MODIFY`, `DELETE`, cursor operations, `COMMIT WORK`, `ROLLBACK WORK` |
| **Dialogs** | `SELECTION-SCREEN`, `WRITE`, `ULINE`, `SKIP`, `FORMAT`, `SET PF-STATUS`, `SET TITLEBAR`, `MESSAGE` |
| **7.40+ expressions** | `NEW`, `VALUE`, `REF`, `CONV`, `CAST`, `EXACT`, `CORRESPONDING`, `COND`, `SWITCH`, `FILTER`, `REDUCE`, `FOR`, `LET`, inline `DATA( )` / `FIELD-SYMBOL( )`, table expressions `itab[ ]` |
| **Remaining** | `AUTHORITY-CHECK`, `SET/GET PARAMETER`, `CALL TRANSFORMATION`, `EXPORT/IMPORT`, `CREATE OBJECT`, `SET HANDLER`, `DESCRIBE FIELD`, event blocks |

### Expressions

Full expression support including arithmetic, comparison (symbolic and keyword forms), logical (`AND`, `OR`, `NOT`, `EQUIV`), bitwise, string concatenation (`&&`), component access (`->`, `=>`, `~`), and predicate expressions (`IS INITIAL/ASSIGNED/BOUND/SUPPLIED/INSTANCE OF`, `BETWEEN`).

### Comments

- `*` at column 1 for full-line comments (handled via external scanner)
- `"` for inline comments

### Chained statements

`DATA:`, `TYPES:`, `CONSTANTS:`, `WRITE:`, etc. with colon/comma syntax.

## Installation

```bash
npm install
npx tree-sitter generate
```

## Testing

```bash
npx tree-sitter test
```

229 tests across 42 test files covering all grammar constructs.

## Queries

The grammar ships with query files for editor integration:

- `queries/highlights.scm` — syntax highlighting
- `queries/locals.scm` — local variable scoping
- `queries/tags.scm` — code navigation (classes, methods, forms, functions)
- `queries/folds.scm` — code folding regions

## Project structure

```
grammar.js           # Main grammar definition
src/scanner.c        # External scanner for column-dependent * comments
queries/             # Tree-sitter query files
test/corpus/         # Test corpus (42 files, 229 tests)
test/sample_program.abap  # Integration test program
```

## Known limitations

- Structure component access with `-` (e.g., `struct-field`) is not yet supported due to ambiguity with the subtraction operator. Use `->` for instance access.
- Space-separated SQL field lists (old syntax) are not supported; use comma-separated lists.
- `=` is ambiguous between assignment and comparison; the parser favors comparison via conflict resolution.
- Interface method names with `~` (e.g., `zif_test~method`) in `METHOD` implementations are not yet supported.

## License

[MIT](LICENSE)
