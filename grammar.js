/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

/**
 * Tree-sitter grammar for the ABAP programming language.
 * Case-insensitive keyword helper.
 */
function ci(word) {
  return new RegExp(
    word
      .split("")
      .map((c) => {
        if (/[a-zA-Z]/.test(c)) {
          return `[${c.toLowerCase()}${c.toUpperCase()}]`;
        }
        return c.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
      })
      .join(""),
  );
}

// Keyword precedence: longer keywords get higher precedence to prevent
// prefix matching (e.g., "IN" matching the start of "INTO")
function kw(word) {
  return alias(token(prec(word.length, ci(word))), word);
}

function kwSeq(...words) {
  const label = words.join(" ");
  const totalLen = words.reduce((sum, w) => sum + w.length, 0);
  return alias(
    token(prec(totalLen, seq(ci(words[0]), ...words.slice(1).map((w) => seq(/\s+/, ci(w)))))),
    label,
  );
}

const PREC = {
  OR: 1,
  AND: 2,
  EQUIV: 3,
  NOT: 4,
  COMPARE: 5,
  BIT_OR: 6,
  BIT_AND: 7,
  BIT_NOT: 8,
  CONCAT: 9,
  ADD: 10,
  MUL: 11,
  POWER: 12,
  UNARY: 13,
  MEMBER: 14,
  CALL: 15,
};

module.exports = grammar({
  name: "abap",

  externals: ($) => [$.line_comment],

  extras: ($) => [/\s/, $.comment, $.line_comment],

  word: ($) => $._identifier_chars,

  conflicts: ($) => [
    [$._expression, $.dynamic_reference],
    [$._expression, $.named_argument],
    [$._comparison_operator, $.assignment_statement],
    [$._expression, $.table_expression],
    [$.method_param],
    [$._expression, $._type_expression],
    [$._expression, $._read_table_addition],
    [$._table_modify_addition, $.delete_statement],
    [$.parenthesized_expression, $.write_statement],
    [$.parenthesized_expression, $._constructor_body],
    [$._block_statement, $.class_implementation],
    [$._comparison_operator, $.update_db_statement],
    [$.parenthesized_expression, $.uline_statement],
    [$.select_statement, $.select_loop_statement],
    [$.message_statement],
    [$._method_additions],
    [$.visibility_section],
    [$.parenthesized_expression, $._sql_target],
    [$._table_key],
  ],

  rules: {
    source_file: ($) => repeat($._statement),

    _statement: ($) =>
      choice(
        // Block constructs (no period needed for the wrapper)
        $._block_statement,
        // Regular period-terminated statements
        seq($._statement_body, "."),
        // Chained statements: KEYWORD: item1, item2.
        $.chained_statement,
      ),

    _statement_body: ($) =>
      choice(
        // Phase 2: Program headers & declarations
        $.report_statement,
        $.program_statement,
        $.function_pool_statement,
        $.class_pool_statement,
        $.interface_pool_statement,
        $.type_pool_statement,
        $.include_statement,
        $.data_statement,
        $.types_statement,
        $.constants_statement,
        $.statics_statement,
        $.class_data_statement,
        $.field_symbols_statement,
        $.parameters_statement,
        $.select_options_statement,
        $.tables_statement,
        $.type_pools_statement,
        $.include_type_statement,
        // Phase 3: Modularization
        $.method_statement,
        $.interface_def_statement,
        $.aliases_statement,
        $.events_statement,
        $.class_events_statement,
        // Phase 4: Calling & flow
        $.call_method_statement,
        $.call_function_statement,
        $.perform_statement,
        $.submit_statement,
        $.call_transaction_statement,
        $.call_screen_statement,
        $.leave_statement,
        $.return_statement,
        $.exit_statement,
        $.check_statement,
        $.continue_statement,
        $.raise_statement,
        $.raise_event_statement,
        // Phase 5: Assignments
        $.assignment_statement,
        $.move_statement,
        $.move_corresponding_statement,
        $.assign_statement,
        $.unassign_statement,
        $.clear_statement,
        $.free_statement,
        $.get_reference_statement,
        // Phase 5: Internal table operations
        $.read_table_statement,
        $.insert_statement,
        $.append_statement,
        $.modify_table_statement,
        $.delete_statement,
        $.sort_statement,
        $.collect_statement,
        $.describe_table_statement,
        // Phase 5: String operations
        $.concatenate_statement,
        $.split_statement,
        $.find_statement,
        $.replace_statement,
        $.search_statement,
        $.condense_statement,
        $.translate_statement,
        $.shift_statement,
        $.overlay_statement,
        // Phase 6: Dialogs & SQL
        $.selection_screen_statement,
        $.write_statement,
        $.uline_statement,
        $.skip_statement,
        $.new_line_statement,
        $.new_page_statement,
        $.format_statement,
        $.set_pf_status_statement,
        $.set_titlebar_statement,
        $.message_statement,
        $.select_statement,
        $.insert_db_statement,
        $.update_db_statement,
        $.modify_db_statement,
        $.delete_db_statement,
        $.open_cursor_statement,
        $.fetch_cursor_statement,
        $.close_cursor_statement,
        $.commit_statement,
        $.rollback_statement,
        // Phase 7: Remaining
        $.authority_check_statement,
        $.set_parameter_statement,
        $.get_parameter_statement,
        $.call_transformation_statement,
        $.export_statement,
        $.import_statement,
        $.create_object_statement,
        $.set_handler_statement,
        $.describe_field_statement,
        $.set_screen_statement,
        // Event block markers
        $.initialization_statement,
        $.start_of_selection_statement,
        $.end_of_selection_statement,
        $.top_of_page_statement,
        $.end_of_page_statement,
        $.at_line_selection_statement,
        $.at_user_command_statement,
        $.at_selection_screen_statement,
        // Expressions as statements (method calls, etc.)
        $._expression,
      ),

    // =========================================================================
    // Chained statements
    // =========================================================================

    chained_statement: ($) =>
      seq(
        $._chain_head,
        ":",
        commaSep1($._chain_body),
        ".",
      ),

    _chain_head: ($) =>
      choice(
        kw("DATA"),
        kw("TYPES"),
        kw("CONSTANTS"),
        kw("STATICS"),
        kwSeq("CLASS-DATA"),
        kwSeq("FIELD-SYMBOLS"),
        kw("METHODS"),
        kwSeq("CLASS-METHODS"),
        kw("INTERFACES"),
        kw("ALIASES"),
        kw("EVENTS"),
        kwSeq("CLASS-EVENTS"),
        kw("INCLUDE"),
        kw("WRITE"),
        kw("TABLES"),
      ),

    _chain_body: ($) =>
      repeat1(choice($._expression, $._type_spec, $._data_additions)),

    // =========================================================================
    // Block statements (compound)
    // =========================================================================

    _block_statement: ($) =>
      choice(
        $.if_statement,
        $.case_statement,
        $.case_type_statement,
        $.do_statement,
        $.while_statement,
        $.loop_statement,
        $.try_statement,
        $.class_definition,
        $.class_implementation,
        $.interface_definition,
        $.method_implementation,
        $.form_definition,
        $.function_implementation,
        $.module_implementation,
        $.define_statement,
        $.select_loop_statement,
        $.provide_block,
        $.on_change_block,
      ),

    _statement_block: ($) => repeat1($._statement),

    // =========================================================================
    // Comments
    // =========================================================================

    comment: (_$) => token(seq('"', /.*/)),

    // =========================================================================
    // Identifiers
    // =========================================================================

    _identifier_chars: (_$) => /[a-zA-Z_][a-zA-Z0-9_]*/,

    identifier: ($) =>
      choice($._identifier_chars, $.namespace_identifier),

    namespace_identifier: (_$) => /\/[a-zA-Z0-9_]+\/[a-zA-Z0-9_]+/,

    field_symbol: (_$) => /<[a-zA-Z_][a-zA-Z0-9_]*>/,

    // =========================================================================
    // Literals
    // =========================================================================

    integer_literal: (_$) => /\d+/,
    number_literal: ($) => choice($.integer_literal, $.float_literal),

    float_literal: (_$) =>
      token(
        choice(
          seq(/\d+/, ".", /\d+/),
          seq(/\d+/, /[eE]/, optional(/[+-]/), /\d+/),
          seq(/\d+/, ".", /\d+/, /[eE]/, optional(/[+-]/), /\d+/),
        ),
      ),

    string_literal: (_$) =>
      token(seq("'", repeat(choice(/[^'\n]/, "''")), "'")),

    hex_literal: (_$) => /[xX]'[0-9a-fA-F]*'/,

    _literal: ($) =>
      choice($.number_literal, $.string_literal, $.hex_literal),

    // =========================================================================
    // Expressions
    // =========================================================================

    _expression: ($) =>
      choice(
        $._literal,
        $.identifier,
        $.field_symbol,
        $.parenthesized_expression,
        $.unary_expression,
        $.binary_expression,
        $.component_access,
        $.method_call_expression,
        $.dynamic_reference,
        $.table_expression,
        $.new_expression,
        $.value_expression,
        $.ref_expression,
        $.conv_expression,
        $.cast_expression,
        $.exact_expression,
        $.corresponding_expression,
        $.cond_expression,
        $.switch_expression,
        $.filter_expression,
        $.reduce_expression,
        $.inline_data_declaration,
        $.inline_field_symbol,
        $.predicate_expression,
      ),

    parenthesized_expression: ($) => seq("(", $._expression, ")"),

    unary_expression: ($) =>
      choice(
        prec(PREC.UNARY, seq("-", $._expression)),
        prec(PREC.NOT, seq(kw("NOT"), $._expression)),
        prec(PREC.BIT_NOT, seq(kw("BIT-NOT"), $._expression)),
      ),

    binary_expression: ($) =>
      choice(
        prec.right(PREC.POWER, seq($._expression, "**", $._expression)),
        prec.left(PREC.MUL, seq($._expression, "*", $._expression)),
        prec.left(PREC.MUL, seq($._expression, "/", $._expression)),
        prec.left(PREC.MUL, seq($._expression, kw("MOD"), $._expression)),
        prec.left(PREC.MUL, seq($._expression, kw("DIV"), $._expression)),
        prec.left(PREC.ADD, seq($._expression, "+", $._expression)),
        prec.left(PREC.ADD, seq($._expression, "-", $._expression)),
        prec.left(PREC.CONCAT, seq($._expression, "&&", $._expression)),
        prec.left(PREC.BIT_AND, seq($._expression, kw("BIT-AND"), $._expression)),
        prec.left(PREC.BIT_OR, seq($._expression, kw("BIT-OR"), $._expression)),
        prec.left(PREC.BIT_OR, seq($._expression, kw("BIT-XOR"), $._expression)),
        prec.left(PREC.COMPARE, seq($._expression, $._comparison_operator, $._expression)),
        prec.left(PREC.AND, seq($._expression, kw("AND"), $._expression)),
        prec.left(PREC.OR, seq($._expression, kw("OR"), $._expression)),
        prec.left(PREC.EQUIV, seq($._expression, kw("EQUIV"), $._expression)),
      ),

    _comparison_operator: (_$) =>
      choice(
        "=", "<>", "<", ">", "<=", ">=",
        kw("EQ"), kw("NE"), kw("LT"), kw("GT"), kw("LE"), kw("GE"),
        kw("CO"), kw("CN"), kw("CA"), kw("CS"),
        kw("NA"), kw("NS"), kw("CP"), kw("NP"),
      ),

    component_access: ($) =>
      prec.left(PREC.MEMBER, seq(
        $._expression,
        choice("->", "=>", "~"),
        choice($.identifier, $.dynamic_reference),
      )),

    method_call_expression: ($) =>
      prec(PREC.CALL, seq(
        choice($.identifier, $.field_symbol, $.component_access),
        $.argument_list,
      )),

    argument_list: ($) =>
      seq("(", optional($._argument_list_body), ")"),

    _argument_list_body: ($) =>
      choice($._expression, commaSep1($.named_argument)),

    named_argument: ($) => seq($.identifier, "=", $._expression),

    dynamic_reference: ($) => seq("(", $.identifier, ")"),

    // Table expression: itab[ idx ] or itab[ key = val ]
    table_expression: ($) =>
      prec(PREC.MEMBER, seq(
        $._expression,
        "[",
        choice(
          $._expression,
          repeat1(seq($.identifier, "=", $._expression)),
        ),
        "]",
      )),

    // Inline DATA(var) and FIELD-SYMBOL(<fs>)
    inline_data_declaration: ($) =>
      seq(kw("DATA"), "(", $.identifier, ")"),

    inline_field_symbol: ($) =>
      seq(kwSeq("FIELD-SYMBOL"), "(", $.field_symbol, ")"),

    // Predicate expressions
    predicate_expression: ($) =>
      prec.left(PREC.COMPARE, choice(
        seq($._expression, kw("IS"), kw("INITIAL")),
        seq($._expression, kw("IS"), kw("NOT"), kw("INITIAL")),
        seq($._expression, kw("IS"), kw("ASSIGNED")),
        seq($._expression, kw("IS"), kw("NOT"), kw("ASSIGNED")),
        seq($._expression, kw("IS"), kw("BOUND")),
        seq($._expression, kw("IS"), kw("NOT"), kw("BOUND")),
        seq($._expression, kw("IS"), kw("SUPPLIED")),
        seq($._expression, kw("IS"), kw("NOT"), kw("SUPPLIED")),
        seq($._expression, kw("IS"), kw("REQUESTED")),
        seq($._expression, kw("IS"), kw("NOT"), kw("REQUESTED")),
        seq($._expression, kw("IS"), kw("INSTANCE"), kw("OF"), $._expression),
        seq($._expression, kw("BETWEEN"), $._expression, kw("AND"), $._expression),
        seq($._expression, kw("IS"), kw("NOT"), kw("INSTANCE"), kw("OF"), $._expression),
        seq($._expression, kw("NOT"), kw("BETWEEN"), $._expression, kw("AND"), $._expression),
      )),

    // =========================================================================
    // Constructor expressions (7.40+)
    // =========================================================================

    new_expression: ($) =>
      seq(kw("NEW"), $._type_reference, "(", optional($._constructor_body), ")"),

    value_expression: ($) =>
      seq(kw("VALUE"), $._type_reference, "(", optional($._constructor_body), ")"),

    ref_expression: ($) =>
      seq(kw("REF"), $._type_reference, "(", $._expression, ")"),

    conv_expression: ($) =>
      seq(kw("CONV"), $._type_reference, "(", $._expression, ")"),

    cast_expression: ($) =>
      seq(kw("CAST"), $._type_reference, "(", $._expression, ")"),

    exact_expression: ($) =>
      seq(kw("EXACT"), $._type_reference, "(", $._expression, ")"),

    corresponding_expression: ($) =>
      seq(kw("CORRESPONDING"), $._type_reference, "(", optional($._constructor_body), ")"),

    cond_expression: ($) =>
      seq(kw("COND"), $._type_reference, "(", repeat1($.cond_branch), ")"),

    switch_expression: ($) =>
      seq(kw("SWITCH"), $._type_reference, "(", $._expression, repeat1($.switch_branch), ")"),

    filter_expression: ($) =>
      seq(kw("FILTER"), $._type_reference, "(", $._expression, optional($.filter_where), ")"),

    reduce_expression: ($) =>
      seq(kw("REDUCE"), $._type_reference, "(", $._reduce_body, ")"),

    _type_reference: ($) =>
      choice($.identifier, $.field_symbol, "#"),

    _constructor_body: ($) =>
      choice(
        $._expression,
        repeat1($.named_argument),
        seq(optional(seq(kw("BASE"), $._expression)), repeat1($.value_line)),
        seq(optional($.let_expression), repeat1($.for_expression), $._expression),
      ),

    cond_branch: ($) =>
      choice(
        seq(kw("WHEN"), $._expression, kw("THEN"), $._expression),
        seq(kw("ELSE"), $._expression),
      ),

    switch_branch: ($) =>
      choice(
        seq(kw("WHEN"), $._expression, kw("THEN"), $._expression),
        seq(kw("ELSE"), $._expression),
      ),

    filter_where: ($) =>
      seq(kw("WHERE"), $._expression),

    _reduce_body: ($) =>
      seq(
        optional($.let_expression),
        kw("INIT"), repeat1(seq($.identifier, optional($._type_spec), "=", $._expression)),
        kw("FOR"), repeat1($.for_expression),
        kw("NEXT"), repeat1(seq($.identifier, "=", $._expression)),
      ),

    value_line: ($) =>
      seq("(", optional($._constructor_body), ")"),

    let_expression: ($) =>
      seq(kw("LET"), repeat1(seq($.identifier, optional($._type_spec), "=", $._expression)), kw("IN")),

    for_expression: ($) =>
      choice(
        // FOR wa IN itab [WHERE (...)]
        seq(
          kw("FOR"),
          choice($.identifier, $.field_symbol, $.inline_data_declaration, $.inline_field_symbol),
          kw("IN"),
          $._expression,
          optional(seq(kw("WHERE"), "(", $._expression, ")")),
        ),
        // FOR i = start [THEN expr] UNTIL/WHILE cond
        seq(
          kw("FOR"),
          choice($.identifier, $.inline_data_declaration),
          "=",
          $._expression,
          optional(seq(kw("THEN"), $._expression)),
          choice(kw("UNTIL"), kw("WHILE")),
          $._expression,
        ),
      ),

    // =========================================================================
    // Phase 2: Program introductions
    // =========================================================================

    report_statement: ($) =>
      seq(kw("REPORT"), optional($.identifier), repeat($._report_addition)),

    _report_addition: ($) =>
      choice(
        seq(kw("NO"), kwSeq("STANDARD PAGE"), kw("HEADING")),
        seq(kwSeq("LINE-SIZE"), $.integer_literal),
        seq(kwSeq("LINE-COUNT"), $.integer_literal, optional(seq("(", $.integer_literal, ")"))),
        seq(kwSeq("MESSAGE-ID"), $.identifier),
      ),

    program_statement: ($) => seq(kw("PROGRAM"), optional($.identifier)),

    function_pool_statement: ($) =>
      seq(kwSeq("FUNCTION-POOL"), $.identifier, optional(seq(kwSeq("MESSAGE-ID"), $.identifier))),

    class_pool_statement: (_$) => kwSeq("CLASS-POOL"),
    interface_pool_statement: (_$) => kwSeq("INTERFACE-POOL"),
    type_pool_statement: ($) => seq(kwSeq("TYPE-POOL"), $.identifier),

    include_statement: ($) =>
      seq(kw("INCLUDE"), $.identifier, optional(kwSeq("IF FOUND"))),

    // =========================================================================
    // Phase 2: Type specifications
    // =========================================================================

    _type_spec: ($) =>
      choice(
        seq(kw("TYPE"), $._type_expression),
        seq(kw("LIKE"), $._type_expression),
        seq(kw("TYPE"), kw("REF"), kw("TO"), $._type_expression),
        seq(kw("LIKE"), kw("REF"), kw("TO"), $._type_expression),
        seq(kw("TYPE"), $._table_type),
        seq(kw("LIKE"), $._table_type),
        seq(kw("TYPE"), kw("RANGE"), kw("OF"), $._type_expression),
        seq(kw("TYPE"), kw("LINE"), kw("OF"), $._type_expression),
      ),

    _type_expression: ($) =>
      choice(
        $.identifier,
        $._builtin_type,
        $.component_access,
      ),

    _builtin_type: (_$) =>
      choice(
        kw("i"), kw("f"), kw("p"), kw("c"), kw("n"), kw("d"), kw("t"), kw("x"),
        kw("string"), kw("xstring"),
        kw("decfloat16"), kw("decfloat34"),
        kw("int8"), kw("utclong"),
        kw("any"), kwSeq("any table"),
        kw("clike"), kw("csequence"), kw("numeric"), kw("xsequence"),
        kwSeq("data"), kw("object"),
      ),

    _table_type: ($) =>
      seq(
        optional(choice(kw("STANDARD"), kw("SORTED"), kw("HASHED"))),
        kw("TABLE"),
        kw("OF"),
        $._type_expression,
        optional($._table_key),
      ),

    _table_key: ($) =>
      choice(
        seq(kw("WITH"), optional(choice(kw("UNIQUE"), kwSeq("NON-UNIQUE"))),
          optional(kw("DEFAULT")), kw("KEY"), optional($._key_fields)),
        seq(kw("WITH"), kw("EMPTY"), kw("KEY")),
      ),

    _key_fields: ($) =>
      repeat1($.identifier),

    // =========================================================================
    // Phase 2: DATA
    // =========================================================================

    data_statement: ($) =>
      choice(
        seq(kw("DATA"), $.identifier, optional($._type_spec), repeat($._data_additions)),
        // Structured: DATA BEGIN OF / END OF
        seq(kw("DATA"), kw("BEGIN"), kw("OF"), $.identifier, optional(kw("READ-ONLY"))),
        seq(kw("DATA"), kw("END"), kw("OF"), $.identifier),
      ),

    _data_additions: ($) =>
      choice(
        seq(kw("VALUE"), choice($._expression, kwSeq("IS INITIAL"))),
        kw("READ-ONLY"),
        seq(kw("LENGTH"), $._expression),
        seq(kw("DECIMALS"), $._expression),
      ),

    // =========================================================================
    // Phase 2: TYPES
    // =========================================================================

    types_statement: ($) =>
      choice(
        seq(kw("TYPES"), $.identifier, optional($._type_spec), repeat($._type_additions)),
        seq(kw("TYPES"), kw("BEGIN"), kw("OF"), $.identifier),
        seq(kw("TYPES"), kw("END"), kw("OF"), $.identifier),
        // ENUM types (7.40+)
        seq(kw("TYPES"), kw("BEGIN"), kw("OF"), kw("ENUM"), $.identifier, optional(seq(kw("STRUCTURE"), $.identifier))),
        seq(kw("TYPES"), kw("END"), kw("OF"), kw("ENUM"), $.identifier, optional(seq(kw("STRUCTURE"), $.identifier))),
      ),

    _type_additions: (_$) =>
      choice(
        seq(kw("LENGTH"), /\d+/),
        seq(kw("DECIMALS"), /\d+/),
      ),

    // =========================================================================
    // Phase 2: CONSTANTS, STATICS, CLASS-DATA
    // =========================================================================

    constants_statement: ($) =>
      choice(
        seq(kw("CONSTANTS"), $.identifier, optional($._type_spec), kw("VALUE"), $._expression),
        seq(kw("CONSTANTS"), kw("BEGIN"), kw("OF"), $.identifier),
        seq(kw("CONSTANTS"), kw("END"), kw("OF"), $.identifier),
      ),

    statics_statement: ($) =>
      seq(kw("STATICS"), $.identifier, optional($._type_spec), repeat($._data_additions)),

    class_data_statement: ($) =>
      choice(
        seq(kwSeq("CLASS-DATA"), $.identifier, optional($._type_spec), repeat($._data_additions)),
        seq(kwSeq("CLASS-DATA"), kw("BEGIN"), kw("OF"), $.identifier),
        seq(kwSeq("CLASS-DATA"), kw("END"), kw("OF"), $.identifier),
      ),

    // =========================================================================
    // Phase 2: FIELD-SYMBOLS
    // =========================================================================

    field_symbols_statement: ($) =>
      seq(kwSeq("FIELD-SYMBOLS"), $.field_symbol, optional($._type_spec)),

    // =========================================================================
    // Phase 2: PARAMETERS, SELECT-OPTIONS
    // =========================================================================

    parameters_statement: ($) =>
      seq(kw("PARAMETERS"), $.identifier, optional($._type_spec), repeat($._parameter_additions)),

    _parameter_additions: ($) =>
      choice(
        seq(kw("DEFAULT"), $._expression),
        kw("OBLIGATORY"),
        seq(kw("LOWER"), kw("CASE")),
        kwSeq("AS CHECKBOX"),
        seq(kwSeq("RADIOBUTTON GROUP"), $.identifier),
        seq(kw("MATCHCODE"), kw("OBJECT"), $.identifier),
        seq(kw("MODIF"), kw("ID"), $.identifier),
        seq(kw("MEMORY"), kw("ID"), $.identifier),
        kwSeq("NO-DISPLAY"),
        seq(kw("VALUE"), kw("CHECK")),
        kw("AS LISTBOX"),
      ),

    select_options_statement: ($) =>
      seq(kwSeq("SELECT-OPTIONS"), $.identifier,
        kw("FOR"), $._expression,
        repeat($._select_option_additions)),

    _select_option_additions: ($) =>
      choice(
        seq(kw("DEFAULT"), $._expression, optional(seq(kw("TO"), $._expression)),
          optional(seq(kw("OPTION"), $.identifier)),
          optional(seq(kw("SIGN"), $.identifier))),
        kwSeq("NO-DISPLAY"),
        kwSeq("NO-EXTENSION"),
        kwSeq("NO INTERVALS"),
        kw("OBLIGATORY"),
        seq(kw("LOWER"), kw("CASE")),
        seq(kw("MODIF"), kw("ID"), $.identifier),
        seq(kw("MEMORY"), kw("ID"), $.identifier),
        seq(kw("MATCHCODE"), kw("OBJECT"), $.identifier),
      ),

    // =========================================================================
    // Phase 2: TABLES, TYPE-POOLS, INCLUDE TYPE/STRUCTURE
    // =========================================================================

    tables_statement: ($) =>
      seq(kw("TABLES"), $.identifier),

    type_pools_statement: ($) =>
      seq(kwSeq("TYPE-POOLS"), $.identifier),

    include_type_statement: ($) =>
      seq(kw("INCLUDE"), choice(kw("TYPE"), kw("STRUCTURE")), $.identifier,
        optional(seq(kw("AS"), $.identifier)),
        optional(seq(kw("RENAMING"), kw("WITH"), kw("SUFFIX"), $.identifier))),

    // =========================================================================
    // Phase 3: CLASS DEFINITION
    // =========================================================================

    class_definition: ($) =>
      seq(
        kw("CLASS"), $.identifier, kw("DEFINITION"),
        repeat($._class_def_additions),
        ".",
        optional($._class_def_body),
        kw("ENDCLASS"), ".",
      ),

    _class_def_additions: ($) =>
      choice(
        seq(kw("INHERITING"), kw("FROM"), $.identifier),
        kw("ABSTRACT"),
        kw("FINAL"),
        seq(kw("CREATE"), choice(kw("PUBLIC"), kw("PROTECTED"), kw("PRIVATE"))),
        kwSeq("FOR TESTING"),
        seq(kwSeq("RISK LEVEL"), choice(kw("HARMLESS"), kw("DANGEROUS"), kw("CRITICAL"))),
        seq(kw("DURATION"), choice(kw("SHORT"), kw("MEDIUM"), kw("LONG"))),
        seq(kw("FRIENDS"), repeat1($.identifier)),
        kw("PUBLIC"),
        kwSeq("DEFERRED"),
        seq(kwSeq("LOCAL FRIENDS"), repeat1($.identifier)),
      ),

    _class_def_body: ($) =>
      repeat1(choice(
        $.visibility_section,
        $._statement,
      )),

    visibility_section: ($) =>
      seq(
        choice(kw("PUBLIC"), kw("PROTECTED"), kw("PRIVATE")),
        kw("SECTION"),
        ".",
        repeat($._statement),
      ),

    // =========================================================================
    // Phase 3: CLASS IMPLEMENTATION
    // =========================================================================

    class_implementation: ($) =>
      seq(
        kw("CLASS"), $.identifier, kw("IMPLEMENTATION"), ".",
        repeat(choice($.method_implementation, $._statement)),
        kw("ENDCLASS"), ".",
      ),

    // =========================================================================
    // Phase 3: INTERFACE
    // =========================================================================

    interface_definition: ($) =>
      seq(
        kw("INTERFACE"), $.identifier, optional(kw("DEFERRED")), ".",
        repeat($._statement),
        kw("ENDINTERFACE"), ".",
      ),

    // =========================================================================
    // Phase 3: METHOD declaration & implementation
    // =========================================================================

    method_statement: ($) =>
      choice(
        seq(choice(kw("METHODS"), kwSeq("CLASS-METHODS")),
          $.identifier, repeat($._method_additions)),
      ),

    _method_additions: ($) =>
      choice(
        kw("ABSTRACT"),
        kw("FINAL"),
        kw("REDEFINITION"),
        $.method_importing,
        $.method_exporting,
        $.method_changing,
        $.method_returning,
        $.method_raising,
        $.method_exceptions,
        seq(kw("DEFAULT"), choice(kw("IGNORE"), kw("FAIL"))),
        seq(kw("FOR"), kw("EVENT"), $.identifier, kw("OF"), $.identifier,
          optional(seq(kw("IMPORTING"), repeat1($.method_param)))),
        seq(kw("FOR"), kw("TESTING")),
      ),

    method_importing: ($) =>
      seq(kw("IMPORTING"), repeat1($.method_param)),

    method_exporting: ($) =>
      seq(kw("EXPORTING"), repeat1($.method_param)),

    method_changing: ($) =>
      seq(kw("CHANGING"), repeat1($.method_param)),

    method_returning: ($) =>
      seq(kw("RETURNING"), kw("VALUE"), "(", $.identifier, ")", optional($._type_spec)),

    method_raising: ($) =>
      seq(kw("RAISING"), repeat1($._type_expression)),

    method_exceptions: ($) =>
      seq(kw("EXCEPTIONS"), repeat1($.identifier)),

    method_param: ($) =>
      seq(
        optional(choice(kw("VALUE"), kw("REFERENCE"))),
        choice(
          seq("(", $.identifier, ")", optional($._type_spec)),
          seq($.identifier, optional($._type_spec)),
        ),
        optional(kw("OPTIONAL")),
        optional(seq(kw("DEFAULT"), $._expression)),
      ),

    method_implementation: ($) =>
      seq(
        kw("METHOD"), $.identifier, ".",
        repeat($._statement),
        kw("ENDMETHOD"), ".",
      ),

    // =========================================================================
    // Phase 3: INTERFACE in class body, ALIASES
    // =========================================================================

    interface_def_statement: ($) =>
      seq(kw("INTERFACES"), $.identifier,
        repeat(choice(
          seq(kw("ALL"), kw("METHODS"), choice(kw("ABSTRACT"), kw("FINAL"))),
          seq(kw("DATA"), kw("VALUES"), repeat1(seq($.identifier, "=", $._expression))),
        ))),

    aliases_statement: ($) =>
      seq(kw("ALIASES"), $.identifier, kw("FOR"), $._expression),

    // =========================================================================
    // Phase 3: EVENTS
    // =========================================================================

    events_statement: ($) =>
      seq(kw("EVENTS"), $.identifier,
        optional(seq(kw("EXPORTING"), repeat1($.method_param)))),

    class_events_statement: ($) =>
      seq(kwSeq("CLASS-EVENTS"), $.identifier,
        optional(seq(kw("EXPORTING"), repeat1($.method_param)))),

    // =========================================================================
    // Phase 3: FORM
    // =========================================================================

    form_definition: ($) =>
      seq(
        kw("FORM"), $.identifier,
        repeat($._form_parameter_section),
        ".",
        repeat($._statement),
        kw("ENDFORM"), ".",
      ),

    _form_parameter_section: ($) =>
      choice(
        seq(kw("USING"), repeat1($._form_param)),
        seq(kw("CHANGING"), repeat1($._form_param)),
        seq(kw("TABLES"), repeat1($._form_param)),
      ),

    _form_param: ($) =>
      seq(
        optional(kw("VALUE")),
        choice(
          seq("(", $.identifier, ")", optional($._type_spec)),
          seq($.identifier, optional($._type_spec)),
          seq($.field_symbol, optional($._type_spec)),
        ),
      ),

    // =========================================================================
    // Phase 3: FUNCTION, MODULE
    // =========================================================================

    function_implementation: ($) =>
      seq(
        kw("FUNCTION"), $.identifier, ".",
        repeat($._statement),
        kw("ENDFUNCTION"), ".",
      ),

    module_implementation: ($) =>
      seq(
        kw("MODULE"), $.identifier,
        optional(choice(kw("INPUT"), kw("OUTPUT"))),
        ".",
        repeat($._statement),
        kw("ENDMODULE"), ".",
      ),

    // =========================================================================
    // Phase 3: DEFINE (macros)
    // =========================================================================

    define_statement: ($) =>
      seq(
        kw("DEFINE"), $.identifier, ".",
        repeat(token(prec(-1, /[^\n]+/))),
        kwSeq("END-OF-DEFINITION"), ".",
      ),

    // =========================================================================
    // Phase 3: Event blocks
    // =========================================================================

    initialization_statement: (_$) => kw("INITIALIZATION"),

    start_of_selection_statement: (_$) => kwSeq("START-OF-SELECTION"),

    end_of_selection_statement: (_$) => kwSeq("END-OF-SELECTION"),

    top_of_page_statement: ($) =>
      seq(kwSeq("TOP-OF-PAGE"), optional(seq(kw("DURING"), kwSeq("LINE-SELECTION")))),

    end_of_page_statement: (_$) => kwSeq("END-OF-PAGE"),

    at_line_selection_statement: (_$) => kwSeq("AT LINE-SELECTION"),

    at_user_command_statement: (_$) => kwSeq("AT USER-COMMAND"),

    at_selection_screen_statement: ($) =>
      seq(
        kwSeq("AT SELECTION-SCREEN"),
        optional(choice(
          seq(kw("ON"), choice($.identifier, $.field_symbol)),
          seq(kw("ON"), kwSeq("VALUE-REQUEST"), kw("FOR"), $.identifier),
          seq(kw("ON"), kwSeq("HELP-REQUEST"), kw("FOR"), $.identifier),
          seq(kw("ON"), kw("BLOCK"), $.identifier),
          seq(kw("ON"), kwSeq("RADIOBUTTON GROUP"), $.identifier),
          seq(kw("ON"), kw("END"), kw("OF"), $.identifier),
          kw("OUTPUT"),
        )),
      ),

    // =========================================================================
    // Phase 4: IF / CASE / LOOP / DO / WHILE
    // =========================================================================

    if_statement: ($) =>
      seq(
        kw("IF"), $._expression, ".",
        repeat($._statement),
        repeat($.elseif_clause),
        optional($.else_clause),
        kw("ENDIF"), ".",
      ),

    elseif_clause: ($) =>
      seq(kw("ELSEIF"), $._expression, ".", repeat($._statement)),

    else_clause: ($) =>
      seq(kw("ELSE"), ".", repeat($._statement)),

    case_statement: ($) =>
      seq(
        kw("CASE"), $._expression, ".",
        repeat($.when_clause),
        optional($.when_others_clause),
        kw("ENDCASE"), ".",
      ),

    case_type_statement: ($) =>
      seq(
        kw("CASE"), kw("TYPE"), kw("OF"), $._expression, ".",
        repeat($.when_type_clause),
        optional($.when_others_clause),
        kw("ENDCASE"), ".",
      ),

    when_clause: ($) =>
      seq(kw("WHEN"), commaSep1($._expression), ".", repeat($._statement)),

    when_type_clause: ($) =>
      seq(kw("WHEN"), kw("TYPE"), $._expression,
        optional(seq(kw("INTO"), $._expression)),
        ".", repeat($._statement)),

    when_others_clause: ($) =>
      seq(kw("WHEN"), kw("OTHERS"), ".", repeat($._statement)),

    do_statement: ($) =>
      seq(
        kw("DO"),
        optional(seq($._expression, kw("TIMES"))),
        ".",
        repeat($._statement),
        kw("ENDDO"), ".",
      ),

    while_statement: ($) =>
      seq(
        kw("WHILE"), $._expression, ".",
        repeat($._statement),
        kw("ENDWHILE"), ".",
      ),

    loop_statement: ($) =>
      seq(
        kw("LOOP"), kw("AT"), $._expression,
        repeat($._loop_addition),
        ".",
        repeat(choice($._statement, $.at_block)),
        kw("ENDLOOP"), ".",
      ),

    _loop_addition: ($) =>
      choice(
        seq(kw("INTO"), $._expression),
        seq(kw("ASSIGNING"), choice($.field_symbol, $.inline_field_symbol)),
        seq(kwSeq("REFERENCE INTO"), $._expression),
        seq(kw("FROM"), $._expression),
        seq(kw("TO"), $._expression),
        seq(kw("WHERE"), $._expression),
        seq(kwSeq("TRANSPORTING NO FIELDS")),
        seq(kwSeq("GROUP BY"), $._expression),
        seq(kw("USING"), kw("KEY"), $.identifier),
      ),

    at_block: ($) =>
      seq(
        kw("AT"),
        choice(kw("FIRST"), kw("LAST"), seq(kw("NEW"), $.identifier), seq(kwSeq("END OF"), $.identifier)),
        ".",
        repeat($._statement),
        kw("ENDAT"), ".",
      ),

    // =========================================================================
    // Phase 4: TRY / CATCH
    // =========================================================================

    try_statement: ($) =>
      seq(
        kw("TRY"), ".",
        repeat($._statement),
        repeat($.catch_clause),
        optional($.cleanup_clause),
        kw("ENDTRY"), ".",
      ),

    catch_clause: ($) =>
      seq(
        kw("CATCH"),
        optional(kwSeq("BEFORE UNWIND")),
        repeat1($._type_expression),
        optional(seq(kw("INTO"), $._expression)),
        ".",
        repeat($._statement),
      ),

    cleanup_clause: ($) =>
      seq(
        kw("CLEANUP"),
        optional(seq(kw("INTO"), $._expression)),
        ".",
        repeat($._statement),
      ),

    // =========================================================================
    // Phase 4: RAISE
    // =========================================================================

    raise_statement: ($) =>
      choice(
        seq(kw("RAISE"), kw("EXCEPTION"), kw("TYPE"), $._type_expression,
          repeat(seq(kw("EXPORTING"), repeat1($.named_argument)))),
        seq(kw("RAISE"), kw("EXCEPTION"), $._expression),
        seq(kw("RAISE"), kw("SHORTDUMP"), kw("TYPE"), $._type_expression),
        seq(kw("RAISE"), $.identifier),
      ),

    raise_event_statement: ($) =>
      seq(kw("RAISE"), kw("EVENT"), $.identifier,
        optional(seq(kw("EXPORTING"), repeat1($.named_argument)))),

    // =========================================================================
    // Phase 4: CALL METHOD / FUNCTION / PERFORM
    // =========================================================================

    call_method_statement: ($) =>
      seq(kwSeq("CALL METHOD"), $._expression,
        repeat($._call_params)),

    call_function_statement: ($) =>
      seq(kwSeq("CALL FUNCTION"), $._expression,
        repeat($._call_function_addition)),

    _call_function_addition: ($) =>
      choice(
        $._call_params,
        seq(kw("DESTINATION"), $._expression),
        kwSeq("IN UPDATE TASK"),
        kwSeq("IN BACKGROUND TASK"),
        seq(kwSeq("STARTING NEW TASK"), $._expression),
        seq(kwSeq("PERFORMING"), $.identifier, kw("ON"), kwSeq("END OF"), kw("TASK")),
        seq(kw("TABLES"), repeat1(seq($.identifier, "=", $._expression))),
      ),

    _call_params: ($) =>
      choice(
        seq(kw("EXPORTING"), repeat1($.named_argument)),
        seq(kw("IMPORTING"), repeat1($.named_argument)),
        seq(kw("CHANGING"), repeat1($.named_argument)),
        seq(kw("RECEIVING"), repeat1($.named_argument)),
        seq(kw("EXCEPTIONS"), repeat1(choice(
          seq($.identifier, "=", $._expression),
          seq(kw("OTHERS"), "=", $._expression),
        ))),
      ),

    perform_statement: ($) =>
      seq(kw("PERFORM"), $._expression,
        optional(seq(kw("IN"), kw("PROGRAM"), $._expression)),
        optional(kwSeq("IF FOUND")),
        repeat($._form_parameter_section)),

    submit_statement: ($) =>
      seq(kw("SUBMIT"), $._expression,
        repeat($._submit_addition)),

    _submit_addition: ($) =>
      choice(
        kwSeq("VIA SELECTION-SCREEN"),
        kwSeq("AND RETURN"),
        seq(kw("WITH"), $.identifier, choice(
          seq("=", $._expression),
          seq(kw("IN"), $._expression),
          seq(kw("NOT"), kw("IN"), $._expression),
          seq(kw("BETWEEN"), $._expression, kw("AND"), $._expression),
          seq(kwSeq("NOT BETWEEN"), $._expression, kw("AND"), $._expression),
        )),
        seq(kw("USING"), kwSeq("SELECTION-SET"), $._expression),
        seq(kw("TO"), kwSeq("SAP-SPOOL")),
        seq(kwSeq("SPOOL PARAMETERS"), $._expression),
        seq(kw("EXPORTING"), kwSeq("LIST TO MEMORY")),
      ),

    call_transaction_statement: ($) =>
      seq(kwSeq("CALL TRANSACTION"), $._expression,
        repeat(choice(
          kwSeq("AND SKIP FIRST SCREEN"),
          seq(kw("USING"), $._expression),
          seq(kw("MODE"), $._expression),
          seq(kw("UPDATE"), $._expression),
          seq(kw("MESSAGES"), kw("INTO"), $._expression),
        ))),

    call_screen_statement: ($) =>
      seq(kwSeq("CALL SCREEN"), $._expression,
        optional(seq(kwSeq("STARTING AT"), $._expression, $._expression)),
        optional(seq(kwSeq("ENDING AT"), $._expression, $._expression))),

    // =========================================================================
    // Phase 4: EXIT statements
    // =========================================================================

    leave_statement: ($) =>
      choice(
        seq(kw("LEAVE"), optional(kw("PROGRAM"))),
        seq(kw("LEAVE"), kw("TO"), kw("TRANSACTION"), $._expression),
        seq(kw("LEAVE"), kw("TO"), kw("SCREEN"), $._expression),
        seq(kw("LEAVE"), kwSeq("LIST-PROCESSING")),
        seq(kw("LEAVE"), kw("TO"), kwSeq("LIST-PROCESSING")),
      ),

    return_statement: (_$) => kw("RETURN"),
    exit_statement: (_$) => kw("EXIT"),
    check_statement: ($) => seq(kw("CHECK"), $._expression),
    continue_statement: (_$) => kw("CONTINUE"),

    // =========================================================================
    // Phase 5: Assignments
    // =========================================================================

    assignment_statement: ($) =>
      prec.right(seq($._expression, choice("=", "+=", "-=", "*=", "/=", "&&="), $._expression)),

    move_statement: ($) =>
      seq(kw("MOVE"), $._expression, kw("TO"), $._expression),

    move_corresponding_statement: ($) =>
      seq(kwSeq("MOVE-CORRESPONDING"), $._expression, kw("TO"), $._expression),

    assign_statement: ($) =>
      seq(kw("ASSIGN"), $._expression, kw("TO"), $.field_symbol,
        repeat(choice(
          seq(kw("CASTING"), optional(choice(
            seq(kw("TYPE"), $._type_expression),
            seq(kw("LIKE"), $._expression),
            seq(kw("TYPE"), kw("HANDLE"), $._expression),
          ))),
          seq(kw("TYPE"), $._type_expression),
          seq(kw("DECIMALS"), $._expression),
        ))),

    unassign_statement: ($) =>
      seq(kw("UNASSIGN"), $.field_symbol),

    clear_statement: ($) =>
      seq(kw("CLEAR"), $._expression,
        optional(choice(
          seq(kw("WITH"), $._expression),
          kwSeq("WITH NULL"),
        ))),

    free_statement: ($) =>
      seq(kw("FREE"), $._expression),

    get_reference_statement: ($) =>
      seq(kw("GET"), kw("REFERENCE"), kw("OF"), $._expression, kw("INTO"), $._expression),

    // =========================================================================
    // Phase 5: Internal table operations
    // =========================================================================

    read_table_statement: ($) =>
      seq(kwSeq("READ TABLE"), $._expression,
        repeat($._read_table_addition)),

    _read_table_addition: ($) =>
      choice(
        seq(kw("INTO"), $._expression),
        seq(kw("ASSIGNING"), choice($.field_symbol, $.inline_field_symbol)),
        seq(kwSeq("REFERENCE INTO"), $._expression),
        kwSeq("TRANSPORTING NO FIELDS"),
        seq(kw("INDEX"), $._expression),
        seq(kwSeq("WITH KEY"), repeat1(choice(
          seq($.identifier, "=", $._expression),
          $._expression,
        ))),
        seq(kwSeq("WITH TABLE KEY"), repeat1(seq($.identifier, "=", $._expression))),
        seq(kw("USING"), kw("KEY"), $.identifier),
        seq(kwSeq("BINARY SEARCH")),
        seq(kw("TRANSPORTING"), repeat1($.identifier)),
        seq(kw("COMPARING"), choice(repeat1($.identifier), kw("ALL"), kw("FIELDS"))),
      ),

    insert_statement: ($) =>
      choice(
        seq(kw("INSERT"), $._expression, kw("INTO"),
          optional(kw("TABLE")), $._expression,
          repeat($._table_modify_addition)),
        seq(kw("INSERT"), kwSeq("LINES OF"), $._expression,
          optional(seq(kw("FROM"), $._expression, optional(seq(kw("TO"), $._expression)))),
          kw("INTO"), optional(kw("TABLE")), $._expression,
          optional(seq(kw("INDEX"), $._expression))),
        seq(kw("INSERT"), kw("INITIAL"), kw("LINE"), kw("INTO"),
          optional(kw("TABLE")), $._expression,
          repeat($._table_modify_addition)),
      ),

    append_statement: ($) =>
      choice(
        seq(kw("APPEND"), $._expression, kw("TO"), $._expression,
          repeat($._table_modify_addition)),
        seq(kw("APPEND"), kwSeq("LINES OF"), $._expression, kw("TO"), $._expression),
        seq(kw("APPEND"), kw("INITIAL"), kw("LINE"), kw("TO"), $._expression,
          repeat($._table_modify_addition)),
      ),

    modify_table_statement: ($) =>
      seq(kw("MODIFY"), optional(kw("TABLE")), $._expression,
        repeat($._table_modify_addition)),

    _table_modify_addition: ($) =>
      choice(
        seq(kw("INDEX"), $._expression),
        seq(kw("FROM"), $._expression),
        seq(kw("TRANSPORTING"), repeat1($.identifier)),
        seq(kw("ASSIGNING"), choice($.field_symbol, $.inline_field_symbol)),
        seq(kwSeq("REFERENCE INTO"), $._expression),
        seq(kw("WHERE"), $._expression),
        seq(kw("USING"), kw("KEY"), $.identifier),
      ),

    delete_statement: ($) =>
      choice(
        seq(kw("DELETE"), optional(kw("TABLE")), $._expression,
          repeat($._table_modify_addition)),
        seq(kw("DELETE"), $._expression, kw("WHERE"), $._expression),
        seq(kw("DELETE"), kwSeq("ADJACENT DUPLICATES"), kw("FROM"), $._expression,
          optional(seq(kw("COMPARING"), choice(repeat1($.identifier), kw("ALL"), kw("FIELDS"))))),
      ),

    sort_statement: ($) =>
      seq(kw("SORT"), $._expression,
        repeat(choice(
          seq(kw("BY"), repeat1(seq($._expression, optional(choice(kw("ASCENDING"), kw("DESCENDING")))))),
          kw("ASCENDING"),
          kw("DESCENDING"),
          kw("STABLE"),
          kwSeq("AS TEXT"),
        ))),

    collect_statement: ($) =>
      seq(kw("COLLECT"), $._expression, kw("INTO"), $._expression),

    describe_table_statement: ($) =>
      seq(kwSeq("DESCRIBE TABLE"), $._expression,
        repeat(choice(
          seq(kw("LINES"), $._expression),
          seq(kw("KIND"), $._expression),
        ))),

    // =========================================================================
    // Phase 5: String operations
    // =========================================================================

    concatenate_statement: ($) =>
      seq(kw("CONCATENATE"), repeat1($._expression),
        kw("INTO"), $._expression,
        optional(seq(kwSeq("SEPARATED BY"), $._expression)),
        optional(kwSeq("RESPECTING BLANKS"))),

    split_statement: ($) =>
      seq(kw("SPLIT"), $._expression, kw("AT"), $._expression,
        kw("INTO"), choice(
          seq(kw("TABLE"), $._expression),
          repeat1($._expression),
        )),

    find_statement: ($) =>
      seq(kw("FIND"), optional(choice(kw("FIRST"), kw("ALL"))),
        optional(kw("OCCURRENCES")), optional(kw("OF")),
        optional(choice(kw("REGEX"), kw("SUBSTRING"))),
        $._expression, kw("IN"), optional(kw("TABLE")), $._expression,
        repeat($._find_replace_addition)),

    replace_statement: ($) =>
      seq(kw("REPLACE"),
        optional(choice(kw("FIRST"), kw("ALL"))),
        optional(kw("OCCURRENCES")), optional(kw("OF")),
        optional(choice(kw("REGEX"), kw("SUBSTRING"))),
        $._expression, kw("IN"), optional(kw("TABLE")), $._expression,
        kw("WITH"), $._expression,
        repeat($._find_replace_addition)),

    _find_replace_addition: ($) =>
      choice(
        seq(kw("MATCH"), kw("OFFSET"), $._expression),
        seq(kw("MATCH"), kw("LENGTH"), $._expression),
        seq(kw("MATCH"), kw("COUNT"), $._expression),
        seq(kw("RESULTS"), $._expression),
        seq(kw("SUBMATCHES"), repeat1($._expression)),
        kwSeq("IGNORING CASE"),
        kwSeq("RESPECTING CASE"),
        seq(kw("IN"), kwSeq("BYTE MODE")),
        seq(kw("IN"), kwSeq("CHARACTER MODE")),
        seq(kw("SECTION"), kw("OFFSET"), $._expression, kw("LENGTH"), $._expression),
      ),

    search_statement: ($) =>
      seq(kw("SEARCH"), $._expression, kw("FOR"), $._expression,
        repeat(choice(
          kwSeq("STARTING AT"), seq($._expression),
          kwSeq("ENDING AT"), seq($._expression),
          kw("ABBREVIATED"),
          kwSeq("AND MARK"),
        ))),

    condense_statement: ($) =>
      seq(kw("CONDENSE"), $._expression,
        optional(kwSeq("NO-GAPS"))),

    translate_statement: ($) =>
      seq(kw("TRANSLATE"), $._expression,
        kw("TO"), choice(kw("UPPER"), kw("LOWER")), kw("CASE")),

    shift_statement: ($) =>
      seq(kw("SHIFT"), $._expression,
        repeat(choice(
          kw("LEFT"),
          kw("RIGHT"),
          kwSeq("CIRCULAR"),
          seq(kw("BY"), $._expression, kw("PLACES")),
          seq(kw("UP"), kw("TO"), $._expression),
          kwSeq("DELETING LEADING"),
          kwSeq("DELETING TRAILING"),
          seq(kwSeq("IN BYTE MODE")),
          seq(kwSeq("IN CHARACTER MODE")),
        ))),

    overlay_statement: ($) =>
      seq(kw("OVERLAY"), $._expression, kw("WITH"), $._expression,
        optional(seq(kw("ONLY"), $._expression))),

    // =========================================================================
    // Phase 6: Selection screen
    // =========================================================================

    selection_screen_statement: ($) =>
      choice(
        seq(kwSeq("SELECTION-SCREEN"), kw("BEGIN"), kw("OF"), kw("BLOCK"), $.identifier,
          optional(seq(kw("WITH"), kw("FRAME"), optional(seq(kw("TITLE"), $._expression))))),
        seq(kwSeq("SELECTION-SCREEN"), kw("END"), kw("OF"), kw("BLOCK"), $.identifier),
        seq(kwSeq("SELECTION-SCREEN"), kw("BEGIN"), kw("OF"), kw("LINE")),
        seq(kwSeq("SELECTION-SCREEN"), kw("END"), kw("OF"), kw("LINE")),
        seq(kwSeq("SELECTION-SCREEN"), kw("BEGIN"), kw("OF"), kw("SCREEN"), $._expression,
          optional(seq(kw("AS"), kw("SUBSCREEN")))),
        seq(kwSeq("SELECTION-SCREEN"), kw("END"), kw("OF"), kw("SCREEN"), $._expression),
        seq(kwSeq("SELECTION-SCREEN"), kw("SKIP"), optional($._expression)),
        seq(kwSeq("SELECTION-SCREEN"), kw("ULINE"),
          optional(seq("/", $._expression, "(", $._expression, ")"))),
        seq(kwSeq("SELECTION-SCREEN"), kw("COMMENT"),
          optional(seq("/", $._expression, "(", $._expression, ")")),
          $._expression,
          optional(seq(kw("FOR"), kw("FIELD"), $.identifier)),
          optional(seq(kw("MODIF"), kw("ID"), $.identifier))),
        seq(kwSeq("SELECTION-SCREEN"), kw("PUSHBUTTON"),
          optional(seq("/", $._expression, "(", $._expression, ")")),
          $._expression,
          kw("USER-COMMAND"), $.identifier,
          optional(seq(kw("MODIF"), kw("ID"), $.identifier))),
        seq(kwSeq("SELECTION-SCREEN"), kw("TAB"),
          "(", $._expression, ")",
          $._expression,
          kw("USER-COMMAND"), $.identifier,
          optional(seq(kw("DEFAULT"), kw("SCREEN"), $._expression))),
        seq(kwSeq("SELECTION-SCREEN"), kw("FUNCTION"), kw("KEY"), $._expression),
      ),

    // =========================================================================
    // Phase 6: WRITE
    // =========================================================================

    write_statement: ($) =>
      seq(kw("WRITE"),
        optional(seq("/", optional($._expression), optional(seq("(", $._expression, ")")))),
        $._expression,
        repeat($._write_addition)),

    _write_addition: ($) =>
      choice(
        seq(kw("COLOR"), choice($._expression, seq("=", $._expression))),
        kw("INTENSIFIED"),
        kwSeq("INTENSIFIED OFF"),
        kw("INVERSE"),
        kwSeq("INVERSE OFF"),
        kw("HOTSPOT"),
        kwSeq("HOTSPOT OFF"),
        kwSeq("INPUT"),
        kwSeq("INPUT OFF"),
        kwSeq("NO-GAP"),
        kwSeq("NO-ZERO"),
        kwSeq("NO-SIGN"),
        kwSeq("LEFT-JUSTIFIED"),
        kwSeq("CENTERED"),
        kwSeq("RIGHT-JUSTIFIED"),
        seq(kw("UNDER"), $._expression),
        seq(kw("USING"), kw("EDIT"), kw("MASK"), $._expression),
        seq(kwSeq("DD/MM/YY")),
        seq(kwSeq("DD/MM/YYYY")),
        seq(kwSeq("MM/DD/YY")),
        seq(kwSeq("MM/DD/YYYY")),
        seq(kw("DECIMALS"), $._expression),
        seq(kw("ROUND"), $._expression),
        seq(kw("CURRENCY"), $._expression),
        kw("NO-GROUPING"),
        seq(kw("TO"), $._expression),
      ),

    uline_statement: ($) =>
      seq(kw("ULINE"),
        optional(seq("/", optional($._expression), optional(seq("(", $._expression, ")"))))),

    skip_statement: ($) =>
      seq(kw("SKIP"), optional($._expression)),

    new_line_statement: (_$) => kwSeq("NEW-LINE"),

    new_page_statement: ($) =>
      seq(kwSeq("NEW-PAGE"),
        repeat(choice(
          kwSeq("NO-HEADING"),
          kwSeq("NO-TITLE"),
          kwSeq("WITH-HEADING"),
          kwSeq("WITH-TITLE"),
          kwSeq("LINE-SIZE"),
          kwSeq("LINE-COUNT"),
          kwSeq("PRINT ON"),
          kwSeq("PRINT OFF"),
        ))),

    format_statement: ($) =>
      seq(kw("FORMAT"),
        repeat(choice(
          kw("RESET"),
          seq(kw("COLOR"), choice($._expression, kw("OFF"))),
          seq(kw("INTENSIFIED"), optional(kw("OFF"))),
          seq(kw("INVERSE"), optional(kw("OFF"))),
          seq(kw("HOTSPOT"), optional(kw("OFF"))),
          seq(kw("INPUT"), optional(kw("OFF"))),
        ))),

    // =========================================================================
    // Phase 6: SET PF-STATUS, SET TITLEBAR, MESSAGE
    // =========================================================================

    set_pf_status_statement: ($) =>
      seq(kw("SET"), kwSeq("PF-STATUS"), $._expression,
        optional(seq(kw("EXCLUDING"), $._expression)),
        optional(seq(kwSeq("OF PROGRAM"), optional($._expression))),
        optional(seq(kw("IMMEDIATELY"))),
      ),

    set_titlebar_statement: ($) =>
      seq(kw("SET"), kw("TITLEBAR"), $._expression,
        optional(seq(kw("WITH"), repeat1($._expression))),
        optional(seq(kwSeq("OF PROGRAM"), $._expression))),

    message_statement: ($) =>
      seq(kw("MESSAGE"),
        choice(
          seq($._expression, kw("TYPE"), $._expression),
          seq(kw("ID"), $._expression, kw("TYPE"), $._expression, kw("NUMBER"), $._expression),
        ),
        repeat(choice(
          seq(kw("WITH"), repeat1($._expression)),
          seq(kwSeq("DISPLAY LIKE"), $._expression),
          seq(kw("INTO"), $._expression),
          kw("RAISING"), $.identifier,
        ))),

    // =========================================================================
    // Phase 6: SELECT / Open SQL
    // =========================================================================

    select_statement: ($) =>
      seq(kw("SELECT"),
        optional(choice(kw("SINGLE"), kw("DISTINCT"))),
        $._sql_field_list,
        kw("FROM"), $._sql_source,
        optional($._sql_target),
        optional(seq(kw("UP"), kw("TO"), $._expression, kw("ROWS"))),
        optional(seq(kw("WHERE"), $._sql_condition)),
        optional(seq(kwSeq("GROUP BY"), commaSep1($._expression))),
        optional(seq(kw("HAVING"), $._sql_condition)),
        optional(seq(kwSeq("ORDER BY"), commaSep1(seq($._expression, optional(choice(kw("ASCENDING"), kw("DESCENDING"))))))),
        optional($._sql_target)),

    select_loop_statement: ($) =>
      seq(kw("SELECT"),
        optional(kw("DISTINCT")),
        $._sql_field_list,
        kw("FROM"), $._sql_source,
        optional($._sql_target),
        optional(seq(kw("UP"), kw("TO"), $._expression, kw("ROWS"))),
        optional(seq(kw("WHERE"), $._sql_condition)),
        optional(seq(kwSeq("GROUP BY"), commaSep1($._expression))),
        optional(seq(kw("HAVING"), $._sql_condition)),
        optional(seq(kwSeq("ORDER BY"), commaSep1(seq($._expression, optional(choice(kw("ASCENDING"), kw("DESCENDING"))))))),
        optional($._sql_target),
        ".",
        repeat($._statement),
        kw("ENDSELECT"), ".",
      ),

    _sql_field_list: ($) =>
      choice(
        "*",
        commaSep1($._sql_field),
      ),

    _sql_field: ($) =>
      choice(
        $._expression,
        seq($._expression, kw("AS"), $.identifier),
        seq(choice(kw("COUNT"), kw("SUM"), kw("AVG"), kw("MIN"), kw("MAX")),
          "(", choice("*", seq(optional(kw("DISTINCT")), $._expression)), ")"),
      ),

    _sql_source: ($) =>
      choice(
        $._expression,
        seq($._expression, kw("AS"), $.identifier),
        seq($._sql_source, $._sql_join, $._sql_source, kw("ON"), $._sql_condition),
      ),

    _sql_join: (_$) =>
      choice(
        kwSeq("INNER JOIN"),
        kwSeq("LEFT OUTER JOIN"),
        kwSeq("LEFT JOIN"),
        kwSeq("RIGHT OUTER JOIN"),
        kwSeq("RIGHT JOIN"),
        kwSeq("CROSS JOIN"),
      ),

    _sql_target: ($) =>
      choice(
        seq(kw("INTO"), choice(
          seq("(", commaSep1($._expression), ")"),
          seq(kw("TABLE"), $._expression),
          seq(kwSeq("CORRESPONDING FIELDS OF"), optional(kw("TABLE")), $._expression),
          $._expression,
        )),
        seq(kw("APPENDING"), choice(
          seq(kw("TABLE"), $._expression),
          seq(kwSeq("CORRESPONDING FIELDS OF"), kw("TABLE"), $._expression),
        )),
      ),

    _sql_condition: ($) => $._expression,

    // =========================================================================
    // Phase 6: DML
    // =========================================================================

    insert_db_statement: ($) =>
      seq(kw("INSERT"), optional(kw("INTO")), $._expression,
        choice(
          seq(kw("VALUES"), $._expression),
          seq(kw("FROM"), choice($._expression, seq(kw("TABLE"), $._expression))),
        ),
        optional(kwSeq("ACCEPTING DUPLICATE KEYS"))),

    update_db_statement: ($) =>
      seq(kw("UPDATE"), $._expression,
        choice(
          seq(kw("SET"), repeat1(seq($._expression, "=", $._expression))),
          seq(kw("FROM"), choice($._expression, seq(kw("TABLE"), $._expression))),
        ),
        optional(seq(kw("WHERE"), $._expression))),

    modify_db_statement: ($) =>
      seq(kw("MODIFY"), $._expression,
        kw("FROM"), choice($._expression, seq(kw("TABLE"), $._expression))),

    delete_db_statement: ($) =>
      seq(kw("DELETE"), kw("FROM"), $._expression,
        optional(seq(kw("WHERE"), $._expression))),

    // =========================================================================
    // Phase 6: Cursor operations
    // =========================================================================

    open_cursor_statement: ($) =>
      seq(kwSeq("OPEN CURSOR"), optional(kwSeq("WITH HOLD")), $._expression,
        kw("FOR"), kw("SELECT"),
        $._sql_field_list,
        kw("FROM"), $._sql_source,
        optional(seq(kw("WHERE"), $._sql_condition)),
        optional(seq(kwSeq("ORDER BY"), commaSep1($._expression)))),

    fetch_cursor_statement: ($) =>
      seq(kwSeq("FETCH NEXT CURSOR"), $._expression,
        kw("INTO"), $._expression),

    close_cursor_statement: ($) =>
      seq(kwSeq("CLOSE CURSOR"), $._expression),

    // =========================================================================
    // Phase 6: COMMIT / ROLLBACK
    // =========================================================================

    commit_statement: ($) =>
      seq(kwSeq("COMMIT WORK"), optional(kwSeq("AND WAIT"))),

    rollback_statement: (_$) => kwSeq("ROLLBACK WORK"),

    // =========================================================================
    // Phase 7: AUTHORITY-CHECK
    // =========================================================================

    authority_check_statement: ($) =>
      seq(kwSeq("AUTHORITY-CHECK OBJECT"), $._expression,
        repeat1(seq(kw("ID"), $._expression, choice(
          seq(kw("FIELD"), $._expression),
          kw("DUMMY"),
        )))),

    // =========================================================================
    // Phase 7: SET/GET PARAMETER
    // =========================================================================

    set_parameter_statement: ($) =>
      seq(kw("SET"), kw("PARAMETER"), kw("ID"), $._expression, kw("FIELD"), $._expression),

    get_parameter_statement: ($) =>
      seq(kw("GET"), kw("PARAMETER"), kw("ID"), $._expression, kw("FIELD"), $._expression),

    // =========================================================================
    // Phase 7: CALL TRANSFORMATION
    // =========================================================================

    call_transformation_statement: ($) =>
      seq(kwSeq("CALL TRANSFORMATION"), $._expression,
        optional(seq(kw("PARAMETERS"), repeat1(seq($.identifier, "=", $._expression)))),
        optional(seq(kw("OPTIONS"), repeat1(seq($.identifier, "=", $._expression)))),
        seq(kw("SOURCE"), repeat1(choice(
          seq($.identifier, "=", $._expression),
          seq(kw("XML"), $._expression),
        ))),
        seq(kw("RESULT"), repeat1(choice(
          seq($.identifier, "=", $._expression),
          seq(kw("XML"), $._expression),
        )))),

    // =========================================================================
    // Phase 7: EXPORT / IMPORT
    // =========================================================================

    export_statement: ($) =>
      seq(kw("EXPORT"),
        repeat1(choice(
          seq($.identifier, optional(seq(kw("FROM"), $._expression))),
          seq($.identifier, "=", $._expression),
        )),
        kw("TO"),
        $._export_import_medium),

    import_statement: ($) =>
      seq(kw("IMPORT"),
        repeat1(choice(
          seq($.identifier, optional(seq(kw("TO"), $._expression))),
          seq($.identifier, "=", $._expression),
        )),
        kw("FROM"),
        $._export_import_medium),

    _export_import_medium: ($) =>
      choice(
        seq(kwSeq("DATA BUFFER"), $._expression),
        seq(kw("MEMORY"), kw("ID"), $._expression),
        seq(kw("DATABASE"), $._expression, "(", $._expression, ")",
          optional(seq(kw("ID"), $._expression)),
          optional(seq(kw("CLIENT"), $._expression))),
        seq(kwSeq("SHARED MEMORY"), $._expression, "(", $._expression, ")",
          optional(seq(kw("ID"), $._expression)),
          optional(seq(kw("CLIENT"), $._expression))),
        seq(kwSeq("SHARED BUFFER"), $._expression, "(", $._expression, ")",
          optional(seq(kw("ID"), $._expression)),
          optional(seq(kw("CLIENT"), $._expression))),
        seq(kwSeq("INTERNAL TABLE"), $._expression),
      ),

    // =========================================================================
    // Phase 7: CREATE OBJECT
    // =========================================================================

    create_object_statement: ($) =>
      seq(kwSeq("CREATE OBJECT"), $._expression,
        optional(seq(kw("TYPE"), $._type_expression)),
        optional(seq(kw("EXPORTING"), repeat1($.named_argument))),
        optional(seq(kw("EXCEPTIONS"), repeat1(seq($.identifier, "=", $._expression))))),

    // =========================================================================
    // Phase 7: SET HANDLER
    // =========================================================================

    set_handler_statement: ($) =>
      seq(kwSeq("SET HANDLER"),
        commaSep1($._expression),
        optional(seq(kw("FOR"), choice($._expression, kwSeq("ALL INSTANCES")))),
        optional(choice(kw("ACTIVATION"), $._expression))),

    // =========================================================================
    // Phase 7: DESCRIBE FIELD
    // =========================================================================

    describe_field_statement: ($) =>
      seq(kwSeq("DESCRIBE FIELD"), $._expression,
        repeat1(choice(
          seq(kw("TYPE"), $._expression),
          seq(kw("LENGTH"), $._expression, optional(seq(kw("IN"), choice(kwSeq("BYTE MODE"), kwSeq("CHARACTER MODE"))))),
          seq(kw("DECIMALS"), $._expression),
          seq(kwSeq("OUTPUT-LENGTH"), $._expression),
          seq(kwSeq("HELP-ID"), $._expression),
          seq(kw("COMPONENTS"), $._expression),
          seq(kwSeq("EDIT MASK"), $._expression),
        ))),

    // =========================================================================
    // Phase 7: SET SCREEN
    // =========================================================================

    set_screen_statement: ($) =>
      seq(kwSeq("SET SCREEN"), $._expression),

    // =========================================================================
    // Phase 7: Obsolete
    // =========================================================================

    on_change_block: ($) =>
      seq(
        kw("ON"), kwSeq("CHANGE OF"), $._expression,
        optional(seq(kw("OR"), $._expression)),
        ".",
        repeat($._statement),
        kw("ENDON"), ".",
      ),

    provide_block: ($) =>
      seq(
        kw("PROVIDE"), repeat1($.identifier), kw("FROM"), $._expression,
        repeat(seq(repeat1($.identifier), kw("FROM"), $._expression)),
        kw("BETWEEN"), $._expression, kw("AND"), $._expression, ".",
        repeat($._statement),
        kw("ENDPROVIDE"), ".",
      ),
  },
});

function commaSep1(rule) {
  return seq(rule, repeat(seq(",", rule)));
}

function commaSep(rule) {
  return optional(commaSep1(rule));
}
