REPORT ztest_full_program.
* Full integration test program
" Inline comment

DATA: lv_name TYPE string,
      lv_count TYPE i,
      lv_result TYPE string.

CONSTANTS gc_max TYPE i VALUE 100.

TYPES: BEGIN OF ty_person,
         name TYPE string,
         age  TYPE i,
       END OF ty_person.

DATA lt_persons TYPE STANDARD TABLE OF ty_person WITH DEFAULT KEY.
DATA ls_person TYPE ty_person.
FIELD-SYMBOLS <ls_person> TYPE ty_person.

* Control flow
IF lv_count > 0.
  CLEAR lv_name.
ELSEIF lv_count = 0.
  lv_name = 'default'.
ELSE.
  FREE lv_result.
ENDIF.

CASE lv_count.
  WHEN 1.
    EXIT.
  WHEN 2.
    RETURN.
  WHEN OTHERS.
    CONTINUE.
ENDCASE.

DO 10 TIMES.
  APPEND ls_person TO lt_persons.
ENDDO.

LOOP AT lt_persons ASSIGNING <ls_person>.
  WRITE <ls_person>-name.
ENDLOOP.

LOOP AT lt_persons INTO ls_person WHERE name = 'Test'.
  EXIT.
ENDLOOP.

TRY.
    DATA(lv_len) = strlen( lv_name ).
  CATCH cx_sy_range_out_of_bounds INTO DATA(lx_error).
    MESSAGE lx_error->get_text( ) TYPE 'E'.
ENDTRY.

* Class definition
CLASS zcl_test DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS constructor IMPORTING iv_name TYPE string.
    METHODS get_name RETURNING VALUE(rv_name) TYPE string.
  PRIVATE SECTION.
    DATA mv_name TYPE string.
ENDCLASS.

CLASS zcl_test IMPLEMENTATION.
  METHOD constructor.
    mv_name = iv_name.
  ENDMETHOD.
  METHOD get_name.
    rv_name = mv_name.
  ENDMETHOD.
ENDCLASS.

* SQL
SELECT SINGLE matnr FROM mara INTO lv_name WHERE matnr = lv_name.

SELECT matnr, maktx FROM makt INTO TABLE lt_persons WHERE spras = 'E' ORDER BY matnr ASCENDING.

* Form
FORM calculate_total USING p_input TYPE i CHANGING p_output TYPE i.
  p_output = p_input * 2.
ENDFORM.

PERFORM calculate_total USING lv_count CHANGING lv_result.
