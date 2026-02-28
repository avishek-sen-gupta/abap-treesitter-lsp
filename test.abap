REPORT ztest.

DATA lv_name TYPE string.
DATA lv_count TYPE i.
CONSTANTS gc_max TYPE i VALUE 100.

CLASS lcl_example DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_example IMPLEMENTATION.
  METHOD run.
    lv_name = 'hello'.
  ENDMETHOD.
ENDCLASS.
