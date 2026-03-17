REPORT zace_calls_test_data.

parameters a type i.

CLASS lcl_demo DEFINITION INHERITING FROM zcl_ace_window.
  PUBLIC SECTION.
    CLASS-METHODS cls_meth IMPORTING iv_a type i OPTIONAL iv_b type i PREFERRED PARAMETER iv_b RETURNING VALUE(rv) type string.
     METHODS constructor .
    METHODS run IMPORTING iv_i TYPE i.
  PRIVATE SECTION.
    DATA mo_ref TYPE REF TO lcl_demo.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD cls_meth.

  ENDMETHOD.

METHOD run.
    DATA lo TYPE REF TO lcl_demo.
    mo_ref = NEW lcl_demo( ).
    lo->run( iv_i = 1 ).
    me->run( iv_i = 2 ).
   lcl_demo=>cls_meth( ).
    PERFORM my_form USING 42.
    CALL FUNCTION 'POPUP_TO_CONFIRM'.
    CALL METHOD lo->run( iv_i = 3 ).
  ENDMETHOD.

  method constructor.
 data: deb TYPE REF TO ZCL_ACE.
     super->constructor( deb ).
     me->run( 1 ).
  ENDMETHOD.


ENDCLASS.

FORM my_form USING p_val TYPE i.
  DATA a TYPE i.
  a = p_val + 6.
ENDFORM.

START-OF-SELECTION.
 data: lo_demo type ref to lcl_demo.
   "data(lv_string) = lcl_demo=>cls_meth(  a ).
  "PERFORM my_form USING a.
 lo_demo = new lcl_demo( ).
