REPORT zace_calls_test_data.

parameters a type i.
INTERFACE lif_abapmerge DEFERRED.

INTERFACE lif_abapmerge.
METHODS: i1, i2.
ENDINTERFACE.

CLASS lcl_demo DEFINITION INHERITING FROM zcl_ace_window.
  PUBLIC SECTION.
    CLASS-METHODS cls_meth IMPORTING iv_a type i  iv_b type i  RETURNING VALUE(rv) type string.
     METHODS constructor .
    METHODS run IMPORTING iv_i TYPE i.
  PRIVATE SECTION.
    DATA mo_ref TYPE REF TO lcl_demo.
ENDCLASS.

INTERFACE lif_abapmerge_marker.
* abapmerge 0.16.0 - 2024-06-17T05:44:59.955Z
  CONSTANTS c_merge_timestamp TYPE string VALUE `2024-06-17T05:44:59.955Z`.
  CONSTANTS c_abapmerge_version TYPE string VALUE `0.16.0`.
ENDINTERFACE.


INTERFACE lif_abapmerge2.
METHODS: i3, i4.
ENDINTERFACE.

CLASS lcl_demo IMPLEMENTATION.
  METHOD cls_meth.

  ENDMETHOD.

METHOD run.
  data: a type i, b type i.
    DATA lo TYPE REF TO lcl_demo.
    mo_ref = NEW lcl_demo( ).
    lo->run( iv_i = 1 ).
    me->run( iv_i = 2 ).
   lcl_demo=>cls_meth( EXPORTING iv_a = a iv_b = b ).
    PERFORM my_form USING 42.
    CALL FUNCTION 'POPUP_TO_CONFIRM'.
    CALL METHOD lo->run( iv_i = 3 ).
  ENDMETHOD.

  method constructor.
 data: deb TYPE REF TO ZCL_ACE.
     super->constructor( deb ).
     me->run( 1 ).
     run( 1 ).
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
