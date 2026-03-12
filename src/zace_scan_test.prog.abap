REPORT zace_calls_test_data.

CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS cls_meth.
     METHODS constructor .
    METHODS run IMPORTING iv_i TYPE i.
  PRIVATE SECTION.
    DATA mo_ref TYPE REF TO lcl_demo.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD cls_meth.
  ENDMETHOD.

  method constructor.
  ENDMETHOD.

  METHOD run.
    DATA lo TYPE REF TO lcl_demo.
    lo = NEW lcl_demo( ).
    lo->run( iv_i = 1 ).
    me->run( iv_i = 2 ).
    lcl_demo=>cls_meth( ).
    PERFORM my_form USING 42.
    CALL FUNCTION 'POPUP_TO_CONFIRM'.
    CALL METHOD lo->run( iv_i = 3 ).
  ENDMETHOD.
ENDCLASS.

FORM my_form USING p_val TYPE i.
  DATA a TYPE i.
  a = p_val.
ENDFORM.

START-OF-SELECTION.
  lcl_demo=>cls_meth( ).
  PERFORM my_form USING 1.
